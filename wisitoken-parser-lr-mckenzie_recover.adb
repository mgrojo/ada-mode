--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Token;
package body WisiToken.Parser.LR.McKenzie_Recover is

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Config : in Configuration)
   is
      use Ada.Text_IO;
      use Ada.Containers;
   begin
      Put ("(" & Image (Descriptor, Config.Stack));
      Put (" " & All_Parse_Action_Verbs'Image (Config.Verb));
      Put (SAL.Base_Peek_Type'Image (Config.Shared_Lookahead_Index) & " ");
      Put (Descriptor, Config.Local_Lookahead);
      Put (" " & Count_Type'Image (Config.Local_Lookahead_Index) & " ");
      Put (Descriptor, Config.Popped);
      Put (" ");
      Put (Image (Descriptor, Config.Pushed));
      Put (" ");
      Put (Descriptor, Config.Inserted);
      Put (" ");
      Put (Descriptor, Config.Deleted);
      Put (Natural'Image (Config.Cost) & ")");
   end Put;

   procedure Put
     (Message : in     String;
      Parser  : access LR.Instance'Class;
      Config  : in     Configuration)
   is
      use Ada.Containers;
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      --  For debugging output
      Put (Trace, Message & ":");
      Put (Trace, Natural'Image (Config.Cost));
      Trace.Put (", ");
      if Trace_Parse > 2 then
         Trace.Put (Image (Trace.Descriptor.all, Config.Stack, Depth => 4));
      else
         Trace.Put (Image (Trace.Descriptor.all, Config.Stack, Depth => 1));
      end if;
      Trace.Put ("|");

      if Config.Local_Lookahead.Length = 0 then
         Put (Trace, Parser.Shared_Lookahead.Peek (Config.Shared_Lookahead_Index));
      else
         Put (Trace, Config.Local_Lookahead);
         Trace.Put (":");
         Put (Trace, Int_Image (Integer (Config.Local_Lookahead_Index)));
      end if;
      Trace.Put ("|");
      Put (Trace, Config.Popped);
      Trace.Put ("/");
      Put (Trace, Config.Inserted);
      Trace.Put ("/");
      Put (Trace, Config.Deleted);
      Trace.New_Line;
   end Put;

   ----------
   --  Core code

   procedure Clear_Queue (Data : in out McKenzie_Data)
   is begin
      Data.Config_Heap.Clear;
   end Clear_Queue;

   procedure Enqueue (Data : in out McKenzie_Data; Config : in Configuration)
   is begin
      --  [1] has a check for duplicate configs here; that only happens with
      --  higher costs, which takes to long for our application.
      if Trace_Parse > 1 then
         Put ("enqueue", Data.Parser, Config);
      end if;
      Data.Config_Heap.Add (Config);
      Data.Enqueue_Count := Data.Enqueue_Count + 1;
   end Enqueue;

   function Get_Current_Input (Data : in McKenzie_Data; Config : in Configuration) return Token_ID
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Config.Local_Lookahead_Index /= Token_Arrays.No_Index and
        Config.Local_Lookahead_Index <= Config.Local_Lookahead.Last_Index
      then
         return Config.Local_Lookahead (Config.Local_Lookahead_Index);
      else
         return Data.Parser.Shared_Lookahead.Peek (Config.Shared_Lookahead_Index);
      end if;
   end Get_Current_Input;

   procedure Do_Shift
     (Data           : in out McKenzie_Data;
      Config         : in out Configuration;
      Action         : in     Shift_Action_Rec;
      Inserted_Token : in     Token_ID)
   is
      Trace : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;
   begin
      if Trace_Parse > 2 then
         Trace.Put (Image (Trace.Descriptor.all, Config.Stack));
         Trace.New_Line;
         Put (Trace, Action);
         Trace.New_Line;
      end if;
      Config.Stack.Push ((Action.State, Inserted_Token));
      Config.Inserted.Append (Inserted_Token);
      Config.Cost := Config.Cost + Data.Parser.Table.McKenzie.Insert (Inserted_Token);
      Enqueue (Data, Config);
   end Do_Shift;

   procedure Do_Reduce
     (Data           : in out McKenzie_Data;
      Config         : in     Configuration;
      Action         : in     Reduce_Action_Rec;
      Inserted_Token : in     Token_ID)
   is
      Trace : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;

      New_Config_1 : Configuration := Config;
      New_Config_2 : Configuration;
      New_State    : Unknown_State_Index;
      Next_Action  : Parse_Action_Node_Ptr;
   begin
      if Trace_Parse > 2 then
         Trace.Put (Image (Trace.Descriptor.all, New_Config_1.Stack));
         Trace.New_Line;
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      for I in 1 .. Action.Token_Count loop
         New_State := New_Config_1.Stack.Pop.State;
      end loop;
      New_State := New_Config_1.Stack.Peek.State;
      New_State := Goto_For (Data.Parser.Table.all, New_State, Action.LHS);

      if New_State = Unknown_State then
         return;
      end if;

      New_Config_1.Stack.Push ((New_State, Action.LHS));

      Next_Action := Action_For (Data.Parser.Table.all, New_State, Inserted_Token);
      loop
         New_Config_2 := New_Config_1;

         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            Do_Shift (Data, New_Config_2, Next_Action.Item, Inserted_Token);

         when Reduce =>
            Do_Reduce (Data, New_Config_2, Next_Action.Item, Inserted_Token);

         when Accept_It | Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
   end Do_Reduce;

   type Check_Item is record
      Config        : Configuration;
      Action        : Parse_Action_Node_Ptr;
      Current_Token : Token_ID;
   end record;

   package Check_Item_Queues is new SAL.Gen_Unbounded_Definite_Queues (Check_Item);

   function Check_One_Item
     (Data                  : in     McKenzie_Data;
      Item                  : in     Check_Item;
      Check_Item_Queue      : in out Check_Item_Queues.Queue_Type;
      Enqueue_Count         : in out Integer;
      Shared_Lookahead_Goal : in     SAL.Peek_Type)
     return Boolean
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;

      Trace      : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;
      Descriptor : WisiToken.Descriptor'Class renames Data.Parser.Semantic_State.Trace.Descriptor.all;

      Check_Config : Configuration := Item.Config;
      Check_Token  : Token_ID      := Item.Current_Token;

      Action : Parse_Action_Node_Ptr :=
        (if Item.Action = null
         then Action_For (Data.Parser.Table.all, Check_Config.Stack.Peek.State, Check_Token)
         else Item.Action);

      New_State          : Unknown_State_Index;
      Last_Token_Virtual : Boolean := False;
      Keep_Going         : Boolean := True;
   begin
      if Trace_Parse > 1 then
         Put ("check  ", Data.Parser, Check_Config);
         if Trace_Parse > 2 then
            Put_Line (Trace, "   action " & Image (Descriptor, Action.Item));
         end if;
      end if;

      loop
         if Trace_Parse > 2 then
            Trace.Put_Line
              ("checking :" & State_Index'Image (Check_Config.Stack.Peek.State) &
                 " : " & Image (Descriptor, Check_Token) &
                 " : " & Image (Descriptor, Action.Item));
         end if;

         if Action.Next /= null then
            if Trace_Parse > 1 then
               Trace.Put_Line ("checking: enqueue conflict " & Image (Descriptor, Action.Next.Item));
            end if;
            Enqueue_Count := Enqueue_Count + 1;
            Check_Item_Queue.Put ((Check_Config, Action.Next, Check_Token));
         end if;

         case Action.Item.Verb is
         when Shift =>
            Check_Config.Stack.Push ((Action.Item.State, Check_Token));
            Keep_Going := True;

            --  Get next token
            if Check_Config.Local_Lookahead.Length > 0 and
              Check_Config.Local_Lookahead.Length > Check_Config.Local_Lookahead_Index
            then
               --  These don't count towards Check_Limit
               Check_Config.Local_Lookahead_Index := Check_Config.Local_Lookahead_Index + 1;
               Check_Token := Check_Config.Local_Lookahead (Check_Config.Local_Lookahead_Index);
               Last_Token_Virtual := True;
            else
               if not Last_Token_Virtual then
                  Check_Config.Shared_Lookahead_Index := Check_Config.Shared_Lookahead_Index + 1;
               end if;
               Last_Token_Virtual := False;

               if Check_Config.Shared_Lookahead_Index > Data.Parser.Shared_Lookahead.Count then
                  Check_Token := Next_Grammar_Token (Data.Parser.Lexer, Data.Parser.Semantic_State);
                  Data.Parser.Shared_Lookahead.Put (Check_Token);
               else
                  Check_Token := Data.Parser.Shared_Lookahead.Peek (Check_Config.Shared_Lookahead_Index);
               end if;
            end if;

         when Reduce =>
            for I in 1 .. Action.Item.Token_Count loop
               New_State := Check_Config.Stack.Pop.State;
            end loop;
            New_State := Check_Config.Stack.Peek.State;
            New_State := Goto_For (Data.Parser.Table.all, New_State, Action.Item.LHS);

            if New_State = Unknown_State then
               Keep_Going := False;
            else
               Check_Config.Stack.Push ((New_State, Action.Item.LHS));
               Keep_Going := True;
            end if;

         when Error =>
            Keep_Going := False;

         when Accept_It =>
            Keep_Going := True;
         end case;

         exit when not Keep_Going or
           Action.Item.Verb = Accept_It or
           Check_Config.Shared_Lookahead_Index > Shared_Lookahead_Goal;

         Action := Action_For (Data.Parser.Table.all, Check_Config.Stack.Peek.State, Check_Token);
      end loop;

      return Keep_Going;
   end Check_One_Item;

   function Check
     (Data          : in     McKenzie_Data;
      Config        : in     Configuration;
      Current_Token : in     Token_ID)
     return Boolean
   is
      use all type SAL.Base_Peek_Type;
      Trace : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;

      Shared_Lookahead_Goal : constant SAL.Peek_Type := Config.Shared_Lookahead_Index +
        SAL.Base_Peek_Type (Config.Deleted.Length) + SAL.Peek_Type (Data.Parser.Table.McKenzie.Check_Limit);

      Check_Item_Queue : Check_Item_Queues.Queue_Type;
      Check_Count      : Integer := 1;
      Enqueue_Count    : Integer := 1;
      Keep_Going       : Boolean;
   begin
      Check_Item_Queue.Clear;
      Check_Item_Queue.Put ((Config, null, Current_Token));

      loop
         Keep_Going := Check_One_Item
           (Data, Check_Item_Queue.Get, Check_Item_Queue, Enqueue_Count, Shared_Lookahead_Goal);

         exit when Keep_Going or Check_Item_Queue.Count = 0;
         Check_Count := Check_Count + 1;

         if Trace_Parse > 1 then
            Trace.New_Line;
            Trace.Put_Line ("checking: dequeue conflict");
         end if;
      end loop;

      if Trace_Parse > 1 then
         Trace.Put ("check enqueue" & Integer'Image (Enqueue_Count) & " check" & Integer'Image (Check_Count));
         if Keep_Going then
            Trace.Put_Line (" : succeed");
         else
            Trace.Put_Line (" : fail");
         end if;
      end if;

      return Keep_Going;
   end Check;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Pattern_1;
      Parser       : in     LR.Instance'Class;
      Parser_State : in     Parser_Lists.Parser_State;
      Error_ID     : in     Token_ID;
      Data         : in out McKenzie_Data;
      Root_Config  : in     Configuration)
   is begin
      if Parser_State.Stack.Peek.ID = Pattern.Stack and
        Error_ID = Pattern.Error
      then
         declare
            --  Don't compute Expecting unless we need it.
            Descriptor : WisiToken.Descriptor'Class renames Parser.Semantic_State.Trace.Descriptor.all;
            Expecting  : constant WisiToken.Token_ID_Set := LR.Expecting
              (Parser.Table.all, Parser_State.Stack.Peek.State);
         begin
            if Expecting (Pattern.Expecting) and Count (Expecting) = 1 then
               if Trace_Parse > 0 then
                  Parser.Semantic_State.Trace.Put_Line
                    ("special rule recover_pattern_1 " &
                       Image (Descriptor, Pattern.Stack) & ", " &
                       Image (Descriptor, Pattern.Error) & ", " &
                       Image (Descriptor, Pattern.Expecting) &
                       " matched.");
               end if;

               declare
                  Config : Configuration := Root_Config;
               begin
                  Config.Local_Lookahead.Prepend (Pattern.Stack);
                  Config.Local_Lookahead.Prepend (Pattern.Error);
                  Config.Local_Lookahead.Prepend (Pattern.Expecting);
                  Config.Local_Lookahead_Index := 1;

                  Enqueue (Data, Config);
               end;
            end if;
         end;
      end if;
   end Apply_Pattern;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Pattern_2;
      Parser       : in     LR.Instance'Class;
      Parser_State : in     Parser_Lists.Parser_State;
      Error_ID     : in     Token_ID;
      Data         : in out McKenzie_Data;
      Root_Config  : in     Configuration)
   is begin
      if Parser_State.Stack.Peek.ID = Pattern.Stack and
        Error_ID = Pattern.Error
      then
         declare
            --  Don't compute Expecting unless we need it.
            Descriptor : WisiToken.Descriptor'Class renames Parser.Semantic_State.Trace.Descriptor.all;
            Expecting  : constant WisiToken.Token_ID_Set := LR.Expecting
              (Parser.Table.all, Parser_State.Stack.Peek.State);
         begin
            if Expecting (Pattern.Expecting) and Count (Expecting) = 1 then
               if Trace_Parse > 0 then
                  Parser.Semantic_State.Trace.Put_Line
                    ("special rule recover_pattern_2 " &
                       Image (Descriptor, Pattern.Stack) & ", " &
                       Image (Descriptor, Pattern.Error) & ", " &
                       Image (Descriptor, Pattern.Expecting) & ", " &
                       Image (Descriptor, Pattern.Insert) &
                       " matched.");
               end if;

               declare
                  Config : Configuration := Root_Config;
               begin
                  Config.Local_Lookahead.Prepend (Pattern.Stack);
                  Config.Local_Lookahead.Prepend (Pattern.Insert);
                  Config.Local_Lookahead.Prepend (Pattern.Expecting);
                  Config.Local_Lookahead_Index := 1;

                  Enqueue (Data, Config);
               end;
            end if;
         end;
      end if;
   end Apply_Pattern;

   procedure Patterns
     (Parser       : in out LR.Instance'Class;
      Parser_State : in     Parser_Lists.Parser_State;
      Data         : in out McKenzie_Data;
      Root_Config  : in Configuration)
   is
      Param    : McKenzie_Param_Type renames Parser.Table.McKenzie;
      Error_ID : Token_ID renames Parser.Shared_Lookahead.Peek (Parser_State.Shared_Lookahead_Index);
   begin
      for Pattern of Param.Patterns loop
         if Pattern in Recover_Pattern_1'Class then
            Apply_Pattern (Recover_Pattern_1 (Pattern), Parser, Parser_State, Error_ID, Data, Root_Config);

         elsif Pattern in Recover_Pattern_2'Class then
            Apply_Pattern (Recover_Pattern_2 (Pattern), Parser, Parser_State, Error_ID, Data, Root_Config);
         end if;
      end loop;
   end Patterns;

   procedure Recover
     (Parser       : in out LR.Instance'Class;
      Parser_State : in out Parser_Lists.Parser_State)
   is
      --  Sets Parser_State.Recover.Success True or False; if True,
      --  Parser_State.Recover.Result is valid.

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Data   : McKenzie_Data renames Parser_State.Recover;
      Trace  : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
      EOF_ID : Token_ID renames Trace.Descriptor.EOF_ID;

      Action_I : Action_List_Iterator;
   begin
      if Trace_Parse > 0 then
         Trace.New_Line;
         Trace.Put_Line ("parser" & Integer'Image (Parser_State.Label) & ":");
      end if;

      if Parser_State.Local_Lookahead.Length > 0 then
         --  Previous error recovery resume not finished.
         raise Programmer_Error;
      end if;

      Clear_Queue (Data);

      --  The special rules are not guaranteed to work when matched, so
      --  always queue the original error condition.
      declare
         Orig : Configuration := Default_Configuration;
      begin
         Orig.Stack := Parser_State.Stack;

         --  Parser_State.Local_Lookahead must be empty (else we would not get
         --  here). Therefore Parser_State current token is in
         --  Parser.Shared_Lookahead(Parser_State.Shared_Lookahead_Index)
         Orig.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index;
         Enqueue (Data, Orig);

         Patterns (Parser, Parser_State, Data, Orig);
      end;

      Check_Configs :
      loop
         exit Check_Configs when Data.Config_Heap.Count = 0 or
           Data.Config_Heap.Min_Key > Data.Parser.Table.McKenzie.Cost_Limit;

         declare
            use all type Fast_Token_ID_Vectors.Vector;

            Config     : constant Configuration := Data.Config_Heap.Remove;
            New_Config : Configuration;

            Current_Input : constant Token_ID := Get_Current_Input (Data, Config);
         begin
            Data.Check_Count := Data.Check_Count + 1;
            if Check (Data, Config, Current_Input) then
               if Trace_Parse > 0 then
                  Trace.Put
                    (Integer'Image (Parser_State.Label) & ": mckenzie enqueue" & Integer'Image (Data.Enqueue_Count) &
                       ", check " & Integer'Image (Data.Check_Count));
                  Put ("; succeed ", Data.Parser, Config);
               end if;
               Data.Result  := Config;
               Data.Success := True;
               Parser_State.Set_Verb (Parser_State.Prev_Verb);
               return;
            end if;

            if Config.Deleted = Fast_Token_ID_Vectors.Empty_Vector and
              Config.Inserted = Fast_Token_ID_Vectors.Empty_Vector and
              Config.Stack.Depth > 1 -- can't delete the first state
            then
               --  Try deleting stack top
               declare
                  Deleted_ID : constant Token_ID := Config.Stack.Peek.ID;
               begin
                  New_Config      := Config;
                  New_Config.Stack.Pop;
                  New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Delete (Deleted_ID);

                  New_Config.Popped.Append (Deleted_ID);
                  if Trace_Parse > 2 then
                     Trace.Put ("pop ");
                     Trace.Put (Deleted_ID);
                     Trace.New_Line;
                  end if;

                  Enqueue (Data, New_Config);
               end;
            end if;

            if Config.Deleted = Fast_Token_ID_Vectors.Empty_Vector then
               --  Find insertions to try
               Action_I := First_Action (Data.Parser.Table.States (Config.Stack.Peek.State));
               loop
                  exit when Action_I.Is_Done;
                  declare
                     ID     : constant Token_ID := Action_I.Symbol;
                     Action : Parse_Action_Rec renames Action_I.Action;
                  begin
                     if ID /= EOF_ID then
                        case Action.Verb is
                        when Shift =>
                           if Trace_Parse > 2 then
                              Trace.Put ("insert ");
                              Trace.Put (ID);
                              Trace.New_Line;
                           end if;

                           New_Config := Config;
                           Do_Shift (Data, New_Config, Action, ID);

                        when Reduce =>
                           if Trace_Parse > 2 then
                              Trace.Put ("try insert ");
                              Trace.Put (ID);
                              Trace.New_Line;
                           end if;
                           Do_Reduce (Data, Config, Action, ID);

                        when Accept_It | Error =>
                           null;
                        end case;
                     end if;
                  end;
                  Action_I.Next;
               end loop;
            end if;

            --  Try deleting current token, but not if it was inserted by a
            --  special rule.
            if Config.Local_Lookahead_Index = Token_Arrays.No_Index or
              Config.Local_Lookahead_Index > Config.Local_Lookahead.Last_Index
            then
               declare
                  Deleted_ID : constant Token_ID := Parser.Shared_Lookahead.Peek (Config.Shared_Lookahead_Index);
               begin
                  if Deleted_ID /= EOF_ID then
                     --  can't delete EOF
                     New_Config      := Config;
                     New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Delete (Deleted_ID);

                     New_Config.Deleted.Append (Deleted_ID);
                     if Trace_Parse > 2 then
                        Trace.Put ("delete ");
                        Trace.Put (Deleted_ID);
                        Trace.New_Line;
                     end if;

                     if New_Config.Shared_Lookahead_Index = Parser.Shared_Lookahead.Count then
                        Parser.Shared_Lookahead.Put (Next_Grammar_Token (Parser.Lexer, Parser.Semantic_State));

                        --  else some other parser already fetched the next token; just use
                        --  it.
                     end if;
                     New_Config.Shared_Lookahead_Index := New_Config.Shared_Lookahead_Index + 1;
                     Enqueue (Data, New_Config);
                  end if;
               end;
            end if;
         end;
      end loop Check_Configs;

      if Trace_Parse > 0 then
         Trace.Put_Line
           ("mckenzie (max cost" & Integer'Image (Parser.Table.McKenzie.Cost_Limit) &
              ") enqueue" & Integer'Image (Data.Enqueue_Count) &
              ", check " & Integer'Image (Data.Check_Count) & "; fail");
      end if;
      Data.Success := False;
   end Recover;

   function Recover
     (Parser  : in out LR.Instance'Class;
      Parsers : in out Parser_Lists.List)
     return Boolean
   is
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going : Integer := 0;
   begin
      if Trace_Parse > 0 then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
         if Trace_Parse > 2 then
            Trace.New_Line;
            Trace.Put ("shared_lookahead: ");
            Put (Trace, Parser.Shared_Lookahead);
            Trace.New_Line;
            Parser.Semantic_State.Put;
         end if;
      end if;

      for Parser_State of Parsers loop
         declare
            Data : McKenzie_Data renames Parser_State.Recover;
         begin
            Recover (Parser, Parser_State);

            if Data.Success then
               Keep_Going := Keep_Going + 1;
            end if;
         end;
      end loop;

      --  Adjust parser state for each successful recovery.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course. We'll have to see how this works in practice.

      for Parser_State of Parsers loop
         declare
            use Parser_Lists;
            use all type SAL.Base_Peek_Type;
            use all type Ada.Containers.Count_Type;
            Data : McKenzie_Data renames Parser_State.Recover;
         begin
            if Data.Success then
               if Parsers.Count > 1 then
                  --  We don't check 'Keep_Going > 1' here, because the successful
                  --  parser may have pending opertions, and the main loop will execute
                  --  them.

                  for ID of Data.Result.Popped loop
                     Parser_State.Stack.Pop;
                     Pend (Parser_State, (Discard_Stack, ID), Trace);
                  end loop;

                  for I in 1 .. Data.Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Data.Result.Pushed.Pop;
                     begin
                        Parser_State.Stack.Push (Item);
                        Pend (Parser_State, (Virtual_To_Lookahead, Item.ID), Trace);
                        Pend (Parser_State, (Push_Current, Item.ID), Trace);
                     end;
                  end loop;

                  for ID of Data.Result.Deleted loop
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;
                     Pend (Parser_State, (Discard_Lookahead, ID), Trace);
                  end loop;

                  for ID of reverse Data.Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  for ID of reverse Data.Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  Pend
                    (Parser_State,
                     (Verb    => Parser_Lists.Recover,
                      Recover => new Configuration'(Data.Result)),
                     Trace);
               else
                  --  Only one parser

                  for ID of Data.Result.Popped loop
                     Parser_State.Stack.Pop;
                     Parser.Semantic_State.Discard_Stack (ID);
                  end loop;

                  for I in 1 .. Data.Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Data.Result.Pushed.Pop;
                     begin
                        Parser_State.Stack.Push (Item);
                        Parser.Semantic_State.Virtual_To_Lookahead (Item.ID);
                        Parser.Semantic_State.Push_Current (Item.ID);
                     end;
                  end loop;

                  for ID of Data.Result.Deleted loop
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;

                     Parser.Semantic_State.Discard_Lookahead (ID);
                  end loop;

                  --  We use Parser_State.Local_Lookahead even when there is only one
                  --  parser, so main loop knows these are virtual tokens.
                  for ID of reverse Data.Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  for ID of reverse Data.Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  Parser.Semantic_State.Recover (Parser_State.Label, Data.Result);
               end if;

               Parser_State.Set_Verb (Data.Result.Verb);

               if Parser_State.Local_Lookahead.Count > 0 then
                  Parser_State.Current_Token := Parser_State.Local_Lookahead.Get;
                  Parser_State.Current_Token_Is_Virtual := True;
                  if Parsers.Count > 1 then
                     Pend (Parser_State, (Virtual_To_Lookahead, Parser_State.Current_Token), Trace);
                  else
                     Parser.Semantic_State.Virtual_To_Lookahead (Parser_State.Current_Token);
                  end if;
               else
                  Parser_State.Current_Token := Parser.Shared_Lookahead.Peek (Parser_State.Shared_Lookahead_Index);
                  Parser_State.Current_Token_Is_Virtual := False;
               end if;
            end if;
         end;
      end loop;

      if Trace_Parse > 2 then
         Parser.Semantic_State.Put;
      end if;

      return Keep_Going > 0;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
