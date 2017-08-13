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
with SAL.Gen_Queue_Interfaces;
with SAL.Gen_Unbounded_Definite_Queues;
package body WisiToken.Parser.LR.McKenzie_Recover is

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Config : in Configuration)
   is
      use Ada.Text_IO;
      use Ada.Containers;
   begin
      Put ("(" & Image (Descriptor, Config.Stack));
      Put (" " & All_Parse_Action_Verbs'Image (Config.Verb));
      Put (SAL.Base_Peek_Type'Image (Config.Shared_Lookahead_Index) & " ");
      WisiToken.Put (Descriptor, Config.Local_Lookahead);
      Put (" " & Count_Type'Image (Config.Local_Lookahead_Index) & " ");
      WisiToken.Put (Descriptor, Config.Popped);
      Put (" ");
      Put (Image (Descriptor, Config.Pushed));
      Put (" ");
      WisiToken.Put (Descriptor, Config.Inserted);
      Put (" ");
      WisiToken.Put (Descriptor, Config.Deleted);
      Put (Float'Image (Config.Cost) & ")");
   end Put;

   procedure Put
     (Message : in     String;
      Parser  : access LR.Instance'Class;
      Config  : in     Configuration)
   is
      use Ada.Containers;
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      Put (Trace, Message & ": ");
      if Trace_Parse > 2 then
         Trace.Put (Image (Trace.Descriptor.all, Config.Stack));
      else
         Put (Trace, Unknown_State_Index'Image (Config.Stack.Peek.State));
      end if;
      Trace.Put (" ");

      if Config.Local_Lookahead.Length = 0 then
         Put (Trace, Parser.Lookahead.Peek (Config.Shared_Lookahead_Index));
      else
         Put (Trace, Config.Local_Lookahead (Config.Local_Lookahead_Index));
      end if;
         Trace.Put (" ");
      Put (Trace, Config.Popped);
      Trace.Put (" ");
      Put (Trace, Config.Inserted);
      Trace.Put (" ");
      Put (Trace, Config.Deleted);
      Trace.Put (" ");
      Put (Trace, Float'Image (Config.Cost));
      Trace.New_Line;
   end Put;

   function Order_Config (A, B : in Configuration) return Boolean
   is begin
      --  Remove lowest cost first.
      return A.Cost < B.Cost;
   end Order_Config;

   package Config_Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Configuration);
   package Config_Queues is new SAL.Gen_Unbounded_Definite_Queues
     (Element_Type     => Configuration,
      Queue_Interfaces => Config_Queue_Interfaces);

   type McKenzie_Data (Parser : access LR.Instance'Class) is new LR.Recover_Data with
   record
      Queue         : Config_Queues.Queue_Type;
      Enqueue_Count : Integer := 0;
      Check_Count   : Integer := 0;
      Result        : Configuration;
      Success       : Boolean := False;
   end record;

   procedure Clear_Queue (Data : in out McKenzie_Data)
   is begin
      Data.Queue.Clear;
   end Clear_Queue;

   procedure Enqueue (Data : in out McKenzie_Data; Config : in Configuration)
   is begin
      --  FIXME: prune duplicate/higher cost configs
      if Trace_Parse > 1 then
         Put ("enqueue", Data.Parser, Config);
      end if;
      Data.Queue.Add (Config, Order_Config'Access);
      Data.Enqueue_Count := Data.Enqueue_Count + 1;
   end Enqueue;

   function Delete_Min (Data : in out McKenzie_Data) return Configuration
   is begin
      return Data.Queue.Remove;
   end Delete_Min;

   function Get_Current_Input (Data : in McKenzie_Data; Config : in Configuration) return Token_ID
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Config.Local_Lookahead_Index /= Token_Arrays.No_Index and
        Config.Local_Lookahead_Index <= Config.Local_Lookahead.Last_Index
      then
         return Config.Local_Lookahead (Config.Local_Lookahead_Index);
      else
         return Data.Parser.Lookahead.Peek (Config.Shared_Lookahead_Index);
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

   function Check_Reduce
     (Data          : in     McKenzie_Data;
      Config        : in out Configuration;
      Action        : in     Reduce_Action_Rec;
      Current_Token : in     Token_ID)
     return Boolean
   is
      use all type Ada.Containers.Count_Type;
      New_State   : Unknown_State_Index;
      Next_Action : Parse_Action_Node_Ptr;
   begin
      for I in 1 .. Action.Token_Count loop
         New_State := Config.Stack.Pop.State;
      end loop;
      New_State := Config.Stack.Peek.State;
      New_State := Goto_For (Data.Parser.Table.all, New_State, Action.LHS);

      if New_State = Unknown_State then
         return False;
      end if;

      Next_Action := Action_For (Data.Parser.Table.all, New_State, Current_Token);
      loop
         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            return True;

         when Reduce =>
            Config.Stack.Push ((New_State, Current_Token));
            if Check_Reduce (Data, Config, Next_Action.Item, Current_Token) then
               return True;
            end if;

         when Accept_It =>
            return True;

         when Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
      return False;
   end Check_Reduce;

   function Check (Data : in McKenzie_Data; Config : in Configuration; Current_Token : in Token_ID) return Boolean
   is
      --  Return True if Config allows parsing to continue
      --
      --  FIXME: verify that parsing can continue thru lookaheads, so
      --  we don't re-enter recover while still processing lookaheads
      --  (which we don't currently allow).

      Action : Parse_Action_Node_Ptr := Action_For (Data.Parser.Table.all, Config.Stack.Peek.State, Current_Token);
   begin
      loop
         exit when Action = null;

         case Action.Item.Verb is
         when Shift =>
            return True;

         when Reduce =>
            --  See if Do_Reduce will succeed
            declare
               Test_Config : Configuration := Config; -- Modified by Check_Reduce
            begin
               if Check_Reduce (Data, Test_Config, Action.Item, Current_Token) then
                  return True;
               end if;
            end;

         when Error =>
            null;

         when Accept_It =>
            return True;
         end case;

         Action := Action.Next;
      end loop;
      return False;
   end Check;

   --  FIXME: make visible, add to some hook in .wy
   function Statement_Terminal_Sequence
     (Parser       : in out LR.Instance'Class;
      Parser_State : in out Parser_Lists.Parser_State;
      Param        : in     McKenzie_Param_Type;
      Config       :    out Configuration)
     return Boolean
   is
      --  Assume Parser_State parser encountered an error at Current_Token.
      --  If Parser_State.Stack, Current_Token match a portion of a terminal
      --  sequence, insert that portion and return True. Else return
      --  False.

      pragma Unreferenced (Param);
      Sequence_Of_Statements_ID : constant Token_ID := 95; -- FIXME: move to Param.
      Begin_ID                  : constant Token_ID := 4;  -- FIXME: move to Param.

      use all type Ada.Containers.Count_Type;

      Stack_ID : Token_ID;

      Descriptor : WisiToken.Descriptor'Class renames Parser.Semantic_State.Trace.Descriptor.all;
   begin
      if not (Descriptor.Image (Sequence_Of_Statements_ID).all = "sequence_of_statements" and
                Descriptor.Image (Begin_ID).all = "BEGIN")
      then
         --  Language is not Ada_Lite; rule not valid
         return False;
      end if;

      if Parser_State.Prev_Verb = Reduce and Parser_State.Pre_Reduce_Stack_Item.ID = Sequence_Of_Statements_ID then
         Stack_ID := Sequence_Of_Statements_ID;

      elsif Parser_State.Prev_Verb = Shift and Parser_State.Stack.Peek.ID = Begin_ID then
         Stack_ID := Begin_ID;

      else
         return False;
      end if;

      declare
         Current_Token : constant Token_ID := Parser.Lookahead.Peek;

         --  FIXME: Hard code a terminal sequence from Ada_Lite for now; need to
         --  generate from grammar, put in Parser.Table.
         Terminal_Sequences : array (1 .. 1) of Token_Array;

         Sequence  : Token_Array;
         First_Set : Boolean;
         First     : Natural_Index_Type; -- index into Sequence of IDs to insert
         Last_Set  : Boolean := False;
         Last      : Natural_Index_Type;
      begin
         --  Just enough for test_mckenzie_recover Error_5
         Terminal_Sequences (1).Append (12); -- if
         Terminal_Sequences (1).Append (22); -- then
         Terminal_Sequences (1).Append (8); -- elsif
         Terminal_Sequences (1).Append (22); -- then

         Find_ID :
         for Seq of Terminal_Sequences loop
            First_Set := False;
            for I in Seq.First_Index .. Seq.Last_Index loop
               case Stack_ID is
               when Sequence_Of_Statements_ID =>
                  if not First_Set and then Parser.Table.Follow (Stack_ID, Seq (I)) then
                     Sequence  := Seq;
                     First     := I;
                     First_Set := True;
                  end if;

               when Begin_ID =>
                  if not First_Set then
                     Sequence  := Seq;
                     First     := I;
                     First_Set := True;
                  end if;
               when others =>
                  raise Programmer_Error;
               end case;

               if First_Set and Seq (I) = Current_Token then
                  Last := I - 1;
                  Last_Set := True;
                  exit Find_ID;
               end if;
            end loop;
         end loop Find_ID;

         if Last_Set then
            if Trace_Parse > 0 then
               Parser.Semantic_State.Trace.Put_Line
                 ("terminal_sequence " & Image (Parser.Semantic_State.Trace.Descriptor.all, Sequence (First)) &
                    " .. " & Image (Parser.Semantic_State.Trace.Descriptor.all, Sequence (Last)));
            end if;

            case Stack_ID is
            when Sequence_Of_Statements_ID =>
               Config       := Default_Configuration;
               Config.Stack := Parser_State.Stack;
               Config.Verb  := Reduce;

               Config.Popped.Append (Config.Stack.Pop.ID);
               Config.Pushed.Push (Parser_State.Pre_Reduce_Stack_Item);
               Config.Stack.Push (Parser_State.Pre_Reduce_Stack_Item);

            when Begin_ID =>
               Config       := Default_Configuration;
               Config.Stack := Parser_State.Stack;
               Config.Verb  := Shift_Local_Lookahead;

            when others =>
               raise Programmer_Error;
            end case;

            for I in reverse First .. Last loop
               Config.Local_Lookahead.Prepend (Sequence (I));
            end loop;
            Config.Local_Lookahead_Index := 1;
            return True;
         else
            return False;
         end if;
      end;
   end Statement_Terminal_Sequence;

   --  FIXME: make visible, add to some hook in .wy
   function Dotted_Name
     (Data         : in out McKenzie_Data;
      Parser_State : in     Parser_Lists.Parser_State;
      Config       :    out Configuration)
     return Boolean
   is
      --  Assume Parser_State parser encountered an error at Current_Token.
      --  If Parser_State.Stack, Current_Token match a dotted name that
      --  errored on '.', set config to an appropriate root config,
      --  and return True. Else return False.

      Parser : LR.Instance'Class renames Data.Parser.all;
      Param  : McKenzie_Param_Type renames Parser.Table.McKenzie;

      Current_ID : constant Token_ID := Parser.Lookahead.Peek;
   begin
      if Param.Dot_ID = Default_McKenzie_Param.Dot_ID and
        Param.Identifier_ID = Default_McKenzie_Param.Identifier_ID
      then
         --  This rule is not enabled
         return False;
      end if;

      if Parser_State.Stack.Peek.ID = Param.Identifier_ID and
        Current_ID = Param.Dot_ID
      then
         if Trace_Parse > 1 then
            Parser.Semantic_State.Trace.Put_Line ("special rule Dotted_Name matched; insert IDENTIFIER");
         end if;

         --  Parser encountered something like:
         --
         --      loop ... end loop Parent.Child;
         --
         --  and errored on '.', expecting ';'. So we insert
         --  'IDENTIFIER', so that normal McKenzie will find 'insert
         --  ;' more quickly, and then '.' is legal.
         --
         --  We don't insert the semicolon as well, because there may
         --  be other situations where the semicolon is wrong (ie,
         --  association_opt in ada_lite.wy).
         --
         --  Ideally we would replace the identifier on the stack with
         --  a dummy inserted one, leaving the real identifier to be
         --  part of the dotted name; that would allow proper syntax
         --  coloring etc. But this is close enough.

         Config       := Default_Configuration;
         Config.Stack := Parser_State.Stack;

         Config.Local_Lookahead.Prepend (Param.Identifier_ID);
         Config.Local_Lookahead_Index := 1;
         return True;
      else
         return False;
      end if;
   end Dotted_Name;

   procedure Recover
     (Parser       : in out LR.Instance'Class;
      Parser_State : in out Parser_Lists.Parser_State)
   is
      --  Sets Parser_State.Recover.Success True or False; if True,
      --  Parser_State.Recover.Result is valid.

      Data   : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);

      Trace  : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
      EOF_ID : Token_ID renames Trace.Descriptor.EOF_ID;

      Root_Config : Configuration;
      Action      : Parse_Action_Node_Ptr;
   begin
      if Trace_Parse > 1 then
         Trace.New_Line;
         Trace.Put_Line ("parser" & Integer'Image (Parser_State.Label) & ":");
      end if;

      Clear_Queue (Data);

      if Dotted_Name (Data, Parser_State, Root_Config) then
         null;
      elsif Statement_Terminal_Sequence (Parser, Parser_State, Data.Parser.Table.McKenzie, Root_Config) then
         null;
      else
         Root_Config       := Default_Configuration;
         Root_Config.Stack := Parser_State.Stack;
      end if;

      Root_Config.Shared_Lookahead_Index := 1; --  Current_Token is in Lookahead.

      Enqueue (Data, Root_Config);

      Check_Configs :
      loop
         exit Check_Configs when Data.Queue.Is_Empty or Data.Enqueue_Count > Data.Parser.Table.McKenzie.Enqueue_Limit;

         declare
            use all type SAL.Base_Peek_Type;
            use all type Token_Array;

            Config     : constant Configuration := Delete_Min (Data);
            New_Config : Configuration;

            Current_Input : constant Token_ID := Get_Current_Input (Data, Config);
         begin
            Data.Check_Count := Data.Check_Count + 1;
            if Trace_Parse > 1 then
               Put ("check  ", Data.Parser, Config);
            end if;
            if Check (Data, Config, Current_Input) then
               if Trace_Parse > 0 then
                  Trace.Put_Line
                    ("mckenzie enqueue" & Integer'Image (Data.Enqueue_Count) &
                       ", check " & Integer'Image (Data.Check_Count));
                  Put ("succeed", Data.Parser, Config);
               end if;
               Data.Result  := Config;
               Data.Success := True;
               Parser_State.Set_Verb (Parser_State.Prev_Verb);
               return;
            end if;

            if Config.Deleted = Empty_Token_Array and
              Config.Inserted = Empty_Token_Array and
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

            if Config.Deleted = Empty_Token_Array then
               --  Find insertions to try
               for ID in Data.Parser.Table.First_Terminal .. Data.Parser.Table.Last_Terminal loop
                  if ID /= EOF_ID then
                     Action := Action_For (Data.Parser.Table.all, Config.Stack.Peek.State, ID);
                     loop
                        exit when Action = null;
                        case Action.Item.Verb is
                        when Shift =>
                           if Trace_Parse > 2 then
                              Trace.Put ("insert ");
                              Trace.Put (ID);
                              Trace.New_Line;
                           end if;

                           New_Config := Config;
                           Do_Shift (Data, New_Config, Action.Item, ID);

                        when Reduce =>
                           if Trace_Parse > 2 then
                              Trace.Put ("try insert ");
                              Trace.Put (ID);
                              Trace.New_Line;
                           end if;
                           Do_Reduce (Data, Config, Action.Item, ID);

                        when Accept_It | Error =>
                           null;
                        end case;
                        Action := Action.Next;
                     end loop;
                  end if;
               end loop;
            end if;

            --  Try deleting current token, but not if it was inserted
            --  by a special rule.
            declare
               use all type Ada.Containers.Count_Type;
               Deleted_ID : constant Token_ID := Parser.Lookahead.Peek (Config.Shared_Lookahead_Index);
            begin
               if (Config.Local_Lookahead_Index = Token_Arrays.No_Index or
                 Config.Local_Lookahead_Index > Config.Local_Lookahead.Last_Index) and
                 Deleted_ID /= EOF_ID
               then
                  --  can't delete EOF
                  New_Config      := Config;
                  New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Delete (Deleted_ID);

                  New_Config.Deleted.Append (Deleted_ID);
                  if Trace_Parse > 2 then
                     Trace.Put ("delete ");
                     Trace.Put (Deleted_ID);
                     Trace.New_Line;
                  end if;

                  if New_Config.Shared_Lookahead_Index = Parser.Lookahead.Count then
                     declare
                        ID : constant Token_ID := Parser.Lexer.Find_Next;
                     begin
                        Parser.Lookahead.Put (ID);
                        --  We must call Input_Lookahead here, while the lexer data is valid
                        Parser.Semantic_State.Input_Lookahead (ID, Parser.Lexer);
                     end;

                     --  else some other parser already fetched the
                     --  next token; just use it.
                  end if;
                  New_Config.Shared_Lookahead_Index := New_Config.Shared_Lookahead_Index + 1;
                  Enqueue (Data, New_Config);
               end if;
            end;
         end;
      end loop Check_Configs;
      if Trace_Parse > 0 then
         Trace.Put_Line
           ("mckenzie enqueue" & Integer'Image (Data.Enqueue_Count) &
              ", check " & Integer'Image (Data.Check_Count) & "; fail");
      end if;
      Data.Success := False;
   end Recover;

   function Recover
     (Parser  : in out LR.Instance'Class;
      Parsers : in out Parser_Lists.List)
     return Boolean
   is
      Trace  : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going      : Integer := 0;
      Prev_Result     : Configuration := Default_Configuration; -- keep compiler happy
      Prev_Result_Set : Boolean := False;
      All_Equal       : Boolean := True;
   begin
      if Trace_Parse > 2 then
         Trace.New_Line;
         Trace.Put ("lookahead: ");
         Put (Trace, Parser.Lookahead);
         Trace.New_Line;
         Parser.Semantic_State.Put;
      end if;

      for Parser_State of Parsers loop
         Free (Parser_State.Recover);
         Parser_State.Recover := new McKenzie_Data (Parser'Unchecked_Access);

         declare
            Data : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);
         begin
            --  FIXME: Check if previous parser result works for this
            --  parser, before starting search - saves time

            Recover (Parser, Parser_State);

            if Data.Success then
               Keep_Going := Keep_Going + 1;
               if Prev_Result_Set and then Prev_Result /= Data.Result then
                  All_Equal := False;
               end if;
               Prev_Result_Set := True;
               Prev_Result     := Data.Result;
            end if;
         end;
      end loop;

      --  Adjust parser state for each successful recovery.
      --
      --  One option here would be to keep only the parser with the
      --  least cost fix. However, the normal reason for having
      --  multiple parsers is to resolve a grammar ambiguity; the
      --  least cost fix might resolve the ambiguity the wrong way. As
      --  could any other fix, of course. We'll have to see how this
      --  works in practice.

      for Parser_State of Parsers loop
         declare
            use all type SAL.Base_Peek_Type;
            Data : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);
         begin
            if Data.Success then
               if Keep_Going > 1 and not All_Equal then

                  for ID of Data.Result.Popped loop
                     Parser_State.Stack.Pop;
                     Parser_State.Pend_Items.Put ((Parser_Lists.Pop, ID));
                  end loop;

                  for I in 1 .. Data.Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Data.Result.Pushed.Pop;
                     begin
                        Parser_State.Stack.Push (Item);
                        Parser_State.Pend_Items.Put ((Parser_Lists.Input, Item.ID));
                        Parser_State.Pend_Items.Put ((Parser_Lists.Push, Item.ID));
                     end;
                  end loop;

                  Parser_State.Shared_Lookahead_Index := 1;

                  for ID of Data.Result.Deleted loop
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;
                     Parser_State.Pend_Items.Put ((Parser_Lists.Discard_Lookahead, ID));
                  end loop;

                  for ID of reverse Data.Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  for ID of reverse Data.Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  --  popped/pushed handled above
                  --  FIXME: if delete panic, delete popped/pushed here.
                  Parser_State.Pend_Items.Put
                    ((Verb    => Parser_Lists.Recover,
                      Popped  => Empty_Token_Array,
                      Pushed  => Empty_Token_Array,
                      Recover => new Configuration'(Data.Result)));

               else
                  --  Only one parser succeeded, or all got the same result.

                  for ID of Data.Result.Popped loop
                     Parser_State.Stack.Pop;
                     Parser.Semantic_State.Pop_Token (ID);
                  end loop;

                  for I in 1 .. Data.Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Data.Result.Pushed.Pop;
                     begin
                        Parser_State.Stack.Push (Item);
                        Parser.Semantic_State.Input_Token (Item.ID, null);
                        Parser.Semantic_State.Push_Token (Item.ID);
                     end;
                  end loop;

                  Parser_State.Shared_Lookahead_Index := 1;

                  for ID of Data.Result.Deleted loop
                     --  Input_Token was called for these tokens, so we must call Discard_Token
                     Parser.Lookahead.Drop;
                     Parser.Semantic_State.Discard_Input (ID);
                  end loop;

                  --  We use Parser_State.Local_Lookahead even when
                  --  there is only one parser, so main loop knows
                  --  whether Semantic_State.Input_Token has been
                  --  called or not.
                  for ID of reverse Data.Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  for ID of reverse Data.Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head (ID);
                  end loop;

                  --  popped/pushed handled above
                  Parser.Semantic_State.Recover
                    (Popped_Tokens => Empty_Token_Array,
                     Pushed_Tokens => Empty_Token_Array,
                     Recover       => new Configuration'(Data.Result));
               end if;

               Parser_State.Set_Verb (Data.Result.Verb);

               case Data.Result.Verb is
               when Reduce =>
                  if Parser_State.Local_Lookahead.Count > 0 then
                     Parser_State.Current_Token := Parser_State.Local_Lookahead.Get;
                     if Keep_Going > 1 then
                        Parser_State.Pend_Items.Put ((Parser_Lists.Input, Parser_State.Current_Token));
                     else
                        Parser.Semantic_State.Input_Token (Parser_State.Current_Token, null);
                     end if;
                  else
                     Parser_State.Current_Token := Parser.Lookahead.Peek (Parser_State.Shared_Lookahead_Index);
                     if Keep_Going > 1 then
                        Parser_State.Pend_Items.Put ((Parser_Lists.Lookahead_To_Input, Parser_State.Current_Token));
                     else
                        Parser.Semantic_State.Move_Lookahead_To_Input (Parser_State.Current_Token);
                     end if;
                  end if;

               when Shift_Local_Lookahead =>
                  --  Main loop will set Parser_State.Current_Token from lookahead.
                  null;

               when Shift | Accept_It | Error =>
                  raise Programmer_Error;
               end case;

            end if;
         end;
      end loop;

      if Trace_Parse > 2 then
         Parser.Semantic_State.Put;
      end if;

      return Keep_Going > 0;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
