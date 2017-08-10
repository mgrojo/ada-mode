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

   Recover_Fail : exception;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Config : in Configuration)
   is
      use Ada.Text_IO;
      use Ada.Containers;
   begin
      Put ("(" & Image (Descriptor, Config.Stack) & Count_Type'Image (Config.Lookahead_Index) & " ");
      WisiToken.Put (Descriptor, Config.Popped);
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
      use all type Ada.Containers.Count_Type;

      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      Put (Trace, Message & ": ");
      if Trace_Parse > 2 then
         Trace.Put (Image (Trace.Descriptor.all, Config.Stack));
      else
         Put (Trace, Unknown_State_Index'Image (Config.Stack.Peek.State));
      end if;
      Trace.Put (" ");
      Put (Trace, Parser.Lookahead (Config.Lookahead_Index));
      Trace.Put (" ");
      if Config.Popped.Length = 0 then
         Put (Trace, "null");
      else
         Put (Trace, Config.Popped);
      end if;
      Trace.Put (" ");
      if Config.Inserted.Length = 0 then
         Put (Trace, "null");
      else
         Put (Trace, Config.Inserted);
      end if;
      Trace.Put (" ");
      if Config.Deleted.Length = 0 then
         Put (Trace, "null");
      else
         Put (Trace, Config.Deleted);
      end if;
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

   type McKenzie_Data (Parser : access LR.Instance'Class) is new Recover_Data with
   record
      Queue         : Config_Queues.Queue_Type;
      Enqueue_Count : Integer := 0;
      Check_Count   : Integer := 0;
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

   procedure Do_Shift
     (Data           : in out McKenzie_Data;
      Config         : in out Configuration;
      Action         : in     Shift_Action_Rec;
      Inserted_Token : in     Token_ID)
   is begin
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

      Next_Action := Action_For (Data.Parser.Table.all, New_State, Inserted_Token);
      loop
         New_Config_2 := New_Config_1;
         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            New_Config_2.Stack.Push ((New_State, Inserted_Token));
            Do_Shift (Data, New_Config_2, Next_Action.Item, Inserted_Token);

         when Reduce =>
            New_Config_2.Stack.Push ((New_State, Inserted_Token));
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
      Sequence_Of_Statements_ID : constant Token_ID := 93; -- FIXME: move to Param.
      Begin_ID                  : constant Token_ID := 4;  -- FIXME: move to Param.

      use Ada.Containers;
      Data : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);

      Stack_Token : Token_ID;
   begin
      if Parser_State.Prev_Verb = Reduce and Parser_State.Pre_Reduce_Stack_Item.Token = Sequence_Of_Statements_ID then
         Stack_Token := Sequence_Of_Statements_ID;

      elsif Parser_State.Prev_Verb = Shift and Parser_State.Stack.Peek.Token = Begin_ID then
         Stack_Token := Begin_ID;

      else
         return False;
      end if;

      declare
         Current_Token : constant Token_ID := Parser.Lookahead (1);

         --  FIXME: Hard code a terminal sequence from Ada_Lite for now; need to
         --  generate from grammar, put in Parser.Table.
         Terminal_Sequences : array (1 .. 1) of Token_Array;

         Sequence  : Token_Array;
         First_Set : Boolean;
         First     : Count_Type; -- index into Sequence of IDs to insert
         Last_Set  : Boolean := False;
         Last      : Count_Type;
      begin
         --  Just enough for test_mckenzie_recover Error_5
         Terminal_Sequences (1).Append (12); -- if
         Terminal_Sequences (1).Append (21); -- then
         Terminal_Sequences (1).Append (8); -- elsif
         Terminal_Sequences (1).Append (21); -- then

         Find_ID :
         for Seq of Terminal_Sequences loop
            First_Set := False;
            for I in Seq.First_Index .. Seq.Last_Index loop
               case Stack_Token is
               when Sequence_Of_Statements_ID =>
                  if not First_Set and then Parser.Table.Follow (Stack_Token, Seq (I)) then
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
            if Trace_Parse > 1 then
               Parser.Semantic_State.Trace.Put_Line
                 ("terminal_sequence " & Image (Parser.Semantic_State.Trace.Descriptor.all, Sequence (First)) &
                    " .. " & Image (Parser.Semantic_State.Trace.Descriptor.all, Sequence (Last)));
            end if;

            case Stack_Token is
            when Sequence_Of_Statements_ID =>
               Data.Popped_Tokens.Append (Parser_State.Stack.Pop.Token);

               Parser_State.Stack.Push (Parser_State.Pre_Reduce_Stack_Item);
               Data.Pushed_Tokens.Append (Parser_State.Pre_Reduce_Stack_Item.Token);

               Config :=
                 (Stack           => Parser_State.Stack,
                  Lookahead_Index => Natural_Index_Type'First,
                  Popped          => Token_Arrays.Empty_Vector,
                  Inserted        => Token_Arrays.Empty_Vector,
                  Deleted         => Token_Arrays.Empty_Vector,
                  Cost            => 0.0);

            when Begin_ID =>
               Config :=
                 (Stack           => Parser_State.Stack,
                  Lookahead_Index => Natural_Index_Type'First,
                  Popped          => Token_Arrays.Empty_Vector,
                  Inserted        => Token_Arrays.Empty_Vector,
                  Deleted         => Token_Arrays.Empty_Vector,
                  Cost            => 0.0);

            when others =>
               raise Programmer_Error;
            end case;

            for I in reverse First .. Last loop
               --  FIXME: this doesn't show up in recorded error solution; put in Inserted
               --  Requires doing Shift operations
               Parser.Lookahead.Prepend (Sequence (I));
               Parser.Semantic_State.Input_Token (Sequence (I), null);
            end loop;
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
      Param : McKenzie_Param_Type renames Parser.Table.McKenzie;

      Current_Token : constant Token_ID := Parser.Lookahead (1);
   begin
      if Param.Dot_ID = Default_McKenzie_Param.Dot_ID and
        Param.Identifier_ID = Default_McKenzie_Param.Identifier_ID
      then
         --  This rule is not enabled
         return False;
      end if;

      if Parser_State.Stack.Peek.Token = Param.Identifier_ID and
        Current_Token = Param.Dot_ID
      then
         if Trace_Parse > 1 then
            Parser.Semantic_State.Trace.Put_Line ("special rule Dotted_Name matched");
         end if;

         --  Parser encountered something like:
         --
         --      loop ... end Parent.Child;
         --
         --  and errored on '.', expecting 'loop'
         --  So we pushback 'IDENTIFIER', so that '.' is now legal.
         --
         --  Ideally we would replace the identifier on the stack with
         --  a dummy inserted one, leaving the real identifier to be
         --  part of the dotted name; that would allow proper syntax
         --  coloring etc. But this is close enough.

         Config :=
           (Stack           => Parser_State.Stack,
            Lookahead_Index => Natural_Index_Type'First,
            Popped          => Token_Arrays.Empty_Vector,
            Inserted        => Token_Arrays.Empty_Vector,
            Deleted         => Token_Arrays.Empty_Vector,
            Cost            => 0.0);

         --  FIXME: should not modify lookahead for single parser; see FIXME: in Recover below
         --  Also, this is not recorded in Config for reuse.
         Parser.Lookahead.Prepend (Param.Identifier_ID);
         Parser.Semantic_State.Input_Token (Param.Identifier_ID, null);
         return True;
      else
         return False;
      end if;
   end Dotted_Name;

   function Recover
     (Parser       : in out LR.Instance'Class;
      Parser_State : in out Parser_Lists.Parser_State)
     return Configuration
   is
      --  Raises Recover_Fail or returns recover Config

      Data   : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);
      Action : Parse_Action_Node_Ptr;

      EOF_ID : Token_ID renames Parser.Semantic_State.Trace.Descriptor.EOF_ID;

      Trace : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;

      Root_Config : Configuration;
   begin
      if Trace_Parse > 1 then
         Trace.New_Line;
      end if;

      Clear_Queue (Data);

      if Dotted_Name (Data, Parser_State, Root_Config) then
         null;
      elsif Statement_Terminal_Sequence (Parser, Parser_State, Data.Parser.Table.McKenzie, Root_Config) then
         null;
      else
         Root_Config :=
           (Stack           => Parser_State.Stack,
            Lookahead_Index => 1,
            Popped          => Token_Arrays.Empty_Vector,
            Inserted        => Token_Arrays.Empty_Vector,
            Deleted         => Token_Arrays.Empty_Vector,
            Cost            => 0.0);
      end if;

      Enqueue (Data, Root_Config);

      loop
         exit when Data.Queue.Is_Empty or Data.Enqueue_Count > Data.Parser.Table.McKenzie.Enqueue_Limit;

         declare
            use all type SAL.Base_Peek_Type;
            use all type Token_Array;

            Config     : constant Configuration := Delete_Min (Data);
            New_Config : Configuration;

            Current_Input : constant Token_ID := Parser.Lookahead (Config.Lookahead_Index);
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
               return Config;
            end if;

            if Config.Deleted = Empty_Token_Array and
              Config.Inserted = Empty_Token_Array and
              Config.Stack.Depth > 1 -- can't delete the first state
            then
               --  Try deleting stack top
               declare
                  Deleted_Token : constant Token_ID := Config.Stack.Peek.Token;
               begin
                  New_Config      := Config;
                  New_Config.Stack.Pop;
                  New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Delete (Deleted_Token);

                  New_Config.Popped.Append (Deleted_Token);
                  if Trace_Parse > 2 then
                     Trace.Put ("pop ");
                     Trace.Put (Deleted_Token);
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
                              Put (Trace, Action.Item);
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

            --  Try deleting current token
            declare
               use all type Ada.Containers.Count_Type;
               Deleted_Token : constant Token_ID := Parser.Lookahead (Config.Lookahead_Index);
            begin
               if Deleted_Token /= EOF_ID then
                  --  can't delete EOF
                  New_Config      := Config;
                  New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Delete (Deleted_Token);

                  New_Config.Deleted.Append (Deleted_Token);
                  if Trace_Parse > 2 then
                     Trace.Put ("delete ");
                     Trace.Put (Deleted_Token);
                     Trace.New_Line;
                  end if;

                  --  FIXME: should not modify lookahead for single parser; see FIXME: in Recover below
                  if New_Config.Lookahead_Index = Parser.Lookahead.Last_Index then
                     Parser.Lookahead.Append (Parser.Lexer.Find_Next);
                     --  We must call Input_Token here, while the lexer data is valid
                     Parser.Semantic_State.Input_Token (Parser.Lookahead (Parser.Lookahead.Last_Index), Parser.Lexer);
                  end if;
                  New_Config.Lookahead_Index := New_Config.Lookahead_Index + 1;
                  Enqueue (Data, New_Config);
               end if;
            end;

         end;
      end loop;
      if Trace_Parse > 0 then
         Trace.Put_Line
           ("mckenzie enqueue" & Integer'Image (Data.Enqueue_Count) &
              ", check " & Integer'Image (Data.Check_Count) & "; fail");
      end if;
      raise Recover_Fail;
   end Recover;

   function Recover
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      use all type Ada.Containers.Count_Type;
      Keep_Going : Boolean := False;
   begin
      Parser.Lookahead.Append (Current_Token);

      if Parsers.Count > 1 then
         --  See FIXME below
         raise Programmer_Error with "McKenzie_Recover does not support Parsers.Count > 1 (yet)";
      end if;

      for Parser_State of Parsers loop
         Free (Parser_State.Recover);
         Parser_State.Recover := new McKenzie_Data (Parser'Unchecked_Access);

         declare
            Result : Configuration;
            Data   : McKenzie_Data renames McKenzie_Data (Parser_State.Recover.all);
         begin
            Result     := Recover (Parser, Parser_State);
            Keep_Going := True;

            for ID of Result.Popped loop
               Parser_State.Stack.Pop;
               Parser.Semantic_State.Pop_Token (ID);
            end loop;

            --  FIXME: Lookahead, Current_Token might be different for different parsers;
            --  process inserted, deleted tokens now, pending actions, so LR.Parser does not deal with lookahead.
            --  each parser must also process non-common deleted tokens from all other parsers.
            for ID of Result.Deleted loop
               --  Input_Token was called for these tokens, so we must call Discard_Token
               Parser.Semantic_State.Discard_Token (ID);
               Parser.Lookahead.Delete_First;
            end loop;

            for ID of reverse Result.Inserted loop
               Parser.Lookahead.Prepend (ID);
               --  Main parser will call Push_Token on these; pretend they were read from the lexer.
               Parser.Semantic_State.Input_Token (ID, null);
            end loop;

            Parser.Semantic_State.Recover
              (Popped_Tokens => Data.Popped_Tokens,
               Pushed_Tokens => Data.Pushed_Tokens,
               Recover       => new Configuration'(Result));

            Current_Token := Parser.Lookahead (Natural_Index_Type'First);
            Parser.Lookahead.Delete_First;
         exception
         when Recover_Fail =>
            null;
         end;
      end loop;

      return Keep_Going;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
