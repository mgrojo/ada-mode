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

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (GPL);

with SAL.Gen_Queue_Interfaces;
with SAL.Gen_Unbounded_Definite_Queues;
package body WisiToken.Parser.LR.McKenzie_Recover is

   Recover_Fail : exception;

   type Configuration is record
      Stack           : State_Stacks.Stack_Type;
      Lookahead_Index : Ada.Containers.Count_Type; -- index into parser.lookahead for next input token
      Inserted        : Token_Arrays.Vector;
      Deleted         : Token_Arrays.Vector;
      Cost            : Float := 0.0;
   end record;

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
         Trace.Put (Image (Config.Stack));
      else
         Put (Trace, Unknown_State_Index'Image (Config.Stack.Peek));
      end if;
      Trace.Put (" ");
      Put (Trace, Parser.Lookahead (Config.Lookahead_Index));
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

   procedure Do_Reduce
     (Data           : in out McKenzie_Data;
      Config         : in     Configuration;
      Action         : in     Reduce_Action_Rec;
      Inserted_Token : in     Token_ID)
   is
      New_Config : Configuration := Config;
      New_State  : Unknown_State_Index;

      Next_Action : Parse_Action_Node_Ptr;
   begin
      if Trace_Parse > 2 then
         Data.Parser.Semantic_State.Trace.Put (Image (New_Config.Stack));
         Data.Parser.Semantic_State.Trace.New_Line;
         Put (Data.Parser.Semantic_State.Trace.all, Action);
         Data.Parser.Semantic_State.Trace.New_Line;
      end if;

      for I in 1 .. Action.Token_Count loop
         New_State := New_Config.Stack.Pop;
      end loop;
      New_State := New_Config.Stack.Peek;
      New_State := Goto_For (Data.Parser.Table.all, New_State, Action.LHS);

      if New_State = Unknown_State then
         return;
      end if;

      Next_Action := Action_For (Data.Parser.Table.all, New_State, Inserted_Token);
      loop
         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            New_Config.Stack.Push (New_State);
            New_Config.Stack.Push (Next_Action.Item.State);
            New_Config.Inserted.Append (Inserted_Token);
            New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Insert (Inserted_Token);
            Enqueue (Data, New_Config);

         when Reduce =>
            New_Config.Stack.Push (New_State);
            Do_Reduce (Data, New_Config, Next_Action.Item, Inserted_Token);

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
         New_State := Config.Stack.Pop;
      end loop;
      New_State := Config.Stack.Peek;
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
            Config.Stack.Push (New_State);
            if Check_Reduce (Data, Config, Next_Action.Item, Current_Token) then
               return True;
            end if;

         when Accept_It | Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
      return False;
   end Check_Reduce;

   function Check (Data : in McKenzie_Data; Config : in Configuration; Current_Token : in Token_ID) return Boolean
   is
      --  Return True if Config allows parsing to continue

      Action : Parse_Action_Node_Ptr := Action_For (Data.Parser.Table.all, Config.Stack.Peek, Current_Token);
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
            raise Programmer_Error;
         end case;

         Action := Action.Next;
      end loop;
      return False;
   end Check;

   function Recover
     (Parser : in out LR.Instance'Class;
      Cursor : in     Parser_Lists.Cursor)
     return Configuration
      --  Raises Recover_Fail or returns recover Config
   is
      Data   : McKenzie_Data renames McKenzie_Data (Cursor.Recover_Ref.Element.all);
      Action : Parse_Action_Node_Ptr;

      EOF_ID : Token_ID renames Parser.Semantic_State.Trace.Descriptor.EOF_ID;

      Trace : WisiToken.Trace'Class renames Data.Parser.Semantic_State.Trace.all;

      procedure Trace_Result
      is begin
         Trace.Put_Line
           ("mckenzie enqueue" & Integer'Image (Data.Enqueue_Count) &
              ", check " & Integer'Image (Data.Check_Count));
      end Trace_Result;

   begin
      if Trace_Parse > 1 then
         Trace.New_Line;
      end if;

      Clear_Queue (Data);
      Enqueue
        (Data,
         (Stack           => Parser_Lists.Copy_Stack (Cursor),
          Lookahead_Index => Positive_Index_Type'First,
          Inserted        => Token_Arrays.Empty_Vector,
          Deleted         => Token_Arrays.Empty_Vector,
          Cost            => 0.0));
      loop
         exit when Data.Queue.Is_Empty or Data.Enqueue_Count > Data.Parser.Table.McKenzie.Enqueue_Limit;

         declare
            use all type Token_Array;
            use all type Ada.Containers.Count_Type;

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
                  Trace_Result;
               end if;
               return Config;
            end if;

            if Config.Deleted = Empty_Token_Array then
               --  Find insertions to try
               for ID in Data.Parser.Table.First_Terminal .. Data.Parser.Table.Last_Terminal loop
                  if ID /= EOF_ID then
                     Action := Action_For (Data.Parser.Table.all, Config.Stack.Peek, ID);
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
                           New_Config.Stack.Push (Action.Item.State);
                           New_Config.Inserted.Append (ID);
                           New_Config.Cost := New_Config.Cost + Data.Parser.Table.McKenzie.Insert (ID);
                           Enqueue (Data, New_Config);

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

            --  Try a deletion
            declare
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
         Trace_Result;
      end if;
      raise Recover_Fail;
   end Recover;

   function Recover
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      Keep_Going : Boolean := False;
   begin
      Parser.Lookahead.Append (Current_Token);

      if Parsers.Count > 1 then
         --  See FIXME below
         raise Programmer_Error with "McKenzie_Recover does not support Parsers.Count > 1 (yet)";
      end if;

      for I in Parsers.Iterate loop
         declare
            Cursor : constant Parser_Lists.Cursor := Parser_Lists.To_Cursor (Parsers, I);
         begin
            Cursor.Set_Recover (new McKenzie_Data (Parser'Unchecked_Access));
         end;
      end loop;

      for I in Parsers.Iterate loop
         declare
            use all type Ada.Containers.Count_Type;
            Result : Configuration;
         begin
            Result     := Recover (Parser, Parser_Lists.To_Cursor (Parsers, I));
            Keep_Going := True;

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
              (Popped_Tokens => WisiToken.Token.List.Null_List,
               Pushed_Tokens => WisiToken.Token.List.Null_List);

            Current_Token := Parser.Lookahead (Positive_Index_Type'First);
            Parser.Lookahead.Delete_First;
         exception
         when Recover_Fail =>
            null;
         end;
      end loop;

      return Keep_Going;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
