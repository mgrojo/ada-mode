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

   Insert_Cost : constant Float := 1.0;
   Delete_Cost : constant Float := 1.0;
   --  FIXME: make insert/delete costs per-language parameters?

   function Order_Config (A, B : in Configuration) return Boolean
   is begin
      --  FIXME: prefer insertions over deletions, or use cost for that?
      return A.Cost > B.Cost;
   end Order_Config;

   package Config_Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Configuration);
   package Config_Queues is new SAL.Gen_Unbounded_Definite_Queues
     (Element_Type     => Configuration,
      Queue_Interfaces => Config_Queue_Interfaces);

   type McKenzie_Data (Table : Parse_Table_Ptr) is new Recover_Data with record

      Queue : Config_Queues.Queue_Type;
   end record;

   procedure Clear_Queue (Data : in out McKenzie_Data)
   is begin
      Data.Queue.Clear;
   end Clear_Queue;

   procedure Enqueue (Data : in out McKenzie_Data; Config : in Configuration)
   is begin
      --  FIXME: prune duplicate/higher cost configs
      Data.Queue.Add (Config, Order_Config'Access);
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
      New_State : State_Index;

      Next_Action : Parse_Action_Node_Ptr;
   begin
      for I in 1 .. Action.Token_Count loop
         New_State := New_Config.Stack.Pop;
      end loop;

      New_State := Goto_For (Data.Table.all, New_State, Action.LHS);

      Next_Action := Action_For (Data.Table.all, New_State, Inserted_Token);
      loop
         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            New_Config.Stack.Push (New_State);
            New_Config.Inserted.Append (Inserted_Token);
            New_Config.Cost := New_Config.Cost + Insert_Cost;
            Enqueue (Data, New_Config);

         when Reduce =>
            Do_Reduce (Data, New_Config, Next_Action.Item, Inserted_Token);

         when Accept_It | Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
   end Do_Reduce;

   function Recover
     (Parser : in out LR.Instance'Class;
      Cursor : in     Parser_Lists.Cursor)
     return Configuration
      --  Raises Recover_Fail or returns recover Config
   is
      Data   : McKenzie_Data renames McKenzie_Data (Cursor.Recover_Ref.Element.all);
      Action : Parse_Action_Node_Ptr;
   begin
      Clear_Queue (Data);
      Enqueue
        (Data,
         (Stack           => Parser_Lists.Copy_Stack (Cursor),
          Lookahead_Index => Positive_Index_Type'First,
          Inserted        => Token_Arrays.Empty_Vector,
          Deleted         => Token_Arrays.Empty_Vector,
          Cost            => 0.0));
      loop
         exit when Data.Queue.Is_Empty;

         --  FIXME: terminate on max insert/delete/queue size?

         declare
            use all type Token_Array;
            use all type Ada.Containers.Count_Type;
            Config     : constant Configuration := Delete_Min (Data);
            New_Config : Configuration;
         begin
            if Action_For (Data.Table.all, Config.Stack.Peek, Parser.Lookahead (Config.Lookahead_Index))
              .Item.Verb in Shift | Reduce
            then
               --  FIXME: require three good tokens
               return Config;
            end if;

            if Config.Deleted = Empty_Token_Array then
               --  Find insertions to try
               for ID in Data.Table.First_Terminal .. Data.Table.Last_Terminal loop
                  Action := Action_For (Data.Table.all, Config.Stack.Peek, ID);
                  loop
                     exit when Action = null;
                     case Action.Item.Verb is
                     when Shift =>
                        New_Config := Config;
                        New_Config.Stack.Push (Action.Item.State);
                        New_Config.Inserted.Append (ID);
                        New_Config.Cost := New_Config.Cost + Insert_Cost;
                        Enqueue (Data, New_Config);

                     when Reduce =>
                        Do_Reduce (Data, Config, Action.Item, ID);

                     when Accept_It | Error =>
                        null;
                     end case;
                     Action := Action.Next;
                  end loop;
               end loop;
            end if;

            --  Try a deletion
            New_Config := Config;

            New_Config.Deleted.Append (Parser.Lookahead (New_Config.Lookahead_Index));

            if New_Config.Lookahead_Index = Parser.Lookahead.Last_Index then
               Parser.Lookahead.Append (Parser.Lexer.Find_Next);
            end if;
            New_Config.Lookahead_Index := New_Config.Lookahead_Index + 1;
            New_Config.Cost := New_Config.Cost + Delete_Cost;
            Enqueue (Data, New_Config);

         end;
      end loop;
      raise Recover_Fail;
   end Recover;

   function Recover
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going : Boolean := False;
   begin
      Keep_Going := False;

      Parser.Lookahead.Append (Current_Token);

      for I in Parsers.Iterate loop
         declare
            Cursor : constant Parser_Lists.Cursor := Parser_Lists.To_Cursor (Parsers, I);
         begin
            Cursor.Set_Recover (new McKenzie_Data (Parser.Table));
         end;
      end loop;

      for I in Parsers.Iterate loop
         declare
            Result : Configuration;
         begin
            Result := Recover (Parser, Parser_Lists.To_Cursor (Parsers, I));

            --  FIXME: process result; adjust semantic_state, drop deleted from lookahead

            Current_Token := Parser.Lookahead (Positive_Index_Type'First);
            Parser.Lookahead.Delete_First;
         exception
         when Recover_Fail =>
            null;
         end;
      end loop;

      if Trace_Parse > 0 then
         if not Keep_Going then
            Trace.Put_Line ("recover: fail");
         end if;
      end if;
      return Keep_Going;
   end Recover;

end WisiToken.Parser.LR.McKenzie_Recover;
