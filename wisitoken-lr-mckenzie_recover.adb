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

with Ada.Exceptions;
with Ada.Task_Identification;
with SAL.Gen_Unbounded_Definite_Queues;
with System.Multiprocessors;
package body WisiToken.LR.McKenzie_Recover is

   --  For protected body entry barriers.
   use all type Ada.Containers.Count_Type;
   use all type SAL.Base_Peek_Type;

   procedure Put
     (Message         : in     String;
      Trace           : in out WisiToken.Trace'Class;
      Parser_Label    : in     Natural;
      Config          : in     Configuration;
      Include_Task_ID : in     Boolean := True)
   is
      --  For debugging output
      use Ada.Containers;

      --  Build a string, call trace.put_line, so output from multiple tasks is not interleaved
      use all type Ada.Strings.Unbounded.Unbounded_String;
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Include_Task_ID then +Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else +"") &
         Integer'Image (Parser_Label) & ": " & Message & ":";
   begin
      Result := Result & Natural'Image (Config.Cost) & ", ";
      if Trace_Parse > Extra then
         Result := Result & Image (Config.Stack, Trace.Descriptor.all, Depth => 4);
      else
         Result := Result & Image (Config.Stack, Trace.Descriptor.all, Depth => 1);
      end if;
      Result := Result & "|";

      if Config.Local_Lookahead.Length = 0 then
         --  We'd like to put the shared lookahead token_id image here, but we
         --  don't want to access Config_Store.
         Result := Result & SAL.Base_Peek_Type'Image (Config.Shared_Lookahead_Index);
      else
         Result := Result & Image (Config.Local_Lookahead, Trace.Descriptor.all) & ":" &
           Int_Image (Integer (Config.Local_Lookahead_Index));
      end if;
      Result := Result & "|" &
        Image (Config.Popped, Trace.Descriptor.all) &
        Image (Config.Pushed, Trace.Descriptor.all) &
        Image (Config.Inserted, Trace.Descriptor.all) &
        Image (Config.Deleted, Trace.Descriptor.all);
      Trace.Put_Line (-Result);
   end Put;

   procedure Put_Line
     (Trace           : in out WisiToken.Trace'Class;
      Parser_Label    : in     Natural;
      Message         : in     String;
      Include_Task_ID : in     Boolean := True)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      Trace.Put_Line
        ((if Include_Task_ID then Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else "") &
           Integer'Image (Parser_Label) & ": " & Message);
   end Put_Line;

   ----------
   --  Protected object specs

   protected type Supervisor (Trace : not null access WisiToken.Trace'Class)
   is
      --  There is only one object of this type, declared in Recover. It
      --  keeps track of which parsers are have succeeded or failed.

      procedure Initialize (Parser_Count : in Ada.Containers.Count_Type);

      procedure Finished (Parser_Label : in Natural; Success : in Boolean);
      --  Report that a parser has failed or succeeded.

      function Success_Count return Natural;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence);
      --  Report a fatal error; abort all processing, make Done
      --  available.

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String);
      --  Available when all parsers have failed or succeeded, or an error
      --  occured.
      --
      --  If Error_ID is not Null_Id, an error occured.

   private
      Active_Parsers  : Ada.Containers.Count_Type; --  Parsers that have neither failed nor succeeded.
      Success_Counter : Natural;
      Fatal_Called    : Boolean;
      Error_ID        : Ada.Exceptions.Exception_Id;
      Error_Message   : Ada.Strings.Unbounded.Unbounded_String;
   end Supervisor;

   protected type Shared_Lookahead (Shared_Parser : not null access LR.Instance'Class)
   is
      --  There is only one object of this type, declared in Recover. It
      --  controls access to the shared lookahead queue.

      function Get_Token (Index : in SAL.Base_Peek_Type) return Base_Token;
      --  Get the token at Index, reading from the lexer if necessary.

   end Shared_Lookahead;

   type Config_Status_Type is (Valid, All_Done, Try_Later);

   protected type Config_Store
     (Super        : not null access Supervisor;
      Trace        : not null access WisiToken.Trace'Class;
      Cost_Limit   : Natural;
      Data         : McKenzie_Access;
      Parser_Label : Natural)
   is
      --  There is one object of this type per parallel parser, declared in
      --  Recover. It controls read/write access to the McKenzie_Data.

      procedure Initialize;

      function Get_Parser_Label return Natural;

      entry Get (Status : out Config_Status_Type; Config : out Configuration);
      --  Get a new configuration to check. Available when there is a
      --  configuration to get.
      --
      --  Status values mean:
      --
      --  Valid - Config is valid, should be checked.
      --
      --  All_Done - Config is not valid; all configs checked, don't call Get again.
      --
      --  Try_Later - Config is not valid; call Get again.

      procedure Success (Config : in Configuration);
      --  Report that Configuration succeeds. This calls Supervisor.Finished.

      procedure Put (Configs : in out Config_Heaps.Heap_Type);
      --  Add Configs to the Config_Heap.

      procedure Fatal;
      --  Some fatal error occured; abort all processing.

   private
      Active_Workers : Integer; -- Worker_Tasks that have done Get but not Put or Success.
      Ready          : Boolean; -- Have at least one solution.
      Done           : Boolean;
   end Config_Store;

   type Config_Store_Access is access Config_Store;
   procedure Free is new Ada.Unchecked_Deallocation (Config_Store, Config_Store_Access);
   type Config_Store_Array is array (Positive_Index_Type range <>) of Config_Store_Access;

   ----------
   --  Protected object bodies

   protected body Supervisor is

      procedure Initialize (Parser_Count : in Ada.Containers.Count_Type)
      is begin
         Active_Parsers  := Parser_Count;
         Success_Counter := 0;
         Fatal_Called    := False;
         Error_ID        := Ada.Exceptions.Null_Id;
      end Initialize;

      procedure Finished (Parser_Label : in Natural; Success : in Boolean)
      is begin
         if Trace_Parse > Extra then
            Put_Line (Trace.all, Parser_Label, "Supervisor: " & (if Success then "succeed" else "fail"));
         end if;
         Active_Parsers := Active_Parsers - 1;
         if Success then
            Success_Counter := Success_Counter + 1;
         end if;
      end Finished;

      function Success_Count return Natural
      is begin
         return Success_Counter;
      end Success_Count;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
      begin
         Active_Parsers := 0;
         Error_ID       := Exception_Identity (E);
         Error_Message  := +Exception_Message (E);
         Trace.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      end Fatal;

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String)
        when Active_Parsers = 0
      is begin
         Error_ID := Supervisor.Error_ID;
         Message  := Error_Message;
         if Trace_Parse > Detail then
            Trace.Put_Line ("Supervisor: Done");
         end if;
      end Done;

   end Supervisor;

   protected body Shared_Lookahead is

      function Get_Token (Index : in SAL.Base_Peek_Type) return Base_Token
      is begin
         if Index > Shared_Parser.Shared_Lookahead.Count then
            Shared_Parser.Shared_Lookahead.Put
              (Next_Grammar_Token (Shared_Parser.Lexer, Shared_Parser.Semantic_State));
         end if;
         return Shared_Parser.Shared_Lookahead.Peek (Index);
      end Get_Token;

   end Shared_Lookahead;

   protected body Config_Store is

      procedure Initialize
      is begin
         Active_Workers := 0;
         Ready          := False;
         Done           := False;

         Data.Results.Clear;
      end Initialize;

      function Get_Parser_Label return Natural
      is begin
         return Config_Store.Parser_Label;
      end Get_Parser_Label;

      entry Get (Status : out Config_Status_Type; Config : out Configuration)
        when Data.Config_Heap.Count > 0 or Done or Ready
      is begin
         if Done then
            Status := All_Done;
            return;
         end if;

         if Data.Config_Heap.Count = 0 then
            --  We can get here if a solution is found very early, when there are
            --  more worker tasks than configs in the heap.
            Status := Try_Later;

         elsif Data.Config_Heap.Min_Key > Cost_Limit then
            --  Worker tasks never reduce the cost of a configuration. There might
            --  be an active worker with a cheaper solution.
            Status := Try_Later;

         elsif Ready then
            if Data.Config_Heap.Min_Key > Data.Results.Min_Key then
               --  Can't get any more solutions with the same cost as existing ones,
               --  unless there are active workers.
               Status := Try_Later;
            else
               Status := Valid;
            end if;
         else
            Status := Valid;
         end if;

         if Status = Valid then
            Config           := Data.Config_Heap.Remove;
            Data.Check_Count := Data.Check_Count + 1;
            Active_Workers   := Active_Workers + 1;

         else
            if Active_Workers = 0 then
               if Trace_Parse > Extra then
                  Put_Line
                    (Trace.all, Parser_Label, "Config_Store: done, " &
                       (if Data.Success then "succeed" else "fail"));
               end if;
               Super.Finished (Parser_Label, Success => Data.Success);
               Done   := True;
               Status := All_Done;
            end if;
         end if;
      end Get;

      procedure Success (Config : in Configuration)
      is begin
         if Trace_Parse > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) &
                 ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Parser_Label, Config);
         end if;

         Data.Success   := True;
         Active_Workers := Active_Workers - 1;

         --  There may be more than one successful config for a parser, so we
         --  keep going. We could get a lot if we always go to the cost limit,
         --  so we stop when we get a successful config that costs more than a
         --  previous one, and only keep the cheapest ones.
         --
         --  We don't set Done here, to keep the termination logic simpler, and
         --  all in one place (in Get).

         if Data.Results.Count = 0 or else Config.Cost <= Data.Results.Min_Key then
            Data.Results.Add (Config);
         else
            Ready := True;
         end if;
      end Success;

      procedure Put (Configs : in out Config_Heaps.Heap_Type)
      is
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count;
      begin
         if Done then
            return;
         end if;

         Active_Workers := Active_Workers - 1;

         loop
            exit when Configs.Count = 0;
            if Trace_Parse > Detail then
               Put ("enqueue", Trace.all, Parser_Label, Configs.Peek);
            end if;
            Data.Config_Heap.Add (Configs.Remove);
            Data.Enqueue_Count := Data.Enqueue_Count + 1;
         end loop;

         if Trace_Parse > Detail then
            Put_Line
              (Trace.all, Parser_Label,
               "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
                 "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
                 "/" & Int_Image (Data.Enqueue_Count) &
                 ", cost:" &
                 (if Data.Config_Heap.Count > 0
                  then Integer'Image (Data.Config_Heap.Min_Key)
                  else " ? ") &
                 ", workers:" & Integer'Image (Active_Workers));
         end if;
      end Put;

      procedure Fatal
      is begin
         Done := True;
      end Fatal;

   end Config_Store;

   ----------
   --  Check code

   procedure Do_Shift
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      McKenzie_Param    : in     McKenzie_Param_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Action            : in     Shift_Action_Rec;
      Inserted_Token    : in     Token_ID)
   is begin
      if Trace_Parse > Extra then
         Put_Line (Trace, Parser_Label, Image (Config.Stack, Trace.Descriptor.all));
         Put_Line (Trace, Parser_Label, Image (Action, Trace.Descriptor.all));
      end if;
      Config.Stack.Push ((Action.State, (Inserted_Token, Null_Buffer_Region, Null_Buffer_Region)));
      Config.Inserted.Append (Inserted_Token);
      Config.Cost := Config.Cost + McKenzie_Param.Insert (Inserted_Token);
      Local_Config_Heap.Add (Config);
   end Do_Shift;

   function Reduce_Stack_1
     (Stack   : in out Parser_Stacks.Stack_Type;
      Action  : in     Reduce_Action_Rec;
      Nonterm :    out Base_Token;
      Lexer   : in     WisiToken.Lexer.Handle)
     return Semantic_Status
   is
      --  Reduce Stack according to Action, calling Action.Check and
      --  returning result, or Ok if null.
   begin
      if Action.Check = null then
         Reduce_Stack (Stack, Action, Nonterm);
         return Ok;
      else
         declare
            Tokens : Base_Token_Arrays.Vector;
         begin
            Reduce_Stack (Stack, Action, Nonterm, Tokens);
            return Action.Check (Lexer, Tokens);
         end;
      end if;
   end Reduce_Stack_1;

   procedure Do_Reduce
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      Table             : in     Parse_Table;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration;
      Action            : in     Reduce_Action_Rec;
      Inserted_Token    : in     Token_ID;
      Lexer             : in     WisiToken.Lexer.Handle)
   is
      --  Perform reduce actions until get to a shift; if all succeed, add
      --  the final configuration to the heap. If any action fails, just
      --  return.

      New_Config_1 : Configuration := Config;
      New_Config_2 : Configuration;
      New_State    : Unknown_State_Index;
      Next_Action  : Parse_Action_Node_Ptr;
      Nonterm      : Base_Token;
   begin
      if Trace_Parse > Extra then
         Put_Line (Trace, Parser_Label, Image (New_Config_1.Stack, Trace.Descriptor.all));
         Put_Line (Trace, Parser_Label, Image (Action, Trace.Descriptor.all));
      end if;

      if Error = Reduce_Stack_1 (New_Config_1.Stack, Action, Nonterm, Lexer) then
         return;
      end if;

      New_State := New_Config_1.Stack.Peek.State;
      New_State := Goto_For (Table, New_State, Action.LHS);

      if New_State = Unknown_State then
         return;
      end if;

      New_Config_1.Stack.Push ((New_State, Nonterm));

      Next_Action := Action_For (Table, New_State, Inserted_Token);
      loop
         New_Config_2 := New_Config_1;

         exit when Next_Action = null;
         case Next_Action.Item.Verb is
         when Shift =>
            Do_Shift
              (Trace, Parser_Label, Table.McKenzie_Param, Local_Config_Heap, New_Config_2,
               Next_Action.Item, Inserted_Token);

         when Reduce =>
            Do_Reduce
              (Trace, Parser_Label, Table, Local_Config_Heap, New_Config_2, Next_Action.Item, Inserted_Token, Lexer);

         when Accept_It | Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
   end Do_Reduce;

   type Check_Item is record
      Config        : Configuration;
      Action        : Parse_Action_Node_Ptr;
      Current_Token : Base_Token;
   end record;

   package Check_Item_Queues is new SAL.Gen_Unbounded_Definite_Queues (Check_Item);

   function Check_One_Item
     (Shared                : not null access Shared_Lookahead;
      Trace                 : in out          WisiToken.Trace'Class;
      Table                 : in              Parse_Table;
      Parser_Label          : in              Natural;
      Check_Item_Queue      : in out          Check_Item_Queues.Queue_Type;
      Enqueue_Count         : in out          Integer;
      Shared_Lookahead_Goal : in              SAL.Peek_Type)
     return Boolean
   is
      --  Perform actions for the first Item in Check_Item_Queue, until
      --  Shared_Lookahead_Goal tokens are consumed. If all actions succeed
      --  return True. If any fail, return False. The modified configuration
      --  is always discarded.
      --
      --  If any actions are a conflict, add the conflict action to
      --  Check_Item_Queue.

      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;

      Item         : constant Check_Item    := Check_Item_Queue.Get;
      Check_Config : Configuration := Item.Config;
      Check_Token  : Base_Token    := Item.Current_Token;

      Action : Parse_Action_Node_Ptr :=
        (if Item.Action = null
         then Action_For (Table, Check_Config.Stack.Peek.State, Check_Token.ID)
         else Item.Action);

      New_State          : Unknown_State_Index;
      Nonterm            : Base_Token;
      Last_Token_Virtual : Boolean := False;
      Keep_Going         : Boolean := True;
   begin
      if Trace_Parse > Detail then
         Put ("check  ", Trace, Parser_Label, Check_Config);
         if Trace_Parse > Extra then
            Put_Line (Trace, "   action " & Image (Action.Item, Descriptor));
         end if;
      end if;

      loop
         if Trace_Parse > Extra then
            Put_Line
              (Trace, Parser_Label, "checking :" & State_Index'Image (Check_Config.Stack.Peek.State) &
                 " : " & Image (Check_Token, Descriptor) &
                 " : " & Image (Action.Item, Descriptor));
         end if;

         if Action.Next /= null then
            if Trace_Parse > Detail then
               Put_Line (Trace, Parser_Label, "checking: enqueue conflict " & Image (Action.Next.Item, Descriptor));
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
               Check_Token :=
                 (Check_Config.Local_Lookahead (Check_Config.Local_Lookahead_Index),
                  Null_Buffer_Region, Null_Buffer_Region);
               Last_Token_Virtual := True;
            else
               if not Last_Token_Virtual then
                  Check_Config.Shared_Lookahead_Index := Check_Config.Shared_Lookahead_Index + 1;
               end if;
               Last_Token_Virtual := False;

               Check_Token := Shared.Get_Token (Check_Config.Shared_Lookahead_Index);
            end if;

         when Reduce =>
            if Error = Reduce_Stack_1 (Check_Config.Stack, Action.Item, Nonterm, Shared.Shared_Parser.Lexer) then
               Keep_Going := False;
            else
               New_State := Check_Config.Stack.Peek.State;
               New_State := Goto_For (Table, New_State, Action.Item.LHS);

               if New_State = Unknown_State then
                  Keep_Going := False;
               else
                  Check_Config.Stack.Push ((New_State, Nonterm));
                  Keep_Going := True;
               end if;
            end if;

         when Error =>
            Keep_Going := False;

         when Accept_It =>
            Keep_Going := True;
         end case;

         exit when not Keep_Going or
           Action.Item.Verb = Accept_It or
           Check_Config.Shared_Lookahead_Index > Shared_Lookahead_Goal;

         Action := Action_For (Table, Check_Config.Stack.Peek.State, Check_Token.ID);
      end loop;

      return Keep_Going;
   end Check_One_Item;

   function Check
     (Shared        : not null access Shared_Lookahead;
      Trace         : in out          WisiToken.Trace'Class;
      Table         : in              Parse_Table;
      Parser_Label  : in              Natural;
      Config        : in              Configuration;
      Current_Token : in              Base_Token)
     return Boolean
   is
      --  Check whether Config is viable; return True if parse will succeed
      --  for Check_Limit tokens.

      Shared_Lookahead_Goal : constant SAL.Peek_Type := Config.Shared_Lookahead_Index +
        SAL.Base_Peek_Type (Config.Deleted.Length) + SAL.Peek_Type (Table.McKenzie_Param.Check_Limit);

      Check_Item_Queue : Check_Item_Queues.Queue_Type; -- Only used for conflicts
      Check_Count      : Integer := 1;
      Enqueue_Count    : Integer := 1;
      Keep_Going       : Boolean;
   begin
      Check_Item_Queue.Clear;
      Check_Item_Queue.Put ((Config, null, Current_Token));

      loop
         Keep_Going := Check_One_Item
           (Shared, Trace, Table, Parser_Label, Check_Item_Queue, Enqueue_Count, Shared_Lookahead_Goal);

         exit when Keep_Going or Check_Item_Queue.Count = 0;
         Check_Count := Check_Count + 1;

         if Trace_Parse > Detail then
            Put_Line (Trace, Parser_Label, "checking: dequeue conflict");
         end if;
      end loop;

      if Trace_Parse > Extra then
         Put_Line
           (Trace, Parser_Label,
            "check enqueue" & Integer'Image (Enqueue_Count) & " check" & Integer'Image (Check_Count) &
              (if Keep_Going
               then " : succeed"
               else " : fail"));
      end if;

      return Keep_Going;
   end Check;

   procedure Process_One
     (Active       : in out          Boolean;
      Config_Store : not null access McKenzie_Recover.Config_Store;
      Shared       : not null access Shared_Lookahead;
      Trace        : in out          WisiToken.Trace'Class;
      Table        : in              Parse_Table)
   is
      use all type Fast_Token_ID_Vectors.Vector;

      Parser_Label   : constant Natural := Config_Store.Get_Parser_Label;
      McKenzie_Param : McKenzie_Param_Type renames Table.McKenzie_Param;
      EOF_ID         : Token_ID renames Trace.Descriptor.EOF_ID;

      Config_Status : Config_Status_Type;
      Config        : Configuration;
      Current_Input : Base_Token;
      New_Config    : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      Action_I          : Action_List_Iterator;
   begin
      Config_Store.Get (Config_Status, Config);

      case Config_Status is
      when Valid =>
         null;
      when All_Done =>
         Active := False;
         return;
      when Try_Later =>
         return;
      end case;

      if Config.Local_Lookahead_Index /= Fast_Token_ID_Vectors.No_Index and
        Config.Local_Lookahead_Index <= Config.Local_Lookahead.Last_Index
      then
         Current_Input :=
           (Config.Local_Lookahead (Config.Local_Lookahead_Index), Null_Buffer_Region, Null_Buffer_Region);
      else
         Current_Input := Shared.Get_Token (Config.Shared_Lookahead_Index);
      end if;

      if Check (Shared, Trace, Table, Parser_Label, Config, Current_Input) then
         Config_Store.Success (Config);
         return;
      end if;

      if Config.Deleted = Fast_Token_ID_Vectors.Empty_Vector and
        Config.Inserted = Fast_Token_ID_Vectors.Empty_Vector and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         --  Try deleting stack top
         declare
            Deleted_Token : Base_Token renames Config.Stack.Peek.Token;
         begin
            New_Config      := Config;
            New_Config.Stack.Pop;
            New_Config.Cost := New_Config.Cost +
              (if Deleted_Token.Byte_Region = Null_Buffer_Region
               then 0
               else McKenzie_Param.Delete (Deleted_Token.ID));

            New_Config.Popped.Append (Deleted_Token.ID);
            if Trace_Parse > Extra then
               Put_Line (Trace, Parser_Label, "pop " & Image (Deleted_Token, Trace.Descriptor.all));
            end if;

            Local_Config_Heap.Add (New_Config);
         end;
      end if;

      if Config.Deleted = Fast_Token_ID_Vectors.Empty_Vector then
         --  Find insertions to try
         Action_I := First_Action (Table.States (Config.Stack.Peek.State));
         loop
            exit when Action_I.Is_Done;
            declare
               ID     : constant Token_ID := Action_I.Symbol;
               Action : Parse_Action_Rec renames Action_I.Action;
            begin
               if ID /= EOF_ID then
                  case Action.Verb is
                  when Shift =>
                     if Trace_Parse > Extra then
                        Put_Line (Trace, "insert " & Image (ID, Trace.Descriptor.all));
                     end if;

                     New_Config := Config;
                     Do_Shift (Trace, Parser_Label, McKenzie_Param, Local_Config_Heap, New_Config, Action, ID);

                  when Reduce =>
                     if Trace_Parse > Extra then
                        Put_Line (Trace, Parser_Label, "try insert " & Image (ID, Trace.Descriptor.all));
                     end if;
                     Do_Reduce
                       (Trace, Parser_Label, Table, Local_Config_Heap, Config, Action, ID, Shared.Shared_Parser.Lexer);

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
      if Config.Local_Lookahead_Index = Fast_Token_ID_Vectors.No_Index or
        Config.Local_Lookahead_Index > Config.Local_Lookahead.Last_Index
      then
         declare
            Deleted_ID : constant Token_ID := Shared.Get_Token (Config.Shared_Lookahead_Index).ID;
            Junk_ID : Token_ID;
            pragma Unreferenced (Junk_ID);
         begin
            if Deleted_ID /= EOF_ID then
               --  can't delete EOF
               New_Config      := Config;
               New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (Deleted_ID);

               New_Config.Deleted.Append (Deleted_ID);
               if Trace_Parse > Extra then
                  Put_Line (Trace, Parser_Label, "delete " & Image (Deleted_ID, Trace.Descriptor.all));
               end if;

               New_Config.Shared_Lookahead_Index := New_Config.Shared_Lookahead_Index + 1;
               Junk_ID := Shared.Get_Token (New_Config.Shared_Lookahead_Index).ID;

               Local_Config_Heap.Add (New_Config);
            end if;
         end;
      end if;

      Config_Store.Put (Local_Config_Heap);
   end Process_One;

   ----------
   --  Top level

   task type Worker_Task
     (Super         : not null access Supervisor;
      Shared        : not null access Shared_Lookahead;
      Config_Stores : not null access McKenzie_Recover.Config_Store_Array;
      Trace         : not null access WisiToken.Trace'Class;
      Table         : not null access Parse_Table)
   is
      entry Start;
      --  Start getting parser/configs to check from Config_Store.

      entry Done;
      --  Available when task is ready to terminate; after this rendezvous,
      --  task discriminants may be freed.

   end Worker_Task;

   task body Worker_Task
   is
      I : Positive_Index_Type := Config_Stores'First;

      Active : array (Config_Stores'First .. Config_Stores'Last) of Boolean := (others => True);
   begin
      accept Start;

      loop
         exit when (for all X of Active => not X);

         Process_One (Active (I), Config_Stores (I), Shared, Trace.all, Table.all);

         if I = Active'Last then
            I := Active'First;
         else
            I := I + 1;
         end if;
      end loop;

      accept Done;
   exception
   when E : others =>
      for Store of Config_Stores.all loop
         Store.Fatal;
      end loop;
      Super.Fatal (E);
   end Worker_Task;

   procedure Enqueue
     (Shared_Parser : in     LR.Instance'Class;
      Parser_Label  : in     Natural;
      Data          : in out McKenzie_Data;
      Config        : in     Configuration)
   is begin
      --  This may only be called from Recover_Init (or procedures it
      --  calls), since it writes to Data without going thru Config_Store.

      --  [1] has a check for duplicate configs here; that only happens with
      --  higher costs, which take too long for our application.
      if Trace_Parse > Detail then
         Put ("enqueue", Shared_Parser.Semantic_State.Trace.all, Parser_Label, Config);
      end if;
      Data.Config_Heap.Add (Config);
      Data.Enqueue_Count := Data.Enqueue_Count + 1;
   end Enqueue;

   procedure Apply_Pattern
     (Pattern       : in     Recover_Pattern_1;
      Shared_Parser : in     LR.Instance'Class;
      Parser_State  : in     Parser_Lists.Parser_State;
      Error_ID      : in     Token_ID;
      Data          : in out McKenzie_Data;
      Root_Config   : in     Configuration)
   is begin
      if Parser_State.Stack.Peek.Token.ID = Pattern.Stack and
        Error_ID = Pattern.Error
      then
         declare
            --  Don't compute Expecting unless we need it.
            Descriptor : WisiToken.Descriptor'Class renames Shared_Parser.Semantic_State.Trace.Descriptor.all;
            Expecting  : constant WisiToken.Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Parser_State.Stack.Peek.State);
         begin
            if Expecting (Pattern.Expecting) and Count (Expecting) = 1 then
               if Trace_Parse > Outline then
                  Shared_Parser.Semantic_State.Trace.Put_Line
                    ("special rule recover_pattern_1 " &
                       Image (Pattern.Stack, Descriptor) & ", " &
                       Image (Pattern.Error, Descriptor) & ", " &
                       Image (Pattern.Expecting, Descriptor) &
                       " matched.");
               end if;

               declare
                  Config : Configuration := Root_Config;
               begin
                  Config.Local_Lookahead.Prepend (Pattern.Stack);
                  Config.Local_Lookahead.Prepend (Pattern.Error);
                  Config.Local_Lookahead.Prepend (Pattern.Expecting);
                  Config.Local_Lookahead_Index := 1;

                  Enqueue (Shared_Parser, Parser_Lists.Label (Parser_State), Data, Config);
               end;
            end if;
         end;
      end if;
   end Apply_Pattern;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Pattern_2;
      Shared_Parser       : in     LR.Instance'Class;
      Parser_State : in     Parser_Lists.Parser_State;
      Error_ID     : in     Token_ID;
      Data         : in out McKenzie_Data;
      Root_Config  : in     Configuration)
   is begin
      if Parser_State.Stack.Peek.Token.ID = Pattern.Stack and
        Error_ID = Pattern.Error
      then
         declare
            --  Don't compute Expecting unless we need it.
            Descriptor : WisiToken.Descriptor'Class renames Shared_Parser.Semantic_State.Trace.Descriptor.all;
            Expecting  : constant WisiToken.Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Parser_State.Stack.Peek.State);
         begin
            if Expecting (Pattern.Expecting) and Count (Expecting) = 1 then
               if Trace_Parse > Outline then
                  Shared_Parser.Semantic_State.Trace.Put_Line
                    ("special rule recover_pattern_2 " &
                       Image (Pattern.Stack, Descriptor) & ", " &
                       Image (Pattern.Error, Descriptor) & ", " &
                       Image (Pattern.Expecting, Descriptor) & ", " &
                       Image (Pattern.Insert, Descriptor) &
                       " matched.");
               end if;

               declare
                  Config : Configuration := Root_Config;
               begin
                  Config.Local_Lookahead.Prepend (Pattern.Stack);
                  Config.Local_Lookahead.Prepend (Pattern.Insert);
                  Config.Local_Lookahead.Prepend (Pattern.Expecting);
                  Config.Local_Lookahead_Index := 1;

                  Enqueue (Shared_Parser, Parser_Lists.Label (Parser_State), Data, Config);
               end;
            end if;
         end;
      end if;
   end Apply_Pattern;

   procedure Patterns
     (Shared_Parser : in out LR.Instance'Class;
      Parser_State  : in     Parser_Lists.Parser_State;
      Data          : in out McKenzie_Data;
      Root_Config   : in     Configuration)
   is
      Param    : McKenzie_Param_Type renames Shared_Parser.Table.McKenzie_Param;
      Error_ID : Token_ID renames Shared_Parser.Shared_Lookahead.Peek (Parser_State.Shared_Lookahead_Index).ID;
   begin
      for Pattern of Param.Patterns loop
         if Pattern in Recover_Pattern_1'Class then
            Apply_Pattern (Recover_Pattern_1 (Pattern), Shared_Parser, Parser_State, Error_ID, Data, Root_Config);

         elsif Pattern in Recover_Pattern_2'Class then
            Apply_Pattern (Recover_Pattern_2 (Pattern), Shared_Parser, Parser_State, Error_ID, Data, Root_Config);
         end if;
      end loop;
   end Patterns;

   procedure Recover_Init
     (Shared_Parser : in out LR.Instance'Class;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      Data  : McKenzie_Data renames Parser_State.Recover;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;
   begin
      if Trace_Parse > Outline then
         Trace.New_Line;
         Trace.Put_Line ("parser" & Integer'Image (Parser_State.Label) & ":");
      end if;

      if Parser_State.Local_Lookahead.Length > 0 then
         --  Previous error recovery resume not finished.
         raise Programmer_Error;
      end if;

      --  This is done here, not in Config_Stores.Initialize, becuase
      --  Patterns does not use Config_Stores. FIXME: change Patterns to use
      --  Config_Stores.
      Data.Config_Heap.Clear;
      Data.Enqueue_Count := 0;
      Data.Check_Count   := 0;
      Data.Success       := False;

      --  The special rules are not guaranteed to work when matched, so
      --  always queue the original error condition.
      declare
         Orig : Configuration := Default_Configuration;
      begin
         Orig.Stack := Parser_State.Stack;

         --  Parser_State.Local_Lookahead must be empty (else we would not get
         --  here). Therefore Parser_State current token is in
         --  Shared_Parser.Shared_Lookahead(Parser_State.Shared_Lookahead_Index)
         Orig.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index;
         Enqueue (Shared_Parser, Parser_Lists.Label (Parser_State), Data, Orig);

         Patterns (Shared_Parser, Parser_State, Data, Orig);
      end;
   end Recover_Init;

   function Recover
     (Shared_Parser : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List)
     return Boolean
   is
      use all type System.Multiprocessors.CPU_Range;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;

      Super         : aliased Supervisor (Trace'Access);
      Shared        : aliased Shared_Lookahead (Shared_Parser'Access);
      Config_Stores : aliased Config_Store_Array := (1 .. Parsers.Count => null);

      Worker_Tasks   : array (1 .. System.Multiprocessors.Number_Of_CPUs - 1) of Worker_Task
        (Super'Access, Shared'Access, Config_Stores'Access, Shared_Parser.Semantic_State.Trace,
         Shared_Parser.Table.all'Access);
      --  Keep one CPU free for this main task, and the user.

      procedure Cleanup
      is begin
         for I in Config_Stores'Range loop
            Free (Config_Stores (I));
         end loop;

         for I in Worker_Tasks'Range loop
            if Worker_Tasks (I)'Callable then
               abort Worker_Tasks (I);
            end if;
         end loop;
      end Cleanup;

   begin
      if Trace_Parse > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
         if Trace_Parse > Extra then
            Trace.New_Line;
            Trace.Put ("shared_lookahead: ");
            Trace.Put (Image (Shared_Parser.Shared_Lookahead, Trace.Descriptor.all));
            Trace.New_Line;
            Shared_Parser.Semantic_State.Put;
         end if;
      end if;

      for Parser_State of Parsers loop
         Recover_Init (Shared_Parser, Parser_State);
      end loop;

      Super.Initialize (Parsers.Count);

      declare
         use Parser_Lists;
         Cur : Cursor                    := Parsers.First;
         I   : Ada.Containers.Count_Type := Config_Stores'First;
      begin
         loop
            Config_Stores (I) := new Config_Store
              (Super'Unchecked_Access, Trace'Access, Shared_Parser.Table.McKenzie_Param.Cost_Limit,
               Cur.McKenzie_Ref, Cur.Label);
            Config_Stores (I).Initialize;
            I := I + 1;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      if Trace_Parse > Outline then
         Trace.New_Line;
         Trace.Put_Line (System.Multiprocessors.CPU_Range'Image (Worker_Tasks'Last) & " parallel tasks");
      end if;

      for I in Worker_Tasks'Range loop
         Worker_Tasks (I).Start;
      end loop;

      declare
         use Ada.Exceptions;
         ID : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Super.Done (ID, Message); -- Wait for all parsers to fail or succeed
         if ID /= Null_Id then
            Raise_Exception (ID, -Message);
         end if;
      end;

      --  Ensure all tasks terminate before proceeding; otherwise local
      --  variables disappear while task is still trying to access them.
      for I in Worker_Tasks'Range loop
         if Worker_Tasks (I)'Callable then
            Worker_Tasks (I).Done;
         end if;
      end loop;

      --  Adjust parser state for each successful recovery.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course.

      --  Spawn new parsers for multiple solutions
      declare
         use Parser_Lists;
         Cur : Cursor := Parsers.First;
      begin
         loop
            declare
               Data : McKenzie_Data renames State_Ref (Cur).Recover;
            begin
               if Data.Success then
                  if Trace_Parse > Outline then
                     Trace.Put_Line
                       (Integer'Image (Label (Cur)) &
                          ": succeed" & SAL.Base_Peek_Type'Image (Data.Results.Count) &
                          ", enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost: " & Integer'Image (Data.Results.Min_Key));
                  end if;

                  --  Restore pre-error parser verb before spawning, just for
                  --  simplicity.
                  Cur.Set_Verb (Cur.Prev_Verb);

                  if Data.Results.Count > 1 then
                     if Parsers.Count + Ada.Containers.Count_Type (Data.Results.Count) > Shared_Parser.Max_Parallel then
                        raise Parse_Error with Error_Message
                          ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                           ": too many parallel parsers required in grammar state" &
                             State_Index'Image (Cur.State_Ref.Stack.Peek.State) &
                             "; simplify grammar, or increase max-parallel (" &
                             Ada.Containers.Count_Type'Image (Shared_Parser.Max_Parallel) & ")");
                     end if;

                     for I in 1 .. Data.Results.Count - 1 loop
                        Parsers.Prepend_Copy (Cur); --  does not copy recover
                        if Trace_Parse > Outline then
                           declare
                              Msg : constant String :=
                                "spawn parser" & Integer'Image (Parsers.First.Label) & " from " &
                                Int_Image (Cur.Label) & " (" & Int_Image (1 + Integer (Parsers.Count)) & " active)";
                           begin
                              if Trace_Parse > Detail then
                                 Put (Msg, Trace, 0, Data.Results.Peek, Include_Task_ID => False);
                              else
                                 Trace.Put_Line (Msg);
                              end if;
                           end;
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                        Shared_Parser.Semantic_State.Spawn (Cur.Label, Parsers.First.Label);
                     end loop;
                  end if;

               else
                  if Trace_Parse > Outline then
                     Trace.Put_Line
                       (Integer'Image (Cur.Label) &
                          ": fail, enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost_limit: " & Integer'Image (Shared_Parser.Table.McKenzie_Param.Cost_Limit));
                  end if;
               end if;

            end;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      for Parser_State of Parsers loop
         if Parser_State.Recover.Success then
            declare
               use Parser_Lists;
               Data   : McKenzie_Data renames Parser_State.Recover;
               Result : Configuration renames Data.Results.Peek;
            begin
               if Parsers.Count > 1 then
                  for ID of Result.Popped loop
                     Parser_State.Stack.Pop;
                     Pend (Parser_State, (Discard_Stack, ID), Trace);
                  end loop;

                  for I in 1 .. Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Result.Pushed.Peek (I);
                     begin
                        Parser_State.Stack.Push (Item);
                        Pend (Parser_State, (Virtual_To_Lookahead, Item.Token), Trace);
                        Pend (Parser_State, (Push_Current, Item.Token), Trace);
                     end;
                  end loop;

                  for ID of Result.Deleted loop
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;
                     Pend (Parser_State, (Discard_Lookahead, ID), Trace);
                  end loop;

                  for ID of reverse Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head ((ID, Null_Buffer_Region, Null_Buffer_Region));
                  end loop;

                  for ID of reverse Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head ((ID, Null_Buffer_Region, Null_Buffer_Region));
                  end loop;

                  Pend
                    (Parser_State,
                     (Verb    => Parser_Lists.Recover,
                      Recover => new Configuration'(Result)),
                     Trace);
               else
                  --  Only one parser

                  for ID of Result.Popped loop
                     Parser_State.Stack.Pop;
                     Shared_Parser.Semantic_State.Discard_Stack (ID);
                  end loop;

                  for I in 1 .. Result.Pushed.Depth loop
                     declare
                        Item : constant Parser_Stack_Item := Result.Pushed.Peek (I);
                     begin
                        Parser_State.Stack.Push (Item);
                        Shared_Parser.Semantic_State.Virtual_To_Lookahead (Item.Token);
                        Shared_Parser.Semantic_State.Push_Current (Item.Token);
                     end;
                  end loop;

                  for ID of Result.Deleted loop
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;

                     Shared_Parser.Semantic_State.Discard_Lookahead (ID);
                  end loop;

                  --  We use Parser_State.Local_Lookahead even when there is only one
                  --  parser, so main loop knows these are virtual tokens.
                  for ID of reverse Result.Local_Lookahead loop
                     Parser_State.Local_Lookahead.Add_To_Head ((ID, Null_Buffer_Region, Null_Buffer_Region));
                  end loop;

                  for ID of reverse Result.Inserted loop
                     Parser_State.Local_Lookahead.Add_To_Head ((ID, Null_Buffer_Region, Null_Buffer_Region));
                  end loop;

                  Shared_Parser.Semantic_State.Recover (Parser_State.Label, Result);
               end if;

               if Parser_State.Local_Lookahead.Count > 0 then
                  Parser_State.Current_Token := Parser_State.Local_Lookahead.Get;
                  Parser_State.Current_Token_Is_Virtual := True;
                  if Parsers.Count > 1 then
                     Pend (Parser_State, (Virtual_To_Lookahead, Parser_State.Current_Token), Trace);
                  else
                     Shared_Parser.Semantic_State.Virtual_To_Lookahead (Parser_State.Current_Token);
                  end if;
               else
                  Parser_State.Current_Token := Shared_Parser.Shared_Lookahead.Peek
                    (Parser_State.Shared_Lookahead_Index);
                  Parser_State.Current_Token_Is_Virtual := False;
               end if;
            end;
         end if;
      end loop;

      if Trace_Parse > Extra then
         Shared_Parser.Semantic_State.Put;
      end if;

      Cleanup;

      return Super.Success_Count > 0;

   exception
   when others =>
      Cleanup;
      raise;
   end Recover;

end WisiToken.LR.McKenzie_Recover;
