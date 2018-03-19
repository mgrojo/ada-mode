--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2018 Stephen Leake All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Task_Identification;
with GNAT.Traceback.Symbolic;
with System.Multiprocessors;
with WisiToken.LR.Parser_Lists;
package body WisiToken.LR.McKenzie_Recover is

   procedure Put
     (Message         : in     String;
      Trace           : in out WisiToken.Trace'Class;
      Parser_Label    : in     Natural;
      Terminals       : in     Base_Token_Arrays.Vector;
      Config          : in     Configuration;
      Include_Task_ID : in     Boolean := True)
   is
      --  For debugging output

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).
      use all type Ada.Containers.Count_Type;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type SAL.Base_Peek_Type;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Include_Task_ID then +Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else +"") &
        Integer'Image (Parser_Label) & ": " &
        (if Message'Length > 0 then Message & ":" else "");
   begin
      Result := Result & Natural'Image (Config.Cost) & ", ";
      Result := Result & Image (Config.Stack, Trace.Descriptor.all, Depth => 1);

      if Config.Current_Inserted = No_Inserted then
         Result := Result & "|" & Image (Config.Current_Shared_Token, Terminals, Trace.Descriptor.all) & "|";
      else
         Result := Result & "/" & Image (Config.Current_Inserted, Config.Inserted, Trace.Descriptor.all) & "/";
      end if;

      Result := Result & Image (Config.Ops, Trace.Descriptor.all);
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
   --  Protected object specs.
   --
   --  Tasking design requirements:
   --
   --  1) For each parse_state, find all solutions of the same lowest
   --  cost.
   --
   --  2) use as many CPUs as available as fully as possible.
   --
   --  3) avoid
   --     a) busy waits
   --     b) race conditions
   --     c) deadlocks.
   --
   --  For 2), we use worker_tasks to perform the check computations on
   --  each configuration. We allocate N - 1 worker_tasks, where N is the
   --  number of available CPUs, saving one CPU for Supervisor and the
   --  foreground IDE.
   --
   --  For 1), worker_tasks always get the lowest cost configuration
   --  available. However, some active worker_task may have a lower cost
   --  configuration that it has not yet delivered to Supervisor.
   --  Therefore we always wait until all current active worker_tasks
   --  deliver their results before deciding we are done.
   --
   --  For 3a) we have one Supervisor protected object that controls
   --  access to all Parse_States and configurations, and a
   --  Shared_Lookahead protected object that controls access to the
   --  Shared_Parser lookahead and lexer.
   --
   --  It is tempting to try to reduce contention for Supervisor by
   --  having one protected object per parser, but that requires the
   --  worker tasks to busy loop checking all the parsers.
   --
   --  There is still a race condition on Success; the solutions can be
   --  delivered in different orders on different runs. This matters
   --  because each solution results in a successful parse, possibly with
   --  different actions (different indentation computed, for example).
   --  Which solution finally succeeds depends on which are terminated
   --  due to identical parser stacks, which in turn depends on the order
   --  they were delivered. See ada-mode/tests/ada_mode-interactive_2.adb
   --  for an example.

   type Config_Status is (Valid, All_Done);
   type Parser_Status is (Active, Ready, Success, Fail);
   type Parser_Status_Array is array (SAL.Peek_Type range <>) of Parser_Status;
   type Parser_Natural_Array is array (SAL.Peek_Type range <>) of Natural;

   type Parser_State_Array is array (SAL.Peek_Type range <>) of Parser_Lists.State_Access;
   --  Index is same as Parser_Status.

   protected type Supervisor
     (Trace        : not null access WisiToken.Trace'Class;
      Parsers      : not null access Parser_Lists.List;
      Terminals    : not null access Base_Token_Arrays.Vector;
      Cost_Limit   : Natural;
      Parser_Count : SAL.Peek_Type)
   is
      --  There is only one object of this type, declared in Recover.

      procedure Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status);
      --  Get a new configuration to check. Available when there is a
      --  configuration to get, or when all configs have been checked.
      --
      --  Status values mean:
      --
      --  Valid - Parser_Index, Config are valid, should be checked.
      --
      --  All_Done - Parser_Index, Config are not valid; all configs checked.

      procedure Success (Parser_Index : in SAL.Peek_Type; Config : in Configuration);
      --  Report that Configuration succeeds for Parser_Label.

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type);
      --  Add Configs to the McKenzie_Data Config_Heap for Parser_Label

      function Recover_Result return Recover_Status;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence);
      --  Report a fatal error; abort all processing, make Done
      --  available.

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String);
      --  Available when all parsers have failed or succeeded, or an error
      --  occured.
      --
      --  If Error_ID is not Null_Id, an error occured.

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type;
      function Label (Parser_Index : in SAL.Peek_Type) return Natural;

   private
      Active_Workers : Parser_Natural_Array (1 .. Parser_Count);
      --  Worker_Tasks for each Parser that have done Get but not Put or
      --  Success.

      All_Parsers_Done : Boolean;
      Success_Counter  : Natural;
      Fatal_Called     : Boolean;
      Result           : Recover_Status;
      Error_ID         : Ada.Exceptions.Exception_Id;
      Error_Message    : Ada.Strings.Unbounded.Unbounded_String;
      Parser_Status    : Parser_Status_Array (1 .. Parser_Count);
      Parser_States    : Parser_State_Array (1 .. Parser_Count);
      Parser_Labels    : Parser_Natural_Array (1 .. Parser_Count); -- For Trace
   end Supervisor;

   protected type Shared_Lookahead (Shared_Parser : not null access LR.Parser.Parser)
   is
      --  There is only one object of this type, declared in Recover. It
      --  controls access to Get_Next_Token and direct access to shared
      --  Terminals.

      function Get_Token (Index : in Token_Index) return Token_Index;
      --  Return Index, after assuring there is a token in shared Terminals
      --  there, reading from the lexer if necessary.

      function Token (Index : in Token_Index) return Base_Token;
      --  Return Shared_Parser.Terminals (Index).

   end Shared_Lookahead;

   ----------
   --  Protected object bodies

   function Get_Barrier
     (Parsers        : not null access Parser_Lists.List;
      Parser_Status  : in              Parser_Status_Array;
      Parser_States  : in              Parser_State_Array;
      Active_Workers : in              Parser_Natural_Array;
      Cost_Limit     : in              Natural)
     return Boolean
   is
      use all type SAL.Base_Peek_Type;
      Done_Count : SAL.Base_Peek_Type := 0;
   begin
      --  Return True if all parsers are done, or if any parser has a config
      --  available to check.
      for I in 1 .. Parsers.Count loop
         case Parser_Status (I) is
         when Active =>
            if Parser_States (I).Recover.Config_Heap.Count > 0 then
               if Parser_States (I).Recover.Config_Heap.Min_Key <= Cost_Limit then
                  return True;
               else
                  if Active_Workers (I) = 0 then
                     --  fail; remaining configs exceed cost limit
                     Done_Count := Done_Count + 1;
                  end if;
               end if;
            else
               if Active_Workers (I) = 0 then
                  --  fail; no configs left to check (rarely happens with real
                  --  languages).
                  Done_Count := Done_Count + 1;
               end if;
            end if;

         when Ready =>
            if Parser_States (I).Recover.Config_Heap.Count > 0 and then
              Parser_States (I).Recover.Config_Heap.Min_Key <= Parser_States (I).Recover.Results.Min_Key
            then
               --  Still more to check.
               return True;

            elsif Active_Workers (I) = 0 then
               Done_Count := Done_Count + 1;
            end if;

         when Success | Fail =>
            Done_Count := Done_Count + 1;
         end case;
      end loop;

      return Done_Count = Parsers.Count;
   end Get_Barrier;

   protected body Supervisor is

      procedure Initialize
      is
         use all type SAL.Base_Peek_Type;
         Index : SAL.Peek_Type := 1;
      begin
         All_Parsers_Done := False;
         Active_Workers   := (others => 0);
         Success_Counter  := 0;
         Fatal_Called     := False;
         Result           := Fail;
         Error_ID         := Ada.Exceptions.Null_Id;

         for I in Parsers.Iterate loop
            if Parsers.Reference (I).Local_Lookahead.Length > 0 then
               --  Previous error recovery resume not finished.
               raise Programmer_Error;
            end if;

            Parser_Status (Index) := Active;
            Parser_States (Index) := Parser_Lists.Persistent_State_Ref (I);
            Parser_Labels (Index) := Parsers.Constant_Reference (I).Label;

            declare
               Data : McKenzie_Data renames Parsers.Reference (I).Recover;
            begin
               Data.Config_Heap.Clear;
               Data.Results.Clear;
               Data.Enqueue_Count := 0;
               Data.Check_Count   := 0;
               Data.Success       := False;
            end;

            Index := Index + 1;
         end loop;
      end Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status)
        when (Fatal_Called or All_Parsers_Done) or else
          Get_Barrier (Parsers, Parser_Status, Parser_States, Active_Workers, Cost_Limit)
      is
         use all type SAL.Base_Peek_Type;
         Done_Count : SAL.Base_Peek_Type := 0;

         procedure Set_Outputs (I : in SAL.Peek_Type)
         is begin
            Parser_Index := I;
            Config       := Parser_States (I).Recover.Config_Heap.Remove;
            Status       := Valid;

            Parser_States (I).Recover.Check_Count := Parser_States (I).Recover.Check_Count + 1;

            Active_Workers (I) := Active_Workers (I) + 1;
         end Set_Outputs;

         procedure Set_All_Done
         is begin
            Parser_Index := SAL.Base_Peek_Type'First;
            Config       := (others => <>);
            Status       := All_Done;
         end Set_All_Done;

      begin
         if Fatal_Called or All_Parsers_Done then
            Set_All_Done;
            return;
         end if;

         --  Same logic as in Get_Barrier, but different actions.
         for I in 1 .. Parsers.Count loop
            case Parser_Status (I) is
            when Active =>
               if Parser_States (I).Recover.Config_Heap.Count > 0 then
                  if Parser_States (I).Recover.Config_Heap.Min_Key <= Cost_Limit then

                     Set_Outputs (I);
                     return;
                  else
                     if Active_Workers (I) = 0 then
                        if Trace_McKenzie > Detail then
                           Put_Line (Trace.all, Parser_Labels (I), "fail");
                        end if;
                        Parser_Status (I) := Fail;
                        Done_Count        := Done_Count + 1;
                     end if;
                  end if;
               else
                  if Active_Workers (I) = 0 then
                     --  No configs left to check (rarely happens with real languages).
                     if Trace_McKenzie > Detail then
                        Put_Line (Trace.all, Parser_Labels (I), "fail (no configs left)");
                     end if;
                     Parser_Status (I) := Fail;
                     Done_Count        := Done_Count + 1;
                  end if;
               end if;

            when Ready =>
               if Parser_States (I).Recover.Config_Heap.Count > 0 and then
                 Parser_States (I).Recover.Config_Heap.Min_Key <= Parser_States (I).Recover.Results.Min_Key
               then
                  --  Still more to check.
                  Set_Outputs (I);
                  return;

               elsif Active_Workers (I) = 0 then
                  Parser_Status (I) := Success;
                  Done_Count        := Done_Count + 1;
               end if;

            when Success | Fail =>
               Done_Count := Done_Count + 1;
            end case;
         end loop;

         if Done_Count = Parsers.Count then
            if Trace_McKenzie > Extra then
               Trace.Put_Line ("Supervisor: done, " & (if Success_Counter > 0 then "succeed" else "fail"));
            end if;

            Set_All_Done;
            All_Parsers_Done := True;
         else
            raise Programmer_Error with "Get_Barrier and Get logic do not match";
         end if;
      end Get;

      procedure Success (Parser_Index : in SAL.Peek_Type; Config : in Configuration)
      is
         use all type SAL.Base_Peek_Type;
         Data : McKenzie_Data renames Parser_States (Parser_Index).Recover;
      begin
         Success_Counter := Success_Counter + 1;
         Result          := Success;

         if Trace_McKenzie > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) & ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Parser_Labels (Parser_Index), Terminals.all, Config);
         end if;

         Data.Success := True;

         --  Active_Workers can be zero when this is called from a pattern.
         Active_Workers (Parser_Index) := Natural'Max (0, Active_Workers (Parser_Index) - 1);

         if Data.Results.Count = 0 then
            Data.Results.Add (Config);
            Parser_Status (Parser_Index) := Ready;

         elsif Config.Cost < Data.Results.Min_Key then
            --  delete higher cost configs from Results
            loop
               Data.Results.Drop;
               exit when Data.Results.Count = 0 or else
                 Config.Cost >= Data.Results.Min_Key;
            end loop;

            Data.Results.Add (Config);

         elsif Config.Cost = Data.Results.Min_Key then
            Data.Results.Add (Config);

         else
            --  Config.Cost > Results.Min_Key
            null;
         end if;
      end Success;

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

         Data : McKenzie_Data renames Parser_States (Parser_Index).Recover;
      begin
         Active_Workers (Parser_Index) := Active_Workers (Parser_Index) - 1;

         loop
            exit when Configs.Count = 0;

            --  [1] has a check for duplicate configs here; that only happens with
            --  higher costs, which take too long for our application.
            Data.Config_Heap.Add (Configs.Remove);
            Data.Enqueue_Count := Data.Enqueue_Count + 1;
         end loop;

         if Trace_McKenzie > Detail then
            Put_Line
              (Trace.all, Parser_Labels (Parser_Index),
               "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
                 "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
                 "/" & Int_Image (Data.Enqueue_Count) &
                 ", cost:" &
                 (if Data.Config_Heap.Count > 0
                  then Integer'Image (Data.Config_Heap.Min_Key)
                  else " ? ") &
                 ", workers:" & Integer'Image (Active_Workers (Parser_Index)));
         end if;
      end Put;

      function Recover_Result return Recover_Status
      is begin
         return Result;
      end Recover_Result;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
      begin
         if Trace_McKenzie > Outline then
            Trace.Put_Line ("Supervisor: Error");
         end if;
         Fatal_Called   := True;
         Error_ID       := Exception_Identity (E);
         Error_Message  := +Exception_Message (E);
         Trace.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end Fatal;

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String)
        when All_Parsers_Done or Fatal_Called
      is begin
         Error_ID := Supervisor.Error_ID;
         Message  := Error_Message;
         if Trace_McKenzie > Detail then
            Trace.Put_Line ("Supervisor: Done");
         end if;
      end Done;

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type
      is begin
         return (Element => Parser_States (Parser_Index));
      end Parser_State;

      function Label (Parser_Index : in SAL.Peek_Type) return Natural
      is begin
         return Parser_Labels (Parser_Index);
      end Label;

   end Supervisor;

   protected body Shared_Lookahead is

      function Get_Token (Index : in Token_Index) return Token_Index
      is
         Temp : Token_Index := Index;
      begin
         if Index > Shared_Parser.Terminals.Last_Index then
            Temp := Next_Grammar_Token
              (Shared_Parser.Terminals, Shared_Parser.Lexer, Shared_Parser.Semantic_State,
               Shared_Parser.Trace.Descriptor.all);
            pragma Assert (Temp = Index);
         end if;
         return Temp;
      end Get_Token;

      function Token (Index : in Token_Index) return Base_Token
      is begin
         return Shared_Parser.Terminals.Element (Index);
      end Token;

   end Shared_Lookahead;

   procedure Put
     (Message         : in              String;
      Super           : not null access Supervisor;
      Shared          : not null access Shared_Lookahead;
      Parser_Index    : in              SAL.Peek_Type;
      Config          : in              Configuration;
      Include_Task_ID : in              Boolean := True)
   is begin
      Put (Message, Super.Trace.all, Super.Parser_State (Parser_Index).Label,
           Shared.Shared_Parser.Terminals, Config, Include_Task_ID);
   end Put;

   ----------
   --  Check code

   function Reduce_Stack
     (Shared  : not null access Shared_Lookahead;
      Stack   : in out          Recover_Stacks.Stack;
      Action  : in              Reduce_Action_Rec;
      Nonterm :    out          Recover_Token)
     return Semantic_Checks.Check_Status
   is
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Semantic_Check;
      use all type Semantic_Checks.Check_Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Recover_Token_Array (1 .. Last);

      Min_Terminal_Index_Set : Boolean := False;
   begin
      Nonterm := (ID => Action.LHS, Virtual => False, others => <>);

      --  We don't pop the stack here, because Semantic_Check_Fixes
      --  needs the pre-reduce stack.
      for I in Tokens'Range loop
         Tokens (I) := Stack (Last - I + 1).Token;
      end loop;

      for T of Tokens loop
         Nonterm.Virtual := Nonterm.Virtual or T.Virtual;

         if Nonterm.Byte_Region.First > T.Byte_Region.First then
            Nonterm.Byte_Region.First := T.Byte_Region.First;
         end if;

         if Nonterm.Byte_Region.Last < T.Byte_Region.Last then
            Nonterm.Byte_Region.Last := T.Byte_Region.Last;
         end if;

         if not Min_Terminal_Index_Set then
            if T.Min_Terminal_Index /= Invalid_Token_Index then
               Min_Terminal_Index_Set     := True;
               Nonterm.Min_Terminal_Index := T.Min_Terminal_Index;
            end if;
         end if;
      end loop;

      if Action.Check = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant Semantic_Checks.Check_Status :=
           Action.Check (Shared.Shared_Parser.Lexer, Nonterm, Tokens)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
            end if;
         end return;
      end if;
   end Reduce_Stack;

   type Parse_Item is record
      Config : Configuration;
      Action : Parse_Action_Node_Ptr;
   end record;

   package Parse_Item_Arrays is new SAL.Gen_Bounded_Definite_Vectors (Positive, Parse_Item, Capacity => 10);
   --  Parse_Item_Arrays.Capacity sets maximum conflicts in one call to Parse

   function Parse_One_Item
     (Super             : not null access Supervisor;
      Shared            : not null access Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Items       : in out          Parse_Item_Arrays.Vector;
      Parse_Item_Index  : in              Positive;
      Shared_Token_Goal : in              Base_Token_Index;
      Trace_Prefix      : in              String)
     return Boolean
   is
      --  Perform parse actions on Parse_Items (Parse_Item_Index), until
      --  one fails (return False) or Shared_Token_Goal is shifted (return True).
      --
      --  If any actions are a conflict, append the conflict action to
      --  Parse_Items.

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor'Class renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Shared_Parser.Table.all;

      Item   : Parse_Item renames Parse_Items (Parse_Item_Index);
      Config : Configuration renames Item.Config;

      Current_Token : Base_Token :=
        (if Config.Current_Inserted = No_Inserted
         then
            Shared.Token (Config.Current_Shared_Token)
         else
           (ID          => Config.Inserted (Config.Current_Inserted),
            Byte_Region => Null_Buffer_Region));

      Action : Parse_Action_Node_Ptr :=
        (if Item.Action = null
         then Action_For (Table, Config.Stack (1).State, Current_Token.ID)
         else Item.Action);

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

   begin
      if Trace_McKenzie > Detail then
         Put (Trace_Prefix & ": " & Image (Current_Token, Descriptor), Super, Shared, Parser_Index, Config);
      end if;

      loop
         if Action.Next /= null then
            if Trace_McKenzie > Detail then
               Put_Line
                 (Trace, Super.Label (Parser_Index), Trace_Prefix & ": add conflict " &
                    Image (Action.Next.Item, Descriptor));
            end if;
            Parse_Items.Append ((Config, Action.Next));
         end if;

         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Super.Label (Parser_Index), Trace_Prefix & ":" & State_Index'Image (Config.Stack.Peek.State) &
                 " : " & Image (Current_Token, Descriptor) &
                 " : " & Image (Action.Item, Descriptor));
         end if;

         case Action.Item.Verb is
         when Shift =>

            Config.Stack.Push
              ((Action.Item.State,
                Syntax_Trees.No_Node_Index,
                (Current_Token.ID,
                 Byte_Region        => Current_Token.Byte_Region,
                 Min_Terminal_Index =>
                   (if Config.Current_Inserted = No_Inserted
                    then Config.Current_Shared_Token
                    else Invalid_Token_Index),
                 Name               => Null_Buffer_Region,
                 Virtual            => False)));

            if Config.Inserted.Last_Index > 0 and Config.Current_Inserted = Config.Inserted.Last_Index then
               Config.Current_Inserted := No_Inserted;
               Config.Inserted.Clear;

               Current_Token := Shared.Token (Config.Current_Shared_Token);

            elsif Config.Current_Inserted /= No_Inserted then
               Config.Current_Inserted := Config.Current_Inserted + 1;

               Current_Token :=
                 (ID          => Config.Inserted (Config.Current_Inserted),
                  Byte_Region => Null_Buffer_Region);

            else
               Config.Current_Shared_Token := Shared.Get_Token (Config.Current_Shared_Token + 1);

               Current_Token := Shared.Token (Config.Current_Shared_Token);
            end if;

         when Reduce =>
            declare
               Nonterm : Recover_Token;
               Status  : constant Semantic_Checks.Check_Status := Reduce_Stack
                 (Shared, Config.Stack, Action.Item, Nonterm);
            begin
               case Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Item.LHS);

                  if New_State = Unknown_State then
                     Success := False;
                  else
                     Config.Stack.Push ((New_State, Syntax_Trees.No_Node_Index, Nonterm));
                  end if;

               when Error =>
                  Success := False;
               end case;
            end;

         when Error =>
            Success := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Item.Verb = Accept_It or
           (if Shared_Token_Goal = Invalid_Token_Index
            then Config.Inserted.Length = 0
            else Config.Current_Shared_Token > Shared_Token_Goal);

         Action := Action_For (Table, Config.Stack (1).State, Current_Token.ID);
      end loop;

      return Success;
   end Parse_One_Item;

   function Parse
     (Super             : not null access Supervisor;
      Shared            : not null access Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Items       :    out          Parse_Item_Arrays.Vector;
      Config            : in              Configuration;
      Shared_Token_Goal : in              Base_Token_Index;
      Trace_Prefix      : in              String)
     return Boolean
   is
      --  Attempt to parse Config until Config.Inserted is all shifted, and
      --  if Shared_Token_Goal /= Invalid_Token_Index, it is shifted. Return
      --  True if successful, False if not.
      --
      --  Parsed configs are in Parse_Items; there is more than one if a
      --  conflict is encountered.

      use all type Ada.Containers.Count_Type;

      Trace : WisiToken.Trace'Class renames Super.Trace.all;

      Last_Index : Positive;
      Success    : Boolean;
   begin
      Parse_Items.Clear;
      Parse_Items.Append ((Config, null));

      loop
         --  Loop over initial config and any conflicts.
         Last_Index := Parse_Items.Last_Index;

         Success := Parse_One_Item
           (Super, Shared, Parser_Index, Parse_Items, Last_Index, Shared_Token_Goal, Trace_Prefix);

         exit when Success or Parse_Items.Last_Index = Last_Index;

         if Trace_McKenzie > Detail then
            Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": parse conflict");
         end if;
      end loop;

      return Success;
   end Parse;

   function Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Branched.Tree)
     return Token_ID
   is
      Nonterm_Item : constant Recover_Stack_Item := Stack.Pop;
   begin
      for C of Tree.Children (Nonterm_Item.Tree_Index) loop
         Stack.Push ((Tree.State (C), C, Tree.Recover_Token (C)));
      end loop;
      return Nonterm_Item.Token.ID;
   end Undo_Reduce;

   procedure Undo_Reduce
     (Stack : in out Parser_Stacks.Stack;
      Tree  : in     Syntax_Trees.Branched.Tree)
   is
      Nonterm_Item : constant Parser_Stack_Item := Stack.Pop;
   begin
      for C of Tree.Children (Nonterm_Item.Token) loop
         Stack.Push ((Tree.State (C), C));
      end loop;
   end Undo_Reduce;

   procedure Do_Shift
     (Super        : not null access Supervisor;
      Shared       : not null access Shared_Lookahead;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration_Access;
      State        : in              State_Index;
      ID           : in              Token_ID)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Trace_McKenzie > Detail then
         Put ("insert " & Image (ID, Super.Trace.Descriptor.all), Super, Shared, Parser_Index, Config.all);
      end if;

      Config.Stack.Push ((State, Syntax_Trees.No_Node_Index, (ID, Virtual => True, others => <>)));
   end Do_Shift;

   procedure Do_Reduce
     (Super               : not null access Supervisor;
      Shared              : not null access Shared_Lookahead;
      Parser_Index        : in              SAL.Peek_Type;
      Local_Config_Heap   : in out          Config_Heaps.Heap_Type;
      Config              : in              Configuration_Access;
      Action              : in              Reduce_Action_Rec;
      Inserted_ID         : in              Token_ID;
      Semantic_Check_Fail : in out          Boolean)
   is
      --  Perform reduce actions until get to a shift of Inserted_Token; if
      --  all succeed, add the final configuration to the heap, return
      --  Success. If a conflict is encountered, process the other action
      --  the same way. If a semantic check fails, enqueue possible
      --  solutions.

      Table       : Parse_Table renames Shared.Shared_Parser.Table.all;
      New_State   : Unknown_State_Index := Unknown_State;
      Next_Action : Parse_Action_Node_Ptr;
   begin
      declare
         Nonterm : Recover_Token;
         Status  : constant Semantic_Checks.Check_Status := Reduce_Stack (Shared, Config.Stack, Action, Nonterm);
      begin
         case Status.Label is
         when Semantic_Checks.Ok =>
            null;
         when Semantic_Checks.Error =>
            if Semantic_Check_Fail then
               return;
               --  This config previously encountered a semantic check error and
               --  applied any appropriate fixes, so don't do that again.
            else
               Semantic_Check_Fail := True;

               if Shared.Shared_Parser.Semantic_Check_Fixes /= null and then
                 Shared.Shared_Parser.Semantic_Check_Fixes
                   (Super.Trace.all, Shared.Shared_Parser.Lexer, Super.Label (Parser_Index), Table.McKenzie_Param,
                    Shared.Shared_Parser.Terminals, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
                    Config.all, Action, Nonterm, Status)
               then
                  --  "ignore error" is viable; continue with Config.
                  --  Finish the reduce.
                  Config.Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
               else
                  --  "ignore error" is not viable; abandon Config.
                  return;
               end if;
            end if;
         end case;

         New_State := Goto_For (Table, Config.Stack (1).State, Action.LHS);

         if New_State = Unknown_State then
            return;
         end if;

         Config.Stack.Push ((New_State, Syntax_Trees.No_Node_Index, Nonterm));
      end;

      if Trace_McKenzie > Extra then
         Put_Line
           (Super.Trace.all, Super.Label (Parser_Index),
            Image (Inserted_ID, Super.Trace.Descriptor.all) & ": " &
              Image (Action, Super.Trace.Descriptor.all) & " => " & Image (New_State));
      end if;

      Next_Action := Action_For (Table, New_State, Inserted_ID);
      case Next_Action.Item.Verb is
      when Shift =>
         Do_Shift (Super, Shared, Parser_Index, Config, Next_Action.Item.State, Inserted_ID);

      when Reduce =>
         Do_Reduce
           (Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item, Inserted_ID,
            Semantic_Check_Fail);

      when Accept_It =>
         raise Programmer_Error with "found test case for Do_Reduce Accept_It";

      when Error =>
         return;
      end case;

      Next_Action := Next_Action.Next;
      if Next_Action /= null then
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config.all);
         begin
            case Next_Action.Item.Verb is
            when Shift =>
               Do_Shift (Super, Shared, Parser_Index, Config, Next_Action.Item.State, Inserted_ID);

            when Reduce =>
               Do_Reduce
                 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Next_Action.Item, Inserted_ID,
                  Semantic_Check_Fail);

            when Accept_It | Error =>
               null;
            end case;
         end;

         --  There can be only one conflict.
      end if;
   end Do_Reduce;

   procedure Process_One
     (Super         : not null access Supervisor;
      Shared        : not null access Shared_Lookahead;
      Config_Status : out McKenzie_Recover.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Trace          : WisiToken.Trace'Class renames Super.Trace.all;
      Table          : Parse_Table renames Shared.Shared_Parser.Table.all;
      McKenzie_Param : McKenzie_Param_Type renames Table.McKenzie_Param;
      EOF_ID         : Token_ID renames Trace.Descriptor.EOF_ID;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : Configuration;
      Parse_Items  : Parse_Item_Arrays.Vector;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.

      Shared_Token_Goal : Token_Index;
   begin
      Super.Get (Parser_Index, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Config.Redo_Reduce then
         --  Config is the result of an Undo_Reduce done in Recover_Init,
         --  because of a failed semantic check; let Semantic_Check_Fixes fix
         --  it.
         declare
            Current_ID : constant Token_ID              := Shared.Token (Config.Current_Shared_Token).ID;
            Action     : constant Parse_Action_Node_Ptr := Action_For (Table, Config.Stack (1).State, Current_ID);
            Nonterm    : Recover_Token;
            New_State  : Unknown_State_Index;

            Status  : constant Semantic_Checks.Check_Status := Reduce_Stack
              (Shared, Config.Stack, Action.Item, Nonterm);

         begin
            if Action.Next /= null then
               raise Programmer_Error with "found test case for conflict in Redo_Reduce";
            end if;

            if Trace_McKenzie > Detail then
               Put_Line (Super.Trace.all, Super.Parser_State (Parser_Index).Label,
                         "redo_reduce " & Image (Nonterm.ID, Trace.Descriptor.all));
            end if;

            if Config.Ops.Length = 1 and then Config.Ops (1).Op = Undo_Reduce then
               --  We are redoing the reduce, so delete this op.
               Config.Ops.Clear;
            else
               raise Programmer_Error with "bad redo_reduce";
            end if;

            Config.Redo_Reduce := False;
            case Status.Label is
            when Semantic_Checks.Ok =>
               raise Programmer_Error with "no error from redo_reduce";

            when Semantic_Checks.Error =>
               if Shared.Shared_Parser.Semantic_Check_Fixes /= null and then
                 Shared.Shared_Parser.Semantic_Check_Fixes
                   (Trace, Shared.Shared_Parser.Lexer, Super.Label (Parser_Index), Table.McKenzie_Param,
                    Shared.Shared_Parser.Terminals, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap, Config,
                    Action.Item, Nonterm, Status)
               then
                  --  "ignore error" is a viable solution, so continue with Config;
                  --  finish reduce.
                  Config.Stack.Pop (SAL.Base_Peek_Type (Action.Item.Token_Count));
               else
                  --  "ignore error" is not viable, so abandon Config, but enqueue
                  --  Local_Config_Heap.

                  if Local_Config_Heap.Count = 0 then
                     raise Programmer_Error with "Semantic_Check_Fixes returned no solution to redo_reduce";
                  end if;

                  Super.Put (Parser_Index, Local_Config_Heap);
                  return;
               end if;
            end case;

            New_State := Goto_For (Table, Config.Stack (1).State, Action.Item.LHS);

            Config.Stack.Push ((New_State, Syntax_Trees.No_Node_Index, Nonterm));
         end;
      end if;

      if Config.Current_Inserted /= No_Inserted then
         if Parse
           (Super, Shared, Parser_Index, Parse_Items, Config,
            Shared_Token_Goal => Invalid_Token_Index,
            Trace_Prefix      => "fast_forward")
         then
            if Parse_Items.Length = 1 then
               Config := Parse_Items (1).Config;
               Parse_Items.Clear;
            else
               raise Programmer_Error with "fast_forward returned multiple configs";
            end if;
         else
            raise Programmer_Error with "fast_forward failed";
         end if;
      end if;

      --  If there are push_backs, we must parse all of the push back
      --  tokens, and then Check_Limit more.
      Shared_Token_Goal := Super.Parser_State (Parser_Index).Shared_Token + Token_Index
        (Table.McKenzie_Param.Check_Limit);

      if Parse (Super, Shared, Parser_Index, Parse_Items, Config, Shared_Token_Goal, "check") then
         Super.Success (Parser_Index, Config);
         return;
      end if;

      if Trace_McKenzie > Detail then
         Put ("continuing", Super, Shared, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Super.Label (Parser_Index), Image (Config.Stack, Trace.Descriptor.all));
         end if;
      end if;

      --  Grouping these operations ensures we know the order to do them in
      --  later in the actual parser, and that there are no duplicate
      --  solutions found.
      --
      --  All possible permutations will be explored, except pop only occurs
      --  after all pushed_back; mixing them would leave holes in
      --  Shared_Parser.Terminals, which we can't handle. This allows
      --  pushing back to the correct error location, and deleting an erroneous
      --  token at that point.

      if None (Config.Ops, Pop) and
        None (Config.Ops, Delete) and
        None (Config.Ops, Insert) and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         declare
            Token : constant Recover_Token := Config.Stack (1).Token;
         begin
            --  Try pushing back the stack top, to allow insert and other
            --  operations at that point.
            --
            --  Since we are not actually changing the source text, it is tempting
            --  to give this operation zero cost. But then we keep doing push_back
            --  forever, making no progress. So we give it a cost.

            if not Token.Virtual and Token.Byte_Region /= Null_Buffer_Region then
               --  If Virtual, this is from a previous recover session; no point in trying to
               --  redo it.
               --
               --  If Null_Buffer_Region, push_back is the same as pop; only do pop.

               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
               begin
                  New_Config.Stack.Pop;
                  New_Config.Cost := New_Config.Cost + McKenzie_Param.Push_Back (Token.ID);
                  New_Config.Ops.Append ((Push_Back, Token.ID));
                  New_Config.Current_Shared_Token := Token.Min_Terminal_Index;
                  if Trace_McKenzie > Detail then
                     Put ("push_back " & Image (Token.ID, Trace.Descriptor.all), Super, Shared,
                          Parser_Index, New_Config.all);
                  end if;
               end;
            end if;
         end;
      end if;

      if None (Config.Ops, Push_Back) and
        None (Config.Ops, Delete) and
        None (Config.Ops, Insert) and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         --  Try deleting stack top
         declare
            New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);

            Item : constant Recover_Stack_Item := New_Config.Stack.Pop;
         begin
            New_Config.Cost := New_Config.Cost +
              (if Item.Token.Byte_Region = Null_Buffer_Region
               then 0
               else McKenzie_Param.Delete (Item.Token.ID));

            New_Config.Ops.Append ((Pop, Item.Token.ID));
            if Trace_McKenzie > Detail then
               Put ("pop " & Image (Item.Token.ID, Trace.Descriptor.all), Super, Shared, Parser_Index, New_Config.all);
            end if;
         end;
      end if;

      if None (Config.Ops, Delete) then
         --  Find terminal insertions to try; loop over input actions for the
         --  current state.
         --
         --  If this config does not pass Check, we want to make other changes
         --  to it, eventually finding a config that passes. So we perform any
         --  needed reductions and one shift, so the config is in a consistent
         --  state, and enqueue the result. If there are any conflicts or
         --  semantic check fails encountered, they create other configs to
         --  enqueue.

         declare
            I : Action_List_Iterator := First (Table.States (Config.Stack.Peek.State));

            Semantic_Check_Fail : Boolean := False;
         begin
            loop
               exit when I.Is_Done;

               declare
                  ID     : constant Token_ID := I.Symbol;
                  Action : Parse_Action_Rec renames I.Action;
               begin
                  if ID /= EOF_ID then
                     case Action.Verb is
                     when Shift | Reduce =>
                        declare
                           New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                        begin
                           New_Config.Ops.Append ((Insert, ID));
                           New_Config.Cost := New_Config.Cost + McKenzie_Param.Insert (ID);

                           if Action.Verb = Shift then
                              Do_Shift (Super, Shared, Parser_Index, New_Config, Action.State, ID);
                           else
                              Do_Reduce
                                (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action, ID,
                                 Semantic_Check_Fail);
                           end if;
                        end;

                     when Accept_It | Error =>
                        null;
                     end case;
                  end if;
               end;
               I.Next;
            end loop;
         end;

         --  Find nonterm insertions to try; loop over goto actions for the
         --  current state

         declare
            I : Goto_List_Iterator := First (Table.States (Config.Stack.Peek.State));
         begin
            loop
               exit when I.Is_Done;

               declare
                  ID : constant Token_ID := I.Symbol;
               begin
                  if ID /= EOF_ID then
                     declare
                        New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                     begin
                        New_Config.Ops.Append ((Insert, ID));
                        New_Config.Cost := New_Config.Cost + McKenzie_Param.Insert (ID);

                        Do_Shift (Super, Shared, Parser_Index, New_Config, I.State, ID);
                     end;
                  end if;
               end;
               I.Next;
            end loop;
         end;
      end if;

      if Config.Current_Inserted = No_Inserted then
         --  Try deleting (= skipping) the current shared input token.
         declare
            ID : constant Token_ID := Shared.Token (Config.Current_Shared_Token).ID;
         begin
            if ID /= EOF_ID then
               --  can't delete EOF
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
               begin
                  New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (ID);

                  New_Config.Ops.Append ((Delete, ID));
                  New_Config.Current_Shared_Token := Shared.Get_Token (New_Config.Current_Shared_Token + 1);

                  if Trace_McKenzie > Detail then
                     Put ("delete " & Image (ID, Trace.Descriptor.all), Super, Shared, Parser_Index, New_Config.all);
                  end if;
               end;
            end if;
         end;
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   end Process_One;

   ----------
   --  Top level

   task type Worker_Task
     (Super  : not null access Supervisor;
      Shared : not null access Shared_Lookahead)
   is
      entry Start;
      --  Start getting parser/configs to check from Config_Store.

      entry Done;
      --  Available when task is ready to terminate; after this rendezvous,
      --  task discriminants may be freed.

   end Worker_Task;

   task body Worker_Task
   is
      Status : McKenzie_Recover.Config_Status;
   begin
      accept Start;

      loop
         Process_One (Super, Shared, Status);

         exit when Status = All_Done;
      end loop;

      accept Done;
   exception
   when E : others =>
      Super.Fatal (E);
   end Worker_Task;

   function To_Recover
     (Parser_Stack : in Parser_Stacks.Stack;
      Tree         : in Syntax_Trees.Branched.Tree)
     return Recover_Stacks.Stack
   is
      use all type SAL.Base_Peek_Type;
      Result : Recover_Stacks.Stack;
      Depth  : constant SAL.Peek_Type := Parser_Stack.Depth;
   begin
      Result.Set_Depth (Depth);
      for I in 1 .. Depth loop
         declare
            Item  : Parser_Stack_Item renames Parser_Stack (I);
            Token : constant Recover_Token := (if I = Depth then (others => <>) else Tree.Recover_Token (Item.Token));
         begin
            Result.Set (I, Depth, (Item.State, Item.Token, Token));
         end;
      end loop;
      return Result;
   end To_Recover;

   procedure Recover_Init
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      Trace  : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Config : constant Configuration_Access := Parser_State.Recover.Config_Heap.Add (Configuration'(others => <>));
      Error  : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser" & Integer'Image (Parser_State.Label) &
              ": State" & State_Index'Image (Parser_State.Stack (1).State) &
              " Current_Token" & Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all));
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Parser_State.Label, Image (Parser_State.Stack, Trace.Descriptor.all, Parser_State.Tree));
         end if;
      end if;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      Config.Stack := To_Recover (Parser_State.Stack, Parser_State.Tree);

      --  Parser_State.Local_Lookahead must be empty (else we would not get
      --  here). Therefore Parser_State current token is in
      --  Shared_Parser.Shared_Token.

      Config.Current_Shared_Token := Parser_State.Shared_Token;

      if Error.Label = Check then
         --  Undo the reduction that encountered the error, let
         --  Process_One enqueue possible solutions. One of those solutions
         --  will be to ignore the error, so we don't enqueue that config here.
         --  We leave the cost at 0, because this is the root config.

         if Trace_McKenzie > Detail then
            Put ("undo_reduce " & Image
                   (Config.Stack (1).Token.ID, Trace.Descriptor.all), Trace, Parser_State.Label,
                 Shared_Parser.Terminals, Config.all, Include_Task_ID => False);
         end if;
         Config.Ops.Append ((Undo_Reduce, Undo_Reduce (Config.Stack, Parser_State.Tree)));
         Config.Redo_Reduce := True;
      else
         if Trace_McKenzie > Detail then
            Put ("enqueue", Trace, Parser_State.Label, Shared_Parser.Terminals, Config.all,
                 Include_Task_ID => False);
         end if;
      end if;

      Parser_State.Recover.Enqueue_Count := Parser_State.Recover.Enqueue_Count + 1;
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Super : aliased Supervisor
        (Trace'Access,
         Parsers'Access,
         Shared_Parser.Terminals'Access,
         Cost_Limit   => Shared_Parser.Table.McKenzie_Param.Cost_Limit,
         Parser_Count => Parsers.Count);

      Shared : aliased Shared_Lookahead (Shared_Parser'Access);

      Worker_Tasks   : array (1 .. System.Multiprocessors.Number_Of_CPUs - 1) of Worker_Task
        (Super'Access, Shared'Access);
      --  Keep one CPU free for this main task, and the user.
      --  FIXME: see if more tasks go faster or slower.

      procedure Cleanup
      is begin
         for I in Worker_Tasks'Range loop
            if Worker_Tasks (I)'Callable then
               abort Worker_Tasks (I);
            end if;
         end loop;
      end Cleanup;

   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
      end if;

      Super.Initialize;

      for Parser_State of Parsers loop
         Recover_Init (Shared_Parser, Parser_State);
      end loop;

      if Trace_McKenzie > Outline then
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
                  if Trace_McKenzie > Outline then
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
                     if Parsers.Count + Data.Results.Count > Shared_Parser.Max_Parallel then
                        raise WisiToken.Parse_Error with Error_Message
                          ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                           ": too many parallel parsers required in grammar state" &
                             State_Index'Image (Cur.State_Ref.Stack.Peek.State) &
                             "; simplify grammar, or increase max-parallel (" &
                             SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");
                     end if;

                     for I in 1 .. Data.Results.Count - 1 loop
                        Parsers.Prepend_Copy (Cur); --  does not copy recover
                        if Trace_McKenzie > Outline then
                           Trace.Put_Line
                             ("spawn parser" & Integer'Image (Parsers.First.Label) & " from " &
                                Int_Image (Cur.Label) & " (" & Int_Image (Integer (Parsers.Count)) & " active)");
                           Put ("", Trace, Parsers.First.Label, Shared_Parser.Terminals,
                                Data.Results.Peek, Include_Task_ID => False);
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline then
                     Put ("", Trace, Cur.State_Ref.Label, Shared_Parser.Terminals, Data.Results.Peek,
                          Include_Task_ID => False);
                  end if;
               else
                  if Trace_McKenzie > Outline then
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
               use all type Ada.Containers.Count_Type;
               use Parser_Lists;
               Descriptor : WisiToken.Descriptor'Class renames Shared_Parser.Trace.Descriptor.all;
               Table      : Parse_Table renames Shared_Parser.Table.all;
               Tree       : Syntax_Trees.Branched.Tree renames Parser_State.Tree;
               Data       : McKenzie_Data renames Parser_State.Recover;
               Result     : Configuration renames Data.Results.Peek;
            begin
               --  There's no need to modify Parser_State.Tree for undone_reduce,
               --  pushed_back, popped, or deleted. The tree nodes created for those
               --  tokens are useful for error repair, and will just be ignored in
               --  future parsing. This also avoids enlarging a non-flushed branched
               --  tree, which saves time and space.
               --
               --  We do have to modify Parser_State.Stack. We check the IDs to
               --  confirm that we did the operations in a consistent order.

               if Trace_McKenzie > Extra then
                  Trace.Put_Line ("parser stack: " & Image (Parser_State.Stack, Descriptor, Tree));
               end if;

               for Op of Result.Ops loop
                  case Op.Op is
                  when Undo_Reduce =>
                     if Op.ID /= Tree.ID (Parser_State.Stack (1).Token) then
                        raise Programmer_Error with "parser stack mismatch: " & Image (Op, Descriptor) & " /= " &
                          Tree.Image (Parser_State.Stack (1).Token, Descriptor);
                     end if;
                     Undo_Reduce (Parser_State.Stack, Tree);

                  when Push_Back =>
                     if Op.ID /= Tree.ID (Parser_State.Stack (1).Token) then
                        raise Programmer_Error with "parser stack mismatch: " & Image (Op, Descriptor) & " /= " &
                          Tree.Image (Parser_State.Stack (1).Token, Descriptor);
                     end if;
                     Parser_State.Shared_Token := Tree.Min_Terminal_Index (Parser_State.Stack.Pop.Token);

                  when Pop =>
                     if Op.ID /= Tree.ID (Parser_State.Stack (1).Token) then
                        raise Programmer_Error with "parser stack mismatch: " & Image (Op, Descriptor) & " /= " &
                          Tree.Image (Parser_State.Stack (1).Token, Descriptor);
                     end if;
                     Parser_State.Stack.Pop;

                  when Insert =>
                     null; --  processed in reverse order below

                  when Delete =>
                     if Op.ID /= Shared_Parser.Terminals (Parser_State.Shared_Token).ID then
                        raise Programmer_Error with "parser Terminals mismatch" & Image (Op, Descriptor) & " /= " &
                          Image (Shared_Parser.Terminals (Parser_State.Shared_Token).ID, Descriptor);
                     end if;
                     Parser_State.Shared_Token := Parser_State.Shared_Token + 1;

                  end case;
               end loop;

               for Op of reverse Result.Ops loop
                  if Op.Op = Insert then
                     if Op.ID in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                        Parser_State.Local_Lookahead.Add_To_Head (Parser_State.Tree.Add_Terminal (Op.ID));
                     else
                        --  This nonterm was pushed pre-reduced on the config stack; change
                        --  that to a sequence of terminals in lookahead
                        for T of reverse Table.Terminal_Sequences (Op.ID) loop
                           Parser_State.Local_Lookahead.Add_To_Head (Parser_State.Tree.Add_Terminal (T));
                        end loop;
                        --  FIXME: if this is not actually useful, delete it to cut down on
                        --  wasted configs in Process_One
                        raise Programmer_Error with "found test case for insert nonterm";
                     end if;
                  end if;
               end loop;

               if Parser_State.Shared_Token /= Result.Current_Shared_Token then
                  raise Programmer_Error with "shared_token mismatch: parser " &
                    Token_Index'Image (Parser_State.Shared_Token) & " /= config " &
                    Token_Index'Image (Result.Current_Shared_Token);
               end if;

               --  Update Current_Token, Current_Token_Is_Virtual
               if Parser_State.Local_Lookahead.Count > 0 then
                  Parser_State.Current_Token := Parser_State.Local_Lookahead.Get;

                  Parser_State.Current_Token_Is_Virtual := True;

               elsif Any (Result.Ops, Push_Back) or Any (Result.Ops, Delete) then
                  --  Result.Current_Shared_Token will be parsed, so it must be current
                  --  in Parser_State.Tree.
                  Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                    (Parser_State.Shared_Token, Shared_Parser.Terminals);

                  Parser_State.Current_Token_Is_Virtual := False;

               else
                  --  No change in Parser_State.Current_Token
                  Parser_State.Current_Token_Is_Virtual := False;
               end if;

               Parser_State.Errors (Parser_State.Errors.Last).Recover := Result;

               if Trace_McKenzie > Extra then
                  Trace.Put_Line ("parser stack: " & Image (Parser_State.Stack, Descriptor, Tree));
                  Trace.Put_Line
                    ("parser Current_Token " & Parser_State.Tree.Image (Parser_State.Current_Token, Descriptor));
               end if;
            end;
         end if;
      end loop;

      Cleanup;

      return Super.Recover_Result;

   exception
   when others =>
      Cleanup;
      raise;
   end Recover;

   ----------
   --  Spec private subprograms not declared above; for language-specific
   --  child packages.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String :=
        (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Count := 0;

      loop
         exit when Matching_Name_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token : Recover_Token renames Config.Stack (Matching_Name_Index).Token;
         begin
            exit when Token.Name /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Token.Name))
               else Lexer.Buffer_Text (Token.Name));

            if Other_ID = Token.ID then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

end WisiToken.LR.McKenzie_Recover;
