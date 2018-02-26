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

with Ada.Exceptions;
with Ada.Task_Identification;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Unbounded_Definite_Queues;
with System.Multiprocessors;
with WisiToken.LR.Parser_Lists;
package body WisiToken.LR.McKenzie_Recover is

   procedure Put
     (Message         : in     String;
      Trace           : in out WisiToken.Trace'Class;
      Parser_Label    : in     Natural;
      Config          : in     Configuration;
      Include_Task_ID : in     Boolean := True)
   is
      --  For debugging output

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).
      use all type SAL.Base_Peek_Type;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Include_Task_ID then +Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else +"") &
         Integer'Image (Parser_Label) & ": " & Message & ":";
   begin
      Result := Result & Natural'Image (Config.Cost) & ", ";
      Result := Result & Image (Config.Stack, Trace.Descriptor.all, Config.Tree, Depth => 1) & "|";

      if Config.Local_Lookahead.Length = 0 then
         Result := Result & Token_Index'Image (Config.Shared_Token);
      else
         Result := Result & Image (Config.Local_Lookahead, Config.Tree, Trace.Descriptor.all) & ":" &
           Int_Image (Integer (Config.Local_Lookahead_Index));
      end if;
      Result := Result & "|" &
        Image (Config.Popped, Trace.Descriptor.all) &
        Image (Config.Pushed, Trace.Descriptor.all, Config.Tree) &
        Image (Config.Inserted, Trace.Descriptor.all) &
        Image (Config.Deleted, Trace.Descriptor.all) &
        Semantic_Checks.Image (Config.Semantic_Check_Fixes);
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
   --  configuration that it is not yet delivered to Supervisor.
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

   type Parser_Data_Array is array (SAL.Peek_Type range <>) of McKenzie_Access;
   --  Index is same as Parser_Status.

   protected type Supervisor
     (Trace        : not null access WisiToken.Trace'Class;
      Parsers      : not null access Parser_Lists.List;
      Cost_Limit   : Natural;
      Parser_Count : SAL.Peek_Type)
   is
      --  There is only one object of this type, declared in Recover.

      procedure Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Parser_Label : out Natural;
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

      procedure Force_Done (Parser_Label : in Natural; Config : in Configuration);
      --  Force parser with Parser_Label to halt all processing, assume
      --  Config is successful. Set Result to Ignore.

      function Recover_Result return Recover_Status;

      procedure Fatal (Parser_Label : in Natural; E : in Ada.Exceptions.Exception_Occurrence);
      --  Report a fatal error; abort all processing, make Done
      --  available.

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String);
      --  Available when all parsers have failed or succeeded, or an error
      --  occured.
      --
      --  If Error_ID is not Null_Id, an error occured.

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
      Parser_Data      : Parser_Data_Array (1 .. Parser_Count);
      Parser_Labels    : Parser_Natural_Array (1 .. Parser_Count); -- For Trace
   end Supervisor;

   protected type Shared_Lookahead (Shared_Parser : not null access LR.Parser.Parser)
   is
      --  There is only one object of this type, declared in Recover. It
      --  controls access to the shared Terminals and the lexer input.

      function Get_Token (Index : in Token_Index) return Token_Index;
      --  Return Index, after assuring there is a token in shared Terminals
      --  there, reading from the lexer if necessary.

      function ID (Index : in Token_Index) return Token_ID;
      --  Return Shared_Parser.Terminals (Index).ID.

   end Shared_Lookahead;

   ----------
   --  Protected object bodies

   function Get_Barrier
     (Parsers        : not null access Parser_Lists.List;
      Parser_Status  : in              Parser_Status_Array;
      Parser_Data    : in              Parser_Data_Array;
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
            if Parser_Data (I).Config_Heap.Count > 0 then
               if Parser_Data (I).Config_Heap.Min_Key <= Cost_Limit then
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
            if Parser_Data (I).Config_Heap.Count > 0 and then
              Parser_Data (I).Config_Heap.Min_Key <= Parser_Data (I).Results.Min_Key
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
            Parser_Data (Index)   := Parser_Lists.McKenzie_Ref (I);
            Parser_Labels (Index) := Parsers.Constant_Reference (I).Label;

            declare
               Data : McKenzie_Data renames Parsers.Reference (I).Recover;
            begin
               Data.Parser_Label := Parser_Labels (Index);
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
         Parser_Label : out Natural;
         Config       : out Configuration;
         Status       : out Config_Status)
        when (Fatal_Called or All_Parsers_Done) or else
          Get_Barrier (Parsers, Parser_Status, Parser_Data, Active_Workers, Cost_Limit)
      is
         use all type SAL.Base_Peek_Type;
         Done_Count : SAL.Base_Peek_Type := 0;

         procedure Set_Outputs (I : in SAL.Base_Peek_Type)
         is begin
            Parser_Index := I;
            Parser_Label := Parser_Labels (I);
            Config       := Parser_Data (I).Config_Heap.Remove;
            Status       := Valid;

            Parser_Data (I).Check_Count := Parser_Data (I).Check_Count + 1;
            Active_Workers (I)          := Active_Workers (I) + 1;
         end Set_Outputs;

         procedure Set_All_Done
         is begin
            Parser_Index := SAL.Base_Peek_Type'First;
            Parser_Label := Natural'First;
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
               if Parser_Data (I).Config_Heap.Count > 0 then
                  if Parser_Data (I).Config_Heap.Min_Key <= Cost_Limit then

                     Set_Outputs (I);
                     return;
                  else
                     if Active_Workers (I) = 0 then
                        if Trace_McKenzie > Detail then
                           Put_Line (Trace.all, Parser_Data (I).Parser_Label, "fail");
                        end if;
                        Parser_Status (I) := Fail;
                        Done_Count        := Done_Count + 1;
                     end if;
                  end if;
               else
                  if Active_Workers (I) = 0 then
                     --  No configs left to check (rarely happens with real languages).
                     if Trace_McKenzie > Detail then
                        Put_Line (Trace.all, Parser_Data (I).Parser_Label, "fail (no configs left)");
                     end if;
                     Parser_Status (I) := Fail;
                     Done_Count        := Done_Count + 1;
                  end if;
               end if;

            when Ready =>
               if Parser_Data (I).Config_Heap.Count > 0 and then
                  Parser_Data (I).Config_Heap.Min_Key <= Parser_Data (I).Results.Min_Key
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
         Data : McKenzie_Data renames Parser_Data (Parser_Index).all;
      begin
         Success_Counter := Success_Counter + 1;
         Result          := Success;

         if Trace_McKenzie > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) &
                 ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Data.Parser_Label, Config);
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

      procedure Force_Done (Parser_Label : in Natural; Config : in Configuration)
      is
      begin
         Success_Counter := Success_Counter + 1;
         Result          := Ignore;

         if Trace_McKenzie > Detail then
            Put ("force_done: ", Trace.all, Parser_Label, Config);
         end if;

         for I in Parser_Labels'Range loop
            if Parser_Labels (I) = Parser_Label then
               Parser_Status (I) := Success;
               declare
                  Data : McKenzie_Data renames Parser_Data (I).all;
               begin
                  Data.Success := True;
                  Data.Results.Add (Config);
               end;
               return;
            end if;
         end loop;
      end Force_Done;

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

         Data : McKenzie_Data renames Parser_Data (Parser_Index).all;
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
              (Trace.all, Data.Parser_Label,
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

      procedure Fatal (Parser_Label : in Natural; E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
      begin
         if Trace_McKenzie > Outline then
            Put_Line (Trace.all, Parser_Label, "Supervisor: Error");
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

   end Supervisor;

   protected body Shared_Lookahead is

      function Get_Token (Index : in Token_Index) return Token_Index
      is
         Temp : Token_Index := Index;
      begin
         if Index > Shared_Parser.Terminals.Last_Index then
            Temp := Next_Grammar_Token (Shared_Parser.Terminals, Shared_Parser.Lexer, Shared_Parser.Semantic_State);
            pragma Assert (Temp = Index);
         end if;
         return Temp;
      end Get_Token;

      function ID (Index : in Token_Index) return Token_ID
      is begin
         return Shared_Parser.Terminals (Index).ID;
      end ID;

   end Shared_Lookahead;

   ----------
   --  Check code

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Begin_ID            : in     Token_ID;
      Matching_Name_Index : in out SAL.Peek_Type;
      Begin_Count         :    out Integer)
   is
      use all type SAL.Peek_Type;
   begin
      --  Search Config.Stack, starting at Matching_Name_Index, for a token
      --  containing a name that matches Name, counting 'begin's.
      --
      --  Matching_Name_Index is Peek index into Config.Stack; >
      --  config.stack.depth if not found.

      Begin_Count := 0;

      loop
         exit when Matching_Name_Index > Config.Stack.Depth - 1; -- First state has invalid token
         declare
            Name_Region : Buffer_Region renames Config.Tree.Name_Region (Config.Stack (Matching_Name_Index).Token);
            ID          : Token_ID renames Config.Tree.ID (Config.Stack (Matching_Name_Index).Token);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Lexer.Buffer_Text (Name_Region) = Name;

            if ID = Begin_ID then
               Begin_Count := Begin_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Do_Shift
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      McKenzie_Param    : in     McKenzie_Param_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Action            : in     Shift_Action_Rec;
      Inserted_Token    : in     Token_ID)
   is begin
      if Trace_McKenzie > Extra then
         Put_Line (Trace, Parser_Label, Image (Config.Stack.Peek.State) & " : " & Image (Action, Trace.Descriptor.all));
      end if;
      Config.Stack.Push ((Action.State, Config.Tree.Add_Terminal (Inserted_Token)));
      Config.Inserted.Append (Inserted_Token);
      Config.Cost := Config.Cost + McKenzie_Param.Insert (Inserted_Token);
      if Trace_McKenzie > Detail then
         Put ("enqueue", Trace, Parser_Label, Config);
      end if;
      Local_Config_Heap.Add (Config);
   end Do_Shift;

   function Try_Semantic_Check_Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      McKenzie_Param    : in     McKenzie_Param_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Status            : in     Semantic_Checks.Check_Status)
     return Boolean
   is
      --  Enqueue fixes for semantic check failure indicated by Code.
      --  Return True if one fix is that error should be ignored, false
      --  otherwise.
   begin
      for Pat of McKenzie_Param.Patterns loop
         --  FIXME: split patterns into two lists, grammar and semantic_check?
         --  FIXME: rename 'pattern' to 'strategy'?

         if Pat in Recover_Block_Mismatched_Names'Class and
           not Config.Semantic_Check_Fixes (Status.Code)
         then
            declare
               use all type SAL.Base_Peek_Type;
               Pattern : Recover_Block_Mismatched_Names renames Recover_Block_Mismatched_Names (Pat);
            begin
               case Status.Code is
               when Semantic_Checks.Match_Names_Error =>
                  --  There are two cases:
                  --
                  --  1. This is a user error; the fix is to make one of the names match
                  --  the other. We can't actually do that without editing the buffer.
                  --  So we just ignore this error.
                  --
                  --  2. The mismatch indicates an extra or missing 'end'. The fix is to
                  --  search the syntax tree to find the error, and fix it.
                  --
                  --  First test for case 2.
                  declare
                     End_Name            : constant String := Lexer.Buffer_Text
                       (Config.Tree.Name_Region (Status.Tokens (Status.Tokens.Last_Index)));
                     Matching_Name_Index : SAL.Peek_Type   := 2;
                     Begin_Count         : Integer         := 0;
                  begin
                     Find_Matching_Name (Config, Lexer, End_Name, Pattern.Begin_ID, Matching_Name_Index, Begin_Count);

                     if Matching_Name_Index < Config.Stack.Depth then
                        if Config.Deleted.Length = 0 and
                          Config.Inserted.Length = 0
                        then
                           --  Since our solution involves popping and inserting, we can't apply
                           --  it if items have already been deleted or inserted; the order of
                           --  operations is lost.

                           raise Programmer_Error with "found test case for Match_Names_Error";

                           --  Found a matching name; case 2.
                           --  declare
                           --     New_Config : Configuration := Config;
                           --  begin
                           --     --  Pop '<name> ;'
                           --     New_Config.Popped.Append (New_Config.Stack.Pop.Token.ID);
                           --     New_Config.Popped.Append (New_Config.Stack.Pop.Token.ID);

                           --     --  Replace the popped ';'
                           --     New_Config.Local_Lookahead.Prepend (Pattern.Semicolon_ID);
                           --     New_Config.Inserted.Prepend (Pattern.Semicolon_ID);

                           --     --  Insert '; end' for each extra 'begin'
                           --     for I in 1 .. Begin_Count loop
                           --        New_Config.Local_Lookahead.Prepend (Pattern.End_ID);
                           --        New_Config.Inserted.Prepend (Pattern.End_ID);

                           --        New_Config.Local_Lookahead.Prepend (Pattern.Semicolon_ID);
                           --        New_Config.Inserted.Prepend (Pattern.Semicolon_ID);
                           --     end loop;

                           --     New_Config.Local_Lookahead_Index := New_Config.Local_Lookahead.First_Index;

                           --     New_Config.Cost := 0;

                           --     New_Config.Semantic_Check_Fixes (Status.Code) := True;
                           --     Config.Semantic_Check_Fixes (Status.Code) := True; -- Avoid duplication.

                           --     if Trace_McKenzie > Extra then
                           --        Put_Line (Trace, Parser_Label, "Match_Names_Error pattern");
                           --     end if;

                           --     Local_Config_Heap.Add (New_Config);

                           --     return False;
                           --  end;
                        else
                           return False;
                        end if;
                     else
                        --  No matching name; case 1.
                        return True;
                     end if;
                  end;

               when Semantic_Checks.Missing_Name_Error =>
                  --  Fix is to add a name; insert a virtual token. FIXME: need test case,
                  --  solution
                  raise Programmer_Error with "found test case for Missing_Name_Error";
                  return False;

               when Semantic_Checks.Extra_Name_Error =>
                  if Config.Deleted.Length = 0 and
                    Config.Inserted.Length = 0
                  then
                     --  Since our solution involves popping, we can't apply it if items
                     --  have already been deleted or inserted; the order of operations is
                     --  lost.

                     --  Input looks like: "begin ... end <name>;"
                     --  Error is a missing 'end'; ideally we want to insert 'end ;' before 'end <name>'.
                     --  The best we can do is pop '<name> ;', insert '; end ;'
                     --
                     --  This solution is different from Apply_Pattern
                     --  Recover_Block_Mismatched_Names Extra_Name_Error, because here the
                     --  production is not yet reduced.
                     --
                     --  See test_mckenzie_recover.adb Pattern_Block_Mismatched_Names.
                     declare
                        New_Config : Configuration := Config;
                     begin
                        New_Config.Popped.Append (New_Config.Tree.ID (New_Config.Stack.Pop.Token)); -- ';'
                        New_Config.Popped.Append (New_Config.Tree.ID (New_Config.Stack.Pop.Token)); -- '<name>'

                        New_Config.Local_Lookahead.Prepend (New_Config.Tree.Add_Terminal (Pattern.Semicolon_ID));
                        New_Config.Inserted.Prepend (Pattern.Semicolon_ID);
                        New_Config.Local_Lookahead.Prepend (New_Config.Tree.Add_Terminal (Pattern.End_ID));
                        New_Config.Inserted.Prepend (Pattern.End_ID);
                        New_Config.Local_Lookahead.Prepend (New_Config.Tree.Add_Terminal (Pattern.Semicolon_ID));
                        New_Config.Inserted.Prepend (Pattern.Semicolon_ID);

                        New_Config.Local_Lookahead_Index := New_Config.Local_Lookahead.First_Index;

                        New_Config.Cost := 0;

                        New_Config.Semantic_Check_Fixes (Status.Code) := True;
                        Config.Semantic_Check_Fixes (Status.Code) := True; -- Avoid duplication.

                        if Trace_McKenzie > Extra then
                           Put_Line (Trace, Parser_Label, "Extra_Name_Error pattern");
                        end if;

                        Local_Config_Heap.Add (New_Config);

                        return False;
                     end;
                  end if;
               end case;
            end;
         end if;
      end loop;
      return False;
   end Try_Semantic_Check_Fixes;

   procedure Do_Reduce
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      Table             : in     Parse_Table;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in out Configuration;
      Action            : in     Reduce_Action_Rec;
      Inserted_Token    : in     Token_ID;
      Lexer             : in     WisiToken.Lexer.Handle)
   is
      --  Perform reduce actions until get to a shift of Inserted_Token; if
      --  all succeed, add the final configuration to the heap. If any
      --  action fails, just return.
      --
      --  This discards intermediate reduce states, which may lead to two
      --  similar solutions appearing to be identical. The full parser will
      --  repeat these reductions, keeping all intermediate states.

      use all type Semantic_Checks.Check_Status_Label;
      use all type Semantic_Checks.Error_Label_Set;

      New_Config_1 : Configuration := Config;
      New_Config_2 : Configuration;
      New_State    : Unknown_State_Index;
      Next_Action  : Parse_Action_Node_Ptr;
      Nonterm      : Syntax_Trees.Valid_Node_Index;
      Status       : Semantic_Checks.Check_Status;

      Trace_Prefix : constant String :=
        (if Trace_McKenzie > Extra
         then Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & Integer'Image (Parser_Label) & ": "
         else "");
   begin
      if Trace_McKenzie > Extra then
         Put_Line (Trace, Parser_Label, Image (Config.Stack.Peek.State) & " : " & Image (Action, Trace.Descriptor.all));
      end if;

      Status := Reduce_Stack
        (New_Config_1.Stack, New_Config_1.Tree, Action, Nonterm, Lexer, Trace, Trace_McKenzie, Trace_Prefix);

      if Status.Label = Error then
         if not Try_Semantic_Check_Fixes
           (Trace, Parser_Label, Table.McKenzie_Param, Lexer, Local_Config_Heap, New_Config_1, Status)
         then
            return;
         end if;
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
            New_Config_1.Semantic_Check_Fixes := New_Config_1.Semantic_Check_Fixes or New_Config_2.Semantic_Check_Fixes;
            Config.Semantic_Check_Fixes := Config.Semantic_Check_Fixes or New_Config_1.Semantic_Check_Fixes;

         when Accept_It | Error =>
            null;
         end case;
         Next_Action := Next_Action.Next;
      end loop;
   end Do_Reduce;

   type Check_Item is record
      Config        : Configuration;
      Action        : Parse_Action_Node_Ptr;
      Current_Token : Syntax_Trees.Valid_Node_Index;
      --  Current_Token is not Token_Index, because inserted tokens are
      --  virtual, and thus are not in Shared_Parser.Terminals. It is not
      --  Token_ID, because inserted tokens are in Config.Tree, so they can
      --  be made children of nonterms during check parsing.
   end record;

   package Check_Item_Queues is new SAL.Gen_Unbounded_Definite_Queues (Check_Item);

   function Check_One_Item
     (Shared            : not null access Shared_Lookahead;
      Trace             : in out          WisiToken.Trace'Class;
      Table             : in              Parse_Table;
      Parser_Label      : in              Natural;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Check_Item_Queue  : in out          Check_Item_Queues.Queue_Type;
      Shared_Token_Goal : in              Token_Index)
     return Boolean
   is
      --  Perform actions for the first Item in Check_Item_Queue, until
      --  Config.Shared_Token reaches Shared_Token_Goal. If all actions succeed
      --  return True. If any fail, return False. The modified configuration
      --  is always discarded.
      --
      --  If any actions are a conflict, add the conflict action to
      --  Check_Item_Queue.

      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;

      Item         : Check_Item        := Check_Item_Queue.Get;
      Check_Config : Configuration renames Item.Config;
      Check_Token  : Syntax_Trees.Valid_Node_Index renames Item.Current_Token;
      Check_ID     : constant Token_ID := Check_Config.Tree.ID (Check_Token);

      Action : Parse_Action_Node_Ptr :=
        (if Item.Action = null
         then Action_For (Table, Check_Config.Stack.Peek.State, Check_ID)
         else Item.Action);

      New_State          : Unknown_State_Index;
      Nonterm            : Syntax_Trees.Valid_Node_Index;
      Last_Token_Virtual : Boolean := False;
      Keep_Going         : Boolean := True;

      Trace_Prefix : constant String :=
        (if Trace_McKenzie > Detail
         then Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) & Integer'Image (Parser_Label) & ": "
         else "");
   begin
      if Trace_McKenzie > Detail then
         Put ("check  ", Trace, Parser_Label, Check_Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Parser_Label, "   action " & Image (Action.Item, Descriptor));
         end if;
      end if;

      loop
         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Parser_Label, "checking :" & State_Index'Image (Check_Config.Stack.Peek.State) &
                 " : " & Image (Check_ID, Descriptor) &
                 " : " & Image (Action.Item, Descriptor));
         end if;

         if Action.Next /= null then
            if Trace_McKenzie > Detail then
               Put_Line (Trace, Parser_Label, "checking: enqueue conflict " & Image (Action.Next.Item, Descriptor));
            end if;
            Check_Item_Queue.Put ((Check_Config, Action.Next, Check_Token));
         end if;

         case Action.Item.Verb is
         when Shift =>
            Check_Config.Stack.Push
              ((Action.Item.State, Check_Config.Tree.Add_Terminal (Check_ID)));

            --  Get next token
            if Check_Config.Local_Lookahead.Length > 0 and
              Check_Config.Local_Lookahead.Length > Check_Config.Local_Lookahead_Index
            then
               --  These don't count towards Check_Limit
               Check_Config.Local_Lookahead_Index := Check_Config.Local_Lookahead_Index + 1;
               Check_Token := Check_Config.Local_Lookahead (Check_Config.Local_Lookahead_Index);

               --  Check_Token added to Check_Config.Inserted by Do_Shift

               Last_Token_Virtual := True;

            else
               if not Last_Token_Virtual then
                  Check_Config.Shared_Token := Check_Config.Shared_Token + 1;
               end if;
               Last_Token_Virtual := False;

               Check_Token := Check_Config.Tree.Add_Terminal (Shared.Get_Token (Check_Config.Shared_Token));
            end if;

         when Reduce =>
            declare
               Status : constant Semantic_Checks.Check_Status := Reduce_Stack
                 (Check_Config.Stack, Check_Config.Tree, Action.Item, Nonterm, Shared.Shared_Parser.Lexer,
                  Trace, Trace_McKenzie, Trace_Prefix);
            begin
               if Status.Label = Ok then
                  New_State := Check_Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Item.LHS);

                  if New_State = Unknown_State then
                     Keep_Going := False;
                  else
                     Check_Config.Stack.Push ((New_State, Nonterm));
                  end if;
               else
                  if not Try_Semantic_Check_Fixes
                    (Trace, Parser_Label, Table.McKenzie_Param, Shared.Shared_Parser.Lexer,
                     Local_Config_Heap, Check_Config, Status)
                  then
                     Keep_Going := False;
                  end if;
               end if;
            end;

         when Error =>
            Keep_Going := False;

         when Accept_It =>
            null;
         end case;

         exit when not Keep_Going or
           Action.Item.Verb = Accept_It or
           Check_Config.Shared_Token > Shared_Token_Goal;

         Action := Action_For (Table, Check_Config.Stack.Peek.State, Check_ID);
      end loop;

      return Keep_Going;
   end Check_One_Item;

   function Check
     (Shared            : not null access Shared_Lookahead;
      Trace             : in out          WisiToken.Trace'Class;
      Table             : in              Parse_Table;
      Parser_Label      : in              Natural;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in              Configuration;
      Current_Token     : in              Syntax_Trees.Valid_Node_Index)
     return Boolean
   is
      --  Check whether Config is viable; return Ok if parse will succeed
      --  for Check_Limit tokens, Error if not.

      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Shared_Token_Goal : constant Token_Index := Config.Shared_Token + Token_Index (Table.McKenzie_Param.Check_Limit);

      Check_Item_Queue : Check_Item_Queues.Queue_Type; -- Only used for conflicts
      Check_Count      : Integer  := 1;
      Keep_Going       : Boolean;
   begin
      Check_Item_Queue.Clear;
      Check_Item_Queue.Put ((Config, null, Current_Token));

      loop
         Keep_Going := Check_One_Item
           (Shared, Trace, Table, Parser_Label, Local_Config_Heap, Check_Item_Queue, Shared_Token_Goal);

         exit when Keep_Going or Check_Item_Queue.Count = 0;
         Check_Count := Check_Count + 1;

         if Trace_McKenzie > Detail then
            Put_Line (Trace, Parser_Label, "checking: dequeue conflict");
         end if;
      end loop;

      if Trace_McKenzie > Extra then
         Put_Line
           (Trace, Parser_Label,
            "check" & Integer'Image (Check_Count) &
              (if Keep_Going
               then " : succeed"
               else " : fail"));
      end if;

      return Keep_Going;
   end Check;

   procedure Process_One
     (Super         : not null access Supervisor;
      Shared        : not null access Shared_Lookahead;
      Trace         : in out          WisiToken.Trace'Class;
      Table         : in              Parse_Table;
      Parser_Label  :    out          Natural;
      Config_Status :    out          McKenzie_Recover.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type SAL.Base_Peek_Type;

      McKenzie_Param : McKenzie_Param_Type renames Table.McKenzie_Param;
      EOF_ID         : Token_ID renames Trace.Descriptor.EOF_ID;

      Parser_Index  : SAL.Base_Peek_Type;
      Config        : Configuration;
      Current_Input : Syntax_Trees.Valid_Node_Index;
      New_Config    : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      Action_I          : Action_List_Iterator;
   begin
      Super.Get (Parser_Index, Parser_Label, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Config.Local_Lookahead_Index /= Fast_Token_ID_Vectors.No_Index and
        Config.Local_Lookahead_Index <= Config.Local_Lookahead.Last_Index
      then
         Current_Input := Config.Local_Lookahead (Config.Local_Lookahead_Index);
      else
         Current_Input := Config.Tree.Add_Terminal (Shared.Get_Token (Config.Shared_Token));
      end if;

      if Check (Shared, Trace, Table, Parser_Label, Local_Config_Heap, Config, Current_Input) then
         Super.Success (Parser_Index, Config);
         return;
      end if;

      if Config.Local_Lookahead_Index /= Fast_Token_ID_Vectors.No_Index and
        Config.Local_Lookahead_Index <= Config.Local_Lookahead.Last_Index
      then
         --  Config was created by a pattern, and failed before the tokens
         --  inserted by the pattern were consumed. Abandon it, tell Super we
         --  are done with this config.
         Super.Put (Parser_Index, Local_Config_Heap);
         return;
      end if;

      if Trace_McKenzie > Detail then
         Put ("continuing", Trace, Parser_Label, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Parser_Label, Image (Config.Stack, Trace.Descriptor.all, Config.Tree));
         end if;
      end if;

      --  Grouping these operations ensures we know the order to do them in
      --  later in the actual parser, and that there are no duplicate
      --  solutions found.

      if Config.Deleted.Length = 0 and
        Config.Inserted.Length = 0 and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         --  Try deleting stack top
         declare
            Popped_Token : Syntax_Trees.Valid_Node_Index renames Config.Stack.Peek.Token;
            Byte_Region  : Buffer_Region renames Config.Tree.Byte_Region (Popped_Token);
            ID           : Token_ID renames Config.Tree.ID (Popped_Token);
         begin
            New_Config := Config;

            New_Config.Stack.Pop;
            New_Config.Cost := New_Config.Cost +
              (if Byte_Region = Null_Buffer_Region
               then 0
               else McKenzie_Param.Delete (ID));

            New_Config.Popped.Append (ID);
            if Trace_McKenzie > Extra then
               Put_Line (Trace, Parser_Label, "try pop " & Image (ID, Trace.Descriptor.all));
               Put ("enqueue", Trace, Parser_Label, Config);
            end if;

            Local_Config_Heap.Add (New_Config);
         end;
      end if;

      if Config.Deleted.Length = 0 then
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
                     if Trace_McKenzie > Extra then
                        Put_Line (Trace, Parser_Label, "try insert " & Image (ID, Trace.Descriptor.all));
                     end if;

                     New_Config := Config;
                     Do_Shift (Trace, Parser_Label, McKenzie_Param, Local_Config_Heap, New_Config, Action, ID);

                  when Reduce =>
                     if Trace_McKenzie > Extra then
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
            Deleted_Index : constant Token_Index := Shared.Get_Token (Config.Shared_Token);
            Deleted_ID    : constant Token_ID    := Shared.ID (Deleted_Index);
         begin
            if Deleted_ID /= EOF_ID then
               --  can't delete EOF
               New_Config      := Config;
               New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (Deleted_ID);

               New_Config.Deleted.Append (Deleted_ID);
               if Trace_McKenzie > Extra then
                  Put_Line (Trace, Parser_Label, "delete " & Image (Deleted_ID, Trace.Descriptor.all));
               end if;

               New_Config.Shared_Token := Shared.Get_Token (New_Config.Shared_Token + 1);

               Local_Config_Heap.Add (New_Config);
            end if;
         end;
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   end Process_One;

   ----------
   --  Top level

   task type Worker_Task
     (Super  : not null access Supervisor;
      Shared : not null access Shared_Lookahead;
      Trace  : not null access WisiToken.Trace'Class;
      Table  : not null access Parse_Table)
   is
      entry Start;
      --  Start getting parser/configs to check from Config_Store.

      entry Done;
      --  Available when task is ready to terminate; after this rendezvous,
      --  task discriminants may be freed.

   end Worker_Task;

   task body Worker_Task
   is
      Status       : Config_Status;
      Parser_Label : Natural;
   begin
      accept Start;

      loop
         Process_One (Super, Shared, Trace.all, Table.all, Parser_Label, Status);

         exit when Status = All_Done;
      end loop;

      accept Done;
   exception
   when E : others =>
      Super.Fatal (Parser_Label, E);
   end Worker_Task;

   procedure Enqueue
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Data         : in out McKenzie_Data;
      Config       : in     Configuration)
   is begin
      --  This may only be called from Recover_Init (or procedures it
      --  calls), since it writes to Data without going thru Config_Store.

      --  [1] has a check for duplicate configs here; that only happens with
      --  higher costs, which take too long for our application.
      if Trace_McKenzie > Detail then
         Put ("enqueue", Trace, Parser_Label, Config);
      end if;
      Data.Config_Heap.Add (Config);
      Data.Enqueue_Count := Data.Enqueue_Count + 1;
   end Enqueue;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Pattern_1;
      Parser_State : in out Parser_Lists.Parser_State;
      Error        : in     Parse_Error;
      Root_Config  : in     Configuration;
      Trace        : in out WisiToken.Trace'Class)
   is
      use all type Parse_Error_Label;
      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;
   begin
      if Error.Label = LR.Action and then
        (Parser_State.Tree.ID (Parser_State.Stack.Peek.Token) = Pattern.Stack and
           Parser_State.Tree.ID (Error.Error_Token) = Pattern.Error and
           Error.Expecting (Pattern.Expecting) and
           Count (Error.Expecting) = 1)
      then
         if Trace_McKenzie > Outline then
            Trace.Put_Line
              ("special rule recover_pattern_1 " &
                 Image (Pattern.Stack, Descriptor) & ", " &
                 Image (Pattern.Error, Descriptor) & ", " &
                 Image (Pattern.Expecting, Descriptor) &
                 " matched.");
         end if;

         declare
            Config : Configuration := Root_Config;
         begin
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Stack));
            Config.Inserted.Prepend (Pattern.Stack);
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Error));
            Config.Inserted.Prepend (Pattern.Error);
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Expecting));
            Config.Inserted.Prepend (Pattern.Expecting);
            Config.Local_Lookahead_Index := 1;

            Enqueue (Trace, Parser_State.Label, Parser_State.Recover, Config);
         end;
      end if;
   end Apply_Pattern;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Pattern_2;
      Parser_State : in out Parser_Lists.Parser_State;
      Error        : in     Parse_Error;
      Root_Config  : in     Configuration;
      Trace        : in out WisiToken.Trace'Class)
   is
      use all type Parse_Error_Label;
      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;
   begin
      if Error.Label = LR.Action and then
        (Parser_State.Tree.ID (Parser_State.Stack.Peek.Token) = Pattern.Stack and
           Parser_State.Tree.ID (Error.Error_Token) = Pattern.Error and
           Error.Expecting (Pattern.Expecting) and
           Count (Error.Expecting) = 1)
      then
         if Trace_McKenzie > Outline then
            Trace.Put_Line
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
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Stack));
            Config.Inserted.Prepend (Pattern.Stack);
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Insert));
            Config.Inserted.Prepend (Pattern.Insert);
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Expecting));
            Config.Inserted.Prepend (Pattern.Expecting);
            Config.Local_Lookahead_Index := 1;

            Enqueue (Trace, Parser_State.Label, Parser_State.Recover, Config);
         end;
      end if;
   end Apply_Pattern;

   procedure Apply_Pattern
     (Pattern      : in     Recover_End_EOF;
      Parser_State : in out Parser_Lists.Parser_State;
      Error        : in     Parse_Error;
      Root_Config  : in     Configuration;
      Shared       : in     Shared_Lookahead;
      Trace        : in out WisiToken.Trace'Class)
   is
      use all type Parse_Error_Label;
      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;
   begin
      if Error.Label = LR.Action and then
        (Parser_State.Tree.ID (Error.Error_Token) = Pattern.Error and
           Error.Expecting (Descriptor.EOF_ID))
      then
         if Trace_McKenzie > Outline then
            Trace.Put_Line
              ("special rule recover_end_eof " &
                 Image (Pattern.Error, Descriptor) & ", " &
                 Image (Pattern.Delete_Thru, Descriptor) &
                 " matched.");
         end if;

         declare
            use all type SAL.Base_Peek_Type;
            Config     : Configuration := Root_Config;
            Deleted_Index : Token_Index;
            Deleted_ID : Token_ID;
         begin
            loop
               Deleted_Index := Shared.Get_Token (Config.Shared_Token);
               Deleted_ID    := Shared.ID (Deleted_Index);

               Config.Shared_Token := Config.Shared_Token + 1;
               Config.Deleted.Append (Deleted_ID);
               exit when Deleted_ID = Descriptor.EOF_ID or Deleted_ID = Pattern.Delete_Thru;
            end loop;

            Enqueue (Trace, Parser_State.Label, Parser_State.Recover, Config);
         end;
      end if;
   end Apply_Pattern;

   procedure Apply_Pattern
     (Pattern      : in     Recover_Block_Mismatched_Names;
      Super        : in out Supervisor;
      Lexer        : in     WisiToken.Lexer.Handle;
      Parser_State : in out Parser_Lists.Parser_State;
      Error        : in     Parse_Error;
      Root_Config  : in out Configuration;
      Trace        : in out WisiToken.Trace'Class)
   is
      use all type SAL.Base_Peek_Type;
      use all type Parse_Error_Label;

      procedure Do_Fix
      is
         End_Node_Index : constant Syntax_Trees.Valid_Node_Index := Error.Tokens (Error.Tokens.Last_Index);

         End_Name : constant String := Lexer.Buffer_Text (Parser_State.Tree.Name_Region (End_Node_Index));

         Matching_Name_Index : SAL.Peek_Type := 2;
         Begin_Count         : Integer;
      begin
         --  First see if there is a matching name. Top of stack is the reduced
         --  block containing the name that does not match.
         Find_Matching_Name (Root_Config, Lexer, End_Name, Pattern.Begin_ID, Matching_Name_Index, Begin_Count);

         if Matching_Name_Index = Root_Config.Stack.Depth then
            --  Did not find matching name; assume user is editing names, so ignore.
            Root_Config.Semantic_Check_Fixes (Error.Code) := True;
            Super.Force_Done (Parser_State.Label, Root_Config);
            return;
         end if;

         declare
            Config : Configuration := Root_Config;
         begin
            --  Pop block with mismatched names
            Config.Popped.Append (Config.Tree.ID (Config.Stack.Pop.Token));

            --  Replace the popped block
            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Semicolon_ID));
            Config.Inserted.Prepend (Pattern.Semicolon_ID);

            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.End_ID));
            Config.Inserted.Prepend (Pattern.End_ID);

            Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Begin_ID));
            Config.Inserted.Prepend (Pattern.Begin_ID);

            --  Insert one missing 'end;' for each extra 'begin'.
            for I in 1 .. Begin_Count loop
               Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.Semicolon_ID));
               Config.Inserted.Prepend (Pattern.Semicolon_ID);

               Config.Local_Lookahead.Prepend (Config.Tree.Add_Terminal (Pattern.End_ID));
               Config.Inserted.Prepend (Pattern.End_ID);
            end loop;

            Config.Local_Lookahead_Index := Config.Local_Lookahead.First_Index;

            Config.Cost := 0;

            Config.Semantic_Check_Fixes (Error.Code) := True;
            Root_Config.Semantic_Check_Fixes (Error.Code) := True; -- Avoid duplication.

            Enqueue (Trace, Parser_State.Label, Parser_State.Recover, Config);
         end;
      end Do_Fix;

   begin
      if Error.Label = LR.Check then
         if Trace_McKenzie > Outline then
            Trace.Put_Line
              ("special rule recover_block_mismatched_names " & Semantic_Checks.Error_Label'Image (Error.Code) &
                 " matched.");
         end if;

         case Error.Code is
         when Semantic_Checks.Match_Names_Error =>
            --  One case is the user editing names; the fix is to make one of the
            --  names match the other. We can't actually do that without editing
            --  the buffer, so we just ignore the error, and say the Root_Config
            --  succeeds. See test_mckenzie_recover.adb Pattern_End_EOF,
            --  propagate_names.ada_lite Proc_2.
            --
            --  Another case is a missing 'end'; same fix as Extra_Name_Error
            --  below. See test_ada_lite.adb Propagate_Names
            Do_Fix;

         when Semantic_Checks.Missing_Name_Error =>
            raise Programmer_Error with "found test case for Apply_Pattern Missing_Name_Error";

         when Semantic_Checks.Extra_Name_Error =>
            --  See test_mckenzie_recover.adb Pattern_Block_Missing_Name_1.
            Do_Fix;

         end case;
      end if;
   end Apply_Pattern;

   procedure Patterns
     (Super         : in out Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Shared        : in     Shared_Lookahead;
      Parser_State  : in out Parser_Lists.Parser_State;
      Root_Config   : in out Configuration;
      Trace         : in out WisiToken.Trace'Class)
   is
      Param : McKenzie_Param_Type renames Shared_Parser.Table.McKenzie_Param;
      Error : constant Parse_Error := Parse_Error_Lists.Element (Parser_State.Errors.Last);
   begin
      for Pattern of Param.Patterns loop
         if Pattern in Recover_Pattern_1'Class then
            Apply_Pattern (Recover_Pattern_1 (Pattern), Parser_State, Error, Root_Config, Trace);

         elsif Pattern in Recover_Pattern_2'Class then
            Apply_Pattern (Recover_Pattern_2 (Pattern), Parser_State, Error, Root_Config, Trace);

         elsif Pattern in Recover_End_EOF'Class then
            Apply_Pattern (Recover_End_EOF (Pattern), Parser_State, Error, Root_Config, Shared, Trace);

         elsif Pattern in Recover_Block_Mismatched_Names'Class then
            Apply_Pattern
              (Recover_Block_Mismatched_Names (Pattern), Super, Shared_Parser.Lexer,
               Parser_State, Error, Root_Config, Trace);
         end if;
      end loop;
   end Patterns;

   procedure Recover_Init
     (Super         : in out Supervisor;
      Shared_Parser : in out LR.Parser.Parser;
      Shared        : in     Shared_Lookahead;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      Trace : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;
   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser" & Integer'Image (Parser_State.Label) & ": Current_Token " &
              Image (Parser_State.Tree.Base_Token (Parser_State.Current_Token), Trace.Descriptor.all));
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Parser_State.Label, Image (Parser_State.Stack, Trace.Descriptor.all, Parser_State.Tree));
         end if;
      end if;

      --  All initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize, except here we add the initial configs to
      --  check.

      --  The special rules are not guaranteed to work when matched, so
      --  always queue the original error condition.
      declare
         Orig : Configuration := (others => <>);
      begin
         Orig.Stack := Parser_State.Stack;

         Orig.Tree.Initialize (Parser_State.Tree'Unchecked_Access);
         --  Orig has the same  or shorter lifetime Parser_State does.
         --  FIXME: try making Parser_State aliased or access.

         --  Parser_State.Local_Lookahead must be empty (else we would not get
         --  here). Therefore Parser_State current token is in
         --  Shared_Parser.Shared_Token.

         Orig.Shared_Token := Parser_State.Shared_Token;

         Patterns (Super, Shared_Parser, Shared, Parser_State, Orig, Trace);

         --  A pattern may have set Orig.Semantic_Check_Fixes.
         Enqueue (Trace, Parser_Lists.Label (Parser_State), Parser_State.Recover, Orig);
      end;
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;

      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Super : aliased Supervisor
        (Trace'Access,
         Parsers'Access,
         Cost_Limit   => Shared_Parser.Table.McKenzie_Param.Cost_Limit,
         Parser_Count => Parsers.Count);

      Shared : aliased Shared_Lookahead (Shared_Parser'Access);

      Worker_Tasks   : array (1 .. System.Multiprocessors.Number_Of_CPUs - 1) of Worker_Task
        (Super'Access, Shared'Access, Shared_Parser.Semantic_State.Trace, Shared_Parser.Table.all'Access);
      --  Keep one CPU free for this main task, and the user.

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
         Recover_Init (Super, Shared_Parser, Shared, Parser_State);
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
                           declare
                              Msg : constant String :=
                                "spawn parser" & Integer'Image (Parsers.First.Label) & " from " &
                                Int_Image (Cur.Label) & " (" & Int_Image (Integer (Parsers.Count)) & " active)";
                           begin
                              Put (Msg, Trace, 0, Data.Results.Peek, Include_Task_ID => False);
                           end;
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline then
                     Put ("  ", Trace, Data.Parser_Label, Data.Results.Peek, Include_Task_ID => False);
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
               use Parser_Lists;
               Data   : McKenzie_Data renames Parser_State.Recover;
               Result : Configuration renames Data.Results.Peek;
            begin
               for ID of Result.Popped loop
                  Parser_State.Stack.Pop;
               end loop;

               for I in 1 .. Result.Pushed.Depth loop
                  raise Programmer_Error with "need to copy Tree node(s?) from Result.Tree to Parser_State.Tree";
                  --  FIXME: delete or fix
                  --  declare
                  --     Item : constant Parser_Stack_Item := Result.Pushed.Peek (I);
                  --  begin
                  --     Parser_State.Stack.Push (Item);
                  --  end;
               end loop;

               for ID of reverse Result.Inserted loop
                  Parser_State.Local_Lookahead.Add_To_Head (Parser_State.Tree.Add_Terminal (ID));
               end loop;

               --  FIXME: set Error.Recover

               if Parser_State.Local_Lookahead.Count > 0 then
                  Parser_State.Current_Token := Parser_State.Local_Lookahead.Get;

                  Parser_State.Current_Token_Is_Virtual := True;

               elsif Result.Deleted.Length > 0 then
                  Parser_State.Shared_Token  := Result.Shared_Token;
                  Parser_State.Current_Token := Parser_State.Tree.Add_Terminal (Parser_State.Shared_Token);

                  Parser_State.Current_Token_Is_Virtual := False;

               else
                  --  No change in Parser_State.Current_Token
                  Parser_State.Current_Token_Is_Virtual := False;
               end if;
            end;
         end if;
      end loop;

      if Trace_McKenzie > Extra then
         Shared_Parser.Semantic_State.Put;
      end if;

      Cleanup;

      return Super.Recover_Result;

   exception
   when others =>
      Cleanup;
      raise;
   end Recover;

end WisiToken.LR.McKenzie_Recover;
