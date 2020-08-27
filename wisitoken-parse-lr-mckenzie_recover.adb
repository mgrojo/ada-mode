--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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
with Ada.Unchecked_Deallocation;
with System.Multiprocessors;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
with WisiToken.Parse.LR.McKenzie_Recover.Explore;
with WisiToken.Parse.LR.Parser_Lists;
package body WisiToken.Parse.LR.McKenzie_Recover is
   use all type System.Multiprocessors.CPU_Range;

   type Supervisor_Access is access all Base.Supervisor;
   type Shared_Access is access all Base.Shared;

   task type Worker_Task is
      entry Start
        (ID     : in Integer;
         Super  : in Supervisor_Access;
         Shared : in Shared_Access);
      --  Start getting parser/configs to check from Super. Stop when
      --  Super reports All_Done;

      entry Done;
      --  Available after Super has reported All_Done.
   end Worker_Task;

   type Worker_Access is access Worker_Task;
   procedure Free is new Ada.Unchecked_Deallocation (Worker_Task, Worker_Access);

   task body Worker_Task
   is
      use all type Base.Config_Status;
      Super  : Supervisor_Access;
      Shared : Shared_Access;

      Status : Base.Config_Status := Valid;
   begin
      loop
         select
            accept Start
              (ID     : in Integer;
               Super  : in Supervisor_Access;
               Shared : in Shared_Access)

            do
               Task_Attributes.Set_Value (ID);
               Worker_Task.Super  := Super;
               Worker_Task.Shared := Shared;
            end Start;
         or
            terminate;
         end select;

         loop
            Explore.Process_One (Super, Shared, Status);
            exit when Status = All_Done;
         end loop;

         accept Done;

         Super  := null;
         Shared := null;
      end loop;

   exception
   when E : others =>
      Super.Fatal (E);
   end Worker_Task;

   Worker_Tasks : array (1 .. System.Multiprocessors.CPU_Range'Max (1, System.Multiprocessors.Number_Of_CPUs)) of
     Worker_Access;
   --  Declaring an array of tasks directly causes a circular elaboration
   --  problem, and would mean a task that terminates due to an exception
   --  is never restarted.

   procedure To_Recover
     (Parser_Stack : in     Syntax_Trees.Stream_ID;
      Tree         : in     Syntax_Trees.Tree;
      Stack        : in out Recover_Stacks.Stack)
   is
      Depth : constant SAL.Peek_Type := Tree.Stream_Length (Parser_Stack);
   begin
      pragma Assert (Stack.Depth = 0);
      if Stack.Size < Depth then
         raise SAL.Programmer_Error with "recover stack needs more space;" & Depth'Image;
      end if;
      for I in reverse 1 .. Depth loop
         declare
            Node : constant Syntax_Trees.Node_Access  := Tree.Get_Node (Parser_Stack, Tree.Peek (Parser_Stack, I));
            Token   : constant Syntax_Trees.Recover_Token :=
              (if I = Depth then (others => <>) else Tree.Get_Recover_Token (Node));
         begin
            Stack.Push ((Tree.State (Node), Node, Token));
         end;
      end loop;
   end To_Recover;

   procedure Recover_Init
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Trace  : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Config : Configuration;
      Error  : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
   begin
      Parser_State.Recover.Enqueue_Count := Parser_State.Recover.Enqueue_Count + 1;

      Config.Resume_Token_Goal := Shared_Parser.Tree.Get_Element_Index (Parser_State.Shared_Token) +
        Shared_Parser.Table.McKenzie_Param.Check_Limit;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
              ": State" & Shared_Parser.Tree.State (Parser_State.Stream)'Image &
              " Current_Token " & Shared_Parser.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) &
              " Resume_Token_Goal" & Config.Resume_Token_Goal'Image);
         Trace.Put_Line
           ((case Error.Label is
             when Action => "Action",
             when Check => "Check, " & Semantic_Checks.Image (Error.Check_Status, Trace.Descriptor.all),
             when Message => raise SAL.Programmer_Error));
         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Shared_Parser.Tree, Parser_State.Stream, "stack: " & Parser_Lists.Image
                 (Parser_State.Stream, Trace.Descriptor.all, Shared_Parser.Tree));
         end if;
      end if;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      To_Recover (Parser_State.Stream, Shared_Parser.Tree, Config.Stack);

      --  Parser_State.Recover_Insert_Delete must be empty (else we would not get
      --  here). Therefore Parser_State current token is in
      --  Shared_Parser.Shared_Token.

      Config.Current_Shared_Token := Parser_State.Shared_Token;

      case Error.Label is
      when Action =>
         Config.Error_Token := Shared_Parser.Tree.Get_Recover_Token (Error.Error_Token);
         if Trace_McKenzie > Detail then
            Put ("enqueue", Trace, Shared_Parser.Tree, Parser_State.Stream, Config, Task_ID => False);
         end if;

      when Check =>
         if Shared_Parser.Language_Fixes = null then
            --  The only fix is to ignore the error.
            if Trace_McKenzie > Detail then
               Config.Strategy_Counts (Ignore_Error) := 1;
               Put ("enqueue", Trace, Shared_Parser.Tree, Parser_State.Stream, Config, Task_ID => False);
            end if;

         else
            --  Undo the reduction that encountered the error, let Process_One
            --  enqueue possible solutions. We leave the cost at 0, since this is
            --  the root config. Later logic will enqueue the 'ignore error'
            --  solution; see McKenzie_Recover.Explore Process_One.

            Config.Check_Status      := Error.Check_Status;
            Config.Error_Token       := Config.Stack.Peek.Token;
            Config.Check_Token_Count := Undo_Reduce (Config.Stack, Shared_Parser.Tree);

            Config_Op_Arrays.Append (Config.Ops, (Undo_Reduce, Config.Error_Token.ID, Config.Check_Token_Count));

            if Trace_McKenzie > Detail then
               Put ("undo_reduce " & Image
                      (Config.Error_Token.ID, Trace.Descriptor.all), Trace, Shared_Parser.Tree, Parser_State.Stream,
                    Config, Task_ID => False);
            end if;
         end if;

      when Message =>
         --  Last error entry should be the failure that caused us to enter
         --  recovery.
         raise SAL.Programmer_Error;
      end case;

      Parser_State.Recover.Config_Heap.Add (Config);
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      use all type Parser.Post_Recover_Access;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Skip_Next : Boolean := False;

      Super : aliased Base.Supervisor
        (Trace'Access,
         Shared_Parser.Tree'Access,
         Check_Delta_Limit => Shared_Parser.Table.McKenzie_Param.Check_Delta_Limit,
         Enqueue_Limit     => Shared_Parser.Table.McKenzie_Param.Enqueue_Limit,
         Parser_Count      => Parsers.Count);

      Shared : aliased Base.Shared
        (Shared_Parser.Lexer.all'Access,
         Shared_Parser.Table,
         Shared_Parser.Language_Fixes,
         Shared_Parser.Language_Matching_Begin_Tokens,
         Shared_Parser.Language_String_ID_Set,
         Shared_Parser.Wrapped_Lexer_Errors'Access,
         Shared_Parser.Line_Begin_Token'Access);

      Task_Count : constant System.Multiprocessors.CPU_Range :=
        (if Shared_Parser.Table.McKenzie_Param.Task_Count = 0
         then Worker_Tasks'Last
         else Shared_Parser.Table.McKenzie_Param.Task_Count);

   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
      end if;

      Super.Initialize (Parsers'Unrestricted_Access);

      for Parser_State of Parsers loop
         Recover_Init (Shared_Parser, Parser_State);
      end loop;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (Task_Count'Image & " parallel tasks");
      end if;

      for I in Worker_Tasks'First .. Task_Count loop
         if Worker_Tasks (I) = null then
            Worker_Tasks (I) := new Worker_Task;
            if Debug_Mode then
               Trace.Put_Line ("new Worker_Task" & System.Multiprocessors.CPU_Range'Image (I));
            end if;

         elsif Worker_Tasks (I)'Terminated then
            Free (Worker_Tasks (I));
            Worker_Tasks (I) := new Worker_Task;
            if Debug_Mode then
               Trace.Put_Line ("recreated Worker_Task" & System.Multiprocessors.CPU_Range'Image (I));
            end if;
         end if;

         Worker_Tasks (I).Start (Integer (I), Super'Unchecked_Access, Shared'Unchecked_Access);
      end loop;

      declare
         use Ada.Exceptions;
         ID      : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Super.Done (ID, Message); -- Wait for all parsers to fail or succeed

         --  Ensure all worker tasks stop getting configs before proceeding;
         --  otherwise local variables disappear while the task is still trying
         --  to access them.
         for I in Worker_Tasks'First .. Task_Count loop
            if not Worker_Tasks (I)'Terminated then
               Worker_Tasks (I).Done;
            end if;
         end loop;

         if ID /= Null_Id then
            Raise_Exception (ID, -Message);
         end if;
      end;

      --  Spawn new parsers for multiple solutions.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course.
      --
      --  We could try to check here for redundant solutions; configs for a
      --  parser that have the same or "equivalent" ops. But those will be
      --  caught in the main parse by the check for duplicate state; doing
      --  the same check here is premature optimization.
      declare
         use Parser_Lists;

         Cur         : Cursor             := Parsers.First;
         Solutions   : SAL.Base_Peek_Type := 0;
         Spawn_Limit : SAL.Base_Peek_Type := Shared_Parser.Max_Parallel; -- per parser
      begin
         for Parser of Parsers loop
            if Parser.Recover.Success then
               Solutions := Solutions + Parser.Recover.Results.Count;
            end if;
         end loop;

         if Solutions > Shared_Parser.Max_Parallel and Trace_McKenzie > Outline then
            Trace.Put_Line ("too many parallel parsers required in recover; dropping some solutions");
            Spawn_Limit := Shared_Parser.Max_Parallel / Parsers.Count;
         end if;

         loop
            declare
               Data : McKenzie_Data renames Cur.State_Ref.Recover;
            begin
               if Data.Success then
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Cur.Stream) &
                          ": succeed" & SAL.Base_Peek_Type'Image (Data.Results.Count) &
                          ", enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost: " & Integer'Image (Data.Results.Min_Key));
                  end if;

                  if Data.Results.Count > 1 then
                     for I in 1 .. SAL.Base_Peek_Type'Min (Spawn_Limit, Data.Results.Count - 1) loop
                        Parsers.Prepend_Copy (Cur, Shared_Parser.Tree, Shared_Parser.User_Data, Trace);
                        --  Does not copy recover.

                        if Trace_McKenzie > Outline or Trace_Parse > Outline then
                           Trace.Put_Line
                             ("spawn parser " & Shared_Parser.Tree.Trimmed_Image (Parsers.First.Stream) & " from " &
                                Shared_Parser.Tree.Trimmed_Image (Cur.Stream) & " (" &
                                Trimmed_Image (Integer (Parsers.Count)) &
                                " active)");
                           Put ("", Trace, Shared_Parser.Tree, Parsers.First.Stream,
                                Data.Results.Peek, Task_ID => False, Strategy => True);
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline or Trace_Parse > Outline then
                     Put ("", Trace, Shared_Parser.Tree, Cur.Stream, Data.Results.Peek,
                          Task_ID => False, Strategy => True);
                  end if;
               else
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Cur.Stream) &
                          ": fail, enqueue" & Integer'Image (Data.Enqueue_Count) &
                          (if Data.Config_Full_Count > 0 then ", config_full" & Data.Config_Full_Count'Image else "") &
                          ", check " & Integer'Image (Data.Check_Count));
                  end if;
               end if;

            end;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      --  Edit Parser_State to apply solutions.

      --  We don't use 'for Parser_State of Parsers loop' here,
      --  because we might need to terminate a parser.
      declare
         Current_Parser : Parser_Lists.Cursor := Parsers.First;
      begin
         loop
            exit when Current_Parser.Is_Done;

            if Current_Parser.State_Ref.Recover.Success then
               begin
                  --  Can't have active 'renames State_Ref' when terminate a parser
                  declare
                     use Parser_Lists;
                     use Config_Op_Arrays, Config_Op_Array_Refs;

                     Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;

                     Descriptor : WisiToken.Descriptor renames Shared_Parser.Trace.Descriptor.all;
                     Stack      : Syntax_Trees.Stream_ID renames Parser_State.Stream;
                     Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
                     Data       : McKenzie_Data renames Parser_State.Recover;
                     Result     : Configuration renames Data.Results.Peek;

                     Error     : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
                     Error_Pos : constant Buffer_Pos :=
                       (case Error.Label is
                        when Action  => Tree.Base_Token (Error.Error_Token).Char_Region.First,
                        when Check   => Error.Check_Status.Begin_Name.Byte_Region.First,
                        when Message => raise SAL.Programmer_Error);

                     Stack_Matches_Ops     : Boolean := True;
                     Shared_Token_Changed  : Boolean := False;
                     Current_Token_Virtual : Boolean := False;
                     First_Insert          : Boolean := True;
                  begin
                     --  The verb will be reset by the main parser; just indicate the
                     --  parser recovered from the error.
                     Parser_State.Set_Verb (Shift);

                     Parser_State.Errors (Parser_State.Errors.Last).Recover := Result;

                     Parser_State.Resume_Token_Goal := Result.Resume_Token_Goal;

                     if Trace_McKenzie > Extra then
                        Put_Line (Trace, Tree, Parser_State.Stream, "before Ops applied:", Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "stack " & Image (Stack, Descriptor, Tree),
                           Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "Shared_Token  " & Tree.Image
                             (Parser_State.Shared_Token, Descriptor),
                           Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "Current_Token " & Tree.Image
                             (Parser_State.Current_Token, Descriptor),
                           Task_ID => False);
                     end if;

                     --  We don't apply all Ops to the parser stack here, because there can
                     --  be other input tokens between the inserts and deletes, and there
                     --  can be conflicts; we let the main parser handle that. We can apply
                     --  all ops up to the first insert.
                     --
                     --  Other than Add_Terminal, there's no need to modify
                     --  Shared_Parser.Tree. Any tree nodes created by the failed parse that
                     --  are pushed back are useful for error repair, and will just be
                     --  ignored in future parsing. This also avoids enlarging a
                     --  non-flushed branched tree, which saves time and space.
                     --
                     --  Language_Fixes may abuse the rules about adding Ops, so we check
                     --  that as much as is reasonable here. We use Assert to get an
                     --  immediate error in a debug build, and raise Bad_Config to avoid
                     --  further corruption in a release build.

                     for I in First_Index (Result.Ops) .. Last_Index (Result.Ops) loop
                        declare
                           use all type WisiToken.Syntax_Trees.Node_Label;
                           Op : Config_Op renames Constant_Ref (Result.Ops, I);
                        begin
                           case Op.Op is
                           when Fast_Forward =>
                              --  The parser would do shifts and reduces for the tokens we are
                              --  skipping here
                              Stack_Matches_Ops := False;

                           when Undo_Reduce =>
                              --  If Stack_Matches_Ops, we must do the Stack.Pop and Pushes, and we
                              --  can use Stack.Peek to check if the Undo_Reduce is valid.
                              --
                              --  If not Stack_Matches_Ops, we have to assume Undo_Reduce is valid.
                              --
                              --  See test_mckenzie_recover.adb Extra_Begin for an example of Undo_Reduce
                              --  after other ops.
                              if Stack_Matches_Ops then

                                 --  We can't use McKenzie_Recover.Undo_Reduce_Valid here; that takes a
                                 --  Config stack, not a parser stack. So we duplicate it.
                                 if not (Nonterm = Tree.Label (Tree.Peek (Stack)) and
                                           (I = First_Index (Result.Ops) or else
                                              Push_Back_Valid
                                                (Tree,
                                                 Tree.First_Shared_Terminal (Stack, Tree.Peek (Stack)),
                                                 Result.Ops, I - 1)))
                                 then
                                    if Trace_McKenzie > Outline then
                                       Put_Line
                                         (Trace, Tree, Parser_State.Stream, "invalid Undo_Reduce in apply config",
                                          Task_ID => False);
                                    end if;
                                    raise Bad_Config;
                                 end if;

                                 Tree.Undo_Reduce (Stack);
                              end if;

                           when Push_Back =>
                              --  If Stack_Matches_Ops, we must do the Stack.Pop, and can use that
                              --  to check if the Push_Back is valid.
                              --
                              --  If not Stack_Matches_Ops, we have to assume Op.PB_Token_Index is
                              --  correct, and we do not do Stack.Pop. We can still check the target
                              --  token index against the previous ops.
                              --
                              --  See test_mckenzie_recover.adb Error_2 for an example of Push_Back
                              --  after other ops.
                              if not
                                (I = First_Index (Result.Ops) or else
                                   Push_Back_Valid
                                     (Tree,
                                      Target_Token_Index =>
                                        (if Stack_Matches_Ops
                                         then Tree.First_Shared_Terminal (Parser_State.Stream, Tree.Peek (Stack))
                                         else Op.PB_Token_Index),
                                      Ops     => Result.Ops,
                                      Prev_Op => I - 1))
                              then
                                 if Trace_McKenzie > Outline then
                                    Put_Line
                                      (Trace, Tree, Parser_State.Stream,
                                       "invalid Push_Back in apply config op" & I'Image,
                                       Task_ID => False);
                                 end if;
                                 pragma Assert (False);
                                 raise Bad_Config;
                              end if;

                              if Stack_Matches_Ops then
                                 Tree.Pop (Stack);

                                 if Op.PB_Token_Index /= Syntax_Trees.Invalid_Stream_Index then
                                    --  Pushing back an empty nonterm has no effect on the input stream.
                                    Parser_State.Shared_Token := Op.PB_Token_Index;
                                    Shared_Token_Changed      := True;
                                 end if;
                              end if;

                           when Insert            =>
                              Recover_Op_Arrays.Append
                                (Parser_State.Recover_Insert_Delete,
                                 (Op              => Insert,
                                  Error_Pos       => Error_Pos,
                                  Ins_ID          => Op.Ins_ID,
                                  Ins_Before      => Op.Ins_Before,
                                  Ins_Node        => Syntax_Trees.Invalid_Node_Access,
                                  Ins_Before_Node => Syntax_Trees.Invalid_Node_Access));

                              if Parser_State.Recover_Insert_Delete_Current = No_Index then
                                 Parser_State.Recover_Insert_Delete_Current :=
                                   Recover_Op_Arrays.Last_Index (Parser_State.Recover_Insert_Delete);
                              end if;

                              if First_Insert and Op.Ins_Before = Parser_State.Shared_Token then
                                 --  We need First_Insert here, not just Stack_Matches_Ops, when the
                                 --  first insert is preceeded only by Push_Back and Undo_Reduce, with
                                 --  at least one Undo_Reduce (so Stack_Matches_Ops is False when we
                                 --  get here). See test_mckenzie_recover.adb Missing_Name_3

                                 First_Insert := False;

                                 Parser_State.Current_Token := Tree.Insert_Terminal
                                   (Parser_State.Stream, Op.Ins_ID, Op.Ins_Before);

                                 --  Normally Insert is completed by Stack.Push; we let the main parser
                                 --  do that.
                                 Stack_Matches_Ops := False;

                                 Recover_Op_Array_Refs.Variable_Ref
                                   (Parser_State.Recover_Insert_Delete,
                                    Recover_Op_Arrays.Last_Index (Parser_State.Recover_Insert_Delete)).Ins_Node :=
                                      Tree.Get_Node (Parser_State.Stream, Parser_State.Current_Token);

                                 Current_Token_Virtual                      := True;
                                 Parser_State.Recover_Insert_Delete_Current := No_Index;
                              else
                                 --  Let main parser handle it
                                 null;
                              end if;

                           when Delete           =>
                              Recover_Op_Arrays.Append
                                (Parser_State.Recover_Insert_Delete,
                                 (Op             => Delete,
                                  Error_Pos      => Error_Pos,
                                  Del_ID         => Op.Del_ID,
                                  Del_Index      => Op.Del_Token_Index,
                                  Del_Node       => Tree.Get_Node (Op.Del_Token_Index),
                                  Del_After_Node => Syntax_Trees.Invalid_Node_Access));

                              --  We don't check Stack_Matches_Ops here; if the current token needs
                              --  to be deleted, this is the only chance to do that. See
                              --  ada_mode-recover_02.adb with LR1 parser for example.
                              if Op.Del_Token_Index = Parser_State.Shared_Token then
                                 --  Delete has no effect on Stack, so we can apply multiple deletes.
                                 Parser_State.Shared_Token := Tree.Stream_Next (Op.Del_Token_Index);
                                 Shared_Token_Changed      := True;

                                 Parser_State.Recover_Insert_Delete_Current := No_Index;
                              else
                                 if Parser_State.Recover_Insert_Delete_Current = No_Index then
                                    Parser_State.Recover_Insert_Delete_Current :=
                                      Recover_Op_Arrays.Last_Index (Parser_State.Recover_Insert_Delete);
                                 end if;

                              end if;

                           end case;
                        end;
                     end loop;

                     --  If not Shared_Token_Changed, Shared_Token is the error token,
                     --  which is the next token to read. If Shared_Token_Changed, we have
                     --  set Shared_Token consistent with that; it is the next token to
                     --  read. If Current_Token_Virtual, then after all the virtual tokens
                     --  are inserted, the main parser would normally increment
                     --  Parser_State.Shared_Token to get the next token, but we don't want
                     --  that now. We could set Shared_Token to 1 less, but this way the
                     --  debug messages all show the expected Shared_Terminal.

                     Parser_State.Inc_Shared_Token := not Current_Token_Virtual;

                     --  The main parser always sets Current_Token to be the syntax tree
                     --  node containing Shared_Token; ensure that is true here (virtual
                     --  tokens where handled above).

                     if (not Current_Token_Virtual) and Shared_Token_Changed then
                        Parser_State.Current_Token := Parser_State.Shared_Token;
                     end if;

                     if Trace_McKenzie > Extra then
                        Put_Line (Trace, Tree, Parser_State.Stream, "after Ops applied:", Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "stack " & Parser_Lists.Image (Stack, Descriptor, Tree),
                           Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "Shared_Token  " & Tree.Image
                             (Parser_State.Shared_Token, Descriptor), Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "Current_Token " & Tree.Image
                             (Parser_State.Current_Token, Descriptor), Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "recover_insert_delete" &
                             Parser_State.Recover_Insert_Delete_Current'Image & ":" &
                             Image (Parser_State.Recover_Insert_Delete, Descriptor), Task_ID => False);
                        Put_Line
                          (Trace, Tree, Parser_State.Stream, "inc_shared_token " &
                             Boolean'Image (Parser_State.Inc_Shared_Token),
                           Task_ID => False);
                     end if;
                  end;
               exception
               when Bad_Config =>
                  if Parsers.Count = 1 then
                     --  Oops. just give up
                     return Fail_Programmer_Error;
                  end if;
                  Parsers.Terminate_Parser (Current_Parser, Shared_Parser.Tree, "bad config in recover", Trace);
                  --  Terminate advances Current_Parser
                  Skip_Next := True;
               end;
            end if;
            if Skip_Next then
               Skip_Next := False;
            else
               Current_Parser.Next;
            end if;
         end loop;
      end;
      if Shared_Parser.Post_Recover /= null then
         Shared_Parser.Post_Recover.all;
      end if;

      return Super.Recover_Result;

   exception
   when E : others =>
      if Debug_Mode then
         Trace.Put (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E), Prefix => True);
         Trace.New_Line;
         raise;
      else
         return Fail_Programmer_Error;
      end if;
   end Recover;

   ----------
   --  Spec private subprograms; for language-specific
   --  child packages.

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID)
   is begin
      if ID /= Expected_ID then
         raise Bad_Config with ID'Image & " /=" & Expected_ID'Image;
      end if;
   end Check;

   function Current_Token
     (Tree                      :         in     Syntax_Trees.Tree;
      Terminals_Current         :         in out Syntax_Trees.Stream_Index;
      Restore_Terminals_Current :            out Syntax_Trees.Stream_Index;
      Insert_Delete             : aliased in out Config_Op_Arrays.Vector;
      Current_Insert_Delete     :         in out SAL.Base_Peek_Type)
     return Base_Token
   is
      use Config_Op_Arrays;
      use Config_Op_Array_Refs;

      procedure Inc_I_D
      is begin
         Current_Insert_Delete := Current_Insert_Delete + 1;
         if Current_Insert_Delete > Last_Index (Insert_Delete) then
            Current_Insert_Delete := No_Insert_Delete;
            Clear (Insert_Delete);
         end if;
      end Inc_I_D;

   begin
      if Terminals_Current = Syntax_Trees.Invalid_Stream_Index then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         raise Bad_Config;
      end if;

      loop
         if Current_Insert_Delete = No_Insert_Delete then
            Restore_Terminals_Current := Terminals_Current;
            return Tree.Base_Token (Tree.Get_Node (Terminals_Current));

         elsif Token_Index (Constant_Ref (Insert_Delete, Current_Insert_Delete)) = Terminals_Current then
            declare
               Op : Insert_Delete_Op renames Constant_Ref (Insert_Delete, Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label (Op.Op) is
               when Insert =>
                  --  Decrement Terminals_Current so Next_Token knows it should always
                  --  increment it. Save the initial value, to restore in case of error.
                  Restore_Terminals_Current := Terminals_Current;
                  Terminals_Current         := Tree.Stream_Prev (Terminals_Current);
                  return (ID => ID (Op), others => <>);

               when Delete =>
                  Terminals_Current         := Tree.Stream_Next (Terminals_Current);
                  Restore_Terminals_Current := Terminals_Current;
                  Inc_I_D;
               end case;
            end;
         else
            return Tree.Base_Token (Tree.Get_Node (Terminals_Current));
         end if;
      end loop;
   end Current_Token;

   function Current_Token_ID_Peek
     (Tree                  :         in Syntax_Trees.Tree;
      Terminals_Current     :         in Syntax_Trees.Stream_Index;
      Insert_Delete         : aliased in Config_Op_Arrays.Vector;
      Current_Insert_Delete :         in SAL.Base_Peek_Type)
     return Token_ID
   is
      use Config_Op_Array_Refs;

      Result : Token_ID;
   begin
      if Terminals_Current = Syntax_Trees.Invalid_Stream_Index then
         --  Happens with really bad syntax.
         raise Bad_Config;
      end if;

      --  May be overridden by Insert_Delete below.
      Result := Tree.ID (Terminals_Current);

      if Current_Insert_Delete = No_Insert_Delete then
         null;

      elsif Token_Index (Constant_Ref (Insert_Delete, Current_Insert_Delete)) = Terminals_Current then
         declare
            Op : Insert_Delete_Op renames Constant_Ref (Insert_Delete, Current_Insert_Delete);
         begin
            case Insert_Delete_Op_Label (Op.Op) is
            when Insert =>
               Result := Op.Ins_ID;

            when Delete =>
               --  This should have been handled in Check
               raise SAL.Programmer_Error;
            end case;
         end;
      end if;
      return Result;
   end Current_Token_ID_Peek;

   procedure Current_Token_ID_Peek_3
     (Tree                  :         in     Syntax_Trees.Tree;
      Terminals_Current     :         in     Syntax_Trees.Stream_Index;
      Insert_Delete         : aliased in     Config_Op_Arrays.Vector;
      Current_Insert_Delete :         in     SAL.Base_Peek_Type;
      Tokens                :            out Token_ID_Array_1_3)
   is
      Terminals_Next : Syntax_Trees.Stream_Index := Tree.Stream_Next (Terminals_Current);
   begin
      if Terminals_Current = Syntax_Trees.Invalid_Stream_Index then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         raise Bad_Config;
      end if;

      Tokens (1) := Tree.ID (Terminals_Current);
      Tokens (2) := Invalid_Token_ID;
      Tokens (3) := Invalid_Token_ID;

      if Terminals_Next /= Syntax_Trees.Invalid_Stream_Index then
         Tokens (2) := Tree.ID (Terminals_Next);

         Terminals_Next := Tree.Stream_Next (Terminals_Next);
         if Terminals_Next /= Syntax_Trees.Invalid_Stream_Index then
            Tokens (3) := Tree.ID (Terminals_Next);
         end if;
      end if;

      if Current_Insert_Delete = No_Insert_Delete then
         null;
      else
         for I in Tokens'Range loop
            declare
               use Config_Op_Arrays, Config_Op_Array_Refs;
               J : constant SAL.Base_Peek_Type := Current_Insert_Delete + SAL.Peek_Type (I) - 1;
            begin
               if (J in First_Index (Insert_Delete) .. Last_Index (Insert_Delete)) and then
                 Token_Index (Constant_Ref (Insert_Delete, J)) = Terminals_Current
               then
                  declare
                     Op : Insert_Delete_Op renames Constant_Ref (Insert_Delete, J);
                  begin
                     case Insert_Delete_Op_Label (Op.Op) is
                     when Insert =>
                        Tokens (I) := Op.Ins_ID;

                     when Delete =>
                        --  This should have been handled in Check
                        raise SAL.Programmer_Error;
                     end case;
                  end;
               end if;
            end;
         end loop;
      end if;
   end Current_Token_ID_Peek_3;

   procedure Delete_Check
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      ID     : in     Token_ID)
   is
      use Config_Op_Arrays;
      Op : constant Config_Op := (Delete, ID, Config.Current_Shared_Token);
   begin
      Check (Tree.ID (Config.Current_Shared_Token), ID);
      if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
         raise Bad_Config;
      end if;
      Append (Config.Ops, Op);
      Append (Config.Insert_Delete, Op);
      Config.Current_Insert_Delete := 1;
   end Delete_Check;

   procedure Delete_Check
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Index  : in out Syntax_Trees.Stream_Index;
      ID     : in     Token_ID)
   is begin
      Check (Tree.ID (Index), ID);
      Delete (Tree, Config, Index);
   end Delete_Check;

   procedure Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Index  : in out Syntax_Trees.Stream_Index)
   is
      use Config_Op_Arrays;
      Op : constant Config_Op := (Delete, Tree.ID (Index), Index);
   begin
      if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
         raise Bad_Config;
      end if;
      Append (Config.Ops, Op);
      Append (Config.Insert_Delete, Op);
      Config.Current_Insert_Delete := 1;
      Index := Tree.Stream_Next (Index);
   end Delete;

   function Find_ID
     (Config         : in     Configuration;
      ID             : in     Token_ID)
     return Boolean
   is begin
      for I in 1 .. Config.Stack.Depth - 1 loop
         --  Depth has Invalid_Token_ID
         if ID = Config.Stack.Peek (I).Token.ID then
            return True;
         end if;
      end loop;
      return False;
   end Find_ID;

   procedure Find_ID
     (Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Stack_ID : Token_ID renames Config.Stack.Peek (Matching_Index).Token.ID;
         begin
            exit when Stack_ID = ID;
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_ID
     (Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            ID : Token_ID renames Config.Stack.Peek (Matching_Index).Token.ID;
         begin
            exit when ID in IDs'First .. IDs'Last and then IDs (ID);
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_Descendant_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      ID_Set         : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is
      use Syntax_Trees;
   begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         exit when Config.Stack.Peek (Matching_Index).Token.ID in ID_Set'Range and then
           (ID_Set (Config.Stack.Peek (Matching_Index).Token.ID) and
              (Config.Stack.Peek (Matching_Index).Node /= Invalid_Node_Access and then
                 Tree.Find_Descendant (Config.Stack.Peek (Matching_Index).Node, ID) /=
                 Invalid_Node_Access));

         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_Descendant_ID;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      loop
         exit when Matching_Name_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Syntax_Trees.Recover_Token renames Config.Stack.Peek (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region :=
              (if Token.Name = Null_Buffer_Region
               then Token.Byte_Region
               else Token.Name);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Name_Region))
               else Lexer.Buffer_Text (Name_Region));

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Count := 0;

      loop
         exit when Matching_Name_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token       : Syntax_Trees.Recover_Token renames Config.Stack.Peek (Matching_Name_Index).Token;
            Name_Region : constant Buffer_Region :=
              (if Token.Name = Null_Buffer_Region
               then Token.Byte_Region
               else Token.Name);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Name_Region))
               else Lexer.Buffer_Text (Name_Region));

            if Other_ID = Token.ID then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Insert (Config : in out Configuration; ID : in Token_ID)
   is begin
      Insert (Config, Config.Current_Shared_Token, ID);
   end Insert;

   procedure Insert (Config : in out Configuration; IDs : in Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Config, ID);
      end loop;
   end Insert;

   procedure Insert (Config : in out Configuration; Index : in Syntax_Trees.Stream_Index; ID : in Token_ID)
   is
      use Config_Op_Arrays;
      Op : constant Config_Op := (Insert, ID, Index);
   begin
      if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
         raise Bad_Config;
      end if;
      Append (Config.Ops, Op);
      Append (Config.Insert_Delete, Op);
      Config.Current_Insert_Delete := 1;
   end Insert;

   function Next_Token
     (Tree                      :         in     Syntax_Trees.Tree;
      Terminals_Current         :         in out Syntax_Trees.Stream_Index;
      Restore_Terminals_Current :         in out Syntax_Trees.Stream_Index;
      Insert_Delete             : aliased in out Config_Op_Arrays.Vector;
      Current_Insert_Delete     :         in out SAL.Base_Peek_Type)
     return Base_Token
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;

      function Next_Terminal return Base_Token
      is begin
         Terminals_Current         := Tree.Stream_Next (Terminals_Current);
         Restore_Terminals_Current := Terminals_Current;
         return Tree.Base_Token (Tree.Get_Node (Terminals_Current));
      end Next_Terminal;

   begin
      loop
         if Last_Index (Insert_Delete) > 0 and then Current_Insert_Delete = Last_Index (Insert_Delete) then
            Current_Insert_Delete := No_Insert_Delete;
            Clear (Insert_Delete);
            return Next_Terminal;

         elsif Current_Insert_Delete = No_Insert_Delete then
            return Next_Terminal;

         elsif Token_Index (Constant_Ref (Insert_Delete, Current_Insert_Delete + 1)) =
           Tree.Stream_Next (Terminals_Current)
         then
            Current_Insert_Delete := Current_Insert_Delete + 1;
            declare
               Op : Insert_Delete_Op renames Constant_Ref (Insert_Delete, Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label'(Op.Op) is
               when Insert =>
                  return (ID => Op.Ins_ID, others => <>);

               when Delete =>
                  Terminals_Current         := Tree.Stream_Next (Terminals_Current);
                  Restore_Terminals_Current := Terminals_Current;
               end case;
            end;

         else
            return Next_Terminal;
         end if;
      end loop;
   end Next_Token;

   function Push_Back_Valid
     (Tree               : in Syntax_Trees.Tree;
      Target_Token_Index : in Syntax_Trees.Stream_Index;
      Ops                : in Config_Op_Arrays.Vector;
      Prev_Op            : in Positive_Index_Type)
     return Boolean
   is
      use Config_Op_Arrays;

      Fast_Forward_Seen : Boolean := False;
   begin
      --  We require a Fast_Forward after Insert or Delete, to eliminate
      --  duplicate results from push_back before and after a
      --  delete (see test_mckenzie_recover.adb Extra_Begin).
      --
      --  If Target_Token_Index is greater than the new current terminal
      --  implied by Prev_Op, the Push_Back is valid. Otherwise, it is
      --  invalid (it should have been done first); we only need to look at
      --  one op other than Fast_Forward.
      for I in reverse First_Index (Ops) .. Prev_Op loop
         declare
            Op : Config_Op renames Element (Ops, I);
         begin
            case Op.Op is
            when Fast_Forward =>
               --  We need to see the op before the Fast_Forward to tell if Push_Back
               --  to Target_Token_Index is ok.
               Fast_Forward_Seen := True;

            when Undo_Reduce =>
               --  We don't know what the new terminal is from this op. We'll just
               --  have to trust the programmers.
               return True;

            when Push_Back =>
               --  If neither the proposed Push_Back nor Op is for an empty token,
               --  successive Push_Backs have decreasing targets; see
               --  test_mckenzie_recover.adb Missing_Name_0.
               --
               --  However, if there is a Fast_Forward between two Push_Backs,
               --  Target_Token_Index must be >= Op.PB_Token_Index. See
               --  ada-mode-recover_27.adb.
               --
               --  If called from Undo_Reduce_Valid where the Undo_Reduce token is
               --  empty, we get Target_Token_Index = Op.PB_Token_Index.
               return Target_Token_Index = Syntax_Trees.Invalid_Stream_Index or else
                 Op.PB_Token_Index = Syntax_Trees.Invalid_Stream_Index or else
                 (if Fast_Forward_Seen
                  then Tree.Get_Element_Index (Target_Token_Index) > Tree.Get_Element_Index (Op.PB_Token_Index)
                  else Tree.Get_Element_Index (Target_Token_Index) <= Tree.Get_Element_Index (Op.PB_Token_Index));

            when Insert =>
               --  If Target_Token_Index = Op.Ins_Before, we want the edit
               --  point to be at the same token as before; that's ok.
               --
               --  If Target_Token_Index > Ins_Token_Index, the Push_Back is partway
               --  into a Fast_Forward.
               return Fast_Forward_Seen and
                 (Target_Token_Index = Syntax_Trees.Invalid_Stream_Index or else
                    Tree.Get_Element_Index (Target_Token_Index) >= Tree.Get_Element_Index (Op.Ins_Before));

            when Delete =>
               --  As for Insert
               return Fast_Forward_Seen and
                 (Target_Token_Index = Syntax_Trees.Invalid_Stream_Index or else
                    Tree.Get_Element_Index (Target_Token_Index) >= Tree.Get_Element_Index (Op.Del_Token_Index));
            end case;
         end;
      end loop;
      --  We can only get here if the only ops in Ops are Fast_Forward,
      --  which is a programming error.
      pragma Assert (False);
      raise Bad_Config;
   end Push_Back_Valid;

   procedure Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;

      Item        : constant Recover_Stack_Item        := Config.Stack.Pop;
      Token_Index : constant Syntax_Trees.Stream_Index := Item.Token.First_Terminal_Index;

      function Compare (Left : in Syntax_Trees.Stream_Index; Right : in Config_Op) return Boolean
        is (case Right.Op is
            when Fast_Forward    => False,
            when Undo_Reduce     => False,
            when Push_Back       => False,
            when Insert => Tree.Get_Element_Index (Left) < Tree.Get_Element_Index (Right.Ins_Before),
            when Delete => Tree.Get_Element_Index (Left) < Tree.Get_Element_Index (Right.Del_Token_Index));
      --  If Left = Right.Token_Index, we assume the Right ops go _after_
      --  the Left, so the Left do not need to be repeated.
   begin
      if Token_Index /= Syntax_Trees.Invalid_Stream_Index then
         Config.Current_Shared_Token := Token_Index;
         for I in First_Index (Config.Ops) .. Last_Index (Config.Ops) loop
            if Compare (Token_Index, Constant_Ref (Config.Ops, I)) then
               if Is_Full (Config.Insert_Delete) then
                  raise Bad_Config;
               end if;
               Append (Config.Insert_Delete, Constant_Ref (Config.Ops, I));
            end if;
         end loop;
      end if;

      if Is_Full (Config.Ops) then
         raise Bad_Config;
      end if;
      Append (Config.Ops, (Push_Back, Item.Token.ID, Config.Current_Shared_Token));
   end Push_Back;

   procedure Push_Back_Check
     (Tree        : in     Syntax_Trees.Tree;
      Config      : in out Configuration;
      Expected_ID : in     Token_ID)
   is begin
      Check (Config.Stack.Peek (1).Token.ID, Expected_ID);
      Push_Back (Tree, Config);
   end Push_Back_Check;

   procedure Push_Back_Check
     (Tree     : in     Syntax_Trees.Tree;
      Config   : in out Configuration;
      Expected : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         if Push_Back_Valid (Tree, Config) then
            Push_Back_Check (Tree, Config, ID);
         else
            raise Bad_Config;
         end if;
      end loop;
   end Push_Back_Check;

   procedure Put
     (Message      : in     String;
      Trace        : in out WisiToken.Trace'Class;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Config       : in     Configuration;
      Task_ID      : in     Boolean := True;
      Strategy     : in     Boolean := False)
   is
      --  For debugging output

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).
      use Config_Op_Array_Refs;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type WisiToken.Semantic_Checks.Check_Status_Label;

      Descriptor : WisiToken.Descriptor renames Trace.Descriptor.all;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Task_ID then +"task" & Task_Attributes.Value'Image & " " else +" ") &
        Tree.Trimmed_Image (Parser_Label) & ": " &
        (if Message'Length > 0 then Message & ":" else "");
   begin
      Result := Result & Natural'Image (Config.Cost);
      if Strategy or Trace_McKenzie > Extra then
         Result := Result & ", (";
         for C of Config.Strategy_Counts loop
            Result := Result & Integer'Image (C);
         end loop;
         Result := Result & "), ";
      else
         Result := Result & ", ";
      end if;
      if Config.Check_Status.Label /= Ok then
         Result := Result & Semantic_Checks.Check_Status_Label'Image (Config.Check_Status.Label) & " ";
      elsif Config.Error_Token.ID /= Invalid_Token_ID then
         Result := Result & "Error " & Syntax_Trees.Image (Config.Error_Token, Descriptor) & " ";
      end if;
      Result := Result & Image (Config.Stack, Descriptor, Depth => 1);

      if Config.Current_Insert_Delete = No_Insert_Delete then
         Result := Result & "| " & Tree.Image (Config.Current_Shared_Token, Descriptor) & "|";
      else
         Result := Result & "/ " & Trimmed_Image (Config.Current_Insert_Delete) & ":" &
           Image (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete), Descriptor) & "/";
      end if;

      Result := Result & Image (Config.Ops, Descriptor);
      if Config.Minimal_Complete_State /= None then
         Result := Result & " minimal_complete " & Config.Minimal_Complete_State'Image;
      end if;
      Trace.Put_Line (-Result);
   end Put;

   procedure Put_Line
     (Trace        : in out WisiToken.Trace'Class;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Message      : in     String;
      Task_ID      : in     Boolean := True)
   is begin
      Trace.Put_Line
        ((if Task_ID then "task" & Task_Attributes.Value'Image & " " else "") &
           Tree.Trimmed_Image (Parser_Label) & ": " & Message);
   end Put_Line;

   function Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
     return Ada.Containers.Count_Type
   is
      Nonterm_Item : constant Recover_Stack_Item := Recover_Stacks.Pop (Stack);

      Children : constant Syntax_Trees.Node_Access_Array := Tree.Children (Nonterm_Item.Node);
   begin
      for C of Children loop
         Stack.Push ((Tree.State (C), C, Tree.Get_Recover_Token (C)));
      end loop;
      return Children'Length;
   end Undo_Reduce;

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID)
   is begin
      pragma Assert (Config.Stack.Depth > 1);
      Check (Config.Stack.Peek (1).Token.ID, Expected);
      Config_Op_Arrays.Append (Config.Ops, (Undo_Reduce, Expected, Undo_Reduce (Config.Stack, Tree)));
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Undo_Reduce_Check;

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         Undo_Reduce_Check (Config, Tree, ID);
      end loop;
   end Undo_Reduce_Check;

end WisiToken.Parse.LR.McKenzie_Recover;
