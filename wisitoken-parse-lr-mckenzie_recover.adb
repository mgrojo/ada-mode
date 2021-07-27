--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
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
with GNAT.Traceback.Symbolic;
with System.Multiprocessors;
with WisiToken.Lexer;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
with WisiToken.Parse.LR.McKenzie_Recover.Explore;
with WisiToken.Parse.LR.McKenzie_Recover.Parse;
with WisiToken.Parse.LR.Parser_Lists;
package body WisiToken.Parse.LR.McKenzie_Recover is
   use all type WisiToken.Syntax_Trees.Sequential_Index;
   use all type System.Multiprocessors.CPU_Range;

   function Push_Back_Undo_Reduce_Valid
     (Super                 : not null access Base.Supervisor;
      Target_Op             : in     Recover_Op_Label;
      Target_Node           : in     Syntax_Trees.Node_Access;
      Ops                   : in     Recover_Op_Arrays.Vector;
      Last_Op_Index         : in     Positive_Index_Type;
      Push_Back_Undo_Reduce : in     Boolean)
     return Boolean
   with Pre => Target_Node = Syntax_Trees.Invalid_Node_Access or else
               Super.Tree.Label (Target_Node) in Syntax_Trees.Terminal_Label;
   --  Target_Node is the first terminal in a token that is the object of
   --  Target_Op; a Push_Back or Undo_Reduce that will be the next Op
   --  after Last_Op. Return True if that respects restrictions on Op
   --  order.
   --
   --  Language_Fixes may set Push_Back_Undo_Reduce True; other callers
   --  must set it False.

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
      Stack        : in out Recover_Stacks.Stack;
      Input_Stream : in out Bounded_Streams.List)
   is
      Parser_Stack_Depth : constant SAL.Peek_Type := Tree.Stack_Depth (Parser_Stack);
   begin
      pragma Assert (Stack.Depth = 0);
      if Stack.Size < Parser_Stack_Depth then
         raise SAL.Programmer_Error with "recover stack needs more space;" & Parser_Stack_Depth'Image;
      end if;
      for I in reverse 1 .. Parser_Stack_Depth loop
         declare
            Element : constant Syntax_Trees.Stream_Index  := Tree.Peek (Parser_Stack, I);
            Node    : constant Syntax_Trees.Node_Access   := Tree.Get_Node (Parser_Stack, Element);
            Token   : constant Syntax_Trees.Recover_Token :=
              (if I = Parser_Stack_Depth
               then (others => <>)
               else Tree.Get_Recover_Token ((Parser_Stack, Element, Node)));
         begin
            Stack.Push ((Tree.State (Parser_Stack, Element), Token));
         end;
      end loop;

      if Tree.Stream_Input_Length (Parser_Stack) > 0 then
         --  Parse stream input has tokens from breakdown of a nonterm in
         --  Shared_Stream.
         declare
            use Syntax_Trees;
            Index : Stream_Index := Tree.Stream_Next (Parser_Stack, Tree.Stack_Top (Parser_Stack));
         begin
            loop
               exit when Index = Invalid_Stream_Index;
               Input_Stream.Append (Tree.Get_Node (Parser_Stack, Index));
               Index := Tree.Stream_Next (Parser_Stack, Index);
            end loop;
         end;
      end if;
   end To_Recover;

   procedure Recover_Init
     (Super        : not null access Base.Supervisor;
      Shared       : in              Base.Shared;
      Parser_State : in out          Parser_Lists.Parser_State)
   is
      use Recover_Op_Arrays;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Trace  : WisiToken.Trace'Class renames Super.Trace.all;
      Config : Configuration;
      Error  : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
   begin
      Parser_State.Recover.Enqueue_Count := @ + 1;

      declare
         use Syntax_Trees;
         First_Current : constant Node_Access := Super.Tree.First_Terminal (Parser_State.Current_Token.Node);
      begin
         Config.Current_Shared_Token := Super.Tree.First_Terminal_In_Node (Parser_State.Shared_Token);

         Config.Resume_Token_Goal := Super.Tree.Get_Sequential_Index
           (if First_Current /= Invalid_Node_Access
            then First_Current --  Current_Token is from breakdown of a shared stream nonterm.
            else Config.Current_Shared_Token.Node) +
           Shared.Table.McKenzie_Param.Check_Limit;
      end;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser " & Super.Tree.Trimmed_Image (Parser_State.Stream) &
              ": State" & Super.Tree.State (Parser_State.Stream)'Image &
              " Current_Token " & Super.Tree.Image (Parser_State.Current_Token) &
              " Resume_Token_Goal" & Config.Resume_Token_Goal'Image);
         Trace.Put_Line
           ((case Error.Label is
             when LR_Parse_Action => "LR_Parse_Action",
             when User_Parse_Action => "User_Parse_Action, " &
               Super.Tree.Image (Super.Tree.Stack_Top (Parser_State.Stream)) & " " &
               In_Parse_Actions.Image (Error.Status, Super.Tree.all),
             when Message => raise SAL.Programmer_Error));
         if Trace_McKenzie > Detail then
            Trace.Put_Line ("parse stream:");
            Trace.Put_Line
              (Super.Tree.Image (Parser_State.Stream, Children => Trace_McKenzie > Extra, Shared => True));
         end if;
      end if;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      Config.Input_Stream.Initialize;
      To_Recover (Parser_State.Stream, Super.Tree.all, Config.Stack, Config.Input_Stream);

      --  Parser_State.Recover_Insert_Delete must be empty (else we would not get
      --  here). Therefore Parser_State current token is in
      --  Parser_State.Shared_Token.

      case Error.Label is
      when LR_Parse_Action =>
         Config.Error_Token := Super.Tree.Get_Recover_Token (Error.Error_Token);

         if Trace_McKenzie > Detail then
            Put ("enqueue", Trace, Super.Tree.all, Parser_State.Stream, Config, Task_ID => False);
         end if;

      when User_Parse_Action =>
         if Shared.Language_Fixes = null then
            --  The only fix is to ignore the error.
            if Trace_McKenzie > Detail then
               Config.Strategy_Counts (Ignore_Error) := 1;
               Put ("enqueue", Trace, Super.Tree.all, Parser_State.Stream, Config, Task_ID => False);
            end if;

         else
            --  Undo the reduction that encountered the error, let Process_One
            --  enqueue possible solutions. We leave the cost at 0, since this is
            --  the root config. Later logic will enqueue the 'ignore error'
            --  solution; see McKenzie_Recover.Explore Process_One.

            --  Undo_Reduce can be invalid here; see ada-mode/test/ada_mode-recover_27.adb
            if Undo_Reduce_Valid (Super, Config) then
               Config.User_Parse_Action_Status := Error.Status;
               Config.Error_Token              := Config.Stack.Peek.Token;

               Unchecked_Undo_Reduce (Super, Shared.Table.all, Config);

               Config.User_Parse_Action_Token_Count := Element (Config.Ops, Last_Index (Config.Ops)).Token_Count;

               if Trace_McKenzie > Detail then
                  Put
                    ("undo_reduce " & Image
                       (Super.Tree.ID (Config.Error_Token), Super.Tree.Lexer.Descriptor.all),
                       Trace, Super.Tree.all, Parser_State.Stream, Config, Task_ID => False);
               end if;
            else
               --  Ignore error
               if Trace_McKenzie > Detail then
                  Config.Strategy_Counts (Ignore_Error) := 1;
                  Put ("enqueue", Trace, Super.Tree.all, Parser_State.Stream, Config,
                       Task_ID => False);
               end if;
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
        (Shared_Parser.Table,
         Shared_Parser.Language_Fixes,
         Shared_Parser.Language_Matching_Begin_Tokens,
         Shared_Parser.Language_String_ID_Set,
         Shared_Parser.Wrapped_Lexer_Errors'Access);

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
         Recover_Init (Super'Access, Shared, Parser_State);
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
            begin
               if not Worker_Tasks (I)'Terminated then
                  Worker_Tasks (I).Done;
               end if;
            exception
            when Tasking_Error =>
               --  Worker terminated after we checked 'Terminated; it's a race condition.
               null;
            end;
         end loop;

         if ID /= Null_Id then
            Raise_Exception (ID, -Message);
         end if;
      end;

      Shared_Parser.Max_Sequential_Index := Super.Max_Sequential_Index;
      Shared_Parser.Min_Sequential_Index := Super.Min_Sequential_Index;

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
         Spawn_Limit : SAL.Base_Peek_Type := Shared_Parser.Table.Max_Parallel; -- per parser
      begin
         for Parser of Parsers loop
            if Parser.Recover.Success then
               Solutions := Solutions + Parser.Recover.Results.Count;
            end if;
         end loop;

         if Solutions > Shared_Parser.Table.Max_Parallel and Trace_McKenzie > Outline then
            Trace.Put_Line ("too many parallel parsers required in recover; dropping some solutions");
            Spawn_Limit := Shared_Parser.Table.Max_Parallel / Parsers.Count;
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
                             ("spawn " & Shared_Parser.Tree.Trimmed_Image (Parsers.First.Stream) & " from " &
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
                     use Recover_Op_Arrays, Recover_Op_Array_Refs;
                     use Syntax_Trees;

                     Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;

                     Stack  : Syntax_Trees.Stream_ID renames Parser_State.Stream;
                     Tree   : Syntax_Trees.Tree renames Shared_Parser.Tree;
                     Data   : McKenzie_Data renames Parser_State.Recover;
                     Result : Configuration renames Data.Results.Peek;

                     Error     : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
                     Error_Pos : constant Buffer_Pos :=
                       (case Error.Label is
                        when LR_Parse_Action   => Tree.Char_Region (Error.Error_Token.Node).First,
                        when User_Parse_Action =>
                          (if Tree.Name (Error.Status.Begin_Name).First /= Invalid_Buffer_Pos
                           then Tree.Name (Error.Status.Begin_Name).First
                           elsif Tree.Name (Error.Status.End_Name).First /= Invalid_Buffer_Pos
                           then Tree.Name (Error.Status.End_Name).First
                           else Buffer_Pos'First),
                        when Message           => raise SAL.Programmer_Error);

                     Stack_Matches_Ops : Boolean := True;
                     First_Insert      : Boolean := True;

                     Last_Recover_Node_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'First;
                  begin
                     --  The verb will be reset by the main parser; just indicate the
                     --  parser recovered from the error.
                     Parser_State.Set_Verb (Shift);

                     Parser_State.Errors (Parser_State.Errors.Last).Recover := Result;

                     Parser_State.Resume_Token_Goal := Result.Resume_Token_Goal;

                     if Trace_McKenzie > Extra then
                        Put_Line (Trace, Tree, Parser_State.Stream, "before Ops applied:", Task_ID => False);
                        Trace.Put_Line
                          ("   stack/stream:" & ASCII.LF & Tree.Image
                             (Parser_State.Stream, Stack => True, Input => True, Shared => True, Children => True));
                        Trace.Put_Line ("   Shared_Token  " & Tree.Image (Parser_State.Shared_Token));
                        Trace.Put_Line ("   Current_Token " & Tree.Image (Parser_State.Current_Token));
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
                           Op : Recover_Op renames Constant_Ref (Result.Ops, I);

                           procedure Raise_Bad_Config (Message : in String)
                           is begin
                              if Debug_Mode then
                                 raise SAL.Programmer_Error with Message;
                              end if;

                              if Trace_McKenzie > Outline then
                                 Put_Line
                                   (Trace, Tree, Parser_State.Stream, Message,
                                    Task_ID => False);
                              end if;
                              raise Bad_Config;
                           end Raise_Bad_Config;
                        begin
                           case Op.Op is
                           when Fast_Forward =>
                              if Stack_Matches_Ops then
                                 if Tree.Label (Parser_State.Current_Token.Node) = Nonterm then
                                    declare
                                       Target : Stream_Node_Parents;

                                       procedure Find_FF_Token_Index (Ref : in Stream_Node_Ref)
                                       is begin
                                          Target := Tree.To_Stream_Node_Parents (Ref);
                                          Tree.First_Sequential_Terminal (Target);
                                          loop
                                             exit when Tree.Get_Sequential_Index (Target.Ref.Node) = Op.FF_Token_Index;
                                             Tree.Next_Sequential_Terminal (Target);
                                          end loop;
                                       end Find_FF_Token_Index;

                                    begin
                                       Find_FF_Token_Index (Parser_State.Current_Token);

                                       if Tree.First_Terminal (Tree.Get_Node (Target.Ref.Stream, Target.Ref.Element)) /=
                                         Target.Ref.Node
                                       then
                                          --  Target is a nonterm that should not be shifted as a whole, so
                                          --  break it down. test case: ada_mode-recover_bad_char.adb
                                          if Target.Ref.Stream = Tree.Shared_Stream then
                                             --  First we need to move all tokens Parser_State.Shared_Token .. Target
                                             --  to the input stream. test case: ada_mode-recover_10.adb
                                             pragma Assert (Parser_State.Shared_Token = Parser_State.Current_Token);

                                             Tree.Move_Shared_To_Input
                                               (First  => Parser_State.Shared_Token,
                                                Last   => Target.Ref,
                                                Stream => Parser_State.Stream);

                                             Find_FF_Token_Index (Tree.First_Input (Parser_State.Stream));
                                          end if;
                                          Tree.Breakdown (Target);
                                          Parser_State.Next_Token (Tree, Set_Current => True, Delete => False);
                                       end if;
                                    end;
                                 end if;
                              end if;

                              --  The parser would do shifts and reduces for the tokens we are
                              --  skipping here
                              Stack_Matches_Ops := False;

                           when Undo_Reduce =>
                              --  If Stack_Matches_Ops, we must do the Stack.Pop and Pushes, and we
                              --  can use Stack.Peek to check if the Undo_Reduce is valid.
                              --
                              --  If not Stack_Matches_Ops, we have to assume Op.UR_Token_Index is correct.
                              --
                              --  See test_mckenzie_recover.adb Extra_Begin for an example of Undo_Reduce
                              --  after other ops.
                              --
                              --  We can't use McKenzie_Recover.Undo_Reduce_Valid here; that takes a
                              --  Config stack, not a parser stack. So we duplicate part of it.
                              if Stack_Matches_Ops then
                                 if not (Nonterm = Tree.Label (Tree.Peek (Stack)) and
                                           Op.Nonterm = Tree.ID (Parser_State.Stream, Tree.Peek (Stack)))
                                 then
                                    Raise_Bad_Config ("Undo_Reduce does not match stack top in apply config");
                                 end if;
                              end if;

                              if Stack_Matches_Ops then
                                 Undo_Reduce (Tree, Shared_Parser.Table.all, Stack);
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
                              if Stack_Matches_Ops then
                                 if not (Op.PB_ID = Tree.ID (Parser_State.Stream, Tree.Peek (Stack))) then
                                    Raise_Bad_Config ("Push_Back does not match stack top in apply config");
                                 end if;
                              end if;

                              if Stack_Matches_Ops then
                                 Tree.Push_Back (Parser_State.Stream);

                                 Parser_State.Current_Token := Tree.First_Input (Parser_State.Stream);

                                 Parser_State.Inc_Shared_Stream_Token := False;
                              end if;

                           when Insert       =>
                              Parser_State.Recover_Insert_Delete.Append
                                ((Op         => Insert,
                                  Error_Pos  => Error_Pos,
                                  Ins_ID     => Op.Ins_ID,
                                  Ins_Before => Op.Ins_Before,
                                  Ins_Node   => Syntax_Trees.Invalid_Node_Access));

                              if First_Insert and Op.Ins_Before = Tree.Get_Sequential_Index
                                (Peek_Current_Sequential_Terminal (Parser_State, Tree).Node)
                              then
                                 --  We need First_Insert here, not just Stack_Matches_Ops, when the
                                 --  first insert is preceeded only by Push_Back and Undo_Reduce, with
                                 --  at least one Undo_Reduce (so Stack_Matches_Ops is False when we
                                 --  get here). See test_mckenzie_recover.adb Missing_Name_3

                                 First_Insert := False;

                                 Parser_State.Current_Token := Tree.Insert_Virtual_Terminal
                                   (Parser_State.Stream, Op.Ins_ID);

                                 Parser_State.Inc_Shared_Stream_Token := False;

                                 --  Normally Insert is completed by Stack.Push; we let the main parser
                                 --  do that.
                                 Stack_Matches_Ops := False;

                                 Parser_State.Recover_Insert_Delete.Variable_Ref
                                   (Parser_State.Recover_Insert_Delete.Last_Index).Ins_Node :=
                                   Parser_State.Current_Token.Node;

                                 pragma Assert (Parser_State.Recover_Insert_Delete_Current = No_Index);
                              else
                                 --  Let main parser handle it
                                 if Parser_State.Recover_Insert_Delete_Current = No_Index then
                                    Parser_State.Recover_Insert_Delete_Current :=
                                      Recover_Op_Nodes_Arrays.Last_Index (Parser_State.Recover_Insert_Delete);
                                 end if;
                              end if;

                           when Delete =>
                              if Op.Del_Token_Index < Last_Recover_Node_Index then
                                 Raise_Bad_Config ("Delete is out of order");
                              end if;
                              Last_Recover_Node_Index := Op.Del_Token_Index;

                              Recover_Op_Nodes_Arrays.Append
                                (Parser_State.Recover_Insert_Delete,
                                 (Op             => Delete,
                                  Error_Pos      => Error_Pos,
                                  Del_ID         => Op.Del_ID,
                                  Del_Index      => Op.Del_Token_Index,
                                  Del_Node       => Syntax_Trees.Invalid_Node_Access,
                                  Del_After_Node => Syntax_Trees.Invalid_Node_Access));

                              --  We have to apply more than one delete here if they are
                              --  consecutive, because the main parser expects
                              --  Parser_State.Current_Token to be correct before checking for
                              --  Delete on return from Recover. Op.Del_After_Node must be a
                              --  non-deleted node (for example, ada_mode-recover_extra_end_loop.adb
                              --  deletes "end loop;"), so we must get Op.Del_After_Node from the
                              --  parser stack, not Prev_Terminal.
                              if Stack_Matches_Ops and Parser_State.Recover_Insert_Delete_Current = No_Index and
                                Op.Del_Token_Index = Tree.Get_Sequential_Index
                                  (Peek_Current_Sequential_Terminal (Parser_State, Tree).Node)
                              then
                                 declare
                                    Op : Recover_Op_Nodes renames Parser_State.Recover_Insert_Delete
                                      (Parser_State.Recover_Insert_Delete.Last_Index);

                                    Deleted_Ref : constant Stream_Node_Ref := Peek_Current_Sequential_Terminal
                                      (Parser_State, Tree);

                                    El  : constant Stream_Index := Tree.Peek (Parser_State.Stream);
                                    Prev_Ref : Stream_Node_Parents   := Tree.To_Stream_Node_Parents
                                      ((Stream  => Parser_State.Stream,
                                        Element => El,
                                        Node    => Get_Node (El)));
                                 begin
                                    Op.Del_Node := Deleted_Ref.Node;
                                    Tree.Last_Terminal (Prev_Ref);
                                    Op.Del_After_Node := Prev_Ref.Ref.Node;
                                 end;

                                 Next_Token (Parser_State, Tree, Set_Current => True, Delete => True);
                              else
                                 if Parser_State.Recover_Insert_Delete_Current = No_Index then
                                    Parser_State.Recover_Insert_Delete_Current :=
                                      Recover_Op_Nodes_Arrays.Last_Index (Parser_State.Recover_Insert_Delete);
                                 end if;
                              end if;
                           end case;
                        end;
                     end loop;

                     if Trace_McKenzie > Extra then
                        Put_Line (Trace, Tree, Parser_State.Stream, "after Ops applied:", Task_ID => False);
                        Trace.Put_Line
                          ("   stack/stream:" & ASCII.LF & Tree.Image
                             (Parser_State.Stream, Stack => True, Input => True, Shared => True, Children => True));
                        Trace.Put_Line ("   Current_Token " & Tree.Image (Parser_State.Current_Token));
                        Trace.Put_Line ("   Shared_Token  " & Tree.Image (Parser_State.Shared_Token));
                        Trace.Put_Line ("   recover_insert_delete " & Image
                                          (Parser_State.Recover_Insert_Delete, Tree,
                                           First => Parser_State.Recover_Insert_Delete_Current));
                        Trace.Put_Line ("   inc_shared_stream_token " & Parser_State.Inc_Shared_Stream_Token'Image);
                        Trace.Put_Line ("   resume_token_goal" & Parser_State.Resume_Token_Goal'Image);
                     end if;
                  end;
               exception
               when E : Bad_Config =>
                  if Debug_Mode then
                     Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
                  end if;

                  Parsers.Terminate_Parser (Current_Parser, Shared_Parser.Tree, "bad config in recover", Trace);
                  --  Terminate advances Current_Parser
                  Skip_Next := True;

                  if Parsers.Count = 0 then
                     --  Oops. just give up
                     return Fail_Programmer_Error;
                  end if;
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
         Trace.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
         raise SAL.Programmer_Error;
      else
         return Fail_Programmer_Error;
      end if;
   end Recover;

   ----------
   --  Spec private subprograms; for child packages. Declaration order

   function Peek_Sequential_Start
     (Tree   :         in Syntax_Trees.Tree;
      Config : aliased in Configuration)
     return Peek_Sequential_State
   is begin
      return State : Peek_Sequential_State (Config.Input_Stream'Access) do
         Parse.First_Sequential_Terminal (Tree, State.Input_Terminal);

         State.Sequential_Terminal := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);
         if Syntax_Trees.Rooted (State.Sequential_Terminal.Ref) or
           State.Sequential_Terminal.Ref.Node = Syntax_Trees.Invalid_Node_Access
           --  Ref is an empty nonterm. ada_mode-interactive_03.adb
         then
            Tree.First_Sequential_Terminal (State.Sequential_Terminal);
         end if;
      end return;
   end Peek_Sequential_Start;

   function Peek_Sequential_Terminal (State : in Peek_Sequential_State) return Syntax_Trees.Node_Access
   is begin
      if State.Input_Terminal.Node = Syntax_Trees.Invalid_Node_Access then
         return State.Sequential_Terminal.Ref.Node;

      else
         return State.Input_Terminal.Node;
      end if;
   end Peek_Sequential_Terminal;

   procedure Peek_Next_Sequential_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      State  : in out Peek_Sequential_State)
   is
      use Syntax_Trees;
   begin
      if State.Input_Terminal.Node = Invalid_Node_Access then
         Tree.Next_Sequential_Terminal (State.Sequential_Terminal);

      else
         Parse.Next_Sequential_Terminal (Tree, State.Input_Terminal);
      end if;
   end Peek_Next_Sequential_Terminal;

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID)
   is begin
      if ID /= Expected_ID then
         raise Bad_Config with ID'Image & " /=" & Expected_ID'Image;
      end if;
   end Check;

   procedure Delete_Check
     (Tree        : in     Syntax_Trees.Tree;
      Config      : in out Configuration;
      Node        : in     Syntax_Trees.Valid_Node_Access;
      Expected_ID : in     Token_ID)
   is
      use Recover_Op_Arrays;
      Op : constant Recover_Op :=
        (Delete,
         (if Expected_ID = Invalid_Token_ID
          then Tree.ID (Node)
          else Expected_ID),
         Tree.Get_Sequential_Index (Node));
   begin
      if Expected_ID /= Invalid_Token_ID then
         Check (Tree.ID (Node), Expected_ID);
      end if;
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
      ID     : in     Token_ID)
   is
      Node : constant Syntax_Trees.Node_Access := Parse.Peek_Current_First_Sequential_Terminal
        (Tree, Config, Following_Element => False);
   begin
      if Node = Syntax_Trees.Invalid_Node_Access then
         raise Bad_Config;
      end if;
      Delete_Check (Tree, Config, Node, ID);
   end Delete_Check;

   procedure Delete_Check
     (Tree   :         in     Syntax_Trees.Tree;
      Config : aliased in out Configuration;
      IDs    :         in     Token_ID_Array)
   is
      State : Peek_Sequential_State := Peek_Sequential_Start (Tree, Config);
   begin
      for ID of IDs loop
         Delete_Check (Tree, Config, Peek_Sequential_Terminal (State), ID);
         Peek_Next_Sequential_Terminal (Tree, State);
      end loop;
   end Delete_Check;

   procedure Delete_Check
     (Tree       : in     Syntax_Trees.Tree;
      Config     : in out Configuration;
      Peek_State : in out Peek_Sequential_State;
      ID         : in     Token_ID)
   is begin
      Delete_Check (Tree, Config, Peek_Sequential_Terminal (Peek_State), ID);
      Peek_Next_Sequential_Terminal (Tree, Peek_State);
   end Delete_Check;

   procedure Do_Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use Syntax_Trees;
      Token : constant Recover_Token := Config.Stack.Pop.Token;
   begin
      Recover_Op_Arrays.Append
        (Config.Ops, (Push_Back, Tree.ID (Token), Tree.Get_Sequential_Index (Tree.First_Terminal (Token))));

      if Token.Virtual then
         if Token.First_Terminal = Invalid_Node_Access then
            --  Token is an empty nonterm. Doing nothing is ok; the empty nonterm
            --  will be created by the parse process.
            null;
         else
            case Tree.Label (Token.First_Terminal) is
            when Terminal_Label =>
               Config.Input_Stream.Prepend (Token.First_Terminal);
            when Nonterm =>
               raise SAL.Programmer_Error;
            end case;
         end if;
      else
         Config.Input_Stream.Prepend (Token.Element_Node);
      end if;
   end Do_Push_Back;

   procedure Clear_Sequential_Index (Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser)
   is
      use Syntax_Trees;

      Tree     : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Terminal : Stream_Node_Parents := Shared_Parser.Max_Sequential_Index;
   begin
      if Terminal /= Syntax_Trees.Invalid_Stream_Node_Parents then
         loop
            Tree.Set_Sequential_Index (Terminal.Ref.Node, Invalid_Sequential_Index);
            exit when Terminal.Ref.Node = Shared_Parser.Min_Sequential_Index.Ref.Node;

            Tree.Prev_Terminal (Terminal);
         end loop;

         Shared_Parser.Min_Sequential_Index := Invalid_Stream_Node_Parents;
         Shared_Parser.Max_Sequential_Index := Invalid_Stream_Node_Parents;
      end if;
   end Clear_Sequential_Index;

   function Find_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration;
      ID     : in Token_ID)
     return Boolean
   is begin
      for I in 1 .. Config.Stack.Depth - 1 loop
         --  Depth has Invalid_Token_ID
         if ID = Tree.ID (Config.Stack.Peek (I).Token) then
            return True;
         end if;
      end loop;
      return False;
   end Find_ID;

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Stack_ID : Token_ID renames Tree.ID (Config.Stack.Peek (Matching_Index).Token);
         begin
            exit when Stack_ID = ID;
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            ID : Token_ID renames Tree.ID (Config.Stack.Peek (Matching_Index).Token);
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
      function Found return Boolean
      is
         use Syntax_Trees;
         Token renames Config.Stack.Peek (Matching_Index).Token;
      begin
         return
           Tree.ID (Token) in ID_Set'Range and then
           (ID_Set (Tree.ID (Token)) and
              (not Token.Virtual and then
                 Tree.Find_Descendant (Token.Element_Node, ID) /=
                 Invalid_Node_Access));
      end Found;
   begin
      loop
         exit when Matching_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         exit when Found;

         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_Descendant_ID;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
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
            Name_Region : constant Buffer_Region := Tree.Name (Token);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Name_Region))
               else Tree.Lexer.Buffer_Text (Name_Region));

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
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
            Name_Region : constant Buffer_Region := Tree.Name (Token);
         begin
            exit when Name_Region /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Name_Region))
               else Tree.Lexer.Buffer_Text (Name_Region));

            if Other_ID = Tree.ID (Token) then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      ID     : in     Token_ID)
   is begin
      Insert (Tree, Config, Parse.Peek_Current_First_Sequential_Terminal (Tree, Config), ID);
   end Insert;

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      IDs    : in     Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Tree, Config, ID);
      end loop;
   end Insert;

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Before : in     Syntax_Trees.Valid_Node_Access;
      ID     : in     Token_ID)
   is
      use Recover_Op_Arrays;
      Op : constant Recover_Op := (Insert, ID, Tree.Get_Sequential_Index (Before));
   begin
      if Is_Full (Config.Ops) or Is_Full (Config.Insert_Delete) then
         raise Bad_Config;
      end if;
      Append (Config.Ops, Op);
      Append (Config.Insert_Delete, Op);
      Config.Current_Insert_Delete := 1;
   end Insert;

   function Undo_Reduce_Op_Order_Valid
     (Ops       : in Recover_Op_Arrays.Vector)
     return Boolean
   --  Subset of checks in Push_Back_Undo_Reduce_Valid, when the target nonterm is empty.
   is
      use Recover_Op_Arrays;
   begin
      declare
         Op : Recover_Op renames Element (Ops, Last_Index (Ops));
      begin
         case Op.Op is
         when Fast_Forward =>
            --  Normally any Undo_Reduce must be done before Insert and after
            --  Delete, to eliminate duplicate results from push_back/reduce
            --  before and after delete (see test_mckenzie_recover.adb
            --  Extra_Begin, ada_mode-recover_extra_end_loop.adb with incremental
            --  parse). Fast_Forward resets that.
            return True;

         when Undo_Reduce | Push_Back =>
            return True;

         when Insert =>
            return False;

         when Delete =>
            return True;
         end case;
      end;
   end Undo_Reduce_Op_Order_Valid;

   function Push_Back_Undo_Reduce_Valid
     (Super                 : not null access Base.Supervisor;
      Target_Op             : in              Recover_Op_Label;
      Target_Node           : in              Syntax_Trees.Node_Access;
      Ops                   : in              Recover_Op_Arrays.Vector;
      Last_Op_Index         : in              Positive_Index_Type;
      Push_Back_Undo_Reduce : in              Boolean)
     return Boolean
   is
      use Syntax_Trees;
      use Recover_Op_Arrays;

      Target_Index : Base_Sequential_Index :=
        (if Target_Node = Invalid_Node_Access
         then Invalid_Sequential_Index
         else Super.Tree.Get_Sequential_Index (Target_Node));

      Fast_Forward_Seen : Boolean := False;

      function Check_Insert_Delete (Op_Index : in Sequential_Index) return Boolean
      is begin
         --  We allow '=' here, so we can try adding more ops at a previous
         --  edit point; in particular, another Language_Fix. See
         --  test_mckenzie_Recover Error_3.
         return Fast_Forward_Seen and
           (Target_Index = Invalid_Sequential_Index or
              Target_Index >= Op_Index);
      end Check_Insert_Delete;

   begin
      if Target_Index = Invalid_Sequential_Index and Target_Node /= Invalid_Node_Access then
         Base.Extend_Sequential_Index (Super, Target_Node, Positive => False);
         Target_Index := Super.Tree.Get_Sequential_Index (Target_Node);
      end if;

      for I in reverse First_Index (Ops) .. Last_Op_Index loop
         declare
            Op : Recover_Op renames Element (Ops, I);
         begin
            case Op.Op is
            when Fast_Forward =>
               --  Normally any Push_Back must be done before any Insert or Delete,
               --  to eliminate duplicate results from push_back/reduce before and
               --  after delete (see test_mckenzie_recover.adb Extra_Begin).
               --  Fast_Forward resets that.
               --
               --  Push_Back/Undo_Reduce into a Fast_Forward region is ok, but not
               --  all of a Fast_Forward; that would just repeat the same ops.
               --
               --  FF_Token_Index is at the end of the Fast_Forward region; we need
               --  to see the next op to find the beginning.
               Fast_Forward_Seen := True;

            when Undo_Reduce =>
               --  We allow mixing push_back and undo_reduce in any order, so we can
               --  get to an arbitrary point inside a nonterm to do insert/delete.

               if Op.UR_Token_Index = Invalid_Sequential_Index then
                  --  Undo_Reduced token was empty; need to see the next one.
                  null;
               else
                  if Target_Index = Invalid_Sequential_Index then
                     --  Target token is empty; it does not cross anything.
                     return True;
                  end if;

                  case Target_Op is
                  when Undo_Reduce =>
                     --  No point in checking Fast_Forward_Seen here; we don't have the
                     --  last terminal index of the push_back. So this allows Undo_Reduce
                     --  of the entire fast_forward.
                     --
                     --  Undo_Reduce after Undo_Reduce; must undo part or all of the original
                     --  nonterm; see test_mckenzie_recover.adb Missing_Name_2.
                     return Target_Index >= Op.UR_Token_Index;

                  when Push_Back =>
                     --  No point in checking Fast_Forward_Seen here; we don't have the
                     --  last terminal index of Op. So this allows Push_Back of the
                     --  entire fast_forward.
                     --
                     --  Push_Back after Undo_Reduce; must push back only part of the
                     --  unreduced nonterm, unless overridden by Language_Fixes.
                     --  test/ada_mode-recover_block_name_mismatch.adb
                     return
                       (if Push_Back_Undo_Reduce
                        then True
                        else Target_Index > Op.UR_Token_Index);

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end if;

            when Push_Back =>
               if Op.PB_Token_Index = Invalid_Sequential_Index then
                  --  Pushed_Back token was empty; need to see the next one.
                  null;
               else
                  if Target_Index = Invalid_Sequential_Index then
                     --  Target token is empty; it does not cross anything.
                     return True;
                  end if;

                  case Target_Op is
                  when Undo_Reduce =>
                     if Fast_Forward_Seen then
                        --  Unreducing a token somewhere in the push_back.
                        return Target_Index >= Op.PB_Token_Index;
                     else
                        --  Need to keep going to see if we cross a fast_forward
                        null;
                     end if;

                  when Push_Back =>
                     --  Between Fast_Forwards, successive non-empty Push_Back
                     --  have decreasing targets; see test_mckenzie_recover.adb
                     --  Missing_Name_0.
                     --
                     --  If the target push_back crosses a Fast_Forward, it must not cross
                     --  a preceding op; Target_Index must be >= Op.PB_Token_Index. See
                     --  ada-mode-recover_27.adb.
                     if Fast_Forward_Seen then
                        --  Target push_back/undo_reduce does not cross the previous
                        --  push_back/undo_reduce.
                        return Target_Index > Op.PB_Token_Index;

                     else
                        --  Need to keep going to see if we cross a fast_forward
                        null;
                     end if;

                  when others =>
                     raise SAL.Programmer_Error;
                  end case;
               end if;

            when Insert =>
               return Check_Insert_Delete (Op.Ins_Before);

            when Delete =>
               return Check_Insert_Delete (Op.Del_Token_Index);
            end case;
         end;
      end loop;

      --  We get here if we are looking for the next Push_Back or
      --  Undo_Reduce. In effect, Op.*_Token_Index is now 0, which means any
      --  Push_Back or Undo_Reduce is ok.
      return True;
   end Push_Back_Undo_Reduce_Valid;

   function Push_Back_Valid
     (Super                 : not null access Base.Supervisor;
      Config                : in              Configuration;
      Push_Back_Undo_Reduce : in              Boolean                       := False)
     return Boolean
   is
      Tree : Syntax_Trees.Tree renames Super.Tree.all;
   begin
      if Config.Stack.Depth <= 1 then
         return False;
      end if;

      declare
         Token : Syntax_Trees.Recover_Token renames Config.Stack.Peek.Token;
         First_Terminal : constant Syntax_Trees.Node_Access := Tree.First_Terminal (Token);
      begin
         return
           not Tree.Contains_Virtual_Terminal (Token) and then
           --  If Contains_Virtual_Terminal, Token was inserted earlier in this
           --  or a previous recover session; no point in recomputing it. In
           --  incremental parse, it can be from a previous recover session;
           --  Edit_Tree would have deleted it if it needed to be recomputed. We
           --  don't exclude Virtual, because sometimes we need to push_back a
           --  virtual nonterm created by a Language_Fix in order to apply a
           --  second Language_Fix; see test_mckenzie_recover.adb Missing_Name_0.

           --  We allow both Push_Back and Undo_Reduce of empty nonterms
           --  (First_Terminal = Invalid_Node_Access); Push_Back is easier to use
           --  in Language_Fixes, Undo_Reduce is required to change the stack
           --  state to allow completing a production with a non-empty nonterm.
           (Recover_Op_Arrays.Length (Config.Ops) = 0 or else
              Push_Back_Undo_Reduce_Valid
                (Super,
                 Push_Back,
                 First_Terminal,
                 Config.Ops,
                 Recover_Op_Arrays.Last_Index (Config.Ops),
                 Push_Back_Undo_Reduce));
      end;
   end Push_Back_Valid;

   procedure Push_Back
     (Super                 : not null access Base.Supervisor;
      Config                : in out          Configuration;
      Push_Back_Undo_Reduce : in              Boolean := False)
   is begin
      --  We relax the "don't push back into previous recover" restriction
      --  for Language_Fixes; see test_mckenzie_recover.adb Missing_Name_5.
      if not Push_Back_Valid (Super, Config, Push_Back_Undo_Reduce => Push_Back_Undo_Reduce) then
         raise Bad_Config;
      end if;

      Do_Push_Back (Super.Tree.all, Config);
   end Push_Back;

   procedure Push_Back_Check
     (Super         : not null access Base.Supervisor;
      Config        : in out Configuration;
      Expected_ID   : in     Token_ID)
   is begin
      Check (Super.Tree.ID (Config.Stack.Peek (1).Token), Expected_ID);
      Push_Back (Super, Config);
   end Push_Back_Check;

   procedure Push_Back_Check
     (Super         : not null access Base.Supervisor;
      Config        : in out Configuration;
      Expected      : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         Push_Back_Check (Super, Config, ID);
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
   --  For debugging output
   is
      use Recover_Op_Array_Refs;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type WisiToken.In_Parse_Actions.Status_Label;
      use all type Bounded_Streams.Cursor;

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

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
      if Config.User_Parse_Action_Status.Label /= Ok then
         Result := Result & In_Parse_Actions.Status_Label'Image (Config.User_Parse_Action_Status.Label) & " ";
      elsif Tree.ID (Config.Error_Token) /= Invalid_Token_ID then
         Result := Result & "Error " & Syntax_Trees.Image (Tree, Config.Error_Token) & " ";
      end if;
      Result := Result & Image (Config.Stack, Tree, Depth => 1);

      if Config.Current_Insert_Delete /= No_Insert_Delete then
         Result := Result & "/" & Trimmed_Image (Config.Current_Insert_Delete) & ":" &
           Image (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete), Descriptor) & "/";

      elsif Config.Input_Stream.First /= Bounded_Streams.No_Element then
         Result := Result & "\" & Tree.Image
           (Config.Input_Stream (Config.Input_Stream.First), Node_Numbers => True) & "\";

      else
         Result := Result & "|" & Tree.Image (Config.Current_Shared_Token) & "|";
      end if;

      Result := Result & Image (Config.Ops, Descriptor);
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
        ((if Task_ID then "task" & Task_Attributes.Value'Image & " " else " ") &
           Tree.Trimmed_Image (Parser_Label) & ": " & Message);
   end Put_Line;

   function Undo_Reduce_Valid
     (Super  : not null access Base.Supervisor;
      Config : in out          Configuration)
     return Boolean
   is
      Tree : Syntax_Trees.Tree renames Super.Tree.all;
   begin
      if Config.Stack.Depth = 0 then
         return False;
      end if;

      declare
         use Recover_Op_Arrays;

         Token : Syntax_Trees.Recover_Token renames Config.Stack.Peek.Token;
      begin
         if Token.Virtual or else not Tree.Is_Nonterm (Token.Element_Node) then
            return False;

         elsif Length (Config.Ops) = 0 then
            return True;

         else
            --  Undo_Reduce needs to know what tokens the nonterm contains, to
            --  push them on the stack. Thus we need a valid Tree first terminal
            --  node, or an empty nonterm.
            return
              (Tree.Child_Count (Token.Node) = 0 and
                 Undo_Reduce_Op_Order_Valid (Config.Ops))
              or else
              (Push_Back_Undo_Reduce_Valid
                 (Super, Undo_Reduce,  Tree.First_Sequential_Terminal (Token.Node), Config.Ops, Last_Index (Config.Ops),
                  Push_Back_Undo_Reduce => False));
         end if;
      end;
   end Undo_Reduce_Valid;

   procedure Unchecked_Undo_Reduce
     (Super  : not null access Base.Supervisor;
      Table  : in              Parse_Table;
      Config : in out          Configuration)
   is
      Tree           : Syntax_Trees.Tree renames Super.Tree.all;
      Stack          : Recover_Stacks.Stack renames Config.Stack;
      Nonterm_Item   : constant Recover_Stack_Item             := Recover_Stacks.Pop (Stack);

      First_Terminal : constant Syntax_Trees.Node_Access := Tree.First_Source_Terminal
        (Nonterm_Item.Token.Element_Node, Trailing_Non_Grammar => False);
      --  If First_Terminal (element) is virtual, it might be from current
      --  error recovery, not the shared_stream, so extend_sequential_index
      --  would not give it an index. Note that having Edit_Tree label all
      --  virtuals in the shared stream would not be incremental.

      Prev_State     : State_Index                             := Stack.Peek.State;
      Children       : constant Syntax_Trees.Node_Access_Array := Tree.Children (Nonterm_Item.Token.Element_Node);
   begin
      for C of Children loop
         if Is_Terminal (Tree.ID (C), Tree.Lexer.Descriptor.all) then
            Prev_State := Shift_State (Action_For (Table, Prev_State, Tree.ID (C)));
         else
            Prev_State := Goto_For (Table, Prev_State, Tree.ID (C));
         end if;
         Stack.Push ((Prev_State, Tree.Get_Recover_Token (C)));
      end loop;

      if First_Terminal /= Syntax_Trees.Invalid_Node_Access then
         Base.Extend_Sequential_Index (Super, First_Terminal, Positive => False);
      end if;

      Recover_Op_Arrays.Append
        (Config.Ops,
         (Undo_Reduce, Tree.ID (Nonterm_Item.Token.Element_Node), Children'Length,
         Tree.Get_Sequential_Index (First_Terminal)));
   exception
   when SAL.Container_Full =>
      raise Bad_Config;
   end Unchecked_Undo_Reduce;

   procedure Undo_Reduce_Check
     (Super    : not null access Base.Supervisor;
      Table    : in              Parse_Table;
      Config   : in out          Configuration;
      Expected : in              Token_ID)
   is begin
      if not Undo_Reduce_Valid (Super, Config) then
         raise Bad_Config;
      end if;
      Check (Super.Tree.ID (Config.Stack.Peek (1).Token), Expected);
      Unchecked_Undo_Reduce (Super, Table, Config);
   end Undo_Reduce_Check;

   procedure Undo_Reduce_Check
     (Super    : not null access Base.Supervisor;
      Table    : in              Parse_Table;
      Config   : in out          Configuration;
      Expected : in              Token_ID_Array)
   is begin
      for ID of Expected loop
         Undo_Reduce_Check (Super, Table, Config, ID);
      end loop;
   end Undo_Reduce_Check;

end WisiToken.Parse.LR.McKenzie_Recover;
