--  Abstract :
--
--  See spec.
--
--  The test names are often misleading; they were correct when first
--  written, but as the algorithm evolved, the tests behavior changed.
--  They all still test something useful.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks.Containers;
with Ada.Containers;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LALR_Main;
with Ada_Lite_LR1_T1_Main;
with WisiToken.AUnit;
with WisiToken.Parse.LR.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Text_IO_Trace;
package body Test_McKenzie_Recover is
   use Ada_Lite_Actions;
   use AUnit.Checks;
   use AUnit.Checks.Containers;
   use WisiToken.Parse.LR.AUnit.Test_Config_Op_Arrays;
   use WisiToken.AUnit;
   use all type WisiToken.BNF.LR_Generate_Algorithm;
   use all type WisiToken.Parse.LR.Config_Op_Label;
   use all type WisiToken.Parse.LR.Strategies;
   use all type WisiToken.Semantic_Checks.Check_Status_Label;

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Trace : aliased WisiToken.Text_IO_Trace.Trace;

   Parser : WisiToken.Parse.LR.Parser.Parser (Ada_Lite_Actions.Descriptor'Access);

   Orig_Params : WisiToken.Parse.LR.McKenzie_Param_Type
     (First_Terminal    => Descriptor.First_Terminal,
      Last_Terminal     => Descriptor.Last_Terminal,
      First_Nonterminal => Descriptor.First_Nonterminal,
      Last_Nonterminal  => Descriptor.Last_Nonterminal);

   Orig_End_Name_Optional : Boolean;

   Empty_Token_ID_Set : constant WisiToken.Token_ID_Set :=
     WisiToken.To_Token_ID_Set
       (Descriptor.First_Terminal, Descriptor.Last_Terminal, (1 .. 0 => WisiToken.Invalid_Token_ID));

   procedure Parse_Text
     (Text             : in String;
      Multiple_Tasks   : in Boolean := False;
      Expect_Exception : in Boolean := False)
   is begin
      if WisiToken.Trace_Tests > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Lexer.Reset_With_String (Text);

      if not Multiple_Tasks then
         Parser.Table.McKenzie_Param.Task_Count := 1;
      end if;

      Parser.Parse;

      --  We don't run Parser.Execute_Actions, so Error.Recover.Ops
      --  Stream_Index values are still valid.

      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Tests > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", True, Expect_Exception);
   end Parse_Text;

   procedure Check_Recover
     (Label                   : in String                                       := "";
      Errors_Length           : in Ada.Containers.Count_Type;
      Checking_Error          : in Ada.Containers.Count_Type                    := 1;
      Error_Token_ID          : in WisiToken.Token_ID;
      Error_Token_Byte_Region : in WisiToken.Buffer_Region                      := WisiToken.Null_Buffer_Region;
      Ops                     : in WisiToken.Parse.LR.AUnit.Test_Config_Op_Arrays.Vector :=
        WisiToken.Parse.LR.AUnit.Test_Config_Op_Arrays.Empty_Vector;
      Ops_Race_Condition      : in Boolean                                      := False;
      Enqueue_Low             : in Integer                                      := 0;
      Enqueue_High            : in Integer                                      := Integer'Last;
      Check_Low               : in Integer                                      := 0;
      Check_High              : in Integer                                      := Integer'Last;
      Cost                    : in Integer;
      Strategy_Counts         : in WisiToken.Parse.LR.Strategy_Counts           := (others => 0);
      Expecting               : in WisiToken.Token_ID_Set                       := Empty_Token_ID_Set;
      Code                    : in WisiToken.Semantic_Checks.Check_Status_Label := WisiToken.Semantic_Checks.Ok)
   is
      --  Only set Ops_Race_Condition True when Parse_Text.Multiple_Tasks is
      --  True; otherwise, add case on LALR | LR1.

      use WisiToken.Parse.LR.AUnit;
      use WisiToken.Semantic_Checks.AUnit;
      use all type WisiToken.Buffer_Region;
      use all type WisiToken.Token_ID;
      use all type WisiToken.Token_ID_Set;
      use all type WisiToken.Parse.LR.Parse_Error_Label;

      Label_I : constant String := Label & "." & Ada.Containers.Count_Type'Image (Checking_Error);

      Parser_State : WisiToken.Parse.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
      Cursor       : WisiToken.Parse.LR.Parse_Error_Lists.Cursor := Parser_State.Errors.First;
   begin
      Check (Label_I & ".Errors.Length", Parser_State.Errors.Length, Errors_Length);

      for I in 2 .. Checking_Error loop
         WisiToken.Parse.LR.Parse_Error_Lists.Next (Cursor);
      end loop;

      declare
         Error : WisiToken.Parse.LR.Parse_Error renames WisiToken.Parse.LR.Parse_Error_Lists.Element (Cursor);
      begin
         if Expecting /= Empty_Token_ID_Set then
            Check (Label_I & ".Expecting", Error.Expecting, Expecting);
         end if;

         if Code = Ok then
            --  Expecting an Action error. Label is "code" so prj.el wisitoken-goto-aunit-fail can find it.
            Check (Label_I & ".Code", Error.Label, Action);
            declare
               Token : WisiToken.Syntax_Trees.Recover_Token renames Parser.Tree.Get_Recover_Token (Error.Error_Token);
            begin
               Check (Label_I & ".Error.TOKEN_ID", Token.ID, Error_Token_ID);
               if Error_Token_ID /= +Wisi_EOI_ID then
                  --  EOF byte_region is unreliable
                  Check (Label_I & ".Error_Token.Byte_Region", Token.Byte_Region, Error_Token_Byte_Region);
               end if;
            end;
         else
            --  Expecting a Check error. We put "Error_Token_ID" in the check
            --  label, so wisitoken-dtrt can find the right place.
            Check (Label_I & ".Code", Error.Label, Check);
            Check (Label_I & ".Code", Error.Check_Status.Label, Code);
            if Error.Check_Status.End_Name.Byte_Region = WisiToken.Null_Buffer_Region then
               --  End_Name is empty; check begin_name
               Check (Label_I & ".Error_Token_ID", Error.Check_Status.Begin_Name.ID, Error_Token_ID);
               Check (Label_I & ".Error_Token_Byte_Region", Error.Check_Status.Begin_Name.Byte_Region,
                      Error_Token_Byte_Region);
            else
               Check (Label_I & ".Error_Token_ID", Error.Check_Status.End_Name.ID, Error_Token_ID);
               Check (Label_I & ".Error_Token_Byte_Region",
                      Error.Check_Status.End_Name.Byte_Region,
                      Error_Token_Byte_Region);
            end if;
         end if;

         if not Ops_Race_Condition then
            Check (Label_I & ".Ops", Parser.Tree, Error.Recover.Ops, Ops);
            Check (Label_I & ".Strategy_Counts", Error.Recover.Strategy_Counts, Strategy_Counts);
         end if;

         --  The enqueue count depends on a race condition when there is more
         --  than one worker task; configs with costs higher than the final
         --  solution may or may not be enqueued. So we test a range; we want
         --  to know if it gets a lot higher when we change something.
         --  Similarly for Check_Low, _High.
         --
         --  In most tests, we use one worker task for repeatability, indicated
         --  by leaving Enqueue_High, Check_High at 0.
         --
         --  Recover does not come from the same parser as Error if the
         --  succeeding parser was spawned after error recovery, but we copy
         --  Enqueue_Count and Check_Count in Parser_Lists.Prepend_Copy just
         --  for this check.
         if Enqueue_Low = 0 and Enqueue_High = Integer'Last then
            --  Not checking enqueue or check counts
            null;
         elsif Enqueue_High = Integer'Last then
            Check (Label_I & ".Enqueue_Low", Parser_State.Recover.Enqueue_Count, Enqueue_Low);
            Check (Label_I & ".Check_Low", Parser_State.Recover.Check_Count, Check_Low);
            Check (Label_I & ".Config_Full", Parser_State.Recover.Config_Full_Count, 0);
         else
            Check_Range (Label_I & ".Enqueue_High", Parser_State.Recover.Enqueue_Count, Enqueue_Low, Enqueue_High);
            Check_Range (Label_I & ".Check_High", Parser_State.Recover.Check_Count, Check_Low, Check_High);
         end if;
         Check (Label_I & ".Cost", Error.Recover.Cost, Cost);
      end;
   end Check_Recover;

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      File_Name : constant String := "../test/bnf/ada_lite.input";
   begin
      --  The test is that there is no exception and no errors.

      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;
      Check ("errors length", Parser.Parsers.First.State_Ref.Errors.Length, 0);
   end No_Error;

   procedure Empty_Comments (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Error recovery returns the cheapest minimal statement.

      Parse_Text ("");
      Check_Recover
        ("1",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOI_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Strategy_Counts         => (Minimal_Complete => 1, Insert => 1, others => 0),
         Enqueue_Low             => 147,
         Check_Low               => 25,
         Cost                    => 5);

      Parse_Text ("   ");
      Check_Recover
        ("2",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOI_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Strategy_Counts         => (Minimal_Complete => 1, Insert => 1, others => 0),
         Enqueue_Low             => 147,
         Check_Low               => 25,
         Cost                    => 5);

      Parse_Text ("--  a comment");
      Check_Recover
        ("3",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOI_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Strategy_Counts         => (Minimal_Complete => 1, Insert => 1, others => 0),
         Enqueue_Low             => 147,
         Check_Low               => 25,
         Cost                    => 5);
   end Empty_Comments;

   procedure Error_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;");
      --           1        |10       |20       |30       |40
      --           1         2      3  4     5  6 7 8 9    10 11 12 13
      --  Missing "if" in "end if;"
      --
      --  error 1 at ';' 44, expecting 'if'. Inserts 'if', succeeds.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (44, 44),
         Ops                     => +(Insert, +IF_ID, 11),
         Strategy_Counts         => (Minimal_Complete => 1, others => 0),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 1);
   end Error_1;

   procedure Error_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end if; end Proc; ");
      --  |1       |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  1         2    3  4     5      6 7     8  9 10 11  13 14  15  16     17 18 19  21  22  23
      --                                                   12                          20

      --  Missing "begin" for Block_2.
      --
      --  Error 1 at 'Block_2' 63, expecting 'if'. Explore finds the
      --  solution below, leaving "Block_2;" as a procedure call in the else
      --  branch.
      --
      --  The desired solution (push_back 'end' 59, push_back
      --  sequence_of_statements_opt, insert 'begin') is also found, but
      --  terminated by duplicate state.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (63, 69),
         Ops                     => +(Push_Back, +END_ID, 15) & (Undo_Reduce, +sequence_of_statements_ID, 0) &
           (Delete, +END_ID, 15),
         Strategy_Counts         => (Push_Back => 1, Undo_Reduce => 1, Delete => 1, others => 0),
         Enqueue_Low             => 26,
         Check_Low               => 6,
         Cost                    => 2);
   end Error_2;

   procedure Error_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text
        ("procedure Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; end Water; "
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
         --  1      2     3  4     5    6     7  9  10     12 13     15  16  18   19   20 22 23    25  26 27 28   29
         --                                    8      11        14         17           21     24
        );
      --  Missing "end if" at 67.
      --
      --  Enters error recovery on 23:';' expecting 'if'. Finds the solution
      --  below. That encounters a second error at 28:'Water' 100.
      --
      --  The desired solution (insert 'if ; end') is cost 3.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (84, 84),
         Ops                     => +(Insert, +IF_ID, 23) & (Fast_Forward, 25) &
           (Insert, +SEMICOLON_ID, 25) & (Fast_Forward, 26) & (Insert, +IDENTIFIER_ID, 26),
         Strategy_Counts         => (Minimal_Complete => 2, Matching_Begin => 1, others => 0),
         Cost                    => 2,
         Expecting               => WisiToken.To_Token_ID_Set
           (Descriptor.First_Terminal,
            Descriptor.Last_Terminal,
            (1                   => +IF_ID)));

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (100, 104),
         Ops                     => +(Push_Back, +END_ID, 27) & (Insert, +END_ID, 27) & (Insert, +LOOP_ID, 27) &
           (Insert, +SEMICOLON_ID, 27) & (Fast_Forward, 28) & (Push_Back, +END_ID, 27) & (Insert, +END_ID, 27) &
           (Insert, +LOOP_ID, 27) & (Insert, +SEMICOLON_ID, 27),
         Strategy_Counts         => (Language_Fix => 2, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 45, when LR1 => 46),
         Check_Low               => 8,
         Cost                    => 0,
         Expecting               => WisiToken.To_Token_ID_Set
           (Descriptor.First_Terminal,
            Descriptor.Last_Terminal,
            (1                   => +LOOP_ID)));
   end Error_3;

   procedure Error_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text ("when in the course of human events", Expect_Exception => True);
      --  Bogus syntax; test no exceptions due to empty stack, hit enqueue limit, etc.

   end Error_4;

   procedure Check_Accept (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text ("procedure Debug is begin A;");
      --                    |10       |20
      --           1         2     3  4     5 6 = SEMICOLON, 7 = Wisi_EOI
      --  Missing "end;"
      --
      --  Inserts 'end ;', continues to EOI, succeeds
      --  Test hitting EOI and Accept_It in error recovery
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +Wisi_EOI_ID,
         Error_Token_Byte_Region => (27, 27),
         Ops                     => +(Insert, +END_ID, 7) & (Insert, +SEMICOLON_ID, 7),
         Strategy_Counts         => (Minimal_Complete => 2, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 16, when LR1 => 15),
         Check_Low               => 3,
         Cost                    => 2);
   end Check_Accept;

   procedure Extra_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Debug is begin procedure Put_Top_10 is begin end Put_Top_10; begin end Debug; ");
         --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Added 'begin' 72, intending to delete 'begin' 20
      --
      --  Error recovery is entered and exited with parallel parsers active;
      --  one parsing a subprogram_body, the other a generic_instantiation
      --  (which will fail eventually).
      --
      --  While checking the prefered solution, there are conflicts that
      --  must be handled.
      --
      --  For the subprogram_body parser (1), error recovery is entered at
      --  'procedure' 26; the desired solution is (push_back 'begin' 20, push_back
      --  declarative_part_opt, delete 'begin'), leaving 'is' 17 on the
      --  parse stack, with cost 1. That allows the subprogram_body parser
      --  to continue to EOI.
      --
      --  For the generic_instantiation parser (0), error recovery is
      --  entered at 'begin 20'. It finds higher cost solutions.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +PROCEDURE_ID,
         Error_Token_Byte_Region => (26, 34),
         Ops                     => +(Push_Back, +BEGIN_ID, 4) & (Undo_Reduce, +declarative_part_ID, 0) &
           (Delete, +BEGIN_ID, 4),
         Strategy_Counts         => (Push_Back => 1, Undo_Reduce => 1, Delete => 1, others => 0),
         Enqueue_Low             => 27,
         Check_Low               => 6,
         Cost                    => 1);
   end Extra_Begin;

   procedure Conflict_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Check_1 is end begin end Check_1;");
      --           |10       |20       |30       |40

      --  Syntax error (extra 'end' 22) while two parsers are sorting out a
      --  conflict
      --
      --  parser 1 for subprogram_body (should succeed) finds (delete
      --  'end'), cost 2. Continues to EOI, succeed.
      --
      --  parser 0 for generic_instantiation (should fail): finds a solution
      --  that later encounters an error and is terminated.

      Check_Recover
        (Label                   => "1",
         Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (22, 24),
         Ops                     => +(Delete, +END_ID, 4),
         Strategy_Counts         => (Delete => 1, others => 0),
         Enqueue_Low             => 30,
         Check_Low               => 7,
         Cost                    => 2);

      --  Symmetric case where generic_instantiation is desired

      Parse_Text
        ("procedure Check_2 is end new Check_2;");
      --           |10       |20       |30       |40       |50       |60       |70       |80

      --  Syntax error (extra 'end' 22) while two parsers are sorting out a
      --  conflict.
      --
      --  parser 1 for subprogram_body (should fail): hits enqueue limit, fails.
      --
      --  parser 0 for generic_instantiation (should succeed):
      --  finds: delete 'end', cost 1. Continue to eof, accept

      Check_Recover
        (Label                   => "2",
         Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (22, 24),
         Ops                     => +(Delete, +END_ID, 4),
         Strategy_Counts         => (Delete => 1, others => 0),
         Enqueue_Low             => 8,
         Check_Low               => 4,
         Cost                    => 2);
   end Conflict_1;

   procedure Conflict_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text
        ("function Find_Path return Path is begin return Result : Path (1 .. Result_Length) end Find_Path; "
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  Syntax error (missing ';' (and rest of extended return) at
      --  82) while two parsers are sorting out a conflict.
      --
      --  both insert semicolon, which leads to identical stacks.
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (83, 85),
         Ops                     => +(Insert, +SEMICOLON_ID, 16),
         Strategy_Counts         => (Minimal_Complete => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 7, when LR1 => 7),
         Check_Low               => 2,
         Cost                    => 1);
   end Conflict_2;

   procedure Missing_Return (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text
        ("procedure McKenzie_Recover is function Check (Data : McKenzie_Data) is begin end Check; begin end; "
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90
        );
      --  Missing 'return <type>' at 69.
      --
      --  Enter recover at 'is' 69; expecting 'return'. Inserts 'return IDENTIFIER'.
      --  continues to eof, succeeds.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IS_ID,
         Error_Token_Byte_Region => (69, 70),
         Ops                     => +(Insert, +RETURN_ID, 11) & (Insert, +IDENTIFIER_ID, 11),
         Strategy_Counts         => (Minimal_Complete => 2, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 6, when LR1 => 5),
         Check_Low               => 3,
         Cost                    => 2);
   end Missing_Return;

   procedure Loop_Bounds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use WisiToken.Syntax_Trees.AUnit_Public;
   begin
      Check ("Check_Limit", Parser.Table.McKenzie_Param.Check_Limit, 3);

      Parse_Text
        ("procedure Foo is begin for I in 1 To Result_Length loop end loop; end Foo;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
         --    1    2   3  4     5   6 7  8 9  10            11   12   13 14 15 16 17
        );
      --  'To' should be '..'
      --
      --  error 1 at 'To' 35; expecting '..'.
      --
      --  The desired solution is (insert, '..') (delete 'To') cost 8.
      --  Recover finds a more complex but cheaper solution, which leads to
      --  a second error and solution.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (35, 36),
         Ops                     => +(Insert, +DOT_DOT_ID, 9) & (Fast_Forward, 10) &
           (Insert, +LOOP_ID, 10) & (Fast_Forward, 11) & (Insert, +SEMICOLON_ID, 11),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Cost                    => (case Test.Alg is when LALR => 3, when LR1 => 3));

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (71, 73),
         Ops                     => +(Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
           (Insert, +LOOP_ID, 15) & (Insert, +SEMICOLON_ID, 15),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => 6,
         Check_Low               => 3,
         Cost                    => 0);
   end Loop_Bounds;

   procedure Pattern_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text
        ("procedure Test_CASE_1 is begin case I is when 1 => A; end;"
         --        |10       |20       |30       |40       |50       |60
         --  1      2           3  4     5    6 7  8    9 10 11 13 14
         --                                                   12
        );
      --  Missing 'end case;'

      Check_Recover
        ("1",
         Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (58, 58),
         Ops                     => +(Insert, +CASE_ID, 14) & (Insert, +SEMICOLON_ID, 14) &
           (Insert, +END_ID, 14),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 69, when LR1 => 71),
         Check_Low               => 15,
         Cost                    => 3);

      --  Similar to Test_CASE_1, but error token is IDENTIFIER (and it could be dotted).
      Parse_Text
        ("procedure Test_CASE_2 is begin case I is when 1 => A; end Test_CASE_2;"
         --        |10       |20       |30       |40       |50       |60       |70
         --  1      2           3  4     5    6 7  8    9 10 11 13  14         15
         --                                                   12
        );
      --  Missing 'end case;'
      --
      --  error 1 at ';' 56; expecting 'case'.
      --

      Check_Recover
        ("2",
         Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (59, 69),
         Ops                     => +(Push_Back, +END_ID, 13) & (Insert, +END_ID, 13) &
           (Insert, +CASE_ID, 13) & (Insert, +SEMICOLON_ID, 13),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => 6,
         Check_Low               => 3,
         Cost                    => 0);

      Parse_Text
        ("procedure Test_IF is begin if A then B; end;");
      --           |10       |20       |30       |40
      --  1         2       3  4     5  6 7    8  10 11
      --                                        9

      --  Missing 'end if;'
      Check_Recover
        ("3",
         Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (44, 44),
         Ops                     => +(Insert, +IF_ID, 11) & (Insert, +SEMICOLON_ID, 11) & (Insert, +END_ID, 11),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 85, when LR1 => 87),
         Check_Low               => 17,
         Cost                    => 3);

      Parse_Text
        ("procedure Test_LOOP is begin for I in A loop B; end;");
      --           |10       |20       |30       |40       |50       |60       |70       |80
      --  1         2         3  4     5   6 7  8 9    10 12 13
      --                                                11

      --  Missing 'end loop;'

      Check_Recover
        ("4",
         Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (52, 52),
         Ops                     => +(Insert, +LOOP_ID, 13) & (Insert, +SEMICOLON_ID, 13) & (Insert, +END_ID, 13),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 108, when LR1 => 111),
         Check_Low               => 23,
         Cost                    => 3);
   end Pattern_1;

   procedure Revive_Zombie_Parser (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Patterns is Ada.Containers.Indefinite_Doubly_Linked_Lists (Pattern);");
         --        |10       |20       |30       |40       |50       |60       |70       |80

      --  A generic instantiation, but missing 'new' after 'is' 20.
      --
      --  Spawns a second parser on 'is'; one for procedure body, one for
      --  generic instantiation.
      --
      --  parser 0 for generic_instantiation errors at 'Ada' 23, expecting
      --  'new'; becomes a zombie.
      --
      --  parser 1 for subprogram_body parser keeps going, thinking it's the
      --  start of an object declaration; errors at '.' 26.
      --
      --  both parsers participate in error recovery:
      --
      --     parser 0 inserts 'new' cost 3.
      --
      --     parser 1 inserts ': IDENTIFIER' cost 7.
      --
      --  parser 0 continues to EOI, succeeds.
      --
      --  parser 1 continues to ( 77, spawns another parser. parser 1
      --  assumes 'primary', parser 2 assumes 'subtype_indication'.
      --
      --  parser 1 continues to EOI, becomes a zombie, is terminated.
      --  parser 2 continues to EOI, becomes a zombie, is terminated.
      --
      --  The three parsers have different error tokens; make sure the correct
      --  one (from the successful parser) is reported.
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (23, 25),
         Ops                     => +(Insert, +NEW_ID, 4),
         Strategy_Counts         => (Minimal_Complete => 1, others => 0),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 1);
   end Revive_Zombie_Parser;

   procedure Error_Token_When_Parallel (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("procedure One is begin if  and B then C; end if; end;");
         --        |10       |20       |30       |40       |50

      --  Missing an expression between 'if' and 'and'.
      --
      --  Spawns a second parser on 'is'; one for procedure body, one for
      --  generic instantiation. Both are still around when the error is
      --  encountered at 'and' 28. Error recovery for the procedure body
      --  finds four cost 4 solutions; the other fails on enqueue_limit.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +AND_ID,
         Error_Token_Byte_Region => (28, 30),
         Ops                     => +(Insert, +NUMERIC_LITERAL_ID, 6),
         Strategy_Counts    => (Minimal_Complete => 1, others => 0),
         Enqueue_Low        => 10,
         Check_Low          => 2,
         Cost               => 1);
   end Error_Token_When_Parallel;

   procedure If_In_Handler (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_Text_File is begin " &
         --        |10       |20       |30       |40       |50       |60
         --  1      2              3  4         5                 6  7
           "exception if then end if; end Process_Text_File; begin begin end; end Journal_To_TSV;");
         --   |67         |80       |90       |100      |110      |120      |130      |140
         --  8        9  10   11  12  14  15               16
         --                         13

      --  Mistakenly pasted 'if then end if' in exception handler 66 .. 91.
      --
      --  Enters error recovery with two parsers active; one for
      --  subprogram_body, the other for subprogram_body_stub.
      --
      --  The subprogram_body parser has the error at 'if' 76; the desired
      --  solution is (push_back and delete 'exception,
      --  sequence_of_statement_opt') cost 8 (since
      --  sequence_of_statements_opt is empty).
      --
      --  Minimal_Complete find solutions for both parsers, and both
      --  encounter more errors. The details change with small code changes,
      --  so we don't check for the actual solution that gets to EOI, just
      --  that one does; there is no exception from Parse_Text.
   end If_In_Handler;

   procedure Zombie_In_Resume (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("package body Ada_Mode.Loop_face  ");
      --           |10       |20       |30

      --  Just started typing a package
      --
      --  This used to raise a Programmer_Error because of a zombie parser
      --  during resume.
      --
      --  Enters error recovery at Wisi_EOI, inserts 'is end;'
      --
      --  During resume, a second parser is spawned on 'is', and errors on
      --  'end'; the parser does not become a zombie, but is terminated
      --  immediately. The first parser continues thru EOI.

      Check_Recover
        (Errors_Length   => 1,
         Error_Token_ID  => +Wisi_EOI_ID,
         Ops             => +(Insert, +IS_ID, 6) & (Insert, +END_ID, 6) & (Insert, +SEMICOLON_ID, 6),
         Strategy_Counts => (Minimal_Complete => 3, others => 0),
         Enqueue_Low     => 15,
         Check_Low       => 4,
         Cost            => 3);
   end Zombie_In_Resume;

   procedure Push_Back_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Tests that Push_Back by Language_Fixes works.

      Parse_Text
        ("procedure Remove is begin loop A := B; loop; end Remove;");
         --        |10       |20       |30       |40       |50
         --  1      2      3  4     5    6 7  8  10  11    13    14
         --                                    9       12

      --  Typed 'loop;' instead of 'end loop;'
      --
      --  Error at ';' 44. Desired solution is (push_back 'loop')(insert
      --  'end'); Language_Fix finds a different solution, because it
      --  assumes forgetting to type 'end' is not a typical error.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (44, 44),
         Ops                     => +(Insert, +IDENTIFIER_ID, 11) & (Fast_Forward, 13) &
           (Push_Back, +END_ID, 12) & (Insert, +END_ID, 12) & (Insert, +LOOP_ID, 12) &
           (Insert, +SEMICOLON_ID, 12) & (Fast_Forward, 13) & (Push_Back, +END_ID, 12) &
           (Insert, +END_ID, 12) & (Insert, +LOOP_ID, 12) & (Insert, +SEMICOLON_ID, 12),
         Strategy_Counts         => (Language_Fix => 2, Matching_Begin => 1, others => 0),
         Enqueue_Low             => 42,
         Check_Low               => 9,
         Cost                    => 0);
   end Push_Back_1;

   procedure String_Quote_0 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that syntax error recovery handles a missing string quote;
      --  Try_Insert_Quote case a.
      --
      --  See also String_Quote_*, ada_mode-recover_bad_char.adb,
      --  ada_mode-recover_string_quote_*.

      Parse_Text
        ("procedure Remove is begin A := ""B""; A := ""C"" &" & ASCII.LF & "at""; " & ASCII.LF & "end Remove;");
      --            |10       |20       |30         |40    |45                 |50     |52

      --  In process of splitting a string across two lines; missing open
      --  quote at 48.
      --
      --  lexer error at '"' 50. The lexer has skipped to LF 52, but then
      --  backtracked to ';' 51.
      --
      --  Desired solution is insert quote char before 'at'. Recover entered
      --  at '"' 50, finds the desired solution, succeeds.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +STRING_LITERAL_ID,
         Error_Token_Byte_Region => (50, 50),
         Ops                     => +(Push_Back, +IDENTIFIER_ID, 13) & (Delete, +IDENTIFIER_ID, 13) &
           (Fast_Forward, 14),
         Strategy_Counts         => (String_Quote => 1, others => 0),
         Enqueue_Low             => 32,
         Check_Low               => 3,
         Cost                    => 1);
   end String_Quote_0;

   procedure Missing_Name_0 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; exception end; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70

      --  Missing 'Remove' at 66, from editing.
      --
      --  Enters error recovery at 'end' 68, with a Missing_Name_Error.
      --  There are three possible fixes here; 'ignore error', 'insert begin
      --  53', 'delete end; 63'. The choice depends on the user intent, but
      --  we cannot fully discern that.
      --
      --  See Missing_Name_1; there is no way to distinguish this case from
      --  that, other than parsing to EOI.
      --
      --  See Missing_Name_2, _3; those have no 'exception'. It is more
      --  likely that there is a missing 'begin' than an extra 'end' after
      --  'exception', so we choose 'insert begin' for this, and 'delete
      --  end' for those.
      --
      --  For all four Missing_Name_* tests, Language_Fixes enqueues
      --  two solutions.
      --
      --  In this case, the desired fix is 'insert "Remove" 68', which
      --  recover doesn't do; it's equivalent to 'ignore error', cost 2.
      --
      --  With help from Language_Fixes, Recover finds a cost 1 solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 15) &
           (Push_Back, +name_opt_ID, 15) & (Push_Back, +END_ID, 14) &
           (Push_Back, +handled_sequence_of_statements_ID, 9) & (Insert, +BEGIN_ID, 9) & (Fast_Forward, 19) &
           (Push_Back, +SEMICOLON_ID, 18) & (Push_Back, +name_opt_ID, 17) & (Push_Back, +END_ID, 16) &
           (Insert, +END_ID, 16) & (Insert, +SEMICOLON_ID, 16),
         Strategy_Counts         => (Language_Fix => 2, others => 0),
         Enqueue_Low             => 6,
         Check_Low               => 6,
         Cost                    => 1,
         Code                    => Missing_Name_Error);
   end Missing_Name_0;

   procedure Missing_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; exception end; A := B; end Remove; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  1       2    3 4  5         6      7  8     9 10 11 13        14 15  17 18 20  21    22    24 26        27
      --                                                    12               16    19            23   25           28

      --  Missing 'begin' 45. Enters error recovery at A 68 with
      --  Missing_Name_Error. See Missing_Name_0 for general discussion. See
      --  Missing_Name_0; there is no way to distinguish the two, other than
      --  parsing to EOI. So Language_Fixes returns two solutions;
      --  'ignore error', and 'push_back, insert begin'.
      --
      --  'ignore error' fails the first check, since "A := B;" is not a
      --  legal declaration.
      --
      --  'push_back, insert' is the result of recovery, and parsing
      --  succeeds to EOI.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 15) &
           (Push_Back, +name_opt_ID, 15) & (Push_Back, +END_ID, 14) &
           (Push_Back, +handled_sequence_of_statements_ID, 9) & (Insert, +BEGIN_ID, 9),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => 12,
         Check_Low               => 3,
         Cost                    => 1,
         Code                    => Missing_Name_Error);
   end Missing_Name_1;

   procedure Missing_Name_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; end; A := B; end Remove; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  1       2    3 4  5         6      7  8     9 10 11 13 14  16
      --                                                    12     15

      --  Excess 'end' 53, from editing. Error recovery entered at A 58,
      --  with Missing_Name_Error. See Missing_Name_0 for general
      --  discussion. See Missing_Name_3; there is no way to distinguish
      --  this case from that, other than parsing to EOI. So
      --  Language_Fixes returns two solutions, 'ignore error' and
      --  'push_back, delete end;'.
      --
      --  In this case, only 'push_back, delete end;' leads to a valid
      --  solution in Recover; it then parses to EOI.
      --
      --  This is also an example of multiple Push_Back and Undo_Reduce in a
      --  solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     =>
           +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 14) &
             (Push_Back, +name_opt_ID, 14) & (Push_Back, +END_ID, 13) &
             (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_ID, 1) &
             (Delete, +END_ID, 13) & (Delete, +SEMICOLON_ID, 14),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => 11,
         Check_Low               => 2,
         Cost                    => 1,
         Code                    => Missing_Name_Error);
   end Missing_Name_2;

   procedure Missing_Name_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; end; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70
      --  1       2    3 4  5         6      7  8     9 10 11 13   15  16 18        19

      --  Missing 'Remove' 56. Enters error recovery on 'end' 58 with
      --  Missing_Name_Error. See Missing_Name_0 for general discussion. See
      --  Missing_Name_2; there is no way to distinguish this case from
      --  that, other than parsing to EOI. So Language_Fixes returns
      --  two solutions; 'ignore error' and 'push_back, delete end;'.
      --
      --  In this case, 'ignore error' passes recover check, but is
      --  more expensive.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 14) &
           (Push_Back, +name_opt_ID, 14) & (Push_Back, +END_ID, 13) &
           (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_ID, 1) &
           (Delete, +END_ID, 13) & (Delete, +SEMICOLON_ID, 14) & (Fast_Forward, 18) & (Push_Back, +SEMICOLON_ID, 17) &
           (Push_Back, +name_opt_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
           (Insert, +SEMICOLON_ID, 15),
         Strategy_Counts         => (Language_Fix => 2, others => 0),
         Enqueue_Low             => 5,
         Check_Low               => 5,
         Cost                    => 1,
         Code                    => Missing_Name_Error);
   end Missing_Name_3;

   procedure Missing_Name_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is package body Remove is A : Integer; end; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70

      --  Missing 'Remove' 58. Enters error recovery on 'end' 60 with
      --  Missing_Name_Error.
      --
      --  In this case, 'ignore error' is the only solution returned by
      --  Language_Fixes. The check immediately succeeds, and that is
      --  the result from recover.
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +name_ID,
         Error_Token_Byte_Region => (32, 37),
         Ops                     => +(Undo_Reduce, +package_body_ID, 9),
         Strategy_Counts         => (Ignore_Error => 1, others => 0),
         Enqueue_Low             => 1,
         Check_Low               => 1,
         Cost                    => 2,
         Code                    => Missing_Name_Error);
   end Missing_Name_4;

   procedure Missing_Name_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Proc_1 is procedure Proc_2 is begin null; end; begin null; end Proc_1;");
      --           |10       |20       |30       |40       |50       |60       |70
      --  1         2      3  4         5      6  7     8   9 10 11 12   13  14 15  16

      --  Missing 'Proc_2' 68. Enters error recovery on 'begin' 58 with
      --  Missing_Name_Error.
      --
      --  Language_Fixes enqueues (push_back ...).
      --
      --  In LALR, that fails at EOI with an Extra_Name_Error; in LR1, with
      --  a parser error - expecting another 'end'. The difference is
      --  because LALR merges lookaheads.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (21, 36),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 11) &
           (Push_Back, +name_opt_ID, 11) & (Push_Back, +END_ID, 10) &
           (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_ID, 1) &
           (Delete, +END_ID, 10) & (Delete, +SEMICOLON_ID, 11),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Cost                    => 1,
         Code                    => Missing_Name_Error);

      case Test.Alg is
      when LALR =>
         Check_Recover
           (Errors_Length           => 2,
            Checking_Error          => 2,
            Error_Token_ID          => +identifier_opt_ID,
            Error_Token_Byte_Region => (74, 79),
            Ops                     => +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
              (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) &
              (Push_Back, +handled_sequence_of_statements_ID, 13) & (Push_Back, +BEGIN_ID, 12) &
              (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 16,
            Check_Low               => 4,
            Cost                    => 1,
            Code                    => Extra_Name_Error);

      when LR1                      =>
         Check_Recover
           (Errors_Length           => 2,
            Checking_Error          => 2,
            Error_Token_ID          => +Wisi_EOI_ID,
            Error_Token_Byte_Region => (80, 79),
            Ops                     => +(Push_Back, +SEMICOLON_ID, 17) & (Push_Back, +identifier_opt_ID, 16) &
              (Push_Back, +END_ID, 15) & (Push_Back, +handled_sequence_of_statements_ID, 13) &
              (Push_Back, +BEGIN_ID, 12) & (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 12) &
              (Insert, +SEMICOLON_ID, 12),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 19,
            Check_Low               => 5,
            Cost                    => 1,
            Expecting               => WisiToken.To_Token_ID_Set
              (Descriptor.First_Terminal,
               Descriptor.Last_Terminal,
               (+BEGIN_ID, +CASE_ID, +DECLARE_ID, +END_ID, +EXIT_ID, +EXCEPTION_ID, +FOR_ID, +IF_ID, +LOOP_ID,
                +RETURN_ID, +IDENTIFIER_ID)));
      end case;
   end Missing_Name_5;

   procedure Block_Match_Names_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text
        ("package body Debug is procedure Find_First is begin begin Match (Middle_Initial_Pat); end Find_First;" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
           "procedure Swap_Names is begin end Swap_Names; begin end Debug; ");
      --    |102    |110

      --  Missing 'end' 87.
      --
      --  Error recovery entered at 'procedure' 102 with Extra_Name_Error.
      --  The desired fix is (Push_Back 'Find_First ;', Insert '; end ;').
      --  The found solution is close to that.
      case Test.Alg is
      when LALR =>
         Check_Recover
           (Errors_Length           => 1,
            Error_Token_ID          => +identifier_opt_ID,
            Error_Token_Byte_Region => (91, 100),
            Ops                     => +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
              (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
              (Insert, +SEMICOLON_ID, 15),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 18,
            Check_Low               => 4,
            Cost                    => 1,
            Code                    => Extra_Name_Error);

      when LR1 =>
         Check_Recover
           (Errors_Length           => 1,
            Error_Token_ID          => +PROCEDURE_ID,
            Error_Token_Byte_Region => (102, 110),
            Ops                     => +(Push_Back, +SEMICOLON_ID, 17) & (Push_Back, +identifier_opt_ID, 16) &
              (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) & (Insert, +SEMICOLON_ID, 15),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 20,
            Check_Low               => 5,
            Cost                    => 1);
      end case;
   end Block_Match_Names_1;

   procedure Two_Parsers_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parse_Text ("package body Debug is procedure A is end Debug;");
      --           |1       |10       |20       |30       |40

      --  Missing 'begin end A;' 35.
      --
      --  Error recovery entered at 'end' 38 with two parsers, expecting
      --  'separate' and 'begin | declaration'.
      --
      --  Recovery finds one solution for each parser, both continue to EOI;
      --  the parser with the longest recover ops is chosen.
      --
      --  LR1 has many more enqueues, because it sees a parse error at EOI
      --  instead of Match_Names_Error (because LALR merges lookaheads).

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (38, 40),
         Ops                     => +(Insert, +BEGIN_ID, 8) & (Fast_Forward, 11) &
           (Push_Back, +SEMICOLON_ID, 10) & (Push_Back, +name_opt_ID, 9) & (Push_Back, +END_ID, 8) &
           (Insert, +END_ID, 8) & (Insert, +SEMICOLON_ID, 8),
         Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 19, when LR1 => 56),
         Check_Low               => (case Test.Alg is when LALR => 5, when LR1 => 7),
         Cost                    => 1);
   end Two_Parsers_1;

   procedure Extra_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is procedure To_Month is begin" &
           --      |10       |20       |30       |40       |50       |60       |70       |80
           --  1    2              3  4         5                6  7         8        9  10
           " begin end Process_CSV_File; begin end Journal_To_TSV;");
      --    |86 |90       |100      |110      |120      |130      |140
      --     11    12  13              14 15   16  17            18

      --  Missing 'end To_Month;' at 86.
      --
      --  Error recovery entered at 'begin' 115, with Extra_Name_Error from
      --  the preceding block ("" begin 87 .. "Process_CSV_File" ; 113).
      --  Desired solution is (push_back block_statement), (insert 'end ;')
      --
      --  Language_Fixes for Extra_Name_Error enqueues the push_backs,
      --  minimal_complete finds the desired solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (97, 112),
         Ops                     => +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 14) &
           (Push_Back, +identifier_opt_ID, 13) & (Push_Back, +END_ID, 12) &
           (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 11) &
           (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 11) &
           (Insert, +SEMICOLON_ID, 11),
         Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
         Enqueue_Low             => 5,
         Check_Low               => 4,
         Cost                    => 1,
         Code                    => Extra_Name_Error);
   end Extra_Name_1;

   procedure Extra_Name_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is procedure To_Month is" &
           --      |10       |20       |30       |40       |50       |60       |70       |80
           --   1   2              3  4         5                6  7         8        9
           " procedure A is begin begin end Process_CSV_File; begin end Journal_To_TSV;");
      --    |80       |90       |100      |110      |120      |130      |140
      --     10        11 12 13   14    15  16              17 18   19  20            21

      --  Similar to Extra_Name_1; here we are missing 'end A; begin end
      --  To_Month;' at 101.
      --
      --  Error recovery entered at 'begin' 130, with Extra_Name_Error from
      --  the preceding block ("" begin 102 .. "Process_CSV_File;" 112).
      --
      --  Language_Fixes finds case Extra_Name_Error 1;
      --  enqueues (push_back 'begin end name_opt ;', insert 'end'); Minimal_Complete finds a solution.
      --

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (112, 127),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
             (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 14) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 14) &
             (Insert, +SEMICOLON_ID, 14) & (Fast_Forward, 18) &
             (Push_Back, +SEMICOLON_ID, 17) & (Push_Back, +name_opt_ID, 16) & (Push_Back, +END_ID, 15) &
             (Insert, +END_ID, 15) & (Insert, +SEMICOLON_ID, 15) & (Insert, +BEGIN_ID, 15),
         Strategy_Counts         => (Language_Fix => 2, Minimal_Complete => 2, others => 0),
         Enqueue_Low             => 14,
         Check_Low               => 8,
         Cost                    => 2,
         Code                    => Extra_Name_Error);
   end Extra_Name_2;

   procedure Extra_Name_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is begin begin" &
           --      |10       |20       |30       |40       |50       |60       |70
           " end Process_CSV_File; begin end Journal_To_TSV;");
      --    |70       |80       |90       |100      |110

      --  Similar to Extra_Name_1; here we are missing 'end;' at 65.
      --  Solution is to insert 'end ;' at 70.
      --
      --  Error recovery entered at 'begin' 93, with Extra_Name_Error from
      --  the preceding block ("" begin 65 .. "Process_CSV_File;" 75).
      --
      --  Desired solution is (push_back 'end name_opt ;'), (insert 'end ;')
      --
      --  Language_Fixes Extra_Name_Error enqueues the desired solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (75, 90),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 11) &
             (Push_Back, +identifier_opt_ID, 10) & (Push_Back, +END_ID, 9) & (Insert, +END_ID, 9) &
             (Insert, +SEMICOLON_ID, 9),
         Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
         Enqueue_Low             => 5,
         Check_Low               => 4,
         Cost                    => 1,
         Code                    => Extra_Name_Error);
   end Extra_Name_3;

   procedure Two_Missing_Ends (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parser.Table.McKenzie_Param.Ignore_Check_Fail := 4;

      Parse_Text
        ("package body Pack_1 is procedure Proc_1 is procedure Proc_A is begin case B is when 1 => a;" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90
           -- 1   2    3      4  5         6      7  8         9      10 11    12   13 14 15  16   18
           --                                                                                   17  19
           " begin end Proc_1; end Pack_1;");
      --    |92     |100      |110
      --     20    21  22    23 24 25    26
      --
      --  Missing 'end case; end Proc_A;' 91; a typical editing situation.
      --
      --  Error recovery 1 entered at 'end' 111, with Extra_Name_Error from
      --  the preceding block (no label on preceding 'begin').
      --
      --  The desired solution is (push_back block_statement, insert 'end
      --  case ; end ;'). With help from Language_Fix and
      --  Minimal_Complete_Actions, an equivalent solution is found.

      --  LR1 has many more enqueues, because it sees a parse error at EOI
      --  instead of Match_Names_Error (because LALR merges lookaheads).

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (103, 108),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 23) &
             (Push_Back, +identifier_opt_ID, 22) & (Push_Back, +END_ID, 21) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 20) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 20) &
             (Insert, +CASE_ID, 20) & (Insert, +SEMICOLON_ID, 20) & (Fast_Forward, 24) &
             (Push_Back, +SEMICOLON_ID, 23) & (Push_Back, +identifier_opt_ID, 22) & (Push_Back, +END_ID, 21) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 20) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 20) & (Insert, +SEMICOLON_ID, 20),
         Strategy_Counts         => (Language_Fix => 2, Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 26, when LR1 => 40),
         Check_Low               => 9,
         Cost                    => 3,
         Code                    => Extra_Name_Error);
   end Two_Missing_Ends;

   procedure Match_Selected_Component_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Ada_Mode.Interactive_2 is procedure Proc_2 is begin null; begin null; end" &
           --      |10       |20       |30       |40       |50       |60       |70       |80
           " Ada_Mode.Interactive_2;");
      --    |84   |90       |100      |110

      --  Missing 'end Proc_2;' 68. Enters error recovery on '.' 93
      --  expecting ';'.
      --
      --  This is similar to an Extra_Name_Error from a semantic check, and
      --  the fix is the same. It provided the first rationale for expanding
      --  Semantic_Check_Fixes into Language_Fixes.
      --
      --  Language_Fixes returns two solutions.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +DOT_ID,
         Error_Token_Byte_Region => (93, 93),
         Ops                     =>
           +(Push_Back, +IDENTIFIER_ID, 16) & (Push_Back, +END_ID, 15) &
             (Push_Back, +handled_sequence_of_statements_ID, 13) & (Push_Back, +BEGIN_ID, 12) &
             (Push_Back, +block_label_opt_ID, 12) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 23, when LR1 => 35),
         Check_Low               => (case Test.Alg is when LALR => 8, when LR1 => 8),
         Cost                    => 0);
   end Match_Selected_Component_1;

   procedure Match_Selected_Component_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Ada_Mode.Recover_6 is begin declare name : int; begin null; end Ada_Mode.Recover_6;");
      --           |10       |20       |30       |40       |50       |60       |70       |80

      --  Missing 'end;' 70. Enters error recovery on '.' 83
      --  expecting ';'. Language_Fixes provides a solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +DOT_ID,
         Error_Token_Byte_Region => (83, 83),
         Ops                     =>
           +(Push_Back, +IDENTIFIER_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
             (Insert, +SEMICOLON_ID, 15),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Enqueue_Low             => 6,
         Check_Low               => 3,
         Cost                    => 0);
   end Match_Selected_Component_2;

   procedure Actual_Parameter_Part_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Slow_Recover_2 is begin if 0 /= Input (Context, Name end Slow_Recover_2;");
      --           |10       |20       |30       |40       |50       |60       |70       |80

      --  Missing ') then end if;' 63. Enters error recovery on 'end' 64
      --  expecting lots of things.
      --
      --  Desired solution is ((insert ') then end if;').
      --
      --  Previous version found that after enqueue 3291; now enqueues much less.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (64, 66),
         Ops                     => +(Insert, +RIGHT_PAREN_ID, 13) & (Insert, +THEN_ID, 13) &
           (Fast_Forward, 14) & (Push_Back, +END_ID, 13) & (Insert, +END_ID, 13) & (Insert, +IF_ID, 13) &
           (Insert, +SEMICOLON_ID, 13),
      Strategy_Counts            => (Language_Fix => 1, Minimal_Complete => 2, others => 0),
      Enqueue_Low                => (case Test.Alg is when LALR => 119, when LR1 => 120),
      Check_Low                  => 11,
      Cost                       => 2);
   end Actual_Parameter_Part_1;

   procedure Unfinished_Subprogram_Type_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_2.adb
      Parse_Text
        ("package body Debug is function Function_Access_1 (A_Param : Float) return Float is begin" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90
           --  1  2    3     4  5        6                 7 8      9 10   11 12    13    14 15
           " type Wait_Return is (Read_Success,); end Debug;");
      --     |90       |100      |110      |120      |130
      --     16   17          18 19 28        21  24  25   26
      --                                       22
      --                                        23

      --  Missing 'end Function_Access_1;' 89 and '<identifier>' 124.
      --
      --  Reported 'error in resume' after both recoveries, now fixed.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +BEGIN_ID,
         Error_Token_Byte_Region => (84, 88),
         Ops                     => +(Delete, +BEGIN_ID, 15) & (Insert, +SEPARATE_ID, 16) &
           (Insert, +SEMICOLON_ID, 16),
         Strategy_Counts         => (Minimal_Complete => 2, Delete => 1, others => 0),
         Cost                    => 3);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +RIGHT_PAREN_ID,
         Error_Token_Byte_Region => (124, 124),
         Ops                     => +(Insert, +IDENTIFIER_ID, 22),
         Strategy_Counts         => (Minimal_Complete => 1, others => 0),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 1);
   end Unfinished_Subprogram_Type_1;

   procedure String_Quote_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-recover_string_quote_2.adb. Try_Insert_Quote case e.
      Parse_Text
        ("procedure Handle_Search is begin if Is_Empty then return ""text/html""; else Response := " &
           --      |10       |20       |30       |40       |50        |60        |70       |80       |90
           --  1    2             3  4     5  6        7    8      9           10 11   12       13
           """</table>""</body></html>"";" & ASCII.LF & "end if; end Handle_Search;");
      --     |88          |100      |110     |114             |120      |130      |140
      --     14         15    18 21
      --                 16    19
      --                  17    20

      --  The actual editing error was to leave an extra quote at 97. To the
      --  lexer this looks like a missing quote to match '"' 112; when it gets
      --  that far, it inserts a matching quote at 112.
      --
      --  Error recover only tries inserting new quotes, not deleting
      --  existing ones; the later involves resetting the lexer, which we
      --  don't support.
      --
      --  Recover entered at '/' 99, before the lexer error.
      --
      --  It inserts a virtual quote before '/' 98, succeeds.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SLASH_ID,
         Error_Token_Byte_Region => (99, 99),
         Ops                     => +(Delete, +SLASH_ID, 16) & (Delete, +BODY_ID, 17) & (Delete, +GREATER_ID, 18) &
           (Delete, +LESS_ID, 19) & (Delete, +SLASH_ID, 20) & (Delete, +IDENTIFIER_ID, 21) &
           (Delete, +GREATER_ID, 22) & (Fast_Forward, 23),
         Strategy_Counts         => (String_Quote => 1, others => 0),
         Enqueue_Low             => 42,
         Check_Low               => 6,
         Cost                    => 1);
   end String_Quote_1;

   procedure String_Quote_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Try_Insert_Quote case b.

      Parse_Text
        ("procedure Handle_Search is begin if Is_Empty then return ""text/html""; else Response := " &
           --      |10       |20       |30       |40       |50        |60        |70       |80       |90
           --  1    2             3  4     5  6        7    8      9           10 11   12       13
           """</table></body></html>;" & ASCII.LF & "end if; end Handle_Search;");
      --     |88         |100      |110  |112               |120      |130      |140
      --     14                     27               28
      --      15

      --  The actual error is a missing quote before ';' 110. Lexer detects
      --  the unbalanced quote at 88, and inserts a matching quote there.
      --
      --  Recover entered at '/' 90, after the lexer error. It moves the
      --  inserted quote to just before LF 112, then inserts ';'. That
      --  parses to EOI.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SLASH_ID,
         Error_Token_Byte_Region => (90, 90),
         Ops                     => +(Push_Back, +LESS_ID, 15) & (Push_Back, +simple_expression_ID, 14) &
           (Fast_Forward, 15) & (Delete, +LESS_ID, 15) & (Delete, +SLASH_ID, 16) & (Delete, +IDENTIFIER_ID, 17) &
           (Delete, +GREATER_ID, 18) & (Delete, +LESS_ID, 19) & (Delete, +SLASH_ID, 20) & (Delete, +BODY_ID, 21) &
           (Delete, +GREATER_ID, 22) & (Delete, +LESS_ID, 23) & (Delete, +SLASH_ID, 24) & (Delete, +IDENTIFIER_ID, 25) &
           (Delete, +GREATER_ID, 26) & (Delete, +SEMICOLON_ID, 27) & (Fast_Forward, 28) &
           (Insert, +SEMICOLON_ID, 28),
         Strategy_Counts => (Minimal_Complete => 1, String_Quote => 1, others => 0),
         Enqueue_Low     => (case Test.Alg is when LALR => 61, when LR1 => 69),
         Check_Low       => 7,
         Cost            => 2);
   end String_Quote_2;

   procedure String_Quote_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Try_Insert_Quote case c
      Parse_Text
        ("procedure Handle_Search is begin if Is_Empty then return ""text/html""; else Response := " &
           --      |10       |20       |30       |40       |50        |60       |70       |80
           --  1    2             3  4     5  6        7    8      9           10 11   12       13
           """</table>"" & </body></html>"";" & ASCII.LF & "end if; end Handle_Search;");
      --     |88          |100      |110   |116               |120      |130      |140
      --     14          15               24                25
      --                   16


      --  Actual error is missing quote at 102.
      --
      --  Recover entered at '<' 101, before lexer error.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +LESS_ID,
         Error_Token_Byte_Region => (101, 101),
         Ops                     => +(Delete, +LESS_ID, 16) & (Delete, +SLASH_ID, 17) & (Delete, +BODY_ID, 18) &
           (Delete, +GREATER_ID, 19) & (Delete, +LESS_ID, 20) & (Delete, +SLASH_ID, 21) &
           (Delete, +IDENTIFIER_ID, 22) & (Delete, +GREATER_ID, 23) & (Fast_Forward,  24),
         Strategy_Counts         => (String_Quote => 1, others => 0),
         Enqueue_Low             => 51,
         Check_Low               => 6,
         Cost                    => 1);
   end String_Quote_3;

   procedure String_Quote_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  From ada_mode-recover_string_quote_1, simplified to just missing
      --  quote. Try_Insert_Quote case d.

      Parse_Text
        ("procedure Quote_Unquote is begin Put_Line (Test_File, ""8"" & Tab & ""nine""); " & ASCII.LF &
           --      |10       |20       |30       |40       |50         |60        |70        |76
           --  1    2             3  4     5        6 7       8  9    10 11 12 13     14
           --                                                                          15
           "Put_Line (ten"" & Tab & "" eleven""); Close (Test_File); end Quote_Unquote;");
      --    |77 |81       |90        |100       |110      |120      |130      |140
      --    16       17  19            20    21   24
      --              18                       22
      --                                        23

      --  Actual error is missing '"' at 87.
      --
      --  Parser enters McKenzie recover at '".."' 90..100, before the lexer error
      --  at 108.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +STRING_LITERAL_ID,
         Error_Token_Byte_Region => (90, 100),
         Ops                     => +(Push_Back, +IDENTIFIER_ID, 18) & (Delete, +IDENTIFIER_ID, 18) &
           (Delete, +STRING_LITERAL_ID, 19) & (Delete, +IDENTIFIER_ID, 20) & (Fast_Forward, 21),
         Strategy_Counts         => (String_Quote => 1, others => 0),
         Enqueue_Low             => 77,
         Check_Low               => (case Test.Alg is when LALR => 6, when LR1 => 6),
         Cost                    => 1);
   end String_Quote_4;

   procedure String_Quote_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Test that syntax error recovery handles a missing string quote at EOI.
      --
      --  See also String_Quote_*, ada_mode-recover_bad_char.adb,
      --  ada_mode-recover_string_quote_*.

      Parse_Text
        ("procedure Remove is begin A := B"";" & ASCII.CR & "-- trailing comment");
      --           |10       |20       |30
      --  1         2      3  4     5 6  7 8 9               10

      --  In process of splitting a string across two lines; missing open
      --  quote at 32.
      --
      --  lexer error at '"' 33.
      --
      --  Desired solution is insert quote char before 'B'; explore finds a
      --  different one.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +STRING_LITERAL_ID,
         Error_Token_Byte_Region => (33, 33),
         Ops                     => +(Push_Back, +IDENTIFIER_ID, 7) & (Delete, +IDENTIFIER_ID, 7) &
           (Fast_Forward, 10) & (Insert, +END_ID, 10) & (Insert, +SEMICOLON_ID, 10),
         Strategy_Counts         => (Minimal_Complete => 2, String_Quote => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 230, when LR1 => 235),
         Check_Low               => 22,
         Cost                    => 3);
   end String_Quote_5;

   procedure String_Quote_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Recover used to delete the same tokens more than once for this.

      Parse_Text
        ("for ID loop" & ASCII.LF & " Recursive"") ;");
      --  1   2  3       4            5        6 7 8

      --  Missing "in foo" before loop, and missing terminating string quote
      --  at EOI. Try_Insert_Quote is called with an error on 5:'Recursive',
      --  which is _not_ due to the string quote; it must take care not
      --  cross a line boundary. In the end, the solutions enqueued by
      --  Try_Insert_Quote are not the least expensive.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +LOOP_ID,
         Error_Token_Byte_Region => (8, 11),
         Ops                     => +(Insert, +IN_ID, 3) & (Insert, +IDENTIFIER_ID, 3) & (Fast_Forward, 5) &
           (Insert, +LEFT_PAREN_ID, 5),
         Strategy_Counts         => (Minimal_Complete => 2, Insert => 1, others => 0),
         Cost                    => 6);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +Wisi_EOI_ID,
         Error_Token_Byte_Region => (26, 25),
         Ops                     => +(Insert, +END_ID, 8) & (Insert, +LOOP_ID, 8) & (Insert, +SEMICOLON_ID, 8),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 14, when LR1 => 15),
         Check_Low               => 4,
         Cost                    => 3);
   end String_Quote_6;

   procedure Enqueue_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test fail on Enqueue_Limit. Same input as Loop_Bounds above.
      --
      --  When Enqueue_Limit is hit, worker tasks stop dequeing configs, but
      --  any active workers will finish enqueuing new ones.

      Parser.Table.McKenzie_Param.Enqueue_Limit := 20;

      Parse_Text
        ("procedure Foo is begin for I in 1 To Result_Length loop end loop; end Foo;",
         Multiple_Tasks   => False,
         Expect_Exception => True);

      --  One error message from the syntax error, one from recover fail
      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (35, 36),
         Ops_Race_Condition      => True,
         Enqueue_Low             => 20,
         Enqueue_High            => 70,
         Check_Low               => 3,
         Check_High              => 20,
         Cost                    => 0);
   end Enqueue_Limit;

   procedure Minimal_Complete_Full_Reduce_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Test McKenzie_Recover.Explore.Insert_Minimal_Complete_Actions when
      --  it reduces, but then inserts nothing (it reduces to
      --  compilation_unit_list). It used to not try any insertions; now it
      --  uses Matching_Begin_Token to start the appropriate production.

      Parse_Text ("A; exception when A => null; end Debug;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +EXCEPTION_ID,
         Error_Token_Byte_Region => (4, 12),
         Ops                     => +(Insert, +BEGIN_ID, 3),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 17, when LR1 => 16),
         Check_Low               => 2,
         Cost                    => 0);
   end Minimal_Complete_Full_Reduce_1;

   procedure Minimal_Complete_Full_Reduce_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Similar to Minimal_Complete_Full_Reduce_1; Matching_Begin_Token
      --  and minimal_complete quickly gives "insert if NUMERIC_LITERAL
      --  then".

      Parse_Text ("A; end if;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (4, 6),
         Ops                     => +(Insert, +IF_ID, 3) & (Insert, +NUMERIC_LITERAL_ID, 3) & (Insert, +THEN_ID, 3),
         Strategy_Counts         => (Matching_Begin => 1, Minimal_Complete => 2, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 79, when LR1 => 78),
         Check_Low               => 7,
         Cost                    => 2);
   end Minimal_Complete_Full_Reduce_2;

   procedure Minimal_Complete_Full_Reduce_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Don't use minimal_complete with nothing on stack; match the
      --  following tokens instead, using Matching_Begin_Token.

      Parse_Text (" end loop;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (2, 4),
         Ops                     => +(Insert, +LOOP_ID, 1),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => 15,
         Check_Low               => 2,
         Cost                    => 0);
   end Minimal_Complete_Full_Reduce_3;

   procedure No_Push_Back_Prev_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This used to test pushing back a fix, causing Config.Ops to be out
      --  of token_index order. However, the other recover ops work so well
      --  that we cannot force pushing back the first fix even by messing
      --  with the McKenzie costs. So this test now serves as a
      --  demonstration that we don't need to support Push_Back past a
      --  previous fix, which simplifies things a lot.

      Parse_Text
        ("procedure To_Non_Grammar is begin for I in Trivia loop declare" &
           --      |10       |20       |30       |40       |50       |60
           --    1  2              3  4     5   6 7  8      9    10
           " Line : Line_Number_Type := Line_Number_Type (Token.Sloc_Range.Start_Line);" &
           --      |70       |80       |90       |100      |110      |120      |130
           --  11 12 13              14 15              16 17  18 19      29 21     22 23
           " begin if Token.Kind = Ada_Comment then" &
           --  |141     |150      |160      |170
           --  24  25 26   27 28 29 30         31
           " Token.Non_Grammar.Append (+COMMENT_ID); end;" &
           --  |180      |190      |200      |210      |220
           --  32 33 34       35 36  37 38 39    40 41 42 43
           " else Error; end if; end loop; end To_Non_Grammar;");
           --       |230      |240      |250      |260      |270
           --  44

      --  "end;" 218 is extra
      --
      --  In an earlier version, the first fix inserted
      --  "end if ;", causing second error at "else"; which pushed back the first
      --  fix (causing out-of-order token indices in Ops).
      --
      --  Now we find the simpler solution below.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (221, 221),
         Ops                     => +(Push_Back, +END_ID, 42) & (Delete, +END_ID, 42) & (Delete, +SEMICOLON_ID, 43),
         Strategy_Counts         => (Push_Back => 1, Delete => 2, others => 0),
         Cost                    => 4);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +LOOP_ID,
         Error_Token_Byte_Region => (247, 250),
         Ops                     => +(Insert, +SEMICOLON_ID, 51) & (Fast_Forward, 52) &
           (Insert, +IDENTIFIER_ID, 52) & (Fast_Forward, 54) & (Push_Back, +END_ID, 53) &
           (Insert, +END_ID, 53) & (Insert, +LOOP_ID, 53) & (Insert, +SEMICOLON_ID, 53) &
           (Fast_Forward, 54) & (Push_Back, +END_ID, 53) & (Insert, +END_ID, 53) & (Insert, +LOOP_ID, 53) &
           (Insert, +SEMICOLON_ID, 53),
         Strategy_Counts            => (Language_Fix => 2, Minimal_Complete => 1, Matching_Begin => 1, others => 0),
         Enqueue_Low             => 56,
         Check_Low               => 11,
         Cost                    => 1);
   end No_Push_Back_Prev_Error;

   procedure Error_During_Resume_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  previously got "error during resume"; stack is empty in error
      --  recover, now finds one solution.

      Parse_Text ("end Process_Node;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (1, 3),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +COLON_ID, 1) &
           (Insert, +BEGIN_ID, 1),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => 15,
         Check_Low               => 2,
         Cost                    => 0);
   end Error_During_Resume_1;

   procedure Error_During_Resume_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  previously got "error during resume"; stack is empty in error
      --  recover.

      Parse_Text ("end; next (I);");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (1, 3),
         Ops                     => +(Insert, +BEGIN_ID, 1),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => 15,
         Check_Low               => 2,
         Cost                    => 0);
   end Error_During_Resume_2;

   procedure Error_During_Resume_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  previously got "error during resume"; Explore did Undo_Reduce wrong.

      Parser.Table.McKenzie_Param.Check_Limit := 4;

      Parse_Text ("Check_Recover (Enqueue_Low => A()84, Cost => 1);");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +NUMERIC_LITERAL_ID,
         Error_Token_Byte_Region => (34, 35),
         Ops_Race_Condition      => True,
         Enqueue_Low             => 60,
         Check_Low               => 27,
         Cost                    => 4);
   end Error_During_Resume_3;

   procedure Conflict_During_Resume_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Error_During_Resume_1, but name is a
      --  selected_component, so inserts 'procedure identifier is', and
      --  encounters a conflict during resume.

      Parse_Text ("end Process.Node;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (1, 3),
         Ops                     => +(Insert, +PACKAGE_ID, 1) & (Insert, +IDENTIFIER_ID, 1) & (Insert, +IS_ID, 1),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => 17,
         Check_Low               => 3,
         Cost                    => 0);
   end Conflict_During_Resume_1;

   procedure Minimal_Complete_Finish_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  Matching_Begin returns the correct start for a name with a dot.
      --  Also tests handling a conflict during resume, and inserting
      --  multiple virtual tokens.

      Parse_Text ("procedure Proc_1; end Ada_Mode.Debbugs_35124;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (19, 21),
         Ops                     => +(Insert, +PACKAGE_ID, 4) &
           (Insert, +IDENTIFIER_ID, 4) & (Insert, +IS_ID, 4),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 19, when LR1 => 18),
         Check_Low               => 3,
         Cost                    => 0);
   end Minimal_Complete_Finish_1;

   procedure Always_Minimal_Complete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  This test case provided the motivation for always doing
      --  Insert_Minimal_Complete. The user is converting an 'if then'
      --  statement to a 'case' statement. The best solution is to finish
      --  the case statement, then start an 'if'. We used to use the error
      --  token to determine if doing Minimal_Complete was appropriate;
      --  that's not possible in this case.

      Parser.Table.McKenzie_Param.Enqueue_Limit := 1500; --  This test needs a little more.

      Parse_Text
        ("case Current_Token is " &
           "= +RIGHT_PAREN_ID then Matching_Begin_Token := +LEFT_PAREN_ID; " &
           "else Matching_Begin_Token := Invalid_Token_ID; end if;");

      --  Recover entered at '='; minimal_complete partially completes the
      --  case statement, explore_table inserts 'if' and one of
      --  STRING_LITERAL, NUMERIC_LITERAL, IDENTIFIER, or deletes '=', all
      --  of which pass check. Which one survives during parse is random,
      --  which is why this result can change with minor changes to the
      --  code, and between LALR and LR1.
      --
      --  That leads to a second error at EOI; minimal complete finishes the
      --  case statement.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +EQUAL_ID,
         Error_Token_Byte_Region => (23, 23),
         Ops                     => +(Insert, +WHEN_ID, 4) & (Insert, +NUMERIC_LITERAL_ID, 4) &
           (Insert, +EQUAL_GREATER_ID, 4) & (Insert, +IF_ID, 4) & (Insert, +NUMERIC_LITERAL_ID, 4),
         Strategy_Counts         => (Minimal_Complete => 4, Insert => 1, others => 0),
         Cost                    => 8);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +Wisi_EOI_ID,
         Error_Token_Byte_Region => (139, 138),
         Ops                     => +(Insert, +END_ID, 21) & (Insert, +CASE_ID, 21) & (Insert, +SEMICOLON_ID, 21),
         Strategy_Counts         => (Minimal_Complete => 3, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 14, when LR1 => 15),
         Check_Low               => 4,
         Cost                    => 3);
   end Always_Minimal_Complete;

   procedure Always_Matching_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  This test case provided the motivation for always doing
      --  Matching_Begin; otherwise Minimal_Complete finishes both enclosing
      --  procedures before inserting 'loop'.

      Parse_Text ("procedure Find_Node is begin Iter.Current := Next_Sibling (Iter.Current); end loop; end Find_Node;");
      --           1         2         3  4     5   6 7      8  9           10 11 12 13   14 16  17  18 19 20

      --  Because of the various cost settings, recover finds the solution
      --  below.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +LOOP_ID,
         Error_Token_Byte_Region => (79, 82),
         Ops                     => +(Insert, +SEMICOLON_ID, 17) & (Fast_Forward, 18) &
           (Insert, +END_ID, 18) & (Insert, +LOOP_ID, 18) & (Fast_Forward, 19) &
           (Insert, +IDENTIFIER_ID, 19) & (Insert, +COLON_ID, 19) & (Insert, +BEGIN_ID, 19),
         Strategy_Counts         => (Minimal_Complete => 3, Matching_Begin => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 168, when LR1 => 171),
         Check_Low               => 30,
         Cost                    => 3);
   end Always_Matching_Begin;

   procedure Do_Delete_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  This test case provided the motivation for doing delete before
      --  insert. If insert is first, the desired solution is:
      --
      --  (insert '; if then', delete ')')
      --
      --  But then Matching_Begin is no help in finding (insert 'if then')
      --
      --  If delete is first:
      --
      --  (delete ')', insert '; if then')
      --
      --  and Matching_Begin works.
      --
      --  However, since insert is much more common in solutions than
      --  delete, this means we enqueue more (useless) configs than with doing insert
      --  first.

      Parse_Text ("procedure Parse is begin Foo (not Data.First)) end if; end Parse;");
      --           1         2     3  4     5   6 7  8   9 10  11 13  14  16  17

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +RIGHT_PAREN_ID,
         Error_Token_Byte_Region => (46, 46),
         Ops                     =>
           +(Delete, +RIGHT_PAREN_ID, 12) & (Insert, +SEMICOLON_ID, 13) & (Insert, +IF_ID, 13) &
             (Insert, +NUMERIC_LITERAL_ID, 13) & (Insert, +THEN_ID, 13),
         Strategy_Counts         => (Minimal_Complete => 3, Matching_Begin => 1, Delete => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 904, when LR1 => 933),
         Check_Low               => (case Test.Alg is when LALR => 123, when LR1 => 122),
         Cost                    => 7);
   end Do_Delete_First;

   procedure Forbid_Minimal_Complete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This test case provided the motivation for adding
      --  Forbid_Minimal_Complete and a cost for Matching_Begin and
      --  Fast_Forward (and another Language_Fix). The error is at
      --  14:'Find_Terminal', expecting 'if'. The desired solution is:
      --
      --  (push_back 'end', insert 'end if; end loop;')
      --
      --  Without Forbid_Minimal_Complete, this is found instead, because it
      --  is cheaper:
      --
      --  (insert 'if ;')
      --
      --  which leads to another error at EOI, and recover fails with
      --  enqueue_limit.
      --
      --  The cost on Matching_Begin and Fast_Forward prevents other solutions that pass
      --  check, but are more complex.

      Parser.Table.McKenzie_Param.Delete (+END_ID) := 4;
      Parser.Table.McKenzie_Param.Fast_Forward     := 3;
      Parser.Table.McKenzie_Param.Matching_Begin   := 3;
      --  We don't set these costs in ada_lite.wy, to avoid disturbing all
      --  the other tests here.

      Parse_Text ("declare procedure Find_Terminal is begin loop if Id then return Index; end Find_Terminal; " &
                    --           1       2         3             4  5     6    7  8  9    10     11  12 13 14
                    "begin Find_Production; end;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (76, 88),
         Ops                     => +(Push_Back, +END_ID, 13) & (Insert, +END_ID, 13) &
           (Insert, +IF_ID, 13) & (Insert, +SEMICOLON_ID, 13) & (Fast_Forward, 14) & (Push_Back, +END_ID, 13) &
           (Insert, +END_ID, 13) & (Insert, +LOOP_ID, 13) &
           (Insert, +SEMICOLON_ID, 13),
         Strategy_Counts         => (Language_Fix => 2, others => 0),
         Enqueue_Low             => 23,
         Check_Low               => 6,
         Cost                    => 0);
   end Forbid_Minimal_Complete;

   procedure Matching_Begin_Parse_All_Conflicts (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  This test case provided the motivation for Check to parse all
      --  conflicts. It also found a long-standing bug in
      --  McKenzie_Recover.Parse.Parse.

      Parse_Text ("procedure Proc_1; end Ada_Mode.Debbugs_35124;");
      --           1         2     3 4   5       6 7           8

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (19, 21),
         Ops                     => +(Insert, +PACKAGE_ID, 4) & (Insert, +IDENTIFIER_ID, 4) &
           (Insert, +IS_ID, 4),
         Strategy_Counts         => (Matching_Begin => 1, others => 0),
         Enqueue_Low             => (case Test.Alg is when LALR => 19, when LR1 => 18),
         Check_Low               => 3,
         Cost                    => 0);
   end Matching_Begin_Parse_All_Conflicts;

   procedure Check_Multiple_Delete_For_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  This test case provided the motivation for
      --  Just_Pushed_Back_Or_Deleted to check multiple previous ops.
      Ada_Lite_Actions.End_Name_Optional := False;

      Parse_Text ("procedure A is begin null; end;");
      --           1         2 3  4     5   6 7  8

      --  There is a missing 'A' after 10:'end'. If only check for one op in
      --  Just_Pushed_Back_Or_Deleted, the Language_Fixes solution followed
      --  by Minimal_Complete ends in:
      --  ... (DELETE, END, 7), (DELETE, SEMICOLON, 8), (INSERT, END, 9), (INSERT, SEMICOLON, 9)

      Check_Recover
        (Label                   => "1",
         Errors_Length           => 1,
         Code                    => Missing_Name_Error,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (1, 11),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9),
         Strategy_Counts         => (Ignore_Error => 1, others => 0),
         Enqueue_Low             => 12,
         Check_Low               => 2,
         Cost                    => 2);

      Parse_Text ("procedure C is procedure A is begin null; end; begin end C  ;");
      --           1         2 3  4         5 6  7     8   9 10 11 12   13  14 15

      --  Here the Language_Fixes solution is prefered, because it is lower
      --  cost and 12:begin looks like a block statement. Then there is a
      --  second error at EOI.
      --
      --  IMPROVEME: If the second error recovery could go back and
      --  reconsider the first error, it could do a better job here. So we
      --  keep this case to illustrate a possible improvement.
      Check_Recover
        (Label                   => "2",
         Errors_Length           => 2,
         Checking_Error          => 1,
         Code                    => Missing_Name_Error,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (16, 26),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 11) &
           (Push_Back, +name_opt_ID, 11) & (Push_Back, +END_ID, 10) &
           (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_ID, 1) &
           (Delete, +END_ID, 10) & (Delete, +SEMICOLON_ID, 11),
         Strategy_Counts         => (Language_Fix => 1, others => 0),
         Cost                    => 1);

      case Test.Alg is
      when LALR =>
         Check_Recover
           (Label                   => "2",
            Errors_Length           => 2,
            Checking_Error          => 2,
            Code                    => Extra_Name_Error,
            Error_Token_ID          => +identifier_opt_ID,
            Error_Token_Byte_Region => (58, 58),
            Ops                     => +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 15) &
              (Push_Back, +identifier_opt_ID, 14) & (Push_Back, +END_ID, 13) &
              (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 12) &
              (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 16,
            Check_Low               => 4,
            Cost                    => 1);

      when LR1 =>
         Check_Recover
           (Label                   => "2",
            Errors_Length           => 2,
            Checking_Error          => 2,
            Error_Token_ID          => +Wisi_EOI_ID,
            Error_Token_Byte_Region => (61, 60),
            Ops                     => +(Push_Back, +SEMICOLON_ID, 15) & (Push_Back, +identifier_opt_ID, 14) &
              (Push_Back, +END_ID, 13) & (Push_Back, +handled_sequence_of_statements_ID, 0) &
              (Push_Back, +BEGIN_ID, 12) & (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 12) &
              (Insert, +SEMICOLON_ID, 12),
            Strategy_Counts         => (Language_Fix => 1, Minimal_Complete => 1, others => 0),
            Enqueue_Low             => 18,
            Check_Low               => 5,
            Cost                    => 1);
      end case;
   end Check_Multiple_Delete_For_Insert;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Error'Access, "No_Error");
      Register_Routine (T, Empty_Comments'Access, "Empty_Comments");
      Register_Routine (T, Error_1'Access, "Error_1");
      Register_Routine (T, Error_2'Access, "Error_2");
      Register_Routine (T, Error_3'Access, "Error_3");
      Register_Routine (T, Error_4'Access, "Error_4");
      Register_Routine (T, Check_Accept'Access, "Check_Accept");
      Register_Routine (T, Extra_Begin'Access, "Extra_Begin");
      Register_Routine (T, Conflict_1'Access, "Conflict_1");
      Register_Routine (T, Conflict_2'Access, "Conflict_2");
      Register_Routine (T, Missing_Return'Access, "Missing_Return");
      Register_Routine (T, Loop_Bounds'Access, "Loop_Bounds");
      Register_Routine (T, Pattern_1'Access, "Pattern_1");
      Register_Routine (T, Revive_Zombie_Parser'Access, "Revive_Zombie_Parser");
      Register_Routine (T, Error_Token_When_Parallel'Access, "Error_Token_When_Parallel");
      Register_Routine (T, If_In_Handler'Access, "If_In_Handler");
      Register_Routine (T, Zombie_In_Resume'Access, "Zombie_In_Resume");
      Register_Routine (T, Push_Back_1'Access, "Push_Back_1");
      Register_Routine (T, String_Quote_0'Access, "String_Quote_0");
      Register_Routine (T, Missing_Name_0'Access, "Missing_Name_0");
      Register_Routine (T, Missing_Name_1'Access, "Missing_Name_1");
      Register_Routine (T, Missing_Name_2'Access, "Missing_Name_2");
      Register_Routine (T, Missing_Name_3'Access, "Missing_Name_3");
      Register_Routine (T, Missing_Name_4'Access, "Missing_Name_4");
      Register_Routine (T, Missing_Name_5'Access, "Missing_Name_5");
      Register_Routine (T, Block_Match_Names_1'Access, "Block_Match_Names_1");
      Register_Routine (T, Two_Parsers_1'Access, "Two_Parsers_1");
      Register_Routine (T, Extra_Name_1'Access, "Extra_Name_1");
      Register_Routine (T, Extra_Name_2'Access, "Extra_Name_2");
      Register_Routine (T, Extra_Name_3'Access, "Extra_Name_3");
      Register_Routine (T, Two_Missing_Ends'Access, "Two_Missing_Ends");
      Register_Routine (T, Match_Selected_Component_1'Access, "Match_Selected_Component_1");
      Register_Routine (T, Match_Selected_Component_2'Access, "Match_Selected_Component_2");
      Register_Routine (T, Actual_Parameter_Part_1'Access, "Actual_Parameter_Part_1");
      Register_Routine (T, Unfinished_Subprogram_Type_1'Access, "Unfinished_Subprogram_Type_1");
      Register_Routine (T, String_Quote_1'Access, "String_Quote_1");
      Register_Routine (T, String_Quote_2'Access, "String_Quote_2");
      Register_Routine (T, String_Quote_3'Access, "String_Quote_3");
      Register_Routine (T, String_Quote_4'Access, "String_Quote_4");
      Register_Routine (T, String_Quote_5'Access, "String_Quote_5");
      Register_Routine (T, String_Quote_6'Access, "String_Quote_6");
      Register_Routine (T, Enqueue_Limit'Access, "Enqueue_Limit");
      Register_Routine (T, Minimal_Complete_Full_Reduce_1'Access, "Minimal_Complete_Full_Reduce_1");
      Register_Routine (T, Minimal_Complete_Full_Reduce_2'Access, "Minimal_Complete_Full_Reduce_2");
      Register_Routine (T, Minimal_Complete_Full_Reduce_3'Access, "Minimal_Complete_Full_Reduce_3");
      Register_Routine (T, No_Push_Back_Prev_Error'Access, "No_Push_Back_Prev_Error");
      Register_Routine (T, Error_During_Resume_1'Access, "Error_During_Resume_1");
      Register_Routine (T, Error_During_Resume_2'Access, "Error_During_Resume_2");
      Register_Routine (T, Error_During_Resume_3'Access, "Error_During_Resume_3");
      Register_Routine (T, Conflict_During_Resume_1'Access, "Conflict_During_Resume_1");
      Register_Routine (T, Minimal_Complete_Finish_1'Access, "Minimal_Complete_Finish_1");
      Register_Routine (T, Always_Minimal_Complete'Access, "Always_Minimal_Complete");
      Register_Routine (T, Always_Matching_Begin'Access, "Always_Matching_Begin");
      Register_Routine (T, Do_Delete_First'Access, "Do_Delete_First");
      Register_Routine (T, Forbid_Minimal_Complete'Access, "Forbid_Minimal_Complete");
      Register_Routine (T, Matching_Begin_Parse_All_Conflicts'Access, "Matching_Begin_Parse_All_Conflicts");
      Register_Routine (T, Check_Multiple_Delete_For_Insert'Access, "Check_Multiple_Delete_For_Insert");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_mckenzie_recover.adb " & WisiToken.BNF.Generate_Algorithm_Image (T.Alg).all);
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      case T.Alg is
      when WisiToken.BNF.LALR              =>
         Ada_Lite_LALR_Main.Create_Parser
           (Parser,
            Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
            Language_Matching_Begin_Tokens =>
              WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
            Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
            Trace                          => Trace'Access,
            User_Data                      => User_Data'Access);

      when WisiToken.BNF.LR1               =>
         Ada_Lite_LR1_T1_Main.Create_Parser
           (Parser,
            Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
            Language_Matching_Begin_Tokens =>
              WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
            Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
            Trace                          => Trace'Access,
            User_Data                      => User_Data'Access,
            Text_Rep_File_Name             => "ada_lite_lr1_t1_re2c_parse_table.txt");
      end case;

      Orig_Params := Parser.Table.McKenzie_Param;

      Orig_End_Name_Optional := End_Name_Optional;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run after all tests in register; ie, after LALR, before LR1
      End_Name_Optional := Orig_End_Name_Optional;
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  Run before each test
      End_Name_Optional := Orig_End_Name_Optional;

      Parser.Table.McKenzie_Param := Orig_Params;

      WisiToken.Parse.LR.McKenzie_Recover.Force_Full_Explore := T.Force_Full_Explore;
      WisiToken.Parse.LR.McKenzie_Recover.Force_High_Cost_Solutions := T.Force_High_Cost_Solutions;

      Parser.Post_Recover := null;
   end Set_Up;

end Test_McKenzie_Recover;
--  Local Variables:
--  ada-case-strict: nil
--  End:
