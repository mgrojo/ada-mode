--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Containers;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LALR_Main;
with WisiToken.AUnit;
with WisiToken.Parse.LR.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Syntax_Trees;
package body Test_McKenzie_Recover is
   use Ada_Lite_LALR_Main; use Ada_Lite_Actions;
   use WisiToken.Parse.LR.Config_Op_Arrays;
   use all type WisiToken.Parse.LR.Config_Op_Label;
   use all type WisiToken.Semantic_Checks.Check_Status_Label;

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser;

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
   is
      use AUnit.Checks;
   begin
      Action_Count := (others => 0);

      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Lexer.Reset_With_String (Text);

      if not Multiple_Tasks then
         Parser.Table.McKenzie_Param.Task_Count := 1;
      end if;

      Parser.Parse;
      Parser.Execute_Actions;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", True, Expect_Exception);
      if Expect_Exception then
         raise;
      end if;
   end Parse_Text;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   procedure Check_Recover
     (Label                   : in String                                       := "";
      Errors_Length           : in Ada.Containers.Count_Type;
      Checking_Error          : in Ada.Containers.Count_Type                    := 1;
      Error_Token_ID          : in WisiToken.Token_ID;
      Error_Token_Byte_Region : in WisiToken.Buffer_Region                      := WisiToken.Null_Buffer_Region;
      Ops                     : in WisiToken.Parse.LR.Config_Op_Arrays.Vector   :=
        WisiToken.Parse.LR.Config_Op_Arrays.Empty_Vector;
      Ops_Race_Condition      : in Boolean                                      := False;
      Enqueue_Low             : in Integer                                      := 0;
      Enqueue_High            : in Integer                                      := Integer'Last;
      Check_Low               : in Integer                                      := 0;
      Check_High              : in Integer                                      := Integer'Last;
      Cost                    : in Integer;
      Expecting               : in WisiToken.Token_ID_Set                       := Empty_Token_ID_Set;
      Code                    : in WisiToken.Semantic_Checks.Check_Status_Label := WisiToken.Semantic_Checks.Ok)
   is
      use AUnit.Checks;
      use WisiToken.AUnit;
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
      Check (Label_I & ".errors.length", Parser_State.Errors.Length, Errors_Length);

      for I in 2 .. Checking_Error loop
         WisiToken.Parse.LR.Parse_Error_Lists.Next (Cursor);
      end loop;

      declare
         Error : WisiToken.Parse.LR.Parse_Error renames WisiToken.Parse.LR.Parse_Error_Lists.Element (Cursor);
      begin
         if Expecting /= Empty_Token_ID_Set then
            Check (Label_I & "expecting", Error.Expecting, Expecting);
         end if;

         if Code = Ok then
            --  Expecting an Action error
            Check (Label_I & ".label", Error.Label, Action);
            declare
               Token : WisiToken.Recover_Token renames Parser_State.Tree.Recover_Token (Error.Error_Token);
            begin
               Check (Label_I & ".error_token.id", Token.ID, Error_Token_ID);
               if Error_Token_ID /= +Wisi_EOI_ID then
                  --  EOF byte_region is unreliable
                  Check (Label_I & ".error_token.byte_region", Token.Byte_Region, Error_Token_Byte_Region);
               end if;
            end;
         else
            --  Expecting a Check error
            Check (Label_I & ".label", Error.Label, Check);
            Check (Label_I & ".code", Error.Check_Status.Label, Code);
            if Error.Check_Status.End_Name.Byte_Region = WisiToken.Null_Buffer_Region then
               --  End_Name is empty; check begin_name
               Check (Label_I & ".begin_name.id", Error.Check_Status.Begin_Name.ID, Error_Token_ID);
               Check (Label_I & ".begin_name.byte_region", Error.Check_Status.Begin_Name.Byte_Region,
                      Error_Token_Byte_Region);
            else
               Check (Label_I & ".end_name.id", Error.Check_Status.End_Name.ID, Error_Token_ID);
               Check (Label_I & ".end_name.byte_region",
                      Error.Check_Status.End_Name.Byte_Region,
                      Error_Token_Byte_Region);
            end if;
         end if;

         if not Ops_Race_Condition then
            Check (Label_I & ".recover.ops", Error.Recover.Ops, Ops);
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
         --  Enqueue_Count and Check_Count in Prepend_Copy just for this check.
         if Enqueue_Low = 0 and Enqueue_High = Integer'Last then
            --  Not checking enqueue or check counts
            null;
         elsif Enqueue_High = Integer'Last then
            Check (Label_I & ".enqueue_low", Parser_State.Recover.Enqueue_Count, Enqueue_Low);
            Check (Label_I & ".check_low", Parser_State.Recover.Check_Count, Check_Low);
         else
            Check_Range (Label_I & ".enqueue_high", Parser_State.Recover.Enqueue_Count, Enqueue_Low, Enqueue_High);
            Check_Range (Label_I & ".check_high", Parser_State.Recover.Check_Count, Check_Low, Check_High);
         end if;
         Check (Label_I & ".cost", Error.Recover.Cost, Cost);
      end;
   end Check_Recover;

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      File_Name : constant String := "../Test/bnf/ada_lite.input";
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
      --  The test is that there are no exceptions and only recovered errors.

      Parse_Text ("");
      Check_Recover
        ("1",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOF_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Enqueue_Low             => 98,
         Check_Low               => 20,
         Cost                    => 4);

      Parse_Text ("   ");
      Check_Recover
        ("2",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOF_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Enqueue_Low             => 98,
         Check_Low               => 20,
         Cost                    => 4);

      Parse_Text ("--  a comment");
      Check_Recover
        ("3",
         Errors_Length           => 1,
         Error_Token_ID          => Descriptor.EOF_ID,
         Error_Token_Byte_Region => (1, 0),
         Ops                     => +(Insert, +IDENTIFIER_ID, 1) & (Insert, +SEMICOLON_ID, 1),
         Enqueue_Low             => 98,
         Check_Low               => 20,
         Cost                    => 4);
   end Empty_Comments;


   procedure Error_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;");
      --           1        |10       |20       |30       |40
      --  Missing "if" in "end if;"
      --
      --  error 1 at ';' 44, expecting 'if'. Inserts 'if', succeeds.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (44, 44),
         Ops                     => +(Insert, +IF_ID, 11),
         Enqueue_Low             => 123,
         Check_Low               => 32,
         Cost                    => 2);
   end Error_1;

   procedure Error_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end if; end Proc; ");
      --  |1       |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  1         2    3  4     5      6 7     8  9 10 11  13 14  15  16     17 18 19  21  22  23
      --                                                   12                          20

      --  Missing "begin" for Block_2.
      --
      --  Error 1 at 'Block_2' 63, expecting 'if'. It finds (push_back 'end'
      --  59, push_back sequence_of_statements_opt, delete 'end' 59) cost 1,
      --  leaving "Block_2;" as a procedure call in the else branch;
      --  succeeds.
      --
      --  The desired solution (push_back 'end' 59, push_back
      --  sequence_of_statements_opt, insert 'begin') is cost 5

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (63, 69),
         Ops                     => +(Push_Back, +END_ID, 15) & (Push_Back, +sequence_of_statements_opt_ID, 15) &
           (Delete,  +END_ID, 15),
         Enqueue_Low             => 32,
         Check_Low               => 9,
         Cost                    => 1);
   end Error_2;

   procedure Error_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; end Water; "
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
         --  1      2     3  4     5    6     7  9  10     12 13     15  16  18   19   20 22 23    25  26 27 28   29
         --                                    8      11        14         17           21     24
        );
      --  Missing "end if" at 67.
      --
      --  Enters error recovery on ';' 84 expecting 'IF'. Inserts 'IF ;
      --  END', succeeds. Demonstrates the need for Check_Limit = 3; with
      --  Check_Limit = 2, only inserts 'if ;', and errors again on 'loop'
      --  90.

      --  Confirm that the subprogram_body was parsed:
      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (84, 84),
         Ops                     => +(Push_Back, +END_ID, 22) & (Insert, +END_ID, 22) & (Insert, +IF_ID, 22) &
           (Insert, +SEMICOLON_ID, 22) & (Fast_Forward,  22),
         Enqueue_Low             => 5,
         Check_Low               => 2,
         Cost                    => 0,
         Expecting               => WisiToken.To_Token_ID_Set
           (Descriptor.First_Terminal,
            Descriptor.Last_Terminal,
            (1                   => +IF_ID)));
   end Error_3;

   procedure Error_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
   begin
      Parse_Text ("when in the course of human events", Expect_Exception => True);
      --  Bogus syntax; test no exceptions due to empty stack etc.

      Assert (False, "1.exception: did not get Syntax_Error");

   exception
   when WisiToken.Syntax_Error =>
      --  One error message from the syntax error, one from recover fail
      Check ("error.length", Parser.Parsers.First.State_Ref.Errors.Length, 2);
   end Error_4;

   procedure Check_Accept (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text ("procedure Debug is begin A;");
      --                    |10       |20
      --           1         2     3  4     5 6 = SEMICOLON, 7 = Wisi_EOI
      --  Missing "end;"
      --
      --  Inserts 'end ;', continues to EOF, succeeds
      --  Test hitting EOF and Accept_It in error recovery
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +Wisi_EOI_ID,
         Error_Token_Byte_Region => (27, 27),
         Ops                     => +(Insert, +END_ID, 7) & (Insert, +SEMICOLON_ID, 7),
         Enqueue_Low             => 33,
         Check_Low               => 5,
         Cost                    => 2);
   end Check_Accept;

   procedure Extra_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Debug is begin procedure Put_Top_10 is begin end Put_Top_10; begin end Debug; ");
         --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Added 'begin' 72, intending to delete 'begin' 20
      --
      --  Error recovery is entered and exited with parallel parsers active;
      --  one parsing a subprogram_body, the other a generic_instantiation
      --  (which will fail eventually). The syntax trees are not flushed.
      --
      --  While checking the prefered solution, there are conflicts that
      --  must be handled.
      --
      --  The desired solution is (push_back 'begin' 20, push_back
      --  declarative_part_opt, delete 'begin'), leaving 'is' 17 on the
      --  parse stack, with cost 1. That allows the subprogram_body parser
      --  to continue to EOF.
      --
      --  For the subprogram_body parser (1), error recovery is entered at
      --  'procedure' 26, and finds the desired solution.
      --
      --  For the generic_instantiation parser (0), error recovery is
      --  entered at 'begin 20'. It finds (push_back 'is' 17, insert ';',
      --  delete 'is' 17, delete 'begin' 20), cost 8, turning this into a
      --  sequence of declarations. That fails eventually (after spawning
      --  yet more parsers).

      --  Confirm that both subprogram_bodies were parsed:
      Check ("action_count", Action_Count (+subprogram_body_ID), 2);

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +PROCEDURE_ID,
         Error_Token_Byte_Region => (26, 34),
         Ops                     =>
           +(Push_Back, +BEGIN_ID, 4) &
             (Push_Back, +declarative_part_opt_ID, 4) &
             (Delete, +BEGIN_ID, 4),
         Enqueue_Low             => 30,
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

      --  Syntax error (extra 'end' 22) while two parsers are sorting out a conflict
      --
      --  parser 1 for subprogram_body (should succeed): delete 'end' 22, cost 1.
      --  Continue to EOF, succeed.
      --
      --  parser 0 for generic_instantiation (should fail): finds: insert
      --  'new', delete 'end begin end' cost 6, succeed.

      --  Thus there is an ambiguous parse. But since there was an error,
      --  one parser is chosen to succeed.
      --
      --  This is an example of error recovery defeating conflict
      --  resolution. In real programs it should not happen often; the
      --  incorrect parser will not find a viable error resolution. However,
      --  that means it will be slow, since error resolution only fails by
      --  hitting the cost limit after trying hundreds of possible
      --  solutions.

      Check_Recover
        (Label                   => "1",
         Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (22, 24),
         Ops                     => +(Delete, +END_ID, 4),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 1);

      --  Symmetric case where generic_instantiation is desired

      Parser.Table.McKenzie_Param.Cost_Limit := 6;

      Parse_Text
        ("procedure Check_2 is end new Check_2;");
      --           |10       |20       |30       |40       |50       |60       |70       |80

      --  Syntax error (extra 'end' 22) while two parsers are sorting out a
      --  conflict.
      --
      --  parser 1 for subprogram_body (should fail): hits cost limit, fails.
      --
      --  parser 0 for generic_instantiation (should succeed):
      --  finds: delete 'end', cost 1. Continue to eof, accept
      --
      --  This is an example of adjusting the cost limit to allow conflict
      --  resolution.

      Check_Recover
        (Label                   => "2",
         Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (22, 24),
         Ops                     => +(Delete, +END_ID, 4),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 1);
   end Conflict_1;

   procedure Conflict_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
         Enqueue_Low             => 5,
         Check_Low               => 3,
         Cost                    => 1);
   end Conflict_2;

   procedure Missing_Return (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
         Enqueue_Low             => 19,
         Check_Low               => 8,
         Cost                    => 6);
   end Missing_Return;

   procedure Loop_Bounds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
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
      --  The desired solution is '(insert, "..") (delete "To")' cost 8.
      --
      --  With no cost for fast_forward, recover finds '(push_back&delete,
      --  "1") (fast_forward "To") (insert, "loop") (fast_forward
      --  "Result_Length") (insert ";")' cost 7. That encounters another
      --  error at 'end' 67.
      --
      --  This is the motivation for a cost for fast_forward; with that, it
      --  finds two cost 8 solutions; the desired one, and
      --  '(push_back&delete, "1") (delete "To")', which treats
      --  "Result_Length" as a subtype.
      --
      --  Both solutions continue to EOF; then the one with the longer
      --  recover ops is terminated.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (35, 36),
         Ops                     => +(Insert, +DOT_DOT_ID, 9) & (Delete, +IDENTIFIER_ID, 9),
         Enqueue_Low             => 509,
         Check_Low               => 79,
         Cost                    => 8);
   end Loop_Bounds;

   procedure Pattern_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  We used to use a generic "pattern" to find a solution to these quickly, now
      --  the pattern is in Language_Fixes.
      Parser.Table.McKenzie_Param.Cost_Limit := 5;

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
         Ops                     => +(Push_Back, +END_ID, 13) & (Insert, +END_ID, 13) & (Insert, +CASE_ID, 13) &
           (Insert, +SEMICOLON_ID, 13) & (Fast_Forward, 13),
         Enqueue_Low             => 5,
         Check_Low               => 2,
         Cost                    => 0);

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

      Check_Recover
        ("2",
         Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (59, 69),
         Ops                     => +(Push_Back, +END_ID, 13) & (Insert, +END_ID, 13) & (Insert, +CASE_ID, 13) &
           (Insert, +SEMICOLON_ID, 13) & (Fast_Forward, 13),
         Enqueue_Low             => 5,
         Check_Low               => 2,
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
         Ops                     => +(Push_Back, +END_ID, 10)  & (Insert, +END_ID, 10) & (Insert, +IF_ID, 10) &
           (Insert, +SEMICOLON_ID, 10) & (Fast_Forward, 10),
         Enqueue_Low             => 5,
         Check_Low               => 2,
         Cost                    => 0);

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
         Ops                     => +(Push_Back, +END_ID, 12) & (Insert, +END_ID, 12) & (Insert, +LOOP_ID, 12) &
           (Insert, +SEMICOLON_ID, 12) & (Fast_Forward, 12),
         Enqueue_Low             => 5,
         Check_Low               => 2,
         Cost                    => 0);
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
      --  parser 0 continues to EOF, succeeds.
      --
      --  parser 1 continues to ( 77, spawns another parser. parser 1
      --  assumes 'primary', parser 2 assumes 'subtype_indication'.
      --
      --  parser 1 continues to EOF, becomes a zombie, is terminated.
      --  parser 2 continues to EOF, becomes a zombie, is terminated.
      --
      --  The three parsers have different error tokens; make sure the correct
      --  one (from the successful parser) is reported.
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (23, 25),
         Ops                     => +(Insert, +NEW_ID, 4),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 3);
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
      --  inserts IDENTIFIER; the other fails.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +AND_ID,
         Error_Token_Byte_Region => (28, 30),
         Ops                     => +(Insert, +IDENTIFIER_ID, 6),
         Enqueue_Low             => 24,
         Check_Low               => 3,
         Cost                    => 3);
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
      --  Enters error recovery at 'if' 76, with two parsers active; one for
      --  subprogram_body, the other for subprogram_body_stub.
      --
      --  The subprogram_body parser pushes back and deletes 'exception,
      --  sequence_of_statement_opt' cost 4 (since
      --  sequence_of_statements_opt is empty), and continues to EOF.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +IF_ID,
         Error_Token_Byte_Region => (76, 77),
         Ops                     =>
           +(Push_Back, +EXCEPTION_ID, 8) & (Push_Back, +sequence_of_statements_opt_ID, 8) &
             (Delete, +EXCEPTION_ID, 8),
         Enqueue_Low             => 43,
         Check_Low               => 18,
         Cost                    => 4);
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
      --  Enters error recovery at Wisi_EOF, inserts 'is end;'
      --
      --  During resume, a second parser is spawned on 'is', and errors on
      --  'end'; the parser does not become a zombie, but is terminated
      --  immediately. The first parser continues thru EOF.

      Check_Recover
        (Errors_Length  => 1,
         Error_Token_ID => +Wisi_EOI_ID,
         Ops            => +(Insert, +IS_ID, 6) & (Insert, +END_ID, 6) & (Insert, +SEMICOLON_ID, 6),
         Enqueue_Low    => 42,
         Check_Low      => 10,
         Cost           => 5);
   end Zombie_In_Resume;

   procedure Push_Back_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that push_back error recovery works.

      Parse_Text
        ("procedure Remove is begin loop A := B; loop; end Remove;");
         --        |10       |20       |30       |40       |50
         --  1      2      3  4     5    6 7  8  10  11    13    14
         --                                    9       12

      --  Typed 'loop;' instead of 'end loop;'
      --
      --  Error at ';' 44. Desired solution is: (push_back 'loop')(insert 'end')
      --  But Language_Fixes finds a cheaper, more complex one

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (44, 44),
         Ops                     => +(Insert, +END_ID, 11) & (Insert, +LOOP_ID, 11) & (Fast_Forward, 13) &
           (Push_Back, +END_ID, 12) & (Insert, +END_ID, 12) & (Insert, +LOOP_ID, 12) & (Insert, +SEMICOLON_ID, 12) &
           (Fast_Forward, 12),
         Enqueue_Low             => 210,
         Check_Low               => 35,
         Cost                    => 4);
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
         --        |10       |20       |30         |40    |45                 |50     |52

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
           (Fast_Forward,  14),
         Enqueue_Low             => 34,
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
      --  that, other than parsing to EOF.
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
      --  recover doesn't do; it's equivalent to 'ignore error'.
      --
      --  We made 'ignore error' expensive, so recover finds a cheaper (due
      --  to Language_Fixes) equivalent solution that involves inserting
      --  'begin end ;'.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 15) &
           (Push_Back, +name_opt_ID, 15) & (Push_Back, +END_ID, 14) &
           (Push_Back, +handled_sequence_of_statements_ID, 9) & (Insert, +BEGIN_ID, 9) &
           (Fast_Forward,  19) & (Push_Back, +SEMICOLON_ID, 18) & (Push_Back, +name_opt_ID, 17) &
           (Push_Back, +END_ID, 16) & (Insert, +END_ID, 16) & (Insert, +SEMICOLON_ID, 16) & (Fast_Forward,  16),
         Enqueue_Low             => 4,
         Check_Low               => 4,
         Cost                    => 2,
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
      --  parsing to EOF. So Language_Fixes returns two solutions;
      --  'ignore error', and 'push_back, insert begin'.
      --
      --  'ignore error' fails the first check, since "A := B;" is not a
      --  legal declaration.
      --
      --  'push_back, insert' is the result of recovery, and parsing
      --  succeeds to EOF.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     =>
           +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 15) & (Push_Back, +name_opt_ID, 15) &
             (Push_Back, +END_ID, 14) & (Push_Back, +handled_sequence_of_statements_ID, 9) & (Insert, +BEGIN_ID, 9) &
             (Fast_Forward, 9),
         Enqueue_Low             => 12,
         Check_Low               => 2,
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
      --  this case from that, other than parsing to EOF. So
      --  Language_Fixes returns two solutions, 'ignore error' and
      --  'push_back, delete end;'.
      --
      --  In this case, only 'push_back, delete end;' is returned from
      --  Recover; it then parses to EOF.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     =>
           +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 14) &
             (Push_Back, +name_opt_ID, 14) & (Push_Back, +END_ID, 13) &
             (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_opt_ID, 1) &
             (Delete, +END_ID, 13) & (Delete, +SEMICOLON_ID, 14),
         Enqueue_Low             => 12,
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

      --  Missing 'Remove' 56. Enters error recovery on 'end' 58 with
      --  Missing_Name_Error. See Missing_Name_0 for general discussion. See
      --  Missing_Name_2; there is no way to distinguish this case from
      --  that, other than parsing to EOF. So Language_Fixes returns
      --  two solutions; 'ignore error' and 'push_back, delete end;'.
      --
      --  In this case, 'ignore error' passes recover check, but is
      --  expensive, so recover does not return it.
      --
      --  'push_back, delete end; ' fails recover check with a
      --  Match_Name_Error at 'procedure' 65, which is fixed in a second
      --  call to Language_Fixes, and finally parses to EOF.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +subprogram_specification_ID,
         Error_Token_Byte_Region => (19, 34),
         Ops                     => +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 14) &
           (Push_Back, +name_opt_ID, 14) & (Push_Back, +END_ID, 13) &
           (Undo_Reduce, +handled_sequence_of_statements_ID, 1) & (Undo_Reduce, +sequence_of_statements_opt_ID, 1) &
           (Delete, +END_ID, 13) & (Delete, +SEMICOLON_ID, 14) & (Fast_Forward,  18) & (Push_Back, +SEMICOLON_ID, 17) &
           (Push_Back, +name_opt_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
           (Insert, +SEMICOLON_ID, 15) & (Fast_Forward,  15),
         Enqueue_Low             => 4,
         Check_Low               => 4,
         Cost                    => 2,
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

      --  Missing 'Remove' 68. Enters error recovery on 'end' 60 with
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
         Enqueue_Low             => 1,
         Check_Low               => 1,
         Cost                    => 4,
         Code                    => Missing_Name_Error);
   end Missing_Name_4;

   procedure Missing_Name_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Proc_1 is procedure Proc_2 is begin null; end; begin null; end Proc_1; ");
      --           |10       |20       |30       |40       |50       |60       |70
      --  1         2      3  4         5      6  7     8   9 10 11 12   13  14 15  16

      --  Missing 'Proc_2' 68. Enters error recovery on 'begin' 58 with
      --  Missing_Name_Error.
      --
      --  Language_Fixes enqueues (push_back, delete 'end ; ').
      --
      --  That fails at EOF with an Extra_Name_Error, and (push_back, insert
      --  'end ;') is found.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (74, 79),
         Ops                     => +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
           (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) &
           (Push_Back, +handled_sequence_of_statements_ID, 13) & (Push_Back, +BEGIN_ID, 12) &
           (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12) &
           (Fast_Forward, 12),
         Enqueue_Low             => 14,
         Check_Low               => 2,
         Cost                    => 0,
         Code                    => Extra_Name_Error);
   end Missing_Name_5;

   procedure Block_Match_Names_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (91, 100),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
             (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) &
             (Insert, +END_ID, 15) & (Insert, +SEMICOLON_ID, 15) & (Fast_Forward, 15),
         Enqueue_Low             => 15,
         Check_Low               => 2,
         Cost                    => 0,
         Code                    => Extra_Name_Error);
   end Block_Match_Names_1;

   procedure Two_Parsers_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text ("package body Debug is procedure A is end Debug;");
      --           |1       |10       |20       |30       |40

      --  Missing 'begin end A;' 35.
      --
      --  Error recovery entered at 'end' 38 with two parsers, expecting
      --  'separate' and 'begin | declaration'.
      --
      --  One recovery config is 'insert begin', which fails the match_names
      --  check on "A"/"Debug".
      --
      --  Recovery finds two solutions for each parser:
      --
      --  1: 5, ((INSERT, BEGIN), (INSERT, END), (INSERT, SEMICOLON))
      --  1: 5, ((POP, IS), (POP, aspect_specification_opt), (INSERT, SEMICOLON))
      --  0: 5, ((INSERT, SEPARATE), (INSERT, SEMICOLON))
      --  0: 5, ((POP, IS), (INSERT, SEMICOLON))
      --
      --  All four continue to EOF, resulting in an ambiguous parse; the
      --  parser with the shortest recover ops is chosen. That's still a
      --  race condition, so we can't test the solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (38, 40),
         Ops_Race_Condition      => True,
         Enqueue_Low             => 7,
         Check_Low               => 4,
         Cost                    => 3);
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
      --  Language_Fixes for Extra_Name_Error enqueues the desired solution.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (97, 112),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 14) &
             (Push_Back, +identifier_opt_ID, 13) & (Push_Back, +END_ID, 12) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 11) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 11) & (Insert, +SEMICOLON_ID, 11) &
             (Fast_Forward, 11),
         Enqueue_Low             => 2,
         Check_Low               => 2,
         Cost                    => 0,
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
      --  First call to Language_Fixes finds case Extra_Name_Error 1;
      --  enqueues (push_back 'begin end name_opt ;'), (insert 'end ;');
      --  recover fast-forwards that.
      --
      --  Checking that fails with Match_Names_Error on 'To_Month' 69,
      --  'Process_CSV_File' 112. Language_Fixes adds the fix (undo_reduce
      --  subprogram_body), (push_back 'end "Process_CSV_File" ;'), (insert
      --  'end ;').
      --
      --  Checking that fails at 'end' 108, expecting 'begin'. Recover
      --  inserts that, succeeds.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (112, 127),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 17) &
             (Push_Back, +identifier_opt_ID, 16) & (Push_Back, +END_ID, 15) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 14) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 14) & (Insert, +SEMICOLON_ID, 14) &
             (Fast_Forward,  18) & (Push_Back, +SEMICOLON_ID, 17) &
             (Push_Back, +name_opt_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
             (Insert, +SEMICOLON_ID, 15) & (Fast_Forward, 15) & (Insert, +BEGIN_ID, 15),
         Enqueue_Low             => 11,
         Check_Low               => 7,
         Cost                    => 3,
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
             (Insert, +SEMICOLON_ID, 9) & (Fast_Forward, 9),
         Enqueue_Low             => 2,
         Check_Low               => 2,
         Cost                    => 0,
         Code                    => Extra_Name_Error);
   end Extra_Name_3;

   procedure Two_Missing_Ends (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
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
      --  case ; end ;'). With help from Language_Fixes, a similar solution
      --  is found.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (103, 108),
         Ops                     =>
           +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 23) &
             (Push_Back, +identifier_opt_ID, 22) & (Push_Back, +END_ID, 21) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 20) &
             (Push_Back, +block_label_opt_ID, 0)  & (Insert, +END_ID, 20) & (Fast_Forward, 20) &
             (Insert, +CASE_ID, 20) & (Insert, +SEMICOLON_ID, 20) & (Fast_Forward,  24) &
             (Push_Back, +SEMICOLON_ID, 23) & (Push_Back, +identifier_opt_ID, 22) & (Push_Back, +END_ID, 21) &
             (Push_Back, +handled_sequence_of_statements_ID, 0) & (Push_Back, +BEGIN_ID, 20) &
             (Push_Back, +block_label_opt_ID, 0) & (Insert, +END_ID, 20) & (Insert, +SEMICOLON_ID, 20) &
             (Fast_Forward,  20),
         Enqueue_Low             => 13,
         Check_Low               => 7,
         Cost                    => 4,
         Code                    => Extra_Name_Error);
   end Two_Missing_Ends;

   procedure Match_Selected_Component_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
             (Push_Back, +block_label_opt_ID, 12) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12) &
             (Fast_Forward, 12),
         Enqueue_Low             => 7,
         Check_Low               => 3,
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
      --  expecting ';'.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +DOT_ID,
         Error_Token_Byte_Region => (83, 83),
         Ops                     =>
           +(Push_Back, +IDENTIFIER_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
             (Insert, +SEMICOLON_ID, 15) & (Fast_Forward, 15),
         Enqueue_Low             => 5,
         Check_Low               => 2,
         Cost                    => 0);
   end Match_Selected_Component_2;

   procedure Actual_Parameter_Part_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
         Ops                     =>
           +(Insert, +RIGHT_PAREN_ID, 13) & (Insert, +THEN_ID, 13) &
             (Insert, +END_ID, 13) & (Insert, +IF_ID, 13) & (Insert, +SEMICOLON_ID, 13),
         Enqueue_Low             => 41,
         Check_Low               => 17,
         Cost                    => 5);
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
         Ops                     => +(Insert, +SEPARATE_ID, 15) & (Insert, +SEMICOLON_ID, 15) & (Delete, +BEGIN_ID, 15),
         --  We only save Enqueue_Count and Check_Count from the last error, so
         --  we can't check them for error 1.
         Cost                    => 6);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +RIGHT_PAREN_ID,
         Error_Token_Byte_Region => (124, 124),
         Ops                     => +(Insert, +IDENTIFIER_ID, 22),
         Enqueue_Low             => 4,
         Check_Low               => 2,
         Cost                    => 3);
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
         Enqueue_Low             => 25,
         Check_Low               => 4,
         Cost                    => 1);
   end String_Quote_1;

   procedure String_Quote_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
      --  the unbalanced quote at 88.
      --
      --  Recover entered at '/' 90, after the lexer error. It moves the
      --  inserted quote to just before LF 112, then inserts ';'.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SLASH_ID,
         Error_Token_Byte_Region => (90, 90),
         Ops                     => +(Push_Back, +LESS_ID, 15) & (Push_Back, +simple_expression_ID, 14) &
           (Fast_Forward, 15) & (Delete, +LESS_ID, 15) & (Delete, +SLASH_ID, 16) & (Delete, +IDENTIFIER_ID, 17) &
           (Delete, +GREATER_ID, 18) & (Delete, +LESS_ID, 19) & (Delete, +SLASH_ID, 20) & (Delete, +BODY_ID, 21) &
           (Delete, +GREATER_ID, 22) & (Delete, +LESS_ID, 23) & (Delete, +SLASH_ID, 24) & (Delete, +IDENTIFIER_ID, 25) &
           (Delete, +GREATER_ID, 26) & (Delete, +SEMICOLON_ID, 27) & (Fast_Forward, 28) & (Insert, +SEMICOLON_ID, 28),
         Enqueue_Low             => 14,
         Check_Low               => 3,
         Cost                    => 1);
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
         Enqueue_Low             => 26,
         Check_Low               => 4,
         Cost                    => 1);

   end String_Quote_3;

   procedure String_Quote_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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
         Enqueue_Low             => 47,
         Check_Low               => 3,
         Cost                    => 1);
   end String_Quote_4;

   procedure Enqueue_Limit (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test fail on Enqueue_Limit. Same input as Loop_Bounds above.
      --
      --  When Enqueue_Limit is hit, worker tasks stop dequeing configs, but
      --  any active workers will finish enqueuing new ones. There are six
      --  active workers, they typically enqueue about 10 configs per cycle.

      Parser.Table.McKenzie_Param.Enqueue_Limit := 100;

      begin
         Parse_Text
           ("procedure Foo is begin for I in 1 To Result_Length loop end loop; end Foo;",
            Multiple_Tasks   => True,
            Expect_Exception => True);
         AUnit.Assertions.Assert (False, "did not get Syntax_Error");
      exception
      when WisiToken.Syntax_Error =>
         null;
      end;

      --  One error message from the syntax error, one from recover fail
      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +IDENTIFIER_ID,
         Error_Token_Byte_Region => (35, 36),
         Ops_Race_Condition      => True,
         Enqueue_Low             => 100,
         Enqueue_High            => 167,
         Check_Low               => 14,
         Check_High              => 25,
         Cost                    => 0);
   end Enqueue_Limit;

   procedure Minimal_Complete_Full_Reduce_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test McKenzie_Recover.Explore.Insert_Minimal_Complete_Actions when
      --  it reduces, but then inserts nothing (it reduces to
      --  compilation_unit_list). It used to not try any insertions; now it
      --  goes back to try normal insertions.
      --
      --  This finds "insert begin" quickly.

      Parse_Text ("A; exception when A => null; end Debug;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +EXCEPTION_ID,
         Error_Token_Byte_Region => (4, 12),
         Ops                     => +(Insert, +BEGIN_ID, 3),
         Enqueue_Low             => 88,
         Check_Low               => 17,
         Cost                    => 3);
   end Minimal_Complete_Full_Reduce_1;

   procedure Minimal_Complete_Full_Reduce_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Minimal_Complete_Full_Reduce_1; this takes longer to
      --  find "insert if then".

      Parse_Text ("A; end if;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (4, 6),
         Ops                     => +(Insert, +IF_ID, 3) & (Insert, +THEN_ID, 3),
         Enqueue_Low             => 216,
         Check_Low               => 65,
         Cost                    => 4);
   end Minimal_Complete_Full_Reduce_2;

   procedure Minimal_Complete_Full_Reduce_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Don't use minimal_complete with nothing on stack; match the
      --  following tokens instead.

      Parse_Text (" end loop;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (2, 4),
         Ops                     => +(Insert, +LOOP_ID, 1),
         Enqueue_Low             => 30,
         Check_Low               => 5,
         Cost                    => 2);
   end Minimal_Complete_Full_Reduce_3;

   procedure Out_Of_Order_Ops (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test pushing back a fix, causing Config.Ops to be out of token_index order.

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

      --  "end;" 218 in wrong place; should be at 270. First fix inserts
      --  "end if ;", causing second error at "else"; that pushes back first
      --  fix, inserts "if then".
      --
      --  A simpler solution would be (delete 'end ;') at 218, followed by
      --  (insert 'end ;') at 270. But that's more expensive, because we did
      --  not make delete semicolon cheap.

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (221, 221),
         Ops                     => +(Push_Back, +END_ID, 42) & (Insert, +END_ID, 42) & (Insert, +IF_ID, 42) &
           (Insert, +SEMICOLON_ID, 42) & (Fast_Forward,  44) & (Push_Back, +sequence_of_statements_opt_ID, 10) &
           (Insert, +IF_ID, 10) & (Insert, +THEN_ID, 10) & (Fast_Forward,  42),
         Enqueue_Low             => 25,
         Check_Low               => 10,
         Cost                    => 1);
   end Out_Of_Order_Ops;

   procedure Error_During_Resume_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  previously got "error during resume"; stack is empty in error recover.

      Parse_Text ("end Process_Node;");

      Check_Recover
        (Errors_Length           => 1,
         Error_Token_ID          => +END_ID,
         Error_Token_Byte_Region => (1, 3),
         Ops                     => +(Delete, +END_ID, 1),
         Enqueue_Low             => 14,
         Check_Low               => 2,
         Cost                    => 1);
   end Error_During_Resume_1;

   procedure Error_During_Resume_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Previously got "error during resume"; second recover set
      --  Current_Token to deleted token 'loop' from first recover.

      Parser.Table.McKenzie_Param.Check_Limit       := 4;
      Parser.Table.McKenzie_Param.Ignore_Check_Fail := 0;     -- Allow finding desired solution.
      End_Name_Optional                             := False; -- Triggers Missing_Name_Error.

      Parse_Text ("procedure Update_First is begin end loop; end loop; end Update_First;");
      --           1         2            3  4     5   6   7 8   9   10 11 12          13

      --  Three solutions succeed to the end. Unfortunately, the "correct"
      --  one (that deleted 'loop' in the first error) has longer recover
      --  ops, so it is terminated. We are mostly verifying that "error
      --  during resume" no longer occurs.

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 1,
         Error_Token_ID          => +BEGIN_ID,
         Error_Token_Byte_Region => (27, 31),
         Ops                     => +(Push_Back, +IS_ID, 3) & (Fast_Forward,  6) & (Push_Back, +END_ID, 5) &
           (Push_Back, +handled_sequence_of_statements_ID, 5) & (Insert, +LOOP_ID, 5) & (Fast_Forward,  5),
         Cost                    => 5);

      Check_Recover
        (Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +LOOP_ID,
         Error_Token_Byte_Region => (47, 50),
         Ops                     => +(Delete, +LOOP_ID, 9) & (Fast_Forward,  11) & (Delete, +END_ID, 11),
         Enqueue_Low             => 181,
         Check_Low               => 39,
         Cost                    => 2);

   end Error_During_Resume_2;

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
      Register_Routine (T, Enqueue_Limit'Access, "Enqueue_Limit");
      Register_Routine (T, Minimal_Complete_Full_Reduce_1'Access, "Minimal_Complete_Full_Reduce_1");
      Register_Routine (T, Minimal_Complete_Full_Reduce_2'Access, "Minimal_Complete_Full_Reduce_2");
      Register_Routine (T, Minimal_Complete_Full_Reduce_3'Access, "Minimal_Complete_Full_Reduce_3");
      Register_Routine (T, Out_Of_Order_Ops'Access, "Out_Of_Order_Ops");
      Register_Routine (T, Error_During_Resume_1'Access, "Error_During_Resume_1");
      Register_Routine (T, Error_During_Resume_2'Access, "Error_During_Resume_2");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_mckenzie_recover.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      --  FIXME: add run-time choice of LR1 vs LALR
      Create_Parser
        (Parser,
         Language_Fixes                        => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Use_Minimal_Complete_Actions =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Use_Minimal_Complete_Actions'Access,
         Language_String_ID_Set                => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         Trace                                 => Trace'Access,
         User_Data                             => User_Data'Access);

      Orig_Params := Parser.Table.McKenzie_Param;

      Orig_End_Name_Optional := End_Name_Optional;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is
      use all type System.Multiprocessors.CPU_Range;
   begin
      --  Run before each test
      End_Name_Optional := Orig_End_Name_Optional;

      Parser.Table.McKenzie_Param := Orig_Params;

      if T.Task_Count /= System.Multiprocessors.CPU_Range'Last then
         Parser.Table.McKenzie_Param.Task_Count := T.Task_Count;
      end if;

      if T.Cost_Limit /= Natural'Last then
         Parser.Table.McKenzie_Param.Cost_Limit := T.Cost_Limit;
      end if;

      WisiToken.Parse.LR.McKenzie_Recover.Force_High_Cost_Solutions := False;
      WisiToken.Parse.LR.McKenzie_Recover.Force_Full_Explore := False;

      Parser.Post_Recover := null;
   end Set_Up;

end Test_McKenzie_Recover;
