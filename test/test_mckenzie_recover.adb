--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada_Lite;
with WisiToken.AUnit;
with WisiToken.Parser.LR.Parser;
with WisiToken.Token_Region.AUnit;
package body Test_McKenzie_Recover is

   Parser : WisiToken.Parser.LR.Parser.Instance;

   Orig_Cost_Limit  : Integer;

   procedure Parse_Text (Text : in String; Debug : in Integer)
   is begin
      Parser.Enable_McKenzie_Recover := True;

      WisiToken.Trace_Parse := Debug;

      Ada_Lite.Action_Count := (others => 0);

      if Debug > 0 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Lexer.Reset_With_String (Text & "   ");
      --  Trailing spaces so final token has proper region;
      --  otherwise it is wrapped to 1.

      Parser.Parse;
   end Parse_Text;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Exceptions;
      use Ada_Lite;

      File_Name : constant String := "../wisi/test/ada_lite.input";
   begin
      --  The test is that there is no exception.

      WisiToken.Trace_Parse := Test.Debug;

      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;
   exception
   when E : WisiToken.Syntax_Error =>
      Ada.Text_IO.Put_Line (File_Name & ":" & Exception_Message (E));
      AUnit.Assertions.Assert (False, "syntax error");
   end No_Error;

   procedure Error_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;", Test.Debug);
      --                1        |10       |20       |30       |40
      --  Missing "if" in "end if;"
      --
      --  This not a likely error, so the pattern rule for the likely error
      --  (missing 'end if;') matches, but then immediately fails.
      --
      --  error 1 at ';' 39, expecting 'if'. Inserts 'if', succeeds.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "exception: got syntax error exception.");
   end Error_1;

   procedure Error_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;

      use all type WisiToken.Region_Lists.Cursor;
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end if; end Proc_1; ", Test.Debug);
      --  |1       |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  Missing "begin" for Block_2, but McKenzie won't find that.
      --
      --  error 1 at 'Block_2' 63, expecting 'if'. Pops 'end' 59, inserts
      --  'else', leaving "Block_2;" as a procedure call in the else branch;
      --  succeeds.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);

      declare
         use WisiToken.Token_Region;
         Error_List : Error_Data_Lists.List renames Ada_Lite.State.Active_Error_List.Element.all;
      begin
         Check ("errors.length", Error_List.Length, 1);
      end;
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "exception: got Syntax_Error");
   end Error_2;

   procedure Error_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; end Water; ",
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
         Test.Debug);
      --  Missing "end if" at 67.
      --
      --  This used to insert lots of stuff finishing all the blocks;
      --  now it uses recover_pattern_1 for 'if'.

      declare
         use WisiToken.AUnit;
         use WisiToken.Token_Region;
         use WisiToken.Token_Region.AUnit;
         use WisiToken.Token_Region.Error_Data_Lists;
         Error_List : Error_Data_Lists.List renames Ada_Lite.State.Active_Error_List.Element.all;
         Cursor : constant Error_Data_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check
           ("1", Element (Cursor),
            (First_Terminal    => Descriptor.First_Terminal,
             Last_Terminal     => Descriptor.Last_Terminal,
             Error_Token       => (+SEMICOLON_ID, 0, 83, (84, 84), (84, 84)),
             Expecting         => To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (1 => +IF_ID)),
             Recover           => null),
            Check_Recover_Data => null);

         Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      end;
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Error_3;

   procedure Error_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("else then ", Test.Debug);
      --  Bogus syntax; test no exceptions due to empty stack etc.

      Assert (False, "1.exception: did not get Syntax_Error");
   exception
   when WisiToken.Syntax_Error =>
      Check ("error.length", State.Active_Error_List.Length, 1);
   end Error_4;

   procedure Check_Accept (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Debug is begin A; ", Test.Debug);
      --  Missing "end;"
      --
      --  Inserts 'loop', continues to EOF, inserts 'end;', succeeds
      --  Test hitting EOF and Accept_It in error recovery
      Check ("errors.length", State.Active_Error_List.Length, 1);

   exception
   when WisiToken.Syntax_Error =>
      Assert (True, "1.exception: got Syntax_Error");
   end Check_Accept;

   procedure Extra_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Debug is begin procedure Put_Top_10 is begin end Put_Top_10; begin end Debug; ",
         --        |10       |20       |30       |40       |50       |60       |70       |80
         Test.Debug);
      --  Added 'begin' 72, intending to delete 'begin' 20
      --
      --  There are no special rules to help with this.
      --
      --  Error recovery is entered and exited with parallel parsers active.
      --
      --  While checking the prefered solution, there are conflicts that
      --  must be handled.
      --
      --  The desired solution is pop 'begin 20' and reduced
      --  declarative_part_opt, leaving 'is' 17 on the parse stack. That has
      --  cost 2 due to grammar cost settings. That allows parsing to
      --  continue to EOF.

      Check ("errors.length", State.Active_Error_List.Length, 1);

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Extra_Begin;

   procedure Conflict_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      begin
         Parse_Text
           ("procedure Check_1 is end begin end Check_1;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  Syntax error (extra 'end' 22) while two parsers are sorting out a conflict
         --
         --  parser 1 for subprogram_body (should succeed): delete 'end' 22, cost 1.
         --  Continue to EOF, succeed.
         --
         --  parser 0 for generic_instantiation (should fail):
         --  finds: insert 'new', delete 'end begin end' cost 6, succeed => ambiguous parse
         --
         --  This is an example of error recovery defeating conflict
         --  resolution. In real programs it should not happen often; the
         --  incorrect parser will not find a viable error resolution. However,
         --  that means it will be slow, since error resolution only fails by
         --  hitting the cost limit after trying hundreds of possible
         --  solutions.

         Assert (False, "1 did not get exception");

      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");

      when E : WisiToken.Parse_Error =>
         declare
            use Ada.Exceptions;
            use Ada.Strings.Fixed;
            Msg : constant String := Exception_Message (E);
         begin
            Assert (0 < Index (Source => Msg, Pattern => "Ambiguous parse"), "1 unexpected exception");
         end;
      end;

      --  Symmetric case where generic_instantiation is desired
      begin

         Parser.Table.McKenzie.Cost_Limit := 6;

         Parse_Text
           ("procedure Check_2 is end new Check_2;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
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

         Check ("2 errors.length", State.Active_Error_List.Length, 1); -- error from surviving parser

      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "2 exception: got Syntax_Error");
      when WisiToken.Parse_Error =>
         Assert (False, "2 exception: got Parse_Error");
      end;
   end Conflict_1;

   procedure Conflict_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("function Find_Path return Path is begin return Result : Path (1 .. Result_Length) end Find_Path; ",
         --        |10       |20       |30       |40       |50       |60       |70       |80
         Test.Debug);
      --  Syntax error (missing ';' (and rest of extended return) at
      --  82) while two parsers are sorting out a conflict.
      --
      --  Both have pending push_token, which used to mess up the
      --  lookahead queue.
      --
      --  both insert semicolon, which leads to identical stacks.
      Check ("1 errors.length", State.Active_Error_List.Length, 1);
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1 exception: got Syntax_Error");
   end Conflict_2;

   procedure Missing_Return (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      begin
         Parse_Text
           ("procedure McKenzie_Recover is function Check (Data : McKenzie_Data) is begin end Check; begin end; ",
            --        |10       |20       |30       |40       |50       |60       |70       |80       |90
            Test.Debug);
         --  Missing 'return foo' in function spec.
         --
         --  error 1 at 'is' 72; expecting 'return'. Inserts 'return IDENTIFIER'.
         --  Spawns 1 parser in state 91: subprogram_body_stub/subprogram_body
         --  terminates 1 at 'begin' 75, shared lookahead not finished. Used to get queue empty error here.
         --  continues to eof, succeeds.

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");

      end;

   end Missing_Return;

   procedure Loop_Bounds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      begin
         Parse_Text
           ("procedure Foo is begin for I in 1 To Result_Length loop end loop; end;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  'To' should be '..'
         --
         --  error 1 at 'To' 35; expecting '..'.
         --  with Check_Token_Limit = 3, pops "1", deletes To, leaving Result_Length as subtype
         --  continues to eof, succeeds.

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");
      end;

   end Loop_Bounds;

   procedure Pattern_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parser.Table.McKenzie.Cost_Limit := 1; -- show that matching the pattern reduces cost

      --  Test 'recover_pattern_1' for CASE
      begin
         Parse_Text
           ("procedure Test_CASE_1 is begin case I is when 1 => A; end;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  Missing 'end case;'

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");
      end;

      --  Similar to Test_CASE_1, but error token is IDENTIFIER (and it could be dotted).
      --  FIXME: recover finds "insert 'case; end'"; need another pattern
      Parser.Table.McKenzie.Cost_Limit := 10; -- no pattern matching here
      begin
         Parse_Text
           ("procedure Test_CASE_2 is begin case I is when 1 => A; end Test_CASE_2;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  Missing 'end case;'
         --
         --  error 1 at ';' 56; expecting 'case'.

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");
      end;

      Parser.Table.McKenzie.Cost_Limit := 2; -- show that matching the pattern reduces enqueues

      --  Test 'recover_pattern_1' for IF
      begin
         Parse_Text
           ("procedure Test_IF is begin if A then B; end;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  Missing 'end if;'

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");
      end;

      --  Test 'recover_pattern_1' for LOOP
      begin
         Parse_Text
           ("procedure Test_LOOP is begin for I in A loop B; end;",
            --        |10       |20       |30       |40       |50       |60       |70       |80
            Test.Debug);
         --  Missing 'end loop;'

         Check ("1 errors.length", State.Active_Error_List.Length, 1);
      exception
      when WisiToken.Syntax_Error =>
         Assert (False, "1 exception: got Syntax_Error");
      end;

   end Pattern_1;

   procedure Revive_Zombie_Parser (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.Token_Region.AUnit;
   begin
      Parse_Text
        ("procedure Patterns is Ada.Containers.Indefinite_Doubly_Linked_Lists (Pattern);",
         --        |10       |20       |30       |40       |50       |60       |70       |80
         Test.Debug);
      --  A generic instantiation, but missing 'new' after 'is' 20.
      --
      --  Spawns a second parser on 'is'; one for procedure body, one for
      --  generic instantiation.
      --
      --  parser 0 for generic_instantiation errors at 'Ada' 23, expecting
      --  'new'. It inserts 'new' cost 3. Continues to EOF, succeeds.
      --
      --  parser 1 for subprogram_body parser keeps going, thinking it's the
      --  start of an object declaration; errors at '.' 26. It inserts ':
      --  IDENTIFIER' cost 7. Continues to EOF, becomes a zombie.
      --
      --  parser 0 is a zombie until parser 1 errors at '.'; both participate in
      --  error recovery, and find a solution.
      --
      --  Parser 1 becomes a zombie again at EOF, but parser 0 accepts, so
      --  no error recovery is attempted.
      --
      --  The two parsers have different error tokens; make sure the correct
      --  one (from the successful parser 0) is reported.

      Check ("1 errors.length", State.Active_Error_List.Length, 1);
      declare
         use WisiToken.Token_Region;
         use WisiToken.Token_Region.Error_Data_Lists;
         Error_List : Error_Data_Lists.List renames Ada_Lite.State.Active_Error_List.Element.all;
         Cursor : constant Error_Data_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.error_token", Element (Cursor).Error_Token, (+IDENTIFIER_ID, 0, 22, (23, 25), (23, 25)));
      end;

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1 exception: got Syntax_Error");
   end Revive_Zombie_Parser;

   procedure Error_Token_When_Parallel (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.Token_Region.AUnit;
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("procedure One is begin if  and B then C; end if; end;",
         --        |10       |20       |30       |40       |50
         Test.Debug);
      --  Missing and expression between 'if' and 'and'.
      --
      --  Spawns a second parser on 'is'; one for procedure body, one for
      --  generic instantiation. Both are still around when the error is
      --  encountered at 'and' 28. Error recovery for the procedure body
      --  inserts IDENTIFIER; the other fails.

      Check ("1 errors.length", State.Active_Error_List.Length, 1);
      declare
         use WisiToken.Token_Region;
         use WisiToken.Token_Region.Error_Data_Lists;
         Error_List : Error_Data_Lists.List renames Ada_Lite.State.Active_Error_List.Element.all;
         Cursor : constant Error_Data_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.error_token", Element (Cursor).Error_Token, (+AND_ID, 0, 27, (28, 30), (28, 30)));
      end;

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1 exception: got Syntax_Error");
   end Error_Token_When_Parallel;

   procedure If_In_Handler (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.Token_Region.AUnit;
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_Text_File is begin " &
         --        |10       |20       |30       |40       |50       |60
           "exception if then end if; end Process_Text_File; begin begin end; end Journal_To_TSV;",
         --   |67         |80       |90       |100      |110      |120      |130      |140
         Test.Debug);
      --  Mistakenly pasted 'if then end if' in exception handler 66 .. 91.
      --
      --  This used to cause a token ID mismatch in
      --  Semantic_State.Push_Current for 'begin' 121.
      --
      --  Enters error recovery at 'if' 76, with two parsers active; one for
      --  subprogram_body, the other for subprogram_body_stub.
      --
      --  The subprogram_body parser inserts 'end; begin', terminating
      --  Process_Text_File and starting the sequence_of_statements for
      --  Journal_To_TSV. The subprogram_body_stub fails error recovery.
      --
      --  The subprogram_body parser proceeds to 'begin' 115, expecting EOF.
      --  It inserts 'procedure IDENTIFIER is' and continues.

      Check ("1 errors.length", State.Active_Error_List.Length, 2);
      declare
         use WisiToken.Token_Region;
         use WisiToken.Token_Region.Error_Data_Lists;
         Error_List : Error_Data_Lists.List renames Ada_Lite.State.Active_Error_List.Element.all;
         Cursor : Error_Data_Lists.Cursor := Error_List.First;
      begin
         Check ("errors 1.error_token", Element (Cursor).Error_Token, (+IF_ID, 0, 75, (76, 77), (76, 77)));
         Next (Cursor);
         Check ("errors 2.error_token", Element (Cursor).Error_Token, (+BEGIN_ID, 0, 114, (115, 119), (115, 119)));
      end;

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1 exception: got Syntax_Error");
   end If_In_Handler;

   procedure Zombie_In_Resume (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.Token_Region.AUnit;
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("package body Ada_Mode.Loop_face",
         --        |10       |20
         Test.Debug);
      --  Just started typing a package
      --
      --  This used to raise a Programmer_Error because of a zombie parser
      --  during resume.
      --
      --  Enters error recovery at Wisi_EOF, inserts 'is end;'
      --
      --  A second parser is spawned on 'is', and errors on 'end'. Resume is
      --  still active, so the parser does not become a zombie, but is
      --  terminated immediately. The first parser continues thru EOF.

      Check ("1 errors.length", State.Active_Error_List.Length, 1);

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1 exception: got Syntax_Error");
   end Zombie_In_Resume;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_mckenzie_recover.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Error'Access, "No_Error");
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
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Ada_Lite.Create_Parser (Parser, WisiToken.LALR, Ada_Lite.State'Access);
      Orig_Cost_Limit := Parser.Table.McKenzie.Cost_Limit;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  Run before each test
      Parser.Table.McKenzie.Cost_Limit := (if T.Cost_Limit = Natural'Last then Orig_Cost_Limit else T.Cost_Limit);
   end Set_Up;

end Test_McKenzie_Recover;
