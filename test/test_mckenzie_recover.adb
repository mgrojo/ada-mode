--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2018 Stephen Leake.  All Rights Reserved.
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
with Ada_Lite;
with WisiToken.AUnit;
with WisiToken.LR.AUnit;
with WisiToken.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.LR.Parser;
with WisiToken.LR.Parser_Lists;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees;
package body Test_McKenzie_Recover is

   Parser    : WisiToken.LR.Parser.Parser;
   User_Data : WisiToken.Syntax_Trees.User_Data_Type;

   EOF_ID : WisiToken.Token_ID renames Ada_Lite.Descriptor.EOF_ID;

   Orig_Params : WisiToken.LR.McKenzie_Param_Type
     (First_Terminal    => Ada_Lite.Descriptor.First_Terminal,
      Last_Terminal     => Ada_Lite.Descriptor.Last_Terminal,
      First_Nonterminal => Ada_Lite.Descriptor.First_Nonterminal,
      Last_Nonterminal  => Ada_Lite.Descriptor.Last_Nonterminal);

   Orig_End_Name_Optional : Boolean;

   procedure Parse_Text
     (Text             : in String;
      Line_Count       : in WisiToken.Line_Number_Type := 1;
      Expect_Exception : in Boolean                    := False)
   is
      use AUnit.Checks;
   begin
      Ada_Lite.Action_Count := (others => 0);

      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Semantic_State.Initialize (Line_Count);
      Parser.Lexer.Reset_With_String (Text & "   ");
      --  Trailing spaces so final token has proper region;
      --  otherwise it is wrapped to 1.

      Parser.Parse;
      Parser.Execute_Actions (User_Data, Compute_Indent => False);

      if WisiToken.Trace_Parse > WisiToken.Outline then
         WisiToken.LR.Put
           (Source_File_Name => "<string>",
            Errors           => Parser.Parsers.First.State_Ref.Errors,
            Tree             => Parser.Parsers.First.State_Ref.Tree,
            Descriptor       => Parser.Trace.Descriptor.all);
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         WisiToken.LR.Put
           (Source_File_Name => "<string>",
            Errors           => Parser.Parsers.First.State_Ref.Errors,
            Tree             => Parser.Parsers.First.State_Ref.Tree,
            Descriptor       => Parser.Trace.Descriptor.all);
      end if;

      Check ("exception", True, Expect_Exception);
      if Expect_Exception then
         raise;
      end if;
   end Parse_Text;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;

      File_Name : constant String := "../wisi/test/ada_lite.input";
   begin
      --  The test is that there is no exception.

      Parser.Semantic_State.Initialize (Line_Count => 49);
      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;
   end No_Error;

   procedure Error_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;");
      --           1        |10       |20       |30       |40
      --  Missing "if" in "end if;"
      --
      --  error 1 at ';' 39, expecting 'if'. Inserts 'if', succeeds.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
   end Error_1;

   procedure Error_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end if; end Proc; ");
      --  |1       |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  Missing "begin" for Block_2.
      --
      --  Error 1 at 'Block_2' 63, expecting 'if'. It finds (push_back 'end'
      --  59, push_back sequence_of_statements_opt, delete 'end' 59) cost 1,
      --  leaving "Block_2;" as a procedure call in the else branch;
      --  succeeds.
      --
      --  The desired solution (push_back 'end' 59, push_back
      --  sequence_of_statements_opt, insert 'begin') is cost 5
      --
      --  IMPROVEME: try to recognize 'end Block_2;' as a possible named
      --  block; insert begin before end.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);

      declare
         use WisiToken.LR;
         Error_List : Parse_Error_Lists.List renames Parser.Parsers.First.State_Ref.Errors;
      begin
         Check ("errors.length", Error_List.Length, 1);
      end;
   end Error_2;

   procedure Error_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; end Water; "
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
         );
      --  Missing "end if" at 67.
      --
      --  Enters error recovery on ';' 84 expecting 'IF'. Inserts 'IF ;
      --  END', succeeds. Demonstrates the need for Check_Limit = 3; with
      --  Check_Limit = 2, only inserts 'if ;', and errors again on 'loop'
      --  90.

      declare
         use WisiToken.AUnit;
         use WisiToken.LR;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Parse_Error_Lists;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.length", Error_List.Length, 1);

         Check ("1.error token.id", Tree.ID (Element (Cursor).Error_Token), +SEMICOLON_ID);
         Check
           ("1.error token.byte_region",
            Tree.Recover_Token (Element (Cursor).Error_Token).Byte_Region,
            (84, 84));

         Check
           ("1.expecting", Element (Cursor).Expecting, To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
               (1 => +IF_ID)));

         --  Confirm that the subprogram_body was parsed:
         Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      end;
   end Error_3;

   procedure Error_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("else then ", Expect_Exception => True);
      --  Bogus syntax; test no exceptions due to empty stack etc.

      Assert (False, "1.exception: did not get Syntax_Error");

   exception
   when WisiToken.Syntax_Error =>
      Check ("error.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Error_4;

   procedure Check_Accept (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Debug is begin A; ");
      --           1         2     3  4     5 6 = SEMICOLON, 7 = Wisi_EOI
      --  Missing "end;"
      --
      --  Inserts 'end ;', continues to EOF, succeeds
      --  Test hitting EOF and Accept_It in error recovery
      Check ("errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Check_Accept;

   procedure Extra_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
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

      if WisiToken.Trace_Parse > WisiToken.Outline then
         for Error of Parser.Parsers.First.State_Ref.Errors loop
            WisiToken.LR.Put
              (Source_File_Name => "<string>",
               Errors           => Parser.Parsers.First.State_Ref.Errors,
               Tree             => Parser.Parsers.First.State_Ref.Tree,
               Descriptor       => Ada_Lite.Descriptor);
         end loop;
      end if;

      declare
         use WisiToken.AUnit;
         use WisiToken.LR;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.Config_Op_Arrays;

         Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.length", Error_List.Length, 1);

         Check ("1.error token.id", Tree.ID (Element (Cursor).Error_Token), +PROCEDURE_ID);
         Check
           ("1.error token.byte_region",
            Tree.Recover_Token (Element (Cursor).Error_Token).Byte_Region,
            (26, 34));

         Check
           ("1.recover.ops", Element (Cursor).Recover.Ops,
            +(Push_Back, +BEGIN_ID, 1) &
              (Push_Back, +declarative_part_opt_ID, 1) &
              (Delete, +BEGIN_ID, 1));

         --  Confirm that both subprogram_bodys were parsed:
         Check ("action_count", Action_Count (+subprogram_body_ID), 2);
      end;
   end Extra_Begin;

   procedure Conflict_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Check_1 is end begin end Check_1;");
      --        |10       |20       |30       |40       |50       |60       |70       |80

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

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1); -- error from surviving parser

      --  Symmetric case where generic_instantiation is desired

      Parser.Table.McKenzie_Param.Cost_Limit := 6;

      Parse_Text
        ("procedure Check_2 is end new Check_2;");
      --        |10       |20       |30       |40       |50       |60       |70       |80

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

      Check ("2 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1); -- error from surviving parser
   end Conflict_1;

   procedure Conflict_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("function Find_Path return Path is begin return Result : Path (1 .. Result_Length) end Find_Path; "
         --        |10       |20       |30       |40       |50       |60       |70       |80
         );
      --  Syntax error (missing ';' (and rest of extended return) at
      --  82) while two parsers are sorting out a conflict.
      --
      --  Both have pending push_token, which used to mess up the
      --  lookahead queue.
      --
      --  both insert semicolon, which leads to identical stacks.
      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Conflict_2;

   procedure Missing_Return (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure McKenzie_Recover is function Check (Data : McKenzie_Data) is begin end Check; begin end; "
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90
        );
      --  Missing 'return <type>' at 69.
      --
      --  Enter recover at 'is' 69; expecting 'return'. Inserts 'return IDENTIFIER'.
      --  continues to eof, succeeds.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Missing_Return;

   procedure Loop_Bounds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Foo is begin for I in 1 To Result_Length loop end loop; end;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  'To' should be '..'
      --
      --  error 1 at 'To' 35; expecting '..'.
      --  with Check_Token_Limit = 3, pops "1", deletes To, leaving Result_Length as subtype
      --  continues to eof, succeeds.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Loop_Bounds;

   procedure Pattern_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      --  We used to need a pattern to find a solution to these quickly, now
      --  it just works.
      Parser.Table.McKenzie_Param.Cost_Limit := 5;

      Parse_Text
        ("procedure Test_CASE_1 is begin case I is when 1 => A; end;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  Missing 'end case;'

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      --  Similar to Test_CASE_1, but error token is IDENTIFIER (and it could be dotted).
      Parse_Text
        ("procedure Test_CASE_2 is begin case I is when 1 => A; end Test_CASE_2;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  Missing 'end case;'
      --
      --  error 1 at ';' 56; expecting 'case'.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      Parse_Text
        ("procedure Test_IF is begin if A then B; end;");
      --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Missing 'end if;'

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      Parse_Text
        ("procedure Test_LOOP is begin for I in A loop B; end;");
      --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Missing 'end loop;'

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Pattern_1;

   procedure Revive_Zombie_Parser (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.AUnit;
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
      --  one (from the successful parser 0) is reported.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("parser label", Parser_State.Label, 0);

         if WisiToken.Trace_Parse > 0 then
            for Error of Error_List loop
               WisiToken.LR.Put
                 (Source_File_Name => "<string>",
                  Errors           => Parser.Parsers.First.State_Ref.Errors,
                  Tree             => Parser.Parsers.First.State_Ref.Tree,
                  Descriptor       => Ada_Lite.Descriptor);
            end loop;
         end if;

         Check ("errors.length", Error_List.Length, 1);

         Check ("error_token.id", Tree.ID (Element (Cursor).Error_Token), +IDENTIFIER_ID);
         Check
           ("1.error token.byte_region",
            Tree.Recover_Token (Element (Cursor).Error_Token).Byte_Region,
            (23, 25));
      end;
   end Revive_Zombie_Parser;

   procedure Error_Token_When_Parallel (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.AUnit;
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

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
      declare
         use WisiToken.Semantic_State;
         use WisiToken.LR.Parse_Error_Lists;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
         Token        : WisiToken.Recover_Token renames Tree.Recover_Token (Element (Cursor).Error_Token);
      begin
         Check ("error_token.id", Token.ID, +AND_ID);
         Check ("error_token.byte_region", Token.Byte_Region, (28, 30));
      end;
   end Error_Token_When_Parallel;

   procedure If_In_Handler (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
      use WisiToken.AUnit;
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_Text_File is begin " &
         --        |10       |20       |30       |40       |50       |60
           "exception if then end if; end Process_Text_File; begin begin end; end Journal_To_TSV;");
         --   |67         |80       |90       |100      |110      |120      |130      |140

      --  Mistakenly pasted 'if then end if' in exception handler 66 .. 91.
      --
      --  Enters error recovery at 'if' 76, with two parsers active; one for
      --  subprogram_body, the other for subprogram_body_stub.
      --
      --  The subprogram_body parser pops 'exception,
      --  sequence_of_statement_opt' cost 4 (since
      --  sequence_of_statements_opt is empty), and continues to EOF.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
      declare
         use WisiToken.LR.Parse_Error_Lists;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
         Token        : WisiToken.Recover_Token renames Tree.Recover_Token (Element (Cursor).Error_Token);
      begin
         Check ("error_token.id", Token.ID, +IF_ID);
         Check ("error_token.byte_region", Token.Byte_Region, (76, 77));
      end;
   end If_In_Handler;

   procedure Zombie_In_Resume (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      --  Test that the correct error token is reported when the error occurs
      --  during parallel parsing (a previous version got this wrong).

      Parse_Text
        ("package body Ada_Mode.Loop_face");
         --        |10       |20

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

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
   end Zombie_In_Resume;

   procedure Push_Back_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use WisiToken.AUnit;
   begin
      --  Test that push_back error recovery works.

      Parse_Text
        ("procedure Remove is begin loop A := B; loop; end Remove;");
         --        |10       |20       |30       |40       |50

      --  Typed 'loop;' instead of 'end loop;'
      --
      --  Error at ';' 44. Desired solution is:
      --
      --  (push_back loop)()(insert end) cost 3

      declare
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("error count", Error_List.Length, 1);
         Check ("error_token.id", Tree.ID (Error.Error_Token), +SEMICOLON_ID);
         Check ("recover.ops", Error.Recover.Ops, +(Push_Back, +LOOP_ID, 1) & (Insert, +END_ID, 1));
      end;
   end Push_Back_1;

   procedure Missing_Quote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      --  Test that syntax error recovery handles a missing string quote.

      Parse_Text
        ("procedure Remove is begin A := ""B""; A := ""C"" &" & ASCII.LF & "at""; " & ASCII.LF & "end Remove;", 3);
         --        |10       |20       |30         |40    |45                 |50     |52

      --  In process of splitting a string across two lines; missing open
      --  quote 'at";' 48.
      --
      --  lexer error at '"' 50. Desired solution is insert quote char
      --  before 'at', but that's not implemented (and impossible for a
      --  string with embedded spaces). Instead we insert a virtual '"' at
      --  the error point, and return a STRING_LITERAL. The lexer has
      --  skipped to LF 52, but then backtracked to ';' 51.
      --
      --  That leads to a parse error at '"' 50; missing operator. Simplest
      --  solution is to delete the STRING_LITERAL.
   end Missing_Quote;

   procedure Missing_Name_0 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

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
      --  For all four Missing_Name_* tests, Semantic_Check_Fixes returns
      --  two solutions.
      --
      --  In this case, the desired fix is 'insert "Remove" 68', which
      --  recover can't do; it's equivalent to 'ignore error'.
      --
      --  Only 'ignore error' survives checks in recover, so recover returns
      --  one solution, which continues to EOF.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Label, Check);
         Check ("error.code", Error.Check_Status.Label, Missing_Name_Error);
         Check ("errors 1.recover.ops.length", Error.Recover.Ops.Length, 0);
      end;
   end Missing_Name_0;

   procedure Missing_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; exception end; A := B; end Remove; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  1       2    3 4  5         6      7  8     9 10 11 13        14 15  17 18 20  21    22    24 26        27
      --                                                    12               16    19            23   25           28

      --  Missing 'begin' 45. Enters error recovery at A 68 with
      --  Missing_Name_Error. See Missing_Name_0 for general discussion. See
      --  Missing_Name_0; there is no way to distinguish the two, other than
      --  parsing to EOF. So Semantic_Check_Fixes returns two solutions;
      --  'ignore error', and 'push_back, insert begin'.
      --
      --  'ignore error' fails the first check, since "A := B;" is not a
      --  legal declaration.
      --
      --  'push_back, insert' is the result of recovery, and parsing
      --  succeeds to EOF.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Label, Check);
         Check ("error.code", Error.Check_Status.Label, Missing_Name_Error);
         Check
           ("errors 1.recover.ops", Error.Recover.Ops,
            +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 15) & (Push_Back, +name_opt_ID, 16) &
              (Push_Back, +END_ID, 14) & (Push_Back, +handled_sequence_of_statements_ID, 9) & (Insert, +BEGIN_ID, 9) &
              (Fast_Forward, EOF_ID));
      end;
   end Missing_Name_1;

   procedure Missing_Name_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; end; A := B; end Remove; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90

      --  Excess 'end' 53, from editing. Error recovery entered at A 58,
      --  with Missing_Name_Error. See Missing_Name_0 for general
      --  discussion. See Missing_Name_3; there is no way to distinguish
      --  this case from that, other than parsing to EOF. So
      --  Semantic_Check_Fixes returns two solutions, 'ignore error' and
      --  'push_back, delete end;'.
      --
      --  In this case, only 'push_back, delete end;' is returned from
      --  Recover; it then parses to EOF.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Label, Check);
         Check ("error.code", Error.Check_Status.Label, Missing_Name_Error);
         Check
           ("errors 1.recover.ops", Error.Recover.Ops,
            +(Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 14) &
              (Push_Back, +name_opt_ID, 15) & (Push_Back, +END_ID, 13) &
              (Undo_Reduce, +handled_sequence_of_statements_ID, 3) & (Undo_Reduce, +sequence_of_statements_opt_ID, 1) &
              (Delete, +END_ID, 1) & (Delete, +SEMICOLON_ID, 14));
      end;
   end Missing_Name_2;

   procedure Missing_Name_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is procedure Remove is begin A := B; end; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70

      --  Missing 'Remove' 56. Enters error recovery on 'end' 58 with
      --  Missing_Name_Error. See Missing_Name_0 for general discussion. See
      --  Missing_Name_2; there is no way to distinguish this case from
      --  that, other than parsing to EOF. So Semantic_Check_Fixes returns
      --  two solutions; 'ignore error' and 'push_back, delete end;'.
      --
      --  In this case, 'ignore error' passes recover check, so recover
      --  returns it. 'push_back, delete end; ' fails recover check with a
      --  Match_Name_Error at 'procedure' 65, which is fixed in a second
      --  call to Language_Fixes; both solutions are returned from recover.
      --
      --  One is eliminated due to duplicate state; we eliminate the one
      --  with the longer Recover.Ops list.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Check_Status.Label, Missing_Name_Error);
         Check ("errors 1.recover.ops.Length", Error.Recover.Ops.Length, 0);
      end;
   end Missing_Name_3;

   procedure Missing_Name_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("package body P is package body Remove is A : Integer; end; end P; procedure Q;");
      --           |10       |20       |30       |40       |50       |60       |70

      --  Missing 'Remove' 68. Enters error recovery on 'end' 60 with
      --  Missing_Name_Error.
      --
      --  In this case, 'ignore error' is the only solution returned by
      --  Semantic_Error_Fixes. The check immediately succeeds, and that is
      --  the result from recover.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Check_Status.Label, Missing_Name_Error);
         Check ("errors 1.recover.ops.Length", Error.Recover.Ops.Length, 0);
      end;
   end Missing_Name_4;

   procedure Missing_Name_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

      Parse_Text
        ("procedure Proc_1 is procedure Proc_2 is begin null; end; begin null; end Proc_1; ");
      --           |10       |20       |30       |40       |50       |60       |70

      --  Missing 'Proc_2' 68. Enters error recovery on 'begin' 58 with
      --  Missing_Name_Error.
      --
      --  Semantic_Error_Fixes returns both 'ignore error' and (push_back,
      --  delete 'end ; '). Both pass checks, and there are two results from
      --  recover.
      --
      --  This tests setting Inc_Shared_Token properly when there are
      --  multiple parsers.
      --
      --  Parser 2 gets the 'push_back' solution, which fails at EOF.
      --  Parser 3 gets the 'ignore error' solution, which succeeds.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Check_Status.Label, Missing_Name_Error);
         Check ("errors 1.recover.ops.Length", Error.Recover.Ops.Length, 0);
      end;
   end Missing_Name_5;

   procedure Block_Match_Names_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("package body Debug is procedure Find_First is begin begin Match (Middle_Initial_Pat); end Find_First;" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
           "procedure Swap_Names is begin end Swap_Names; begin end Debug; ");
      --    |102    |110

      --  Missing 'end' 87.
      --
      --  Error recovery entered at 'procedure' 102, expecting statement.
      --  The semantic check that would fail at Find_First 90 (begin 53 "",
      --  "Find_First" 91) is not done by the main parser, because the
      --  block_statement is not reduced.
      --
      --  That check does fail with Extra_Name_Error in recover. The desired
      --  fix is (Push_Back 'Find_First ;', Insert '; end ;'). The
      --  found solution is close to that.

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
         Token        : WisiToken.Recover_Token renames Tree.Recover_Token (Error.Error_Token);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("errors 1.error_token.id", Token.ID, +PROCEDURE_ID);
         Check ("errors 1.error_token.byte_region", Token.Byte_Region, (102, 110));
         Check
           ("errors 1.recover.ops", Error.Recover.Ops,
            +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 1) &
              (Push_Back, +identifier_opt_ID, 1) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 1) &
              (Insert, +SEMICOLON_ID, 1) & (Fast_Forward, EOF_ID));
      end;
   end Block_Match_Names_1;

   procedure Two_Parsers_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      --  Show that the Match_Names_Error pattern is not applied when an
      --  insert has already been done.

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
      --  0: 5, (15 : SEMICOLON)| 8|((POP, IS), (INSERT, SEMICOLON))
      --
      --  All four continue to EOF, resulting in an ambiguous parse; one
      --  parser is chosen to succeed. Since it's a race condition which is
      --  chosen, we can't test the solution.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1); -- error from surviving parser
   end Two_Parsers_1;

   procedure Extra_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is procedure To_Month is begin" &
           --      |10       |20       |30       |40       |50       |60       |70       |80
           " begin end Process_CSV_File; begin end Journal_To_TSV;");
      --    |86 |90       |100      |110      |120      |130      |140

      --  Missing 'end To_Month;' at 86.
      --
      --  Error recovery entered at 'begin' 115, with Extra_Name_Error from
      --  the preceding block ("" begin 87 .. "Process_CSV_File" ; 113).
      --  Desired solution is (push_back block_statement), (insert 'end ;')
      --
      --  Semantic_Check Extra_Name_Error enqueues the desired solution.

      declare
         use WisiToken.Semantic_State;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.Semantic_Checks.AUnit;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;

         Error_List : List renames Parser.Parsers.First.State_Ref.Errors;
         Cursor     : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error      : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error 1.code", Error.Check_Status.Label, Extra_Name_Error);
         Check
           ("error 1.recover.ops", Error.Recover.Ops,
            +(Push_Back, +block_statement_ID, 11) & (Insert, +END_ID, 11) & (Insert, +SEMICOLON_ID, 11) &
              (Fast_Forward, EOF_ID));
      end;
   end Extra_Name_1;

   procedure Extra_Name_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is procedure To_Month is" &
           --      |10       |20       |30       |40       |50       |60       |70       |80
           --   1   2              3  4         5                6  7         8        9
           " procedure A is begin begin end Process_CSV_File; begin end Journal_To_TSV;");
      --    |80       |90       |100      |110      |120      |130      |140
      --     10        11 12 13   14    15  16              17 18   19  20            21

      --  Similar to Extra_Name_1; here we are missing 'end A; begin end
      --  To_Month;' at 86.
      --
      --  Error recovery entered at 'begin' 130, with Extra_Name_Error from
      --  the preceding block ("" begin 102 .. "Process_CSV_File;" 112).
      --
      --  Desired solution is (push_back 'end name_opt ;'), (insert
      --  'end ; end ; begin ;'). The found solution is close to this.
      --
      --  First call to Semantic_Check_Fixes enqueues (push_back 'begin end
      --  name_opt ;'), (insert 'end ;'); recover fast-forwards that.
      --
      --  Checking that fails with Match_Names_Error on 'To_Month' 69,
      --  'Process_CSV_File' 112. Semantic_Check_Fixes adds the fix
      --  (Undo_Reduce subprogram_body, Push_Back 'end "Process_CSV_File"
      --  ;', Insert 'end ;').
      --
      --  Checking that fails at 'end' 108, expecting 'begin'. Recover
      --  inserts that, succeeds.

      declare
         use WisiToken.Semantic_State;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.Semantic_Checks.AUnit;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;

         Error_List : List renames Parser.Parsers.First.State_Ref.Errors;
         Cursor     : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error      : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error 1.code", Error.Check_Status.Label, Extra_Name_Error);
         Check ("error 1.recover.ops.length", Error.Recover.Ops.Length, 12);
         Check
           ("error 1.recover.ops", Error.Recover.Ops,
            +(Push_Back, +block_statement_ID, 14) & (Insert, +END_ID, 14) & (Insert, +SEMICOLON_ID, 14) &
               (Fast_Forward, EOF_ID) & (Undo_Reduce, +subprogram_body_ID, 9) & (Push_Back, +SEMICOLON_ID, 17) &
               (Push_Back, +name_opt_ID, 16) & (Push_Back, +END_ID, 15) & (Insert, +END_ID, 15) &
               (Insert, +SEMICOLON_ID, 15) & (Fast_Forward, EOF_ID) & (Insert, +BEGIN_ID, 15));
      end;
   end Extra_Name_2;

   procedure Extra_Name_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("procedure Journal_To_TSV is procedure Process_CSV_File is begin begin" &
           --      |10       |20       |30       |40       |50       |60       |70
           " end Process_CSV_File; begin end Journal_To_TSV;");
      --    |70       |80       |100      |110      |120

      --  Similar to Extra_Name_1; here we are missing 'end;' at 65.
      --  Solution is to insert 'end ;' at 70.
      --
      --  Error recovery entered at 'begin' 93, with Extra_Name_Error from
      --  the preceding block ("" begin 65 .. "Process_CSV_File;" 75).
      --
      --  Desired solution is (push_back 'end name_opt ;'), (insert 'end ;')
      --
      --  Semantic_Check Extra_Name_Error enqueues the desired solution.

      declare
         use WisiToken.Semantic_State;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.Semantic_Checks.AUnit;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;

         Error_List : List renames Parser.Parsers.First.State_Ref.Errors;
         Cursor     : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error      : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error 1.code", Error.Check_Status.Label, Extra_Name_Error);
         Check
           ("error 1.recover.ops", Error.Recover.Ops,
            +(Undo_Reduce, +block_statement_ID, 6) & (Push_Back, +SEMICOLON_ID, 11) &
              (Push_Back, +identifier_opt_ID, 10) & (Push_Back, +END_ID, 9) & (Insert, +END_ID, 1) &
              (Insert, +SEMICOLON_ID, 1) & (Fast_Forward, EOF_ID));
      end;
   end Extra_Name_3;

   procedure Two_Missing_Ends (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("package body Pack_1 is procedure Proc_1 is procedure Proc_A is begin case B is when 1 => a;" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90
           -- 1   2    3      4  5         6      7  8         9      10 11    12   13 14 15  16 17 18 19
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
      --  case ; end ;'). With help from Semantic_Check_Fixes, that solution
      --  is found with cost 3 after checking 5 configs.

      declare
         use WisiToken.Semantic_Checks.AUnit;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;

         Error_List : WisiToken.LR.Parse_Error_Lists.List renames Parser.Parsers.First.State_Ref.Errors;
         Cursor     : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error      : WisiToken.LR.Parse_Error renames WisiToken.LR.Parse_Error_Lists.Element (Cursor);
      begin
         Check ("errors 1.code", Error.Check_Status.Label, Extra_Name_Error);
         Check ("errors 1.recover.cost", Error.Recover.Cost, 3);
         Check
           ("errors 1.recover.ops", Error.Recover.Ops,
            +(Push_Back, +block_statement_ID, 20) & (Insert, +END_ID, 20) & (Insert, +CASE_ID, 20) &
              (Insert, +SEMICOLON_ID, 20) & (Fast_Forward, EOF_ID) & (Push_Back, +block_statement_ID, 20) &
              (Insert, +END_ID, 20) & (Insert, +SEMICOLON_ID, 20) & (Fast_Forward, EOF_ID));
      end;
   end Two_Missing_Ends;

   procedure Match_Selected_Component_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Ada_Lite.End_Name_Optional := False; -- Triggers Missing_Name_Error.

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
      --  Language_Fixes returns (push_back 'end IDENTIFIER', insert 'end ; ').

      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Config_Op_Arrays;
         use WisiToken.Semantic_Checks.AUnit;
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         use all type WisiToken.LR.Config_Op_Label;
         use all type WisiToken.LR.Parse_Error_Label;

         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
         Error        : WisiToken.LR.Parse_Error renames Element (Cursor);
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check ("error.label", Error.Label, Action);
         Check
           ("errors 1.recover.ops", Error.Recover.Ops,
            +(Push_Back, +IDENTIFIER_ID, 16) & (Push_Back, +END_ID, 15) &
              (Push_Back, +handled_sequence_of_statements_ID, 13) & (Push_Back, +BEGIN_ID, 12) &
              (Push_Back, +block_label_opt_ID, 12) & (Insert, +END_ID, 12) & (Insert, +SEMICOLON_ID, 12) &
              (Fast_Forward, EOF_ID));
      end;
   end Match_Selected_Component_1;

   ----------
   --  Public subprograms

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
      Register_Routine (T, Push_Back_1'Access, "Push_Back_1");
      Register_Routine (T, Missing_Quote'Access, "Missing_Quote");
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
      Ada_Lite.Create_Parser
        (Parser, WisiToken.LALR, Ada_Lite.Trace'Access,
         WisiToken.LR.McKenzie_Recover.Ada_Lite.Language_Fixes'Access);

      Orig_Params := Parser.Table.McKenzie_Param;

      Orig_End_Name_Optional := Ada_Lite.End_Name_Optional;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  Run before each test
      Ada_Lite.End_Name_Optional := Orig_End_Name_Optional;

      Parser.Table.McKenzie_Param := Orig_Params;

      if T.Cost_Limit /= Natural'Last then
         Parser.Table.McKenzie_Param.Cost_Limit := T.Cost_Limit;
      end if;
   end Set_Up;

end Test_McKenzie_Recover;
