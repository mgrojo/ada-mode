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
with WisiToken.LR.Parser;
with WisiToken.LR.Parser_Lists;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees.Branched;
package body Test_McKenzie_Recover is

   Parser    : WisiToken.LR.Parser.Parser;
   User_Data : WisiToken.Syntax_Trees.User_Data_Type;

   Orig_Params : WisiToken.LR.McKenzie_Param_Type
     (First_Terminal    => Ada_Lite.Descriptor.First_Terminal,
      Last_Terminal     => Ada_Lite.Descriptor.Last_Terminal,
      First_Nonterminal => Ada_Lite.Descriptor.First_Nonterminal,
      Last_Nonterminal  => Ada_Lite.Descriptor.Last_Nonterminal);

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
            Syntax_Tree      => Parser.Parsers.First.State_Ref.Tree,
            Descriptor       => Parser.Trace.Descriptor.all);
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         WisiToken.LR.Put
           (Source_File_Name => "<string>",
            Errors           => Parser.Parsers.First.State_Ref.Errors,
            Syntax_Tree      => Parser.Parsers.First.State_Ref.Tree,
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
      --  This not a likely error, so the pattern rule for the likely error
      --  (missing 'end if;') matches, but then immediately fails.
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

      use all type WisiToken.Region_Lists.Cursor;
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end if; end Proc; ");
      --  |1       |10       |20       |30       |40       |50       |60       |70       |80       |90
      --  Missing "begin" for Block_2, but McKenzie won't find that.
      --
      --  error 1 at 'Block_2' 63, expecting 'if'. Pops 'end' 59, inserts
      --  'else', leaving "Block_2;" as a procedure call in the else branch;
      --  succeeds.

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
      --  This used to insert lots of stuff finishing all the blocks;
      --  now it uses recover_pattern_1 for 'if'.

      declare
         use WisiToken.AUnit;
         use WisiToken.LR;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.Parse_Error_Lists;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("errors.length", Error_List.Length, 1);
         Check
           ("1", Element (Cursor),
            (Label             => Action,
             First_Terminal    => Descriptor.First_Terminal,
             Last_Terminal     => Descriptor.Last_Terminal,
             Error_Token       => 73,
             Expecting         => To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (1             => +IF_ID)),
             Recover           => (others => <>)));

         Check ("1.error token.id", Tree.ID (Element (Cursor).Error_Token), +SEMICOLON_ID);
         Check
           ("1.error token.byte_region",
            Tree.Byte_Region (Element (Cursor).Error_Token),
            (84, 84));

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
      --  There are no special rules to help with this.
      --
      --  Error recovery is entered and exited with parallel parsers active.
      --
      --  While checking the prefered solution, there are conflicts that
      --  must be handled.
      --
      --  The desired solution is pop 'begin' 20 and reduced
      --  declarative_part_opt, leaving 'is' 17 on the parse stack.
      --  That has cost 1. That allows
      --  parsing to continue to EOF. Error recovery finds 3 solutions
      --  with cost 2, and the desired succeeds, because it encounters
      --  no more errors.

      if WisiToken.Trace_Parse > 0 then
         for Error of Parser.Parsers.First.State_Ref.Errors loop
            WisiToken.LR.Put
              (Source_File_Name => "<string>",
               Errors           => Parser.Parsers.First.State_Ref.Errors,
               Syntax_Tree      => Parser.Parsers.First.State_Ref.Tree,
               Descriptor       => Ada_Lite.Descriptor);
         end loop;
      end if;

      Check ("errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
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
      --  Missing 'return foo' in function spec.
      --
      --  error 1 at 'is' 72; expecting 'return'. Inserts 'return IDENTIFIER'.
      --  Spawns 1 parser in state 91: subprogram_body_stub/subprogram_body
      --  terminates 1 at 'begin' 75, shared lookahead not finished. Used to get queue empty error here.
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
      Parser.Table.McKenzie_Param.Cost_Limit := 1; -- show that matching the pattern reduces cost

      --  Test 'recover_pattern_1' for CASE
      Parse_Text
        ("procedure Test_CASE_1 is begin case I is when 1 => A; end;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  Missing 'end case;'

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      --  Similar to Test_CASE_1, but error token is IDENTIFIER (and it could be dotted).
      --  FIXME: recover finds "insert 'case; end'"; need another pattern
      Parser.Table.McKenzie_Param.Cost_Limit := 10; -- no pattern matching here
      Parse_Text
        ("procedure Test_CASE_2 is begin case I is when 1 => A; end Test_CASE_2;"
         --        |10       |20       |30       |40       |50       |60       |70       |80
        );
      --  Missing 'end case;'
      --
      --  error 1 at ';' 56; expecting 'case'.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      Parser.Table.McKenzie_Param.Cost_Limit := 2; -- show that matching the pattern reduces enqueues

      --  Test 'recover_pattern_1' for IF
      Parse_Text
        ("procedure Test_IF is begin if A then B; end;");
      --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Missing 'end if;'

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);

      --  Test 'recover_pattern_1' for LOOP
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
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("parser label", Parser_State.Label, 0);

         if WisiToken.Trace_Parse > 0 then
            for Error of Error_List loop
               WisiToken.LR.Put
                 (Source_File_Name => "<string>",
                  Errors           => Parser.Parsers.First.State_Ref.Errors,
                  Syntax_Tree      => Parser.Parsers.First.State_Ref.Tree,
                  Descriptor       => Ada_Lite.Descriptor);
            end loop;
         end if;

         Check ("errors.length", Error_List.Length, 1);

         Check ("error_token.id", Tree.ID (Element (Cursor).Error_Token), +IDENTIFIER_ID);
         Check
           ("1.error token.byte_region",
            Tree.Byte_Region (Element (Cursor).Error_Token),
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
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("error_token.id", Tree.ID (Element (Cursor).Error_Token), +AND_ID);
         Check ("error_token.byte_region", Tree.Byte_Region (Element (Cursor).Error_Token), (28, 30));
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
      --  This used to cause a token ID mismatch in
      --  Semantic_State.Push_Current for 'begin' 121.
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
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.First;
      begin
         Check ("error_token.id", Tree.ID (Element (Cursor).Error_Token), +IF_ID);
         Check ("error_token.byte_region", Tree.Byte_Region (Element (Cursor).Error_Token), (76, 77));
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

   procedure Match_Name (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      --  Test that block name matching is used to reject some solutions
      --  during error recovery.

      Parser.Table.McKenzie_Param.Check_Limit := 4;
      --  Force checking solution thru '; end Remove;'

      Parse_Text
        ("procedure Remove is begin loop A := B; loop; end Remove;");
         --        |10       |20       |30       |40       |50

      --  Typed 'loop;' instead of 'end loop;'
      --
      --  Error at ';' 44. Desired solutions are:
      --
      --  (pop loop)(insert 'end loop ')
      --
      --  or
      --
      --  (insert 'end loop ; end loop ')
      --
      --  both cost 7. With help from the Match_Name semantic check, and
      --  cost_limit 4, this finds both. The ambiguity is resolved by
      --  a duplicate parse state.
      --
      --  This used to fail recovery, so the test is there is no
      --  Syntax_Error.
   end Match_Name;

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

   procedure Pattern_End_EOF (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      Parse_Text
        ("package body P is procedure Remove is begin A := B; end; A := B; end Remove; end P; procedure Q;");
         --        |10       |20       |30       |40       |50       |60       |70       |80

      --  Excess 'end' 53 from editing.
      --
      --  First error recovery entered at ':=' 60, expecting declaration;
      --  inserts ': identifier'.
      --
      --  Second error recovery entered at 'end' 78, with semantic check
      --  Match_Names_Error for "P"/"Remove". No useful solution, so error
      --  is ignored.
      --
      --  Third error recovery entered at 'end' 78, expecting EOF or
      --  compilation_unit. In Ada_Lite, there is only one solution; insert
      --  'procedure', delete 'end'. In full Ada, there are three equal cost
      --  solutions, so the parse is ambiguous.
      --
      --  Applying Pattern_End_EOF resolves the ambiguity by deleting 'end P;'.
      --
      --  If we had a syntax tree, we could edit that to delete actual error
      --  'end', based on name matching.

      Check ("errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 3);
      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.AUnit.Fast_Token_ID_Vectors_AUnit;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
      begin
         Check ("error_token.id", Tree.ID (Element (Cursor).Error_Token), +END_ID);
         Check ("error_token.byte_region", Tree.Byte_Region (Element (Cursor).Error_Token), (78, 80));
         Check
           ("errors 3.recover.deleted",
            Element (Cursor).Recover.Deleted,
            To_Fast_Token_ID_Vector ((+END_ID, +IDENTIFIER_ID, +SEMICOLON_ID)));
      end;
   end Pattern_End_EOF;

   procedure Pattern_Block_Match_Names_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
      --  The semantic check that would fail at Find_First 90 (begin 53 "" =
      --  "Find_First") is not done, because the block_statement is not
      --  reduced.
      --
      --  Without pattern_block_mismatched_names, recover fails to find a
      --  solution.
      --
      --  In recover_init, pattern_block_mismatched_names doesn't match,
      --  because the error is not a semantic check error. The pattern does
      --  match while checking the root config; it pops 'Find_First ;',
      --  inserts '; end ;'. Then the parse completes.

      Check ("errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
      declare
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.AUnit.Fast_Token_ID_Vectors_AUnit;
         Parser_State : WisiToken.LR.Parser_Lists.Parser_State renames Parser.Parsers.First.State_Ref.Element.all;
         Error_List   : List renames Parser_State.Errors;
         Tree         : WisiToken.Syntax_Trees.Branched.Tree renames Parser_State.Tree;
         Cursor       : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
      begin
         Check ("errors 1.error_token.id", Tree.ID (Element (Cursor).Error_Token), +PROCEDURE_ID);
         Check ("errors 1.error_token.byte_region", Tree.Byte_Region (Element (Cursor).Error_Token), (102, 110));
         Check
           ("errors 1.recover.popped",
            Element (Cursor).Recover.Popped,
            To_Fast_Token_ID_Vector ((+SEMICOLON_ID, +identifier_opt_ID)));
         Check
           ("errors 1.recover.inserted",
            Element (Cursor).Recover.Inserted,
            To_Fast_Token_ID_Vector ((+SEMICOLON_ID, +END_ID, +SEMICOLON_ID)));
      end;

      --  FIXME: "procedure Name_A is begin end Name_B;" edit one? see pattern_end_eof
      --  FIXME: required name missing

   end Pattern_Block_Match_Names_1;

   procedure Pattern_Block_Match_Names_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
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

      --  Missing 'begin end A' 35.
      --
      --  Error recovery entered at 'end' 38 with two parsers, expecting
      --  either 'separate' or 'begin | declaration'.
      --
      --  One recovery config is 'insert begin', which fails the match_names
      --  check on "A"/"Debug". But the match_names pattern does not match,
      --  because we've already done an insertion.
      --
      --  Recovery finds one solution for each parser, and both continue to
      --  EOF, resulting in an ambiguous parse; one parser is chosen to
      --  succeed.

      Check ("1 errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1); -- error from surviving parser
   end Pattern_Block_Match_Names_2;

   procedure Pattern_Block_Extra_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
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

      --  Missing 'end To_Month;' 86.
      --
      --  Error recovery entered at 'begin' 115, Extra_Name_Error
      --  ""/"Process_CSV_File". Search stack finds matching name at 39,
      --  with 1 intervening 'begin'; insert 'end' before next 'begin'.
      --
      --  block 'begin end Process_CSV_File;' has already been reduced. pop
      --  that, insert 'end; begin end;'.
      --
      --  Need syntax tree to avoid throwing away a whole block.

      Check ("errors.length", Parser.Parsers.First.State_Ref.Errors.Length, 1);
      declare
         use WisiToken.Semantic_State;
         use WisiToken.LR.Parse_Error_Lists;
         use WisiToken.Semantic_Checks.AUnit;
         use WisiToken.LR.AUnit;
         use WisiToken.LR.AUnit.Fast_Token_ID_Vectors_AUnit;
         use all type WisiToken.Semantic_Checks.Error_Label;
         Error_List : List renames Parser.Parsers.First.State_Ref.Errors;
         Cursor : constant WisiToken.LR.Parse_Error_Lists.Cursor := Error_List.Last;
      begin
         Check ("errors 1.code", Element (Cursor).Code, Extra_Name_Error);
         Check
           ("errors 1.recover.popped",
            Element (Cursor).Recover.Popped,
            To_Fast_Token_ID_Vector ((1 => +block_statement_ID)));
         Check
           ("errors 1.recover.inserted",
            Element (Cursor).Recover.Inserted,
            To_Fast_Token_ID_Vector ((+END_ID, +SEMICOLON_ID, +BEGIN_ID, +END_ID, +SEMICOLON_ID)));
      end;

      --  FIXME: "procedure Name_A is begin end Name_B;" edit one? see pattern_end_eof
      --  FIXME: required name missing

   end Pattern_Block_Extra_Name_1;

   procedure Abandon_Pattern (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Checks;
      use Ada_Lite;
      use WisiToken.AUnit;
   begin
      --  FIXME: debugging; really slow, need 17 to pass
      Parser.Table.McKenzie_Param.Cost_Limit := 8;

      Parse_Text
        ("package body Pack_1 is procedure Proc_1 is procedure Proc_A is begin case B is when 1 => a;" &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90
           " begin end Proc_1; end Pack_1;");
      --    |92     |100      |110
      --
      --  Missing 'end case; end Proc_A' 91.
      --
      --  Error recovery 1 entered at 'end' 111, with extra_name_error from
      --  the preceding block (no label on preceding 'begin').
      --  extra_name_error pattern matches, replacing 'begin end Proc_1;'
      --  with 'end; begin end;'. That fails on the first virtual ';',
      --  expecting 'end case;'. In a previous version, that config was
      --  continued incorrectly, causing the parse to fail.
      --
      --  The desired solution is 'pop block_statement', 'insert end case;
      --  end <proc_a>; end <proc_1>;' cost (+ 8 1 3 1 1 1 1 1) => 17
      --
      --  Even better; pop block_statement terminals from syntax tree to
      --  local_lookahead, insert 'end case ;', reparse block_statement.

   end Abandon_Pattern;

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
      Register_Routine (T, Match_Name'Access, "Match_Name");
      Register_Routine (T, Missing_Quote'Access, "Missing_Quote");
      Register_Routine (T, Pattern_End_EOF'Access, "Pattern_End_EOF");
      Register_Routine (T, Pattern_Block_Match_Names_1'Access, "Pattern_Block_Match_Names_1");
      Register_Routine (T, Pattern_Block_Match_Names_2'Access, "Pattern_Block_Match_Names_2");
      Register_Routine (T, Pattern_Block_Extra_Name_1'Access, "Pattern_Block_Extra_Name_1");
      Register_Routine (T, Abandon_Pattern'Access, "Abandon_Pattern");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Ada_Lite.Create_Parser (Parser, WisiToken.LALR, Ada_Lite.Trace'Access);
      Orig_Params := Parser.Table.McKenzie_Param;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  Run before each test
      Parser.Table.McKenzie_Param := Orig_Params;
      if T.Cost_Limit /= Natural'Last then
         Parser.Table.McKenzie_Param.Cost_Limit := T.Cost_Limit;
      end if;
   end Set_Up;

end Test_McKenzie_Recover;
