-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephen Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
-----------------------------------------------------------------------------

with AUnit.Assertions;
with AUnit.Check;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer.AUnit;
with OpenToken.Token.Selection;
with OpenToken.Token.Sequence;
package body Test_Backtrack is

   --  The grammar, illustrating need for partial backtracking:
   --
   --   A -> B C       : sequence of selections
   --   B -> E | F     : selection of sequences
   --   C -> E | G     : sequence of sequences
   --   E -> T1 T2 T3  : sequence of terminals
   --   F -> T1 T2 T4  : sequence of terminals
   --   G -> T1 T2 T5  : sequence of terminals
   --
   --  This is a really horrible grammar, but we want OpenToken to
   --  cope with it. We assume the user is naive, and is just getting
   --  started with grammars; they want it to "just work", and will
   --  worry about optimizing things later.
   --
   --  To make things worse, we assume this is embedded in a higher
   --  grammar, so we are parsing A inactively.
   --
   --  We declare the sequence tokens with lookahead 3, since E, F, G
   --  need that. Our naive user figures that out after the first
   --  attempt using the default lookahead of 1. We leave A with
   --  lookahead 3, as a naive user will do.
   --
   --  Consider the input (... T1 T2 T5 T1 T2 T3). The ... has been
   --  parsed by the higher level, possibly inactively so some of it
   --  is in the lookahead queue.
   --
   --  Here is the parse sequence; all calls to Parse are inactive.
   --
   --  current token = T1
   --  sequence.parse (A)
   --     selection.parse (B)
   --        mark_push_back
   --        sequence.parse (E)
   --           terminal.parse (T1)
   --           success, current token = T2
   --           terminal.parse (T2)
   --           success, current token = T5
   --           terminal.parse (T3)
   --           fails
   --        push_back_to_mark, current token = T1
   --        sequence.parse (F)
   --           terminal.parse (T1)
   --           success, current token = T2
   --           terminal.parse (T2)
   --           success, current token = T5
   --           terminal.parse (T4)
   --           success
   --        success
   --     selection.parse (C)
   --        ...
   --     success
   --  success
   --
   --  The only place backtracking is needed is in the selection
   --  token, when it tries the next selection. It has no idea how
   --  many tokens might need to be pushed back, so it marks the
   --  lookahead queue when it starts, and restores to that mark
   --  before each lower-level parse.

   type Token_ID is (T0, T1, T2, T3, T4, T5, EOF, Whitespace);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID);
   package Tokenizer is new Master_Token.Analyzer;

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_ID);

   package Analyzer_AUnit is new Tokenizer.AUnit (Check);

   Syntax : constant Tokenizer.Syntax :=
     (T0    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T0")),
      T1    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T1")),
      T2    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T2")),
      T3    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T3")),
      T4    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T4")),
      T5    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T5")),
      EOF         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                      (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  We are only interested in testing the backtracking, so we don't
   --  define any Build actions.

   use OpenToken.Token;
   use type Sequence.Instance;
   use type Selection.Instance;

   --  Terminal tokens
   T0_Token : constant Master_Token.Handle := Syntax (T0).Token_Handle;
   T1_Token : constant Master_Token.Handle := Syntax (T1).Token_Handle;
   T2_Token : constant Master_Token.Handle := Syntax (T2).Token_Handle;
   T3_Token : constant Master_Token.Handle := Syntax (T3).Token_Handle;
   T4_Token : constant Master_Token.Handle := Syntax (T4).Token_Handle;
   T5_Token : constant Master_Token.Handle := Syntax (T5).Token_Handle;

   --  Nonterminal tokens
   G : constant Sequence.Handle  := Sequence.New_Instance (T1_Token & T2_Token & T5_Token, "G", 3);
   F : constant Sequence.Handle  := Sequence.New_Instance (T1_Token & T2_Token & T4_Token, "F", 3);
   E : constant Sequence.Handle  := Sequence.New_Instance (T1_Token & T2_Token & T3_Token, "E", 3);
   C : constant Selection.Handle := Selection.New_Instance (E or G, "C");
   B : constant Selection.Handle := Selection.New_Instance (E or F, "B");
   A : constant Sequence.Handle  := Sequence.New_Instance (B & C, "A", 3);

   ----------
   --  Test procedures

   procedure Enumerated_Lookahead (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Check;

      Text : constant String := "T0 T1 T2";
   begin
      --  Verify that Enumerated.Parse leaves the analyzer in the proper
      --  state with Actively => False.
      --
      --  This parser never does backtracking, so this is mostly
      --  confirming the analyzer push back mechanism. But it's good
      --  to do the simple stuff first.

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Tokenizer.Reset (Analyzer);
      --  The very first call to find_next can't be look_ahead =>
      --  true, since no real parser would do that.
      Tokenizer.Find_Next (Analyzer, Look_Ahead => False);

      --  The higher level parser does this:
      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
      begin
         Master_Token.Parse (T0_Token, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("T0", Analyzer,
            Last_Token  => T1,
            Tail_Tokens => (T0, T1),
            Queue_Token => T1,
            Head_Null   => True);

         declare
            Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
         begin
            --  Now we do this:
            begin
               Master_Token.Parse (T3_Token, Analyzer, Actively => False);
               Assert (False, "T3 did not raise exception");
            exception
            when E : OpenToken.Parse_Error =>
               Check ("T3.exception message", Ada.Exceptions.Exception_Message (E), "");
            end;

            --  Parse did not do Find_Next
            Analyzer_AUnit.Check
              ("T3", Analyzer,
               Last_Token  => T1,
               Tail_Tokens => (T0, T1),
               Queue_Token => T1,
               Head_Null   => True);

            --  The higher level tries another token:
            Tokenizer.Push_Back (Analyzer, Mark);
         end;

         Analyzer_AUnit.Check
           ("T3a", Analyzer,
            Last_Token  => T1,
            Tail_Tokens => (T0, T1),
            Queue_Token => T1,
            Head_Null  => True);

         Master_Token.Parse (T1_Token, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("T1", Analyzer,
            Last_Token  => T2,
            Tail_Tokens => (T0, T1, T2),
            Queue_Token => T1,
            Head_Null   => True);

         --  And finally the higher level does the active parse. Since
         --  it is switching from inactive to active, it must
         --  push_back to restore Last_Token.
         Tokenizer.Push_Back (Analyzer, Mark);
      end;
      Master_Token.Parse (T0_Token, Analyzer);

      Analyzer_AUnit.Check
        ("Final T0", Analyzer,
         Last_Token  => T1,
         Tail_Tokens => (T1, T2),
         Queue_Token => T2,
         Head_Token  => T2);

      Master_Token.Parse (T1_Token, Analyzer);
      Master_Token.Parse (T2_Token, Analyzer);

      Analyzer_AUnit.Check
        ("Final", Analyzer,
         Last_Token => EOF,
         Tail_Null  => True,
         Queue_Null => True,
         Head_Null  => True);

   end Enumerated_Lookahead;

   procedure Sequence_Lookahead (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use AUnit.Assertions;
      use AUnit.Check;

      --  This tests lookahead encountering EOF. It matches T0 E.
      Text : constant String := "T0 T1 T2 T3";
   begin
      --  Verify that Sequence.Parse leaves the analyzer in the proper
      --  state with Actively => False.

      --  This parse does lookahead, but not backtracking.

      if Test.Debug then
         Ada.Text_IO.Put_Line ("Sequence_Lookahead " & Text);
      end if;

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer, Look_Ahead => False);

      --  The higher level parser does this:
      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
      begin
         Master_Token.Parse (T0_Token, Analyzer, Actively => False);

         declare
            Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
         begin
            --  Now we do this:
            begin
               Sequence.Parse (F, Analyzer, Actively => False);
               Assert (False, "F did not raise exception");
            exception
            when E : OpenToken.Parse_Error =>
               Check ("F.exception message", Ada.Exceptions.Exception_Message (E), "");
            end;

            Analyzer_AUnit.Check
              ("F", Analyzer,
               Last_Token  => T3,
               Tail_Tokens => (T0, T1, T2, T3),
               Queue_Token => T1,
               Head_Null   => True);

            Tokenizer.Push_Back (Analyzer, Mark);
         end;

         Sequence.Parse (E, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("F", Analyzer,
            Last_Token  => EOF,
            Tail_Tokens => (T0, T1, T2, T3, EOF),
            Queue_Token => T1,
            Head_Null   => True);

         --  And finally the active parse
         Tokenizer.Push_Back (Analyzer, Mark);
      end;
      Master_Token.Parse (T0_Token, Analyzer);
      Sequence.Parse (E, Analyzer);

      Analyzer_AUnit.Check
        ("Final", Analyzer,
         Last_Token => EOF,
         Tail_Null  => True,
         Queue_Null => True,
         Head_Null  => True);

   end Sequence_Lookahead;

   procedure Selection_Backtrack (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use AUnit.Assertions;
      use AUnit.Check;
      --  Match T0 C C, on the second and then first selection. Does
      --  not match T0 B.
      Text : constant String := "T0 T1 T2 T5 T1 T2 T3";
   begin
      --  Verify that Selection.Parse returns the analyzer to the
      --  proper state with Actively => False.

      if Test.Debug then
         Ada.Text_IO.Put_Line ("Selection_Backtrack " & Text);
      end if;

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer, Look_Ahead => False);

      --  The higher level parser does this:
      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
      begin
         Master_Token.Parse (T0_Token, Analyzer, Actively => False);
         declare
            Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
         begin
            --  Now we do this:
            begin
               --  This requires backtracking to check the second selection
               Selection.Parse (B, Analyzer, Actively => False);
               Assert (False, "B did not raise exception");
            exception
            when E : OpenToken.Parse_Error =>
               Check ("B.exception message", Ada.Exceptions.Exception_Message (E), "");
            end;

            --  Selection.Parse called Push_Back after the error
            Analyzer_AUnit.Check
              ("B", Analyzer,
               Last_Token  => T1,
               Tail_Tokens => (T0, T1, T2, T5),
               Queue_Token => T1,
               Head_Token  => T2);

            Tokenizer.Push_Back (Analyzer, Mark);
         end;

         Selection.Parse (C, Analyzer, Actively => False);

         --  Selection.Parse did _not_ call Push_Back
         Analyzer_AUnit.Check
           ("C", Analyzer,
            Last_Token  => T1,
            Tail_Tokens => (T0, T1, T2, T5, T1),
            Queue_Token => T1,
            Head_Null   => True);

         Selection.Parse (C, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("C2", Analyzer,
            Last_Token  => EOF,
            Tail_Tokens => (T0, T1, T2, T5, T1, T2, T3, EOF),
            Queue_Token => T1,
            Head_Null   => True);

         --  And finally the active parse
         Tokenizer.Push_Back (Analyzer, Mark);
      end;
      Master_Token.Parse (T0_Token, Analyzer);
      Selection.Parse (C, Analyzer);
      Selection.Parse (C, Analyzer);

      Analyzer_AUnit.Check
        ("Final", Analyzer,
         Last_Token => EOF,
         Tail_Null  => True,
         Queue_Null => True,
         Head_Null  => True);

   end Selection_Backtrack;

   procedure Sequence_Backtrack (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
      use AUnit.Check;

      --  Match T0 A, with backtracking.
      Text : constant String := "T0 T1 T2 T4 T1 T2 T3";
   begin
      --  Verify that Sequence.Parse leaves the analyzer in the proper
      --  state with Actively => False, when a lower level parser
      --  does backtracking.

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer, Look_Ahead => False);

      --  The higher level parser does this:
      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Tokenizer.Mark_Push_Back (Analyzer);
      begin
         Master_Token.Parse (T0_Token, Analyzer, Actively => False);

         --  Now we do this:
         Sequence.Parse (A, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("A", Analyzer,
            Last_Token  => EOF,
            Tail_Tokens => (T0, T1, T2, T4, T1, T2, T3, EOF),
            Queue_Token => T1,
            Head_Null   => True);

         --  And finally the active parse
         Tokenizer.Push_Back (Analyzer, Mark);
      end;
      Master_Token.Parse (T0_Token, Analyzer);
      Sequence.Parse (A, Analyzer);

      Analyzer_AUnit.Check
        ("Final", Analyzer,
         Last_Token => EOF,
         Tail_Null  => True,
         Queue_Null => True,
         Head_Null  => True);

   end Sequence_Backtrack;

   ----------
   --  Public subprograms

   overriding function Name (T : in Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Backtrack");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Enumerated_Lookahead'Access, "Enumerated_Lookahead");
      Register_Routine (T, Sequence_Lookahead'Access, "Sequence_Lookahead");
      Register_Routine (T, Selection_Backtrack'Access, "Selection_Backtrack");
      Register_Routine (T, Sequence_Backtrack'Access, "Sequence_Backtrack");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      OpenToken.Token.Trace_Parse := T.Debug;
   end Set_Up_Case;

end Test_Backtrack;
