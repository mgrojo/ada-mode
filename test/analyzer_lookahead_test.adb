-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2010, 2012, 2013, 2014 Stephen Leake
--  Copyright (C) 2000 Ted Dennison
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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Check;
with Ada.Tags;
with Ada.Text_IO;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer.AUnit;
with OpenToken.Token.Enumerated.String;
package body Analyzer_Lookahead_Test is

   type Token_ID is (If_ID, Then_ID, Quit_ID, String_ID, Whitespace, EOF);

   package Master_Example_Token is new OpenToken.Token.Enumerated
     (Token_ID, Token_ID'First, Token_ID'Last, Token_ID'Image);
   package Tokenizer is new Master_Example_Token.Analyzer;
   package String_Literal is new Master_Example_Token.String;

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_ID);

   package Analyzer_AUnit is new Tokenizer.AUnit (Check);

   Syntax : constant Tokenizer.Syntax :=
     (If_ID      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("if")),
      Then_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("then")),
      Quit_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("quit")),
      String_ID  => Tokenizer.Get (OpenToken.Recognizer.String.Get,
                                   New_Token => String_Literal.Get (String_ID)),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                     (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   procedure Step
     (Label       : in String;
      Lookahead   : in Boolean;
      Expected_ID : in Token_ID)
   is
      use Tokenizer;
   begin
      Analyzer.Find_Next (Lookahead);

      Check (Label, Analyzer.ID, Expected_ID);
   end Step;

   ----------
   --  Test procedures

   procedure Lookahead (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Text : constant String := """This is a standard WSIWG """"Ada"""" string \n.""if then quit";
   begin
      ---------------------------------------------------------------------------
      --  Purpose          : Verify that the analyzer tokenizes properly in
      --                     lookahead mode.
      --  Input            : A string, followed by an "if", a "then" and a "quit"
      --                     token.
      --  Expected Results : The tokens should be returned in the correct order.
      --                     When non-lookahead calls to Find_Next resume, the
      --                     sequence of tokens should resume from right after
      --                     where it left off.
      ---------------------------------------------------------------------------

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Analyzer.Reset;

      Step ("1", False, String_ID);
      Step ("2", True, If_ID);
      Step ("3", True, Then_ID);
      Step ("4", True, Quit_ID);
      Step ("5", True, EOF);

      Step ("6", False, If_ID);
      Step ("7", False, Then_ID);
      Step ("8", False, Quit_ID);

   end Lookahead;

   procedure Lookahead_Push_Back (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Tokenizer;

   begin
      --  Verify that Push_Back works.

      OpenToken.Trace_Parse := Test.Trace_Parse;

      OpenToken.Text_Feeder.String.Set (Feeder, "if then ""string"" quit");
      Analyzer.Reset;

      Analyzer_AUnit.Check
        ("0", Analyzer,
         Last_Token  => If_ID,
         Tail_Null   => True,
         Queue_Null  => True,
         Head_Null   => True);

      Step ("1", False, If_ID);

      Analyzer_AUnit.Check
        ("1a", Analyzer,
         Last_Token  => If_ID,
         Tail_Null   => True,
         Queue_Null  => True,
         Head_Null   => True);

      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Analyzer.Mark_Push_Back;
      begin
         Step ("2", True, Then_ID);

         Analyzer_AUnit.Check
           ("2a", Analyzer,
            Last_Token  => Then_ID,
            Tail_Tokens => (If_ID, Then_ID),
            Queue_Token => Then_ID,
            Head_Null   => True);

         Analyzer.Push_Back (Mark);
      end;

      Analyzer_AUnit.Check
        ("3", Analyzer,
         Last_Token  => If_ID,
         Tail_Tokens => (If_ID, Then_ID),
         Queue_Token => Then_ID,
         Head_Token  => Then_ID);

      declare
         Mark_2 : OpenToken.Token.Queue_Mark'Class renames Analyzer.Mark_Push_Back;
      begin
         Step ("4", True, Then_ID);

         Analyzer_AUnit.Check
           ("4a", Analyzer,
            Last_Token  => Then_ID,
            Tail_Tokens => (If_ID, Then_ID),
            Queue_Token => Then_ID,
            Head_Null   => True);

         Step ("5", True, String_ID);

         Analyzer_AUnit.Check
           ("5a", Analyzer,
            Last_Token  => String_ID,
            Tail_Tokens => (If_ID, Then_ID, String_ID),
            Queue_Token => Then_ID,
            Head_Null   => True);

         declare
            Mark_1 : OpenToken.Token.Queue_Mark'Class renames Analyzer.Mark_Push_Back;
         begin
            Step ("6", True, Quit_ID);

            Analyzer_AUnit.Check
              ("6a", Analyzer,
               Last_Token  => Quit_ID,
               Tail_Tokens => (If_ID, Then_ID, String_ID, Quit_ID),
               Queue_Token => Then_ID,
               Head_Null   => True);

            Analyzer.Push_Back (Mark_1);
         end;

         Analyzer_AUnit.Check
           ("7", Analyzer,
            Last_Token  => String_ID,
            Tail_Tokens => (If_ID, Then_ID, String_ID, Quit_ID),
            Queue_Token => Then_ID,
            Head_Token  => Quit_ID);

         Analyzer.Push_Back (Mark_2);
      end;

      Analyzer_AUnit.Check
        ("7a", Analyzer,
         Last_Token  => If_ID,
         Tail_Tokens => (If_ID, Then_ID, String_ID, Quit_ID),
         Queue_Token => Then_ID,
         Head_Token  => Then_ID);

      Step ("8", True, Then_ID);
      Step ("9", False, Then_ID);

      Analyzer_AUnit.Check
        ("9a", Analyzer,
         Last_Token  => Then_ID,
         Tail_Tokens => (Then_ID, String_ID, Quit_ID),
         Queue_Token => String_ID,
         Head_Token  => String_ID);

      Step ("10", False, String_ID);

      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Analyzer.Mark_Push_Back;
      begin
         Step ("11", True, Quit_ID);

         Analyzer_AUnit.Check
           ("11a", Analyzer,
            Last_Token  => Quit_ID,
            Tail_Tokens => (String_ID, Quit_ID),
            Queue_Token => Quit_ID,
            Head_Null   => True);

         Analyzer.Push_Back (Mark);
      end;

      Analyzer_AUnit.Check
        ("11b", Analyzer,
         Last_Token  => String_ID,
         Tail_Tokens => (String_ID, Quit_ID),
         Queue_Token => Quit_ID,
         Head_Token  => Quit_ID);

      Step ("12", False, Quit_ID);
      Step ("13", False, EOF);

      Analyzer_AUnit.Check
        ("13a", Analyzer,
         Last_Token => EOF,
         Tail_Null  => True,
         Queue_Null => True,
         Head_Null  => True);

   end Lookahead_Push_Back;

   procedure Check_Lexeme
     (Label           : in     String;
      Token           : in     Master_Example_Token.Handle;
      Expected_Lexeme : access String)
   is
      use AUnit.Check;
   begin
      if Expected_Lexeme /= null then
         declare
            String_Token : String_Literal.Instance renames String_Literal.Instance (Token.all);
         begin
            Check (Label & ".lexeme", String_Literal.To_String (String_Token.Value), Expected_Lexeme.all);
         end;
      end if;
   end Check_Lexeme;

   procedure Lookahead_Lexemes (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Text : constant String := """string 1"" ""string 2"" ";

      String_1_Value : aliased String := "string 1";
      String_2_Value : aliased String := "string 2";

      String_Token : aliased String_Literal.Instance := String_Literal.Instance (String_Literal.Get (String_ID));

      procedure Test
        (Label           : in     String;
         Actively        : in     Boolean;
         Token           : access Master_Example_Token.Class;
         Expected_Lexeme : in     String                := "")
      is
         use AUnit.Check;
         use type Ada.Tags.Tag;
      begin
         Master_Example_Token.Parse (Token, Analyzer, Actively);
         Check
           (Label & ".lexeme", String_Literal.To_String (String_Literal.Instance (Token.all).Value), Expected_Lexeme);
      end Test;

   begin
      --  Verify that tokens preserve lexemes when parsing inactively,
      --  even if the tokens are repeated.

      if Test_Case (T).Trace_Parse > 0 then
         Ada.Text_IO.Put_Line ("Lookahead_Lexemes");
      end if;

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Analyzer.Reset;

      --  The very first call to find_next can't be look_ahead =>
      --  true, since no real parser would do that.
      Analyzer.Find_Next (Look_Ahead => False);

      declare
         Mark : OpenToken.Token.Queue_Mark'Class renames Analyzer.Mark_Push_Back;
      begin
         --  Lexemes are not copied to tokens during inactive parse,
         --  but they are copied into the tokens pushed on the queue,
         --  so we check that here.
         String_Literal.Parse (String_Token'Access, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("1p", Analyzer,
            Last_Token   => String_ID,
            Tail_Tokens  => (String_ID, String_ID),
            Tail_Lexemes => (String_1_Value'Unchecked_Access, String_2_Value'Unchecked_Access),
            Queue_Token  => String_ID,
            Head_Null    => True,
            Check_Token  => Check_Lexeme'Access);

         String_Literal.Parse (String_Token'Access, Analyzer, Actively => False);

         Analyzer_AUnit.Check
           ("2p", Analyzer,
            Last_Token   => EOF,
            Tail_Tokens  => (String_ID, String_ID, EOF),
            Tail_Lexemes => (String_1_Value'Unchecked_Access, String_2_Value'Unchecked_Access, null),
            Queue_Token  => String_ID,
            Head_Null    => True,
            Check_Token  => Check_Lexeme'Access);

         Analyzer.Push_Back (Mark);
      end;

      --  The lexemes were preserved by Copy, so they are correct here
      Test ("1a", True, String_Token'Access, "string 1");
      Test ("2a", True, String_Token'Access, "string 2");

   end Lookahead_Lexemes;

   procedure Mismatched_Tokens (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Text : constant String := """string 1"" ";

      Bad_String_Token : aliased Master_Example_Token.Class := Master_Example_Token.Get (String_ID);

   begin
      --  Verify the error from Parse when called with a token type
      --  that doesn't match the one in Syntax_List. This test doesn't
      --  do lookahead, but it tests a run-time check required by the
      --  lookahead design.

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Analyzer.Reset;
      Analyzer.Find_Next (Look_Ahead => False);

      begin
         Master_Example_Token.Parse (Bad_String_Token'Access, Analyzer, Actively => True);
         AUnit.Assertions.Assert (False, "did not get exception");
      exception
      when OpenToken.Programmer_Error =>
         null;
      end;
   end Mismatched_Tokens;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/analyzer_lookahead_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Lookahead'Access, "Lookahead");
      Register_Routine (T, Lookahead_Push_Back'Access, "Lookahead_Push_Back");
      Register_Routine (T, Lookahead_Lexemes'Access, "Lookahead_Lexemes");
      Register_Routine (T, Mismatched_Tokens'Access, "Mismatched_Tokens");
   end Register_Tests;

end Analyzer_Lookahead_Test;
