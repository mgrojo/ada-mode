-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephen Leake
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
-----------------------------------------------------------------------------

with AUnit.Check;
with AUnit.Test_Cases.Registration;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer.AUnit;
package body Lookahead_Test is

   type Example_Token_ID is (If_ID, Then_ID, Quit_ID, String_ID, Whitespace, EOF);

   package Master_Example_Token is new OpenToken.Token.Enumerated (Example_Token_ID);
   package Tokenizer is new Master_Example_Token.Analyzer;

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Example_Token_ID);

   package Analyzer_AUnit is new Tokenizer.AUnit (Check);

   Syntax : constant Tokenizer.Syntax :=
     (If_ID      => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("if")),
      Then_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("then")),
      Quit_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("quit")),
      String_ID  => Tokenizer.Get (OpenToken.Recognizer.String.Get),
      Whitespace => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                     (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get));

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   procedure Step
     (Label       : in String;
      Lookahead   : in Boolean;
      Expected_ID : in Example_Token_ID)
   is
      use Tokenizer;
   begin
      Find_Next (Analyzer, Lookahead);

      Check (Label, ID (Analyzer), Expected_ID);
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
      Tokenizer.Reset (Analyzer);

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

      OpenToken.Token.Trace_Parse := Test.Debug;

      OpenToken.Text_Feeder.String.Set (Feeder, "if then ""string"" quit");
      Reset (Analyzer);

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

      Step ("2", True, Then_ID);

      Analyzer_AUnit.Check
        ("2a", Analyzer,
         Last_Token  => Then_ID,
         Tail_Tokens => (If_ID, Then_ID),
         Queue_Token => Then_ID,
         Head_Null   => True);

      Push_Back (Analyzer, 1);

      Analyzer_AUnit.Check
        ("3", Analyzer,
         Last_Token  => If_ID,
         Tail_Tokens => (If_ID, Then_ID),
         Queue_Token => Then_ID,
         Head_Token  => Then_ID);

      Step ("4", True, Then_ID);

      Analyzer_AUnit.Check
        ("4a", Analyzer,
         Last_Token  => Then_ID,
         Tail_Tokens => (If_ID, Then_ID),
         Queue_Token => Then_ID,
         Head_Null   => True);

      Step ("5", True, String_ID);
      Step ("6", True, Quit_ID);

      Analyzer_AUnit.Check
        ("6a", Analyzer,
         Last_Token  => Quit_ID,
         Tail_Tokens => (If_ID, Then_ID, String_ID, Quit_ID),
         Queue_Token => Then_ID,
         Head_Null   => True);

      Push_Back (Analyzer, 1);

      Analyzer_AUnit.Check
        ("7", Analyzer,
         Last_Token  => String_ID,
         Tail_Tokens => (If_ID, Then_ID, String_ID, Quit_ID),
         Queue_Token => Then_ID,
         Head_Token  => Quit_ID);

      Push_Back (Analyzer, 2);

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

      Step ("11", True, Quit_ID);

      Analyzer_AUnit.Check
        ("11a", Analyzer,
         Last_Token  => Quit_ID,
         Tail_Tokens => (String_ID, Quit_ID),
         Queue_Token => Quit_ID,
         Head_Null   => True);

      Push_Back (Analyzer, 1);

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

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Lookahead_Test");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Lookahead'Access, "Lookahead");
      Register_Routine (T, Lookahead_Push_Back'Access, "Lookahead_Push_Back");
   end Register_Tests;

end Lookahead_Test;
