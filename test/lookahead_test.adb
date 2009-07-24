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
with OpenToken.Token.Enumerated.Analyzer;
package body Lookahead_Test is

   type Example_Token_ID is (If_ID, Then_ID, Quit_ID, String_ID, Whitespace, EOF);

   package Master_Example_Token is new OpenToken.Token.Enumerated (Example_Token_ID);
   package Tokenizer is new Master_Example_Token.Analyzer;

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

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Example_Token_ID);

   procedure Step (Label : in String; Lookahead : in Boolean; Expected_ID : in Example_Token_ID)
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

      Step ("1", False, If_ID);

      Step ("2", True, Then_ID);
      --  last_token => Then_ID
      --  tail => If_ID
      --  tail.prev => null
      --  queue => null
      --  head => null

      Push_Back (Analyzer, 1);
      --  last_token => If_ID
      --  tail => then_id
      --  tail.prev => if_id
      --  tail.prev.prev => null
      --  queue => null
      --  head => then

      Check ("2a", ID (Analyzer), If_ID);
      Step ("2b", True, Then_ID);
      --  last_token => Then_ID
      --  tail => Then_ID
      --  tail.prev => null
      --  queue => null
      --  head => null

      Step ("3", True, String_ID);
      Step ("4", True, Quit_ID);

      --  last_token => Quit_ID
      --  tail => String_ID
      --  tail.prev => Then_ID
      --  tail.prev.prev => If_ID
      --  tail.prev.prev.prev => null
      --  queue => If_ID
      --  head => null

      Push_Back (Analyzer, 3);
      --  last_token => Quit_ID
      --  tail => String_ID
      --  tail.prev => Then_ID
      --  tail.prev.prev => If_ID
      --  tail.prev.prev.prev => null
      --  queue => If_ID
      --  head => null

      Check ("4a", ID (Analyzer), If_ID);

      Step ("5", True, Then_ID);    --  Read from Lookahead_Head
      Step ("6", False, Then_ID);   --  Read from Lookahead_Queue
      Step ("7", False, String_ID); --  Read from Lookahead_Queue, update Lookahead_Head
      Step ("8", True, Quit_ID);    --  Read from Lookahead_Head

      Push_Back (Analyzer, 1);

      Step ("9", False, Quit_ID);
      Step ("10", False, EOF);
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
