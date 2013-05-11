--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2010, 2012, 2013 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Strings.Maps.Constants;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Recognizer.Based_Integer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Accept_Index is

   --  A simple grammar that OpenToken used to get wrong.
   --
   --  set foo = integer;

   type Token_IDs is
     (Whitespace_ID,
      Equals_ID,
      Int_ID,
      Set_ID,
      EOF_ID,
      Identifier_ID,

      --  non-terminals
      Parse_Sequence_ID,
      Statement_ID);

   --  Instantiate all the necessary packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Tokenizer is new Master_Token.Analyzer (Equals_ID, Identifier_ID);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package OpenToken_Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new OpenToken_Parser.LALR (First_State_Index => 1);

   package Integer_Literal is new Master_Token.Integer;

   --  Define all our tokens
   Equals : constant Master_Token.Class := Master_Token.Get (Equals_ID);
   Int    : constant Master_Token.Class := Integer_Literal.Get (Int_ID);
   EOF    : constant Master_Token.Class := Master_Token.Get (EOF_ID);

   Identifier     : constant Master_Token.Class := Master_Token.Get (Identifier_ID);
   Parse_Sequence : constant Nonterminal.Class  := Nonterminal.Get (Parse_Sequence_ID);
   Statement      : constant Nonterminal.Class  := Nonterminal.Get (Statement_ID);

   Syntax : constant Tokenizer.Syntax :=
     (Equals_ID         => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("="), Master_Token.Get (Equals_ID, "=")),
      Int_ID            => Tokenizer.Get (OpenToken.Recognizer.Based_Integer.Get, Int),
      Set_ID            => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("set"), Master_Token.Get (Set_ID, "set")),
      EOF_ID            => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Master_Token.Get (EOF_ID)),
      Identifier_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
            Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
         Master_Token.Get (Identifier_ID, "identifier")),
      Whitespace_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   Grammar : constant Production_List.Instance :=
     --  First production in Grammar must be the terminating
     --  production; it gets the accept action.
     Parse_Sequence <= Statement & EOF and
     Statement <= Master_Token.Get (Set_ID) & Identifier & Equals & Int + Nonterminal.Synthesize_Self;

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer      : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   Parser        : LALR_Parser.Instance;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  The test is that there are no exceptions.

      Parser := LALR_Parser.Generate
        (Grammar, Analyzer,
         Trace       => Test.Debug,
         Put_Grammar => Test.Debug);

      OpenToken.Trace_Parse := Test.Debug;

      LALR_Parser.Set_Text_Feeder (Parser, String_Feeder'Unchecked_Access);

      OpenToken.Text_Feeder.String.Set (String_Feeder, "set A = 2");

      LALR_Parser.Parse (Parser);

   exception
   when E : others =>
      declare
         use Ada.Exceptions;
      begin
         AUnit.Assertions.Assert (False, Exception_Name (E) & ": " & Exception_Message (E));
      end;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_accept_index.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Accept_Index;
