--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2010, 2012, 2013, 2014 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Maps.Constants;
with AUnit.Assertions;
with AUnit.Check;
with Ada.Exceptions;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
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
package body Test_LR_Expecting is

   --  A simple grammar for testing the Expecting function for generating nice error messages.
   --
   --  legal statments:
   --
   --  set foo = integer;
   --  verify foo = integer +- integer;
   --
   --  Nice errors:
   --
   --  foo; => expecting 'set, verify'
   --  set foo bar; => "expecting '='
   --  etc

   type Token_IDs is
     (Whitespace_ID,
      Equals_ID,
      Int_ID,
      Plus_Minus_ID,
      Semicolon_ID,
      Set_ID,
      Verify_ID,

      --  Identifier must be after keywords, so they are recognized instead
      Identifier_ID,

      EOF_ID,

      --  non-terminals
      Statement_ID,
      Parse_Sequence_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs, Equals_ID, EOF_ID, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Integer is new Master_Token.Integer;

   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package OpenToken_Parser is new Production.Parser (Tokenizer);
   package LALRs is new OpenToken_Parser.LALR (First_State_Index => 1);
   package LALR_Generators is new LALRs.Generator (Token_IDs'Width, Production_List);
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label => 1);
   package LALR_Parsers is new LALRs.Parser (1, Parser_Lists);

   --  Terminals
   EOF        : constant Master_Token.Class := Master_Token.Get (EOF_ID, Name => "EOF");
   Equals     : constant Master_Token.Class := Master_Token.Get (Equals_ID);
   Int        : constant Master_Token.Class := Integer.Get (Int_ID, Name => "integer");
   Plus_Minus : constant Master_Token.Class := Master_Token.Get (Plus_Minus_ID);
   Semicolon  : constant Master_Token.Class := Master_Token.Get (Semicolon_ID);

   Identifier : constant Master_Token.Class := Master_Token.Get (Identifier_ID);

   --  Nonterminals
   Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Statement      : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   package Set_Statement is

      type Instance is new Nonterminal.Instance with null record;

      overriding function Name (Token : in Instance) return String;

      Set_Statement : constant Instance := (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        --  set symbol = value
        Production_List.Only
        (Set_Statement <= Nonterminal.Get (Set_ID) & Identifier & Equals & Int + Nonterminal.Synthesize_Self);

   end Set_Statement;

   package body Set_Statement is
      overriding function Name (Token : in Instance) return String
      is
         pragma Unreferenced (Token);
      begin
         return "set";
      end Name;
   end Set_Statement;

   package Verify_Statement is

      type Instance is new Nonterminal.Instance with null record;

      overriding function Name (Token : in Instance) return String;

      Verify_Statement : constant Instance :=
        (Master_Token.Instance (Master_Token.Get (Statement_ID)) with null record);

      Grammar : constant Production_List.Instance :=
        --  verify symbol = value +- tolerance
        Production_List.Only
        (Verify_Statement  <= Nonterminal.Get (Verify_ID) & Equals & Int & Plus_Minus & Int +
           Nonterminal.Synthesize_Self);
   end Verify_Statement;

   package body Verify_Statement is
      overriding function Name (Token : in Instance) return String
      is
         pragma Unreferenced (Token);
      begin
         return "verify";
      end Name;
   end Verify_Statement;

   Syntax : constant Tokenizer.Syntax :=
     (Equals_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("="),
                                      Master_Token.Get (Name => "=")),
      Int_ID        => Tokenizer.Get (OpenToken.Recognizer.Based_Integer.Get, Int),
      Plus_Minus_ID => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+-"),
                                      Master_Token.Get (Name => "+-")),
      Semicolon_ID  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";"),
                                      Master_Token.Get (Name => ";")),
      Set_ID        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("set"),
                                      Master_Token.Get (Name => "set")),
      Verify_ID     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("verify"),
                                      Master_Token.Get (Name => "verify")),

      Identifier_ID => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,
            Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set),
         Master_Token.Get (Name => "identifier")),

      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, EOF));

   Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Statement & Semicolon & EOF + Nonterminal.Synthesize_Self and
     Set_Statement.Grammar and
     Verify_Statement.Grammar;

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer      : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax);
   Parser        : LALR_Parsers.Instance;

   procedure Execute
     (Command          : in String;
      Expected_Message : in String)
   is
      use LALR_Parsers;
   begin
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);

      Set_Text_Feeder (Parser, String_Feeder'Unchecked_Access);

      Parse (Parser);
      AUnit.Assertions.Assert (False, Command & "; no exception");
   exception
   when E : OpenToken.Syntax_Error =>
      AUnit.Check.Check (Command, Ada.Exceptions.Exception_Message (E), Expected_Message);
   end Execute;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parser := LALR_Parsers.Initialize
        (Analyzer,
         LALR_Generators.Generate
           (Grammar,
            Trace           => Test.Debug,
            Put_Parse_Table => Test.Debug));

      OpenToken.Trace_Parse := (if Test.Debug then 1 else 0);

      Execute ("set A = 2", "1:10: Syntax error; expecting ';'; found EOF '" & ASCII.EOT & "'");

      if Test.Debug then
         Execute ("set A = ", "1:9: Syntax error; expecting 'integer  2'; found EOF '" & ASCII.EOT & "'");
      else
         Execute ("set A = ", "1:9: Syntax error; expecting 'integer'; found EOF '" & ASCII.EOT & "'");
      end if;

      Execute ("set A", "1:6: Syntax error; expecting '='; found EOF '" & ASCII.EOT & "'");
      Execute ("set", "1:4: Syntax error; expecting 'identifier'; found EOF '" & ASCII.EOT & "'");

      if Test.Debug then
         Execute ("2", "1:1: Syntax error; expecting 'set' or 'verify'; found integer  2 '2'");
      else
         Execute ("2", "1:1: Syntax error; expecting 'set' or 'verify'; found integer '2'");
      end if;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_lr_expecting.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR_Expecting;
