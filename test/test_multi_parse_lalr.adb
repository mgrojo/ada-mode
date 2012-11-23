--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2010 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with AUnit.Assertions;
with AUnit.Check;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Source_Info;
with OpenToken.Production.List.Print;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Parser;
with OpenToken.Production.Print;
with OpenToken.Recognizer.Based_Integer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Multi_Parse_LALR is

   --  A simple grammar for testing calling Parse in a loop with a
   --  grammar that does not allow multiple statements, but input that
   --  has multiple statements.
   --
   --  legal statments:
   --
   --  set foo = integer;

   type Token_IDs is
     (Equals_ID,
      Int_ID,
      Set_ID,
      Semicolon_ID,

      --  Identifier must be after keywords, so they are recognized instead
      Identifier_ID,

      Whitespace_ID,

      --  non-terminals
      Parse_Sequence_ID,
      Statement_ID);

   --  Instantiate all the nessecary packages
   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package OpenToken_Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new OpenToken_Parser.LALR;

   package Integer_Literal is new Master_Token.Integer;

   --  Define all our tokens
   Equals    : constant Master_Token.Class := Master_Token.Get (Equals_ID);
   Int       : constant Master_Token.Class := Integer_Literal.Get (Int_ID, Name    => "integer");
   Semicolon : constant Master_Token.Class := Master_Token.Get (Semicolon_ID, Name => "semicolon");

   Identifier     : constant Master_Token.Class := Master_Token.Get (Identifier_ID);
   Parse_Sequence : constant Nonterminal.Class  := Nonterminal.Get (Parse_Sequence_ID);
   Statement      : constant Nonterminal.Class  := Nonterminal.Get (Statement_ID);

   Statement_Count : Integer := 0;

   procedure Count_Statements
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token_IDs)
   is begin
      Nonterminal.Synthesize_Self (New_Token, Source, To_ID);
      Statement_Count := Statement_Count + 1;
   end Count_Statements;

   Syntax : constant Tokenizer.Syntax :=
     (Equals_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("="), Master_Token.Get (Equals_ID, "=")),
      Int_ID        => Tokenizer.Get (OpenToken.Recognizer.Based_Integer.Get, Int),
      Set_ID        => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("set"), Master_Token.Get (Set_ID, "set")),
      Semicolon_ID  => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (";"), Master_Token.Get (Semicolon_ID, "=")),
      Identifier_ID => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get, Master_Token.Get (Identifier_ID, "identifier")),
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   Grammar : constant Production_List.Instance :=
     --  set symbol = value
     Statement <= Nonterminal.Get (Set_ID) & Identifier & Equals & Int + Count_Statements'Access and
     Parse_Sequence <= Statement & Semicolon;


   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Parser        : LALR_Parser.Instance;

   procedure Print_Action (Action : in Nonterminal.Synthesize) is null;

   procedure Dump_Grammar
   is
      package Print_Token_List is new Token_List.Print;
      package Print_Production is new Production.Print (Print_Token_List, Print_Action);
      package Print_Production_List is new Production_List.Print (Print_Production.Print);
   begin
      Print_Production_List.Print (Grammar);
      Ada.Text_IO.New_Line;
   end Dump_Grammar;

   procedure Dump_Parse_Table
   is begin
      LALR_Parser.Print_Table (Parser);
      Ada.Text_IO.New_Line;
   end Dump_Parse_Table;

   procedure Execute (Command : in String)
   is
      use AUnit.Check;

      Expected_Statement_Count : Integer;
      I                        : Integer := 0;
   begin
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);
      LALR_Parser.Reset (Parser);

      loop
         Expected_Statement_Count := Statement_Count + 1;
         LALR_Parser.Parse (Parser);
         I := I + 1;
         Check (Command & Integer'Image (I), Statement_Count, Expected_Statement_Count);
         exit when LALR_Parser.End_Of_Text (Parser);
      end loop;

   exception
   when E : OpenToken.Syntax_Error =>
      AUnit.Assertions.Assert
        (False, "SYNTAX_ERROR: " & Command & " :" & Integer'Image (I) & " : " & Ada.Exceptions.Exception_Message (E));
   end Execute;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parser := LALR_Parser.Generate (Grammar, Tokenizer.Initialize (Syntax), Trace => Test.Debug);

      if Test.Debug then
         Dump_Grammar;
         Dump_Parse_Table;
         OpenToken.Trace_Parse := True;
      end if;

      LALR_Parser.Set_Text_Feeder (Parser, String_Feeder'Unchecked_Access);

      Execute ("set A = 2;"); -- FIXME: this prefetches the EOF and reports an error

      Execute ("set A = 3; set B = 4;"); -- FIXME: this does the two statements properly, but then fetches the EOF

   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'(GNAT.Source_Info.File);
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Multi_Parse_LALR;
