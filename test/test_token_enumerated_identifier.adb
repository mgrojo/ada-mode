--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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
with AUnit.Test_Cases.Registration;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Parser;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Identifier;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Token_Enumerated_Identifier is

   type Token_ID_Type is
     (EOF_ID,
      Identifier_ID,
      Whitespace_ID,

      --  non-terminals
      Parse_Sequence_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID_Type);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);

   package Identifier_Tokens is new Master_Token.Identifier;

   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;

   use type Production.Instance;        --  "<="
   use type Production_List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_List.Instance;        --  "&"

   package Tokens is
      --  Terminals
      EOF        : constant Master_Token.Class := Master_Token.Get (EOF_ID);
      Identifier : constant Master_Token.Class := Identifier_Tokens.Get (Identifier_ID);

      --  Nonterminals
      Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   end Tokens;

   package Tokenizer is new Master_Token.Analyzer (Last_Terminal => Whitespace_ID);

   Syntax : constant Tokenizer.Syntax :=
     (EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get, Tokens.EOF),
      Identifier_ID => Tokenizer.Get (OpenToken.Recognizer.Identifier.Get, New_Token => Tokens.Identifier),
      Whitespace_ID => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   Expected_Identifier : Ada.Strings.Unbounded.Unbounded_String;

   procedure Test_Action
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_ID_Type)
   is
      use Token_List;
      use Ada.Strings.Unbounded;
      use OpenToken.Buffers;

      I : constant List_Iterator := Initial_Iterator (Source); --  identifier

      Identifier : Identifier_Tokens.Instance renames Identifier_Tokens.Instance (Token_Handle (I).all);
   begin
      New_Token := Nonterminal.Get (To_ID);

      AUnit.Assertions.Assert
        (To_String (Expected_Identifier) = To_String (Identifier.Identifier), "identifier mismatch");
   end Test_Action;

   Grammar : constant Production_List.Instance :=
     Production_List.Only (Tokens.Parse_Sequence <= Tokens.Identifier + Test_Action'Access);

   package OpenToken_Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new OpenToken_Parser.LALR;
   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   An_Analyzer : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax);
   Command_Parser : LALR_Parser.Instance;

   procedure Execute_Command (Command : in String; Trace : in Boolean)
   is begin
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);

      LALR_Parser.Parse (Command_Parser);
   end Execute_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      procedure One (Item : in String)
      is begin
         Expected_Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Item);
         Execute_Command (Item, Trace => Test.Debug);
      end One;

   begin
      --  We assume the recognizer works; just show that the
      --  identifier gets stored in the token properly.

      Command_Parser := LALR_Parser.Generate (Grammar, An_Analyzer, Trace => Test.Debug);

      LALR_Parser.Set_Text_Feeder (Command_Parser, String_Feeder'Unchecked_Access);

      LALR_Parser.Set_Trace (Command_Parser, Test.Debug);

      One ("An_Identifier");
      One ("Another_Identifier");
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Token_Enumerated_Identifier");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Token_Enumerated_Identifier;
