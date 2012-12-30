--  Abstract :
--
--  OpenToken grammar for Wisent input file
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Strings.Maps;
with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.Line_Comment;
with OpenToken.Recognizer.String;
with Wisi.Rules_Token;
with Wisi.Declarations_Token;
package Wisi.Grammar is

   --  Define tokens used in syntax and productions below
   --
   --  Terminals
   Action : constant Tokens.Class := Tokens.Get (Action_ID);
   EOF    : constant Tokens.Class := Tokens.Get (EOF_ID);

   --  Nonterminals
   Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Prologue       : constant Nonterminal.Class := Nonterminal.Get (Prologue_ID);

   --  Define a lexer syntax for the terminals

   use Ada.Strings.Maps;

   Symbol_Set : constant Character_Set :=
     To_Set (Character_Range'('0', '9')) or
     To_Set (Character_Range'('A', 'Z')) or
     To_Set (Character_Range'('a', 'z')) or
     To_Set ('_') or
     To_Set ('-'); -- for elisp names

   use OpenToken.Recognizer;
   Syntax : constant Tokenizer.Syntax :=
     (Action_ID         => Tokenizer.Get
        (Bracketed_Comment.Get
           ("(", ")",
            Reportable  => True,
            Nested      => True),
         Action),
      Bar_ID            => Tokenizer.Get (Keyword.Get ("|"), Tokens.Get (Bar_ID)),
      Bracket_Symbol_ID => Tokenizer.Get (Keyword.Get ("<symbol>"), Tokens.Get (Bracket_Symbol_ID)),
      Comment_1_ID      => Tokenizer.Get (Line_Comment.Get ("//")),
      Comment_2_ID      => Tokenizer.Get (Line_Comment.Get (";;")),
      Colon_ID          => Tokenizer.Get (Keyword.Get (":"), Tokens.Get (Colon_ID)),
      EOF_ID            => Tokenizer.Get (End_Of_File.Get),
      Keyword_ID        => Tokenizer.Get (Keyword.Get ("keyword"), Tokens.Get (Keyword_ID)),
      Package_ID        => Tokenizer.Get (Keyword.Get ("package"), Tokens.Get (Package_ID)),
      Percent_ID        => Tokenizer.Get (Keyword.Get ("%"), Percent),
      Prologue_ID       => Tokenizer.Get (Bracketed_Comment.Get ("%{", "%}", Reportable => True), Prologue),
      Semicolon_ID      => Tokenizer.Get (Keyword.Get (";"), Tokens.Get (Semicolon_ID)),
      Start_ID          => Tokenizer.Get (Keyword.Get ("start"), Tokens.Get (Start_ID)),
      String_ID         => Tokenizer.Get (OpenToken.Recognizer.String.Get, Tokens.Get (String_ID)),
      Token_ID          => Tokenizer.Get (Keyword.Get ("token"), Tokens.Get (Token_ID)),

      Symbol_ID         => Tokenizer.Get (Identifier.Get (Symbol_Set, Symbol_Set, False), Symbol),
      Whitespace_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   procedure Output_Elisp
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token_IDs);

   Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Prologue & Declarations_Token.Declarations & Rules_Token.Rules &
     EOF + Output_Elisp'Access and
     Declarations_Token.Grammar and
     Rules_Token.Grammar;

end Wisi.Grammar;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
