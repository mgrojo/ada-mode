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
with Wisi.Rules_Token;
package Wisi.Grammar is

   --  Define tokens used in syntax and productions below
   --
   --  Terminals
   Action          : aliased constant Tokens.Class := Tokens.Get (Action_ID);
   EOF             : aliased constant Tokens.Class := Tokens.Get (EOF_ID);
   Percent_Percent : aliased constant Tokens.Class := Tokens.Get (Percent_Percent_ID);

   --  Nonterminals
   Parse_Sequence : aliased constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Prologue       : aliased constant Nonterminal.Class := Nonterminal.Get (Prologue_ID);

   --  Define a lexer syntax for the terminals

   use Ada.Strings.Maps;

   Symbol_Set : constant Character_Set :=
     To_Set (Character_Range'('0', '9')) or
     To_Set (Character_Range'('A', 'Z')) or
     To_Set (Character_Range'('a', 'z')) or
     To_Set ('_');

   use OpenToken.Recognizer;
   Syntax : constant Tokenizer.Syntax :=
     (Action_ID          => Tokenizer.Get
        (Bracketed_Comment.Get
           ("(", ")",
            Reportable   => True,
            Nested       => True),
         Action),
      Bar_ID             => Tokenizer.Get (Keyword.Get ("|"), Tokens.Get (Bar_ID)),
      Comment_ID         => Tokenizer.Get (Line_Comment.Get ("//")),
      Colon_ID           => Tokenizer.Get (Keyword.Get (":"), Tokens.Get (Colon_ID)),
      EOF_ID             => Tokenizer.Get (End_Of_File.Get),
      Prologue_ID        => Tokenizer.Get
        (Bracketed_Comment.Get
           ("%{", "}%",
            Reportable   => True),
         Prologue),
      Percent_Percent_ID => Tokenizer.Get (Keyword.Get ("("), Percent_Percent),
      Semicolon_ID       => Tokenizer.Get (Keyword.Get (";"), Tokens.Get (Semicolon_ID)),
      Symbol_ID          => Tokenizer.Get
        (Recognizer      => Identifier.Get (Body_Chars => Symbol_Set),
         New_Token       => Symbol),
      Whitespace_ID      => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   procedure Output_Elisp
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token_IDs);

   Grammar : constant Production_List.Instance :=
     Parse_Sequence <= Prologue & Percent_Percent & Rules_Token.Rules & Percent_Percent & EOF + Output_Elisp'Access and
     Rules_Token.Grammar;

end Wisi.Grammar;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
