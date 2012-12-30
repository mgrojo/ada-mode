--  Abstract :
--
--  root package
--
--  "wisi" is short for "Wisent Indentation engine"; the Emacs 'wisi'
--  package implements an indentation engine based on the Emacs wisent
--  parser.
--
--  "wisent" is the European bison [4].
--
--  The 'wisent' parser generator is the Gnu parser generator
--  implemented in Emacs elisp, as part of the semantic package [2].
--  The parser itself (wisent-parse) is part of Emacs, but the parser
--  compiler (wisent-compile-grammar) is not.
--
--  The input file syntax is the based on Gnu bison syntax [1] with
--  some additions, apparently not documented anywhere. See [3] for
--  the syntax accepted by wisi-compile.
--
--  The OpenToken wisent compiler (wisi-compile) reads the Wisent
--  input file, and outputs a compiled grammar in elisp format; it
--  matches the output of wisent-compile-grammar, and can thus be used
--  by wisent-parse.
--
--  Reference :
--
--  [1] http://www.gnu.org/software/bison/manual/ (info "(bison)Top")
--  [2] http://cedet.sourceforge.net/semantic.shtml
--  [3] wisi-user-manual.texi
--  [4] http://en.wikipedia.org/wiki/Wisent
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

with OpenToken.Production.List;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package Wisi is

   type Token_IDs is
     (
      --  terminals
      Action_ID,
      Bar_ID,
      Bracket_Symbol_ID,
      Colon_ID,
      Comment_1_ID,
      Comment_2_ID,
      EOF_ID,
      Keyword_ID,
      Package_ID,
      Percent_ID,
      Prologue_ID,
      Semicolon_ID,
      Start_ID,
      String_ID,
      Token_ID,

      --  Keywords have precedence over symbols
      Symbol_ID,

      --  last terminal
      Whitespace_ID,

      --  non-terminals
      Declaration_ID,
      Declarations_ID,
      Parse_Sequence_ID,
      Right_Hand_Sides_ID,
      Rule_ID,
      Rules_ID);

   package Tokens is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Tokens.Analyzer (Whitespace_ID);
   package Token_List is new Tokens.List;
   package Nonterminal is new Tokens.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Tokens, Token_List, Nonterminal);
   package Production_List is new Production.List;

   --  Allow infix operators for grammar
   use type Token_List.Instance;        -- "&"
   use type Production.Right_Hand_Side; -- "+"
   use type Production.Instance;        -- "<="
   use type Production_List.Instance;   -- "and"

   --  Tokens used in more than one grammar and syntax declarations
   Percent   : constant Tokens.Class := Tokens.Get (Percent_ID);
   Semicolon : constant Tokens.Class := Tokens.Get (Semicolon_ID);
   Symbol    : constant Tokens.Class := Tokens.Get (Symbol_ID);

end Wisi;
