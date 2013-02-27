-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2010, 2012, 2013 Stephen Leake
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

with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Trivial_Productions_Test is

   Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   ----------
   --  Test procedures

   procedure Expression (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      type Token_IDs is
        (
         --  Terminals
         Symbol_ID, EOF_ID,

         --  Nonterminals
         E_ID, F_ID, T_ID);

      package Tokens is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
      package Analyzers is new Tokens.Analyzer (EOF_ID);
      package Token_Lists is new Tokens.List;
      package Nonterminals is new Tokens.Nonterminal (Token_Lists);
      package Productions is new OpenToken.Production (Tokens, Token_Lists, Nonterminals);
      package Production_Lists is new Productions.List;
      package Parsers is new Productions.Parser (Production_Lists, Analyzers);
      package LALR_Parsers is new Parsers.LALR;

      EOF    : constant Tokens.Class       := Tokens.Get (EOF_ID);
      Symbol : constant Tokens.Class       := Tokens.Get (Symbol_ID);
      E      : constant Nonterminals.Class := Nonterminals.Get (E_ID);
      F      : constant Nonterminals.Class := Nonterminals.Get (F_ID);
      T      : constant Nonterminals.Class := Nonterminals.Get (T_ID);

      Syntax : constant Analyzers.Syntax :=
        (EOF_ID    => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get, EOF),
         Symbol_ID => Analyzers.Get (OpenToken.Recognizer.Keyword.Get ("symbol"), Symbol));

      Analyzer : Analyzers.Instance := Analyzers.Initialize (Syntax, Feeder'Access);

      --  Allow infix operators for building productions
      use type Token_Lists.Instance;
      use type Productions.Right_Hand_Side;
      use type Productions.Instance;
      use type Production_Lists.Instance;

      --  The grammar is:
      --
      --  E -> T      expression -> term
      --  T -> F      term -> factor
      --  F -> symbol

      Grammar : constant Production_Lists.Instance :=
        E <= T & EOF + -- OpenToken requires an explicit EOF
          Nonterminals.Synthesize_Self and
        T <= F + Nonterminals.Synthesize_Self and
        F <= Symbol + Nonterminals.Synthesize_Self;

      Parser : LALR_Parsers.Instance;

      Text : constant String := "symbol";
   begin
      --  The test is that there are no exceptions raised, either during grammar construction or parsing

      Parser := LALR_Parsers.Generate
        (Grammar, Analyzer,
         Non_Reporting_Tokens => (EOF_ID => True, others => False),
         Trace                => Test_Case (Test).Debug);

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Analyzer.Reset;

      Parser.Parse;

   end Expression;

   procedure Subprograms (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  Hand-written OpenToken grammar matching
      --  ../wisi/test/subprograms.wy, to show that wisi-generate
      --  produces the same grammar.

      type Token_IDs is
        (
         --  Terminals
         Function_ID,
         Procedure_ID,
         Symbol_ID,
         Left_Paren_ID,
         Right_Paren_ID,
         Whitespace_ID,
         EOF_ID,

         --  Nonterminals
         OpenToken_Accept_ID,
         Declarations_ID,
         Declaration_ID,
         Subprogram_ID,
         Parameter_List_ID);

      package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
      package Analyzers is new Tokens_Pkg.Analyzer (EOF_ID);
      package Token_Lists is new Tokens_Pkg.List;
      package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
      package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
      package Production_Lists is new Productions.List;
      package Parsers is new Productions.Parser (Production_Lists, Analyzers);
      package LALR_Parsers is new Parsers.LALR;

      EOF            : constant Tokens_Pkg.Class := Tokens_Pkg.Get (EOF_ID);
      Function_Tok   : constant Tokens_Pkg.Class := Tokens_Pkg.Get (Function_ID);
      Left_Paren     : constant Tokens_Pkg.Class := Tokens_Pkg.Get (Left_Paren_ID);
      Procedure_Tok  : constant Tokens_Pkg.Class := Tokens_Pkg.Get (Procedure_ID);
      Right_Paren    : constant Tokens_Pkg.Class := Tokens_Pkg.Get (Right_Paren_ID);
      Symbol         : constant Tokens_Pkg.Class := Tokens_Pkg.Get (Symbol_ID);

      OpenToken_Accept : constant Nonterminals.Class := Nonterminals.Get (OpenToken_Accept_ID);
      Declarations     : constant Nonterminals.Class := Nonterminals.Get (Declarations_ID);
      Declaration      : constant Nonterminals.Class := Nonterminals.Get (Declaration_ID);
      Parameter_List   : constant Nonterminals.Class := Nonterminals.Get (Parameter_List_ID);
      Subprogram       : constant Nonterminals.Class := Nonterminals.Get (Subprogram_ID);

      Syntax : constant Analyzers.Syntax :=
        (EOF_ID         => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get, EOF),
         Function_ID    => Analyzers.Get (OpenToken.Recognizer.Keyword.Get ("function"), Function_Tok),
         Left_Paren_ID  => Analyzers.Get (OpenToken.Recognizer.Keyword.Get ("("), Function_Tok),
         Procedure_ID   => Analyzers.Get (OpenToken.Recognizer.Keyword.Get ("procedure"), Procedure_Tok),
         Right_Paren_ID => Analyzers.Get (OpenToken.Recognizer.Keyword.Get (")"), Function_Tok),
         Symbol_ID      => Analyzers.Get (OpenToken.Recognizer.Keyword.Get ("symbol"), Symbol),
         Whitespace_ID  => Analyzers.Get (OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

      Analyzer : Analyzers.Instance := Analyzers.Initialize (Syntax, Feeder'Access);

      --  Allow infix operators for building productions
      use type Token_Lists.Instance;
      use type Productions.Right_Hand_Side;
      use type Productions.Instance;
      use type Production_Lists.Instance;

      Grammar : constant Production_Lists.Instance :=
        OpenToken_Accept <= Declarations & EOF + Nonterminals.Synthesize_Self and
        Declarations     <= Declaration + Nonterminals.Synthesize_Self and
        Declarations     <= Declarations & Declaration + Nonterminals.Synthesize_Self and
        Declaration      <= Subprogram + Nonterminals.Synthesize_Self and
        Subprogram       <= Function_Tok & Parameter_List & Symbol + Nonterminals.Synthesize_Self and
        Subprogram       <= Procedure_Tok & Parameter_List + Nonterminals.Synthesize_Self and
        Parameter_List   <= +Nonterminals.Synthesize_Self and
        Parameter_List   <= Left_Paren & Symbol & Right_Paren + Nonterminals.Synthesize_Self;

      Parser : LALR_Parsers.Instance;

      Text : constant String := "function (symbol) symbol procedure";
   begin
      --  The test is that there are no exceptions raised, either during grammar construction or parsing

      --  FIXME: 'expecting' is wrong if leave out '('

      Parser := LALR_Parsers.Generate
        (Grammar, Analyzer,
         Non_Reporting_Tokens => (EOF_ID | Whitespace_ID => True, others => False),
         Trace                => Test_Case (Test).Debug,
         Put_Grammar          => Test_Case (Test).Debug);

      OpenToken.Text_Feeder.String.Set (Feeder, Text);
      Analyzer.Reset;

      Parser.Parse;

   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Expression'Access, "Expression");
      Register_Routine (T, Subprograms'Access, "Subprograms");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/trivial_productions_test.adb");
   end Name;

end Trivial_Productions_Test;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
