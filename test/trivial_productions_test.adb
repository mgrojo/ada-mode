--  Abstract:
--
--  See spec
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 Stephen Leake
--  Copyright (C) 2000 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO;
with FastToken.Lexer.Regexp;
with FastToken.Production;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Text_Feeder.String;
with FastToken.Token;
package body Trivial_Productions_Test is

   Feeder : aliased FastToken.Text_Feeder.String.Instance;

   ----------
   --  Test procedures

   procedure Expression (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      type Token_ID is
        (
         --  Terminals
         Symbol_ID, EOF_ID,

         --  Nonterminals
         E_ID, F_ID, T_ID);

      package Token_Pkg is new FastToken.Token (Token_ID, Symbol_ID, EOF_ID, Token_ID'Image);
      package Production is new FastToken.Production (Token_Pkg);
      package Lexer_Root is new FastToken.Lexer (Token_Pkg);
      package Lexer is new Lexer_Root.Regexp;
      package Parser_Root is new FastToken.Parser
        (Token_ID, Symbol_ID, EOF_ID, EOF_ID, E_ID, Token_ID'Image, Ada.Text_IO.Put, Token_Pkg, Lexer_Root);
      First_State_Index : constant := 1;
      package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width);
      package LR1_Items is new Parser_Root.LR1_Items
        (LR.Unknown_State_Index, LR.Unknown_State, Production);
      package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
      package LALR_Generator is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

      Syntax : constant Lexer.Syntax :=
        (EOF_ID    => Lexer.Get ("" & FastToken.EOF_Character),
         Symbol_ID => Lexer.Get ("symbol"));

      use type Token_Pkg.List.Instance;
      use type Production.Right_Hand_Side;
      use type Production.Instance;
      use type Production.List.Instance;

      Grammar : constant Production.List.Instance :=
        E_ID <= T_ID & EOF_ID + Token_Pkg.Null_Action and
        T_ID <= F_ID + Token_Pkg.Null_Action and
        F_ID <= Symbol_ID + Token_Pkg.Null_Action;

      First_Parser_Label : constant := 1;
      package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
      package Panic_Mode is new LR.Panic_Mode (First_Parser_Label, Parser_Lists => Parser_Lists);
      package LR_Parser is new LR.Parser
        (First_Parser_Label, Parser_Lists => Parser_Lists, Panic_Mode => Panic_Mode);
      Parser : LR_Parser.Instance;

      Text : constant String := "symbol";
   begin
      --  The test is that there are no exceptions raised, either during grammar construction or parsing

      Parser := LR_Parser.Initialize
        (Lexer.Initialize (Syntax, Feeder'Access, Buffer_Size => Text'Length + 1),
         LALR_Generator.Generate (Grammar, Trace => Test_Case (Test).Debug));

      Feeder.Set (Text);

      Parser.Parse;

   end Expression;

   procedure Subprograms (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      --  Hand-written FastToken grammar matching
      --  ../wisi/test/subprograms.wy, to show that wisi-generate
      --  produces the same grammar.

      type Token_ID is
        (Whitespace_ID,

         --  Terminals
         Function_ID,
         Procedure_ID,
         Symbol_ID,
         Left_Paren_ID,
         Right_Paren_ID,
         EOF_ID,

         --  Nonterminal
         FastToken_Accept_ID,
         Declarations_ID,
         Declaration_ID,
         Subprogram_ID,
         Parameter_List_ID);

      package Token_Pkg is new FastToken.Token (Token_ID, Function_ID, EOF_ID, Token_ID'Image);
      package Production is new FastToken.Production (Token_Pkg);
      package Lexer_Root is new FastToken.Lexer (Token_Pkg);
      package Lexer is new Lexer_Root.Regexp;
      package Parser_Root is new FastToken.Parser
        (Token_ID, Function_ID, EOF_ID, EOF_ID, FastToken_Accept_ID, Token_ID'Image, Ada.Text_IO.Put,
         Token_Pkg, Lexer_Root);
      First_State_Index : constant := 1;
      package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width);
      First_Parser_Label : constant := 1;
      package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
      package Panic_Mode is new LR.Panic_Mode (First_Parser_Label, Parser_Lists => Parser_Lists);
      package LR_Parser is new LR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists, Panic_Mode => Panic_Mode);
      package LR1_Items is new Parser_Root.LR1_Items
        (LR.Unknown_State_Index, LR.Unknown_State, Production);
      package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
      package LALR_Generator is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

      Syntax : constant Lexer.Syntax :=
        (
         Whitespace_ID  => Lexer.Get (" ", Report => False),
         Function_ID    => Lexer.Get ("function"),
         Left_Paren_ID  => Lexer.Get ("\("),
         Procedure_ID   => Lexer.Get ("procedure"),
         Right_Paren_ID => Lexer.Get ("\)"),
         Symbol_ID      => Lexer.Get ("symbol"),
         EOF_ID         => Lexer.Get ("" & FastToken.EOF_Character)
        );

      use type Token_Pkg.List.Instance;
      use type Production.Right_Hand_Side;
      use type Production.Instance;
      use type Production.List.Instance;

      Null_Action : Token_Pkg.Semantic_Action renames Token_Pkg.Null_Action;

      Grammar : constant Production.List.Instance :=
        FastToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and
        Declarations_ID     <= Declaration_ID + Null_Action and
        Declarations_ID     <= Declarations_ID & Declaration_ID + Null_Action and
        Declaration_ID      <= Subprogram_ID + Null_Action and
        Subprogram_ID       <= Function_ID & Parameter_List_ID & Symbol_ID + Null_Action and
        Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and
        Parameter_List_ID   <= +Null_Action and
        Parameter_List_ID   <= Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action;

      Parser : LR_Parser.Instance;

      Text : constant String := "function (symbol) symbol procedure";
   begin
      --  The test is that there are no exceptions raised, either during grammar construction or parsing

      Parser := LR_Parser.Initialize
        (Lexer.Initialize (Syntax, Feeder'Access, Buffer_Size => Text'Length + 1),
         LALR_Generator.Generate
           (Grammar,
            Trace       => Test.Debug,
            Put_Parse_Table => Test.Debug));

      Feeder.Set (Text);
      FastToken.Trace_Parse := (if Test.Debug then 1 else 0);
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
