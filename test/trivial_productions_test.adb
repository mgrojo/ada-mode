--  Abstract:
--
--  See spec
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 - 2020 Stephen Leake
--  Copyright (C) 2000 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Characters.Latin_1;
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.Productions;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
with WisiToken.Text_IO_Trace;
package body Trivial_Productions_Test is

   ----------
   --  Test procedures

   package Expression is
      procedure Test_One (Test : in out AUnit.Test_Cases.Test_Case'Class);
   end Expression;

   package body Expression is

      type Token_Enum_ID is
        (
         --  Terminals
         Symbol_ID, EOF_ID,

         --  Nonterminals
         E_ID, F_ID, T_ID);

      package Token_Enum is new WisiToken.Gen_Token_Enum
        (Token_Enum_ID     => Token_Enum_ID,
         First_Terminal    => Symbol_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => E_ID,
         Last_Nonterminal  => T_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => E_ID,
         Case_Insensitive  => False);
      use Token_Enum;

      Trace : aliased WisiToken.Text_IO_Trace.Trace;

      procedure Test_One (Test : in out AUnit.Test_Cases.Test_Case'Class)
      is
         pragma Unreferenced (Test);

         package Lexer renames WisiToken.Lexer.Regexp;

         Syntax : constant Lexer.Syntax := To_Syntax
           ((EOF_ID    => Lexer.Get ("" & Ada.Characters.Latin_1.EOT),
             Symbol_ID => Lexer.Get ("symbol")));

         Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

         Grammar : WisiToken.Productions.Prod_Arrays.Vector :=
           E_ID <= T_ID & EOF_ID + Null_Action and
           T_ID <= F_ID + Null_Action and
           F_ID <= Symbol_ID + Null_Action;

         Parser : WisiToken.Parse.LR.Parser.Parser (LR1_Descriptor'Access);

         Text : constant String := "symbol";
      begin
         --  The test is that there are no exceptions raised, either during grammar construction or parsing

         WisiToken.Parse.LR.Parser.New_Parser
           (Parser,
            Trace'Access,
            Lexer.New_Lexer (Parser.Descriptor, Syntax),
            WisiToken.Generate.LR.LALR_Generate.Generate (Grammar, LALR_Descriptor, Grammar_File_Name => ""),
            User_Data                      => null,
            Language_Fixes                 => null,
            Language_Matching_Begin_Tokens => null,
            Language_String_ID_Set         => null);

         Parser.Lexer.Reset_With_String (Text);

         Parser.Parse;

      end Test_One;
   end Expression;

   package Subprograms is
      procedure Test_One (T : in out AUnit.Test_Cases.Test_Case'Class);
   end Subprograms;
   package body Subprograms is
      --  Hand-written WisiToken grammar matching
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
         WisiToken_Accept_ID,
         Declarations_ID,
         Declaration_ID,
         Subprogram_ID,
         Parameter_List_ID);

      package Token_Enum is new WisiToken.Gen_Token_Enum
        (Token_Enum_ID     => Token_ID,
         First_Terminal    => Function_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => WisiToken_Accept_ID,
         Last_Nonterminal  => Parameter_List_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => WisiToken_Accept_ID,
         Case_Insensitive  => False);
      use Token_Enum;

      Trace : aliased WisiToken.Text_IO_Trace.Trace;

      procedure Test_One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         pragma Unreferenced (T);

         package Lexer renames WisiToken.Lexer.Regexp;

         Syntax : constant Lexer.Syntax := To_Syntax
           ((
             Whitespace_ID  => Lexer.Get (" ", Report => False),
             Function_ID    => Lexer.Get ("function"),
             Left_Paren_ID  => Lexer.Get ("\("),
             Procedure_ID   => Lexer.Get ("procedure"),
             Right_Paren_ID => Lexer.Get ("\)"),
             Symbol_ID      => Lexer.Get ("symbol"),
             EOF_ID         => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
            ));

         Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

         Grammar : WisiToken.Productions.Prod_Arrays.Vector :=
           WisiToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and
           (Declarations_ID    <= Declaration_ID + Null_Action or
                                  Declarations_ID & Declaration_ID + Null_Action) and
           Declaration_ID      <= Subprogram_ID + Null_Action and
           Subprogram_ID       <= Function_ID & Parameter_List_ID & Symbol_ID + Null_Action and
           Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and
           (Parameter_List_ID  <= +Null_Action or
                                  Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action);

         Parser : WisiToken.Parse.LR.Parser.Parser (LR1_Descriptor'Access);

         Text : constant String := "function (symbol) symbol procedure";
      begin
         --  The test is that there are no exceptions raised, either during grammar construction or parsing

         WisiToken.Parse.LR.Parser.New_Parser
           (Parser,
            Trace'Access,
            Lexer.New_Lexer (Parser.Descriptor, Syntax),
            WisiToken.Generate.LR.LALR_Generate.Generate (Grammar, LALR_Descriptor, Grammar_File_Name => ""),
            User_Data                      => null,
            Language_Fixes                 => null,
            Language_Matching_Begin_Tokens => null,
            Language_String_ID_Set         => null);

         Parser.Lexer.Reset_With_String (Text);
         Parser.Parse;

      end Test_One;
   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Expression.Test_One'Access, "Expression");
      Register_Routine (T, Subprograms.Test_One'Access, "Subprograms");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("trivial_productions_test.adb");
   end Name;

end Trivial_Productions_Test;
