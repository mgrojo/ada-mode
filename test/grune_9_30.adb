--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
with FastToken.Lexer.Regexp;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Grune_9_30 is

   type Token_ID is
     (
      --  terminals
      Lower_A_ID,
      Lower_B_ID,
      Lower_C_ID,
      EOF_ID,

      --  non-terminals
      Upper_S_ID,
      Upper_A_ID,
      Upper_B_ID
     );

   package Token_Pkg is new FastToken.Token (Token_ID, Lower_A_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, Upper_S_ID, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package LR1_Generator is new LR.LR1_Generator (Production, LR1_Items, Generator_Utils);

   use all type Production.Instance;
   use all type Production.List.Instance;
   use all type Production.Right_Hand_Side;
   use all type Token_Pkg.List.Instance;

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Upper_S_ID <= Upper_A_ID & Upper_B_ID & Lower_C_ID & EOF_ID + Self -- 1
     and
     Upper_A_ID <= Lower_A_ID + Self                           -- 2
     and
     Upper_B_ID <= Lower_B_ID + Self                           -- 3
     and
     Upper_B_ID <= +Self                                       -- 4
   ;

   package Lexer is new Lexer_Root.Regexp;
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package LR_Parser is new LR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Syntax : constant Lexer.Syntax :=
     (
      Lower_A_ID => Lexer.Get ("a", +Lower_A_ID),
      Lower_B_ID => Lexer.Get ("b", +Lower_B_ID),
      Lower_C_ID => Lexer.Get ("c", +Lower_C_ID),
      EOF_ID     => Lexer.Get ("" & FastToken.EOF_Character, +EOF_ID)
     );

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, Lower_A_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, 1, LR, LR1_Items, Grammar);

   Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set :=
     LR1_Items.Has_Empty_Production (Grammar);

   First : constant LR1_Items.Derivation_Matrix := LR1_Items.First
     (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Test_Item_Sets (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Text_IO;
      use FastToken_AUnit;
      use LR1_Items;

      Computed : Item_Set_List := LR1_Generator.LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, Trace => Test.Debug);

      Expected : Item_Set_List :=
        --  Item sets from [Grune] fig 9.31 a. States are numbered as in
        --  [Grune], but added to the list in the order we compute.
        --
        --  Our Item_Sets also include the gotos.
        (1 + (Get_Item (1, 1, +EOF_ID) &
                Get_Item (2, 1, +(Lower_B_ID, Lower_C_ID)))) &
        (2 + Get_Item (2, 2, +(Lower_B_ID, Lower_C_ID))) &
        (3 + (Get_Item (1, 2, +EOF_ID) &
                Get_Item (3, 1, +Lower_C_ID) &
                Get_Item (4, 1, +Lower_C_ID))) &
        (4 + Get_Item (3, 2, +Lower_C_ID)) &
        (5 + Get_Item (1, 3, +EOF_ID)) &
        (6 + Get_Item (1, 4, +EOF_ID));

   begin
      Add_Gotos (Expected, 1, +(Lower_A_ID, Get_Set (2, Expected)) & (Upper_A_ID, Get_Set (3, Expected)));
      --  no gotos from state 2
      Add_Gotos (Expected, 3, +(Lower_B_ID, Get_Set (4, Expected)) & (Upper_B_ID, Get_Set (5, Expected)));
      --  no gotos from state 4
      Add_Gotos (Expected, 5, +(Lower_C_ID, Get_Set (6, Expected)));

      if Test.Debug then
         Put_Line ("computed:");
         Put (Computed);
         Put_Line ("expected:");
         Put (Expected);
      end if;

      Check ("", Computed, Expected);

      Free (Computed);
      Free (Expected);
   end Test_Item_Sets;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Parser : LR_Parser.Instance := LR_Parser.Initialize
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         LR1_Generator.Generate (Grammar, Trace => Test.Debug));

      procedure Execute_Command (Command : in String)
      is begin
         String_Feeder.Set (Command);

         Parser.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

         Parser.Parse;
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Command;

   begin
      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute_Command ("abc");
      Execute_Command ("ac");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/grune_9_30.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_Parse'Access, "Debug");
      else
         Register_Routine (T, Test_Item_Sets'Access, "Test_Item_Sets");
         Register_Routine (T, Test_Parse'Access, "Test_Parse");
      end if;
   end Register_Tests;

end Grune_9_30;
--  Local_Variables:
--  eval: (ada-indent-opentoken-mode)
--  End:
