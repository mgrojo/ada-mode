--  Abstract:
--
--  See spec
--
--  Copyright (C) 2017, 2018 Stephen Leake
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

with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate;
with WisiToken.LR.LR1_Items;
with WisiToken.LR;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
package body Test_Follow is

   package Subprograms is
      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class);
   end Subprograms;
   package body Subprograms is

      type Token_ID is
        (
         --  Terminals
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
         First_Terminal    => Procedure_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => WisiToken_Accept_ID,
         Last_Nonterminal  => Parameter_List_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => WisiToken_Accept_ID,
         Case_Insensitive  => False);
      use Token_Enum;

      Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

      --  This grammar has right recursion on Declarations_ID, and an
      --  empty production for Parameter_List_ID
      Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
        WisiToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and                -- 1
        (Declarations_ID    <= Declaration_ID + Null_Action or                           -- 2
                               Declarations_ID & Declaration_ID + Null_Action) and       -- 3
        Declaration_ID      <= Subprogram_ID + Null_Action and                           -- 4
        Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and        -- 5
        (Parameter_List_ID  <= +Null_Action or                                           -- 6
                               Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action); -- 7

      Has_Empty_Production : constant WisiToken.Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);
      First                : constant WisiToken.Token_Array_Token_Set := WisiToken.LR.LR1_Items.First
        (Grammar, LALR_Descriptor, Has_Empty_Production, Trace => False);

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         Test : Test_Case renames Test_Case (T);

         use Ada.Text_IO;
         use WisiToken.LR.LR1_Items;

         Computed : constant WisiToken.Token_Array_Token_Set :=
           Follow (Grammar, LALR_Descriptor, First, Has_Empty_Production);

         Expected : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Terminal_Set
           ((WisiToken_Accept_ID => (others => False),
             Declarations_ID     => (EOF_ID | Procedure_ID => True, others => False),
             Declaration_ID      => (EOF_ID | Procedure_ID => True, others => False),
             Subprogram_ID       => (EOF_ID | Procedure_ID => True, others => False),
             Parameter_List_ID   => (EOF_ID | Procedure_ID => True, others => False)));

      begin
         if Test.Debug then
            Put_Line ("Computed: ");
            WisiToken.Put (LALR_Descriptor, Computed);
            New_Line;
            Put_Line ("Expected:");
            WisiToken.Put (LALR_Descriptor, Expected);
         end if;
         WisiToken.AUnit.Check ("1", Computed, Expected);
      end One;

   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Subprograms.One'Access, "debug");
      else
         Register_Routine (T, Subprograms.One'Access, "Subprograms.One");
      end if;
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_follow.adb");
   end Name;

end Test_Follow;
