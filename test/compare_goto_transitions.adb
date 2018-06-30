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

with AUnit.Assertions;
with Ada.Text_IO;
with WisiToken.Generate;
with WisiToken.Gen_Token_Enum;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.LR1_Generator;
with WisiToken.LR.LR1_Items;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Ada;
with WisiToken.LR.LR1_Items.AUnit;
package body Compare_Goto_Transitions is

   ----------
   --  Test procedures

   package Subprograms is
      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class);
   end Subprograms;
   package body Subprograms is

      type Token_Enum_ID is
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
        (Token_Enum_ID     => Token_Enum_ID,
         First_Terminal    => Procedure_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => WisiToken_Accept_ID,
         Last_Nonterminal  => Parameter_List_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => WisiToken_Accept_ID,
         Case_Insensitive  => False);
      use Token_Enum;
      use WisiToken.Wisi_Ada;

      Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

      --  This grammar has an empty production (number 6); test that
      --  Closure and Goto_Transitions handle it properly.
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
        (Grammar, Token_Enum.LALR_Descriptor, Has_Empty_Production, Trace => False);

      procedure Compare (Prod : in WisiToken.Production_ID; Symbol : in Token_Enum_ID)
      is
         use Ada.Text_IO;
         use WisiToken;
         use WisiToken.LR.LR1_Items;

         Set : constant Item_Set := Closure
           ((Set      => WisiToken.LR.LR1_Items.AUnit.Get_Item_Node
               (Grammar, Prod, 1, WisiToken.To_Lookahead (+Symbol, Token_Enum.LALR_Descriptor)),
             Goto_List => null,
             State     => WisiToken.Unknown_State,
             Next      => null),
           Has_Empty_Production, First, Grammar, Token_Enum.LALR_Descriptor,
           Trace      => False);

         LR1           : Item_Set;
         LR1_Filtered  : Item_Set;
         LALR          : Item_Set;
         Mismatch      : Boolean := False;
      begin
         for ID in Token_Enum_ID loop
            declare
               Label : constant String :=
                 Trimmed_Image (Prod) & "." &
                 Token_Enum_ID'Image (Symbol) & "." &
                 Token_Enum_ID'Image (ID);
            begin
               LR1 := WisiToken.LR.LR1_Generator.LR1_Goto_Transitions
                    (Set, +ID, Has_Empty_Production, First, Grammar, LR1_Descriptor, Trace => False);
               LR1_Filtered := Filter (LR1, Grammar, LR1_Descriptor, In_Kernel'Access);
               Free (LR1);

               LALR := WisiToken.LR.LALR_Generator.LALR_Goto_Transitions
                 (Set, +ID, First, Grammar, Token_Enum.LALR_Descriptor);

               WisiToken.LR.LR1_Items.AUnit.Check (Label, LR1_Filtered, LALR, Match_Lookaheads => False);
               Free (LR1_Filtered);
               Free (LALR);
            exception
            when AUnit.Assertions.Assertion_Error =>
               Mismatch := True;
               if Trace_Action > Outline then
                  Put_Line (Label & " mismatch");
                  Put_Line ("  lr1:");
                  Put (Grammar, LR1_Descriptor, LR1_Filtered, Show_Goto_List => True);
                  New_Line;
                  Put_Line ("  lalr:");
                  Put (Grammar, Token_Enum.LALR_Descriptor, LALR, Show_Goto_List => True);
                  New_Line;
                  Free (LR1_Filtered);
                  Free (LALR);
               else
                  Free (LR1_Filtered);
                  Free (LALR);
                  raise;
               end if;
            end;
         end loop;
         if Trace_Action > Outline and Mismatch then
            Put_Line (Trimmed_Image (Prod) & "." & Token_Enum_ID'Image (Symbol) & ".closure");
            Put (Grammar, LR1_Descriptor, Set);
            New_Line;
         end if;

      end Compare;

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         pragma Unreferenced (T);
      begin
         for Nonterm in Grammar.First_Index .. Grammar.Last_Index loop
            for RHS in Grammar (Nonterm).RHSs.First_Index .. Grammar (Nonterm).RHSs.Last_Index loop
               for Symbol in LALR_Descriptor.First_Terminal .. LALR_Descriptor.Last_Terminal loop
                  Compare ((Nonterm, RHS), -Symbol);
               end loop;
            end loop;
         end loop;
      end One;

   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Subprograms.One'Access, "Subprograms");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("compare_goto_transitions.adb");
   end Name;

end Compare_Goto_Transitions;
