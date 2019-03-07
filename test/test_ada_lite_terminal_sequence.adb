--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks.Containers;
with Ada.Containers;
with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.LR.AUnit;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items;
with WisiToken.Parse.LR.AUnit;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
package body Test_Ada_Lite_Terminal_Sequence is

   Input_File_Name  : constant String := "../Test/bnf/ada_lite.wy";

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;
   Generate_Data  : access WisiToken.BNF.Generate_Utils.Generate_Data;

   Save_Trace_Generate : Integer := 0;

   function "+" (Token_Name : in String) return WisiToken.Token_ID
   is begin
      return WisiToken.BNF.Generate_Utils.Find_Token_ID (Generate_Data.all, Token_Name);
   end "+";

   function To_List
     (Item        : in WisiToken.Token_ID;
      Token_Count : in Ada.Containers.Count_Type := 0)
     return WisiToken.Parse.LR.Minimal_Action_Lists.List
   is
      use all type WisiToken.Token_ID;
      use WisiToken.Parse.LR;
   begin
      if Item < Generate_Data.Descriptor.First_Nonterminal then
         return WisiToken.Parse.LR.Minimal_Action_Lists.To_List
           ((Verb => Shift, ID => Item, State => WisiToken.State_Index'Last));
      else
         return WisiToken.Parse.LR.Minimal_Action_Lists.To_List
           ((Verb => Reduce, Nonterm => Item, Token_Count => Token_Count));
      end if;
   end To_List;

   ----------
   --  Test procedures

   procedure Test_Terminal_Sequence (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken;
      use WisiToken.AUnit;
      use WisiToken.Generate.LR;
      use WisiToken.Generate.LR.AUnit.Minimal_RHS_Arrays_AUnit;
      use WisiToken.Generate.LR.Minimal_RHS_Arrays;
      use WisiToken.Token_ID_Arrays;
      use AUnit.Checks.Containers;
      use WisiToken.BNF.Generate_Utils;
   begin
      Trace_Generate := Save_Trace_Generate;
      declare
         Computed : constant Minimal_Sequence_Array :=
           Compute_Minimal_Terminal_Sequences
             (Generate_Data.Descriptor.all, Generate_Data.Grammar);
      begin
         if Trace_Generate > Detail then
            Ada.Text_IO.New_Line;
            for I in Computed'Range loop
               Ada.Text_IO.Put_Line
                 (Image (I, Generate_Data.Descriptor.all) & " => " &
                    Image (Computed (I), Generate_Data.Descriptor.all));
            end loop;
         end if;

         --  We only check a couple things; the main test is that this runs in
         --  a reasonable time, and there are no exceptions.
         Check ("first", Computed'First, Find_Token_ID (Generate_Data.all, "wisitoken_accept"));
         Check ("last", Computed'Last, Find_Token_ID (Generate_Data.all, "unary_adding_operator"));

         Check ("empty 1", Min (Computed (+"aspect_specification_opt")).Length, 0);

         Check
           ("assignment_statement",
            Computed (+"assignment_statement"),
            Minimal_RHS_Arrays.To_Vector (+"IDENTIFIER" & (+"COLON_EQUAL") & (+"SEMICOLON")));

         Check
           ("if_statement",
            Computed (+"if_statement"),
            (+"IF" & (+"THEN") & (+"ELSIF") & (+"THEN") & (+"ELSE") & (+"END") & (+"IF") & (+"SEMICOLON")) &
              (+"IF" & (+"THEN") & (+"ELSE") & (+"END") & (+"IF") & (+"SEMICOLON")) &
              (+"IF" & (+"THEN") & (+"ELSIF") & (+"THEN") & (+"END") & (+"IF") & (+"SEMICOLON")) &
              (+"IF" & (+"THEN") & (+"END") & (+"IF") & (+"SEMICOLON")));

         Check
           ("name",
            Computed (+"name"),
            ((+"IDENTIFIER") & (+"LEFT_PAREN") & (+"NUMERIC_LITERAL") & (+"DOT_DOT") & (+"NUMERIC_LITERAL") &
               (+"RIGHT_PAREN")) &
              ((+"IDENTIFIER") & (+"LEFT_PAREN") & (+"RIGHT_PAREN")) &
              To_Vector (+"IDENTIFIER") &
              ((+"IDENTIFIER") & (+"DOT") & (+"IDENTIFIER")));
      end;
   end Test_Terminal_Sequence;

   procedure Test_Minimal_Complete_Actions (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken;
      use WisiToken.Generate.LR;
      use WisiToken.Parse.LR.AUnit.Minimal_Action_Lists_AUnit;

      Has_Empty_Production : constant Token_ID_Set := Generate.Has_Empty_Production (Generate_Data.Grammar);

      First_Nonterm_Set : constant Token_Array_Token_Set := Generate.First
        (Generate_Data.Grammar, Has_Empty_Production, Generate_Data.Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Generate_Data.Descriptor.all);

      Item_Sets : constant Generate.LR1_Items.Item_Set_List := Generate.LR.LR1_Generate.LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Generate_Data.Grammar, Generate_Data.Descriptor.all);

      Unknown_Conflicts : Generate.LR.Conflict_Lists.List;

      Table : Parse.LR.Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
         First_Terminal    => Generate_Data.Descriptor.First_Terminal,
         Last_Terminal     => Generate_Data.Descriptor.Last_Terminal,
         First_Nonterminal => Generate_Data.Descriptor.First_Nonterminal,
         Last_Nonterminal  => Generate_Data.Descriptor.Last_Nonterminal);

      Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
        Compute_Minimal_Terminal_Sequences (Generate_Data.Descriptor.all, Generate_Data.Grammar);

      Minimal_Terminal_First : constant Token_Array_Token_ID := Generate.LR.Compute_Minimal_Terminal_First
          (Generate_Data.Descriptor.all, Minimal_Terminal_Sequences);

      function Build_Dot
        (Grammar   : in Productions.Prod_Arrays.Vector;
         Prod_ID   : in Production_ID;
         Dot_After : in Natural)
        return Token_ID_Arrays.Cursor
      is
         Tokens : Token_ID_Arrays.Vector renames Grammar (Prod_ID.LHS).RHSs (Prod_ID.RHS).Tokens;
      begin
         return Tokens.To_Cursor (Dot_After + 1);
      end Build_Dot;

      procedure Check
        (Label     : in String;
         Prod_ID   : in Production_ID;
         Dot_After : in Natural;
         Expected  : in WisiToken.Parse.LR.Minimal_Action_Lists.List)
      is
         State : Unknown_State_Index := Unknown_State;

         procedure Find_State (Prod_ID : in Production_ID; Dot_After : in Natural)
         is
            use all type Generate.LR1_Items.Item_Lists.Cursor;
         begin
            for I in Item_Sets.First_Index .. Item_Sets.Last_Index loop
               if Generate.LR1_Items.Item_Lists.No_Element /= Generate.LR1_Items.Find
                 (Prod_ID, Build_Dot (Generate_Data.Grammar, Prod_ID, Dot_After), Item_Sets (I))
               then
                  State := I;
                  exit;
               end if;
            end loop;
         end Find_State;

      begin
         Find_State (Prod_ID, Dot_After);
         if Trace_Generate > Detail then
            Ada.Text_IO.Put_Line ("state" & Unknown_State_Index'Image (State));
         end if;

         Set_Minimal_Complete_Actions
           (Table.States (State),
            Generate.LR1_Items.Filter
              (Item_Sets (State), Generate_Data.Grammar, Generate_Data.Descriptor.all,
               Generate.LR1_Items.In_Kernel'Access),
            Generate_Data.Descriptor.all, Generate_Data.Grammar,
            Minimal_Terminal_Sequences, Minimal_Terminal_First);
         Check (Label, Table.States (State).Minimal_Complete_Actions, Expected);

      end Check;

   begin
      Generate.LR.LR1_Generate.Add_Actions
        (Item_Sets, Generate_Data.Grammar, Has_Empty_Production, First_Nonterm_Set, Unknown_Conflicts, Table,
         Generate_Data.Descriptor.all);

      Trace_Generate := Save_Trace_Generate;

      if Trace_Generate > Extra then
         for Nonterm in Minimal_Terminal_Sequences'Range loop
            Ada.Text_IO.Put_Line
              (Image (Nonterm, Generate_Data.Descriptor.all) & " => " &
                 Image (Minimal_Terminal_Sequences (Nonterm), Generate_Data.Descriptor.all));
         end loop;
      end if;

      Check ("if 4", Prod_ID => (+"if_statement", 3), Dot_After => 4, Expected => To_List (+"END"));
      Check ("case 1", Prod_ID => (+"case_statement", 0), Dot_After => 1, Expected => To_List (+"expression_opt"));
      Check ("body_stub 1", Prod_ID => (+"body_stub", 0), Dot_After => 1, Expected => To_List (+"body_stub", 1));
      Check ("body_g 1", Prod_ID => (+"body_g", 1), Dot_After => 1, Expected => To_List (+"body_g", 1));
   end Test_Minimal_Complete_Actions;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Terminal_Sequence'Access, "Test_Terminal_Sequence");
      Register_Routine (T, Test_Minimal_Complete_Actions'Access, "Test_Minimal_Complete_Actions");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_ada_lite_terminal_sequence.adb");
   end Name;

   overriding procedure Set_Up_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      Save_Trace_Generate      := WisiToken.Trace_Generate;
      WisiToken.Trace_Generate := 0; -- Only trace the part we are interested in.

      Wisitoken_Grammar_Main.Create_Parser (Grammar_Parser, Trace'Unchecked_Access, Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (Input_File_Name);
      Grammar_Parser.Parse;
      Input_Data.User_Parser := WisiToken.BNF.LALR;
      Grammar_Parser.Execute_Actions;
      Generate_Data := new WisiToken.BNF.Generate_Utils.Generate_Data'
        (WisiToken.BNF.Generate_Utils.Initialize (Input_Data));

   end Set_Up_Case;

end Test_Ada_Lite_Terminal_Sequence;
