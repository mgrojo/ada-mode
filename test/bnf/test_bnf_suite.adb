--  Abstract :
--
--  Build AUnit test suite containing tests that run wisitoken-bnf-generate
--
--  Copyright (C) 2013-2015, 2017 - 2022 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Strings.Unbounded;
with BNF_WY_Errors_Test;
with BNF_WY_Test;
with Test_Generate_Errors;
with Test_LR1_Parallel;
with WisiToken.BNF;
function Test_BNF_Suite return Access_Test_Suite
is
   use all type WisiToken.BNF.Generate_Algorithm;

   function "+" (Item : in String) return Ada.Strings.Unbounded.String_Access
   is begin
      return Ada.Strings.Unbounded.String_Access'(new String'(Item));
   end "+";
   function "+" (Item : in WisiToken.BNF.Generate_Algorithm_Set) return WisiToken.BNF.Generate_Algorithm_Set_Access
   is begin
      return new WisiToken.BNF.Generate_Algorithm_Set'(Item);
   end "+";

   Suite : constant Access_Test_Suite := new Test_Suite;
begin
   --  Tests are grouped by test package, then alphabetical by grammar name

   --  test error handling when generate fails
   Add_Test
     (Suite, Test_Case_Access'
        (new Test_Generate_Errors.Test_Case
           (+"../test/bnf/unused_tokens", +(LR1 | LALR => True, others => False))));

   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Errors_Test.Test_Case (+"indent_param_count.wy")));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Errors_Test.Test_Case (+"same_delimiters.wy")));

   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"ada_ebnf", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"ada_lite", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"ada_lite_ebnf", +"ada_lite")));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"body_instantiation_conflict", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"case_expression", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"character_literal", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"conflict_name", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"dragon_4_43", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_1", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_2", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_3", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_4", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_5", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_6", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_7", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"empty_production_8", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"grammar_grammar_01", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"identifier_list_name_conflict", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"java_ebnf", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"java_enum_ch19", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"java_expressions_antlr", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"java_expressions_ch19", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"java_types_ch19", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"lalr_generator_bug_01", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"nested_ebnf_optional", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_conflict_01", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_conflict_02", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_conflict_03", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_conflict_04", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_list", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"optimized_list_ebnf", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"python_ebnf", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"range_conflict", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"skip_to_grammar", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"subprograms", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"three_action_conflict", null)));
   Add_Test (Suite, Test_Case_Access'(new BNF_WY_Test.Test_Case (+"warth_left_recurse_expr_1", null)));

   Add_Test (Suite, Test_Case_Access'(new Test_LR1_Parallel.Test_Case (+"ada_lite")));
   Add_Test (Suite, Test_Case_Access'(new Test_LR1_Parallel.Test_Case (+"body_instantiation_conflict")));
   Add_Test (Suite, Test_Case_Access'(new Test_LR1_Parallel.Test_Case (+"java_types_ch19")));

   --  other *.wy files in ../wisi/test are used in Ada parser
   --  generator/parse tests, not run from here.

   --  end test cases

   return Suite;

end Test_BNF_Suite;
