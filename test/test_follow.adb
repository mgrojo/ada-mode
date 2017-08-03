--  Abstract:
--
--  See spec
--
--  Copyright (C) 2017 Stephen Leake
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

with Ada_Lite;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Wisi.Declarations;
with Wisi.Gen_Generate_Utils;
with Wisi.Prologue;
with Wisi.Rules;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Parser.LR.LR1_Items;
with WisiToken.Parser.LR;
with WisiToken.Production;
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
         Accept_ID         => WisiToken_Accept_ID);
      use Token_Enum;

      use all type WisiToken.Production.Right_Hand_Side;
      use all type WisiToken.Production.List.Instance;

      Null_Action : WisiToken.Semantic_Action renames WisiToken.Null_Action;

      --  This grammar has right recursion on Declarations_ID, and an
      --  empty production for Parameter_List_ID
      Grammar : constant WisiToken.Production.List.Instance :=
        WisiToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and                -- 1
        Declarations_ID     <= Declaration_ID + Null_Action and                          -- 2
        Declarations_ID     <= Declarations_ID & Declaration_ID + Null_Action and        -- 3
        Declaration_ID      <= Subprogram_ID + Null_Action and                           -- 4
        Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and        -- 5
        Parameter_List_ID   <= +Null_Action and                                          -- 6
        Parameter_List_ID   <= Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action; -- 7

      Has_Empty_Production : constant WisiToken.Token_ID_Set := WisiToken.Parser.LR.LR1_Items.Has_Empty_Production
        (Grammar, LALR_Descriptor);
      First                : constant WisiToken.Token_Array_Token_Set := WisiToken.Parser.LR.LR1_Items.First
        (Grammar, LALR_Descriptor, Has_Empty_Production, Trace => False);

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         Test : Test_Case renames Test_Case (T);

         use Ada.Text_IO;
         use WisiToken.Parser.LR.LR1_Items;

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

   package Test_Ada_Lite is
      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class);
   end Test_Ada_Lite;
   package body Test_Ada_Lite is
      use Wisi;
      use WisiToken;

      Input_File_Name : constant String := "../wisi/test/ada_lite.wy";
      Input_File      : Ada.Text_IO.File_Type;

      WisiToken_Accept_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"wisitoken_accept";

      Prologue_Context_Clause : String_Lists.List;
      Prologue_Declarations   : String_Lists.List;
      Keywords                : String_Pair_Lists.List;
      Tokens                  : Token_Lists.List;
      Conflicts               : Conflict_Lists.List;
      Rules                   : Rule_Lists.List;
      Generate_Params         : Generate_Param_Type;
      Panic_Recover           : String_Lists.List;
      McKenzie_Recover        : McKenzie_Recover_Param_Type;
      Rule_Count              : Integer;
      Action_Count            : Integer;

      function To_Token_Ada_Name (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
      is begin
         return -Item;
      end To_Token_Ada_Name;

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         Test : Test_Case renames Test_Case (T);

         use Ada.Text_IO;
         use WisiToken.Parser.LR.LR1_Items;
      begin
         Open (Input_File, In_File, Input_File_Name);
         Wisi.Prologue (Input_File, Prologue_Context_Clause, Prologue_Declarations);
         Wisi.Declarations (Input_File, Generate_Params, Keywords, Tokens, Conflicts, Panic_Recover, McKenzie_Recover);
         Wisi.Rules (Input_File, Generate_Params.Output_Language, Rules, Rule_Count, Action_Count);

         declare
            use Ada_Lite;

            package Token_Enum is new WisiToken.Gen_Token_Enum
              --  We only need this for To_Nonterminal_Array_Terminal_Set below.
              (Token_Enum_ID     => Ada_Lite.Token_Enum_ID,
               First_Terminal    => AND_ID,
               Last_Terminal     => Wisi_EOI_ID,
               First_Nonterminal => wisitoken_accept_ID,
               Last_Nonterminal  => unary_adding_operator_ID,
               EOF_ID            => Wisi_EOI_ID,
               Accept_ID         => wisitoken_accept_id);

            package Generate_Utils is new Gen_Generate_Utils
              (Keywords, Tokens, Conflicts, Rules, +"Wisi_EOI", WisiToken_Accept_Name, To_Token_Ada_Name);

            Descriptor : WisiToken.LALR_Descriptor renames Generate_Utils.LALR_Descriptor;

            Grammar : constant WisiToken.Production.List.Instance := Generate_Utils.To_Grammar
              (Descriptor, Input_File_Name, -Generate_Params.Start_Token);

            Has_Empty : constant WisiToken.Token_ID_Set := WisiToken.Parser.LR.LR1_Items.Has_Empty_Production
              (Grammar, Descriptor);

            Expected_Has_Empty : constant WisiToken.Token_ID_Set
              (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal) := Token_Enum.To_Token_ID_Set
                ((association_opt_ID | association_list_ID | declarative_part_opt_ID | expression_opt_ID |
                    identifier_opt_ID | name_opt_ID | parameter_profile_opt_ID | parameter_specification_ID |
                    parameter_specification_list_ID | sequence_of_statements_opt_ID => True,
                  others => False));

            First : constant WisiToken.Token_Array_Token_Set := WisiToken.Parser.LR.LR1_Items.First
              (Grammar, Descriptor, Has_Empty, Trace => False);

            Expected_First : constant WisiToken.Token_Array_Token_Set := Token_Enum.To_Nonterminal_Array_Token_Set
              ((wisitoken_accept_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | compilation_unit_ID | compilation_unit_list_ID |
                     function_specification_ID | library_item_ID | procedure_specification_ID | subprogram_body_ID |
                     subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                actual_parameter_part_ID => (LEFT_PAREN_ID => True, others => False),
                assignment_statement_ID => (IDENTIFIER_ID | name_ID | selected_component_ID => True, others => False),
                association_opt_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | expression_ID |
                     expression_opt_ID | factor_ID | name_ID | paren_expression_ID | primary_ID |
                     relation_and_list_ID | relation_or_list_ID | relation_xor_list_ID | relation_ID |
                     selected_component_ID | simple_expression_ID | term_ID | term_list_ID |
                     unary_adding_operator_ID => True,
                   others => False),
                association_list_ID =>
                  (COMMA_ID | LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID |
                     association_opt_ID | association_list_ID | expression_ID | expression_opt_ID | factor_ID |
                     name_ID | paren_expression_ID | primary_ID | relation_and_list_ID | relation_or_list_ID |
                     relation_xor_list_ID | relation_ID | selected_component_ID | simple_expression_ID | term_ID |
                     term_list_ID | unary_adding_operator_ID => True,
                   others => False),
                binary_adding_operator_ID => (MINUS_ID | PLUS_ID => True, others => False),
                block_statement_ID => (BEGIN_ID | DECLARE_ID => True, others => False),
                body_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID |
                     proper_body_ID | subprogram_body_ID | subprogram_specification_ID => True,
                   others => False),
                case_statement_ID => (CASE_ID => True, others => False),
                case_statement_alternative_ID => (WHEN_ID => True, others => False),
                case_statement_alternative_list_ID =>
                  (WHEN_ID | case_statement_alternative_ID | case_statement_alternative_list_ID => True,
                   others => False),
                compilation_unit_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | library_item_ID |
                   procedure_specification_ID | subprogram_body_ID | subprogram_declaration_ID |
                   subprogram_specification_ID => True,
                   others => False),
                compilation_unit_list_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | compilation_unit_ID | compilation_unit_list_ID |
                   function_specification_ID | library_item_ID | procedure_specification_ID | subprogram_body_ID |
                   subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                compound_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | IF_ID | LOOP_ID | block_statement_ID | case_statement_ID |
                   if_statement_ID | loop_statement_ID => True,
                   others => False),
                declaration_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID | body_ID | function_specification_ID |
                   object_declaration_ID | procedure_specification_ID | proper_body_ID | subprogram_body_ID |
                   subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                declarations_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID | body_ID | declaration_ID | declarations_ID |
                   function_specification_ID | object_declaration_ID | procedure_specification_ID | proper_body_ID |
                   subprogram_body_ID | subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                declarative_part_opt_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID | body_ID | declaration_ID | declarations_ID |
                   function_specification_ID | object_declaration_ID | procedure_specification_ID | proper_body_ID |
                   subprogram_body_ID | subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                elsif_statement_item_ID => (ELSIF_ID => True, others => False),
                elsif_statement_list_ID =>
                  (ELSIF_ID | elsif_statement_item_ID | elsif_statement_list_ID => True, others => False),
                exit_statement_ID => (EXIT_ID => True, others => False),
                expression_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | relation_and_list_ID | relation_or_list_ID |
                   relation_xor_list_ID | relation_ID | selected_component_ID | simple_expression_ID | term_ID |
                   term_list_ID | unary_adding_operator_ID => True,
                   others => False),
                expression_opt_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | expression_ID |
                   factor_ID | name_ID | paren_expression_ID | primary_ID | relation_and_list_ID |
                   relation_or_list_ID | relation_xor_list_ID | relation_ID | selected_component_ID |
                   simple_expression_ID | term_ID | term_list_ID | unary_adding_operator_ID => True,
                   others => False),
                factor_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | name_ID | paren_expression_ID |
                   primary_ID | selected_component_ID => True,
                   others => False),
                formal_part_ID => (LEFT_PAREN_ID => True, others => False),
                function_specification_ID => (FUNCTION_ID => True, others => False),
                handled_sequence_of_statements_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | EXIT_ID | IF_ID | LOOP_ID | RETURN_ID | IDENTIFIER_ID |
                   assignment_statement_ID | block_statement_ID | case_statement_ID | compound_statement_ID |
                   exit_statement_ID | if_statement_ID | label_ID | loop_statement_ID | name_ID |
                   procedure_call_statement_ID | selected_component_ID | sequence_of_statements_ID |
                   sequence_of_statements_opt_ID | simple_return_statement_ID | simple_statement_ID |
                   statement_ID => True,
                   others => False),
                identifier_opt_ID => (IDENTIFIER_ID => True, others => False),
                if_statement_ID => (IF_ID => True, others => False),
                label_ID => (IDENTIFIER_ID => True, others => False),
                library_item_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID |
                   subprogram_body_ID | subprogram_declaration_ID | subprogram_specification_ID => True,
                   others => False),
                loop_statement_ID => (LOOP_ID => True, others => False),
                multiplying_operator_ID => (SLASH_ID | STAR_ID => True, others => False),
                name_ID => (IDENTIFIER_ID | name_ID | selected_component_ID => True, others => False),
                name_opt_ID => (IDENTIFIER_ID | name_ID | selected_component_ID => True, others => False),
                object_declaration_ID => (IDENTIFIER_ID => True, others => False),
                parameter_and_result_profile_ID =>
                  (LEFT_PAREN_ID | RETURN_ID | formal_part_ID => True,
                   others => False),
                parameter_profile_opt_ID => (LEFT_PAREN_ID | formal_part_ID => True, others => False),
                parameter_specification_ID => (IDENTIFIER_ID => True, others => False),
                parameter_specification_list_ID =>
                  (IDENTIFIER_ID | SEMICOLON_ID | parameter_specification_ID | parameter_specification_list_ID => True,
                   others => False),
                paren_expression_ID => (LEFT_PAREN_ID => True, others => False),
                primary_ID =>
                  (LEFT_PAREN_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | name_ID | paren_expression_ID |
                   selected_component_ID => True,
                   others => False),
                procedure_call_statement_ID =>
                  (IDENTIFIER_ID | name_ID | selected_component_ID => True,
                   others => False),
                procedure_specification_ID => (PROCEDURE_ID => True, others => False),
                proper_body_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID |
                   subprogram_body_ID | subprogram_specification_ID => True,
                   others => False),
                relation_and_list_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | relation_and_list_ID | relation_ID |
                   selected_component_ID | simple_expression_ID | term_ID | term_list_ID |
                   unary_adding_operator_ID => True,
                   others => False),
                relation_or_list_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | relation_or_list_ID | relation_ID |
                   selected_component_ID | simple_expression_ID | term_ID | term_list_ID |
                   unary_adding_operator_ID => True,
                   others => False),
                relation_xor_list_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | relation_xor_list_ID | relation_ID |
                   selected_component_ID | simple_expression_ID | term_ID | term_list_ID |
                   unary_adding_operator_ID => True,
                   others => False),
                relation_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | selected_component_ID | simple_expression_ID |
                   term_ID | term_list_ID | unary_adding_operator_ID => True,
                   others => False),
                relational_operator_ID =>
                  (EQUAL_ID | GREATER_ID | GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | SLASH_EQUAL_ID => True,
                   others => False),
                selected_component_ID => (IDENTIFIER_ID | name_ID | selected_component_ID => True, others => False),
                sequence_of_statements_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | EXIT_ID | IF_ID | LOOP_ID | RETURN_ID | IDENTIFIER_ID |
                   assignment_statement_ID | block_statement_ID | case_statement_ID | compound_statement_ID |
                   exit_statement_ID | if_statement_ID | label_ID | loop_statement_ID | name_ID |
                   procedure_call_statement_ID | selected_component_ID | sequence_of_statements_ID |
                   simple_return_statement_ID | simple_statement_ID | statement_ID => True,
                   others => False),
                sequence_of_statements_opt_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | EXIT_ID | IF_ID | LOOP_ID | RETURN_ID | IDENTIFIER_ID |
                   assignment_statement_ID | block_statement_ID | case_statement_ID | compound_statement_ID |
                   exit_statement_ID | if_statement_ID | label_ID | loop_statement_ID | name_ID |
                   procedure_call_statement_ID | selected_component_ID | sequence_of_statements_ID |
                   simple_return_statement_ID | simple_statement_ID | statement_ID => True,
                   others => False),
                simple_expression_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID |
                   name_ID | paren_expression_ID | primary_ID | selected_component_ID | term_ID | term_list_ID |
                   unary_adding_operator_ID => True,
                   others => False),
                simple_return_statement_ID => (RETURN_ID => True, others => False),
                simple_statement_ID =>
                  (EXIT_ID | RETURN_ID | IDENTIFIER_ID | assignment_statement_ID | exit_statement_ID | name_ID |
                   procedure_call_statement_ID | selected_component_ID | simple_return_statement_ID => True,
                   others => False),
                statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | EXIT_ID | IF_ID | LOOP_ID | RETURN_ID | IDENTIFIER_ID |
                   assignment_statement_ID | block_statement_ID | case_statement_ID | compound_statement_ID |
                   exit_statement_ID | if_statement_ID | label_ID | loop_statement_ID | name_ID |
                   procedure_call_statement_ID | selected_component_ID | simple_return_statement_ID |
                   simple_statement_ID => True,
                   others => False),
                subprogram_body_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID |
                   subprogram_specification_ID => True,
                   others => False),
                subprogram_declaration_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID |
                   subprogram_specification_ID => True,
                   others => False),
                subprogram_specification_ID =>
                  (FUNCTION_ID | PROCEDURE_ID | function_specification_ID | procedure_specification_ID => True,
                   others => False),
                term_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID | name_ID |
                   paren_expression_ID | primary_ID | selected_component_ID | term_ID => True,
                   others => False),
                term_list_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID | factor_ID | name_ID |
                   paren_expression_ID | primary_ID | selected_component_ID | term_ID | term_list_ID => True,
                   others => False),
                unary_adding_operator_ID => (MINUS_ID | PLUS_ID => True, others => False),
                others => (others => False)));

            Follow : constant WisiToken.Token_Array_Token_Set := WisiToken.Parser.LR.LR1_Items.Follow
              (Grammar, Descriptor, First, Has_Empty);

            Expected_Follow : constant WisiToken.Token_Array_Token_Set := Token_Enum.To_Nonterminal_Array_Terminal_Set
              ((wisitoken_accept_ID => (others => False),
                actual_parameter_part_ID =>
                  (AND_ID | IS_ID | LEFT_PAREN_ID | OR_ID | RETURN_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID |
                     COLON_EQUAL_ID | COMMA_ID | DOT_ID | EQUAL_ID | GREATER_ID | GREATER_EQUAL_ID | LESS_ID |
                     LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID | SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                assignment_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                association_opt_ID => (RIGHT_PAREN_ID | COMMA_ID => True, others => False),
                association_list_ID => (RIGHT_PAREN_ID | COMMA_ID => True, others => False),
                binary_adding_operator_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID => True,
                   others => False),
                block_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                body_ID => (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID => True, others => False),
                case_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                case_statement_alternative_ID => (END_ID | WHEN_ID => True, others => False),
                 case_statement_alternative_list_ID => (END_ID | WHEN_ID => True, others => False),
                compilation_unit_ID => (FUNCTION_ID | PROCEDURE_ID | Wisi_EOI_ID => True, others => False),
                compilation_unit_list_ID => (FUNCTION_ID | PROCEDURE_ID | Wisi_EOI_ID => True, others => False),
                compound_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                declaration_ID => (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID => True, others => False),
                declarations_ID => (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID => True, others => False),
                declarative_part_opt_ID => (BEGIN_ID => True, others => False),
                elsif_statement_item_ID => (ELSE_ID | ELSIF_ID | END_ID => True, others => False),
                elsif_statement_list_ID => (ELSE_ID | ELSIF_ID | END_ID => True, others => False),
                exit_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                expression_ID => (IS_ID | RIGHT_PAREN_ID | THEN_ID | COMMA_ID | SEMICOLON_ID => True, others => False),
                expression_opt_ID =>
                  (IS_ID | RIGHT_PAREN_ID | THEN_ID | COMMA_ID | SEMICOLON_ID => True,
                   others => False),
                factor_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID |
                     SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                formal_part_ID => (IS_ID | RETURN_ID | SEMICOLON_ID => True, others => False),
                function_specification_ID => (IS_ID | SEMICOLON_ID => True, others => False),
                handled_sequence_of_statements_ID => (END_ID => True, others => False),
                identifier_opt_ID => (WHEN_ID | SEMICOLON_ID => True, others => False),
                if_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                label_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                library_item_ID => (FUNCTION_ID | PROCEDURE_ID | Wisi_EOI_ID => True, others => False),
                loop_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                multiplying_operator_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID => True,
                   others => False),
                name_ID =>
                  (AND_ID | IS_ID | LEFT_PAREN_ID | OR_ID | RETURN_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID |
                     COLON_EQUAL_ID | COMMA_ID | DOT_ID | EQUAL_ID | GREATER_ID | GREATER_EQUAL_ID | LESS_ID |
                     LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID | SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                name_opt_ID => (SEMICOLON_ID => True, others => False),
                object_declaration_ID =>
                  (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID => True,
                   others => False),
                parameter_and_result_profile_ID => (IS_ID | SEMICOLON_ID => True, others => False),
                parameter_profile_opt_ID => (IS_ID | SEMICOLON_ID => True, others => False),
                parameter_specification_ID => (RIGHT_PAREN_ID | SEMICOLON_ID => True, others => False),
                parameter_specification_list_ID => (RIGHT_PAREN_ID | SEMICOLON_ID => True, others => False),
                paren_expression_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID |
                     SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                primary_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID |
                     SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                procedure_call_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                procedure_specification_ID => (IS_ID | SEMICOLON_ID => True, others => False),
                proper_body_ID => (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID => True, others => False),
                relation_and_list_ID =>
                  (AND_ID | IS_ID | RIGHT_PAREN_ID | THEN_ID | COMMA_ID | SEMICOLON_ID => True,
                   others => False),
                relation_or_list_ID =>
                  (IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | COMMA_ID | SEMICOLON_ID => True,
                   others => False),
                relation_xor_list_ID =>
                  (IS_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | SEMICOLON_ID => True,
                   others => False),
                relation_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | SEMICOLON_ID => True,
                   others => False),
                relational_operator_ID =>
                  (LEFT_PAREN_ID | NOT_ID | MINUS_ID | PLUS_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID => True,
                   others => False),
                selected_component_ID =>
                  (AND_ID | IS_ID | LEFT_PAREN_ID | OR_ID | RETURN_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID |
                     COLON_EQUAL_ID | COMMA_ID | DOT_ID | EQUAL_ID | GREATER_ID | GREATER_EQUAL_ID | LESS_ID |
                     LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID | SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                sequence_of_statements_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                sequence_of_statements_opt_ID => (ELSE_ID | ELSIF_ID | END_ID | WHEN_ID => True, others => False),
                simple_expression_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | SEMICOLON_ID | SLASH_EQUAL_ID => True,
                   others => False),
                simple_return_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                simple_statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                statement_ID =>
                  (BEGIN_ID | CASE_ID | DECLARE_ID | ELSE_ID | ELSIF_ID | END_ID | EXIT_ID | IF_ID | LOOP_ID |
                     RETURN_ID | WHEN_ID | IDENTIFIER_ID => True,
                   others => False),
                subprogram_body_ID =>
                  (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID | Wisi_EOI_ID => True,
                   others => False),
                subprogram_declaration_ID =>
                  (BEGIN_ID | FUNCTION_ID | PROCEDURE_ID | IDENTIFIER_ID | Wisi_EOI_ID => True,
                   others => False),
                subprogram_specification_ID => (IS_ID | SEMICOLON_ID => True, others => False),
                term_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID | SLASH_ID |
                     SLASH_EQUAL_ID | STAR_ID => True,
                   others => False),
                term_list_ID =>
                  (AND_ID | IS_ID | OR_ID | RIGHT_PAREN_ID | THEN_ID | XOR_ID | COMMA_ID | EQUAL_ID | GREATER_ID |
                     GREATER_EQUAL_ID | LESS_ID | LESS_EQUAL_ID | MINUS_ID | PLUS_ID | SEMICOLON_ID |
                     SLASH_EQUAL_ID => True,
                   others => False),
                unary_adding_operator_ID =>
                  (LEFT_PAREN_ID | NOT_ID | NUMERIC_LITERAL_ID | IDENTIFIER_ID => True, others => False),
                others => (others => False)));

         begin
            if Test.Debug then
               Put_Line ("Computed First: ");
               WisiToken.Put (Descriptor, First);
               New_Line;
               Put_Line ("Expected First:");
               WisiToken.Put (Descriptor, Expected_First);
               New_Line;

               Put_Line ("Computed Follow: ");
               WisiToken.Put (Descriptor, Follow);
               New_Line;
               Put_Line ("Expected Follow:");
               WisiToken.Put (Descriptor, Expected_Follow);
            end if;
            WisiToken.AUnit.Check ("Has_Empty", Has_Empty, Expected_Has_Empty);
            WisiToken.AUnit.Check ("First", First, Expected_First);
            WisiToken.AUnit.Check ("Follow", Follow, Expected_Follow);
         end;
      end One;

   end Test_Ada_Lite;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_Ada_Lite.One'Access, "debug");
      else
         Register_Routine (T, Subprograms.One'Access, "Subprograms.One");
         Register_Routine (T, Test_Ada_Lite.One'Access, "Test_Ada_Lite.One");
      end if;
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../Test/test_follow.adb");
   end Name;

end Test_Follow;
