--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
with Ada_Process_Actions; use Ada_Process_Actions;
pragma Warnings (Off, "license of withed unit ""GNATCOLL.Traces"" may be inconsistent");
with GNATCOLL.Traces;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Analysis.More;
package body Wisi.Libadalang is
   use all type WisiToken.Token_ID;
   package L_Lex renames Standard.Libadalang.Lexer;
   package L_Ana renames Standard.Libadalang.Analysis;

   L_Terminal_To_Wisi_ID : constant array (L_Lex.Token_Kind) of WisiToken.Token_ID :=
     (L_Lex.Ada_Termination    => +Wisi_EOI_ID,
      L_Lex.Ada_Lexing_Failure => WisiToken.Invalid_Token_ID,
      L_Lex.Ada_Identifier     => +IDENTIFIER_ID,
      L_Lex.Ada_All            => +ALL_ID,
      L_Lex.Ada_Abort          => +ABORT_ID,
      L_Lex.Ada_Else           => +ELSE_ID,
      L_Lex.Ada_New            => +NEW_ID,
      L_Lex.Ada_Return         => +RETURN_ID,
      L_Lex.Ada_Abs            => +ABS_ID,
      L_Lex.Ada_Elsif          => +ELSIF_ID,
      L_Lex.Ada_Not            => +NOT_ID,
      L_Lex.Ada_Reverse        => +REVERSE_ID,
      L_Lex.Ada_Abstract       => +ABSTRACT_ID,
      L_Lex.Ada_End            => +END_ID,
      L_Lex.Ada_Null           => +NULL_ID,
      L_Lex.Ada_Accept         => +ACCEPT_ID,
      L_Lex.Ada_Entry          => +ENTRY_ID,
      L_Lex.Ada_Select         => +SELECT_ID,
      L_Lex.Ada_Access         => +ACCESS_ID,
      L_Lex.Ada_Exception      => +EXCEPTION_ID,
      L_Lex.Ada_Of             => +OF_ID,
      L_Lex.Ada_Separate       => +SEPARATE_ID,
      L_Lex.Ada_Aliased        => +ALIASED_ID,
      L_Lex.Ada_Exit           => +EXIT_ID,
      L_Lex.Ada_Or             => +OR_ID,
      L_Lex.Ada_Some           => +SOME_ID,
      L_Lex.Ada_Others         => +OTHERS_ID,
      L_Lex.Ada_Subtype        => +SUBTYPE_ID,
      L_Lex.Ada_And            => +AND_ID,
      L_Lex.Ada_For            => +FOR_ID,
      L_Lex.Ada_Out            => +OUT_ID,
      L_Lex.Ada_Array          => +ARRAY_ID,
      L_Lex.Ada_Function       => +FUNCTION_ID,
      L_Lex.Ada_At             => +AT_ID,
      L_Lex.Ada_Tagged         => +TAGGED_ID,
      L_Lex.Ada_Generic        => +GENERIC_ID,
      L_Lex.Ada_Package        => +PACKAGE_ID,
      L_Lex.Ada_Task           => +TASK_ID,
      L_Lex.Ada_Begin          => +BEGIN_ID,
      L_Lex.Ada_Goto           => +GOTO_ID,
      L_Lex.Ada_Pragma         => +PRAGMA_ID,
      L_Lex.Ada_Terminate      => +TERMINATE_ID,
      L_Lex.Ada_Body           => +BODY_ID,
      L_Lex.Ada_Private        => +PRIVATE_ID,
      L_Lex.Ada_Then           => +THEN_ID,
      L_Lex.Ada_If             => +IF_ID,
      L_Lex.Ada_Procedure      => +PROCEDURE_ID,
      L_Lex.Ada_Type           => +TYPE_ID,
      L_Lex.Ada_Case           => +CASE_ID,
      L_Lex.Ada_In             => +IN_ID,
      L_Lex.Ada_Constant       => +CONSTANT_ID,
      L_Lex.Ada_Is             => +IS_ID,
      L_Lex.Ada_Raise          => +RAISE_ID,
      L_Lex.Ada_Use            => +USE_ID,
      L_Lex.Ada_Declare        => +DECLARE_ID,
      L_Lex.Ada_Range          => +RANGE_ID,
      L_Lex.Ada_Delay          => +DELAY_ID,
      L_Lex.Ada_Until          => +UNTIL_ID,
      L_Lex.Ada_Limited        => +LIMITED_ID,
      L_Lex.Ada_Record         => +RECORD_ID,
      L_Lex.Ada_When           => +WHEN_ID,
      L_Lex.Ada_Delta          => +DELTA_ID,
      L_Lex.Ada_Loop           => +LOOP_ID,
      L_Lex.Ada_Rem            => +REM_ID,
      L_Lex.Ada_While          => +WHILE_ID,
      L_Lex.Ada_Digits         => +DIGITS_ID,
      L_Lex.Ada_Renames        => +RENAMES_ID,
      L_Lex.Ada_Do             => +DO_ID,
      L_Lex.Ada_Mod            => +MOD_ID,
      L_Lex.Ada_Requeue        => +REQUEUE_ID,
      L_Lex.Ada_Xor            => +XOR_ID,
      L_Lex.Ada_Par_Close      => +LEFT_PAREN_ID,
      L_Lex.Ada_Par_Open       => +RIGHT_PAREN_ID,
      L_Lex.Ada_Semicolon      => +SEMICOLON_ID,
      L_Lex.Ada_Colon          => +COLON_ID,
      L_Lex.Ada_Comma          => +COMMA_ID,
      L_Lex.Ada_Doubledot      => +DOT_DOT_ID,
      L_Lex.Ada_Dot            => +DOT_ID,
      L_Lex.Ada_Diamond        => +BOX_ID,
      L_Lex.Ada_Lte            => +LESS_EQUAL_ID,
      L_Lex.Ada_Gte            => +GREATER_EQUAL_ID,
      L_Lex.Ada_Arrow          => +EQUAL_GREATER_ID,
      L_Lex.Ada_Equal          => +EQUAL_ID,
      L_Lex.Ada_Lt             => +LESS_ID,
      L_Lex.Ada_Gt             => +GREATER_ID,
      L_Lex.Ada_Plus           => +PLUS_ID,
      L_Lex.Ada_Minus          => +MINUS_ID,
      L_Lex.Ada_Power          => +STAR_STAR_ID,
      L_Lex.Ada_Mult           => +STAR_ID,
      L_Lex.Ada_Amp            => +AMPERSAND_ID,
      L_Lex.Ada_Notequal       => +SLASH_EQUAL_ID,
      L_Lex.Ada_Divide         => +SLASH_ID,
      L_Lex.Ada_Tick           => +TICK_1_ID,
      L_Lex.Ada_Pipe           => +BAR_ID,
      L_Lex.Ada_Assign         => +COLON_EQUAL_ID,
      L_Lex.Ada_Label_Start    => +LESS_LESS_ID,
      L_Lex.Ada_Label_End      => +GREATER_GREATER_ID,
      L_Lex.Ada_Target         => +AT_ID,
      L_Lex.Ada_String         => +STRING_LITERAL_ID,
      L_Lex.Ada_Char           => +CHARACTER_LITERAL_ID,
      L_Lex.Ada_With           => +WITH_ID,
      L_Lex.Ada_Decimal        => +NUMERIC_LITERAL_ID,
      L_Lex.Ada_Integer        => +NUMERIC_LITERAL_ID,
      L_Lex.Ada_Comment        => +COMMENT_ID,
      L_Lex.Ada_Prep_Line      => +COMMENT_ID,
      L_Lex.Ada_Whitespace     => +WHITESPACE_ID);

   No_Direct_Map : constant WisiToken.Token_ID := WisiToken.Invalid_Token_ID - 1;

   L_Node_To_Wisi_ID : constant array (L_Ana.Ada_Node_Kind_Type) of WisiToken.Token_ID :=
     (L_Ana.Ada_Abort_Absent => No_Direct_Map,
      L_Ana.Ada_Abort_Present => +ABORT_ID,
      L_Ana.Ada_Abstract_Absent => No_Direct_Map,
      L_Ana.Ada_Abstract_Present => +ABSTRACT_ID,
      L_Ana.Ada_Ada_Node_List => No_Direct_Map, -- ?
      L_Ana.Ada_Alternatives_List => No_Direct_Map, -- ?,
      L_Ana.Ada_Constraint_List => +discrete_subtype_definition_list_ID,
      L_Ana.Ada_Decl_List => +declarations_ID,
      L_Ana.Ada_Stmt_List => +sequence_of_statements_ID,
      L_Ana.Ada_Aspect_Assoc_List => +association_list_ID,
      L_Ana.Ada_Base_Assoc_List => +association_list_ID,
      L_Ana.Ada_Assoc_List => +association_list_ID,
      L_Ana.Ada_Case_Expr_Alternative_List => +case_expression_alternative_list_ID,
      L_Ana.Ada_Case_Stmt_Alternative_List => +case_statement_alternative_list_ID,
      L_Ana.Ada_Compilation_Unit_List => +compilation_unit_list_ID,
      L_Ana.Ada_Component_Clause_List => +component_clause_list_ID,
      L_Ana.Ada_Contract_Case_Assoc_List => +association_list_ID,
      L_Ana.Ada_Defining_Name_List => +identifier_list_ID,
      L_Ana.Ada_Discriminant_Spec_List => +discriminant_specification_list_ID,
      L_Ana.Ada_Elsif_Expr_Part_List => +elsif_expression_list_ID,
      L_Ana.Ada_Elsif_Stmt_Part_List => +elsif_statement_list_ID,
      L_Ana.Ada_Enum_Literal_Decl_List => +enumeration_literal_list_ID,
      L_Ana.Ada_Expr_Alternatives_List => +membership_choice_list_ID,
      L_Ana.Ada_Discriminant_Choice_List => +discrete_choice_list_ID,
      L_Ana.Ada_Name_List => +name_list_ID,
      L_Ana.Ada_Parent_List => +interface_list_ID,
      L_Ana.Ada_Param_Spec_List => +parameter_specification_list_ID,
      L_Ana.Ada_Pragma_Node_List => No_Direct_Map, --  statement or declaration
      L_Ana.Ada_Select_When_Part_List => +select_alternative_ID,
      L_Ana.Ada_Unconstrained_Array_Index_List => +index_subtype_definition_list_ID,
      L_Ana.Ada_Variant_List => +variant_list_ID,
      L_Ana.Ada_Aliased_Absent => No_Direct_Map,
      L_Ana.Ada_Aliased_Present => +ALIASED_ID,
      L_Ana.Ada_All_Absent => No_Direct_Map,
      L_Ana.Ada_All_Present => +ALL_ID,
      L_Ana.Ada_Constrained_Array_Indices => +discrete_subtype_definition_list_ID,
      L_Ana.Ada_Unconstrained_Array_Indices => +index_subtype_definition_list_ID,
      L_Ana.Ada_Aspect_Assoc => +association_list_ID,
      L_Ana.Ada_At_Clause => +at_clause_ID,
      L_Ana.Ada_Attribute_Def_Clause => +aspect_clause_ID,
      L_Ana.Ada_Enum_Rep_Clause => +enumeration_representation_clause_ID,
      L_Ana.Ada_Record_Rep_Clause => +record_representation_clause_ID,
      L_Ana.Ada_Aspect_Spec => +aspect_specification_opt_ID,
      L_Ana.Ada_Contract_Case_Assoc => +association_list_ID,
      L_Ana.Ada_Pragma_Argument_Assoc => +association_list_ID,
      L_Ana.Ada_Entry_Spec => No_Direct_Map, --  part of Entry_Declaration_ID
      L_Ana.Ada_Subp_Spec => +subprogram_specification_ID,
      L_Ana.Ada_Component_List => +component_list_ID,
      L_Ana.Ada_Known_Discriminant_Part => +discriminant_part_opt_ID,
      L_Ana.Ada_Unknown_Discriminant_Part => +discriminant_part_opt_ID,
      L_Ana.Ada_Generic_Formal_Part => +generic_formal_part_ID,
      L_Ana.Ada_Null_Record_Def => +record_definition_ID,
      L_Ana.Ada_Record_Def => +record_definition_ID,
      L_Ana.Ada_Aggregate_Assoc => +association_opt_ID,
      L_Ana.Ada_Multi_Dim_Array_Assoc => +association_opt_ID,
      L_Ana.Ada_Discriminant_Assoc => +association_opt_ID,
      L_Ana.Ada_Param_Assoc => +association_opt_ID,
      L_Ana.Ada_Component_Decl => +component_declaration_ID,
      L_Ana.Ada_Discriminant_Spec => +discriminant_specification_opt_ID,
      L_Ana.Ada_Generic_Formal_Obj_Decl => +formal_object_declaration_ID,
      L_Ana.Ada_Generic_Formal_Package => +formal_package_declaration_ID,
      L_Ana.Ada_Generic_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      L_Ana.Ada_Generic_Formal_Type_Decl => +formal_type_declaration_ID,
      L_Ana.Ada_Param_Spec => +parameter_specification_ID,
      L_Ana.Ada_Generic_Package_Internal => +generic_package_declaration_ID,
      L_Ana.Ada_Package_Decl => +package_declaration_ID,
      L_Ana.Ada_Classwide_Type_Decl => No_Direct_Map, -- ?
      L_Ana.Ada_Incomplete_Type_Decl => +incomplete_type_declaration_ID,
      L_Ana.Ada_Incomplete_Tagged_Type_Decl => +incomplete_type_declaration_ID,
      L_Ana.Ada_Protected_Type_Decl => +protected_type_declaration_ID,
      L_Ana.Ada_Subtype_Decl => +subtype_declaration_ID,
      L_Ana.Ada_Task_Type_Decl => +task_type_declaration_ID,
      L_Ana.Ada_Single_Task_Type_Decl => +single_task_declaration_ID,
      L_Ana.Ada_Type_Decl => +type_declaration_ID,
      L_Ana.Ada_Anonymous_Type_Decl => No_Direct_Map, -- part of Object_Declaration_ID
      L_Ana.Ada_Synth_Anonymous_Type_Decl => +object_declaration_ID,
      L_Ana.Ada_Abstract_Subp_Decl => +abstract_subprogram_declaration_ID,
      L_Ana.Ada_Abstract_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      L_Ana.Ada_Concrete_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      L_Ana.Ada_Null_Subp_Decl => +null_procedure_declaration_ID,
      L_Ana.Ada_Subp_Decl => +subprogram_declaration_ID,
      L_Ana.Ada_Subp_Renaming_Decl => +subprogram_renaming_declaration_ID,
      L_Ana.Ada_Generic_Subp_Internal => +generic_subprogram_declaration_ID,
      L_Ana.Ada_Expr_Function => +expression_function_declaration_ID,
      L_Ana.Ada_Subp_Body => +subprogram_body_ID,
      L_Ana.Ada_Package_Body_Stub => +package_body_stub_ID,
      L_Ana.Ada_Protected_Body_Stub => +protected_body_stub_ID,
      L_Ana.Ada_Subp_Body_Stub => +subprogram_body_stub_ID,
      L_Ana.Ada_Task_Body_Stub => +task_body_stub_ID,
      L_Ana.Ada_Entry_Body => +entry_body_ID,
      L_Ana.Ada_Package_Body => +package_body_ID,
      L_Ana.Ada_Protected_Body => +protected_body_ID,
      L_Ana.Ada_Task_Body => +task_body_ID,
      L_Ana.Ada_Entry_Decl => +entry_declaration_ID,
      L_Ana.Ada_Enum_Literal_Decl => +enumeration_literal_ID,
      L_Ana.Ada_Error_Decl => No_Direct_Map, -- part of error handling; insert ?
      L_Ana.Ada_Exception_Decl => +exception_declaration_ID,
      L_Ana.Ada_Exception_Handler => +exception_handler_ID,
      L_Ana.Ada_For_Loop_Var_Decl => No_Direct_Map, --  part of Iterator_Specification_ID
      L_Ana.Ada_Generic_Package_Decl => +generic_package_declaration_ID,
      L_Ana.Ada_Generic_Subp_Decl => +generic_subprogram_declaration_ID,
      L_Ana.Ada_Generic_Package_Instantiation => +generic_instantiation_ID,
      L_Ana.Ada_Generic_Subp_Instantiation => +generic_instantiation_ID,
      L_Ana.Ada_Generic_Package_Renaming_Decl => +generic_renaming_declaration_ID,
      L_Ana.Ada_Generic_Subp_Renaming_Decl => +generic_renaming_declaration_ID,
      L_Ana.Ada_Label_Decl => +IDENTIFIER_ID,
      L_Ana.Ada_Named_Stmt_Decl => +block_statement_ID,
      L_Ana.Ada_Number_Decl => +declaration_ID,
      L_Ana.Ada_Object_Decl => +object_declaration_ID,
      L_Ana.Ada_Extended_Return_Stmt_Object_Decl => +extended_return_object_declaration_ID,
      L_Ana.Ada_Package_Renaming_Decl => +package_renaming_declaration_ID,
      L_Ana.Ada_Single_Protected_Decl => +single_protected_declaration_ID,
      L_Ana.Ada_Single_Task_Decl => +single_task_declaration_ID,
      L_Ana.Ada_Case_Stmt_Alternative => +case_statement_alternative_ID,
      L_Ana.Ada_Compilation_Unit => +compilation_unit_ID,
      L_Ana.Ada_Component_Clause => +component_clause_ID,
      L_Ana.Ada_Component_Def => +component_definition_ID,
      L_Ana.Ada_Constant_Absent => No_Direct_Map,
      L_Ana.Ada_Constant_Present => +CONSTANT_ID,
      L_Ana.Ada_Delta_Constraint => +constraint_ID,
      L_Ana.Ada_Digits_Constraint => +constraint_ID,
      L_Ana.Ada_Discriminant_Constraint => +constraint_ID,
      L_Ana.Ada_Index_Constraint => +index_constraint_ID,
      L_Ana.Ada_Range_Constraint => +constraint_ID,
      L_Ana.Ada_Declarative_Part => +declarative_part_opt_ID,
      L_Ana.Ada_Private_Part => +declarative_part_opt_ID,
      L_Ana.Ada_Public_Part => +declarative_part_opt_ID,
      L_Ana.Ada_Elsif_Expr_Part => +elsif_expression_list_ID,
      L_Ana.Ada_Elsif_Stmt_Part => +elsif_statement_list_ID,
      L_Ana.Ada_Entry_Index_Spec => No_Direct_Map, -- part of Entry_Body_Formal_Part_ID
      L_Ana.Ada_Allocator => +primary_ID,
      L_Ana.Ada_Aggregate => +aggregate_ID,
      L_Ana.Ada_Null_Record_Aggregate => +aggregate_ID,
      L_Ana.Ada_Bin_Op => No_Direct_Map, -- Binary_Adding_Operator_ID or multiplying_operator
      L_Ana.Ada_Relation_Op => +relational_operator_ID,
      L_Ana.Ada_Box_Expr => +BOX_ID,
      L_Ana.Ada_Case_Expr => +case_expression_ID,
      L_Ana.Ada_Case_Expr_Alternative => +case_expression_alternative_ID,
      L_Ana.Ada_Contract_Cases => +actual_parameter_part_ID,
      L_Ana.Ada_If_Expr => +if_expression_ID,
      L_Ana.Ada_Membership_Expr => +relation_ID,
      L_Ana.Ada_Attribute_Ref => +attribute_reference_ID,
      L_Ana.Ada_Update_Attribute_Ref => +attribute_reference_ID,
      L_Ana.Ada_Call_Expr => +name_ID,
      L_Ana.Ada_Defining_Name => +selected_component_ID,
      L_Ana.Ada_Discrete_Subtype_Name => +membership_choice_ID,
      L_Ana.Ada_Dotted_Name => +selected_component_ID,
      L_Ana.Ada_End_Name => +name_ID,
      L_Ana.Ada_Explicit_Deref => +selected_component_ID,
      L_Ana.Ada_Qual_Expr => +qualified_expression_ID,
      L_Ana.Ada_Char_Literal => +CHARACTER_LITERAL_ID,
      L_Ana.Ada_Identifier => +IDENTIFIER_ID,
      L_Ana.Ada_String_Literal => +STRING_LITERAL_ID,
      L_Ana.Ada_Null_Literal => +NULL_ID,
      L_Ana.Ada_Int_Literal => +NUMERIC_LITERAL_ID,
      L_Ana.Ada_Real_Literal => +NUMERIC_LITERAL_ID,
      L_Ana.Ada_Target_Name => +AT_ID,
      L_Ana.Ada_Paren_Expr => +paren_expression_ID,
      L_Ana.Ada_Quantified_Expr => +quantified_expression_ID,
      L_Ana.Ada_Raise_Expr => +raise_expression_ID,
      L_Ana.Ada_Un_Op => No_Direct_Map, -- Unary_Adding_Operator_ID or ABS_ID, NOT_ID
      L_Ana.Ada_Handled_Stmts => +handled_sequence_of_statements_ID,
      L_Ana.Ada_Interface_Kind_Limited => +LIMITED_ID,
      L_Ana.Ada_Interface_Kind_Protected => +PROTECTED_ID,
      L_Ana.Ada_Interface_Kind_Synchronized => +SYNCHRONIZED_ID,
      L_Ana.Ada_Interface_Kind_Task => +TASK_ID,
      L_Ana.Ada_Iter_Type_In => +IN_ID,
      L_Ana.Ada_Iter_Type_Of => +OF_ID,
      L_Ana.Ada_Library_Item => +compilation_unit_ID,
      L_Ana.Ada_Limited_Absent => No_Direct_Map,
      L_Ana.Ada_Limited_Present => +LIMITED_ID,
      L_Ana.Ada_For_Loop_Spec => +iterator_specification_ID,
      L_Ana.Ada_While_Loop_Spec => +expression_ID,
      L_Ana.Ada_Mode_Default => +mode_opt_ID,
      L_Ana.Ada_Mode_In => +IN_ID,
      L_Ana.Ada_Mode_In_Out => +mode_opt_ID,
      L_Ana.Ada_Mode_Out => +OUT_ID,
      L_Ana.Ada_Not_Null_Absent => No_Direct_Map,
      L_Ana.Ada_Not_Null_Present => +null_exclusion_opt_ID,
      L_Ana.Ada_Null_Component_Decl => +NULL_ID,
      L_Ana.Ada_Op_Abs => +ABS_ID,
      L_Ana.Ada_Op_And => +AND_ID,
      L_Ana.Ada_Op_And_Then => No_Direct_Map, -- part of Relation_And_Then_List_ID
      L_Ana.Ada_Op_Concat => +AMPERSAND_ID,
      L_Ana.Ada_Op_Div => +SLASH_ID,
      L_Ana.Ada_Op_Double_Dot => +DOT_DOT_ID,
      L_Ana.Ada_Op_Eq => +EQUAL_ID,
      L_Ana.Ada_Op_Gt => +GREATER_ID,
      L_Ana.Ada_Op_Gte => +GREATER_EQUAL_ID,
      L_Ana.Ada_Op_In => +IN_ID,
      L_Ana.Ada_Op_Lt => +LESS_ID,
      L_Ana.Ada_Op_Lte => +LESS_EQUAL_ID,
      L_Ana.Ada_Op_Minus => +MINUS_ID,
      L_Ana.Ada_Op_Mod => +MOD_ID,
      L_Ana.Ada_Op_Mult => +STAR_ID,
      L_Ana.Ada_Op_Neq => +SLASH_EQUAL_ID,
      L_Ana.Ada_Op_Not => +NOT_ID,
      L_Ana.Ada_Op_Not_In => No_Direct_Map, -- part of relation
      L_Ana.Ada_Op_Or => +OR_ID,
      L_Ana.Ada_Op_Or_Else => No_Direct_Map, -- relation_or_else_list
      L_Ana.Ada_Op_Plus => +PLUS_ID,
      L_Ana.Ada_Op_Pow => +STAR_STAR_ID,
      L_Ana.Ada_Op_Rem => +REM_ID,
      L_Ana.Ada_Op_Xor => +XOR_ID,
      L_Ana.Ada_Others_Designator => +OTHERS_ID,
      L_Ana.Ada_Overriding_Not_Overriding => +overriding_indicator_opt_ID,
      L_Ana.Ada_Overriding_Overriding => +overriding_indicator_opt_ID,
      L_Ana.Ada_Overriding_Unspecified => +overriding_indicator_opt_ID,
      L_Ana.Ada_Params => +formal_part_ID,
      L_Ana.Ada_Pragma_Node => No_Direct_Map, -- ?
      L_Ana.Ada_Private_Absent => No_Direct_Map,
      L_Ana.Ada_Private_Present => +PRIVATE_ID,
      L_Ana.Ada_Protected_Absent => No_Direct_Map,
      L_Ana.Ada_Protected_Present => +PROTECTED_ID,
      L_Ana.Ada_Protected_Def => +protected_definition_ID,
      L_Ana.Ada_Quantifier_All => +ALL_ID,
      L_Ana.Ada_Quantifier_Some => +SOME_ID,
      L_Ana.Ada_Range_Spec => No_Direct_Map, -- part of range_g
      L_Ana.Ada_Renaming_Clause => No_Direct_Map, -- part of renaming_declaration
      L_Ana.Ada_Reverse_Absent => No_Direct_Map,
      L_Ana.Ada_Reverse_Present => +REVERSE_ID,
      L_Ana.Ada_Select_When_Part => No_Direct_Map, -- part of select_statement
      L_Ana.Ada_Accept_Stmt => +accept_statement_ID,
      L_Ana.Ada_Accept_Stmt_With_Stmts => +accept_statement_ID,
      L_Ana.Ada_For_Loop_Stmt => +loop_statement_ID,
      L_Ana.Ada_Loop_Stmt => +loop_statement_ID,
      L_Ana.Ada_While_Loop_Stmt => +loop_statement_ID,
      L_Ana.Ada_Begin_Block => +block_statement_ID,
      L_Ana.Ada_Decl_Block => +block_statement_ID,
      L_Ana.Ada_Case_Stmt => +case_statement_ID,
      L_Ana.Ada_Extended_Return_Stmt => +extended_return_statement_ID,
      L_Ana.Ada_If_Stmt => +if_statement_ID,
      L_Ana.Ada_Named_Stmt => +block_statement_ID,
      L_Ana.Ada_Select_Stmt => +select_statement_ID,
      L_Ana.Ada_Error_Stmt => No_Direct_Map, -- part of error handling; insert sequence_of_statements
      L_Ana.Ada_Abort_Stmt => +simple_statement_ID,
      L_Ana.Ada_Assign_Stmt => +assignment_statement_ID,
      L_Ana.Ada_Call_Stmt => +procedure_call_statement_ID,
      L_Ana.Ada_Delay_Stmt => +simple_statement_ID,
      L_Ana.Ada_Exit_Stmt => +exit_statement_ID,
      L_Ana.Ada_Goto_Stmt => +simple_statement_ID,
      L_Ana.Ada_Label => +goto_label_ID,
      L_Ana.Ada_Null_Stmt => +simple_statement_ID,
      L_Ana.Ada_Raise_Stmt => +raise_statement_ID,
      L_Ana.Ada_Requeue_Stmt => +requeue_statement_ID,
      L_Ana.Ada_Return_Stmt => +simple_return_statement_ID,
      L_Ana.Ada_Terminate_Alternative => +select_alternative_ID,
      L_Ana.Ada_Subp_Kind_Function => No_Direct_Map, -- part of generic_renaming_declaration or generic_instantiation
      L_Ana.Ada_Subp_Kind_Procedure => No_Direct_Map, -- part of generic_renaming_declaration or generic_instantiation
      L_Ana.Ada_Subunit => +subunit_ID,
      L_Ana.Ada_Synchronized_Absent => No_Direct_Map,
      L_Ana.Ada_Synchronized_Present => +SYNCHRONIZED_ID,
      L_Ana.Ada_Tagged_Absent => No_Direct_Map,
      L_Ana.Ada_Tagged_Present => +TAGGED_ID,
      L_Ana.Ada_Task_Def => +task_definition_ID,
      L_Ana.Ada_Access_To_Subp_Def => +access_definition_ID,
      L_Ana.Ada_Anonymous_Type_Access_Def => No_Direct_Map, --  part of ?
      L_Ana.Ada_Type_Access_Def => +access_definition_ID,
      L_Ana.Ada_Array_Type_Def => +array_type_definition_ID,
      L_Ana.Ada_Derived_Type_Def => +derived_type_definition_ID,
      L_Ana.Ada_Enum_Type_Def => +enumeration_type_definition_ID,
      L_Ana.Ada_Formal_Discrete_Type_Def => +formal_type_definition_ID,
      L_Ana.Ada_Interface_Type_Def => +interface_type_definition_ID,
      L_Ana.Ada_Mod_Int_Type_Def => +type_definition_ID,
      L_Ana.Ada_Private_Type_Def => No_Direct_Map, -- part of private_type_declaration
      L_Ana.Ada_Decimal_Fixed_Point_Def => +type_definition_ID,
      L_Ana.Ada_Floating_Point_Def => +type_definition_ID,
      L_Ana.Ada_Ordinary_Fixed_Point_Def => +type_definition_ID,
      L_Ana.Ada_Record_Type_Def => +type_definition_ID,
      L_Ana.Ada_Signed_Int_Type_Def => +type_definition_ID,
      L_Ana.Ada_Anonymous_Type => No_Direct_Map, -- part of subtype_indication, access_definition
      L_Ana.Ada_Subtype_Indication => +subtype_indication_ID,
      L_Ana.Ada_Constrained_Subtype_Indication => +subtype_indication_ID,
      L_Ana.Ada_Discrete_Subtype_Indication => +subtype_indication_ID,
      L_Ana.Ada_Unconstrained_Array_Index => No_Direct_Map, --  part of arrary_type_definition
      L_Ana.Ada_Until_Absent => No_Direct_Map,
      L_Ana.Ada_Until_Present => +UNTIL_ID,
      L_Ana.Ada_Use_Package_Clause => +use_clause_ID,
      L_Ana.Ada_Use_Type_Clause => +use_clause_ID,
      L_Ana.Ada_Variant => +variant_ID,
      L_Ana.Ada_Variant_Part => +variant_part_ID,
      L_Ana.Ada_With_Clause => +with_clause_ID,
      L_Ana.Ada_With_Private_Absent => No_Direct_Map,  --  part of derived_type_definition
      L_Ana.Ada_With_Private_Present => No_Direct_Map  --  part of derived_type_definition
     );

   function To_Byte_Region (First, Last : in Integer) return WisiToken.Buffer_Region
   is begin
      --  Libadalang buffer indices are for Wide_Wide_Character, not bytes.
      --  We just ignore that, and copy them.
      return (WisiToken.Buffer_Pos (First), WisiToken.Buffer_Pos (Last));
   end To_Byte_Region;

   procedure To_Non_Grammar
     (TDH     : in     L_Lex.Token_Data_Handlers.Token_Data_Handler;
      Trivia  : in     L_Lex.Token_Data_Handlers.Token_Index_Vectors.Elements_Array;
      W_Token : in out Augmented_Token)
   is
      use WisiToken;
   begin
      for I of Trivia loop
         declare
            use all type L_Lex.Token_Kind;
            L_Token : L_Lex.Token_Data_Type renames TDH.Trivias.Get (Integer (I)).T;
            Line    : constant Line_Number_Type := Line_Number_Type (L_Token.Sloc_Range.Start_Line);
         begin
            case L_Token.Kind is
            when L_Lex.Ada_Comment =>
               W_Token.Non_Grammar.Append
                 ((ID    => +COMMENT_ID,
                   Line  => Line,
                   Col   => Ada.Text_IO.Count (L_Token.Sloc_Range.Start_Column),
                   First => Line /= W_Token.Line));

            when Ada_Whitespace =>
               null;

            when others =>
               raise WisiToken.Programmer_Error with "trivia kind: '" & L_Lex.Token_Kind'Image (L_Token.Kind) & "'";
            end case;
         end;
      end loop;
   end To_Non_Grammar;

   procedure To_WisiToken_Tree
     (Ast_Root  : in     Standard.Libadalang.Analysis.Ada_Node;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Terminals : in     WisiToken.Base_Token_Arrays.Vector;
      Data      : in     Wisi.Parse_Data_Type)
   is
      use Standard.Libadalang.Analysis;

      subtype Terminal is WisiToken.Token_ID range Data.Descriptor.First_Terminal .. Data.Descriptor.Last_Terminal;

      function Create_Tree_Node
        (Ast_Node    : in     Ada_Node;
         Terminals   : in     WisiToken.Base_Token_Arrays.Vector;
         Tree        : in out WisiToken.Syntax_Trees.Tree)
        return WisiToken.Syntax_Trees.Valid_Node_Index
      is
         --  Create a Tree node matching Ast_Node
         W_Token_ID : constant WisiToken.Token_ID := L_Node_To_Wisi_ID (Kind (Ast_Node));
      begin

         if W_Token_ID = No_Direct_Map then
            raise SAL.Not_Implemented;

         elsif W_Token_ID in Terminal then
            declare
               L_Token_Index : constant L_Lex.Token_Data_Handlers.Token_Index := Index (Token_Start (Ast_Node));
               W_Token_Index : constant WisiToken.Token_Index := WisiToken.Token_Index (L_Token_Index);
            begin
               return Tree.Add_Terminal (W_Token_Index, Terminals);
            end;

         else
            declare
               use WisiToken.Syntax_Trees;
               Ast_Children  : constant Ada_Node_Array := Ast_Node.Children;
               Tree_Children : Valid_Node_Index_Array
                 (SAL.Base_Peek_Type (Ast_Children'First) .. SAL.Base_Peek_Type (Ast_Children'Last));
            begin
               for I in Ast_Children'Range loop
                  Tree_Children (SAL.Base_Peek_Type (I)) := Create_Tree_Node
                    (Ast_Node    => Ast_Children (I),
                     Terminals   => Terminals,
                     Tree        => Tree);
               end loop;

               return Tree.Add_Nonterm
                 (Production      => WisiToken.Invalid_Production_ID,
                  --  FIXME: Production only used for debug messages?, add later if needed
                  Children        => Tree_Children,
                  Action          => null, --  FIXME: lookup action in?
                  Default_Virtual => False);
            end;
         end if;
      end Create_Tree_Node;

   begin
      --  The operations in WisiToken.Syntax_Trees support building the tree
      --  bottom up, so we walk thru Ast that way.

      if Ast_Root /= No_Ada_Node and then Ast_Root.Kind = Ada_Compilation_Unit then
         declare
            W_Root : WisiToken.Syntax_Trees.Valid_Node_Index := Create_Tree_Node
              (Ast_Node    => Ast_Root,
               Terminals   => Terminals,
               Tree        => Tree);
            pragma Unreferenced (W_Root);
         begin
            null;
         end;
      else
         raise WisiToken.Programmer_Error;
      end if;
   end To_WisiToken_Tree;

   ----------
   --  Public subprograms

   overriding procedure Reset_With_String (Lexer : in out Wisi.Libadalang.Lexer; Input : in String)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer : in out Wisi.Libadalang.Lexer;
      Input : in     Ada.Strings.Unbounded.String_Access)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_String_Access;

   overriding procedure Reset_With_File (Lexer : in out Wisi.Libadalang.Lexer; File_Name : in String)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Wisi.Libadalang.Lexer)
   is begin
      raise SAL.Not_Implemented;
   end Reset;

   overriding procedure Discard_Rest_Of_Input (Lexer : in out Wisi.Libadalang.Lexer)
   is begin
      raise SAL.Not_Implemented;
   end Discard_Rest_Of_Input;

   overriding
   function Buffer_Text
     (Lexer       : in Wisi.Libadalang.Lexer;
      Byte_Region : in WisiToken.Buffer_Region)
     return String
   is begin
      return Langkit_Support.Text.Transcode
        (Text => L_Lex.Text
           (Lexer.TDH.all,
            (Kind         => L_Lex.Ada_Comment,
             Source_First => Integer (Byte_Region.First),
             Source_Last  => Integer (Byte_Region.Last),
             Symbol       => null,
             Sloc_Range   => Langkit_Support.Slocs.No_Source_Location_Range)),
        Charset => "UTF-8");
   end Buffer_Text;

   overriding procedure Parse (Parser : aliased in out Wisi.Libadalang.Parser)
   is
      use Standard.Libadalang.Analysis;
      use WisiToken;

      Ctx : constant Analysis_Context := Create (With_Trivia => True); --  FIXME: specify utf-8 charset

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (Parser.User_Data.all);

      TDH : access constant L_Lex.Token_Data_Handlers.Token_Data_Handler;
   begin
      if WisiToken.Trace_Parse > Detail then
         GNATCOLL.Traces.Set_Active (L_Ana.Main_Trace, True);
      end if;

      Parser.Unit := Get_From_File (Ctx, -Parser.Source_File_Name, "", True, Rule => Compilation_Rule);
      TDH         := L_Ana.More.TDH (Parser.Unit);

      Wisi.Libadalang.Lexer (Parser.Lexer.all).TDH := TDH;

      declare
         L_Last_Token_Index : constant L_Lex.Token_Data_Handlers.Token_Index := L_Lex.Token_Data_Handlers.Last_Token
           (TDH.all);
         L_Last_Token : constant L_Lex.Token_Data_Type := L_Lex.Token_Data_Handlers.Get_Token
           (TDH.all, L_Last_Token_Index);
         W_Last_Line : constant Line_Number_Type := Line_Number_Type (L_Last_Token.Sloc_Range.End_Line);
         --  FIXME: includes comments after last token?
      begin
         Parser.Terminals.Set_First_Last (1, Token_Index (L_Last_Token_Index));
         Data.Terminals.Set_First_Last (1, Token_Index (L_Last_Token_Index));
         Data.Line_Begin_Pos.Set_First_Last (1, W_Last_Line);
         Data.Line_Paren_State.Set_First_Last (1, W_Last_Line);
      end;

      declare
         use L_Lex.Token_Data_Handlers;
         Prev_Line : Line_Number_Type := Line_Number_Type'First;
      begin
         for I in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
            declare
               L_Token_Data : constant L_Lex.Token_Data_Type              := Get_Token
                 (TDH.all, L_Lex.Token_Data_Handlers.Token_Index (I));
               Trivia       : constant Token_Index_Vectors.Elements_Array := Get_Trivias
                 (TDH.all, L_Lex.Token_Data_Handlers.Token_Index (I));

               W_Base_Token : constant Base_Token :=
                 (ID          => L_Terminal_To_Wisi_ID (L_Token_Data.Kind),
                  Byte_Region => To_Byte_Region (L_Token_Data.Source_First, L_Token_Data.Source_Last),
                  Line        => WisiToken.Line_Number_Type (L_Token_Data.Sloc_Range.Start_Line),
                  Column      => Ada.Text_IO.Count (L_Token_Data.Sloc_Range.Start_Column),
                  Char_Region => Null_Buffer_Region);

               First : constant Boolean := Prev_Line /= W_Base_Token.Line;
            begin
               Parser.Terminals (I) := W_Base_Token;

               if First then
                  Data.Line_Paren_State (W_Base_Token.Line + 1) := Data.Current_Paren_State;
               end if;

               Data.Terminals.Replace_Element
                 (I,
                  (W_Base_Token with
                   First                       => Prev_Line = W_Base_Token.Line,
                   Paren_State                 => Data.Current_Paren_State,
                   First_Terminals_Index       => I,
                   Last_Terminals_Index        => I,
                   First_Indent_Line           => (if First then W_Base_Token.Line else Invalid_Line_Number),
                   Last_Indent_Line            => (if First then W_Base_Token.Line else Invalid_Line_Number),
                   First_Trailing_Comment_Line => <>,
                   Last_Trailing_Comment_Line  => <>,
                   Non_Grammar                 => <>));

               To_Non_Grammar (TDH.all, Trivia, Data.Terminals (I));

               if W_Base_Token.ID = Descriptor.Left_Paren_ID then
                  Data.Current_Paren_State := Data.Current_Paren_State + 1;

               elsif W_Base_Token.ID = Descriptor.Right_Paren_ID then
                  Data.Current_Paren_State := Data.Current_Paren_State - 1;
               end if;

               Prev_Line := W_Base_Token.Line;
            end;
         end loop;
      end;

      if Root (Parser.Unit) = No_Ada_Node then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "parse failed; no libadalang tree");
      else
         To_WisiToken_Tree (Root (Parser.Unit), Parser.Tree, Parser.Terminals, Data);
      end if;
   end Parse;

   overriding function Any_Errors (Parser : in Wisi.Libadalang.Parser) return Boolean
   is begin
      return Standard.Libadalang.Analysis.Has_Diagnostics (Parser.Unit);
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in Wisi.Libadalang.Parser; Input_File_Name : in String)
   is
      pragma Unreferenced (Input_File_Name);
   begin
      for D of Standard.Libadalang.Analysis.Diagnostics (Parser.Unit) loop
         --  FIXME: convert to Parser.Errors, let main put thru Wisi_Runtime for elisp
         Ada.Text_IO.Put_Line (Standard.Libadalang.Analysis.Format_GNU_Diagnostic (Parser.Unit, D));
      end loop;
   end Put_Errors;

   overriding procedure Execute_Actions (Parser : in out Wisi.Libadalang.Parser)
   is
      use all type WisiToken.Syntax_Trees.Node_Index;

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;
      Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (Parser.User_Data.all);

      procedure Process_Node
        (Tree : in out WisiToken.Syntax_Trees.Tree;
         Node : in     WisiToken.Syntax_Trees.Valid_Node_Index)
      is
         use all type WisiToken.Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type WisiToken.Syntax_Trees.Semantic_Action;
            Tree_Children : constant WisiToken.Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Node);
         begin
            Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Data, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         if Parser.Tree.Root = WisiToken.Syntax_Trees.Invalid_Node_Index then
            Parser.Trace.Put_Line ("no root node");
         else
            Parser.Trace.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root, Descriptor));
         end if;
      end if;

      if Parser.Tree.Root /= WisiToken.Syntax_Trees.Invalid_Node_Index then
         Parser.Tree.Process_Tree (Process_Node'Access);
      end if;
   end Execute_Actions;

end Wisi.Libadalang;
