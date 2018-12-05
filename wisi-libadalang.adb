--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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
with Ada_Process_Actions;
with Ada_Process_External_Main;
pragma Warnings (Off, "license of withed unit ""GNATCOLL.Traces"" may be inconsistent");
with GNATCOLL.Traces;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Analysis.More;
package body Wisi.Libadalang is
   package L_Lex renames Standard.Libadalang.Lexer;
   package L_Ana renames Standard.Libadalang.Analysis;
   use all type WisiToken.Token_ID;
   use all type Ada_Process_Actions.Token_Enum_ID;
   use all type L_Lex.Token_Kind;
   use all type L_Ana.Ada_Node_Kind_Type;

   L_Terminal_To_Wisi_ID : constant array (L_Lex.Token_Kind) of WisiToken.Token_ID :=
     (Ada_Termination    => +Wisi_EOI_ID,
      Ada_Lexing_Failure => WisiToken.Invalid_Token_ID,
      Ada_Identifier     => +IDENTIFIER_ID,
      Ada_All            => +ALL_ID,
      Ada_Abort          => +ABORT_ID,
      Ada_Else           => +ELSE_ID,
      Ada_New            => +NEW_ID,
      Ada_Return         => +RETURN_ID,
      Ada_Abs            => +ABS_ID,
      Ada_Elsif          => +ELSIF_ID,
      Ada_Not            => +NOT_ID,
      Ada_Reverse        => +REVERSE_ID,
      Ada_Abstract       => +ABSTRACT_ID,
      Ada_End            => +END_ID,
      Ada_Null           => +NULL_ID,
      Ada_Accept         => +ACCEPT_ID,
      Ada_Entry          => +ENTRY_ID,
      Ada_Select         => +SELECT_ID,
      Ada_Access         => +ACCESS_ID,
      Ada_Exception      => +EXCEPTION_ID,
      Ada_Of             => +OF_ID,
      Ada_Separate       => +SEPARATE_ID,
      Ada_Aliased        => +ALIASED_ID,
      Ada_Exit           => +EXIT_ID,
      Ada_Or             => +OR_ID,
      Ada_Some           => +SOME_ID,
      Ada_Others         => +OTHERS_ID,
      Ada_Subtype        => +SUBTYPE_ID,
      Ada_And            => +AND_ID,
      Ada_For            => +FOR_ID,
      Ada_Out            => +OUT_ID,
      Ada_Array          => +ARRAY_ID,
      Ada_Function       => +FUNCTION_ID,
      Ada_At             => +AT_ID,
      Ada_Tagged         => +TAGGED_ID,
      Ada_Generic        => +GENERIC_ID,
      Ada_Package        => +PACKAGE_ID,
      Ada_Task           => +TASK_ID,
      Ada_Begin          => +BEGIN_ID,
      Ada_Goto           => +GOTO_ID,
      Ada_Pragma         => +PRAGMA_ID,
      Ada_Terminate      => +TERMINATE_ID,
      Ada_Body           => +BODY_ID,
      Ada_Private        => +PRIVATE_ID,
      Ada_Then           => +THEN_ID,
      Ada_If             => +IF_ID,
      Ada_Procedure      => +PROCEDURE_ID,
      Ada_Type           => +TYPE_ID,
      Ada_Case           => +CASE_ID,
      Ada_In             => +IN_ID,
      Ada_Constant       => +CONSTANT_ID,
      Ada_Is             => +IS_ID,
      Ada_Raise          => +RAISE_ID,
      Ada_Use            => +USE_ID,
      Ada_Declare        => +DECLARE_ID,
      Ada_Range          => +RANGE_ID,
      Ada_Delay          => +DELAY_ID,
      Ada_Until          => +UNTIL_ID,
      Ada_Limited        => +LIMITED_ID,
      Ada_Record         => +RECORD_ID,
      Ada_When           => +WHEN_ID,
      Ada_Delta          => +DELTA_ID,
      Ada_Loop           => +LOOP_ID,
      Ada_Rem            => +REM_ID,
      Ada_While          => +WHILE_ID,
      Ada_Digits         => +DIGITS_ID,
      Ada_Renames        => +RENAMES_ID,
      Ada_Do             => +DO_ID,
      Ada_Mod            => +MOD_ID,
      Ada_Requeue        => +REQUEUE_ID,
      Ada_Xor            => +XOR_ID,
      Ada_Par_Close      => +LEFT_PAREN_ID,
      Ada_Par_Open       => +RIGHT_PAREN_ID,
      Ada_Semicolon      => +SEMICOLON_ID,
      Ada_Colon          => +COLON_ID,
      Ada_Comma          => +COMMA_ID,
      Ada_Doubledot      => +DOT_DOT_ID,
      Ada_Dot            => +DOT_ID,
      Ada_Diamond        => +BOX_ID,
      Ada_Lte            => +LESS_EQUAL_ID,
      Ada_Gte            => +GREATER_EQUAL_ID,
      Ada_Arrow          => +EQUAL_GREATER_ID,
      Ada_Equal          => +EQUAL_ID,
      Ada_Lt             => +LESS_ID,
      Ada_Gt             => +GREATER_ID,
      Ada_Plus           => +PLUS_ID,
      Ada_Minus          => +MINUS_ID,
      Ada_Power          => +STAR_STAR_ID,
      Ada_Mult           => +STAR_ID,
      Ada_Amp            => +AMPERSAND_ID,
      Ada_Notequal       => +SLASH_EQUAL_ID,
      Ada_Divide         => +SLASH_ID,
      Ada_Tick           => +TICK_1_ID,
      Ada_Pipe           => +BAR_ID,
      Ada_Assign         => +COLON_EQUAL_ID,
      Ada_Label_Start    => +LESS_LESS_ID,
      Ada_Label_End      => +GREATER_GREATER_ID,
      Ada_Target         => +AT_ID,
      Ada_String         => +STRING_LITERAL_ID,
      Ada_Char           => +CHARACTER_LITERAL_ID,
      Ada_With           => +WITH_ID,
      Ada_Decimal        => +NUMERIC_LITERAL_ID,
      Ada_Integer        => +NUMERIC_LITERAL_ID,
      Ada_Comment        => +COMMENT_ID,
      Ada_Prep_Line      => +COMMENT_ID,
      Ada_Whitespace     => +WHITESPACE_ID);

   No_Direct_Map : constant WisiToken.Token_ID := WisiToken.Invalid_Token_ID - 1;

   L_Node_To_Wisi_ID : constant array (L_Ana.Ada_Node_Kind_Type) of WisiToken.Token_ID :=
     (Ada_Abort_Absent => No_Direct_Map,
      Ada_Abort_Present => +ABORT_ID,
      Ada_Abstract_Absent => No_Direct_Map,
      Ada_Abstract_Present => +ABSTRACT_ID,
      Ada_Ada_Node_List => No_Direct_Map, -- ?
      Ada_Alternatives_List => No_Direct_Map, -- ?,
      Ada_Constraint_List => +discrete_subtype_definition_list_ID,
      Ada_Decl_List => +declarations_ID,
      Ada_Stmt_List => +sequence_of_statements_ID,
      Ada_Aspect_Assoc_List => +association_list_ID,
      Ada_Base_Assoc_List => +association_list_ID,
      Ada_Assoc_List => +association_list_ID,
      Ada_Case_Expr_Alternative_List => +case_expression_alternative_list_ID,
      Ada_Case_Stmt_Alternative_List => +case_statement_alternative_list_ID,
      Ada_Compilation_Unit_List => +compilation_unit_list_ID,
      Ada_Component_Clause_List => +component_clause_list_ID,
      Ada_Contract_Case_Assoc_List => +association_list_ID,
      Ada_Defining_Name_List => +identifier_list_ID,
      Ada_Discriminant_Spec_List => +discriminant_specification_list_ID,
      Ada_Elsif_Expr_Part_List => +elsif_expression_list_ID,
      Ada_Elsif_Stmt_Part_List => +elsif_statement_list_ID,
      Ada_Enum_Literal_Decl_List => +enumeration_literal_list_ID,
      Ada_Expr_Alternatives_List => +membership_choice_list_ID,
      Ada_Discriminant_Choice_List => +discrete_choice_list_ID,
      Ada_Name_List => +name_list_ID,
      Ada_Parent_List => +interface_list_ID,
      Ada_Param_Spec_List => +parameter_specification_list_ID,
      Ada_Pragma_Node_List => No_Direct_Map, --  statement or declaration
      Ada_Select_When_Part_List => +select_alternative_ID,
      Ada_Unconstrained_Array_Index_List => +index_subtype_definition_list_ID,
      Ada_Variant_List => +variant_list_ID,
      Ada_Aliased_Absent => No_Direct_Map,
      Ada_Aliased_Present => +ALIASED_ID,
      Ada_All_Absent => No_Direct_Map,
      Ada_All_Present => +ALL_ID,
      Ada_Constrained_Array_Indices => +discrete_subtype_definition_list_ID,
      Ada_Unconstrained_Array_Indices => +index_subtype_definition_list_ID,
      Ada_Aspect_Assoc => +association_list_ID,
      Ada_At_Clause => +at_clause_ID,
      Ada_Attribute_Def_Clause => +aspect_clause_ID,
      Ada_Enum_Rep_Clause => +enumeration_representation_clause_ID,
      Ada_Record_Rep_Clause => +record_representation_clause_ID,
      Ada_Aspect_Spec => +aspect_specification_opt_ID,
      Ada_Contract_Case_Assoc => +association_list_ID,
      Ada_Pragma_Argument_Assoc => +association_list_ID,
      Ada_Entry_Spec => No_Direct_Map, --  part of Entry_Declaration_ID
      Ada_Subp_Spec => +subprogram_specification_ID,
      Ada_Component_List => +component_list_ID,
      Ada_Known_Discriminant_Part => +discriminant_part_opt_ID,
      Ada_Unknown_Discriminant_Part => +discriminant_part_opt_ID,
      Ada_Generic_Formal_Part => +generic_formal_part_ID,
      Ada_Null_Record_Def => +record_definition_ID,
      Ada_Record_Def => +record_definition_ID,
      Ada_Aggregate_Assoc => +association_opt_ID,
      Ada_Multi_Dim_Array_Assoc => +association_opt_ID,
      Ada_Discriminant_Assoc => +association_opt_ID,
      Ada_Param_Assoc => +association_opt_ID,
      Ada_Component_Decl => +component_declaration_ID,
      Ada_Discriminant_Spec => +discriminant_specification_opt_ID,
      Ada_Generic_Formal_Obj_Decl => +formal_object_declaration_ID,
      Ada_Generic_Formal_Package => +formal_package_declaration_ID,
      Ada_Generic_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      Ada_Generic_Formal_Type_Decl => +formal_type_declaration_ID,
      Ada_Param_Spec => +parameter_specification_ID,
      Ada_Generic_Package_Internal => +generic_package_declaration_ID,
      Ada_Package_Decl => +package_declaration_ID,
      Ada_Classwide_Type_Decl => No_Direct_Map, -- ?
      Ada_Incomplete_Type_Decl => +incomplete_type_declaration_ID,
      Ada_Incomplete_Tagged_Type_Decl => +incomplete_type_declaration_ID,
      Ada_Protected_Type_Decl => +protected_type_declaration_ID,
      Ada_Subtype_Decl => +subtype_declaration_ID,
      Ada_Task_Type_Decl => +task_type_declaration_ID,
      Ada_Single_Task_Type_Decl => +single_task_declaration_ID,
      Ada_Type_Decl => +type_declaration_ID,
      Ada_Anonymous_Type_Decl => No_Direct_Map, -- part of Object_Declaration_ID
      Ada_Synth_Anonymous_Type_Decl => +object_declaration_ID,
      Ada_Abstract_Subp_Decl => +abstract_subprogram_declaration_ID,
      Ada_Abstract_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      Ada_Concrete_Formal_Subp_Decl => +formal_subprogram_declaration_ID,
      Ada_Null_Subp_Decl => +null_procedure_declaration_ID,
      Ada_Subp_Decl => +subprogram_declaration_ID,
      Ada_Subp_Renaming_Decl => +subprogram_renaming_declaration_ID,
      Ada_Generic_Subp_Internal => +generic_subprogram_declaration_ID,
      Ada_Expr_Function => +expression_function_declaration_ID,
      Ada_Subp_Body => +subprogram_body_ID,
      Ada_Package_Body_Stub => +package_body_stub_ID,
      Ada_Protected_Body_Stub => +protected_body_stub_ID,
      Ada_Subp_Body_Stub => +subprogram_body_stub_ID,
      Ada_Task_Body_Stub => +task_body_stub_ID,
      Ada_Entry_Body => +entry_body_ID,
      Ada_Package_Body => +package_body_ID,
      Ada_Protected_Body => +protected_body_ID,
      Ada_Task_Body => +task_body_ID,
      Ada_Entry_Decl => +entry_declaration_ID,
      Ada_Enum_Literal_Decl => +enumeration_literal_ID,
      Ada_Error_Decl => No_Direct_Map, -- part of error handling; insert ?
      Ada_Exception_Decl => +exception_declaration_ID,
      Ada_Exception_Handler => +exception_handler_ID,
      Ada_For_Loop_Var_Decl => No_Direct_Map, --  part of Iterator_Specification_ID
      Ada_Generic_Package_Decl => +generic_package_declaration_ID,
      Ada_Generic_Subp_Decl => +generic_subprogram_declaration_ID,
      Ada_Generic_Package_Instantiation => +generic_instantiation_ID,
      Ada_Generic_Subp_Instantiation => +generic_instantiation_ID,
      Ada_Generic_Package_Renaming_Decl => +generic_renaming_declaration_ID,
      Ada_Generic_Subp_Renaming_Decl => +generic_renaming_declaration_ID,
      Ada_Label_Decl => +IDENTIFIER_ID,
      Ada_Named_Stmt_Decl => +block_statement_ID,
      Ada_Number_Decl => +declaration_ID,
      Ada_Object_Decl => +object_declaration_ID,
      Ada_Extended_Return_Stmt_Object_Decl => +extended_return_object_declaration_ID,
      Ada_Package_Renaming_Decl => +package_renaming_declaration_ID,
      Ada_Single_Protected_Decl => +single_protected_declaration_ID,
      Ada_Single_Task_Decl => +single_task_declaration_ID,
      Ada_Case_Stmt_Alternative => +case_statement_alternative_ID,
      Ada_Compilation_Unit => +compilation_unit_ID,
      Ada_Component_Clause => +component_clause_ID,
      Ada_Component_Def => +component_definition_ID,
      Ada_Constant_Absent => No_Direct_Map,
      Ada_Constant_Present => +CONSTANT_ID,
      Ada_Delta_Constraint => +constraint_ID,
      Ada_Digits_Constraint => +constraint_ID,
      Ada_Discriminant_Constraint => +constraint_ID,
      Ada_Index_Constraint => +index_constraint_ID,
      Ada_Range_Constraint => +constraint_ID,
      Ada_Declarative_Part => +declarative_part_opt_ID,
      Ada_Private_Part => +declarative_part_opt_ID,
      Ada_Public_Part => +declarative_part_opt_ID,
      Ada_Elsif_Expr_Part => +elsif_expression_list_ID,
      Ada_Elsif_Stmt_Part => +elsif_statement_list_ID,
      Ada_Entry_Index_Spec => No_Direct_Map, -- part of Entry_Body_Formal_Part_ID
      Ada_Allocator => +primary_ID,
      Ada_Aggregate => +aggregate_ID,
      Ada_Null_Record_Aggregate => +aggregate_ID,
      Ada_Bin_Op => No_Direct_Map, -- Binary_Adding_Operator_ID or multiplying_operator
      Ada_Relation_Op => +relational_operator_ID,
      Ada_Box_Expr => +BOX_ID,
      Ada_Case_Expr => +case_expression_ID,
      Ada_Case_Expr_Alternative => +case_expression_alternative_ID,
      Ada_Contract_Cases => +actual_parameter_part_ID,
      Ada_If_Expr => +if_expression_ID,
      Ada_Membership_Expr => +relation_ID,
      Ada_Attribute_Ref => +attribute_reference_ID,
      Ada_Update_Attribute_Ref => +attribute_reference_ID,
      Ada_Call_Expr => +name_ID,
      Ada_Defining_Name => +selected_component_ID,
      Ada_Discrete_Subtype_Name => +membership_choice_ID,
      Ada_Dotted_Name => +selected_component_ID,
      Ada_End_Name => +name_ID,
      Ada_Explicit_Deref => +selected_component_ID,
      Ada_Qual_Expr => +qualified_expression_ID,
      Ada_Char_Literal => +CHARACTER_LITERAL_ID,
      Ada_Identifier => +IDENTIFIER_ID,
      Ada_String_Literal => +STRING_LITERAL_ID,
      Ada_Null_Literal => +NULL_ID,
      Ada_Int_Literal => +NUMERIC_LITERAL_ID,
      Ada_Real_Literal => +NUMERIC_LITERAL_ID,
      Ada_Target_Name => +AT_ID,
      Ada_Paren_Expr => +paren_expression_ID,
      Ada_Quantified_Expr => +quantified_expression_ID,
      Ada_Raise_Expr => +raise_expression_ID,
      Ada_Un_Op => No_Direct_Map, -- Unary_Adding_Operator_ID or ABS_ID, NOT_ID
      Ada_Handled_Stmts => +handled_sequence_of_statements_ID,
      Ada_Interface_Kind_Limited => +LIMITED_ID,
      Ada_Interface_Kind_Protected => +PROTECTED_ID,
      Ada_Interface_Kind_Synchronized => +SYNCHRONIZED_ID,
      Ada_Interface_Kind_Task => +TASK_ID,
      Ada_Iter_Type_In => +IN_ID,
      Ada_Iter_Type_Of => +OF_ID,
      Ada_Library_Item => +compilation_unit_ID,
      Ada_Limited_Absent => No_Direct_Map,
      Ada_Limited_Present => +LIMITED_ID,
      Ada_For_Loop_Spec => +iterator_specification_ID,
      Ada_While_Loop_Spec => +expression_ID,
      Ada_Mode_Default => +mode_opt_ID,
      Ada_Mode_In => +IN_ID,
      Ada_Mode_In_Out => +mode_opt_ID,
      Ada_Mode_Out => +OUT_ID,
      Ada_Not_Null_Absent => No_Direct_Map,
      Ada_Not_Null_Present => +null_exclusion_opt_ID,
      Ada_Null_Component_Decl => +NULL_ID,
      Ada_Op_Abs => +ABS_ID,
      Ada_Op_And => +AND_ID,
      Ada_Op_And_Then => No_Direct_Map, -- part of Relation_And_Then_List_ID
      Ada_Op_Concat => +AMPERSAND_ID,
      Ada_Op_Div => +SLASH_ID,
      Ada_Op_Double_Dot => +DOT_DOT_ID,
      Ada_Op_Eq => +EQUAL_ID,
      Ada_Op_Gt => +GREATER_ID,
      Ada_Op_Gte => +GREATER_EQUAL_ID,
      Ada_Op_In => +IN_ID,
      Ada_Op_Lt => +LESS_ID,
      Ada_Op_Lte => +LESS_EQUAL_ID,
      Ada_Op_Minus => +MINUS_ID,
      Ada_Op_Mod => +MOD_ID,
      Ada_Op_Mult => +STAR_ID,
      Ada_Op_Neq => +SLASH_EQUAL_ID,
      Ada_Op_Not => +NOT_ID,
      Ada_Op_Not_In => No_Direct_Map, -- part of relation
      Ada_Op_Or => +OR_ID,
      Ada_Op_Or_Else => No_Direct_Map, -- relation_or_else_list
      Ada_Op_Plus => +PLUS_ID,
      Ada_Op_Pow => +STAR_STAR_ID,
      Ada_Op_Rem => +REM_ID,
      Ada_Op_Xor => +XOR_ID,
      Ada_Others_Designator => +OTHERS_ID,
      Ada_Overriding_Not_Overriding => +overriding_indicator_opt_ID,
      Ada_Overriding_Overriding => +overriding_indicator_opt_ID,
      Ada_Overriding_Unspecified => +overriding_indicator_opt_ID,
      Ada_Params => +formal_part_ID,
      Ada_Pragma_Node => No_Direct_Map, -- ?
      Ada_Private_Absent => No_Direct_Map,
      Ada_Private_Present => +PRIVATE_ID,
      Ada_Protected_Absent => No_Direct_Map,
      Ada_Protected_Present => +PROTECTED_ID,
      Ada_Protected_Def => +protected_definition_ID,
      Ada_Quantifier_All => +ALL_ID,
      Ada_Quantifier_Some => +SOME_ID,
      Ada_Range_Spec => No_Direct_Map, -- part of range_g
      Ada_Renaming_Clause => No_Direct_Map, -- part of renaming_declaration
      Ada_Reverse_Absent => No_Direct_Map,
      Ada_Reverse_Present => +REVERSE_ID,
      Ada_Select_When_Part => No_Direct_Map, -- part of select_statement
      Ada_Accept_Stmt => +accept_statement_ID,
      Ada_Accept_Stmt_With_Stmts => +accept_statement_ID,
      Ada_For_Loop_Stmt => +loop_statement_ID,
      Ada_Loop_Stmt => +loop_statement_ID,
      Ada_While_Loop_Stmt => +loop_statement_ID,
      Ada_Begin_Block => +block_statement_ID,
      Ada_Decl_Block => +block_statement_ID,
      Ada_Case_Stmt => +case_statement_ID,
      Ada_Extended_Return_Stmt => +extended_return_statement_ID,
      Ada_If_Stmt => +if_statement_ID,
      Ada_Named_Stmt => +block_statement_ID,
      Ada_Select_Stmt => +select_statement_ID,
      Ada_Error_Stmt => No_Direct_Map, -- part of error handling; insert sequence_of_statements
      Ada_Abort_Stmt => +simple_statement_ID,
      Ada_Assign_Stmt => +assignment_statement_ID,
      Ada_Call_Stmt => +procedure_call_statement_ID,
      Ada_Delay_Stmt => +simple_statement_ID,
      Ada_Exit_Stmt => +exit_statement_ID,
      Ada_Goto_Stmt => +simple_statement_ID,
      Ada_Label => +goto_label_ID,
      Ada_Null_Stmt => +simple_statement_ID,
      Ada_Raise_Stmt => +raise_statement_ID,
      Ada_Requeue_Stmt => +requeue_statement_ID,
      Ada_Return_Stmt => +simple_return_statement_ID,
      Ada_Terminate_Alternative => +select_alternative_ID,
      Ada_Subp_Kind_Function => No_Direct_Map, -- part of generic_renaming_declaration or generic_instantiation
      Ada_Subp_Kind_Procedure => No_Direct_Map, -- part of generic_renaming_declaration or generic_instantiation
      Ada_Subunit => +subunit_ID,
      Ada_Synchronized_Absent => No_Direct_Map,
      Ada_Synchronized_Present => +SYNCHRONIZED_ID,
      Ada_Tagged_Absent => No_Direct_Map,
      Ada_Tagged_Present => +TAGGED_ID,
      Ada_Task_Def => +task_definition_ID,
      Ada_Access_To_Subp_Def => +access_definition_ID,
      Ada_Anonymous_Type_Access_Def => No_Direct_Map, --  part of ?
      Ada_Type_Access_Def => +access_definition_ID,
      Ada_Array_Type_Def => +array_type_definition_ID,
      Ada_Derived_Type_Def => +derived_type_definition_ID,
      Ada_Enum_Type_Def => +enumeration_type_definition_ID,
      Ada_Formal_Discrete_Type_Def => +formal_type_definition_ID,
      Ada_Interface_Type_Def => +interface_type_definition_ID,
      Ada_Mod_Int_Type_Def => +type_definition_ID,
      Ada_Private_Type_Def => No_Direct_Map, -- part of private_type_declaration
      Ada_Decimal_Fixed_Point_Def => +type_definition_ID,
      Ada_Floating_Point_Def => +type_definition_ID,
      Ada_Ordinary_Fixed_Point_Def => +type_definition_ID,
      Ada_Record_Type_Def => +type_definition_ID,
      Ada_Signed_Int_Type_Def => +type_definition_ID,
      Ada_Anonymous_Type => No_Direct_Map, -- part of subtype_indication, access_definition
      Ada_Subtype_Indication => +subtype_indication_ID,
      Ada_Constrained_Subtype_Indication => +subtype_indication_ID,
      Ada_Discrete_Subtype_Indication => +subtype_indication_ID,
      Ada_Unconstrained_Array_Index => No_Direct_Map, --  part of arrary_type_definition
      Ada_Until_Absent => No_Direct_Map,
      Ada_Until_Present => +UNTIL_ID,
      Ada_Use_Package_Clause => +use_clause_ID,
      Ada_Use_Type_Clause => +use_clause_ID,
      Ada_Variant => +variant_ID,
      Ada_Variant_Part => +variant_part_ID,
      Ada_With_Clause => +with_clause_ID,
      Ada_With_Private_Absent => No_Direct_Map,
      Ada_With_Private_Present => No_Direct_Map  --  part of derived_type_definition
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
      subtype Child_Index is SAL.Base_Peek_Type range 0 .. 20;
      --  FIXME: '20' is max WisiToken grammar production token count; declare in external_main.
      type Create_Result (Count : Child_Index := 0) is record
         Nodes : WisiToken.Syntax_Trees.Valid_Node_Index_Array (1 .. Count);
      end record;

      function Create_Tree_Node
        (Ast_Node    : in     Ada_Node;
         Terminals   : in     WisiToken.Base_Token_Arrays.Vector;
         Tree        : in out WisiToken.Syntax_Trees.Tree)
        return Create_Result;

      function Ast_Children (Ast_Node : in Ada_Node) return Create_Result
      is
         --  Return syntax tree nodes for children of Ast_Node

         use all type SAL.Base_Peek_Type;
         use WisiToken.Syntax_Trees;
         Ast_Children  : constant Ada_Node_Array := Ast_Node.Children;
         Child_Result  : Create_Result;
         Tree_Children : Valid_Node_Index_Array (1 .. Child_Index'Last);
         Last          : SAL.Base_Peek_Type      := Tree_Children'First - 1;
      begin
         for I in Ast_Children'Range loop
            if Ast_Children (I) /= No_Ada_Node then
               Child_Result := Create_Tree_Node
                 (Ast_Node  => Ast_Children (I),
                  Terminals => Terminals,
                  Tree      => Tree);

               for I in 1 .. Child_Result.Count loop
                  Last := Last + 1;
                  Tree_Children (Last) := Child_Result.Nodes (I);
               end loop;
            end if;
         end loop;

         return (Last, Tree_Children (1 .. Last));
      end Ast_Children;

      function Create_Tree_Node
        (Ast_Node    : in     Ada_Node;
         Terminals   : in     WisiToken.Base_Token_Arrays.Vector;
         Tree        : in out WisiToken.Syntax_Trees.Tree)
        return Create_Result
      is
         --  Create a Tree node matching Ast_Node; Invalid_Node_Index if there
         --  is no matching node.
         W_Token_ID : constant WisiToken.Token_ID := L_Node_To_Wisi_ID (Kind (Ast_Node));
      begin

         if W_Token_ID = No_Direct_Map then
            case Kind (Ast_Node) is
            when Ada_Abort_Absent |
              Ada_Abstract_Present |
              Ada_Not_Null_Absent |
              Ada_Private_Absent |
              Ada_Protected_Absent |
              Ada_Reverse_Absent |
              Ada_Synchronized_Absent |
              Ada_Tagged_Absent |
              Ada_Until_Absent |
              Ada_With_Private_Absent |
              Ada_Aliased_Absent |
              Ada_All_Absent |
              Ada_Constant_Absent |
              Ada_Limited_Absent =>
               return (0, (1 .. 0 => WisiToken.Syntax_Trees.Invalid_Node_Index));

            when others =>
               return Ast_Children (Ast_Node);
            end case;

         elsif W_Token_ID in Terminal then
            declare
               L_Token_Index : constant L_Lex.Token_Data_Handlers.Token_Index := Index (Token_Start (Ast_Node));
               W_Token_Index : constant WisiToken.Token_Index := WisiToken.Token_Index (L_Token_Index);
            begin
               return (1, (1 => Tree.Add_Terminal (W_Token_Index, Terminals)));
            end;

         else
            declare
               use all type SAL.Base_Peek_Type;
               use WisiToken.Syntax_Trees;

               Tree_Children : constant Create_Result := Ast_Children (Ast_Node);
               Prod          : WisiToken.Production_ID := (W_Token_ID, 0);

               function Get_Action return WisiToken.Syntax_Trees.Semantic_Action
               is
                  use Ada_Process_External_Main;
               begin
                  if Actions (-Prod.LHS) = null then
                     return null;
                  end if;

                  for I in Actions (-Prod.LHS).all'Range loop
                     declare
                        use all type Ada.Containers.Count_Type;
                        Item : Action_Item renames Actions (-Prod.LHS)(I);
                     begin
                        --  FIXME: token_count not enough; see selected_component, name
                        if SAL.Base_Peek_Type (Item.Token_Count) = Tree_Children.Count then
                           Prod.RHS := I;
                           return Item.Action;
                        end if;
                     end;
                  end loop;
                  return null;
               end Get_Action;

            begin
               return
                 (Count => 1,
                  Nodes =>
                    (1 =>
                       Tree.Add_Nonterm
                         (Production      => Prod,
                          Children        => Tree_Children.Nodes,
                          Action          => Get_Action,
                          Default_Virtual => False)));
            end;
         end if;
      end Create_Tree_Node;

   begin
      --  The operations in WisiToken.Syntax_Trees support building the tree
      --  bottom up, so we walk thru Ast that way.

      if Ast_Root /= No_Ada_Node and then Ast_Root.Kind = Ada_Compilation_Unit then
         declare
            W_Root : Create_Result := Create_Tree_Node
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
         Parser.Terminals.Clear;
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

               Buf_Region : constant Buffer_Region := To_Byte_Region
                 (L_Token_Data.Source_First, L_Token_Data.Source_Last);
               --  Libadalang uses the Quex lexer, which converts the input text to
               --  32 bit characters. Buf_Region is the index of those characters;
               --  this is the same as Emacs utf-8 character position.

               W_Base_Token : constant Base_Token :=
                 (ID          => L_Terminal_To_Wisi_ID (L_Token_Data.Kind),
                  Byte_Region => Buf_Region,
                  Line        => WisiToken.Line_Number_Type (L_Token_Data.Sloc_Range.Start_Line),
                  Column      => Ada.Text_IO.Count (L_Token_Data.Sloc_Range.Start_Column),
                  Char_Region => Buf_Region);

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
         if WisiToken.Trace_Parse > Detail then
            L_Ana.Print (Root (Parser.Unit));
            New_Line;
         end if;

         Parser.Tree.Clear;

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
