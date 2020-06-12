--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS text_rep ada.wy
--

--  Copyright (C) 2013 - 2020 Free Software Foundation, Inc.

--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version.
--
--  This software is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with Ada_Process_Actions; use Ada_Process_Actions;
with WisiToken.Lexer.re2c;
with ada_re2c_c;
package body Ada_Process_LR1_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (ada_re2c_c.New_Lexer,
      ada_re2c_c.Free_Lexer,
      ada_re2c_c.Reset_Lexer,
      ada_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                         :    out WisiToken.Parse.LR.Parser.Parser;
      Language_Fixes                 : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set       : in     WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access;
      Text_Rep_File_Name : in String)
   is
      use WisiToken.Parse.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 110,
         First_Nonterminal => 111,
         Last_Nonterminal  => 336,
         Insert =>
           (4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
         Delete =>
           (4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Minimal_Complete_Cost_Delta => -3,
         Fast_Forward =>  2,
         Matching_Begin =>  3,
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Check_Limit => 4,
         Check_Delta_Limit => 100,
         Enqueue_Limit => 58000);

      function Actions return WisiToken.Parse.LR.Semantic_Action_Array_Arrays.Vector
      is begin
         return Acts : WisiToken.Parse.LR.Semantic_Action_Array_Arrays.Vector do
            Acts.Set_First_Last (111, 336);
            Acts (115).Set_First_Last (0, 0);
            Acts (115)(0) := (abstract_subprogram_declaration_0'Access, null);
            Acts (116).Set_First_Last (0, 1);
            Acts (116)(0) := (accept_statement_0'Access, accept_statement_0_check'Access);
            Acts (116)(1) := (accept_statement_1'Access, null);
            Acts (117).Set_First_Last (0, 2);
            Acts (117)(0) := (access_definition_0'Access, null);
            Acts (117)(1) := (access_definition_1'Access, null);
            Acts (117)(2) := (access_definition_2'Access, null);
            Acts (118).Set_First_Last (0, 1);
            Acts (118)(0) := (actual_parameter_part_0'Access, null);
            Acts (118)(1) := (actual_parameter_part_1'Access, null);
            Acts (120).Set_First_Last (0, 6);
            Acts (120)(0) := (aggregate_0'Access, null);
            Acts (120)(2) := (aggregate_2'Access, null);
            Acts (120)(3) := (aggregate_3'Access, null);
            Acts (120)(4) := (aggregate_4'Access, null);
            Acts (120)(5) := (aggregate_5'Access, null);
            Acts (120)(6) := (aggregate_6'Access, null);
            Acts (123).Set_First_Last (0, 1);
            Acts (123)(0) := (array_type_definition_0'Access, null);
            Acts (123)(1) := (array_type_definition_1'Access, null);
            Acts (124).Set_First_Last (0, 3);
            Acts (124)(0) := (aspect_clause_0'Access, null);
            Acts (125).Set_First_Last (0, 1);
            Acts (125)(0) := (aspect_specification_opt_0'Access, null);
            Acts (126).Set_First_Last (0, 0);
            Acts (126)(0) := (assignment_statement_0'Access, null);
            Acts (127).Set_First_Last (0, 7);
            Acts (127)(0) := (association_opt_0'Access, null);
            Acts (127)(2) := (association_opt_2'Access, null);
            Acts (127)(3) := (association_opt_3'Access, null);
            Acts (127)(4) := (association_opt_4'Access, null);
            Acts (127)(6) := (association_opt_6'Access, null);
            Acts (127)(7) := (association_opt_7'Access, null);
            Acts (129).Set_First_Last (0, 0);
            Acts (129)(0) := (asynchronous_select_0'Access, null);
            Acts (130).Set_First_Last (0, 0);
            Acts (130)(0) := (at_clause_0'Access, null);
            Acts (134).Set_First_Last (0, 0);
            Acts (134)(0) := (block_label_0'Access, block_label_0_check'Access);
            Acts (135).Set_First_Last (0, 1);
            Acts (135)(0) := (null, block_label_opt_0_check'Access);
            Acts (135)(1) := (null, null);
            Acts (136).Set_First_Last (0, 1);
            Acts (136)(0) := (block_statement_0'Access, block_statement_0_check'Access);
            Acts (136)(1) := (block_statement_1'Access, block_statement_1_check'Access);
            Acts (139).Set_First_Last (0, 0);
            Acts (139)(0) := (case_expression_0'Access, null);
            Acts (140).Set_First_Last (0, 0);
            Acts (140)(0) := (case_expression_alternative_0'Access, null);
            Acts (141).Set_First_Last (0, 1);
            Acts (141)(0) := (case_expression_alternative_list_0'Access, null);
            Acts (142).Set_First_Last (0, 0);
            Acts (142)(0) := (case_statement_0'Access, null);
            Acts (143).Set_First_Last (0, 0);
            Acts (143)(0) := (case_statement_alternative_0'Access, null);
            Acts (144).Set_First_Last (0, 1);
            Acts (144)(0) := (case_statement_alternative_list_0'Access, null);
            Acts (145).Set_First_Last (0, 4);
            Acts (145)(2) := (compilation_unit_2'Access, null);
            Acts (146).Set_First_Last (0, 1);
            Acts (146)(0) := (compilation_0'Access, null);
            Acts (146)(1) := (compilation_1'Access, compilation_1_check'Access);
            Acts (147).Set_First_Last (0, 0);
            Acts (147)(0) := (component_clause_0'Access, null);
            Acts (149).Set_First_Last (0, 1);
            Acts (149)(0) := (component_declaration_0'Access, null);
            Acts (149)(1) := (component_declaration_1'Access, null);
            Acts (152).Set_First_Last (0, 4);
            Acts (152)(4) := (component_list_4'Access, null);
            Acts (155).Set_First_Last (0, 0);
            Acts (155)(0) := (conditional_entry_call_0'Access, null);
            Acts (160).Set_First_Last (0, 16);
            Acts (160)(9) := (declaration_9'Access, null);
            Acts (164).Set_First_Last (0, 1);
            Acts (164)(0) := (delay_statement_0'Access, null);
            Acts (164)(1) := (delay_statement_1'Access, null);
            Acts (166).Set_First_Last (0, 1);
            Acts (166)(0) := (derived_type_definition_0'Access, null);
            Acts (166)(1) := (derived_type_definition_1'Access, null);
            Acts (173).Set_First_Last (0, 2);
            Acts (173)(1) := (discriminant_part_opt_1'Access, null);
            Acts (176).Set_First_Last (0, 0);
            Acts (176)(0) := (elsif_expression_item_0'Access, null);
            Acts (177).Set_First_Last (0, 1);
            Acts (177)(0) := (elsif_expression_list_0'Access, null);
            Acts (178).Set_First_Last (0, 0);
            Acts (178)(0) := (elsif_statement_item_0'Access, null);
            Acts (179).Set_First_Last (0, 1);
            Acts (179)(0) := (elsif_statement_list_0'Access, null);
            Acts (180).Set_First_Last (0, 0);
            Acts (180)(0) := (entry_body_0'Access, entry_body_0_check'Access);
            Acts (181).Set_First_Last (0, 1);
            Acts (181)(0) := (entry_body_formal_part_0'Access, null);
            Acts (183).Set_First_Last (0, 1);
            Acts (183)(0) := (entry_declaration_0'Access, null);
            Acts (183)(1) := (entry_declaration_1'Access, null);
            Acts (186).Set_First_Last (0, 0);
            Acts (186)(0) := (enumeration_representation_clause_0'Access, null);
            Acts (187).Set_First_Last (0, 0);
            Acts (187)(0) := (enumeration_type_definition_0'Access, null);
            Acts (190).Set_First_Last (0, 0);
            Acts (190)(0) := (exception_declaration_0'Access, null);
            Acts (191).Set_First_Last (0, 1);
            Acts (191)(0) := (exception_handler_0'Access, null);
            Acts (191)(1) := (exception_handler_1'Access, null);
            Acts (192).Set_First_Last (0, 2);
            Acts (192)(0) := (exception_handler_list_0'Access, null);
            Acts (194).Set_First_Last (0, 1);
            Acts (194)(0) := (exit_statement_0'Access, null);
            Acts (194)(1) := (exit_statement_1'Access, null);
            Acts (197).Set_First_Last (0, 0);
            Acts (197)(0) := (expression_function_declaration_0'Access, null);
            Acts (198).Set_First_Last (0, 1);
            Acts (198)(0) := (extended_return_object_declaration_0'Access, null);
            Acts (198)(1) := (extended_return_object_declaration_1'Access, null);
            Acts (200).Set_First_Last (0, 1);
            Acts (200)(0) := (extended_return_statement_0'Access, null);
            Acts (200)(1) := (extended_return_statement_1'Access, null);
            Acts (202).Set_First_Last (0, 3);
            Acts (202)(0) := (formal_object_declaration_0'Access, null);
            Acts (202)(1) := (formal_object_declaration_1'Access, null);
            Acts (202)(2) := (formal_object_declaration_2'Access, null);
            Acts (202)(3) := (formal_object_declaration_3'Access, null);
            Acts (203).Set_First_Last (0, 0);
            Acts (203)(0) := (formal_part_0'Access, null);
            Acts (204).Set_First_Last (0, 3);
            Acts (204)(0) := (formal_subprogram_declaration_0'Access, null);
            Acts (204)(1) := (formal_subprogram_declaration_1'Access, null);
            Acts (204)(2) := (formal_subprogram_declaration_2'Access, null);
            Acts (204)(3) := (formal_subprogram_declaration_3'Access, null);
            Acts (205).Set_First_Last (0, 2);
            Acts (205)(0) := (formal_type_declaration_0'Access, null);
            Acts (205)(1) := (formal_type_declaration_1'Access, null);
            Acts (205)(2) := (formal_type_declaration_2'Access, null);
            Acts (207).Set_First_Last (0, 1);
            Acts (207)(0) := (formal_derived_type_definition_0'Access, null);
            Acts (207)(1) := (formal_derived_type_definition_1'Access, null);
            Acts (208).Set_First_Last (0, 0);
            Acts (208)(0) := (formal_package_declaration_0'Access, null);
            Acts (210).Set_First_Last (0, 2);
            Acts (210)(0) := (full_type_declaration_0'Access, null);
            Acts (211).Set_First_Last (0, 0);
            Acts (211)(0) := (function_specification_0'Access, function_specification_0_check'Access);
            Acts (214).Set_First_Last (0, 1);
            Acts (214)(0) := (generic_formal_part_0'Access, null);
            Acts (214)(1) := (generic_formal_part_1'Access, null);
            Acts (217).Set_First_Last (0, 2);
            Acts (217)(0) := (generic_instantiation_0'Access, null);
            Acts (217)(1) := (generic_instantiation_1'Access, null);
            Acts (217)(2) := (generic_instantiation_2'Access, null);
            Acts (218).Set_First_Last (0, 0);
            Acts (218)(0) := (generic_package_declaration_0'Access, null);
            Acts (219).Set_First_Last (0, 2);
            Acts (219)(0) := (generic_renaming_declaration_0'Access, null);
            Acts (219)(1) := (generic_renaming_declaration_1'Access, null);
            Acts (219)(2) := (generic_renaming_declaration_2'Access, null);
            Acts (220).Set_First_Last (0, 0);
            Acts (220)(0) := (generic_subprogram_declaration_0'Access, null);
            Acts (221).Set_First_Last (0, 0);
            Acts (221)(0) := (goto_label_0'Access, null);
            Acts (222).Set_First_Last (0, 1);
            Acts (222)(0) := (handled_sequence_of_statements_0'Access, null);
            Acts (223).Set_First_Last (0, 1);
            Acts (223)(0) := (identifier_list_0'Access, null);
            Acts (223)(1) := (identifier_list_1'Access, null);
            Acts (224).Set_First_Last (0, 1);
            Acts (224)(0) := (null, identifier_opt_0_check'Access);
            Acts (224)(1) := (null, null);
            Acts (225).Set_First_Last (0, 3);
            Acts (225)(0) := (if_expression_0'Access, null);
            Acts (225)(1) := (if_expression_1'Access, null);
            Acts (225)(2) := (if_expression_2'Access, null);
            Acts (225)(3) := (if_expression_3'Access, null);
            Acts (226).Set_First_Last (0, 3);
            Acts (226)(0) := (if_statement_0'Access, null);
            Acts (226)(1) := (if_statement_1'Access, null);
            Acts (226)(2) := (if_statement_2'Access, null);
            Acts (226)(3) := (if_statement_3'Access, null);
            Acts (227).Set_First_Last (0, 1);
            Acts (227)(0) := (incomplete_type_declaration_0'Access, null);
            Acts (227)(1) := (incomplete_type_declaration_1'Access, null);
            Acts (228).Set_First_Last (0, 0);
            Acts (228)(0) := (index_constraint_0'Access, null);
            Acts (231).Set_First_Last (0, 1);
            Acts (231)(0) := (interface_list_0'Access, null);
            Acts (231)(1) := (interface_list_1'Access, null);
            Acts (233).Set_First_Last (0, 1);
            Acts (233)(0) := (iteration_scheme_0'Access, null);
            Acts (233)(1) := (iteration_scheme_1'Access, null);
            Acts (234).Set_First_Last (0, 5);
            Acts (234)(2) := (iterator_specification_2'Access, null);
            Acts (234)(5) := (iterator_specification_5'Access, null);
            Acts (236).Set_First_Last (0, 1);
            Acts (236)(0) := (loop_statement_0'Access, loop_statement_0_check'Access);
            Acts (236)(1) := (loop_statement_1'Access, loop_statement_1_check'Access);
            Acts (243).Set_First_Last (0, 8);
            Acts (243)(0) := (name_0'Access, null);
            Acts (243)(1) := (name_1'Access, null);
            Acts (243)(2) := (null, name_2_check'Access);
            Acts (243)(3) := (null, null);
            Acts (243)(4) := (null, null);
            Acts (243)(5) := (name_5'Access, name_5_check'Access);
            Acts (243)(6) := (null, null);
            Acts (243)(7) := (null, name_7_check'Access);
            Acts (243)(8) := (null, null);
            Acts (244).Set_First_Last (0, 1);
            Acts (244)(0) := (null, name_opt_0_check'Access);
            Acts (244)(1) := (null, null);
            Acts (246).Set_First_Last (0, 3);
            Acts (246)(0) := (null_exclusion_opt_name_type_0'Access, null);
            Acts (246)(1) := (null_exclusion_opt_name_type_1'Access, null);
            Acts (246)(2) := (null_exclusion_opt_name_type_2'Access, null);
            Acts (246)(3) := (null_exclusion_opt_name_type_3'Access, null);
            Acts (247).Set_First_Last (0, 0);
            Acts (247)(0) := (null_procedure_declaration_0'Access, null);
            Acts (248).Set_First_Last (0, 7);
            Acts (248)(0) := (object_declaration_0'Access, null);
            Acts (248)(1) := (object_declaration_1'Access, null);
            Acts (248)(2) := (object_declaration_2'Access, null);
            Acts (248)(3) := (object_declaration_3'Access, null);
            Acts (248)(4) := (object_declaration_4'Access, null);
            Acts (248)(5) := (object_declaration_5'Access, null);
            Acts (249).Set_First_Last (0, 2);
            Acts (249)(0) := (object_renaming_declaration_0'Access, null);
            Acts (249)(1) := (object_renaming_declaration_1'Access, null);
            Acts (249)(2) := (object_renaming_declaration_2'Access, null);
            Acts (250).Set_First_Last (0, 2);
            Acts (250)(0) := (overriding_indicator_opt_0'Access, null);
            Acts (250)(1) := (overriding_indicator_opt_1'Access, null);
            Acts (251).Set_First_Last (0, 1);
            Acts (251)(0) := (package_body_0'Access, package_body_0_check'Access);
            Acts (251)(1) := (package_body_1'Access, package_body_1_check'Access);
            Acts (252).Set_First_Last (0, 0);
            Acts (252)(0) := (package_body_stub_0'Access, null);
            Acts (253).Set_First_Last (0, 0);
            Acts (253)(0) := (package_declaration_0'Access, null);
            Acts (254).Set_First_Last (0, 0);
            Acts (254)(0) := (package_renaming_declaration_0'Access, null);
            Acts (255).Set_First_Last (0, 1);
            Acts (255)(0) := (package_specification_0'Access, package_specification_0_check'Access);
            Acts (255)(1) := (package_specification_1'Access, package_specification_1_check'Access);
            Acts (256).Set_First_Last (0, 1);
            Acts (256)(0) := (parameter_and_result_profile_0'Access, null);
            Acts (258).Set_First_Last (0, 4);
            Acts (258)(0) := (parameter_specification_0'Access, null);
            Acts (258)(1) := (parameter_specification_1'Access, null);
            Acts (258)(2) := (parameter_specification_2'Access, null);
            Acts (258)(3) := (parameter_specification_3'Access, null);
            Acts (260).Set_First_Last (0, 1);
            Acts (260)(0) := (paren_expression_0'Access, null);
            Acts (261).Set_First_Last (0, 2);
            Acts (261)(0) := (pragma_g_0'Access, null);
            Acts (261)(1) := (pragma_g_1'Access, null);
            Acts (261)(2) := (pragma_g_2'Access, null);
            Acts (262).Set_First_Last (0, 5);
            Acts (262)(0) := (primary_0'Access, null);
            Acts (262)(2) := (primary_2'Access, null);
            Acts (262)(4) := (primary_4'Access, null);
            Acts (262)(5) := (primary_5'Access, null);
            Acts (263).Set_First_Last (0, 0);
            Acts (263)(0) := (private_extension_declaration_0'Access, null);
            Acts (264).Set_First_Last (0, 0);
            Acts (264)(0) := (private_type_declaration_0'Access, null);
            Acts (265).Set_First_Last (0, 0);
            Acts (265)(0) := (procedure_call_statement_0'Access, null);
            Acts (266).Set_First_Last (0, 0);
            Acts (266)(0) := (procedure_specification_0'Access, procedure_specification_0_check'Access);
            Acts (268).Set_First_Last (0, 0);
            Acts (268)(0) := (protected_body_0'Access, protected_body_0_check'Access);
            Acts (269).Set_First_Last (0, 0);
            Acts (269)(0) := (protected_body_stub_0'Access, null);
            Acts (270).Set_First_Last (0, 1);
            Acts (270)(0) := (protected_definition_0'Access, protected_definition_0_check'Access);
            Acts (270)(1) := (protected_definition_1'Access, protected_definition_1_check'Access);
            Acts (275).Set_First_Last (0, 1);
            Acts (275)(0) := (protected_type_declaration_0'Access, protected_type_declaration_0_check'Access);
            Acts (275)(1) := (protected_type_declaration_1'Access, protected_type_declaration_1_check'Access);
            Acts (276).Set_First_Last (0, 0);
            Acts (276)(0) := (qualified_expression_0'Access, null);
            Acts (277).Set_First_Last (0, 0);
            Acts (277)(0) := (quantified_expression_0'Access, null);
            Acts (279).Set_First_Last (0, 1);
            Acts (279)(0) := (raise_expression_0'Access, null);
            Acts (280).Set_First_Last (0, 2);
            Acts (280)(0) := (raise_statement_0'Access, null);
            Acts (280)(1) := (raise_statement_1'Access, null);
            Acts (280)(2) := (raise_statement_2'Access, null);
            Acts (281).Set_First_Last (0, 2);
            Acts (281)(0) := (range_g_0'Access, null);
            Acts (284).Set_First_Last (0, 1);
            Acts (284)(0) := (record_definition_0'Access, null);
            Acts (285).Set_First_Last (0, 0);
            Acts (285)(0) := (record_representation_clause_0'Access, null);
            Acts (294).Set_First_Last (0, 1);
            Acts (294)(0) := (requeue_statement_0'Access, null);
            Acts (294)(1) := (requeue_statement_1'Access, null);
            Acts (295).Set_First_Last (0, 1);
            Acts (295)(0) := (result_profile_0'Access, null);
            Acts (295)(1) := (result_profile_1'Access, null);
            Acts (297).Set_First_Last (0, 3);
            Acts (297)(0) := (selected_component_0'Access, selected_component_0_check'Access);
            Acts (297)(1) := (selected_component_1'Access, null);
            Acts (297)(2) := (selected_component_2'Access, selected_component_2_check'Access);
            Acts (297)(3) := (selected_component_3'Access, null);
            Acts (298).Set_First_Last (0, 1);
            Acts (298)(0) := (selective_accept_0'Access, null);
            Acts (298)(1) := (selective_accept_1'Access, null);
            Acts (299).Set_First_Last (0, 5);
            Acts (299)(0) := (select_alternative_0'Access, null);
            Acts (299)(1) := (select_alternative_1'Access, null);
            Acts (299)(2) := (select_alternative_2'Access, null);
            Acts (299)(4) := (select_alternative_4'Access, null);
            Acts (300).Set_First_Last (0, 1);
            Acts (300)(0) := (select_alternative_list_0'Access, null);
            Acts (300)(1) := (select_alternative_list_1'Access, null);
            Acts (306).Set_First_Last (0, 0);
            Acts (306)(0) := (simple_return_statement_0'Access, null);
            Acts (307).Set_First_Last (0, 10);
            Acts (307)(0) := (simple_statement_0'Access, null);
            Acts (307)(3) := (simple_statement_3'Access, null);
            Acts (307)(8) := (simple_statement_8'Access, null);
            Acts (308).Set_First_Last (0, 1);
            Acts (308)(0) := (single_protected_declaration_0'Access, single_protected_declaration_0_check'Access);
            Acts (308)(1) := (single_protected_declaration_1'Access, single_protected_declaration_1_check'Access);
            Acts (309).Set_First_Last (0, 2);
            Acts (309)(0) := (single_task_declaration_0'Access, single_task_declaration_0_check'Access);
            Acts (309)(1) := (single_task_declaration_1'Access, single_task_declaration_1_check'Access);
            Acts (309)(2) := (single_task_declaration_2'Access, null);
            Acts (311).Set_First_Last (0, 0);
            Acts (311)(0) := (subprogram_body_0'Access, subprogram_body_0_check'Access);
            Acts (312).Set_First_Last (0, 0);
            Acts (312)(0) := (subprogram_body_stub_0'Access, null);
            Acts (313).Set_First_Last (0, 0);
            Acts (313)(0) := (subprogram_declaration_0'Access, null);
            Acts (314).Set_First_Last (0, 2);
            Acts (314)(0) := (subprogram_default_0'Access, null);
            Acts (315).Set_First_Last (0, 0);
            Acts (315)(0) := (subprogram_renaming_declaration_0'Access, null);
            Acts (316).Set_First_Last (0, 1);
            Acts (316)(0) := (null, subprogram_specification_0_check'Access);
            Acts (316)(1) := (null, subprogram_specification_1_check'Access);
            Acts (317).Set_First_Last (0, 0);
            Acts (317)(0) := (subtype_declaration_0'Access, null);
            Acts (318).Set_First_Last (0, 3);
            Acts (318)(0) := (subtype_indication_0'Access, null);
            Acts (318)(1) := (subtype_indication_1'Access, null);
            Acts (318)(2) := (subtype_indication_2'Access, null);
            Acts (318)(3) := (subtype_indication_3'Access, null);
            Acts (319).Set_First_Last (0, 0);
            Acts (319)(0) := (subunit_0'Access, null);
            Acts (320).Set_First_Last (0, 0);
            Acts (320)(0) := (task_body_0'Access, task_body_0_check'Access);
            Acts (321).Set_First_Last (0, 0);
            Acts (321)(0) := (task_body_stub_0'Access, null);
            Acts (322).Set_First_Last (0, 1);
            Acts (322)(0) := (task_definition_0'Access, null);
            Acts (322)(1) := (task_definition_1'Access, null);
            Acts (323).Set_First_Last (0, 2);
            Acts (323)(0) := (task_type_declaration_0'Access, task_type_declaration_0_check'Access);
            Acts (323)(1) := (task_type_declaration_1'Access, task_type_declaration_1_check'Access);
            Acts (323)(2) := (task_type_declaration_2'Access, null);
            Acts (327).Set_First_Last (0, 0);
            Acts (327)(0) := (timed_entry_call_0'Access, null);
            Acts (331).Set_First_Last (0, 0);
            Acts (331)(0) := (variant_part_0'Access, null);
            Acts (332).Set_First_Last (0, 1);
            Acts (332)(0) := (variant_list_0'Access, null);
            Acts (333).Set_First_Last (0, 0);
            Acts (333)(0) := (variant_0'Access, null);
            Acts (335).Set_First_Last (0, 2);
            Acts (335)(0) := (use_clause_0'Access, null);
            Acts (335)(1) := (use_clause_1'Access, null);
            Acts (335)(2) := (use_clause_2'Access, null);
            Acts (336).Set_First_Last (0, 3);
            Acts (336)(0) := (with_clause_0'Access, null);
            Acts (336)(1) := (with_clause_1'Access, null);
            Acts (336)(2) := (with_clause_2'Access, null);
            Acts (336)(3) := (with_clause_3'Access, null);
         end return;
      end Actions;

      Table : constant Parse_Table_Ptr := Get_Text_Rep
        (Text_Rep_File_Name, McKenzie_Param, Actions);
   begin
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         Language_Fixes,
         Language_Matching_Begin_Tokens,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Ada_Process_LR1_Main;
