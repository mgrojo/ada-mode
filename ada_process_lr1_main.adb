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
         Last_Terminal     => 111,
         First_Nonterminal => 112,
         Last_Nonterminal  => 343,
         Insert =>
           (4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4),
         Delete =>
           (4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
            4),
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
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
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
            Acts.Set_First_Last (112, 343);
            Acts (116).Set_First_Last (0, 0);
            Acts (116)(0) := (abstract_subprogram_declaration_0'Access, null);
            Acts (117).Set_First_Last (0, 1);
            Acts (117)(0) := (accept_statement_0'Access, accept_statement_0_check'Access);
            Acts (117)(1) := (accept_statement_1'Access, null);
            Acts (118).Set_First_Last (0, 2);
            Acts (118)(0) := (access_definition_0'Access, null);
            Acts (118)(1) := (access_definition_1'Access, null);
            Acts (118)(2) := (access_definition_2'Access, null);
            Acts (119).Set_First_Last (0, 1);
            Acts (119)(0) := (actual_parameter_part_0'Access, null);
            Acts (119)(1) := (actual_parameter_part_1'Access, null);
            Acts (121).Set_First_Last (0, 6);
            Acts (121)(0) := (aggregate_0'Access, null);
            Acts (121)(2) := (aggregate_2'Access, null);
            Acts (121)(3) := (aggregate_3'Access, null);
            Acts (121)(4) := (aggregate_4'Access, null);
            Acts (121)(5) := (aggregate_5'Access, null);
            Acts (121)(6) := (aggregate_6'Access, null);
            Acts (124).Set_First_Last (0, 1);
            Acts (124)(0) := (array_type_definition_0'Access, null);
            Acts (124)(1) := (array_type_definition_1'Access, null);
            Acts (125).Set_First_Last (0, 3);
            Acts (125)(0) := (aspect_clause_0'Access, null);
            Acts (126).Set_First_Last (0, 1);
            Acts (126)(0) := (aspect_specification_opt_0'Access, null);
            Acts (127).Set_First_Last (0, 0);
            Acts (127)(0) := (assignment_statement_0'Access, null);
            Acts (128).Set_First_Last (0, 7);
            Acts (128)(0) := (association_opt_0'Access, null);
            Acts (128)(2) := (association_opt_2'Access, null);
            Acts (128)(3) := (association_opt_3'Access, null);
            Acts (128)(4) := (association_opt_4'Access, null);
            Acts (128)(6) := (association_opt_6'Access, null);
            Acts (128)(7) := (association_opt_7'Access, null);
            Acts (130).Set_First_Last (0, 0);
            Acts (130)(0) := (asynchronous_select_0'Access, null);
            Acts (131).Set_First_Last (0, 0);
            Acts (131)(0) := (at_clause_0'Access, null);
            Acts (135).Set_First_Last (0, 0);
            Acts (135)(0) := (block_label_0'Access, block_label_0_check'Access);
            Acts (136).Set_First_Last (0, 1);
            Acts (136)(0) := (null, block_label_opt_0_check'Access);
            Acts (136)(1) := (null, null);
            Acts (137).Set_First_Last (0, 1);
            Acts (137)(0) := (block_statement_0'Access, block_statement_0_check'Access);
            Acts (137)(1) := (block_statement_1'Access, block_statement_1_check'Access);
            Acts (140).Set_First_Last (0, 0);
            Acts (140)(0) := (case_expression_0'Access, null);
            Acts (141).Set_First_Last (0, 0);
            Acts (141)(0) := (case_expression_alternative_0'Access, null);
            Acts (142).Set_First_Last (0, 1);
            Acts (142)(0) := (case_expression_alternative_list_0'Access, null);
            Acts (143).Set_First_Last (0, 0);
            Acts (143)(0) := (case_statement_0'Access, null);
            Acts (144).Set_First_Last (0, 0);
            Acts (144)(0) := (case_statement_alternative_0'Access, null);
            Acts (145).Set_First_Last (0, 1);
            Acts (145)(0) := (case_statement_alternative_list_0'Access, null);
            Acts (147).Set_First_Last (0, 4);
            Acts (147)(2) := (compilation_unit_2'Access, null);
            Acts (148).Set_First_Last (0, 1);
            Acts (148)(0) := (compilation_0'Access, null);
            Acts (148)(1) := (compilation_1'Access, compilation_1_check'Access);
            Acts (149).Set_First_Last (0, 0);
            Acts (149)(0) := (component_clause_0'Access, null);
            Acts (151).Set_First_Last (0, 1);
            Acts (151)(0) := (component_declaration_0'Access, null);
            Acts (151)(1) := (component_declaration_1'Access, null);
            Acts (154).Set_First_Last (0, 4);
            Acts (154)(4) := (component_list_4'Access, null);
            Acts (157).Set_First_Last (0, 0);
            Acts (157)(0) := (conditional_entry_call_0'Access, null);
            Acts (162).Set_First_Last (0, 16);
            Acts (162)(9) := (declaration_9'Access, null);
            Acts (166).Set_First_Last (0, 1);
            Acts (166)(0) := (delay_statement_0'Access, null);
            Acts (166)(1) := (delay_statement_1'Access, null);
            Acts (168).Set_First_Last (0, 1);
            Acts (168)(0) := (derived_type_definition_0'Access, null);
            Acts (168)(1) := (derived_type_definition_1'Access, null);
            Acts (175).Set_First_Last (0, 2);
            Acts (175)(1) := (discriminant_part_opt_1'Access, null);
            Acts (178).Set_First_Last (0, 0);
            Acts (178)(0) := (elsif_expression_item_0'Access, null);
            Acts (179).Set_First_Last (0, 1);
            Acts (179)(0) := (elsif_expression_list_0'Access, null);
            Acts (180).Set_First_Last (0, 0);
            Acts (180)(0) := (elsif_statement_item_0'Access, null);
            Acts (181).Set_First_Last (0, 1);
            Acts (181)(0) := (elsif_statement_list_0'Access, null);
            Acts (182).Set_First_Last (0, 0);
            Acts (182)(0) := (entry_body_0'Access, entry_body_0_check'Access);
            Acts (183).Set_First_Last (0, 1);
            Acts (183)(0) := (entry_body_formal_part_0'Access, null);
            Acts (185).Set_First_Last (0, 1);
            Acts (185)(0) := (entry_declaration_0'Access, null);
            Acts (185)(1) := (entry_declaration_1'Access, null);
            Acts (188).Set_First_Last (0, 0);
            Acts (188)(0) := (enumeration_representation_clause_0'Access, null);
            Acts (189).Set_First_Last (0, 0);
            Acts (189)(0) := (enumeration_type_definition_0'Access, null);
            Acts (192).Set_First_Last (0, 0);
            Acts (192)(0) := (exception_declaration_0'Access, null);
            Acts (193).Set_First_Last (0, 1);
            Acts (193)(0) := (exception_handler_0'Access, null);
            Acts (193)(1) := (exception_handler_1'Access, null);
            Acts (194).Set_First_Last (0, 2);
            Acts (194)(0) := (exception_handler_list_0'Access, null);
            Acts (196).Set_First_Last (0, 1);
            Acts (196)(0) := (exit_statement_0'Access, null);
            Acts (196)(1) := (exit_statement_1'Access, null);
            Acts (199).Set_First_Last (0, 0);
            Acts (199)(0) := (expression_function_declaration_0'Access, null);
            Acts (200).Set_First_Last (0, 1);
            Acts (200)(0) := (extended_return_object_declaration_0'Access, null);
            Acts (200)(1) := (extended_return_object_declaration_1'Access, null);
            Acts (202).Set_First_Last (0, 1);
            Acts (202)(0) := (extended_return_statement_0'Access, null);
            Acts (202)(1) := (extended_return_statement_1'Access, null);
            Acts (204).Set_First_Last (0, 3);
            Acts (204)(0) := (formal_object_declaration_0'Access, null);
            Acts (204)(1) := (formal_object_declaration_1'Access, null);
            Acts (204)(2) := (formal_object_declaration_2'Access, null);
            Acts (204)(3) := (formal_object_declaration_3'Access, null);
            Acts (205).Set_First_Last (0, 0);
            Acts (205)(0) := (formal_part_0'Access, null);
            Acts (206).Set_First_Last (0, 3);
            Acts (206)(0) := (formal_subprogram_declaration_0'Access, null);
            Acts (206)(1) := (formal_subprogram_declaration_1'Access, null);
            Acts (206)(2) := (formal_subprogram_declaration_2'Access, null);
            Acts (206)(3) := (formal_subprogram_declaration_3'Access, null);
            Acts (207).Set_First_Last (0, 2);
            Acts (207)(0) := (formal_type_declaration_0'Access, null);
            Acts (207)(1) := (formal_type_declaration_1'Access, null);
            Acts (207)(2) := (formal_type_declaration_2'Access, null);
            Acts (209).Set_First_Last (0, 1);
            Acts (209)(0) := (formal_derived_type_definition_0'Access, null);
            Acts (209)(1) := (formal_derived_type_definition_1'Access, null);
            Acts (210).Set_First_Last (0, 0);
            Acts (210)(0) := (formal_package_declaration_0'Access, null);
            Acts (212).Set_First_Last (0, 2);
            Acts (212)(0) := (full_type_declaration_0'Access, null);
            Acts (213).Set_First_Last (0, 0);
            Acts (213)(0) := (function_specification_0'Access, function_specification_0_check'Access);
            Acts (216).Set_First_Last (0, 1);
            Acts (216)(0) := (generic_formal_part_0'Access, null);
            Acts (216)(1) := (generic_formal_part_1'Access, null);
            Acts (219).Set_First_Last (0, 2);
            Acts (219)(0) := (generic_instantiation_0'Access, null);
            Acts (219)(1) := (generic_instantiation_1'Access, null);
            Acts (219)(2) := (generic_instantiation_2'Access, null);
            Acts (220).Set_First_Last (0, 0);
            Acts (220)(0) := (generic_package_declaration_0'Access, null);
            Acts (221).Set_First_Last (0, 2);
            Acts (221)(0) := (generic_renaming_declaration_0'Access, null);
            Acts (221)(1) := (generic_renaming_declaration_1'Access, null);
            Acts (221)(2) := (generic_renaming_declaration_2'Access, null);
            Acts (222).Set_First_Last (0, 0);
            Acts (222)(0) := (generic_subprogram_declaration_0'Access, null);
            Acts (223).Set_First_Last (0, 0);
            Acts (223)(0) := (goto_label_0'Access, null);
            Acts (224).Set_First_Last (0, 1);
            Acts (224)(0) := (handled_sequence_of_statements_0'Access, null);
            Acts (225).Set_First_Last (0, 1);
            Acts (225)(0) := (identifier_list_0'Access, null);
            Acts (225)(1) := (identifier_list_1'Access, null);
            Acts (226).Set_First_Last (0, 1);
            Acts (226)(0) := (null, identifier_opt_0_check'Access);
            Acts (226)(1) := (null, null);
            Acts (227).Set_First_Last (0, 3);
            Acts (227)(0) := (if_expression_0'Access, null);
            Acts (227)(1) := (if_expression_1'Access, null);
            Acts (227)(2) := (if_expression_2'Access, null);
            Acts (227)(3) := (if_expression_3'Access, null);
            Acts (228).Set_First_Last (0, 3);
            Acts (228)(0) := (if_statement_0'Access, null);
            Acts (228)(1) := (if_statement_1'Access, null);
            Acts (228)(2) := (if_statement_2'Access, null);
            Acts (228)(3) := (if_statement_3'Access, null);
            Acts (229).Set_First_Last (0, 1);
            Acts (229)(0) := (incomplete_type_declaration_0'Access, null);
            Acts (229)(1) := (incomplete_type_declaration_1'Access, null);
            Acts (230).Set_First_Last (0, 0);
            Acts (230)(0) := (index_constraint_0'Access, null);
            Acts (233).Set_First_Last (0, 1);
            Acts (233)(0) := (interface_list_0'Access, null);
            Acts (233)(1) := (interface_list_1'Access, null);
            Acts (236).Set_First_Last (0, 1);
            Acts (236)(0) := (iteration_scheme_0'Access, null);
            Acts (236)(1) := (iteration_scheme_1'Access, null);
            Acts (237).Set_First_Last (0, 5);
            Acts (237)(2) := (iterator_specification_2'Access, null);
            Acts (237)(5) := (iterator_specification_5'Access, null);
            Acts (239).Set_First_Last (0, 1);
            Acts (239)(0) := (loop_statement_0'Access, loop_statement_0_check'Access);
            Acts (239)(1) := (loop_statement_1'Access, loop_statement_1_check'Access);
            Acts (246).Set_First_Last (0, 8);
            Acts (246)(0) := (name_0'Access, null);
            Acts (246)(1) := (name_1'Access, null);
            Acts (246)(2) := (null, name_2_check'Access);
            Acts (246)(3) := (null, null);
            Acts (246)(4) := (null, null);
            Acts (246)(5) := (name_5'Access, name_5_check'Access);
            Acts (246)(6) := (null, null);
            Acts (246)(7) := (null, name_7_check'Access);
            Acts (246)(8) := (null, null);
            Acts (247).Set_First_Last (0, 1);
            Acts (247)(0) := (null, name_opt_0_check'Access);
            Acts (247)(1) := (null, null);
            Acts (249).Set_First_Last (0, 3);
            Acts (249)(0) := (null_exclusion_opt_name_type_0'Access, null);
            Acts (249)(1) := (null_exclusion_opt_name_type_1'Access, null);
            Acts (249)(2) := (null_exclusion_opt_name_type_2'Access, null);
            Acts (249)(3) := (null_exclusion_opt_name_type_3'Access, null);
            Acts (250).Set_First_Last (0, 0);
            Acts (250)(0) := (null_procedure_declaration_0'Access, null);
            Acts (251).Set_First_Last (0, 7);
            Acts (251)(0) := (object_declaration_0'Access, null);
            Acts (251)(1) := (object_declaration_1'Access, null);
            Acts (251)(2) := (object_declaration_2'Access, null);
            Acts (251)(3) := (object_declaration_3'Access, null);
            Acts (251)(4) := (object_declaration_4'Access, null);
            Acts (251)(5) := (object_declaration_5'Access, null);
            Acts (252).Set_First_Last (0, 2);
            Acts (252)(0) := (object_renaming_declaration_0'Access, null);
            Acts (252)(1) := (object_renaming_declaration_1'Access, null);
            Acts (252)(2) := (object_renaming_declaration_2'Access, null);
            Acts (253).Set_First_Last (0, 2);
            Acts (253)(0) := (overriding_indicator_opt_0'Access, null);
            Acts (253)(1) := (overriding_indicator_opt_1'Access, null);
            Acts (254).Set_First_Last (0, 1);
            Acts (254)(0) := (package_body_0'Access, package_body_0_check'Access);
            Acts (254)(1) := (package_body_1'Access, package_body_1_check'Access);
            Acts (255).Set_First_Last (0, 0);
            Acts (255)(0) := (package_body_stub_0'Access, null);
            Acts (256).Set_First_Last (0, 0);
            Acts (256)(0) := (package_declaration_0'Access, null);
            Acts (257).Set_First_Last (0, 0);
            Acts (257)(0) := (package_renaming_declaration_0'Access, null);
            Acts (258).Set_First_Last (0, 1);
            Acts (258)(0) := (package_specification_0'Access, package_specification_0_check'Access);
            Acts (258)(1) := (package_specification_1'Access, package_specification_1_check'Access);
            Acts (259).Set_First_Last (0, 1);
            Acts (259)(0) := (parameter_and_result_profile_0'Access, null);
            Acts (261).Set_First_Last (0, 4);
            Acts (261)(0) := (parameter_specification_0'Access, null);
            Acts (261)(1) := (parameter_specification_1'Access, null);
            Acts (261)(2) := (parameter_specification_2'Access, null);
            Acts (261)(3) := (parameter_specification_3'Access, null);
            Acts (263).Set_First_Last (0, 1);
            Acts (263)(0) := (paren_expression_0'Access, null);
            Acts (264).Set_First_Last (0, 2);
            Acts (264)(0) := (pragma_g_0'Access, null);
            Acts (264)(1) := (pragma_g_1'Access, null);
            Acts (264)(2) := (pragma_g_2'Access, null);
            Acts (265).Set_First_Last (0, 5);
            Acts (265)(0) := (primary_0'Access, null);
            Acts (265)(2) := (primary_2'Access, null);
            Acts (265)(4) := (primary_4'Access, null);
            Acts (265)(5) := (primary_5'Access, null);
            Acts (266).Set_First_Last (0, 0);
            Acts (266)(0) := (private_extension_declaration_0'Access, null);
            Acts (267).Set_First_Last (0, 0);
            Acts (267)(0) := (private_type_declaration_0'Access, null);
            Acts (268).Set_First_Last (0, 0);
            Acts (268)(0) := (procedure_call_statement_0'Access, null);
            Acts (269).Set_First_Last (0, 0);
            Acts (269)(0) := (procedure_specification_0'Access, procedure_specification_0_check'Access);
            Acts (271).Set_First_Last (0, 0);
            Acts (271)(0) := (protected_body_0'Access, protected_body_0_check'Access);
            Acts (272).Set_First_Last (0, 0);
            Acts (272)(0) := (protected_body_stub_0'Access, null);
            Acts (273).Set_First_Last (0, 1);
            Acts (273)(0) := (protected_definition_0'Access, protected_definition_0_check'Access);
            Acts (273)(1) := (protected_definition_1'Access, protected_definition_1_check'Access);
            Acts (278).Set_First_Last (0, 1);
            Acts (278)(0) := (protected_type_declaration_0'Access, protected_type_declaration_0_check'Access);
            Acts (278)(1) := (protected_type_declaration_1'Access, protected_type_declaration_1_check'Access);
            Acts (279).Set_First_Last (0, 0);
            Acts (279)(0) := (qualified_expression_0'Access, null);
            Acts (280).Set_First_Last (0, 0);
            Acts (280)(0) := (quantified_expression_0'Access, null);
            Acts (282).Set_First_Last (0, 1);
            Acts (282)(0) := (raise_expression_0'Access, null);
            Acts (283).Set_First_Last (0, 2);
            Acts (283)(0) := (raise_statement_0'Access, null);
            Acts (283)(1) := (raise_statement_1'Access, null);
            Acts (283)(2) := (raise_statement_2'Access, null);
            Acts (284).Set_First_Last (0, 2);
            Acts (284)(0) := (range_g_0'Access, null);
            Acts (287).Set_First_Last (0, 1);
            Acts (287)(0) := (record_definition_0'Access, null);
            Acts (288).Set_First_Last (0, 0);
            Acts (288)(0) := (record_representation_clause_0'Access, null);
            Acts (300).Set_First_Last (0, 1);
            Acts (300)(0) := (requeue_statement_0'Access, null);
            Acts (300)(1) := (requeue_statement_1'Access, null);
            Acts (301).Set_First_Last (0, 1);
            Acts (301)(0) := (result_profile_0'Access, null);
            Acts (301)(1) := (result_profile_1'Access, null);
            Acts (303).Set_First_Last (0, 3);
            Acts (303)(0) := (selected_component_0'Access, selected_component_0_check'Access);
            Acts (303)(1) := (selected_component_1'Access, null);
            Acts (303)(2) := (selected_component_2'Access, selected_component_2_check'Access);
            Acts (303)(3) := (selected_component_3'Access, null);
            Acts (304).Set_First_Last (0, 1);
            Acts (304)(0) := (selective_accept_0'Access, null);
            Acts (304)(1) := (selective_accept_1'Access, null);
            Acts (305).Set_First_Last (0, 5);
            Acts (305)(0) := (select_alternative_0'Access, null);
            Acts (305)(1) := (select_alternative_1'Access, null);
            Acts (305)(2) := (select_alternative_2'Access, null);
            Acts (305)(4) := (select_alternative_4'Access, null);
            Acts (306).Set_First_Last (0, 1);
            Acts (306)(0) := (select_alternative_list_0'Access, null);
            Acts (306)(1) := (select_alternative_list_1'Access, null);
            Acts (312).Set_First_Last (0, 0);
            Acts (312)(0) := (simple_return_statement_0'Access, null);
            Acts (313).Set_First_Last (0, 10);
            Acts (313)(0) := (simple_statement_0'Access, null);
            Acts (313)(3) := (simple_statement_3'Access, null);
            Acts (313)(8) := (simple_statement_8'Access, null);
            Acts (314).Set_First_Last (0, 1);
            Acts (314)(0) := (single_protected_declaration_0'Access, single_protected_declaration_0_check'Access);
            Acts (314)(1) := (single_protected_declaration_1'Access, single_protected_declaration_1_check'Access);
            Acts (315).Set_First_Last (0, 2);
            Acts (315)(0) := (single_task_declaration_0'Access, single_task_declaration_0_check'Access);
            Acts (315)(1) := (single_task_declaration_1'Access, single_task_declaration_1_check'Access);
            Acts (315)(2) := (single_task_declaration_2'Access, null);
            Acts (317).Set_First_Last (0, 0);
            Acts (317)(0) := (subprogram_body_0'Access, subprogram_body_0_check'Access);
            Acts (318).Set_First_Last (0, 0);
            Acts (318)(0) := (subprogram_body_stub_0'Access, null);
            Acts (319).Set_First_Last (0, 0);
            Acts (319)(0) := (subprogram_declaration_0'Access, null);
            Acts (320).Set_First_Last (0, 2);
            Acts (320)(0) := (subprogram_default_0'Access, null);
            Acts (321).Set_First_Last (0, 0);
            Acts (321)(0) := (subprogram_renaming_declaration_0'Access, null);
            Acts (322).Set_First_Last (0, 1);
            Acts (322)(0) := (null, subprogram_specification_0_check'Access);
            Acts (322)(1) := (null, subprogram_specification_1_check'Access);
            Acts (323).Set_First_Last (0, 0);
            Acts (323)(0) := (subtype_declaration_0'Access, null);
            Acts (324).Set_First_Last (0, 3);
            Acts (324)(0) := (subtype_indication_0'Access, null);
            Acts (324)(1) := (subtype_indication_1'Access, null);
            Acts (324)(2) := (subtype_indication_2'Access, null);
            Acts (324)(3) := (subtype_indication_3'Access, null);
            Acts (325).Set_First_Last (0, 0);
            Acts (325)(0) := (subunit_0'Access, null);
            Acts (326).Set_First_Last (0, 0);
            Acts (326)(0) := (task_body_0'Access, task_body_0_check'Access);
            Acts (327).Set_First_Last (0, 0);
            Acts (327)(0) := (task_body_stub_0'Access, null);
            Acts (328).Set_First_Last (0, 1);
            Acts (328)(0) := (task_definition_0'Access, null);
            Acts (328)(1) := (task_definition_1'Access, null);
            Acts (329).Set_First_Last (0, 2);
            Acts (329)(0) := (task_type_declaration_0'Access, task_type_declaration_0_check'Access);
            Acts (329)(1) := (task_type_declaration_1'Access, task_type_declaration_1_check'Access);
            Acts (329)(2) := (task_type_declaration_2'Access, null);
            Acts (333).Set_First_Last (0, 0);
            Acts (333)(0) := (timed_entry_call_0'Access, null);
            Acts (338).Set_First_Last (0, 0);
            Acts (338)(0) := (variant_part_0'Access, null);
            Acts (339).Set_First_Last (0, 1);
            Acts (339)(0) := (variant_list_0'Access, null);
            Acts (340).Set_First_Last (0, 0);
            Acts (340)(0) := (variant_0'Access, null);
            Acts (342).Set_First_Last (0, 2);
            Acts (342)(0) := (use_clause_0'Access, null);
            Acts (342)(1) := (use_clause_1'Access, null);
            Acts (342)(2) := (use_clause_2'Access, null);
            Acts (343).Set_First_Last (0, 3);
            Acts (343)(0) := (with_clause_0'Access, null);
            Acts (343)(1) := (with_clause_1'Access, null);
            Acts (343)(2) := (with_clause_2'Access, null);
            Acts (343)(3) := (with_clause_3'Access, null);
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
