--  Abstract :
--
--  hand-written binding to tree-sitter code generated for ada_lite.wy
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Interfaces.C.Extensions;
with WisiToken;
package Ada_Lite_Tree_Sitter_Actions is
   use all type WisiToken.Token_ID;

   function Tree_Sitter_Ada_Lite return Interfaces.C.Extensions.void_ptr
   with Import     => True,
     External_Name => "tree_sitter_Ada_Lite",
     Convention    => C;

   --  Copied from ada_lite_tree_sitter.c, converted to Ada
   type Token_Enum_ID is
     (TS_EOI_ID,                             -- 0
      AMP_ID,                                -- 1
      COLON_ID,                              -- 2
      COLON_EQ_ID,                           -- 3
      COMMA_ID,                              -- 4
      DOT_ID,                                -- 5
      DOT_DOT_ID,                            -- 6
      EQ_ID,                                 -- 7
      EQ_GT_ID,                              -- 8
      GT_ID,                                 -- 9
      GT_EQ_ID,                              -- 10
      LT_ID,                                 -- 11
      LT_EQ_ID,                              -- 12
      DASH_ID,                               -- 13
      PLUS_ID,                               -- 14
      SEMI_ID,                               -- 15
      SLASH_ID,                              -- 16
      SLASH_EQ_ID,                           -- 17
      STAR_ID,                               -- 18
      STRING_LITERAL_ID,                     -- 19
      IDENTIFIER_ID,                         -- 20
      COMMENT_ID,                            -- 21
      NUMERIC_LITERAL_ID,                    -- 22
      LPAREN_ID,                             -- 23
      RPAREN_ID,                             -- 24
      with_ID,                               -- 25
      declare_ID,                            -- 26
      begin_ID,                              -- 27
      end_ID,                                -- 28
      case_ID,                               -- 29
      is_ID,                                 -- 30
      when_ID,                               -- 31
      range_ID,                              -- 32
      elsif_ID,                              -- 33
      then_ID,                               -- 34
      exit_ID,                               -- 35
      return_ID,                             -- 36
      not_ID,                                -- 37
      type_ID,                               -- 38
      function_ID,                           -- 39
      package_ID,                            -- 40
      new_ID,                                -- 41
      procedure_ID,                          -- 42
      exception_ID,                          -- 43
      if_ID,                                 -- 44
      else_ID,                               -- 45
      for_ID,                                -- 46
      in_ID,                                 -- 47
      loop_ID,                               -- 48
      body_ID,                               -- 49
      and_ID,                                -- 50
      or_ID,                                 -- 51
      xor_ID,                                -- 52
      separate_ID,                           -- 53
      compilation_unit_list_ID,              -- 54
      actual_parameter_part_ID,              -- 55
      aspect_specification_opt_ID,           -- 56
      assignment_statement_ID,               -- 57
      association_opt_ID,                    -- 58
      association_list_ID,                   -- 59
      binary_adding_operator_ID,             -- 60
      block_label_opt_ID,                    -- 61
      block_statement_ID,                    -- 62
      body_g_ID,                             -- 63
      body_stub_ID,                          -- 64
      case_statement_ID,                     -- 65
      case_statement_alternative_ID,         -- 66
      case_statement_alternative_list_ID,    -- 67
      compilation_unit_ID,                   -- 68
      compound_statement_ID,                 -- 69
      constraint_ID,                         -- 70
      declaration_ID,                        -- 71
      declarations_ID,                       -- 72
      declarative_part_ID,                   -- 73
      discrete_subtype_definition_ID,        -- 74
      elsif_statement_item_ID,               -- 75
      elsif_statement_list_ID,               -- 76
      enumeration_literal_list_ID,           -- 77
      enumeration_type_definition_ID,        -- 78
      exception_choice_ID,                   -- 79
      exception_handler_ID,                  -- 80
      exception_handler_list_ID,             -- 81
      exception_handler_list_opt_ID,         -- 82
      exit_statement_ID,                     -- 83
      expression_ID,                         -- 84
      expression_opt_ID,                     -- 85
      extended_return_object_declaration_ID, -- 86
      extended_return_statement_ID,          -- 87
      factor_ID,                             -- 88
      formal_part_ID,                        -- 89
      full_type_declaration_ID,              -- 90
      function_specification_ID,             -- 91
      generic_instantiation_ID,              -- 92
      handled_sequence_of_statements_ID,     -- 93
      identifier_opt_ID,                     -- 94
      if_statement_ID,                       -- 95
      index_constraint_ID,                   -- 96
      iteration_scheme_ID,                   -- 97
      loop_statement_ID,                     -- 98
      multiplying_operator_ID,               -- 99
      name_ID,                               -- 100
      name_opt_ID,                           -- 101
      object_declaration_ID,                 -- 102
      package_body_ID,                       -- 103
      package_specification_ID,              -- 104
      parameter_and_result_profile_ID,       -- 105
      parameter_profile_opt_ID,              -- 106
      parameter_specification_ID,            -- 107
      parameter_specification_list_ID,       -- 108
      paren_expression_ID,                   -- 109
      primary_ID,                            -- 110
      procedure_call_statement_ID,           -- 111
      procedure_specification_ID,            -- 112
      proper_body_ID,                        -- 113
      range_g_ID,                            -- 114
      range_list_ID,                         -- 115
      relation_and_list_ID,                  -- 116
      relation_or_list_ID,                   -- 117
      relation_xor_list_ID,                  -- 118
      relation_ID,                           -- 119
      relational_operator_ID,                -- 120
      selected_component_ID,                 -- 121
      sequence_of_statements_list_ID,        -- 122
      sequence_of_statements_ID,             -- 123
      simple_expression_ID,                  -- 124
      simple_return_statement_ID,            -- 125
      simple_statement_ID,                   -- 126
      statement_ID,                          -- 127
      subprogram_body_ID,                    -- 128
      subprogram_body_stub_ID,               -- 129
      subprogram_declaration_ID,             -- 130
      subprogram_specification_ID,           -- 131
      subtype_indication_ID,                 -- 132
      term_ID,                               -- 133
      term_list_ID,                          -- 134
      type_declaration_ID,                   -- 135
      type_definition_ID,                    -- 136
      unary_adding_operator_ID               -- 137
     );

   subtype Terminal is Token_Enum_ID range AMP_ID .. separate_ID;
   subtype Nonterminal is Token_Enum_ID range compilation_unit_list_ID .. unary_adding_operator_ID;

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
   is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));

   function To_Token_Enum (ID : in WisiToken.Token_ID) return Token_Enum_ID
   is (Token_Enum_ID'Val (ID - WisiToken.Token_ID'First));

   Descriptor : constant WisiToken.Descriptor :=
     (First_Terminal    => +AMP_ID,
      Last_Terminal     => +separate_ID,
      First_Nonterminal => +compilation_unit_list_ID,
      Last_Nonterminal  => +unary_adding_operator_ID,
      EOI_ID            => WisiToken.Invalid_Token_ID,
      Accept_ID         => WisiToken.Invalid_Token_ID,
      Case_Insensitive  => True,
      New_Line_ID       => WisiToken.Invalid_Token_ID,

      String_1_ID => +STRING_LITERAL_ID,
      String_2_ID => WisiToken.Invalid_Token_ID,
      Image =>
        (new String'("<eoi>"),
         new String'("&"),
         new String'(":"),
         new String'(":="),
         new String'(","),
         new String'("."),
         new String'(".."),
         new String'("="),
         new String'("=>"),
         new String'(">"),
         new String'(">="),
         new String'("<"),
         new String'("<="),
         new String'("-"),
         new String'("+"),
         new String'(";"),
         new String'("/"),
         new String'("/="),
         new String'("*"),
         new String'("STRING_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("COMMENT"),
         new String'("NUMERIC_LITERAL"),
         new String'("("),
         new String'(")"),
         new String'("with"),
         new String'("declare"),
         new String'("begin"),
         new String'("end"),
         new String'("case"),
         new String'("is"),
         new String'("when"),
         new String'("range"),
         new String'("elsif"),
         new String'("then"),
         new String'("exit"),
         new String'("return"),
         new String'("not"),
         new String'("type"),
         new String'("function"),
         new String'("package"),
         new String'("new"),
         new String'("procedure"),
         new String'("exception"),
         new String'("if"),
         new String'("else"),
         new String'("for"),
         new String'("in"),
         new String'("loop"),
         new String'("body"),
         new String'("and"),
         new String'("or"),
         new String'("xor"),
         new String'("separate"),
         new String'("compilation_unit_list"),
         new String'("actual_parameter_part"),
         new String'("aspect_specification_opt"),
         new String'("assignment_statement"),
         new String'("association_opt"),
         new String'("association_list"),
         new String'("binary_adding_operator"),
         new String'("block_label_opt"),
         new String'("block_statement"),
         new String'("body_g"),
         new String'("body_stub"),
         new String'("case_statement"),
         new String'("case_statement_alternative"),
         new String'("case_statement_alternative_list"),
         new String'("compilation_unit"),
         new String'("compound_statement"),
         new String'("constraint"),
         new String'("declaration"),
         new String'("declarations"),
         new String'("declarative_part"),
         new String'("discrete_subtype_definition"),
         new String'("elsif_statement_item"),
         new String'("elsif_statement_list"),
         new String'("enumeration_literal_list"),
         new String'("enumeration_type_definition"),
         new String'("exception_choice"),
         new String'("exception_handler"),
         new String'("exception_handler_list"),
         new String'("exception_handler_list_opt"),
         new String'("exit_statement"),
         new String'("expression"),
         new String'("expression_opt"),
         new String'("extended_return_object_declaration"),
         new String'("extended_return_statement"),
         new String'("factor"),
         new String'("formal_part"),
         new String'("full_type_declaration"),
         new String'("function_specification"),
         new String'("generic_instantiation"),
         new String'("handled_sequence_of_statements"),
         new String'("identifier_opt"),
         new String'("if_statement"),
         new String'("index_constraint"),
         new String'("iteration_scheme"),
         new String'("loop_statement"),
         new String'("multiplying_operator"),
         new String'("name"),
         new String'("name_opt"),
         new String'("object_declaration"),
         new String'("package_body"),
         new String'("package_specification"),
         new String'("parameter_and_result_profile"),
         new String'("parameter_profile_opt"),
         new String'("parameter_specification"),
         new String'("parameter_specification_list"),
         new String'("paren_expression"),
         new String'("primary"),
         new String'("procedure_call_statement"),
         new String'("procedure_specification"),
         new String'("proper_body"),
         new String'("range_g"),
         new String'("range_list"),
         new String'("relation_and_list"),
         new String'("relation_or_list"),
         new String'("relation_xor_list"),
         new String'("relation"),
         new String'("relational_operator"),
         new String'("selected_component"),
         new String'("sequence_of_statements_list"),
         new String'("sequence_of_statements"),
         new String'("simple_expression"),
         new String'("simple_return_statement"),
         new String'("simple_statement"),
         new String'("statement"),
         new String'("subprogram_body"),
         new String'("subprogram_body_stub"),
         new String'("subprogram_declaration"),
         new String'("subprogram_specification"),
         new String'("subtype_indication"),
         new String'("term"),
         new String'("term_list"),
         new String'("type_declaration"),
         new String'("type_definition"),
         new String'("unary_adding_operator")),

      Terminal_Image_Width => 0,
      Image_Width          => 0,
      Last_Lookahead       => WisiToken.Invalid_Token_ID);

end Ada_Lite_Tree_Sitter_Actions;
