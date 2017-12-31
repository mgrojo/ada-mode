--  generated by WisiToken Wisi from gpr.wy
--  with command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum gpr.wy
--
--  Copyright (C) 2013 - 2015 Free Software Foundation, Inc.

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

with WisiToken.Wisi_Runtime.Gpr;
with WisiToken.Semantic_State;
with WisiToken.LR;
package Gpr_Process is

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal      => 3,
      Last_Terminal       => 37,
      First_Nonterminal   => 38,
      Last_Nonterminal    => 71,
      EOF_ID              => 37,
      Accept_ID           => 38,
      New_Line_ID         => 1,
      Comment_ID          => 2,
      Left_Paren_ID       => 2147483647,
      Right_Paren_ID      => 2147483647,
      Terminal_Name_ID    => 35,
      Nonterminal_Name_ID => 2147483647,
      String_1_ID         => 2147483647,
      String_2_ID         => 36,
      Image               =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("ABSTRACT"),
         new String'("AGGREGATE"),
         new String'("CASE"),
         new String'("CONFIGURATION"),
         new String'("END"),
         new String'("EXTENDS"),
         new String'("EXTERNAL"),
         new String'("EXTERNAL_AS_LIST"),
         new String'("FOR"),
         new String'("IS"),
         new String'("LEFT_PAREN"),
         new String'("LIBRARY"),
         new String'("NULL"),
         new String'("OTHERS"),
         new String'("PACKAGE"),
         new String'("PROJECT"),
         new String'("RENAMES"),
         new String'("RIGHT_PAREN"),
         new String'("STANDARD"),
         new String'("TYPE"),
         new String'("USE"),
         new String'("WHEN"),
         new String'("WITH"),
         new String'("AMPERSAND"),
         new String'("COLON"),
         new String'("COLON_EQUALS"),
         new String'("COMMA"),
         new String'("DOT"),
         new String'("EQUAL_GREATER"),
         new String'("QUOTE"),
         new String'("SEMICOLON"),
         new String'("VERTICAL_BAR"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL"),
         new String'("Wisi_EOI"),
         new String'("wisitoken_accept"),
         new String'("aggregate_g"),
         new String'("attribute_declaration"),
         new String'("attribute_prefix"),
         new String'("attribute_reference"),
         new String'("case_statement"),
         new String'("case_item"),
         new String'("case_items"),
         new String'("compilation_unit"),
         new String'("context_clause"),
         new String'("context_clause_opt"),
         new String'("declarative_item"),
         new String'("declarative_items"),
         new String'("declarative_items_opt"),
         new String'("discrete_choice"),
         new String'("discrete_choice_list"),
         new String'("expression"),
         new String'("external_value"),
         new String'("identifier_opt"),
         new String'("name"),
         new String'("package_declaration"),
         new String'("package_spec"),
         new String'("package_extension"),
         new String'("package_renaming"),
         new String'("project_declaration_opt"),
         new String'("project_extension"),
         new String'("project_qualifier_opt"),
         new String'("simple_declarative_item"),
         new String'("simple_project_declaration"),
         new String'("string_primary"),
         new String'("string_list"),
         new String'("term"),
         new String'("typed_string_declaration"),
         new String'("with_clause")),
      Terminal_Image_Width => 16,
      Image_Width          => 26);

   type Token_Enum_ID is
     (WHITESPACE_ID,
      NEW_LINE_ID,
      COMMENT_ID,
      ABSTRACT_ID,
      AGGREGATE_ID,
      CASE_ID,
      CONFIGURATION_ID,
      END_ID,
      EXTENDS_ID,
      EXTERNAL_ID,
      EXTERNAL_AS_LIST_ID,
      FOR_ID,
      IS_ID,
      LEFT_PAREN_ID,
      LIBRARY_ID,
      NULL_ID,
      OTHERS_ID,
      PACKAGE_ID,
      PROJECT_ID,
      RENAMES_ID,
      RIGHT_PAREN_ID,
      STANDARD_ID,
      TYPE_ID,
      USE_ID,
      WHEN_ID,
      WITH_ID,
      AMPERSAND_ID,
      COLON_ID,
      COLON_EQUALS_ID,
      COMMA_ID,
      DOT_ID,
      EQUAL_GREATER_ID,
      QUOTE_ID,
      SEMICOLON_ID,
      VERTICAL_BAR_ID,
      IDENTIFIER_ID,
      STRING_LITERAL_ID,
      Wisi_EOI_ID,
      wisitoken_accept_ID,
      aggregate_g_ID,
      attribute_declaration_ID,
      attribute_prefix_ID,
      attribute_reference_ID,
      case_statement_ID,
      case_item_ID,
      case_items_ID,
      compilation_unit_ID,
      context_clause_ID,
      context_clause_opt_ID,
      declarative_item_ID,
      declarative_items_ID,
      declarative_items_opt_ID,
      discrete_choice_ID,
      discrete_choice_list_ID,
      expression_ID,
      external_value_ID,
      identifier_opt_ID,
      name_ID,
      package_declaration_ID,
      package_spec_ID,
      package_extension_ID,
      package_renaming_ID,
      project_declaration_opt_ID,
      project_extension_ID,
      project_qualifier_opt_ID,
      simple_declarative_item_ID,
      simple_project_declaration_ID,
      string_primary_ID,
      string_list_ID,
      term_ID,
      typed_string_declaration_ID,
      with_clause_ID);

   use all type WisiToken.Token_ID;
   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));
   function "-" (Item : in WisiToken.Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));

   Parse_Data : WisiToken.Wisi_Runtime.Gpr.Parse_Data_Type;

   procedure Create_Parser
     (Parser         :    out WisiToken.LR.Instance;
      Algorithm      : in     WisiToken.Parser_Algorithm_Type;
      Semantic_State : in     WisiToken.Semantic_State.Semantic_State_Access);

end Gpr_Process;
