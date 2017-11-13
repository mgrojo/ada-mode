--  generated by WisiToken Wisi from gpr.wy
--  with command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process gpr.wy
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

with WisiToken.Wisi_Runtime;
with WisiToken.Token;
with WisiToken.Parser.LR.Parser;
package Gpr_Process is

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal    => 3,
      Last_Terminal     => 37,
      First_Nonterminal => 38,
      Last_Nonterminal  => 71,
      EOF_ID            => 37,
      Accept_ID         => 38,
      New_Line_ID       => 1,
      Comment_ID        => 2147483647,
      Left_Paren_ID     => 2147483647,
      Right_Paren_ID    => 2147483647,
      Image             =>
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

   Parse_Data : WisiToken.Wisi_Runtime.Parse_Data_Type;

   procedure Create_Parser
     (Parser         :    out WisiToken.Parser.LR.Parser.Instance;
      Algorithm      : in     WisiToken.Parser_Algorithm_Type;
      Semantic_State : in     WisiToken.Token.Semantic_State_Access);

end Gpr_Process;
