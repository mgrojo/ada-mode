--  generated parser support file.
--  command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process wisi_grammar_1.wy
--

--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with WisiToken.Syntax_Trees;
package Wisi_Grammar_1_Process_Actions is

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal                => 3,
      Last_Terminal                 => 25,
      First_Nonterminal             => 26,
      Last_Nonterminal              => 37,
      EOF_ID                        => 25,
      Accept_ID                     => 26,
      Case_Insensitive              => False,
      New_Line_ID                   => 1,
      Comment_ID                    => 2,
      Left_Paren_ID                 => 2147483647,
      Right_Paren_ID                => 2147483647,
      String_1_ID                   => 24,
      String_2_ID                   => 23,
      Embedded_Quote_Escape_Doubled => False,
      Image                         =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("CODE"),
         new String'("END"),
         new String'("IF"),
         new String'("KEYWORD"),
         new String'("NON_GRAMMAR"),
         new String'("TOKEN"),
         new String'("RAW_CODE"),
         new String'("REGEXP"),
         new String'("ACTION"),
         new String'("BAR"),
         new String'("COLON"),
         new String'("COMMA"),
         new String'("EQUAL"),
         new String'("GREATER"),
         new String'("LESS"),
         new String'("PERCENT"),
         new String'("SEMICOLON"),
         new String'("SLASH"),
         new String'("NUMERIC_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL"),
         new String'("STRING_LITERAL_CASE_INS"),
         new String'("Wisi_EOI"),
         new String'("wisitoken_accept"),
         new String'("declaration"),
         new String'("token_keyword_non_grammar"),
         new String'("identifier_list"),
         new String'("declaration_item_list"),
         new String'("declaration_item"),
         new String'("nonterminal"),
         new String'("rhs_list"),
         new String'("rhs"),
         new String'("token_list"),
         new String'("compilation_unit"),
         new String'("compilation_unit_list")),
      Terminal_Image_Width => 23,
      Image_Width          => 25);

   procedure declaration_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_5
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure token_keyword_non_grammar_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure token_keyword_non_grammar_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure declaration_item_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure nonterminal_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure rhs_list_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure rhs_list_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure rhs_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
   procedure rhs_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);
end Wisi_Grammar_1_Process_Actions;
