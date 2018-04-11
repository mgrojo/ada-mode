--  generated by WisiToken Wisi from wisi_grammar.wy
--  with command line: wisi-generate.exe -v 1 wisi_grammar.wy
--

with WisiToken.Syntax_Trees;
with WisiToken.LR.Parser_No_Recover;
package Wisi_Grammar is

   type Token_Enum_ID is
     (
      WHITESPACE_ID,
      NEW_LINE_ID,
      COMMENT_ID,
      END_ID,
      IF_ID,
      KEYWORD_ID,
      NON_GRAMMAR_ID,
      TOKEN_ID,
      PREAMBLE_ID,
      REGEXP_ID,
      ACTION_ID,
      BAR_ID,
      COLON_ID,
      COMMA_ID,
      EQUAL_ID,
      GREATER_ID,
      LESS_ID,
      PERCENT_ID,
      PERCENT_PERCENT_ID,
      SEMICOLON_ID,
      SLASH_ID,
      NUMERIC_LITERAL_ID,
      IDENTIFIER_ID,
      STRING_LITERAL_ID,
      STRING_LITERAL_CASE_INS_ID,
      Wisi_EOI_ID,
      wisitoken_accept_ID,
      declaration_ID,
      token_keyword_non_grammar_ID,
      declaration_item_list_ID,
      declaration_item_ID,
      nonterminal_ID,
      rhs_list_ID,
      rhs_ID,
      token_list_ID,
      compilation_unit_ID,
      compilation_unit_list_ID);

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

   function "-" (Item : in WisiToken.Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (WisiToken."-" (Item, WisiToken.Token_ID'First)));

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal    => 3,
      Last_Terminal     => 25,
      First_Nonterminal => 26,
      Last_Nonterminal  => 36,
      EOF_ID            => 25,
      Accept_ID         => 26,
      Case_Insensitive  => False,
      New_Line_ID       => 1,
      Comment_ID        => 2,
      Left_Paren_ID     => 2147483647,
      Right_Paren_ID    => 2147483647,
      String_1_ID       => 24,
      String_2_ID       => 23,
      Image             =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("END"),
         new String'("IF"),
         new String'("KEYWORD"),
         new String'("NON_GRAMMAR"),
         new String'("TOKEN"),
         new String'("PREAMBLE"),
         new String'("REGEXP"),
         new String'("ACTION"),
         new String'("BAR"),
         new String'("COLON"),
         new String'("COMMA"),
         new String'("EQUAL"),
         new String'("GREATER"),
         new String'("LESS"),
         new String'("PERCENT"),
         new String'("PERCENT_PERCENT"),
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

   procedure Create_Parser
     (Parser    :    out          WisiToken.LR.Parser_No_Recover.Parser;
      Algorithm : in              WisiToken.Generator_Algorithm_Type;
      Trace     : not null access WisiToken.Trace'Class;
      User_Data : in              WisiToken.Syntax_Trees.User_Data_Access);

end Wisi_Grammar;
