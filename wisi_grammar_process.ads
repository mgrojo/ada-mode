--  generated by WisiToken Wisi from wisi_grammar.wy
--  with command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum
--  wisi_grammar.wy
--

with WisiToken.Wisi_Runtime.Wisi_Grammar;
with WisiToken.Semantic_State;
with WisiToken.LR;
package Wisi_Grammar_Process is

   Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal      => 3,
      Last_Terminal       => 25,
      First_Nonterminal   => 26,
      Last_Nonterminal    => 39,
      EOF_ID              => 25,
      Accept_ID           => 26,
      New_Line_ID         => 1,
      Comment_ID          => 2,
      Left_Paren_ID       => 2147483647,
      Right_Paren_ID      => 2147483647,
      Terminal_Name_ID    => 22,
      Nonterminal_Name_ID => 2147483647,
      Image               =>
        (new String'("WHITESPACE"),
         new String'("NEW_LINE"),
         new String'("COMMENT"),
         new String'("PERCENT"),
         new String'("PERCENT_PERCENT"),
         new String'("BACKSLASH"),
         new String'("BAR"),
         new String'("CARET"),
         new String'("COLON"),
         new String'("COMMA"),
         new String'("GREATER"),
         new String'("LESS"),
         new String'("LEFT_BRACKET"),
         new String'("LEFT_PAREN"),
         new String'("MINUS"),
         new String'("PLUS"),
         new String'("RIGHT_BRACKET"),
         new String'("RIGHT_PAREN"),
         new String'("SEMICOLON"),
         new String'("SLASH"),
         new String'("STAR"),
         new String'("NUMERIC_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL"),
         new String'("PREAMBLE"),
         new String'("Wisi_EOI"),
         new String'("wisitoken_accept"),
         new String'("declaration"),
         new String'("declaration_list"),
         new String'("declaration_item_list"),
         new String'("declaration_item"),
         new String'("nonterminal"),
         new String'("nonterminal_list"),
         new String'("rhs_list"),
         new String'("rhs"),
         new String'("token_list"),
         new String'("action"),
         new String'("action_item_list"),
         new String'("action_item"),
         new String'("compilation_unit")),
      Terminal_Image_Width => 15,
      Image_Width          => 21);

   type Token_Enum_ID is
     (WHITESPACE_ID,
      NEW_LINE_ID,
      COMMENT_ID,
      PERCENT_ID,
      PERCENT_PERCENT_ID,
      BACKSLASH_ID,
      BAR_ID,
      CARET_ID,
      COLON_ID,
      COMMA_ID,
      GREATER_ID,
      LESS_ID,
      LEFT_BRACKET_ID,
      LEFT_PAREN_ID,
      MINUS_ID,
      PLUS_ID,
      RIGHT_BRACKET_ID,
      RIGHT_PAREN_ID,
      SEMICOLON_ID,
      SLASH_ID,
      STAR_ID,
      NUMERIC_LITERAL_ID,
      IDENTIFIER_ID,
      STRING_LITERAL_ID,
      PREAMBLE_ID,
      Wisi_EOI_ID,
      wisitoken_accept_ID,
      declaration_ID,
      declaration_list_ID,
      declaration_item_list_ID,
      declaration_item_ID,
      nonterminal_ID,
      nonterminal_list_ID,
      rhs_list_ID,
      rhs_ID,
      token_list_ID,
      action_ID,
      action_item_list_ID,
      action_item_ID,
      compilation_unit_ID);

   use all type WisiToken.Token_ID;
   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));
   function "-" (Item : in WisiToken.Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));

   Parse_Data : WisiToken.Wisi_Runtime.Wisi_Grammar.Parse_Data_Type;

   procedure Create_Parser
     (Parser         :    out WisiToken.LR.Instance;
      Algorithm      : in     WisiToken.Parser_Algorithm_Type;
      Semantic_State : in     WisiToken.Semantic_State.Semantic_State_Access);

end Wisi_Grammar_Process;
