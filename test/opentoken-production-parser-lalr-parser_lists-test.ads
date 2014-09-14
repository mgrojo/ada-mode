--  Access to lalr.parser.parser_lists internals for unit test

generic
package OpenToken.Production.Parser.LALR.Parser_Lists.Test is

   procedure Check_Action_Stack
     (Label  : in String;
      Cursor : in Parser_Lists.Cursor);
   --  Verify that all Action_Token.New_Token point to tokens on
   --  Stack or later Action_Token.Tokens, and that all
   --  nonterminals in Stack and Action_Token.Tokens are pointed to
   --  by previous Action_Token.New_Token.

end OpenToken.Production.Parser.LALR.Parser_Lists.Test;
