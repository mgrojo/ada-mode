--  Access to lalr.parser.parser_lists internals for unit test

with OpenToken.Token.Enumerated.List.Print;
generic
   with package Token_List_Print is new Token_List.Print;
package OpenToken.Production.Parser.LALR.Parser_Lists.Test is

   procedure Put_Action_Tokens (Cursor : in Parser_Lists.Cursor);

   procedure Check_Action_Stack
     (Label  : in String;
      Cursor : in Parser_Lists.Cursor);
   --  Verify that all Action_Token.New_Token point to tokens on
   --  Stack or later Action_Token.Tokens, and that all
   --  nonterminals in Stack and Action_Token.Tokens are pointed to
   --  by previous Action_Token.New_Token.

end OpenToken.Production.Parser.LALR.Parser_Lists.Test;
