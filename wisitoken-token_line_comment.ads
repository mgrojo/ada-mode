--  Abstract :
--
--  Store token regions.
--
--  The parser deals only with token_ids; this package adds additional
--  information.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Lexer;
with WisiToken.Token_Region;
package WisiToken.Token_Line_Comment is

   type Token is new WisiToken.Token_Region.Token with record
      First : Boolean;
      --  For a terminal, True if it is first on a line, or if it contains
      --  trailing blank or comment lines.
      --
      --  For a nonterminal, True if some contained token's First is True.

      Non_Grammar : WisiToken.Augmented_Token_Array;
      --  Non_Grammar tokens between this token and the next grammar one or
      --  EOF.
      --
      --  Only set when State.Parse_Action is Indent, and only for terminal
      --  tokens; for nonterminals, trailing Non_Grammar tokens are stored
      --  in the last token in the nonterminal region, and *_Region includes
      --  them.

      First_Indent_Line : Line_Number_Type;
      Last_Indent_Line  : Line_Number_Type;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token.
      --
      --  First_, Last_Indent_Line include comments between tokens, but
      --  exclude trailing comments after the last token, so they can be
      --  indented differently.

      First_Trailing_Comment_Line : Line_Number_Type;
      Last_Trailing_Comment_Line  : Line_Number_Type;
      --  Trailing comment or blank lines (contained by the last contained
      --  token) that need indenting.
   end record;

   type State_Type is new WisiToken.Token_Region.State_Type with record
      Initial_Non_Grammar : WisiToken.Augmented_Token_Array;
      --  Non_Grammar tokens before first grammar token.
   end record;
   type State_Access is access all State_Type;

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID);

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      IDs     : in              WisiToken.Token_Array;
      Action  : in              Semantic_Action);

end WisiToken.Token_Line_Comment;
