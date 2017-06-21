--  Abstract :
--
--  Just Token_IDs, for unit tests.
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

with FastToken.Lexer;
with FastToken.Token;
package FastToken.Token_Plain is

   type Augmented_Token is new FastToken.Augmented_Token with null record;

   type State_Type is new FastToken.Token.Semantic_State with null record;

   overriding procedure Reset (State : access State_Type) is null;

   overriding procedure Input_Token
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle)
     is null;

   overriding procedure Push_Token
     (Token : in     Token_ID;
      State : access State_Type)
   is null;

   overriding procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type);
   --  Puts trace of production, and calls Action if non-null;
   --  otherwise does nothing.

   overriding procedure Recover
     (Popped_Tokens  : in     Token.List.Instance;
      Skipped_Tokens : in     Token.List.Instance;
      Pushed_Token   : in     Token_ID;
      State          : access State_Type)
     is null;

end FastToken.Token_Plain;
