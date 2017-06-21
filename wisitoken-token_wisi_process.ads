--  Abstract :
--
--  Communicate with the Emacs Ada mode wisi lexer over a process interface.
--
--  Reference:
--
--  [1] Emacs wisi-ext-parse.el wisi-ext-parse-execute defines protocol
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

pragma License (GPL);

with FastToken.Lexer;
with FastToken.Token;
package FastToken.Token_Wisi_Process is

   type Augmented_Token is new FastToken.Augmented_Token with null record;

   type State_Type is new FastToken.Token.Semantic_State with null record;
   --  The augmented tokens stack and input queue are kept in Emacs lisp,
   --  with the lexer, to minimize traffic on the process interface.

   overriding
   procedure Reset (State : access State_Type) is null;
   --  Elisp tells the parser to reset, so it has already reset the
   --  state.

   overriding
   procedure Input_Token
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle);
   --  Elisp lexes ahead and sends the tokens in parallel with parser
   --  execution; they are already in the queue. Echoes the tokens if
   --  Trace_Parse > 3.

   overriding
   procedure Push_Token
     (Token : in     Token_ID;
      State : access State_Type);

   overriding
   procedure Error
     (Expecting : in     Token_ID_Set;
      State     : access State_Type);

   overriding
   procedure Discard_Token
     (Token : in     Token_ID;
      State : access State_Type);

   overriding
   procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type);

   overriding
   procedure Recover
     (Popped_Tokens : in     Token.List.Instance;
      Pushed_Tokens : in     Token.List.Instance;
      State         : access State_Type);

end FastToken.Token_Wisi_Process;
