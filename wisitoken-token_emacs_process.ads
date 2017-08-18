--  Abstract :
--
--  Communicate with the Emacs Ada mode wisi lexer over a process interface.
--
--  Reference:
--
--  [1] Emacs ada-mode wisi-process-parse.el function
--  wisi-process-parse--execute defines the elisp to Ada protocol
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

with WisiToken.Lexer;
with WisiToken.Token;
package WisiToken.Token_Emacs_Process is

   type Augmented_Token is new WisiToken.Augmented_Token with null record;

   type State_Type is new WisiToken.Token.Semantic_State with null record;
   --  The augmented tokens stack and input queue are kept in Emacs
   --  lisp, with the lexer, to minimize traffic on the process
   --  interface. The operations on State_Type send information over
   --  the interface to the Emacs lisp code; it then implements the
   --  operations.

   overriding
   procedure Put (State : access State_Type) is null;
   --  Only used for debugging.

   overriding
   procedure Reset (State : access State_Type) is null;
   --  Elisp tells the parser to reset, so it has already reset the
   --  state.

   overriding
   procedure Input_Token
     (State : access State_Type;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle);

   overriding
   procedure Input_Lookahead
     (State : access State_Type;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle);

   overriding
   procedure Move_Lookahead_To_Input
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Move_Input_To_Lookahead
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Push_Token
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Error
     (State     : access State_Type;
      Expecting : in     Token_ID_Set);

   overriding
   procedure Discard_Input
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Discard_Lookahead
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Pop_Token
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Merge_Tokens
     (State   : access State_Type;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action);

   overriding
   procedure Recover
     (State   : access State_Type;
      Recover : in     WisiToken.Token.Recover_Data'Class)
     is null;
   --  Not used.

end WisiToken.Token_Emacs_Process;
