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
   procedure Put (State : not null access State_Type) is null;
   --  Only used for debugging.

   overriding
   procedure Reset (State : not null access State_Type) is null;
   --  Elisp tells the parser to reset, so it has already reset the
   --  state.

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   overriding
   procedure Error
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set);

   overriding
   procedure Spawn
     (State         : not null access State_Type;
      Old_Parser_ID : in              Natural;
      New_Parser_ID : in              Natural);

   overriding
   procedure Terminate_Parser
     (State     : not null access State_Type;
      Parser_ID : in              Natural);

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID);

   overriding
   procedure Push_Current
     (State : not null access State_Type;
      ID    : in              Token_ID);

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      IDs     : in              Token_Array;
      Action  : in              Semantic_Action);

   overriding
   procedure Discard_Lookahead
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID);

   overriding
   procedure Discard_Stack
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID);

   overriding
   procedure Recover
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Recover   : in              WisiToken.Token.Recover_Data'Class);

end WisiToken.Token_Emacs_Process;
