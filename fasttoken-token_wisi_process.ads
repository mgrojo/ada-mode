--  Abstract :
--
--  Communicate with the Emacs Ada mode wisi lexer over a process interface.
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

package FastToken.Token_Wisi_Process is

   type Semantic_Action is access procedure
     (Nonterm : in Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token.List.Instance);

   procedure Null_Semantic_Action
     (Nonterm : in Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token.List.Instance)
     is null;

   Null_Action : constant Semantic_Action := Null_Semantic_Action'Access;

   type State_Type is tagged null record;
   --  The augmented tokens stack and input queue are kept in Emacs lisp,
   --  with the lexer, to minimize traffic on the process interface.

   procedure Reset (State : access State_Type) is null;
   --  Elisp tells the parser to reset, so it has already reset the
   --  state.

   procedure Input_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type;
      Lexer : in     Token_Plain.Lexer.Handle)
     is null;
   --  Elisp lexes ahead and sends the tokens in parallel with parser
   --  execution; they are already in the queue.

   procedure Push_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type);

   procedure Merge_Tokens
     (Nonterm : in Token.Nonterminal_ID;
      Index   : in Natural;
      Tokens  : in Token.List.Instance;
      Action  : in Semantic_Action);

   procedure Recover
     (Popped_Tokens  : in     Token_Pkg.List.Instance;
      Skipped_Tokens : in     Token_Pkg.List.Instance;
      Pushed_Token   : in     Token_Pkg.Nonterminal_ID;
      State          : access State_Type);

   State : aliased State_Type;

end FastToken.Token_Wisi;
