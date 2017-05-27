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
generic
   with package Token_Pkg is new FastToken.Token (<>);
   with package Lexer is new FastToken.Lexer (<>);
package FastToken.Token_Plain is

   type Semantic_Action is access procedure
     (Nonterm : in Token_Pkg.Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token_Pkg.List.Instance);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production (0 origin);
   --  Source is the right hand side tokens.

   procedure Null_Semantic_Action
     (Nonterm : in Token_Pkg.Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token_Pkg.List.Instance)
     is null;

   Null_Action : constant Semantic_Action := Null_Semantic_Action'Access;

   type State_Type is tagged null record;

   procedure Reset (State : access State_Type) is null;

   procedure Input_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type;
      Lexer : in     Token_Plain.Lexer.Handle)
     is null;

   procedure Push_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type)
   is null;

   procedure Merge_Tokens
     (Nonterm : in     Token_Pkg.Nonterminal_ID;
      Index   : in     Natural;
      Tokens  : in     Token_Pkg.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type)
     is null;

   procedure Recover
     (Popped_Tokens  : in     Token_Pkg.List.Instance;
      Skipped_Tokens : in     Token_Pkg.List.Instance;
      Pushed_Token   : in     Token_Pkg.Nonterminal_ID;
      State          : access State_Type)
     is null;

   State : aliased State_Type;

end FastToken.Token_Plain;
