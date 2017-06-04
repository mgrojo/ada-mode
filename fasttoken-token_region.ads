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

with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Token;
with SAL.Gen_Definite_Queues;
package FastToken.Token_Region is

   type Token is new FastToken.Augmented_Token with record
      Region : Buffer_Region;
   end record;

   function Image (Item : in Token; ID_Only : in Boolean) return String;
   --  Return a string for debug/test messages

   function Get (ID : in Token_Pkg.Grammar_ID) return Token;
   --  Return a token with ID; other components from Default_Token.

   Default_Token : constant Token := (Token_Pkg.Grammar_ID'First, Null_Buffer_Region);


   package Token_Queues is new SAL.Gen_Definite_Queues (Token);

   type State_Type is new Augmented_State with record
      Stack : Token_Stack_Type;
      --  Top of stack is Stack.Last_Index; Push = Append, Pop = Delete_Last.

      Pending_Input : Token_Queues.Queue_Type (Integer (Max_Stack_Size));
      --  Tokens are kept in Pending_Input during parallel parser
      --  execution, and during error recovery. Max_Stack_Size is a
      --  reasonable guess for the maximum queue needed; better would
      --  be to set it at run-time based on history of compiling a
      --  particular project.

      Invalid_Regions : Region_Lists.List;
   end record;

   procedure Reset (State : access State_Type);

   procedure Input_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type;
      Lexer : in     Token_Region.Lexer.Handle);
   --  Parser just fetched Token from Lexer; save it on the input
   --  queue for later push or recover operations.

   procedure Push_Token
     (ID    : in     Token_Pkg.Terminal_ID;
      State : access State_Type);

   procedure Merge_Tokens
     (Nonterm : in     Token_Pkg.Nonterminal_ID;
      Index   : in     Natural;
      Tokens  : in     Token_Pkg.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type);
   --  For instantiating fasttoken-parser-lr-parser.ads.
   --  Merge top items on State.Stack matching Token_IDs into one new token
   --  with Nonterm ID, call Semantic_Action, push it onto State.Stack.

   procedure Recover
     (Popped_Tokens  : in     Token_Pkg.List.Instance;
      Skipped_Tokens : in     Token_Pkg.List.Instance;
      Pushed_Token   : in     Token_Pkg.Nonterminal_ID;
      State          : access State_Type);
   --  For instantiating fasttoken-parser-lr-parser.ads.
   --  An error recover algorithm succeeded; adjust the augmented
   --  token stack to match, add region to State.Invalid_Region

end FastToken.Token_Region;
