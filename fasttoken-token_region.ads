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

pragma License (GPL);

with Ada.Containers.Bounded_Vectors;
with FastToken.Lexer;
with FastToken.Token;
generic
   with package Token_Pkg is new FastToken.Token (<>);
   with package Lexer is new FastToken.Lexer (<>);
   Max_Stack_Size : in Ada.Containers.Count_Type := 500;
package FastToken.Token_Region is

   type Token is tagged record
      ID     : Token_Pkg.Grammar_ID;
      Region : Buffer_Region;
   end record;

   function Image (Item : in Token; ID_Only : in Boolean) return String;
   --  Return a string for debug/test messages

   function Get (ID : in Token_Pkg.Grammar_ID) return Token;
   --  Return a token with ID; other components from Default_Token.

   Default_Token : constant Token := (Token_Pkg.Grammar_ID'First, Null_Buffer_Region);

   subtype Stack_Index_Type is Ada.Containers.Count_Type range 1 .. Max_Stack_Size;
   package Token_Stacks is new Ada.Containers.Bounded_Vectors (Stack_Index_Type, Token);

   subtype Token_List_Type is Token_Stacks.Vector (Capacity => Max_Stack_Size);

   type State is record
      Stack : Token_List_Type;
      --  Top of stack is Stack.Length

      Invalid_Regions : Region_Lists.List;
   end record;

   type State_Access is access all State;

   type Semantic_Action is access procedure
     (Nonterm : in Token'Class;
      Index   : in Natural;
      Source  : in Token_List_Type);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production (0 origin);
   --  Source is the right hand side tokens.
   --
   --  Nonterm is classwide to avoid freezing rules.

   procedure Null_Semantic_Action
     (Nonterm : in Token'Class;
      Index   : in Natural;
      Source  : in Token_List_Type)
     is null;

   Null_Action : constant Semantic_Action := Null_Semantic_Action'Access;

   procedure Push_Token
     (Token : in Token_Pkg.Grammar_ID;
      State : in State_Access;
      Lexer : in Token_Region.Lexer.Handle);

   procedure Merge_Tokens
     (Nonterm : in Token_Pkg.Nonterminal_ID;
      Index   : in Natural;
      Tokens  : in Token_Pkg.List.Instance;
      Action  : in Semantic_Action;
      State   : in State_Access);
   --  For instantiating fasttoken-parser-lr-parser.ads.
   --  Merge top items on State.Stack matching Token_IDs into one new token
   --  with Nonterm ID, call Semantic_Action, push it onto State.Stack.

end FastToken.Token_Region;
