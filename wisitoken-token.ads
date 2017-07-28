--  Abstract :
--
--  Utilities for tokens.
--
--  Copyright (C) 2017 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with WisiToken.Lexer;
package WisiToken.Token is

   ----------
   --  Token lists

   package List is
      --  FIXME: replace with Token_Array?

      type Instance is tagged private;

      Null_List : constant Instance;

      function Is_Empty (Item : in Instance) return Boolean;

      function Length (Item : in Instance) return Ada.Containers.Count_Type;

      function Only (Subject : in Token_ID) return Instance;

      function "&" (Left : in Token_ID; Right : in Token_ID) return Instance;
      function "&" (Left : in Instance; Right : in Token_ID) return Instance;

      procedure Clean (List : in out Instance);
      --  Delete and free all elements of List

      procedure Free (List : in out Instance) renames Clean;
      procedure Clear (List : in out Instance) renames Clean;

      function Deep_Copy (Item : in Instance) return Instance;

      type List_Iterator is private;
      Null_Iterator : constant List_Iterator;

      function First (List : in Instance) return List_Iterator;

      function Pop (List : in out Instance) return Token_ID;
      --  Delete head of List from List, return it.

      function Peek (List : in out Instance; I : in Integer) return Token_ID;
      --  Return the Ith element; head is 1.

      procedure Next (Iterator : in out List_Iterator);
      function Next (Iterator : in List_Iterator) return List_Iterator;
      --  Null_Iterator if there is no next token.

      function Is_Done (Iterator : in List_Iterator) return Boolean;
      function Is_Null (Iterator : in List_Iterator) return Boolean renames Is_Done;

      function Current (Iterator : in List_Iterator) return Token_ID;
      function ID (Iterator : in List_Iterator) return Token_ID renames Current;

      procedure Prepend (List : in out Instance; Item : in Token_ID);
      --  Add Token to the head of List.

      procedure Append (List  : in out Instance; Item : in Token_ID);
      --  Append to tail of List.

      procedure Put_Trace (Trace : in out WisiToken.Trace'Class; Item : in Instance);
      --  Put Item to Put_Trace.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         ID   : Token_ID;
         Next : List_Node_Ptr;
      end record;

      type Instance is tagged record
         Head : List_Node_Ptr;
         Tail : List_Node_Ptr;
      end record;

      type List_Iterator is new List_Node_Ptr;
      Null_Iterator : constant List_Iterator := null;

      Null_List : constant Instance := (null, null);
   end List;

   ----------
   --  Semantic state

   type Semantic_State (Trace : not null access WisiToken.Trace'Class) is abstract tagged limited null record;
   --  For storing augmented tokens, other semantic information

   procedure Reset (State : access Semantic_State) is abstract;
   --  Reset State to start a new parse.

   procedure Input_Token
     (State : access Semantic_State;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
     is abstract;
   --  If Lexer is not null, the parser just fetched Token from Lexer;
   --  add it to the tail of the State input queue, with augmenting
   --  data from Lexer, for later operations (Append is ignored).
   --
   --  If Lexer is null, Token was inserted in an error recover
   --  operation; add to queue head with default augmenting data.

   procedure Push_Token
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Parser just pushed Token on the parse stack; remove the
   --  corresponding augmented token from the tail of the State input
   --  queue, push it on the State stack.

   procedure Error
     (State     : access Semantic_State;
      Expecting : in     Token_ID_Set)
   is abstract;
   --  The parser has detected an error with the current token (must
   --  be the tail of the State input queue). Expecting is the set of
   --  tokens expected by the parser. Save information useful for an
   --  error message.

   procedure Discard_Token
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Token was discarded in an error recover opertation; discard the
   --  corresponding augmented token from the head of the State input
   --  queue, and record the buffer region as invalid.

   procedure Merge_Tokens
     (State   : access Semantic_State;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     List.Instance;
      Action  : in     Semantic_Action)
   is abstract;
   --  Parser reduced Tokens to Nonterm; perform same operations on
   --  State stack, call associated Action.

   procedure Recover
     (State         : access Semantic_State;
      Popped_Tokens : in     List.Instance;
      Pushed_Tokens : in     List.Instance)
     is abstract;
   --  An error recover algorithm succeeded; adjust the State augmented
   --  token stack, input queue, and invalid region to match.
   --
   --  Skipped tokens were reported via Discard_Token.
   --
   --  Popped_Tokens were popped off the stack, Pushed_Tokens were
   --  pushed on the stack; add those to the invalid region.

end WisiToken.Token;
