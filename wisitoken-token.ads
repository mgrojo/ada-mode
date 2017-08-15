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

with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
with WisiToken.Lexer;
package WisiToken.Token is

   ----------
   --  Token lists

   package List is
      --  FIXME: replace with Token_Array?

      type Instance is tagged private
      with
        Constant_Indexing => Constant_Reference,
        Default_Iterator  => Iterate,
        Iterator_Element  => Token_ID;

      type Constant_Reference_Type (Element : not null access constant Token_ID) is null record
      with Implicit_Dereference => Element;

      type List_Node_Ptr is private;

      function Constant_Reference
        (Container : aliased in Instance'Class;
         Position  :         in List_Node_Ptr)
        return Constant_Reference_Type;

      function Has_Element (Cursor : in List_Node_Ptr) return Boolean;
      package Iterator_Interfaces is new Ada.Iterator_Interfaces (List_Node_Ptr, Has_Element);
      function Iterate (Container : aliased Instance) return Iterator_Interfaces.Forward_Iterator'Class;

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

      procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Instance);
      --  Put Item to Trace.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         ID   : aliased Token_ID;
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

   type Recover_Data is abstract tagged null record;
   --  For storing error recovery information, for reuse in subsequent
   --  parse. Compare to WisiToken.Parser.LR.Recover_Data.

   type Recover_Data_Access is access all Recover_Data'Class;
   --  For storing Recover_Data in a Parser_Lists.Pend_Item.

   procedure Free is new Ada.Unchecked_Deallocation (Recover_Data'Class, Recover_Data_Access);

   procedure Put (State : access Semantic_State) is abstract;
   --  Put a trace of State to State.Trace. Detail depends on
   --  WisiToken.Trace_Parse.

   procedure Reset (State : access Semantic_State) is abstract;
   --  Reset State to start a new parse.

   procedure Input_Token
     (State : access Semantic_State;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
     is abstract;
   --  If Lexer is not null, the parser just fetched Token from Lexer;
   --  add it to the back of the State input queue, with augmenting
   --  data from Lexer, for later operations.
   --
   --  If Lexer is null, Token was inserted in an error recover
   --  operation; add to queue front with default augmenting data.

   procedure Input_Lookahead
     (State : access Semantic_State;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
     is abstract;
   --  Lexer cannot be null. The parser just fetched Token from Lexer
   --  during an error recovery lookahead. Add it to the back of the
   --  State lookahead queue, with augmenting data from Lexer, for
   --  later operations.

   procedure Move_Lookahead_To_Input
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Parser just removed Token from lookahead; remove the corresponding
   --  augmented token from the front of the State lookahead queue,
   --  add it to the front of the State input queue.

   procedure Move_Input_To_Lookahead
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Parser is entering error recovery; move Token from the front of
   --  the State input queue to the front of the State lookahead
   --  queue.

   procedure Push_Token
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Parser just pushed Token on the parse stack; remove the
   --  corresponding augmented token from the front of the State input
   --  queue, push it on the State stack.

   procedure Error
     (State     : access Semantic_State;
      Expecting : in     Token_ID_Set)
   is abstract;
   --  The parser has detected an error with the current token (the
   --  front of the State input queue). Expecting is the set of tokens
   --  expected by the parser. Save information useful for an error
   --  message.
   --
   --  Error recover has started; mark the start of an invalid buffer
   --  region.

   procedure Discard_Input
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Token was discarded from input in an error recover operation;
   --  discard the corresponding augmented token from the front of the
   --  State input queue, and add the token's buffer region to the
   --  current invalid region.

   procedure Discard_Lookahead
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Token was discarded from lookahead in an error recover
   --  operation; discard the corresponding augmented token from the
   --  front of the State lookahead queue, and add the token's buffer
   --  region to the current invalid region.

   procedure Pop_Token
     (State : access Semantic_State;
      Token : in     Token_ID)
     is abstract;
   --  Token was popped from the parse stack in an error recover
   --  operation; discard the corresponding augmented token from the
   --  top of the State stack, and add the token's buffer region to
   --  the current invalid region.

   procedure Merge_Tokens
     (State   : access Semantic_State;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     List.Instance;
      Action  : in     Semantic_Action)
   is abstract;
   --  Parser reduced Tokens to Nonterm; perform same operations on
   --  State stack, call associated Action. Index identifies the
   --  production used in the current parser state (on the top of the
   --  parse stack).

   procedure Recover
     (State   : access Semantic_State;
      Recover : in     Recover_Data'Class)
     is abstract;
   --  An error recover algorithm finished; save the current invalid
   --  region.

end WisiToken.Token;
