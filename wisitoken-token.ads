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
   --  For storing augmented tokens and other semantic information.
   --  This allows separating the parser notion of "token" (which is
   --  just an ID) from the semantic notion of "token" (which can
   --  contain other information).
   --
   --  For example, in an Ada_Emacs parser, the semantic state is held
   --  in a separate process; redundant token IDs are used to validate
   --  (and debug) the inter-process communications.
   --
   --  During normal parser operation, whether single or parallel, all
   --  parsers have the same current token. However, during parallel
   --  parsing, all semantic operations are pended, so tokens read
   --  from the lexer during parallel parsing are put on
   --  Semantic_State.Lookahead_Queue.
   --
   --  Similarly, during error recovery, tokens may be read from Lexer
   --  for lookahead (error recovery is speculative parallel parsing).
   --  Note that error recovery can occur when parallel parsers are
   --  operating.
   --
   --  In order to simplify transitions between normal parsing,
   --  parallel parsing, and error recovery, we define the front of
   --  Semantic_State.Lookahead_Queue as the current token.
   --
   --  Error recovery may also insert virtual tokens, not read from
   --  the Lexer. Those are managed internal to the parser and/or
   --  error recovery algorithm; when the parser retrieves them as the
   --  current token, it calls Virtual_To_Current.

   type Recover_Data is abstract tagged null record;
   --  For storing error recovery information, for reuse in subsequent
   --  parse. Compare to WisiToken.Parser.LR.Recover_Data.

   function Image (Data : in Recover_Data; Descriptor : in WisiToken.Descriptor'Class) return String is abstract;
   --  Human readable (for extended error messages), with token string images.

   type Recover_Data_Access is access all Recover_Data'Class;
   --  For storing Recover_Data in a Parser_Lists.Pend_Item.

   procedure Free is new Ada.Unchecked_Deallocation (Recover_Data'Class, Recover_Data_Access);

   procedure Put (State : access Semantic_State) is abstract;
   --  Put a trace of State to State.Trace. Detail depends on
   --  WisiToken.Trace_Parse.

   procedure Reset (State : access Semantic_State) is abstract;
   --  Reset State to start a new parse.

   procedure Lexer_To_Lookahead
     (State : access          Semantic_State;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
     is abstract;
   --  The parser just fetched ID from Lexer during parallel parsing
   --  or error recovery lookahead, stored it in the shared parser
   --  lookahead. Add augmented data from Lexer, and add it to the
   --  back of the State lookahead queue.

   procedure Virtual_To_Lookahead
     (State : access Semantic_State;
      ID    : in     Token_ID)
     is abstract;
   --  The parser just retrieved ID from an error recover solution
   --  during parallel parsing; it is now the current token. Add
   --  default augmented data, and add to front of the lookahead
   --  queue.

   procedure Push_Current
     (State : access Semantic_State;
      ID    : in     Token_ID)
     is abstract;
   --  Parser just pushed the current token (ID) on the parse stack;
   --  remove the corresponding token from the front of the State
   --  lookahead queue, push it on the State stack.

   procedure Begin_Parallel_Parse (State : access Semantic_State) is abstract;
   --  The parser has started parallel parsers; semantic state operations
   --  will be kept on a pending queue for each parser until all but one
   --  error out. Then they will be executed. The only exception is
   --  Lexer_To_Lookahead; that is called immediately, so the lexer
   --  information is valid.
   --
   --  If Error is called during parallel parse, the State lookahead
   --  queue is out of sync with the parser's notion of current token.
   --  Therefore State must record the lookahead count when
   --  Start_Parallel_Parse is called, and count the calls to Lexer_To_Lookahead, and use that in Error to get the
   --  right token.

   procedure End_Parallel_Parse (State : access Semantic_State) is abstract;
   --  Parser has finished parallel parsing (see Start_Parallel_Parse).

   procedure Error
     (State     : access Semantic_State;
      Expecting : in     Token_ID_Set)
   is abstract;
   --  The parser has detected an error with the current token, which is
   --  on the State lookahead queue at the location described in the
   --  description of Start_Parallel_Parse, or at the front of the queue
   --  if parallel parsing is not active.
   --
   --  Expecting is the set of tokens expected by the parser.
   --
   --  Save information useful for an error message.
   --
   --  Error recover has started; mark the start of an invalid buffer
   --  region.

   procedure Discard_Lookahead
     (State : access Semantic_State;
      ID    : in     Token_ID)
     is abstract;
   --  ID was discarded from lookahead in an error recover
   --  operation; discard the corresponding augmented token from the
   --  front of the State lookahead queue, and add the token's buffer
   --  region to the current invalid region.

   procedure Discard_Stack
     (State : access Semantic_State;
      ID    : in     Token_ID)
     is abstract;
   --  ID was discarded from the top of the parser parse stack in an
   --  error recover operation; discard the corresponding augmented
   --  token from the top of the State stack, and add the token's
   --  buffer region to the current invalid region.

   procedure Reduce_Stack
     (State   : access Semantic_State;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      IDs     : in     List.Instance;
      Action  : in     Semantic_Action)
   is abstract;
   --  Parser reduced IDs to Nonterm; perform same operations on State
   --  stack, call Action. Index identifies the production for Nonterm
   --  used.

   procedure Recover
     (State   : access Semantic_State;
      Recover : in     Recover_Data'Class)
     is abstract;
   --  An error recover algorithm finished; save the current invalid
   --  region and recover information.

end WisiToken.Token;
