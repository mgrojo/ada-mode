--  Abstract :
--
--  Abstract interface to a semantic state.
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Unbounded_Indefinite_Queues;
with WisiToken.Lexer;
package WisiToken.Semantic_State is

   type Augmented_Token is abstract new Base_Token with record
      Virtual : Boolean;
      --  Derived types add various lexical information.
   end record;

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean)
     return String is abstract;
   --  Return a string for debug/test messages

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token'Class);
   --  Put Image to Trace.

   package Augmented_Token_Arrays is new Ada.Containers.Indefinite_Vectors (Positive_Index_Type, Augmented_Token'Class);

   subtype Augmented_Token_Array is Augmented_Token_Arrays.Vector;

   package Augmented_Token_Queues is new SAL.Gen_Unbounded_Indefinite_Queues (Augmented_Token'Class);

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token_Queues.Queue_Type);

   type Semantic_Action is access procedure
     (Nonterm : in Augmented_Token'Class;
      Index   : in Natural;
      Tokens  : in Augmented_Token_Array);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production for
   --  Nonterm (0 origin); Tokens is the right hand side tokens.
   --
   --  Nonterm is classwide to avoid freezing rules.

   Null_Action : constant Semantic_Action := null;

   type Semantic_State (Trace : not null access WisiToken.Trace'Class) is abstract tagged limited null record;
   type Semantic_State_Access is access all Semantic_State'Class;
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
   --  parsing, most semantic operations are pended, so the semantic
   --  state notion of current token is not in sync with the parser
   --  notion. Tokens read from the lexer during parallel parsing are put
   --  on Semantic_State.Lookahead_Queue; the front of that queue is the
   --  semantic state current token, and the back of the queue is the
   --  lexer current token.
   --
   --  During error recovery, parallel parsers are not in sync as to
   --  the current token, and tokens may be read from Lexer for lookahead
   --  (error recovery is speculative parallel parsing). However, the
   --  error recovery algorithms ensure that a second error will not be
   --  encountered until the parallel parsers are back in sync (if they
   --  cannot ensure that, error recovery fails, the parse fails, and the
   --  error is reported immediately).
   --
   --  Error recovery may also insert virtual tokens, not read from
   --  the Lexer. Those are managed internal to the parser and
   --  error recovery algorithm; when the parser retrieves them as the
   --  current token, it calls Virtual_To_Lookahead.

   type Init_Data is tagged null record;

   Null_Init_Data : Init_Data;

   procedure Initialize (State : not null access Semantic_State; Init : in Init_Data'Class) is abstract;
   --  Reset State to start a new parse, using data from Init; should normally call Reset.

   procedure Reset (State : not null access Semantic_State; Init_Done : in Boolean := False) is abstract;
   --  Reset State to start a new parse, with previous Init data.
   --
   --  If Init_Done is True, assume Initialize has just been called.

   type Recover_Data is abstract tagged null record;
   --  For storing error recovery information, for reuse in subsequent
   --  parse, or to inform the IDE about repairing errors.

   function Image (Data : in Recover_Data; Descriptor : in WisiToken.Descriptor'Class) return String is abstract;
   --  Human readable (for extended error messages), with token string images.

   type Recover_Data_Access is access all Recover_Data'Class;
   --  For storing Recover_Data in a Parser_Lists.Pend_Item.

   procedure Free is new Ada.Unchecked_Deallocation (Recover_Data'Class, Recover_Data_Access);

   procedure Put (State : not null access Semantic_State) is abstract;
   --  Put a trace of State to State.Trace. Detail depends on
   --  WisiToken.Trace_Parse.

   ----------
   --  Operations that are not pended

   procedure Lexer_To_Lookahead
     (State : not null access Semantic_State;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
     is abstract;
   --  The parser just fetched ID from Lexer during parallel parsing
   --  or error recovery lookahead, stored it in the shared parser
   --  lookahead. Add augmented data from Lexer, and add it to the
   --  back of the State lookahead queue.

   procedure Error
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set)
   is abstract;
   --  A parser Parser_ID has detected an error with the current token,
   --  which is at the back of the State lookahead queue (the token most
   --  recently fetched from the lexer)..
   --
   --  Expecting is the set of tokens expected by the parser.
   --
   --  Save information useful for an error message.

   procedure Spawn
     (State         : not null access Semantic_State;
      Old_Parser_ID : in              Natural;
      New_Parser_ID : in              Natural)
   is abstract;
   --  A new parser was spawned; copy saved error information.

   procedure Terminate_Parser
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural)
   is abstract;
   --  A parser Parser_ID has been terminated; discard any saved error
   --  information.

   ----------
   --  Operations that are pended during parallel parsing

   procedure Virtual_To_Lookahead
     (State : not null access Semantic_State;
      ID    : in              Token_ID)
     is abstract;
   --  The parser just retrieved ID from an error recover solution
   --  during parallel parsing; it is now the current token. Add
   --  default augmented data, and add to front of the lookahead
   --  queue.

   procedure Push_Current
     (State : not null access Semantic_State;
      ID    : in              Token_ID)
     is abstract;
   --  Parser just pushed the current token (ID) on the parse stack;
   --  remove the corresponding token from the front of the State
   --  lookahead queue, push it on the State stack.

   procedure Discard_Lookahead
     (State : not null access Semantic_State;
      ID    : in              Token_ID)
     is abstract;
   --  ID was discarded from lookahead in an error recover operation;
   --  discard the corresponding augmented token from the front of the
   --  State lookahead queue.

   procedure Discard_Stack
     (State : not null access Semantic_State;
      ID    : in              Token_ID)
     is abstract;
   --  ID was discarded from the top of the parser parse stack in an
   --  error recover operation; discard the corresponding augmented token
   --  from the top of the State stack.

   procedure Recover
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural;
      Recover   : in              Recover_Data'Class)
     is abstract;
   --  Parser Parser_ID has finished error recovery and found a solution
   --  described by Recover; save the recover information.

   procedure Reduce_Stack
     (State       : not null access Semantic_State;
      Nonterm     : in              Token_ID;
      Index       : in              Natural;
      Base_Tokens : in              Token_ID_Arrays.Vector; -- FIXME: replace with base_Token_Arrays
      Action      : in              Semantic_Action)
   is abstract;
   --  Parser reduced Base_Tokens to Nonterm; perform same operations on
   --  State stack, call Action. Index identifies the production for
   --  Nonterm used.

end WisiToken.Semantic_State;
