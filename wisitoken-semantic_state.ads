--  Abstract :
--
--  Abstract interface to a semantic state.
--
--  Copyright (C) 2017, 2018 Stephe Leake
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Lexer;
package WisiToken.Semantic_State is

   Invalid_All_Tokens_Index : constant Positive_Index_Type := Positive_Index_Type'Last;

   type Augmented_Token is new Base_Token with record
      Line        : Line_Number_Type  := Invalid_Line_Number;
      Col         : Ada.Text_IO.Count := 0;
      Char_Region : Buffer_Region     := Null_Buffer_Region;

      Virtual : Boolean := False;
      --  For non-grammar and terminal tokens, True if inserted by
      --  Virtual_To_Lookahead. For nonterminal tokens, True if any
      --  contained token has Virtual True.
      --
      --  Useful in semantic actions; don't report errors if can't perform
      --  semantic actions on virtual tokens.

      First_All_Tokens_Index : Positive_Index_Type := Invalid_All_Tokens_Index;
      --  For non-grammar and terminal tokens, index of this token in
      --  State.All_Tokens.
      --
      --  For nonterminal tokens, index of first contained token in
      --  State.All_Tokens.
      --
      --  For virtual tokens, Invalid_All_Tokens_Index.

      Last_All_Tokens_Index : Positive_Index_Type := Invalid_All_Tokens_Index;
      --  For non-grammar tokens, Invalid_All_Tokens_Index.
      --
      --  For terminal tokens, index of last following non-grammar token in
      --  State.All_Tokens (normally a new-line), or
      --  Invalid_All_Tokens_Index if following token is a grammar token.
      --
      --  For nonterminal tokens, index of last contained token in
      --  State.All_Tokens (normally a new-line).
      --
      --  For virtual tokens, Invalid_All_Tokens_Index.

      First : Boolean := False;
      --  For a terminal, True if the token is not empty and it is first on
      --  a line, or if it contains trailing blank or comment lines.
      --
      --  For a nonterminal, True if some contained token's First is True.

      First_Indent_Line : Line_Number_Type := Invalid_Line_Number;
      Last_Indent_Line  : Line_Number_Type := Invalid_Line_Number;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token. If First is False, these are Invalid_Line_Number.
      --
      --  First_, Last_Indent_Line include blank and comment lines between
      --  grammar tokens, but exclude trailing blanks and comments after the
      --  last token, so they can be indented differently.

      First_Trailing_Comment_Line : Line_Number_Type := Invalid_Line_Number;
      Last_Trailing_Comment_Line  : Line_Number_Type := Invalid_Line_Number;
      --  Trailing comment or blank lines (after the last contained grammar
      --  token) that need indenting. Excludes comments following code on a
      --  line. If there are no such lines, these are Invalid_Line_Number.

      Paren_State : Integer := 0;
      --  Parenthesis nesting count, before token.
   end record;

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean := False)
     return String;
   --  Return a string for debug/test messages

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token'Class);
   --  Put Image to Trace.

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   --  Return first and last line in Token's region.

   package Augmented_Token_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Augmented_Token);

   package Augmented_Token_Queues is new SAL.Gen_Unbounded_Definite_Queues (Augmented_Token);

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token_Queues.Queue_Type);

   type Semantic_Action is access procedure
     (Nonterm : in Augmented_Token;
      Tokens  : in Augmented_Token_Arrays.Vector);
   --  Routines of this type are called by the parser when it reduces
   --  a production to Nonterm. Index indicates which production for
   --  Nonterm (0 origin); Tokens is the right hand side tokens.
   --
   --  Nonterm is classwide to avoid freezing rules.

   Null_Action : constant Semantic_Action := null;

   ----------
   --  Semantic_State

   type Recover_Data is abstract tagged null record;
   --  For storing error recovery information, for reuse in subsequent
   --  parse, or to inform the IDE about repairing errors.

   function Image (Data : in Recover_Data; Descriptor : in WisiToken.Descriptor'Class) return String is abstract;
   --  Human readable (for extended error messages), with token string images.

   type Recover_Data_Access is access all Recover_Data'Class;
   --  For storing Recover_Data in a Parser_Lists.Pend_Item.

   procedure Free is new Ada.Unchecked_Deallocation (Recover_Data'Class, Recover_Data_Access);

   type Parser_Error_Data
     (First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is record
      Error_Token : Augmented_Token;
      Expecting   : Token_ID_Set (First_Terminal .. Last_Terminal);
      Recover     : access Recover_Data'Class;
   end record;

   package Parser_Error_Data_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Parser_Error_Data);

   package Parser_Error_List_Arrays is new Ada.Containers.Vectors
     (Natural, Parser_Error_Data_Lists.List, Parser_Error_Data_Lists."=");
   --  IMPROVEME: parser ids are never reused. So this ends up with many
   --  empty slots, because first_index = 0. Use a sparse vector (= map?).

   procedure Put
     (Source_File_Name : in String;
      Errors           : in Parser_Error_List_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor'Class);
   --  Put user-friendly error messages to Ada.Text_IO.Current_Output.

   package Int_Vectors is new Ada.Containers.Vectors (Line_Number_Type, Integer);

   type Semantic_State (Trace : not null access WisiToken.Trace'Class) is tagged limited record
      Stack : Augmented_Token_Arrays.Vector;
      --  Top of stack is Stack.Last_Index; Push = Append, Pop = Delete_Last.
      --  Tokens are added by Push_Current, removed by Reduce_Stack.

      Lookahead_Queue : Augmented_Token_Queues.Queue_Type;

      Parser_Errors : Parser_Error_List_Arrays.Vector;
      --  Indexed by Parser_ID.

      Lexer_Errors : aliased Lexer.Error_Lists.List;
      --  Shared by all parsers. No access functions provided below; set
      --  directly by Lexer.Find_Next.

      All_Tokens : Augmented_Token_Arrays.Vector;
      --  All terminal grammar and non-grammar tokens, in lexical order. Does not
      --  contain nonterminal or virtual tokens.

      Line_Paren_State : Int_Vectors.Vector;
      --  Parenthesis nesting state at the start of each line; used by
      --  Indent. Set by Lexer_To_Lookahead on New_Line_ID.

      Current_Paren_State : Integer;
      --  Current parenthesis nesting state; used by Indent. Set by
      --  Lexer_To_Lookahead on Left, Right_Paren_ID.
   end record;
   type Semantic_State_Access is access all Semantic_State'Class;

   function Find
     (State : in Semantic_State;
      ID    : in Token_ID;
      Token : in Augmented_Token'Class)
     return SAL.Base_Peek_Type;
   --  Return index to State.All_Tokens of first token in
   --  Token.Char_Region with ID. If not found, return
   --  Invalid_All_Tokens_Index.

   function Find_Line_Begin
     (State : in Semantic_State;
      Line  : in Line_Number_Type;
      Start : in Augmented_Token'Class)
     return Positive_Index_Type;
   --  Return index to State.All_Tokens of first grammar, comment, or
   --  new-line token on Line. Start is used to find a starting point to
   --  search State.All_Tokens; Line must be <=
   --  Start.Last_All_Tokens_Index line.
   --
   --  If the result is a new-line token, the line is empty.

   ----------
   --  Parser operations on Semantic_State

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

   procedure Initialize
     (State      : not null access Semantic_State;
      Line_Count : in              Line_Number_Type);
   --  Reset State to start a new parse; should call Reset.

   procedure Reset (State : not null access Semantic_State);
   --  Reset State to start a new parse, with previous initialization
   --  data.

   function Active_Error_List
     (State : not null access Semantic_State)
     return Parser_Error_List_Arrays.Constant_Reference_Type;
   --  Return a reference to the single active error list.
   --  If more than one is active, raise Programmer_Error.

   procedure Put (State : not null access Semantic_State);
   --  Put a trace of State to State.Trace. Detail depends on
   --  WisiToken.Trace_Parse.

   ----------
   --  Operations that are not pended

   procedure Lexer_To_Lookahead
     (State : not null access Semantic_State;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class);
   --  The parser just fetched Token from Lexer during parallel parsing
   --  or error recovery lookahead, stored it in the shared parser
   --  lookahead. Add augmented data from Lexer, and add it to the
   --  back of the State lookahead queue.

   procedure Error
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set);
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
      New_Parser_ID : in              Natural);
   --  A new parser was spawned; copy saved error information.

   procedure Terminate_Parser
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural);
   --  A parser Parser_ID has been terminated; discard any saved error
   --  information.

   ----------
   --  Operations that are pended during parallel parsing

   procedure Virtual_To_Lookahead
     (State : not null access Semantic_State;
      Token : in              Base_Token);
   --  The parser just retrieved Token from an error recover solution
   --  during parallel parsing; it is now the current token. Add default
   --  augmented data, and add to the front of the lookahead queue.

   procedure Push_Current
     (State : not null access Semantic_State;
      Token : in              Base_Token);
   --  Parser just pushed Token on the parse stack; remove the
   --  corresponding augmented token from the front of the State
   --  lookahead queue, push it on the State stack.

   procedure Discard_Lookahead
     (State : not null access Semantic_State;
      ID    : in              Token_ID);
   --  ID was discarded from lookahead in an error recover operation;
   --  discard the corresponding augmented token from the front of the
   --  State lookahead queue.

   procedure Discard_Stack
     (State : not null access Semantic_State;
      ID    : in              Token_ID);
   --  ID was discarded from the top of the parser parse stack in an
   --  error recover operation; discard the corresponding augmented token
   --  from the top of the State stack.

   procedure Recover
     (State     : not null access Semantic_State;
      Parser_ID : in              Natural;
      Recover   : in              Recover_Data'Class);
   --  Parser Parser_ID has finished error recovery and found a solution
   --  described by Recover; save the recover information.

   procedure Reduce_Stack
     (State       : not null access Semantic_State;
      Nonterm     : in              Base_Token;
      Index       : in              Natural;
      Base_Tokens : in              Base_Token_Arrays.Vector;
      Action      : in              Semantic_Action);
   --  Parser reduced Base_Tokens to Nonterm; perform same operations on
   --  State stack, call Action. Index identifies the production for
   --  Nonterm used.

end WisiToken.Semantic_State;
