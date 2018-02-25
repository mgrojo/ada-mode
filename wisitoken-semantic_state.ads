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

with WisiToken.Lexer;
package WisiToken.Semantic_State is

   Invalid_All_Tokens_Index : constant Positive_Index_Type := Positive_Index_Type'Last;

   type Augmented_Token is new Base_Token with record
      Virtual     : Boolean           := False;               -- FIXME: now in Syntax_Trees
      Line        : Line_Number_Type  := Invalid_Line_Number; -- at start of token
      Col         : Ada.Text_IO.Count := 0;
      Char_Region : Buffer_Region     := Null_Buffer_Region;

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

   package Int_Vectors is new Ada.Containers.Vectors (Line_Number_Type, Integer);

   type Semantic_State (Trace : not null access WisiToken.Trace'Class) is tagged limited record
      Stack : Augmented_Token_Arrays.Vector;
      --  Top of stack is Stack.Last_Index; Push = Append, Pop = Delete_Last.
      --  Tokens are added by Push_Current, removed by Reduce_Stack.

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
   --  Operations on Semantic_State

   --  Tokens read from the lexer during parsing are put in
   --  Semantic_State.All_Tokens, by calling Lexer_To_Augmented, which
   --  retrieves additional information from the lexer.
   --
   --  Parsing produces a syntax tree, which stores the action subprogram
   --  pointers from the grammar.
   --
   --  Error recovery may insert virtual tokens, not read from
   --  the Lexer. FIXME: how are these handled now?

   procedure Initialize
     (State      : not null access Semantic_State;
      Line_Count : in              Line_Number_Type);
   --  Reset State to start a new parse; should call Reset.

   procedure Reset (State : not null access Semantic_State);
   --  Reset State to start a new parse, with previous initialization
   --  data.

   procedure Put (State : not null access Semantic_State);
   --  Put a trace of State to State.Trace. Detail depends on
   --  WisiToken.Trace_Parse.

   procedure Lexer_To_Augmented
     (State : not null access Semantic_State;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class);
   --  The parser just fetched Token from Lexer; add additional
   --  information, store in State.All_Tokens.

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
