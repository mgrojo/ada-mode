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

with Ada.Unchecked_Deallocation;
with WisiToken.Lexer;
package WisiToken.Semantic_State is

   type Non_Grammar_Token is record
      ID    : Token_ID;
      Line  : Line_Number_Type;
      Col   : Ada.Text_IO.Count;
      First : Boolean;
      --  Column is needed to detect comments in column 0.
   end record;

   package Non_Grammar_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Non_Grammar_Token);

   type Augmented_Token is new Base_Token with record
      --  Most fields are set by Lexer_To_Augmented at parse time; others
      --  are set by Reduce for nonterminals.

      Line        : Line_Number_Type  := Invalid_Line_Number; -- At start of token. Not set for nonterm.
      Col         : Ada.Text_IO.Count := 0;
      Char_Region : Buffer_Region     := Null_Buffer_Region;

      --  The following fields are only needed for indent.

      First : Boolean := False;
      --  For a terminal, True if the token is not empty and it is first on
      --  a line, or if it contains trailing blank or comment lines.
      --
      --  For a nonterminal, True if some contained token's First is True,
      --  including trailing comments and blank lines.

      Paren_State : Integer := 0;
      --  Parenthesis nesting count, before token.

      First_Terminals_Index : Base_Token_Index := Base_Token_Arrays.No_Index;
      --  For virtual tokens, No_Index.
      --
      --  For terminal tokens, index of this token in State.Terminals.
      --
      --  For nonterminal tokens, index of first contained token in
      --  State.Terminals.

      Last_Terminals_Index : Base_Token_Index := Base_Token_Arrays.No_Index;
      --  For non-virtual nonterminal tokens, index of last contained
      --  token in State.Terminals.
      --
      --  For all others, same as First_Terminals_Index.

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

      Non_Grammar : Non_Grammar_Token_Arrays.Vector;
      --  For terminals, non-grammar tokens immediately following. For
      --  nonterminals, empty.

   end record;

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   --  Return a string for debug/test messages

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   --  Return first and last line in Token's region.

   type Augmented_Token_Access is access all Augmented_Token;
   procedure Free is new Ada.Unchecked_Deallocation (Augmented_Token, Augmented_Token_Access);

   type Augmented_Token_Access_Array is array (Positive_Index_Type range <>) of Augmented_Token_Access;
   --  1 indexed to match token numbers in grammar actions.

   function Image
     (Item       : in Augmented_Token_Access_Array;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

   package Augmented_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Augmented_Token);
   --  Index matches Base_Token_Arrays.

   ----------
   --  Semantic_State

   package Line_Paren_Vectors is new SAL.Gen_Unbounded_Definite_Vectors (Line_Number_Type, Integer);
   package Line_Begin_Pos_Vectors is new SAL.Gen_Unbounded_Definite_Vectors (Line_Number_Type, Buffer_Pos);
   package Line_Begin_Token_Vectors is new SAL.Gen_Unbounded_Definite_Vectors (Line_Number_Type, Base_Token_Index);

   type Semantic_State is tagged limited record
      Terminals : Augmented_Token_Arrays.Vector;
      --  All terminal grammar tokens, in lexical order. Each contains any
      --  immediately following non-grammar tokens. Does not contain
      --  nonterminal or virtual tokens. Therefore Semantic_Tree.Terminal
      --  and Semantic_State.Terminals indices are the same for
      --  corresponding tokens. .

      Leading_Non_Grammar : Base_Token_Arrays.Vector;
      --  non-grammar tokens before first grammar token.

      Line_Begin_Pos : Line_Begin_Pos_Vectors.Vector;
      --  Character position at the start of the first token on each line.

      Line_Begin_Token : Line_Begin_Token_Vectors.Vector;
      --  Index into Terminals of first token on line; may be a Non_Grammar
      --  contained by the token.

      Line_Paren_State : Line_Paren_Vectors.Vector;
      --  Parenthesis nesting state at the start of each line; used by
      --  Indent. Set by Lexer_To_Augmented on New_Line_ID.

      Current_Paren_State : Integer;
      --  Current parenthesis nesting state; used by Indent. Set by
      --  Lexer_To_Augmented on Left_Paren_ID, Right_Paren_ID.
   end record;
   type Semantic_State_Access is access all Semantic_State'Class;

   function Find
     (State : in Semantic_State;
      ID    : in Token_ID;
      Token : in Augmented_Token'Class)
     return Base_Token_Index;
   --  Return index to State.Terminals of first token in
   --  Token.Char_Region with ID. If not found, return
   --  No_Index.

   ----------
   --  Operations on Semantic_State

   --  Tokens read from the lexer during parsing are put in
   --  Semantic_State.Terminals, by calling Lexer_To_Augmented, which
   --  retrieves additional information from the lexer.
   --
   --  Parsing produces a syntax tree, which stores the action subprogram
   --  pointers from the grammar.
   --
   --  Error recovery may insert virtual tokens, not read from the Lexer.
   --  They are stored in the syntax tree, and passed to Reduce in
   --  Children.

   procedure Initialize
     (State      : in out Semantic_State;
      Line_Count : in     Line_Number_Type);
   --  Reset State to start a new parse; should call Reset.

   procedure Reset (State : in out Semantic_State);
   --  Reset State to start a new parse, with previous initialization
   --  data.

   procedure Lexer_To_Augmented
     (State      : in out          Semantic_State;
      Descriptor : in              WisiToken.Descriptor'Class;
      Token      : in              Base_Token;
      Lexer      : not null access WisiToken.Lexer.Instance'Class);
   --  The parser just fetched Token from Lexer; add additional
   --  information, store in State.Terminals.

   procedure Reduce
     (Nonterm        : in out Augmented_Token'Class;
      Tokens         : in     Augmented_Token_Access_Array;
      Descriptor     : in     WisiToken.Descriptor'Class;
      Compute_Indent : in     Boolean);
   --  Reduce Tokens to Nonterm.ID, update Tokens, Nonterm. Nonterm.Byte_Region
   --  is computed by caller. If Compute_Indent, compute values only
   --  needed by indent.

end WisiToken.Semantic_State;
