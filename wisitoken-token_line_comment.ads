--  Abstract :
--
--  Store token information useful for indentation.
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

with WisiToken.Lexer;
with WisiToken.Semantic_State;
with WisiToken.Token_Region;
package WisiToken.Token_Line_Comment is

   Invalid_All_Tokens_Index : constant Positive_Index_Type := Positive_Index_Type'Last;

   type Token is new WisiToken.Token_Region.Token with record
      First_All_Tokens_Index : Positive_Index_Type;
      --  For non-grammar and terminal tokens, index of this token in
      --  State.All_Tokens.
      --
      --  For nonterminal tokens, index of first contained token in
      --  State.All_Tokens.
      --
      --  For virtual tokens, Invalid_All_Tokens_Index.

      Last_All_Tokens_Index : Positive_Index_Type;
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

      First : Boolean;
      --  For a terminal, True if the token is not empty and it is first on
      --  a line, or if it contains trailing blank or comment lines.
      --
      --  For a nonterminal, True if some contained token's First is True.

      First_Indent_Line : Line_Number_Type;
      Last_Indent_Line  : Line_Number_Type;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token. If First is False, these are Invalid_Line_Number.
      --
      --  First_, Last_Indent_Line include blank and comment lines between
      --  grammar tokens, but exclude trailing blanks and comments after the
      --  last token, so they can be indented differently.

      First_Trailing_Comment_Line : Line_Number_Type;
      Last_Trailing_Comment_Line  : Line_Number_Type;
      --  Trailing comment or blank lines (after the last contained grammar
      --  token) that need indenting. Excludes comments following code on a
      --  line. If there are no such lines, these are Invalid_Line_Number.

      Paren_State : Integer;
      --  Parenthesis nesting count, before token.
   end record;

   function First_Line
     (Token             : in Token_Line_Comment.Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   function Last_Line
     (Token             : in Token_Line_Comment.Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type;
   --  Return first and last line in Token's region.

   package Token_Vectors is new Ada.Containers.Vectors (Positive_Index_Type, Token);

   package Int_Vectors is new Ada.Containers.Vectors (Line_Number_Type, Integer);

   type State_Type is new WisiToken.Token_Region.State_Type with record
      All_Tokens : Token_Vectors.Vector;
      --  All terminal grammar and non-grammar tokens, in lexical order. Does not
      --  contain nonterminal or virtual tokens.

      Line_Paren_State : Int_Vectors.Vector;
      --  Parenthesis nesting state at the start of each line; used by
      --  Indent. Set by Lexer_To_Lookahead on New_Line_ID.

      Current_Paren_State : Integer;
      --  Current parenthesis nesting state; used by Indent. Set by
      --  Lexer_To_Lookahead on Left, Right_Paren_ID.
   end record;
   type State_Access is access all State_Type;

   function Find
     (State : in State_Type;
      ID    : in Token_ID;
      Token : in Token_Line_Comment.Token'Class)
     return Ada.Containers.Count_Type;
   --  Return index to State.All_Tokens of first token in
   --  Token.Char_Region with ID. If not found, return
   --  Invalid_All_Tokens_Index.

   function Find_Line_Begin
     (State : in State_Type;
      Line  : in Line_Number_Type;
      Start : in Token'Class)
     return Positive_Index_Type;
   --  Return index to State.All_Tokens of first grammar, comment, or
   --  new-line token on Line. Start is used to find a starting point to
   --  search State.All_Tokens; Line must be <=
   --  Start.Last_All_Tokens_Index line.
   --
   --  If the result is a new-line token, the line is empty.

   type Init_Data is new WisiToken.Semantic_State.Init_Data with record
      Line_Count : Line_Number_Type;
   end record;

   overriding
   procedure Initialize (State : not null access State_Type; Init : in WisiToken.Semantic_State.Init_Data'Class);

   overriding
   procedure Reset (State : not null access State_Type; Init_Done : in Boolean := False);

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      Token : in              Base_Token);

   overriding
   procedure Reduce_Stack
     (State       : not null access State_Type;
      Nonterm     : in              Base_Token;
      Index       : in              Natural;
      Base_Tokens : in              WisiToken.Base_Token_Arrays.Vector;
      Action      : in              WisiToken.Semantic_State.Semantic_Action);

end WisiToken.Token_Line_Comment;
