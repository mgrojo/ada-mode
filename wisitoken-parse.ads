--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   type Tree_Ref is record
      --  Used in error recover for stream_prev/_next, so need stream_index.
      --  Used in post-parse actions, so need Node_Access.
      Node  : Syntax_Trees.Node_Access  := Syntax_Trees.Invalid_Node_Access;
      Index : Syntax_Trees.Stream_Index := Syntax_Trees.Invalid_Stream_Index;
   end record;
   package Line_Begin_Token_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (Line_Number_Type, Tree_Ref, Default_Element => (others => <>));

   type Wrapped_Lexer_Error is record
      Recover_Token_Node  : Syntax_Trees.Valid_Node_Access;
      Recover_Token_Index : Syntax_Trees.Stream_Index;
      --  Token that lexer returned at the error.
      --
      --  Stream_Index is needed by error recovery, for Stream_Prev/_Next.
      --
      --  Node_Access is needed for post parse actions, when
      --  Tree.Terminal_Stream does not exist.

      Error : WisiToken.Lexer.Error;
   end record;

   package Wrapped_Lexer_Error_Lists is new Ada.Containers.Doubly_Linked_Lists (Wrapped_Lexer_Error);

   type Base_Parser (Descriptor : Descriptor_Access_Constant) is abstract new Ada.Finalization.Limited_Controlled
   with record
      Trace     : access WisiToken.Trace'Class;
      Lexer     : WisiToken.Lexer.Handle;
      Tree      : aliased Syntax_Trees.Tree (Descriptor);
      User_Data : WisiToken.Syntax_Trees.User_Data_Access;

      Wrapped_Lexer_Errors : aliased Wrapped_Lexer_Error_Lists.List;
      --  For access by error recover.

      Line_Begin_Token : aliased Line_Begin_Token_Vectors.Vector;
      --  Line_Begin_Token (I) is the node in Tree of the first
      --  Shared_Terminal token on line I; Invalid_Node_Access if there are
      --  no grammar tokens on the line (ie only comment or whitespace).
      --  Line_Begin_Token.First_Index is the first line containing a
      --  grammar token (after leading comments).

      Last_Grammar_Node : Syntax_Trees.Node_Access := Syntax_Trees.Invalid_Node_Access;
      --  Last grammar token returned from Lexer; for storing non_grammar
      --  tokens in it.

   end record;
   --  Common to all parsers. Finalize should free any allocated objects.

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Parser.Tree (Stream) and return
   --  its ID. Otherwise, repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All (Parser : in out Base_Parser'Class);
   --  Clear Line_Begin_Token, Last_Grammar_Node; reset User_Data. Then
   --  call Next_Grammar_Token repeatedly until EOF_ID is returned,
   --  storing all tokens in Parser.Tree Terminal_Stream.
   --
   --  The user must first call Lexer.Reset_* to set the input text.

   procedure Parse (Parser : in out Base_Parser) is abstract;
   --  Call Lex_All, then execute parse algorithm to parse the tokens,
   --  storing the result in Parser for Execute_Actions.
   --
   --  If a parse error is encountered, raises Syntax_Error.
   --  Parser.Lexer_Errors and Parser contain information about the
   --  errors.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.

   function Any_Errors (Parser : in Base_Parser) return Boolean is abstract;

   procedure Put_Errors (Parser : in Base_Parser) is abstract;
   --  Output error messages to Ada.Text_IO.Current_Error.

   procedure Execute_Actions
     (Parser          : in out Base_Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null)
     is abstract;
   --  Execute all actions in Parser.Tree.

end WisiToken.Parse;
