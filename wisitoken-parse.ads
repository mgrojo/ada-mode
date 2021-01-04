--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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
with SAL.Gen_Definite_Doubly_Linked_Lists;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   type Wrapped_Lexer_Error is record
      Recover_Token_Ref : Syntax_Trees.Terminal_Ref;
      --  Token that lexer returned at the error.
      --
      --  If the error token is a grammar token, Recover_Token_Ref is in
      --  Shared_Stream; it is needed by error recovery, for
      --  Stream_Prev/_Next in Try_Insert_Quote.
      --
      --  If the error token is a non-grammar token, Recover_Token_Index is
      --  Invalid_Terminal_Ref.

      Error : WisiToken.Lexer.Error;
   end record;

   package Wrapped_Lexer_Error_Lists is new Ada.Containers.Doubly_Linked_Lists (Wrapped_Lexer_Error);

   type Base_Parser is abstract new Ada.Finalization.Limited_Controlled
   with record
      Trace     : WisiToken.Trace_Access;
      Tree      : aliased Syntax_Trees.Tree;
      User_Data : WisiToken.Syntax_Trees.User_Data_Access;

      Wrapped_Lexer_Errors : aliased Wrapped_Lexer_Error_Lists.List;
      --  For access by error recover.

      Last_Grammar_Node : Syntax_Trees.Node_Access := Syntax_Trees.Invalid_Node_Access;
      --  Last grammar token returned from Lexer; for storing non_grammar
      --  tokens in it.

   end record;
   --  Common to all parsers. Finalize should free any allocated objects.

   function Source_File_Name (Item : in Base_Parser'Class) return String
   is (Item.Tree.Lexer.File_Name);

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Parser.Tree (Stream) and return
   --  its ID. Otherwise, repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All (Parser : in out Base_Parser'Class);
   --  Clear Line_Begin_Token, Last_Grammar_Node; reset User_Data. Then
   --  call Next_Grammar_Token repeatedly until EOF_ID is returned,
   --  storing all tokens in Parser.Tree.Shared_Stream.
   --
   --  The user must first call Lexer.Reset_* to set the input text.

   type KMN is record
      --  Similar to [Lahav 2004] page 6; describes changed and unchanged
      --  regions in a text buffer. We assume the range boundaries do not
      --  break a multi-byte character.

      Stable_Bytes : Base_Buffer_Pos; -- Count of unmodified bytes before change
      Stable_Chars : Base_Buffer_Pos; -- "" characters

      Deleted_Bytes : Base_Buffer_Pos; -- Count of deleted bytes, after Stable
      Deleted_Chars : Base_Buffer_Pos;

      Inserted_Bytes : Base_Buffer_Pos; -- Count of inserted bytes, after Stable.
      Inserted_Chars : Base_Buffer_Pos;
   end record;

   procedure Validate_KMN
     (KMN                      : in WisiToken.Parse.KMN;
      Initial_Stable_Byte_First : in Buffer_Pos;
      Initial_Stable_Char_First : in Buffer_Pos;
      Edited_Stable_Byte_First  : in Buffer_Pos;
      Edited_Stable_Char_First  : in Buffer_Pos;
      Initial_Text_Byte_Region  : in Buffer_Region;
      Initial_Text_Char_Region  : in Buffer_Region;
      Edited_Text_Byte_Region   : in Buffer_Region;
      Edited_Text_Char_Region   : in Buffer_Region);
   --  Raise User_Error if KMN violates text regions.

   package KMN_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (KMN);

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Stable_Byte_First        : in Buffer_Pos;
      Stable_Char_First        : in Buffer_Pos;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region);

   procedure Edit_Tree
     (Parser : in out Base_Parser'Class;
      Edits  : in     KMN_Lists.List)
   with Pre => Parser.Tree.Fully_Parsed or Parser.Tree.Editable,
     Post => Parser.Tree.Stream_Count = 1;
   --  Assumes Parser.Lexer.Source has changed in a way reflected in
   --  Edits. Uses Edits to direct editing Parser.Tree to reflect lexing
   --  the changed source, in preparation for Incremental_Parse; result
   --  is in Tree.Shared_Stream.

   procedure Parse
     (Parser   : in out Base_Parser;
      Log_File : in     Ada.Text_IO.File_Type;
      Edits    : in     KMN_Lists.List := KMN_Lists.Empty_List)
   is abstract;
   --  If Edits is empty, call Lex_All. If Edits is not empty, call
   --  Edit_Tree. Then execute parse algorithm to parse the new tokens,
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

   procedure Execute_Actions (Parser : in out Base_Parser) is abstract;
   --  Execute all actions in Parser.Tree. See wisitoken-syntax_trees.ads
   --  for other actions performed by Execute_Actions.

end WisiToken.Parse;
