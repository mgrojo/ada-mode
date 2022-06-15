--  Abstract :
--
--  Generalized LR parser, with McKenzie error recovery. Allows adding
--  Packrat with McKenzie error recovery; see
--  wisitoken-parse-packrat-parser.ads.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Finalization;
with WisiToken.Parse.LR.Parser_Lists;
package WisiToken.Parse.Parser is

   type Parser (First_Nonterminal, Last_Nonterminal : Token_ID)
     is new Ada.Finalization.Limited_Controlled
   with record
      Tree        : aliased Syntax_Trees.Tree;
      Productions : Syntax_Trees.Production_Info_Trees.Vector;
      User_Data   : Syntax_Trees.User_Data_Access;

      --  LR components
      Table                   : WisiToken.Parse.LR.Parse_Table_Ptr;
      Parsers                 : aliased WisiToken.Parse.LR.Parser_Lists.List;
      Partial_Parse_Active    : access Boolean;
      Partial_Parse_Byte_Goal : access WisiToken.Buffer_Pos;
      --  Used by In_Parse_Actions to terminate Partial_Parse.

      --  LR Error recover components
      Language_Fixes                 : WisiToken.Parse.LR.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : WisiToken.Parse.LR.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : WisiToken.Parse.LR.Language_String_ID_Set_Access;

      String_Quote_Checked : Base_Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Error : Boolean := False;
      --  True if any error occured during parsing.

      Resume_Active         : Boolean                       := False;
      Min_Sequential_Index  : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
      Max_Sequential_Index  : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'First;
      Recover_Enqueue_Count : Integer                       := 0;
      Recover_Check_Count   : Integer                       := 0;
   end record;

   type Parser_Access is access all Parser'Class;

   overriding procedure Finalize (Object : in out Parser);
   --  Free all allocated memory. Object.User_Data is not freed; caller
   --  is responsible for that.

   type Factory is access function return Parser_Access;

   function Source_File_Name (Item : in Parser'Class) return String
   is (Item.Tree.Lexer.File_Name);

   procedure Process_Grammar_Token
     (Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Token  : in     Lexer.Token;
      Node   : in     Syntax_Trees.Valid_Node_Access);

   procedure Process_Non_Grammar_Token
     (Parser       : in out WisiToken.Parse.Parser.Parser'Class;
      Grammar_Node : in     Syntax_Trees.Valid_Node_Access;
      Token        : in     Lexer.Token);

   function Get_In_Parse_Action
     (Parser : in WisiToken.Parse.Parser.Parser;
      ID     : in Production_ID)
     return Syntax_Trees.In_Parse_Actions.In_Parse_Action;

   function Get_Post_Parse_Action
     (Productions : in Syntax_Trees.Production_Info_Trees.Vector;
      ID          : in Production_ID)
     return Syntax_Trees.Post_Parse_Action;

   function Get_Post_Parse_Action
     (Parser : in WisiToken.Parse.Parser.Parser;
      ID     : in Production_ID)
     return Syntax_Trees.Post_Parse_Action;

   function Next_Grammar_Token
     (Parser            : in out WisiToken.Parse.Parser.Parser'Class;
      Last_Grammar_Node : in out WisiToken.Syntax_Trees.Node_Access)
     return Token_ID
   with Post => Next_Grammar_Token'Result /= Invalid_Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Parser.Tree (Stream) and return
   --  its ID. If is it a non_grammar token, store it in
   --  Last_Grammar_Node.Non_Grammar or Parser.Tree.Non_Grammar, and
   --  repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All (Parser : in out WisiToken.Parse.Parser.Parser'Class)
   with Pre => Parser.Tree.Cleared;
   --  Call Tree.Start_Lex, clear Last_Grammar_Node; reset User_Data.
   --  Then call Next_Grammar_Token repeatedly until EOF_ID is returned,
   --  storing all tokens in Parser.Tree.Shared_Stream.
   --
   --  The user must first call Lexer.Reset_* to set the input text.

   procedure LR_Parse
     (Parser     : in out WisiToken.Parse.Parser.Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False);
   --  If Pre_Edited, skip this first step (used for unit tests). Else if
   --  Edits is empty, call Lex_All; if Edits is not empty, call
   --  Edit_Tree.
   --
   --  Then execute the LR parse algorithm to parse the shared stream
   --  tokens, storing the result in Parser.Tree for Execute_Actions.
   --
   --  If Log_File is open, write information about each error recover
   --  session to it. See implementation for format.
   --
   --  If a non-recoverable parse error is encountered, raises
   --  Syntax_Error. Error information is stored in the tree.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.

   procedure LR_Core_Parse
     (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Log_File      : in     Ada.Text_IO.File_Type;
      Recover_Only  : in     Boolean);
   --  Run the core LR parse algorithm starting from the current state of
   --  Parser, with error recover. If Recover_Only, exit when resume
   --  after error recover is done.
   --
   --  Used by LR_Parse to run parser, and by packrat parse to run error
   --  recover.

   procedure Parse
     (Parser   : in out WisiToken.Parse.Parser.Parser'Class;
      Log_File : in     Ada.Text_IO.File_Type);
   --  Call LR_Parse (Edits is empty) or
   --  WisiToken.Parse.Packrat.Parser.Packrat_Parse, depending on the
   --  class of Parser.

   procedure Put_Errors (Parser : in WisiToken.Parse.Parser.Parser'Class)
   with Pre => Parser.Tree.Fully_Parsed;
   --  Output Parser.Tree errors to Ada.Text_IO.Current_Output.

   procedure Put_Errors (Parser : in WisiToken.Parse.Parser.Parser'Class; Stream : in Syntax_Trees.Stream_ID);
   --  Output Parser.Tree.Stream errors to Ada.Text_IO.Current_Output.

   procedure Execute_Actions
     (Tree                : in out Syntax_Trees.Tree;
      Productions         : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data           : in     Syntax_Trees.User_Data_Access;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region);
   --  Implements Execute_Actions, allows specifying different tree
   --  (needed by wisitoken-bnf-generate).

   procedure Execute_Actions
     (Parser              : in out WisiToken.Parse.Parser.Parser'Class;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region);
   --  Execute all actions in Parser.Tree nodes that overlap
   --  Action_Region_Bytes; all nodes if Action_Region_Bytes =
   --  Null_Buffer_Region. See wisitoken-syntax_trees.ads for other
   --  actions performed by Execute_Actions.

end WisiToken.Parse.Parser;
