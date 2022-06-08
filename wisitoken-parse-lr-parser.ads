--  Abstract :
--
--  Utilities for a generalized LR parser.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 - 2022 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Lexer;
with WisiToken.Parse.Parser;
with WisiToken.Parse.LR.Parser_Lists;
package WisiToken.Parse.LR.Parser is

   procedure Get_Action
     (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Parser_State  : in out Parser_Lists.Parser_State;
      Action_Cur    :    out Parse_Action_Node_Ptr;
      Action        :    out Parse_Action_Rec);

   procedure Do_Action
     (Action         : in     Parse_Action_Rec;
      Current_Parser : in     WisiToken.Parse.LR.Parser_Lists.Cursor;
      Shared_Parser  : in out WisiToken.Parse.Parser.Parser'Class);
   --  Apply Action to Current_Parser; sets Current_Parser.Verb.

   procedure Parse_Verb
     (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Recover_Only  : in     Boolean;
      Verb          :    out All_Parse_Action_Verbs;
      Zombie_Count  :    out SAL.Base_Peek_Type);
   --  Recover_Only is true when the LR parser is called from the Packrat
   --  parser for error recover.
   --
   --  Verb: the type of parser cycle to execute;
   --
   --     Accept_It : all Parsers.Verb return Accept - done parsing.
   --
   --     Shift : some Parsers.Verb return Shift.
   --
   --     Pause : Resume is active, and this parser has reached
   --  Resume_Goal, so it is waiting for the others to catch up. Or
   --  resume is not active, and this parser has shifted a nonterminal,
   --  while some other parser has broken down that nonterminal; it is
   --  waiting for the others to catch up. This ensures parsers are
   --  within Mckenzie_Param.Zombie_Limit of the same terminal when they
   --  enter error recovery.
   --
   --     Reduce : some Parsers.Verb return Reduce.
   --
   --     Error : all Parsers.Verb return Error.
   --
   --  Zombie_Count: count of parsers in Error state

   procedure Recover_To_Log
     (Shared_Parser            : in out WisiToken.Parse.Parser.Parser'Class;
      Recover_Log_File         : in     Ada.Text_IO.File_Type;
      Recover_Result           : in     McKenzie_Recover.Recover_Status;
      Pre_Recover_Parser_Count : in     SAL.Base_Peek_Type);

   procedure Check_Error
     (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Check_Parser  : in out WisiToken.Parse.LR.Parser_Lists.Cursor);

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Parser.Parser'Class);
   --  Final actions after LR accept state reached; call
   --  User_Data.Insert_Token, Delete_Token.

   procedure Add_Parser
     (Parser                         : in out WisiToken.Parse.Parser.Parser'Class;
      Table                          : in     Parse_Table_Ptr;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access);
   --  Populate LR and error recover components of Parser.

   function New_Parser
     (Lexer                          : in     WisiToken.Lexer.Handle;
      Table                          : in     Parse_Table_Ptr;
      Productions                    : in     Syntax_Trees.Production_Info_Trees.Vector;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access;
      User_Data                      : in     WisiToken.Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.Parser.Parser;

   procedure Edit_Tree
     (Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Edits  : in     KMN_Lists.List)
   with Pre => Parser.Tree.Editable,
     Post => Parser.Tree.Stream_Count = 1;
   --  Assumes Parser.Lexer.Source has changed in a way reflected in
   --  Edits. Uses Edits to direct editing Parser.Tree to reflect lexing
   --  the changed source, in preparation for Incremental_Parse; result
   --  is in Tree.Shared_Stream.

end WisiToken.Parse.LR.Parser;
