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
package WisiToken.Parse.LR.Parser is

   procedure New_Parser
     (Parser                         : in out WisiToken.Parse.Parser.Parser'Class;
      Lexer                          : in     WisiToken.Lexer.Handle;
      Table                          : in     Parse_Table_Ptr;
      Productions                    : in     Syntax_Trees.Production_Info_Trees.Vector;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access;
      User_Data                      : in     WisiToken.Syntax_Trees.User_Data_Access);
   --  Populate LR and error recover components of Parser.

   procedure Edit_Tree
     (Parser : in out WisiToken.Parse.Parser.Parser;
      Edits  : in     KMN_Lists.List)
   with Pre => Parser.Tree.Editable,
     Post => Parser.Tree.Stream_Count = 1;
   --  Assumes Parser.Lexer.Source has changed in a way reflected in
   --  Edits. Uses Edits to direct editing Parser.Tree to reflect lexing
   --  the changed source, in preparation for Incremental_Parse; result
   --  is in Tree.Shared_Stream.

end WisiToken.Parse.LR.Parser;
