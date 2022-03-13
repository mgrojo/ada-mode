--  Abstract:
--
--  Main program to run an LR text_rep parser with error recovery.
--
--  Copyright (C) 2018 - 2020, 2022 Stephe Leake
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

pragma License (GPL);

with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
generic
   Text_Rep_File_Name : in String;

   Language_Fixes : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;

   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;

   Language_String_ID_Set : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;

   with function Create_Parse_Table (Text_Rep_File_Name : in String) return WisiToken.Parse.LR.Parse_Table_Ptr;
   with function Create_Productions return WisiToken.Parse.Production_Info_Trees.Vector;
   with function Create_Lexer (Trace : in WisiToken.Trace_Access) return WisiToken.Lexer.Handle;

procedure Gen_LR_Text_Rep_Parser_Run;
