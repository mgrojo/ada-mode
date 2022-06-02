--  Abstract:
--
--  Main program to run an LR parser with error recovery.
--
--  Copyright (C) 2015, 2017 - 2020, 2022 Stephe Leake
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

with WisiToken.Parse.LR;
with WisiToken.Parse.Parser;
with WisiToken.Syntax_Trees;
generic
   Language_Fixes : in WisiToken.Parse.LR.Language_Fixes_Access;

   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Language_Matching_Begin_Tokens_Access;

   Language_String_ID_Set : in WisiToken.Parse.LR.Language_String_ID_Set_Access;

   with function Create_Parser
     (Trace                          : in WisiToken.Trace_Access;
      User_Data                      : in WisiToken.Syntax_Trees.User_Data_Access;
      Language_Fixes                 : in WisiToken.Parse.LR.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in WisiToken.Parse.LR.Language_String_ID_Set_Access)
     return WisiToken.Parse.Parser.Parser;

procedure Gen_LR_Parser_Run;
