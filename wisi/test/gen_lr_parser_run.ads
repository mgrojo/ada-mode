--  Abstract:
--
--  Main program to run an LR parser with error recovery.
--
--  Copyright (C) 2015, 2017, 2018 Stephe Leake
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

with WisiToken.LR.Parser;
with WisiToken.Syntax_Trees;
generic
   Descriptor : in WisiToken.Descriptor;

   with procedure Create_Parser
     (Parser                              :    out          WisiToken.LR.Parser.Parser;
      Language_Fixes                      : in              WisiToken.LR.Parser.Language_Fixes_Access;
      Language_Use_Minimal_Complete_Actions : in
        WisiToken.LR.Parser.Language_Use_Minimal_Complete_Actions_Access;
      Language_String_ID_Set              : in              WisiToken.LR.Parser.Language_String_ID_Set_Access;
      Trace                               : not null access WisiToken.Trace'Class;
      User_Data                           : in              WisiToken.Syntax_Trees.User_Data_Access);

procedure Gen_LR_Parser_Run;
