--  Abstract:
--
--  Main program to run a parser.
--
--  Copyright (C) 2022 Stephe Leake
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

with WisiToken.Parse.Packrat.Generated;
with WisiToken.Parse;
with WisiToken.Syntax_Trees;
generic
   with function Create_Parser
     (Trace     : in WisiToken.Trace_Access;
      User_Data : in WisiToken.Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.Packrat.Generated.Parser;
procedure Gen_Packrat_Gen_Parser_Run;
