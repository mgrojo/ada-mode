--  Abstract:
--
--  Main program to run a parser, using a Counted_GNAT_OS_Lib text feeder.
--
--  Copyright (C) 2015, 2017 Stephe Leake
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

with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder;
generic
   with function Create_Parser
     (Algorithm    : in WisiToken.Parser_Algorithm_Type;
      Text_Feeder  : in WisiToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size  : in Integer                               := 1024)
     return WisiToken.Parser.LR.Parser.Instance;

procedure Gen_Parser_Run_Counted_GNAT_OS_Lib;
