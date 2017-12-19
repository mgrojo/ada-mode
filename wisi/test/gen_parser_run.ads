--  Abstract:
--
--  Main program to run a parser.
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

with WisiToken.LR;
with WisiToken.Semantic_State;
generic
   Descriptor : in WisiToken.Descriptor'Class;

   with procedure  Create_Parser
     (Parser         :    out WisiToken.LR.Instance;
      Algorithm      : in     WisiToken.Parser_Algorithm_Type;
      Semantic_State : in     WisiToken.Semantic_State.Semantic_State_Access);

   LR1 : in Boolean;
procedure Gen_Parser_Run;
