--  Abstract :
--
--  Run the Ada parser standalone. Useful for debugging grammar issues.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada_Process;
with Gen_Run_Wisi_Parse;
with WisiToken.Wisi_Runtime;
procedure Run_Ada_Parse is new Gen_Run_Wisi_Parse
  ("Ada", Ada_Process.Descriptor,
   WisiToken.Wisi_Runtime.Parse_Data_Type'Class (Ada_Process.Parse_Data),
   Ada_Process.Create_Parser);
