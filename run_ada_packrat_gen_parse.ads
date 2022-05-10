--  Abstract :
--
--  Run the Ada Packrat parser standalone. Useful for debugging grammar issues.
--
--  Copyright (C) 2022 Free Software Foundation, Inc.
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

with Ada_Annex_P_Process_Packrat_Gen_Main;
with Gen_Run_Wisi_Packrat_Gen_Parse;
--  FIXME: packrat support error recover. with WisiToken.Parse.LR.McKenzie_Recover.Ada;
with Wisi.Ada;
procedure Run_Ada_Packrat_Gen_Parse is new Gen_Run_Wisi_Packrat_Gen_Parse
  (Parse_Data_Type => Wisi.Ada.Parse_Data_Type,
   Create_Parser   => Ada_Annex_P_Process_Packrat_Gen_Main.Create_Parser);
