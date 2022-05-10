--  Abstract :
--
--  External process parser for Ada mode
--
--  Copyright (C) 2017 - 2020, 2022 Free Software Foundation, Inc.
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

with Gen_Emacs_Wisi_Packrat_Gen_Parse;
with Ada_Annex_P_Process_Packrat_Gen_Main;
with Wisi.Ada;
procedure Ada_Mode_Wisi_Packrat_Gen_Parse is new Gen_Emacs_Wisi_Packrat_Gen_Parse
  (Parse_Data_Type           => Wisi.Ada.Parse_Data_Type,
   Name                      => "Ada packrat",
   Language_Protocol_Version => Wisi.Ada.Language_Protocol_Version,
   Create_Parser             => Ada_Annex_P_Process_Packrat_Gen_Main.Create_Parser);
