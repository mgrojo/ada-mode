--  Abstract :
--
--  External process parser for Ada mode
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada_Annex_P_Process_Actions;
with Ada_Annex_P_Process_Main;
with Gen_Run_Wisi_LR_Parse;
--  with WisiToken.Parse.LR.McKenzie_Recover.Ada_Annex_P;
with Wisi.Ada;
procedure Run_Ada_Annex_P_LALR_Parse is new Gen_Run_Wisi_LR_Parse
  (Wisi.Ada.Parse_Data_Type,
   Ada_Annex_P_Process_Actions.Descriptor,
   null, --  FIXME: WisiToken.Parse.LR.McKenzie_Recover.Ada_Annex_P.Language_Fixes'Access,
   null, --  FIXME: WisiToken.Parse.LR.McKenzie_Recover.Ada_Annex_P.Matching_Begin_Tokens'Access,
   null, --  FIXME: WisiToken.Parse.LR.McKenzie_Recover.Ada_Annex_P.String_ID_Set'Access,
   Ada_Annex_P_Process_Main.Create_Parser);
