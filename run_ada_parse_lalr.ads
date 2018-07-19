--  Abstract :
--
--  Run the Ada LALR parser standalone. Useful for debugging grammar issues.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with Ada_Process_Actions;
with Ada_Process_LALR_Main;
with Gen_Run_Wisi_LR_Parse;
with WisiToken.LR.McKenzie_Recover.Ada;
with WisiToken.Wisi_Runtime.Ada;
procedure Run_Ada_Parse_LALR is new Gen_Run_Wisi_LR_Parse
  (WisiToken.Wisi_Runtime.Ada.Parse_Data_Type,
   "Ada",
   Ada_Process_Actions.Descriptor,
   WisiToken.LR.McKenzie_Recover.Ada.Language_Fixes'Access,
   WisiToken.LR.McKenzie_Recover.Ada.Constrain_Terminals'Access,
   WisiToken.LR.McKenzie_Recover.Ada.String_ID_Set'Access,
   Ada_Process_LALR_Main.Create_Parser);
