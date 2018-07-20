--  Abstract :
--
--  Run the wisi-grammar parser standalone. Useful for debugging grammar issues.
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

with Wisi_Grammar_1_Process_Actions;
with Wisi_Grammar_1_Process_Main;
with Gen_Run_Wisi_LR_Parse;
with WisiToken.Wisi_Runtime;
procedure Run_Wisi_Grammar_Parse is new Gen_Run_Wisi_LR_Parse
  (WisiToken.Wisi_Runtime.Parse_Data_Type,
   "Wisi_Grammar", Wisi_Grammar_1_Process_Actions.Descriptor,
   null, null, null,
   Wisi_Grammar_1_Process_Main.Create_Parser);
