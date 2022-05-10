--  Abstract :
--
--  Run the gpr LR1 parser standalone. Useful for debugging grammar issues.
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

with Gen_Run_Wisi_LR_Parse;
with Gpr_Process_Actions;
with Gpr_Process_LR1_Main;
with Wisi.Gpr;
procedure Run_Gpr_LR1_Parse is new Gen_Run_Wisi_LR_Parse
  (Parse_Data_Type                => Wisi.Gpr.Parse_Data_Type,
   Partial_Parse_Active           => Gpr_Process_Actions.Partial_Parse_Active'Access,
   Partial_Parse_Byte_Goal        => Gpr_Process_Actions.Partial_Parse_Byte_Goal'Access,
   Language_Fixes                 => null,
   Language_Matching_Begin_Tokens => null,
   Language_String_ID_Set         => null,
   Create_Lexer                   => Gpr_Process_LR1_Main.Create_Lexer,
   Create_Parse_Table             => Gpr_Process_LR1_Main.Create_Parse_Table,
   Create_Productions             => Gpr_Process_LR1_Main.Create_Productions);
