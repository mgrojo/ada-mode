--  Abstract :
--
--  External process parser for wisitoken-grammar mode
--
--  Copyright (C) 2017 - 2019 Stephen Leake All Rights Reserved.
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

with Wisitoken_Grammar_1_Process_Actions;
with Wisitoken_Grammar_1_Process_Main;
with Gen_Emacs_Wisi_LR_Parse;
with Wisi;
procedure WisiToken_Grammar_Mode_Parse is new Gen_Emacs_Wisi_LR_Parse
  (Parse_Data_Type                       => Wisi.Parse_Data_Type,
   Name                                  => "wisi_grammar_mode",
   Descriptor                            => Wisitoken_Grammar_1_Process_Actions.Descriptor,
   Partial_Parse_Active                  => Wisitoken_Grammar_1_Process_Actions.Partial_Parse_Active,
   Language_Fixes                        => null,
   Language_Use_Minimal_Complete_Actions => null,
   Language_String_ID_Set                => null,
   Create_Parser                         => Wisitoken_Grammar_1_Process_Main.Create_Parser);
