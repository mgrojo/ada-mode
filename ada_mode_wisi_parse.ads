--  Abstract :
--
--  External process parser for Ada mode
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

with Ada_Process;
with Gen_Emacs_Wisi_Parse;
with WisiToken.LR.McKenzie_Recover.Ada;
with WisiToken.Wisi_Runtime;
procedure Ada_Mode_Wisi_Parse is new Gen_Emacs_Wisi_Parse
  (Name           => "Ada_mode",
   Descriptor     => Ada_Process.Descriptor,
   Parse_Data     => WisiToken.Wisi_Runtime.Parse_Data_Type'Class (Ada_Process.Parse_Data),
   Language_Fixes => WisiToken.LR.McKenzie_Recover.Ada.Language_Fixes'Access,
   Create_Parser  => Ada_Process.Create_Parser);
