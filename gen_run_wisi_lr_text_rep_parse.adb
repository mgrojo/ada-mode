--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Directories;
with Run_Wisi_Common_Parse;
procedure Gen_Run_Wisi_LR_Text_Rep_Parse
is
   Parse_Data_Template : aliased Parse_Data_Type;
begin
   Run_Wisi_Common_Parse.Parse_File
     ((Descriptor, Create_Lexer, Create_Parse_Table
         (Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name) & "/" & Text_Rep_File_Name),
       Partial_Parse_Active, Partial_Parse_Byte_Goal, Language_Fixes, Language_Matching_Begin_Tokens,
       Language_String_ID_Set, Parse_Data_Template'Unchecked_Access));
end Gen_Run_Wisi_LR_Text_Rep_Parse;
