--  Abstract :
--
--  Generic Emacs background process; parse token stream, return
--  parser actions.
--
--  See gen_run_wisi_parse.ads for a standalone version.
--
--  References : see gen_emacs_wisi_lr_parse.ads
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
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

with Wisi;
with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
generic
   type Parse_Data_Type  is new Wisi.Parse_Data_Type with private;

   Name                           : in String; --  for Usage, error messages. "_wisi_parse" will be appended
   Language_Protocol_Version      : in String; --  Defines language-specific parse parameters.
   Descriptor                     : in WisiToken.Descriptor_Access_Constant;
   Partial_Parse_Active           : in WisiToken.Boolean_Access;
   Partial_Parse_Byte_Goal        : in WisiToken.Buffer_Pos_Access;
   Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;
   Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
   Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
   Text_Rep_File_Name             : in String;

   with function Create_Lexer return WisiToken.Lexer.Handle;
   with function Create_Parse_Table (Text_Rep_File_Name : in String) return WisiToken.Parse.LR.Parse_Table_Ptr;

procedure Gen_Emacs_Wisi_LR_Text_Rep_Parse;
