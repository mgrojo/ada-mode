--  Abstract :
--
--  Common utilities for Gen_Run_Wisi_*_Parse
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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

with Ada.Strings.Unbounded;
with Wisi;
with WisiToken;
with Wisi.Parse_Context;
package Run_Wisi_Common_Parse is

   type Command_Type is (Parse_Partial, Parse_Incremental, Refactor, Command_File);

   type Command_Line_Params (Command : Command_Type) is record

      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Language_Params  : Ada.Strings.Unbounded.Unbounded_String;
      Repeat_Count     : Integer                    := 1;

      case Command is
      when Parse_Partial =>
         Partial_Post_Parse_Action : Wisi.Post_Parse_Action_Type;
         Partial_Begin_Byte_Pos    : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_End_Byte_Pos      : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_Goal_Byte_Pos     : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_Begin_Char_Pos    : WisiToken.Buffer_Pos       := WisiToken.Buffer_Pos'First;
         Partial_End_Char_Pos      : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Partial_Begin_Line        : WisiToken.Line_Number_Type := WisiToken.Line_Number_Type'First;
         Partial_Begin_Indent      : Integer                    := 0;

      when Parse_Incremental =>
         --  Incremental edit, parse, post_parse_action
         Changes               : Wisi.Change_Lists.List;
         Inc_Post_Parse_Action : Wisi.Post_Parse_Action_Type;
         Inc_Begin_Byte_Pos    : WisiToken.Buffer_Pos := WisiToken.Invalid_Buffer_Pos;
         Inc_Begin_Char_Pos    : WisiToken.Buffer_Pos := WisiToken.Invalid_Buffer_Pos;
         Inc_End_Byte_Pos      : WisiToken.Buffer_Pos := WisiToken.Invalid_Buffer_Pos;
         Inc_End_Char_Pos      : WisiToken.Buffer_Pos := WisiToken.Invalid_Buffer_Pos;

      when Refactor =>
         --  We assume the file contains only the one statement/declaration
         --  that needs refactoring.

         Refactor_Action : Wisi.Refactor_Action;
         --  Language-specific

         Edit_Begin : WisiToken.Buffer_Pos;
         --  Source file byte position at start of expression to refactor.

      when Command_File =>
         Command_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   procedure Parse_File (Language : in Wisi.Parse_Context.Language);
   --  Reads command line, processes command(s).

end Run_Wisi_Common_Parse;
