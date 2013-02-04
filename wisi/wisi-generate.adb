--  Abstract :
--
--  Non-OpenToken parser for Wisent grammar files, producing OpenToken Ada source files.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Wisi.Declarations;
with Wisi.Output;
with Wisi.Prologue;
with Wisi.Rules;
procedure Wisi.Generate
is

   procedure Put_Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("wisi-generate [-v] {wisent grammar file}");
      Put_Line ("  -v : verbose");
      Put_Line ("  generate Ada OpenToken source corresponding to 'wisent grammar file'");
   end Put_Usage;

   Input_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
   Input_File       : Ada.Text_IO.File_Type;
   Output_File_Root : Ada.Strings.Unbounded.Unbounded_String;
   Prologue         : String_Lists.List;
   Declarations     : String_Pair_Lists.List;
   Tokens           : String_Triplet_Lists.List;
   Rules            : Rule_Lists.List;

   Copyright : constant String := "2013 Stephen Leake.  All Rights Reserved.";
   --  FIXME: get copyright from grammar file

   procedure Use_Input_File (File_Name : in String)
   is
      use Ada.Text_IO;
   begin
      Input_File_Name := +File_Name;
      Open (Input_File, In_File, File_Name);
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         Use_Input_File (Argument (1));
         Output_File_Root := +Ada.Directories.Base_Name (Argument (1));

      when 2 =>
         if Argument (1) = "-v" then
            Verbose := True;
            Use_Input_File (Argument (2));
            Output_File_Root := +Ada.Directories.Base_Name (Argument (2));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   end;

   Wisi.Prologue (Input_File, Prologue);
   Wisi.Declarations (Input_File, Declarations, Tokens);
   Wisi.Rules (Input_File, Rules);
   Wisi.Output (-Input_File_Name, -Output_File_Root, Copyright, Prologue, Declarations, Tokens, Rules);

end Wisi.Generate;
