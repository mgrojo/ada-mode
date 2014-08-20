--  Abstract :
--
--  Non-OpenToken parser for Wisent grammar files, producing Ada or
--  Elisp source files.
--
--  Copyright (C) 2012 - 2014 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with OpenToken;
with Wisi.Declarations;
with Wisi.Output_Ada;
with Wisi.Output_Elisp;
with Wisi.Prologue;
with Wisi.Rules;
with Wisi.Test_Generate;
procedure Wisi.Generate
is

   procedure Put_Usage
   is
      use Standard.Ada.Text_IO;
   begin
      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line ("wisi-generate [-v level] {wisent grammar file} {output language}");
      Put_Line ("generate output language source corresponding to 'wisent grammar file'");
      Put_Line ("output language is one of Ada, Elisp, Test");
      Put_Line ("-v sets verbosity (defaults to 0 with no -v, 1 with just -v):");
      Put_Line ("   level 0 - only error messages to standard error");
      Put_Line ("   level 1 - add compiled grammar output to standard out");
      Put_Line ("   level 2 - add diagnostics to standard out, ignore unused tokens, unknown conflicts");
   end Put_Usage;

   type Output_Language_Type is (Ada, Elisp, Test);

   Output_Language : Output_Language_Type;

   Input_File_Name  : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Input_File       : Standard.Ada.Text_IO.File_Type;
   Output_File_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Prologue         : String_Lists.List;
   Keywords         : String_Pair_Lists.List;
   Tokens           : Token_Lists.List;
   Start_Token      : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts        : Conflict_Lists.List;
   Rules            : Rule_Lists.List;
   Actions_Present  : Boolean;

   procedure Use_Input_File (File_Name : in String)
   is
      use Standard.Ada.Text_IO;
   begin
      Input_File_Name  := +File_Name;
      Output_File_Root := +Standard.Ada.Directories.Base_Name (File_Name);
      Open (Input_File, In_File, File_Name);
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

   procedure Set_Output_Language (Image : in String)
   is begin
      Output_Language := Output_Language_Type'Value (Image);
   exception
   when Constraint_Error =>
      raise User_Error;
   end Set_Output_Language;

begin
   declare
      use Standard.Ada.Command_Line;
      Arg_Next     : Integer := 1;
      Options_Done : Boolean := False;
   begin
      loop
         exit when Options_Done;
         if Argument (Arg_Next)(1) /= '-' then
            Options_Done := True;
         else
            if Argument (Arg_Next) = "-v" then
               Arg_Next  := Arg_Next + 1;
               Verbosity := Integer'Value (Argument (Arg_Next));
               Arg_Next  := Arg_Next + 1;

            else
               raise User_Error;
            end if;
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));
      Arg_Next := Arg_Next + 1;
      Set_Output_Language (Argument (Arg_Next));

      if Arg_Next /= Argument_Count then
         raise User_Error;
      end if;
   end;

   Wisi.Prologue (Input_File, Prologue);
   Wisi.Declarations (Input_File, Keywords, Tokens, Start_Token, Conflicts);
   Wisi.Rules (Input_File, Rules, Actions_Present);

   case Output_Language is
   when Ada =>
      Wisi.Output_Ada
        (-Input_File_Name, -Output_File_Root, Prologue, Keywords, Tokens, Start_Token, Conflicts, Rules,
         Actions_Present);
   when Elisp =>
      Wisi.Output_Elisp (-Output_File_Root, Prologue, Keywords, Tokens, Start_Token, Conflicts, Rules);
   when Test =>
      Wisi.Test_Generate (-Input_File_Name, Keywords, Tokens, Start_Token, Rules);
   end case;

exception
when User_Error =>
   Standard.Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);
   Put_Usage;

when E : OpenToken.Grammar_Error =>
   Standard.Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);
   Standard.Ada.Text_IO.Put_Line (Standard.Ada.Exceptions.Exception_Message (E));

when E :  others =>
   --  IMPROVEME: for some exceptions, Error message already output via wisi.utils.Put_Error
   declare
      use Standard.Ada.Text_IO;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Command_Line;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Set_Exit_Status (Failure);
   end;

end Wisi.Generate;
