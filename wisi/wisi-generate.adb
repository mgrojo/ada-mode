--  Abstract :
--
--  Non-OpenToken parser for Wisent grammar files, producing Ada or
--  Elisp source files.
--
--  Copyright (C) 2012 - 2015 Stephen Leake.  All Rights Reserved.
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
with Wisi.Output_Ada_Emacs;
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
      Put_Line ("wisi-generate [options] {wisent grammar file} {lexer} {output language}");
      Put_Line ("version 0.00 - experimental");
      Put_Line ("generate output language source implementing a parser for 'wisent grammar file'");
      Put_Line ("'lexer' is one of Aflex_Lexer, OpenToken_Lexer");
      Put_Line ("'output language' is one of Ada_Emacs, Elisp, Test");
      Put_Line ("options are:");
      Put_Line ("  -v level: sets verbosity (default 0):");
      Put_Line ("     level 0 - only error messages to standard error");
      Put_Line ("     level 1 - add compiled grammar output to standard out");
      Put_Line ("     level 2 - add diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line ("  --first_state_index <n>; default 0");
      Put_Line ("  --first_parser label <n>; default 0");
      Put_Line ("  --profile; actions just count");
      Put_Line ("  --suffix <string>; appended to grammar file name");
   end Put_Usage;

   type Output_Language_Type is (Ada_Emacs, Elisp, Test);

   Output_Language : Output_Language_Type;

   Input_File_Name    : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Input_File         : Standard.Ada.Text_IO.File_Type;
   Output_File_Root   : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Suffix             : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Prologue           : String_Lists.List;
   Keywords           : String_Pair_Lists.List;
   Tokens             : Token_Lists.List;
   Start_Token        : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts          : Conflict_Lists.List;
   Rules              : Rule_Lists.List;
   Rule_Count         : Integer;
   Action_Count       : Integer;
   Lexer              : Lexer_Type;
   First_State_Index  : Integer := 0; -- default
   First_Parser_Label : Integer := 0; -- default
   Profile            : Boolean := False;

   procedure Use_Input_File (File_Name : in String)
   is
      use Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
   begin
      Input_File_Name  := +File_Name;
      Output_File_Root := +Standard.Ada.Directories.Base_Name (File_Name) & Suffix;
      Open (Input_File, In_File, File_Name);
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

begin
   declare
      use Standard.Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         if Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            Verbosity := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--first_state_index" then
            Arg_Next  := Arg_Next + 1;
            First_State_Index := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--first_parser_label" then
            Arg_Next  := Arg_Next + 1;
            First_Parser_Label := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--profile" then
            Arg_Next := Arg_Next + 1;
            Profile  := True;

         elsif Argument (Arg_Next) = "--suffix" then
            Arg_Next := Arg_Next + 1;
            Suffix   := +Argument (Arg_Next);
            Arg_Next := Arg_Next + 1;

         else
            raise User_Error;
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));
      Arg_Next := Arg_Next + 1;

      begin
         Lexer := Lexer_Type'Value (Argument (Arg_Next));
         Arg_Next := Arg_Next + 1;
      exception
      when Constraint_Error =>
         raise User_Error;
      end;

      begin
         Output_Language := Output_Language_Type'Value (Argument (Arg_Next));
      exception
      when Constraint_Error =>
         raise User_Error;
      end;

      if Arg_Next /= Argument_Count then
         raise User_Error with "arg count" & Integer'Image (Argument_Count) &
           " different from expected count" & Integer'Image (Arg_Next);
      end if;
   end;

   Wisi.Prologue (Input_File, Prologue);
   Wisi.Declarations (Input_File, Keywords, Tokens, Start_Token, Conflicts);
   Wisi.Rules (Input_File, Rules, Rule_Count, Action_Count);

   case Output_Language is
   when Ada_Emacs =>
      Wisi.Output_Ada_Emacs
        (-Input_File_Name, -Output_File_Root, Prologue, Keywords, Tokens, Start_Token, Conflicts, Rules,
         Rule_Count, Action_Count, Lexer, First_State_Index, First_Parser_Label, Profile);

   when Elisp =>
      Wisi.Output_Elisp
        (-Input_File_Name, -Output_File_Root, Prologue, Keywords, Tokens, Start_Token, Conflicts, Rules,
         First_State_Index);

   when Test =>
      Wisi.Test_Generate (-Input_File_Name, Keywords, Tokens, Start_Token, Conflicts, Rules, First_State_Index);
   end case;

exception
when E : User_Error =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
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
