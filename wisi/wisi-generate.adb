--  Abstract :
--
--  Parser for Wisi grammar files, producing Ada or Elisp source
--  files for a parser.
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with WisiToken;
with Wisi.Declarations;
with Wisi.Output_Ada;
with Wisi.Output_Ada_Emacs;
with Wisi.Output_Elisp;
with Wisi.Prologue;
with Wisi.Rules;
procedure Wisi.Generate
is
   procedure Put_Usage
   is
      use Standard.Ada.Text_IO;
   begin
      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line ("wisi-generate [options] {wisi grammar file}");
      Put_Line ("version 0.00 - experimental");
      Put_Line ("Generate source code implementing a parser for the grammar.");
      New_Line;
      Put_Line ("The following grammar file directives control parser generation:");
      Put_Line ("%first_state_index <n> - default 0");
      Put_Line ("%first_parser_label <n> -  default 0");
      Put_Line ("%parser_algorithm {LALR | LR1 | LALR_LR1}");
      Put_Line ("   LALR_LR1 generates both parsers; choice is made at parser run-time.");
      Put_Line ("%output_language' {Ada | Ada_Emacs | Elisp}");
      Put_Line ("%lexer {Aflex | Elisp | Regexp}");
      Put_Line ("%interface {Process | Module}");
      New_Line;
      Put_Line ("Interface is only valid with Ada_Emacs:");
      Put_Line ("   Ada_Emacs, Process is for an external subprocess communicating with Emacs.");
      Put_Line ("   Ada_Emacs, Module  is for a dynamically loaded Emacs module.");
      New_Line;
      Put_Line ("options are:");
      Put_Line ("  -v level: sets verbosity (default 0):");
      Put_Line ("     0 - only error messages to standard error");
      Put_Line ("     1 - add compiled grammar output to standard out");
      Put_Line ("     2 - add diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line ("  --suffix <string>; appended to grammar file name");
      Put_Line ("  --profile; grammar file production actions are replaced by counters");
      Put_Line ("  --<directive_name> <value>; same as directive in grammar file; override grammar file");

   end Put_Usage;

   Generate_Params : Generate_Param_Type;

   Input_File_Name         : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Input_File              : Standard.Ada.Text_IO.File_Type;
   Output_File_Root        : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Suffix                  : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Prologue_Context_Clause : String_Lists.List;
   Prologue_Declarations   : String_Lists.List;
   Keywords                : String_Pair_Lists.List;
   Tokens                  : Token_Lists.List;
   Conflicts               : Conflict_Lists.List;
   Panic_Recover           : String_Lists.List;
   Rules                   : Rule_Lists.List;
   Rule_Count              : Integer;
   Action_Count            : Integer;
   Profile                 : Boolean := False;

   procedure Use_Input_File (File_Name : in String)
   is
      use Standard.Ada.Strings.Unbounded;
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
            Generate_Params.First_State_Index := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--first_parser_label" then
            Arg_Next  := Arg_Next + 1;
            Generate_Params.First_Parser_Label := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--interface" then
            Arg_Next  := Arg_Next + 1;
            begin
               Generate_Params.Interface_Kind := Interface_Type'Value (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for interface: '" & Argument (Arg_Next) & ";";
            end;
         elsif Argument (Arg_Next) = "--lexer" then
            Arg_Next  := Arg_Next + 1;
            begin
               Generate_Params.Lexer := To_Lexer (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for lexer: '" & Argument (Arg_Next) & ";";
            end;

         elsif Argument (Arg_Next) = "--output_language" then
            Arg_Next  := Arg_Next + 1;
            begin
               Generate_Params.Output_Language := Output_Language_Type'Value (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for output_language: '" & Argument (Arg_Next) & ";";
            end;

         elsif Argument (Arg_Next) = "--parser_algorithm" then
            Arg_Next  := Arg_Next + 1;
            begin
               Generate_Params.Parser_Algorithm := Parser_Algorithm_Type'Value (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for parser_algorithm: '" & Argument (Arg_Next) & ";";
            end;

         elsif Argument (Arg_Next) = "--profile" then
            Arg_Next := Arg_Next + 1;
            Profile  := True;

         elsif Argument (Arg_Next) = "--suffix" then
            Arg_Next := Arg_Next + 1;
            Suffix   := +Argument (Arg_Next);
            Arg_Next := Arg_Next + 1;

         else
            raise User_Error with "invalid argument '" & Argument (Arg_Next) & "'";
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));

      if Arg_Next /= Argument_Count then
         raise User_Error with "arg count" & Integer'Image (Argument_Count) &
           " different from expected count" & Integer'Image (Arg_Next);
      end if;
   end;

   Wisi.Prologue (Input_File, Prologue_Context_Clause, Prologue_Declarations);
   Wisi.Declarations (Input_File, Generate_Params, Keywords, Tokens, Conflicts, Panic_Recover);
   Wisi.Rules (Input_File, Generate_Params.Output_Language, Rules, Rule_Count, Action_Count);

   --  FIXME: use run-time factory, dispatching instead of this case
   --  statement; allow moving output_elisp, output_ada_emacs to
   --  ada-mode source repository.
   case Generate_Params.Output_Language is
   when None =>
      raise Programmer_Error; -- checked in wisi.declarations

   when Ada =>
      Wisi.Output_Ada
        (-Input_File_Name, -Output_File_Root, Generate_Params, Prologue_Context_Clause, Prologue_Declarations,
         Keywords, Tokens, Conflicts, Panic_Recover, Rules, Rule_Count, Action_Count, Profile);

   when Ada_Emacs =>
      Wisi.Output_Ada_Emacs
        (-Input_File_Name, -Output_File_Root, Generate_Params, Prologue_Context_Clause, Prologue_Declarations,
         Keywords, Tokens, Conflicts, Panic_Recover, Rules, Rule_Count, Action_Count, Profile);

   when Elisp =>
      Wisi.Output_Elisp
        (-Input_File_Name, -Output_File_Root, Generate_Params, Prologue_Context_Clause, Keywords, Tokens, Conflicts,
         Panic_Recover, Rules, Rule_Count, Action_Count);

   end case;

exception
when Syntax_Error =>
   --  error message already output
   Standard.Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);

when E : User_Error =>
   declare
      use Standard.Ada.Command_Line;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);
      Put_Usage;
   end;

when E : WisiToken.Grammar_Error =>
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
