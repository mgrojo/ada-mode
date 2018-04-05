--  Abstract :
--
--  Parser for Wisi grammar files, producing Ada or Elisp source
--  files for a parser.
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Wisi.Output_Ada;
with Wisi.Output_Ada_Emacs;
with Wisi.Output_Elisp;
with Wisi.Output_Elisp_Common;
with WisiToken.LR.Parser;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Grammar_Runtime;
with Wisi_Grammar;
procedure Wisi.Generate
is
   use all type Standard.Ada.Containers.Count_Type;

   procedure Put_Usage
   is
      use Standard.Ada.Text_IO;
   begin
      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line (Standard_Error, "wisi-generate [options] {wisi grammar file}");
      Put_Line (Standard_Error, "version 0.00 - experimental");
      Put_Line (Standard_Error, "Generate source code implementing a parser for the grammar.");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "The following grammar file directives control parser generation:");
      Put_Line (Standard_Error, "%first_state_index <n> - default 0");
      Put_Line (Standard_Error, "%first_parser_label <n> -  default 0");
      Put_Line (Standard_Error, "%parser_algorithm {LALR | LR1 | LALR_LR1}");
      Put_Line (Standard_Error, "   LALR_LR1 generates both parsers; choice is made at parser run-time.");
      Put_Line (Standard_Error, "%output_language' {Ada | Ada_Emacs | Elisp}");
      Put_Line (Standard_Error, "%lexer {re2c | Elisp | Regexp}");
      Put_Line (Standard_Error, "%interface {Process | Module}");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "Interface is only valid with Ada_Emacs:");
      Put_Line (Standard_Error, "   Ada_Emacs, Process is for an external subprocess communicating with Emacs.");
      Put_Line (Standard_Error, "   Ada_Emacs, Module  is for a dynamically loaded Emacs module.");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "options are:");
      Put_Line (Standard_Error, "  -v level: sets verbosity (default 0):");
      Put_Line (Standard_Error, "     0 - only error messages to standard error");
      Put_Line (Standard_Error, "     1 - add compiled grammar output to standard out");
      Put_Line (Standard_Error, "     2 - add diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line (Standard_Error, "  --enum; declare enumeration token type");
      Put_Line (Standard_Error, "  --suffix <string>; appended to grammar file name");
      Put_Line (Standard_Error,
                "  --<directive_name> <value>; same as directive in grammar file; override grammar file");

   end Put_Usage;

   Language_Name      : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Output_File_Root   : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Declare_Enum       : Boolean := False;
   Suffix             : Standard.Ada.Strings.Unbounded.Unbounded_String;

   Trace              : aliased WisiToken.Text_IO_Trace.Trace (Wisi_Grammar.Descriptor'Access);
   Grammar_Parse_Data : aliased WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
   Grammar_Parser     : WisiToken.LR.Parser.Parser;

   procedure Use_Input_File (File_Name : in String)
   is
      use Standard.Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
   begin
      Grammar_Parse_Data.Input_File_Name := +File_Name;

      Output_File_Root := +Standard.Ada.Directories.Base_Name (File_Name) & Suffix;

      Wisi_Grammar.Create_Parser
        (Grammar_Parser, WisiToken.LALR, Trace'Unchecked_Access,
         Language_Fixes => null,
         User_Data      => Grammar_Parse_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (File_Name);

      declare
         Language_Name_Dir   : constant Integer := Standard.Ada.Strings.Fixed.Index
           (File_Name, Standard.Ada.Strings.Maps.To_Set ("/\"), Going => Standard.Ada.Strings.Backward);
         Language_Name_Ext   : constant Integer := Standard.Ada.Strings.Fixed.Index (File_Name, ".wy");
      begin
         Language_Name := +Wisi.Output_Elisp_Common.Elisp_Name_To_Ada
           (File_Name
              ((if Language_Name_Dir = 0
                then File_Name'First
                else Language_Name_Dir + 1) ..
                 Language_Name_Ext - 1),
            Append_ID => False,
            Trim      => 0);
      end;
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

         --   -v first, then alphabetical

         if Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_Generate := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--enum" then
            Declare_Enum := True;
            Arg_Next     := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--first_state_index" then
            Arg_Next  := Arg_Next + 1;
            Grammar_Parse_Data.Generate_Params.First_State_Index := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--first_parser_label" then
            Arg_Next  := Arg_Next + 1;
            Grammar_Parse_Data.Generate_Params.First_Parser_Label := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--interface" then
            Arg_Next  := Arg_Next + 1;
            begin
               Grammar_Parse_Data.Generate_Params.Interface_Kind := Interface_Type'Value (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for interface: '" & Argument (Arg_Next) & ";";
            end;
         elsif Argument (Arg_Next) = "--lexer" then
            Arg_Next  := Arg_Next + 1;
            begin
               Grammar_Parse_Data.Generate_Params.Lexer := To_Lexer (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for lexer: '" & Argument (Arg_Next) & ";";
            end;

         elsif Argument (Arg_Next) = "--output_language" then
            Arg_Next  := Arg_Next + 1;
            begin
               Grammar_Parse_Data.Generate_Params.Output_Language := Output_Language_Type'Value (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for output_language: '" & Argument (Arg_Next) & ";";
            end;

         elsif Argument (Arg_Next) = "--parser_algorithm" then
            Arg_Next  := Arg_Next + 1;
            begin
               Grammar_Parse_Data.Generate_Params.Parser_Algorithm := Parser_Algorithm_Type'Value
                 (Argument (Arg_Next));
               Arg_Next := Arg_Next + 1;
            exception
            when Constraint_Error =>
               raise User_Error with "invalid value for parser_algorithm: '" & Argument (Arg_Next) & ";";
            end;

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

   begin
      Grammar_Parser.Parse;
      Grammar_Parser.Execute_Actions;
      if Grammar_Parser.Any_Errors then
         raise WisiToken.Syntax_Error;
      end if;
   exception
   when WisiToken.Syntax_Error =>
      Grammar_Parser.Put_Errors (-Grammar_Parse_Data.Input_File_Name);
      raise;
   end;

   if Grammar_Parse_Data.Rule_Count = 0 or Grammar_Parse_Data.Tokens.Rules.Length = 0 then
      raise WisiToken.Grammar_Error with "no rules";
   end if;

   case Grammar_Parse_Data.Generate_Params.Output_Language is
   when None =>
      raise Programmer_Error; -- checked in wisi.declarations

   when Ada =>
      Wisi.Output_Ada
        (-Grammar_Parse_Data.Input_File_Name, -Output_File_Root, Grammar_Parse_Data.Generate_Params,
         Grammar_Parse_Data.Prologues, Grammar_Parse_Data.Tokens, Grammar_Parse_Data.Conflicts,
         Grammar_Parse_Data.McKenzie_Recover, Grammar_Parse_Data.Elisp_Names, Grammar_Parse_Data.Rule_Count,
         Grammar_Parse_Data.Action_Count, Grammar_Parse_Data.Check_Count, Declare_Enum);

   when Ada_Emacs =>
      Wisi.Output_Ada_Emacs
        (-Grammar_Parse_Data.Input_File_Name, -Output_File_Root, -Language_Name, Grammar_Parse_Data.Generate_Params,
         Grammar_Parse_Data.Prologues, Grammar_Parse_Data.Tokens, Grammar_Parse_Data.Conflicts,
         Grammar_Parse_Data.McKenzie_Recover, Grammar_Parse_Data.Elisp_Names, Grammar_Parse_Data.Rule_Count,
         Grammar_Parse_Data.Action_Count, Grammar_Parse_Data.Check_Count, Declare_Enum);

   when Elisp =>
      --  The Elisp parser does not support any error recover algorithms,
      --  thus no semantic checks.
      Wisi.Output_Elisp
        (-Grammar_Parse_Data.Input_File_Name, -Output_File_Root, Grammar_Parse_Data.Generate_Params,
         Grammar_Parse_Data.Prologues, Grammar_Parse_Data.Tokens, Grammar_Parse_Data.Conflicts,
         Grammar_Parse_Data.Rule_Count, Grammar_Parse_Data.Action_Count);

   end case;

exception
when WisiToken.Syntax_Error =>
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
   --  error message not already output
   declare
      use Standard.Ada.Command_Line;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);
   end;

when E :  others =>
   --  IMPROVEME: for some exceptions, Error message already output via wisi.utils.Put_Error
   declare
      use Standard.Ada.Text_IO;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;

end Wisi.Generate;
