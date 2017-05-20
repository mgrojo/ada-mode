--  Abstract :
--
--  Emacs background process for Ada mode; parse buffer text, return wisi Actions.
--
--  Copyright (C) 2014, 2017  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Grammar;
with GNAT.OS_Lib;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
with System.Storage_Elements;
procedure Ada_Mode_Wisi_Parse
is
   Protocol_Version : constant String := "1";
   Version          : constant String := "5.1.5";

   Prompt : constant String := ";;> ";
   --  so the echoed command is an elisp comment

   procedure Usage
   is
   begin
      Put_Line ("usage: ada_mode_wisi_parse [-v level]");
      Put_Line ("-v level : enable parse trace output (will screw up Emacs eval)");
      Put_Line ("enters a loop waiting for commands:");
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");
      New_Line;
      Put_Line ("Commands: ");
      New_Line;
      Put_Line ("NNparse ""<buffer-name>"" <text_byte_count><text>");
      Put_Line ("  NN includes 'parse ""<buffer-name>"" <text_byte_count>'");
      Put_Line ("  <buffer-name> used in error messages");
      Put_Line ("  outputs: elisp forms for wisi parser actions or post-parser actions");
      Put_Line ("  wisi parser actions have names encoded as integers; others do not");
      New_Line;
      Put_Line ("NNlex <text_byte_count><text>");
      Put_Line ("  NN includes 'parse <text_byte_count>'");
      Put_Line ("  runs lexer on text, for timing.");

      Put_Line ("04quit");
   end Usage;

   Programmer_Error : exception;

   --  we use GNAT.OS_Lib because it does not buffer input, so it runs
   --  under Emacs nicely; GNAT Text_IO does not return text until
   --  some fairly large buffer is filled.
   Parser : Ada_Grammar.LALR_Parsers.Instance := Ada_Grammar.Create_Parser
     (Text_Feeder => OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (GNAT.OS_Lib.Standin));

   Syntax : constant Ada_Grammar.Analyzers.Syntax := Ada_Grammar.Create_Syntax;
   --  needed to reallocate analyzer buffer
   --  FIXME: fix analyzer.reset to reallocate buffer

   procedure Read_Input (A : System.Address; N : Integer)
   is
      use System.Storage_Elements;

      B         : System.Address := A;
      Remaining : Integer        := N;
      Read      : Integer;
   begin
      --  WORKAROUND: with GNAT GPL 2016, GNAT.OS_Lib.Read does _not_
      --  wait for all N bytes or EOF; it returns as soon as it gets
      --  some bytes.
      --
      --  Note that with this loop we cannot detect EOF; that's ok for
      --  this application.
      loop
         Read := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, B, Remaining);
         Remaining := Remaining - Read;
         exit when Remaining <= 0;
         B := B + Storage_Offset (Read);
      end loop;
   end Read_Input;

   function Get_Command_Length return Integer
   is
      Temp : aliased String (1 .. 2) := "  ";
   begin
      Read_Input (Temp'Address, 2);
      return Integer'Value (Temp);
   exception
   when Constraint_Error =>
      --  From Integer'Value
      raise Programmer_Error with "command byte count not provided; got '" & Temp & "'";
   end Get_Command_Length;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      First : constant Integer := Index
        (Source  => Source,
         Pattern => """",
         From    => Last + 1);
   begin
      Last := Index
        (Source  => Source,
         Pattern => """",
         From    => First + 1);

      if First = 0 or Last = 0 then
         raise Programmer_Error with "no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 2; -- skip leading space
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First);

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when E : others =>
      Put_Line ("bad integer '" & Source (First .. Source'Last) & "'");
      Put_Line ("Exception : " & Exception_Name (E));
      Put_Line (Exception_Message (E));
      raise;
   end Get_Integer;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when 2 =>
         if Argument (1) = "-v" then
            OpenToken.Trace_Parse := Integer'Value (Argument (2));
         else
            raise Programmer_Error with "invalid option: " & Argument (1);
         end if;

      when others =>
         raise Programmer_Error with "invalid option count: " & Integer'Image (Argument_Count);
      end case;
   end;

   Put_Line ("ada_mode_wisi_parse " & Version & ", protocol version " & Protocol_Version);

   --  Read commands and text from standard_input via GNAT.OS_Lib,
   --  send results to standard_output.
   loop
      Put (Prompt); Flush;
      declare
         use Ada.Strings.Fixed;
         Command_Length : constant Integer := Get_Command_Length;
         Command_Line   : aliased String (1 .. Command_Length);
         Last           : Integer;

         function Match (Target : in String) return Boolean
         is begin
            Last := Command_Line'First + Target'Length - 1;
            return Last <= Command_Line'Last and then Command_Line (Command_Line'First .. Last) = Target;
         end Match;
      begin
         Read_Input (Command_Line'Address, Command_Length);

         Put_Line (";; " & Command_Line);

         if Match ("parse") then
            --  Args: <buffer_name> <byte_count>
            --  Input: <text_line>...
            --  Response:
            --  [wisi action lisp forms]
            --  [error form]
            --  prompt
            declare
               Buffer_Name : constant String  := Get_String (Command_Line, Last);
               Byte_Count  : constant Integer := Get_Integer (Command_Line, Last);
               Feeder      : OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
                 OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Parser.Analyzer.Feeder.all);
            begin
               Feeder.Reset (Byte_Count);

               --  Reallocate analyzer buffer to hold entire file;
               --  avoids delays in Emacs send-process EWOULDBLOCK
               --  handling.
               Parser.Analyzer := Ada_Grammar.Analyzers.Initialize
                 (Syntax, Parser.Analyzer.Feeder, Byte_Count, First_Column => 0);

               Parser.Parse;
               --  Set point for wisi-cache-max
               Put_Line ("(goto-char " & OpenToken.Int_Image (Parser.Analyzer.Bounds.End_Pos) & ")");
            exception
            when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
               Put_Line
                 ("(signal 'wisi-parse-error """ & Buffer_Name & ":" &
                    Ada.Exceptions.Exception_Message (E) & """)");
               Feeder.Discard_Rest_Of_Input;
            end;

         elsif Match ("lex") then
            --  Args: <byte_count>
            --  Input: <text_line>...
            --  Response:
            --  prompt
            declare
               Byte_Count  : constant Integer := Get_Integer (Command_Line, Last);
               Feeder      : OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
                 OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Parser.Analyzer.Feeder.all);
            begin
               Feeder.Reset (Byte_Count);

               Parser.Analyzer := Ada_Grammar.Analyzers.Initialize
                 (Syntax, Parser.Analyzer.Feeder, Byte_Count, First_Column => 0);

               loop
                  Parser.Analyzer.Find_Next;
                  exit when Parser.Analyzer.End_Of_Text;
               end loop;

            exception
            when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
               Put_Line ("(signal 'wisi-parse-error """ & Ada.Exceptions.Exception_Message (E) & """)");
               Feeder.Discard_Rest_Of_Input;
            end;

         elsif Match ("quit") then
            --  Args:
            exit;
         else
            --  Not elisp comments, so errors are generated and the problem is noticed.
            Put_Line ("bad command: '" & Command_Line & "'");
            Usage;
         end if;
      exception
      when E : Constraint_Error =>
         --  from get_command_length
         Put_Line ("bad command length: " & Ada.Exceptions.Exception_Message (E));
      end;
   end loop;
exception
when End_Error =>
   null;

when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line (2);
   Put_Line
     ("unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
end Ada_Mode_Wisi_Parse;
