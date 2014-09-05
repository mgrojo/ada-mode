--  Abstract :
--
--  Emacs background process for Ada mode; parse buffer text, return wisi Actions.
--
--  Copyright (C) 2014  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Grammar;
with GNAT.OS_Lib;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
procedure Ada_Mode_Wisi_Parse
is
   Protocol_Version : constant String := "1";
   Version          : constant String := "5.1.5";

   Prompt : constant String := ";;> ";
   --  so the echoed command is an elisp comment

   procedure Usage
   is
   begin
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");

      Put_Line ("Commands: ");

      Put_Line ("NNparse <text_byte_count><text>");
      Put_Line ("  NN includes 'parse <text_byte_count>'");
      Put_Line ("  outputs: elisp forms for wisi actions");

      Put_Line ("04quit");
   end Usage;

   Programmer_Error : exception;

   --  we use GNAT.OS_Lib because it does not buffer input, so it runs
   --  under Emacs nicely; GNAT Text_IO does not return text until
   --  some fairly large buffer is filled.
   Parser : Ada_Grammar.LALR_Parsers.Instance := Ada_Grammar.Create_Parser
     (Text_Feeder => OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (GNAT.OS_Lib.Standin));

   function Get_Command_Length return Integer
   is
      Temp       : aliased String (1 .. 2) := "  ";
      Read_Bytes : constant Integer        := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Temp'Address, 2);
   begin
      if Read_Bytes /= 2 then
         raise Programmer_Error with "2 bytes of command byte count not provided; got" &
           Integer'Image (Read_Bytes) & "'" & Temp & "'";
      end if;
      return Integer'Value (Temp);
   end Get_Command_Length;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 2;
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
   Put_Line ("ada_mode_wisi_parse " & Version & ", protocol version " & Protocol_Version);

   --  read commands from standard_input via GNAT.OS_Lib, send results to standard_output.
   loop
      Put (Prompt); Flush;
      declare
         use Ada.Strings.Fixed;
         Command_Length : constant Integer := Get_Command_Length;
         Command_Line   : aliased String (1 .. Command_Length);

         Read_Bytes : constant Integer := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Command_Line'Address, Command_Length);

         Last : Integer := Index (Source => Command_Line, Pattern => " ");

         function Match (Target : in String) return Boolean
         is begin
            Last := Command_Line'First + Target'Length - 1;
            return Command_Line (Command_Line'First .. Last) = Target;
         end Match;
      begin
         if Read_Bytes /= Command_Length then
            raise Programmer_Error with
              "Read_Bytes" & Integer'Image (Read_Bytes) & " /= Command_Length" & Integer'Image (Command_Length);
         end if;

         if Last = 0 then
            Last := Command_Line'Last;
         else
            Last := Last - 1;
         end if;

         Put_Line (";; " & Command_Line);

         if Match ("parse") then
            --  Args: <byte_count>
            --  Input: <text_line>...
            --  Response:
            --  [wisi action lisp forms]
            --  [error form]
            --  prompt
            declare
               Byte_Count : constant Integer := Get_Integer (Command_Line, Last);
            begin
               OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Parser.Analyzer.Feeder.all).Reset (Byte_Count);
               Parser.Analyzer.Reset;
               Parser.Parse;
            exception
            when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
               Put_Line ("(signal 'wisi-parse-error """ & Ada.Exceptions.Exception_Message (E) & """)");
               --  FIXME: read and discard rest of input text
            end;

         elsif Match ("quit") then
            --  Args:
            exit;
         else
            --  Not elisp comments, so errors are generated and the problem is noticed.
            Put_Line ("bad command: '" & Command_Line & "'");
            Usage;
         end if;
      end;
   end loop;
end Ada_Mode_Wisi_Parse;
