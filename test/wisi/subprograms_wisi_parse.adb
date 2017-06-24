--  Abstract :
--
--  Emacs background process for wisi test
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
with FastToken.Lexer.Elisp_Process;
with FastToken.Parser.LR.Parser;
with GNAT.OS_Lib;
with Subprograms_Process;
with System.Storage_Elements;
procedure Subprograms_Wisi_Parse
is
   Protocol_Version : constant String := "1";
   Version          : constant String := "0.0";

   Prompt : constant String := ";;> ";

   Protocol_Error : exception;

   procedure Usage
   is
   begin
      Put_Line ("usage: subprograms_wisi_parse [-v level]");
      Put_Line ("-v level : enable parse trace output");
      Put_Line ("enters a loop waiting for commands:");
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");
      New_Line;
      Put_Line ("Commands: ");
      New_Line;
      Put_Line ("NNparse ""<buffer-name>"" <tokens>");
      Put_Line ("  NN includes 'parse ""<buffer-name>"">'");
      Put_Line ("  <buffer-name> used in error messages");
      Put_Line ("  outputs: elisp vectors for parser actions or post-parser actions.");
      Put_Line ("  See wisi-ext-parse-execute for details.");
      New_Line;
      Put_Line ("04quit");
   end Usage;

   Programmer_Error : exception;

   Parser : FastToken.Parser.LR.Parser.Instance := Subprograms_Process.Create_Parser
     (Algorithm   => FastToken.LALR);

   procedure Read_Input (A : System.Address; N : Integer)
   is
      use System.Storage_Elements;

      B         : System.Address := A;
      Remaining : Integer        := N;
      Read      : Integer;
   begin
      --  We use GNAT.OS_Lib because it does not buffer input, so it runs
      --  under Emacs nicely; GNAT Text_IO does not return text until
      --  some fairly large buffer is filled.
      --
      --  With GNAT GPL 2016, GNAT.OS_Lib.Read does _not_ wait for all
      --  N bytes or EOF; it returns as soon as it gets some bytes.
      --  FastToken.Lexer.Elisp_Process handles this for the tokens.
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
      raise Protocol_Error with "invalid command byte count; '" & Temp & "'";
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

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when 2 =>
         if Argument (1) = "-v" then
            FastToken.Trace_Parse := Integer'Value (Argument (2));
         else
            raise Programmer_Error with "invalid option: " & Argument (1);
         end if;

      when others =>
         Usage;
         raise Programmer_Error with "invalid option count: " & Integer'Image (Argument_Count);
      end case;
   end;

   Put_Line ("subprograms_wisi_parse " & Version & ", protocol version " & Protocol_Version);

   --  Read commands and tokens from standard_input via GNAT.OS_Lib,
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
            --  Args: <buffer_name>
            --  Input: <text_line>...
            --  Response:
            --  [wisi action lisp vector]...
            --  [error form]...
            --  prompt
            declare
               Buffer_Name : constant String  := Get_String (Command_Line, Last);
            begin
               Parser.Lexer.Reset (0);
               Parser.Parse;
               --  Set point to match elisp parser
               Put_Line ("(goto-char " & FastToken.Int_Image (Parser.Lexer.Bounds.End_Pos) & ")");
            exception
            when E : FastToken.Parse_Error | FastToken.Syntax_Error =>
               Put_Line
                 ("(signal 'wisi-parse-error """ & Buffer_Name & ":" &
                    Ada.Exceptions.Exception_Message (E) & """)");
               FastToken.Lexer.Elisp_Process.Instance (Parser.Lexer.all).Discard_Rest_Of_Input;
            end;

         elsif Match ("quit") then
            exit;

         else
            Put_Line ("(error ""bad command: '" & Command_Line & "'"")");
         end if;
      exception
      when E : Protocol_Error =>
         --  don't exit the loop; allow debugging bad elisp
         Put_Line ("(protocol error "": " & Ada.Exceptions.Exception_Message (E) & """)");
      end;
   end loop;
exception
when End_Error =>
   null;

when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line (2);
   Put_Line
     ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E) & """)");
end Subprograms_Wisi_Parse;
