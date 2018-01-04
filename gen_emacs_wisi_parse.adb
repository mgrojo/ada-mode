--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2014, 2017 All Rights Reserved.
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
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with System.Storage_Elements;
with WisiToken.Lexer;
with WisiToken.LR;
with WisiToken.Text_IO_Trace;
procedure Gen_Emacs_Wisi_Parse
is
   use WisiToken; -- "+", "-" Unbounded_string

   Protocol_Version : constant String := "1";
   Version          : constant String := "0.0";

   Prompt : constant String := ";;> ";

   Protocol_Error   : exception;
   Programmer_Error : exception;

   procedure Usage
   is
   begin
      Put_Line ("usage: " & Name & "_wisi_parse");
      Put_Line ("enters a loop waiting for commands:");
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");
      New_Line;
      Put_Line ("Commands: ");
      New_Line;
      Put_Line
      ("NNNparse <action> <source_file_name> <line_count> <verbosity> <mckenzie_disable> <mckenzie_cost_limit>" &
       " <mckenzie_check_limit> <source_byte_count> <language-specific params> <source bytes>");
      Put_Line ("  NNN excludes <source bytes>");
      Put_Line ("  <action> is an integer; 0 - navigate, 1 - face, 2 - indent");
      Put_Line ("  <line-count> is integer count of lines in source");
      Put_Line ("  <verbosity> is an integer; set parse trace output level");
      Put_Line ("  <mckenzie_disable> is 0 | 1; 0 = use default, 1 = disable");
      Put_Line ("  <*_limit> is integer; -1 means use default");
      Put_Line ("  outputs: elisp vectors for set-text-property from parser actions or elisp forms for errors.");
      New_Line;
      Put_Line ("NNNnoop <source_byte_count> <source bytes>");
      Put_Line ("  Just receive source; otherwise no operation. NN excludes <source bytes>");
      New_Line;
      Put_Line ("04quit");
   end Usage;

   Trace  : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);
   State  : WisiToken.Semantic_State.Semantic_State (Trace'Access);
   Parser : WisiToken.LR.Instance;

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
      --  With GNAT GPL 2016, GNAT.OS_Lib.Read does _not_ wait for all N
      --  bytes or EOF; it returns as soon as it gets some bytes.
      loop
         Read := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, B, Remaining);
         if Read = 0 then
            --  Pipe closed; probably parent Emacs crashed. Force exit.
            raise Programmer_Error with "input pipe closed";
         end if;
         Remaining := Remaining - Read;
         exit when Remaining <= 0;
         B := B + Storage_Offset (Read);
      end loop;
   end Read_Input;

   function Get_Command_Length return Integer
   is
      Temp : aliased String (1 .. 3) := (others => ' '); -- initialize for error message
   begin
      Read_Input (Temp'Address, Temp'Length);
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
         raise Protocol_Error with Name & "_wisi_parse: no '""' found for string";
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
      First : constant Integer := Last + 2; -- final char of previous item, space
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
   when others =>
      Put_Line ("bad integer '" & Source (First .. Source'Last) & "'");
      raise;
   end Get_Integer;

begin
   Create_Parser (Parser, LALR, State'Unrestricted_Access);

   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when others =>
         Usage;
         raise Programmer_Error with "invalid option count: " & Integer'Image (Argument_Count);
      end case;
   end;

   Put_Line (Name & "_wisi_parse " & Version & ", protocol version " & Protocol_Version);

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
            --  Args: see Usage
            --  Input: <source text>
            --  Response:
            --  [response elisp vector]...
            --  [elisp error form]...
            --  prompt
            declare
               use WisiToken.Wisi_Runtime;
               Parse_Action     : constant Parse_Action_Type := Wisi_Runtime.Parse_Action_Type'Val
                 (Get_Integer (Command_Line, Last));
               Source_File_Name : constant Ada.Strings.Unbounded.Unbounded_String := +Get_String (Command_Line, Last);
               Line_Count       : constant Line_Number_Type := Line_Number_Type (Get_Integer (Command_Line, Last));
               Verbosity        : constant Integer := Get_Integer (Command_Line, Last);
               McKenzie_Disable : constant Integer := Get_Integer (Command_Line, Last);
               Cost_Limit       : constant Integer := Get_Integer (Command_Line, Last);
               Check_Limit      : constant Integer := Get_Integer (Command_Line, Last);
               Byte_Count       : constant Integer := Get_Integer (Command_Line, Last);
               Buffer           : Ada.Strings.Unbounded.String_Access;
            begin
               --  Computing Line_Count in elisp allows parsing in parallel with
               --  sending source text.

               Trace_Parse := Verbosity;

               --  Default Enable_McKenzie_Recover is False if there is no McKenzie
               --  information; don't override that.
               Parser.Enable_McKenzie_Recover :=
                 (if McKenzie_Disable = 0
                  then Parser.Enable_McKenzie_Recover
                  else False);

               Parse_Data.Initialize
                 (Semantic_State   => Parser.Semantic_State,
                  Parse_Action     => Parse_Action,
                  Source_File_Name => -Source_File_Name,
                  Line_Count       => Line_Count,
                  Params           => Command_Line (Last + 2 .. Command_Line'Last));

               if Cost_Limit > 0 then
                  Parser.Table.McKenzie_Param.Cost_Limit := Cost_Limit;
               end if;
               if Check_Limit > 0 then
                  Parser.Table.McKenzie_Param.Check_Limit := Check_Limit;
               end if;

               Buffer := new String (1 .. Byte_Count);
               Read_Input (Buffer (1)'Address, Byte_Count);

               Parser.Lexer.Reset_With_String_Access (Buffer);
               Parser.Parse;
               Put (Parse_Data);
               Put (State.Parser_Errors, Trace.Descriptor.all);
               Put (State.Lexer_Errors);

               Ada.Strings.Unbounded.Free (Buffer);
            exception
            when Parse_Error | Syntax_Error =>
               Parser.Lexer.Discard_Rest_Of_Input;
               Put (State.Parser_Errors, Trace.Descriptor.all);
               Put (State.Lexer_Errors);
               Ada.Strings.Unbounded.Free (Buffer);
               Put_Line ("(parse_error)");
            end;

         elsif Match ("noop") then
            --  Args: <source byte count>
            --  Input: <source text>
            --  Response: prompt
            declare
               Byte_Count : constant Integer                             := Get_Integer (Command_Line, Last);
               Buffer     : constant Ada.Strings.Unbounded.String_Access := new String (1 .. Byte_Count);
               ID         : Token_ID                                     := Invalid_Token_ID;
            begin
               Read_Input (Buffer (1)'Address, Byte_Count);

               Parser.Lexer.Reset_With_String_Access (Buffer);
               loop
                  exit when ID = Parser.Semantic_State.Trace.Descriptor.EOF_ID;
                  ID := Parser.Lexer.Find_Next;
               end loop;
            exception
            when Syntax_Error =>
               Parser.Lexer.Discard_Rest_Of_Input;
            end;

         elsif Match ("quit") then
            exit;

         else
            Put_Line ("(error ""bad command: '" & Command_Line & "'"")");
         end if;
      exception
      when E : Protocol_Error =>
         --  don't exit the loop; allow debugging bad elisp
         Put_Line ("(error ""protocol error "": " & Ada.Exceptions.Exception_Message (E) & """)");
      end;
   end loop;
exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line (2);
   Put_Line
     ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E) & """)");
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Emacs_Wisi_Parse;
