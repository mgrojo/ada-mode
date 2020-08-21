--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 - 2020 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken;
procedure Gen_Tree_Sitter_Parser_Run
is
   procedure Put_Usage
   is
      use Ada.Command_Line;
   begin
      Put (Command_Name);
      for I in 1 .. Argument_Count loop
         Put (' ' & Argument (I));
      end loop;
      New_Line;

      Put_Line (Command_Name & " usage: [-v <integer>] filename");
      Put_Line ("  parse input file");
      Put_Line ("  -v : output trace of states while parsing");
      Put_Line ("  -debug : set Wisitoken.Debug_Mode");
   end Put_Usage;

   function WisiToken_Tree_Sitter_Parse_File
     (Language    : in Interfaces.C.Extensions.void_ptr;
      File_Name   : in Interfaces.C.char_array;
      Trace_Parse : in Interfaces.C.int)
     return Interfaces.C.int
   with Import     => True,
     External_Name => "wisitoken_tree_sitter_parse_file",
     Convention    => C;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Parse
   is
      use all type Interfaces.C.int;
      Status : constant Interfaces.C.int := WisiToken_Tree_Sitter_Parse_File
        (Tree_Sitter_Language, Interfaces.C.To_C (-File_Name), Interfaces.C.int (WisiToken.Trace_Parse));
   begin
      if Status = 0 then
         null;
      else
         Put_Line (Standard_Error, "parse failed");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Parse;

begin
   declare
      use Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         if Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_Parse := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "-debug" then
            Arg_Next             := Arg_Next + 1;
            WisiToken.Debug_Mode := True;

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;
      end loop;

      File_Name := +Argument (Arg_Next);

   exception
   when others =>
      Set_Exit_Status (Failure);
      Put_Usage;
      return;
   end;

   Parse;

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Tree_Sitter_Parser_Run;
