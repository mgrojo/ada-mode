--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017, 2018 Stephe Leake
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.LR.Parser;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
procedure Gen_Parser_Run
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [-v <integer>] filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);

   User_Data : WisiToken.Syntax_Trees.User_Data_Type;

   procedure Parse (Algorithm : in WisiToken.Parser_Algorithm_Type)
   is
      use all type WisiToken.Token_ID;
      Parser : WisiToken.LR.Parser.Parser;
   begin
      case Algorithm is
      when WisiToken.LALR =>
         Create_Parser (Parser, WisiToken.LALR, Trace'Unchecked_Access);
         Put_Line ("LALR_Parser parse:");

      when WisiToken.LR1 =>
         Create_Parser (Parser, WisiToken.LR1, Trace'Unchecked_Access);
         Put_Line ("LR1_Parser parse:");
      end case;

      Parser.Lexer.Reset_With_File (-File_Name);
      loop
         exit when Parser.Lexer.Find_Next = Descriptor.EOF_ID;
      end loop;

      Parser.Semantic_State.Initialize (Line_Count => Parser.Lexer.Line);

      Parser.Lexer.Reset;
      Parser.Parse;
      Parser.Execute_Actions (User_Data, Compute_Indent => True);

   exception
   when E : WisiToken.Parse_Error | WisiToken.Syntax_Error =>
      Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));

   when Name_Error =>
      Put_Line (-File_Name & " cannot be opened");
      raise WisiToken.User_Error;
   end Parse;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         File_Name := +Argument (1);

      when 3 =>
         if Argument (1) = "-v" then
            WisiToken.Trace_Parse := Integer'Value (Argument (2));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         File_Name := +Argument (3);

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   exception
   when others =>
      Set_Exit_Status (Failure);
      Put_Usage;
      return;
   end;

   Parse (WisiToken.LALR);

   if LR1 then
      New_Line;
      Parse (WisiToken.LR1);
   end if;

exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Parser_Run;
