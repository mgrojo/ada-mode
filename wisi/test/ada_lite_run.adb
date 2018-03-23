--  Abstract:
--
--  Only used for profiling the parser.
--
--  We can't use Gen_Parser_Run for this, because it declares State
--  internally.
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
with Ada_Lite;
with GNAT.Traceback.Symbolic;
with WisiToken.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.LR.Parser;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees;
procedure Ada_Lite_Run
is
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [-v <trace_parse> <trace_mckenzie] <repeat count> filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
   end Put_Usage;

   File_Name    : Ada.Strings.Unbounded.Unbounded_String;
   Repeat_Count : Integer;

   User_Data : WisiToken.Syntax_Trees.User_Data_Type;

   procedure Parse
   is
      Parser : WisiToken.LR.Parser.Parser;
   begin
      Ada_Lite.Create_Parser
        (Parser, WisiToken.LALR, Ada_Lite.Trace'Unchecked_Access,
         WisiToken.LR.McKenzie_Recover.Ada_Lite.Semantic_Check_Fixes'Access);
      Parser.Lexer.Reset_With_File (-File_Name);

      for I in 1 .. Repeat_Count loop
         Parser.Semantic_State.Initialize (Line_Count => 100); -- big enough

         Parser.Lexer.Reset;
         Parser.Parse;
         Parser.Execute_Actions (User_Data, Compute_Indent => True);
      end loop;
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
      when 2 =>
         Repeat_Count := Integer'Value (Argument (1));
         File_Name := +Argument (2);

      when 5 =>
         if Argument (1) = "-v" then
            WisiToken.Trace_Parse    := Integer'Value (Argument (2));
            WisiToken.Trace_McKenzie := Integer'Value (Argument (3));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Repeat_Count := Integer'Value (Argument (4));
         File_Name := +Argument (5);

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

   Parse;


exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Ada_Lite_Run;
