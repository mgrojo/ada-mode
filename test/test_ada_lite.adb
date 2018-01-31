--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2018 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Lite;
with GNAT.Traceback.Symbolic;
with WisiToken.LR;
with WisiToken.Semantic_State;
package body Test_Ada_Lite is

   Parser : WisiToken.LR.Instance;

   ----------
   --  Test procedures

   procedure Propagate_Names (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Exceptions;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;
      use Ada_Lite;

      Input_File_Name  : constant String := "../wisi/test/propagate_names.ada_lite";
      Output_File_Name : constant String := "propagate_names.parse";
      Output_File      : File_Type;
      Good_File_Name   : constant String := "../wisi/test/propagate_names.good_parse";
   begin
      WisiToken.Trace_Parse := WisiToken.Detail + 1;

      Create (Output_File, Out_File, Output_File_Name);
      Set_Output (Output_File);

      State.Initialize (Line_Count => 20);
      Parser.Lexer.Reset_With_File (Input_File_Name);
      begin
         Parser.Parse;
      exception
      when E : WisiToken.Syntax_Error =>
         Ada.Text_IO.Put_Line (Input_File_Name & ":" & Exception_Message (E));
         WisiToken.Semantic_State.Put (Input_File_Name, Ada_Lite.State.Parser_Errors, Ada_Lite.Descriptor);

      when E : others =>
         Set_Output (Standard_Output);
         Close (Output_File);
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         AUnit.Assertions.Assert
           (False, "parser raised exception: " & Exception_Name (E) & ": " & Exception_Message (E));
      end;
      Set_Output (Standard_Output);
      Close (Output_File);

      Check_Files ("", Output_File_Name, Good_File_Name);
   end Propagate_Names;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_ada_lite.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Propagate_Names'Access, "Propagate_Names");
   end Register_Tests;

   overriding procedure Tear_Down_Case (Test : in out Test_Case)
   is
      pragma Unreferenced (Test);
   begin
      WisiToken.Trace_Parse := 0;
   end Tear_Down_Case;

begin
   Ada_Lite.Create_Parser (Parser, WisiToken.LALR, Ada_Lite.State'Access);
end Test_Ada_Lite;
