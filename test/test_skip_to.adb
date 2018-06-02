--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with Skip_To_Grammar_Actions;
with Skip_To_Grammar_Main;
with Test_Skip_To_Aux;
with WisiToken.LR.Parser_No_Recover;
with WisiToken.Text_IO_Trace;
package body Test_Skip_To is

   Trace  : aliased WisiToken.Text_IO_Trace.Trace (Skip_To_Grammar_Actions.Descriptor'Access);
   Parser : WisiToken.LR.Parser_No_Recover.Parser;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      File_Name : constant String := "../wisi/test/skip_to_grammar.input";
   begin
      Test_Skip_To_Aux.Enable := True;

      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;
   exception
   when E : WisiToken.Syntax_Error | WisiToken.Parse_Error =>
      declare
         use Ada.Exceptions;
      begin
         Ada.Text_IO.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         Parser.Put_Errors (File_Name);
      end;
      AUnit.Assertions.Assert (False, "exception");
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_skip_to.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

begin
   Skip_To_Grammar_Main.Create_Parser (Parser, WisiToken.LALR, Trace'Access, null);
end Test_Skip_To;
