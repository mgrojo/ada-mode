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

with AUnit.Checks;
with Ada.Text_IO;
with Ada_Lite_Packrat_Actions; use Ada_Lite_Packrat_Actions;
with Ada_Lite_Packrat_Main;    use Ada_Lite_Packrat_Main;
with WisiToken.Parse.Packrat;
with WisiToken.Syntax_Trees;
package body Test_Packrat_Ada_Lite is

   Parser : WisiToken.Parse.Packrat.Parser;

   procedure Parse_Text
     (Text             : in String;
      Expect_Exception : in Boolean := False)
   is
      use AUnit.Checks;
   begin
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Lexer.Reset_With_String (Text);
      Parser.Parse;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors (Input_File_Name => "<string>");
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Parser.Put_Errors (Input_File_Name => "<string>");
      end if;

      Check ("exception", True, Expect_Exception);
      if Expect_Exception then
         raise;
      end if;
   end Parse_Text;

   ----------
   --  Test procedures

   procedure No_Indirect_Recursion (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      File_Name : constant String := "../wisi/test/ada_lite.input";
   begin
      --  The test is that there is no exception.

      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;
   end No_Indirect_Recursion;

   procedure Indirect_Recursion (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  selected_component causes name to be indirectly recursive.

      Parse_Text ("procedure Parent.Child is begin end;");
      --           1        |10       |20       |30       |40
      --           1         2     3 4    5   6    7  8
   end Indirect_Recursion;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Indirect_Recursion'Access, "No_Indirect_Recursion");
      Register_Routine (T, Indirect_Recursion'Access, "Indirect_Recursion");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_packrat_ada_lite.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Create_Parser
        (Parser,
         Trace     => Trace'Access,
         User_Data => null);
   end Set_Up_Case;

end Test_Packrat_Ada_Lite;
