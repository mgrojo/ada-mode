--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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
with AUnit.Checks;
with Ada.Exceptions;
with Ada.Text_IO;
with Character_Literal;
with WisiToken.AUnit;
with WisiToken.Parser.LR.Parser;
package body Test_Character_Literal is

   Parser : WisiToken.Parser.LR.Parser.Instance := Character_Literal.Create_Parser (WisiToken.LALR);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Exceptions;
      use AUnit.Checks;
      use Character_Literal;

      File_Name : constant String := "../wisi/test/character_literal.input";
   begin
      WisiToken.Trace_Parse := Test.Debug;

      Parser.Lexer.Reset_With_File (File_Name);
      Parser.Parse;

      Check ("character_literal_count", Character_Literal_Count, 6);
      Check ("string_literal_count", String_Literal_Count, 2);
   exception
   when AUnit.Assertions.Assertion_Error =>
      raise;

   when E : WisiToken.Syntax_Error =>
      Ada.Text_IO.Put_Line (File_Name & ":" & Exception_Message (E));
      AUnit.Assertions.Assert (False, "syntax error");

   when E : others =>
      AUnit.Assertions.Assert (False, "parser raised exception: " & Exception_Name (E) & ": " & Exception_Message (E));
   end Nominal;

   procedure Character_Position (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Tst : Test_Case renames Test_Case (T);
      use WisiToken.AUnit;

      procedure Test (Label : in String; Input : in String; Expected_Bounds : in WisiToken.Buffer_Region)
      is begin
         Parser.Lexer.Reset_With_String (Input);
         Parser.Parse;

         Check (Label, Character_Literal.Bounds, Expected_Bounds);
      end Test;

   begin
      WisiToken.Trace_Parse := Tst.Debug;

      Test ("1", "object'attribute;", (8, 16));
            --    |1       |10       |20

      Test ("2", "objectΠ'attributeΕ;", (9, 18));
            --    |1        |10       |20

   exception
   when E : WisiToken.Syntax_Error =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      AUnit.Assertions.Assert (False, "syntax error");
   end Character_Position;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_character_literal.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Character_Position'Access, "Character_Position");
   end Register_Tests;

end Test_Character_Literal;
