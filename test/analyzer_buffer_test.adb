-------------------------------------------------------------------------------
--
--  Copyright (C) 2014 Stephen Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Check;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with GNAT.OS_Lib;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Identifier;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
package body Analyzer_Buffer_Test is

   type Token_ID is (Identifier_ID, Whitespace);

   package Master_Example_Token is new OpenToken.Token.Enumerated
     (Token_ID, Token_ID'First, Token_ID'Last, Token_ID'Image);
   package Tokenizer is new Master_Example_Token.Analyzer;

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_ID);

   Syntax : constant Tokenizer.Syntax :=
     (Identifier_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Identifier.Get
           (Start_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set,
            Body_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
      Whitespace        => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer      : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, null, Buffer_Size => 10);

   procedure Step
     (Label           : in String;
      Expected_ID     : in Token_ID;
      Expected_Lexeme : in String)
   is
      use Tokenizer;
      use AUnit.Check;
   begin
      Analyzer.Find_Next (Look_Ahead => False);

      Check (Label & ".ID", Analyzer.ID, Expected_ID);
      Check (Label & ".lexeme", Analyzer.Lexeme, Expected_Lexeme);
   end Step;

   ----------
   --  Test procedures

   procedure Nominal_String (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Text : constant String := "a23456 b90123";
   begin
      --  Verify that the analyzer tokenizes properly when tokens
      --  cross the internal buffer end, when reading from a
      --  String text feeder.

      OpenToken.Text_Feeder.String.Set (String_Feeder, Text);
      Analyzer.Feeder := String_Feeder'Access;
      Analyzer.Reset;

      Step ("1", Identifier_ID, "a23456");
      Step ("2", Identifier_ID, "b90123");

   end Nominal_String;

   procedure Nominal_Counted_Gnat (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Directories;
      use Ada.Text_IO;
      use GNAT.OS_Lib;

      Text      : constant String := "a23456 b90123";
      File_Name : constant String := "analyzer_buffer_test.text";
      Text_File : File_Type;
   begin
      --  Verify that the analyzer tokenizes properly when tokens
      --  cross the internal buffer end, when reading from a
      --  Counted_GNAT_OS_Lib text feeder.
      --
      --  There used to be a bug in Counted_GNAT_OS_Lib.End_Of_File
      --  that caused this to fail

      if Exists (File_Name) then
         Delete_File (File_Name);
      end if;
      Create (Text_File, Out_File, File_Name);
      Put (Text_File, Text);
      Close (Text_File);

      Analyzer.Feeder := OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create
        (Open_Read (File_Name, Binary));

      OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Analyzer.Feeder.all).Reset (Text'Length);
      Analyzer.Reset;

      Step ("1", Identifier_ID, "a23456");
      Step ("2", Identifier_ID, "b90123");

   end Nominal_Counted_Gnat;

   procedure Buffer_Full (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Check;

      Text : constant String := "a2345678901";
   begin
      --  Verify that the analyzer raises a nice error message when a
      --  token is bigger than the internal buffer.

      OpenToken.Text_Feeder.String.Set (String_Feeder, Text);
      Analyzer.Feeder := String_Feeder'Access;
      Analyzer.Reset;

      Analyzer.Find_Next (Look_Ahead => False);
      AUnit.Assertions.Assert (False, "did not get exception");
   exception
   when E : others =>
      Check ("exception message", Ada.Exceptions.Exception_Message (E), "token larger than buffer size of 10");
   end Buffer_Full;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal_String'Access, "Nominal_String");
      Register_Routine (T, Nominal_Counted_Gnat'Access, "Nominal_Counted_Gnat");
      Register_Routine (T, Buffer_Full'Access, "Buffer_Full");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/analyzer_buffer_test.adb");
   end Name;

end Analyzer_Buffer_Test;
