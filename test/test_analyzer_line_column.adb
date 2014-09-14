--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2014 Stephen Leake.  All Rights Reserved.
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

with AUnit.Check;
with Ada.Directories;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with OpenToken.Recognizer.Bracketed_Comment;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Identifier;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Token.Enumerated.Analyzer;
package body Test_Analyzer_Line_Column is

   --  A simple grammar for testing Line, Column

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,

      --  terminals
      Comment_ID,
      COLON_EQUAL_ID,
      SEMICOLON_ID,
      IDENTIFIER_ID,
      EOF_ID,

      --  non-terminals
      assignment_list_ID,
      assignment_ID,
      opentoken_accept_ID);

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Comment_ID, EOF_ID, Token_IDs'Image);
   package Analyzers is new Tokens_Pkg.Analyzer;

   Syntax : constant Analyzers.Syntax :=
     (
      --  terminals
      Whitespace_ID     => Analyzers.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),
      Comment_ID        => Analyzers.Get (OpenToken.Recognizer.Bracketed_Comment.Get
        (Comment_Opener => "/*",
         Comment_Closer => "*/",
         Reportable     => True,
         Nested         => False)),
      COLON_EQUAL_ID    => Analyzers.Get (OpenToken.Recognizer.Separator.Get (":=")),
      SEMICOLON_ID      => Analyzers.Get (OpenToken.Recognizer.Separator.Get (";")),
      IDENTIFIER_ID     => Analyzers.Get
        (Recognizer     => OpenToken.Recognizer.Identifier.Get
          (Start_Chars  => Ada.Strings.Maps.Constants.Alphanumeric_Set,
           Body_Chars   => Ada.Strings.Maps.Constants.Alphanumeric_Set)),
      EOF_ID            => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get));

   Analyzer : constant Analyzers.Handle := Analyzers.Initialize (Syntax);

   ----------
   --  Test procedures

   Input_File : aliased Ada.Text_IO.File_Type;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Check;
      Input_File_Name : constant String := "test_analyzer_line_column.in";
   begin
      --  Create input file
      if Ada.Directories.Exists (Input_File_Name) then
         Ada.Directories.Delete_File (Input_File_Name);
      end if;

      declare
         use Ada.Text_IO;
      begin
         Create (Input_File, Out_File, Input_File_Name);
         Put_Line (Input_File, "A := B;");
         Put_Line (Input_File, "Foo := Bar;");
         Put_Line (Input_File, "/* comment line 1");
         Put_Line (Input_File, "   line 2 */ Cat := D;");
         --                     1234567890123456789012
         Close (Input_File);

         Open (Input_File, In_File, Input_File_Name);
      end;

      Analyzer.Set_Text_Feeder (OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access));

      for I in 1 .. 13 loop
         Analyzer.Find_Next;
         case I is
         when 1 =>
            --  A
            Check ("1.Line  ", Analyzer.Line, 1);
            Check ("1.Column", Analyzer.Column, 1);

         when 2 =>
            --  :=
            Check ("2.Line  ", Analyzer.Line, 1);
            Check ("2.Column", Analyzer.Column, 3);

         when 3 =>
            --  B
            Check ("3.Line  ", Analyzer.Line, 1);
            Check ("3.Column", Analyzer.Column, 6);

         when 4 =>
            --  ;
            Check ("4.Line  ", Analyzer.Line, 1);
            Check ("4.Column", Analyzer.Column, 7);

         when 5 =>
            --  Foo
            Check ("5.Line  ", Analyzer.Line, 2);
            Check ("5.Column", Analyzer.Column, 1);

         when 6 =>
            --  :=
            Check ("6.Line  ", Analyzer.Line, 2);
            Check ("6.Column", Analyzer.Column, 5);

         when 7 =>
            --  Bar
            Check ("7.Line  ", Analyzer.Line, 2);
            Check ("7.Column", Analyzer.Column, 8);

         when 8 =>
            --  ;
            Check ("8.Line  ", Analyzer.Line, 2);
            Check ("8.Column", Analyzer.Column, 11);

         when 9 =>
            --  comment
            Check ("9.Line  ", Analyzer.Line, 3);
            Check ("9.Column", Analyzer.Column, 1);

         when 10 =>
            --  Cat
            Check ("10.Line  ", Analyzer.Line, 4);
            Check ("10.Column", Analyzer.Column, 14);

         when 11 =>
            --  :=
            Check ("11.Line  ", Analyzer.Line, 4);
            Check ("11.Column", Analyzer.Column, 18);

         when 12 =>
            --  D
            Check ("12.Line  ", Analyzer.Line, 4);
            Check ("12.Column", Analyzer.Column, 21);

         when 13 =>
            --  ;
            Check ("13.Line  ", Analyzer.Line, 4);
            Check ("13.Column", Analyzer.Column, 22);

         end case;
      end loop;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_8.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Analyzer_Line_Column;
