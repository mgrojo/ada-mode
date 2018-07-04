--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017, 2018 Stephen Leake
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

with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with WisiToken;
package body Wisi_WY_Test is

   procedure Dos2unix (File_Name : in String)
   is
      use GNAT.OS_Lib;
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then
         declare
            Exe : constant String_Access := Locate_Exec_On_Path ("dos2unix.exe");
            Success : Boolean;
            pragma Unreferenced (Success);
         begin
            Spawn
              (Program_Name => Exe.all,
               Args         =>
                 (1         => new String'("-q"),
                  2         => new String'(File_Name)),
               Success      => Success);
         end;
      end if;
   end Dos2unix;

   procedure Generate
     (Root_Name       : in String;
      Generate_Alg    : in Wisi.Valid_Generator_Algorithm;
      Output_Language : in Wisi.Output_Language)
   is
      use AUnit.Checks;
      use GNAT.OS_Lib;
      use Wisi;
      Success : Boolean;
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "generate " & Root_Name);
      end if;

      Spawn
        (Program_Name => Locate_Exec_On_Path ("wisi-generate.exe").all,
         Args         =>
           (1         => new String'("--generator_algorithm"),
            2         => new String'(Generator_Algorithm'Image (Generate_Alg)),
            3         => new String'("--output_language"),
            4         => new String'(Wisi.Output_Language'Image (Output_Language)),
            5         => new String'("--lexer"),
            6         => new String'(if Output_Language = Elisp then "Elisp" else "re2c"),
            7         => new String'("--test_main"),
            8         => new String'("../wisi/test/" & Root_Name & ".wy")),
         Success      => Success);

      Check ("wisi-generate", Success, True);

      if Output_Language /= Elisp then
         Spawn
           (Program_Name => Locate_Exec_On_Path ("re2c").all,
            Args         =>
              (1         => new String'("--no-generation-date"),
               2         => new String'("--debug-output"),
               3         => new String'("--input"),
               4         => new String'("custom"),
               5         => new String'("-W"),
               6         => new String'("-Werror"),
               7         => new String'("--utf-8"),
               8         => new String'("-o"),
               9         => new String'(Root_Name & "_re2c.c"),
               10        => new String'(Root_Name & ".re2c")),
            Success      => Success);

         Check ("re2c", Success, True);
      end if;
   end Generate;

   procedure Diff_Gen
     (Root_Name       : in String;
      Generate_Alg    : in Wisi.Generator_Algorithm;
      Output_Language : in Wisi.Valid_Output_Language)
   is
      use Wisi;

      Computed_1 : constant String := Root_Name &
        (case Output_Language is
         when Wisi.Ada | Ada_Emacs => "_actions.adb",
         when Elisp => "-" & To_Lower (Generator_Algorithm'Image (Generate_Alg)) & "-elisp.el");

      Computed_2 : constant String :=
         Root_Name & "_" & To_Lower (Generator_Algorithm'Image (Generate_Alg)) & "_main.adb";

      Expected_1 : constant String := "../wisi/test/" & Computed_1 & "_good";
      Expected_2 : constant String := "../wisi/test/" & Computed_2 & "_good";

   begin
      Dos2unix (Computed_1);
      AUnit.Checks.Text_IO.Check_Files
        ("", Computed_1, Expected_1,
         Skip => (1 => 2));
      --  Line 2 contains the wisi-generate command line, which includes the
      --  generator algorithm, which is irrelevant for the actions file, but
      --  changes anyway.

      if Output_Language /= Elisp then
         Dos2unix (Computed_2);
         AUnit.Checks.Text_IO.Check_Files ("", Computed_2, Expected_2);
      end if;
   end Diff_Gen;

   procedure Compile
     (Root_Name    : in String;
      Generate_Alg : in Wisi.Generator_Algorithm)
   is
      use AUnit.Checks;
      use GNAT.OS_Lib;
      use Wisi;

      --  We know Output_Language is Ada

      Exe : constant String := Root_Name & "_" &
        To_Lower (Generator_Algorithm'Image (Generate_Alg)) & "_run.exe";

      Success : Boolean;
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "compile " & Root_Name);
      end if;

      Spawn
        (Program_Name => Locate_Exec_On_Path ("make").all,
         Args         =>
           (1         => new String'(Exe)),
         Success      => Success);

      Check ("compile", Success, True);
   end Compile;

   procedure Execute_Parse
     (Root_Name    : in String;
      Generate_Alg : in Wisi.Generator_Algorithm)
   is
      use GNAT.OS_Lib;
      use Wisi;

      --  We know Output_Language is Ada

      Exe : constant String := Root_Name & "_" & To_Lower (Generator_Algorithm'Image (Generate_Alg)) & "_run.exe";

      Output : constant String := Root_Name & "_" &
        To_Lower (Generator_Algorithm'Image (Generate_Alg)) & ".parse";

      Return_Code : Integer;
      pragma Unreferenced (Return_Code);
      Success : Boolean;
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "parse " & Root_Name);
      end if;

      Spawn
        (Program_Name => Locate_Exec_On_Path (Exe).all,
         Args         =>
           (1         => new String'("../wisi/test/" & Root_Name & ".input")),
         Output_File  => Output,
         Return_Code  => Return_Code,
         Success      => Success);

      AUnit.Checks.Check ("parse", Success, True);

      Dos2unix (Output);
      AUnit.Checks.Text_IO.Check_Files ("", Output, "../wisi/test/" & Output & "_good");
   end Execute_Parse;

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Simple_Name : constant String := Ada.Directories.Simple_Name (Test.Root_Name.all);
   begin
      Generate (Simple_Name, Test.Generate, Test.Output_Language);
      Diff_Gen (Simple_Name, Test.Generate, Test.Output_Language);

      case Test.Output_Language is
      when Wisi.Ada =>
         Compile (Simple_Name, Test.Generate);
         Execute_Parse (Simple_Name, Test.Generate);

      when Wisi.Ada_Emacs | Wisi.Elisp =>
         null;
      end case;
   end Run_Test;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_Test'Access, "Run_Test");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("wisi_wy_test.adb-" & T.Root_Name.all);
   end Name;

end Wisi_WY_Test;
