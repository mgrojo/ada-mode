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

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;
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

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;
      Test : Test_Case renames Test_Case (T);

      Success : Boolean;

      Exe : constant String_Access := Locate_Exec_On_Path ("./wisi-generate.exe");

      WY_File : constant String_Access := new String'(Test.Root_Name.all & ".wy");

      Computed_LALR_El_File : constant String := Simple_Name (Test.Root_Name.all) &
        (if Test.LR1 then "-lalr-elisp.el" else "-elisp.el");
      Expected_LALR_El_File : constant String := Test.Root_Name.all &
        (if Test.LR1 then "-lalr-elisp.good_el" else "-elisp.good_el");

      Computed_LR1_El_File : constant String := Simple_Name (Test.Root_Name.all) & "-lr1-elisp.el";
      Expected_LR1_El_File : constant String := Test.Root_Name.all & "-lr1-elisp.good_el";
   begin
      Spawn
        (Program_Name => Exe.all,
         Args         =>
           (1         => new String'("--generator_algorithm"),
            2         => new String'((if Test.LR1 then "LALR_LR1" else "LALR")),
            3         => new String'("--output_language"),
            4         => new String'("Elisp"),
            5         => new String'("--lexer"),
            6         => new String'("Elisp"),
            7         => WY_File),
         Success      => Success);

      AUnit.Assertions.Assert
        (Success,
         "spawn or execution of 'wisi-generate.exe' " & WY_File.all & "' failed");

      Dos2unix (Computed_LALR_El_File);
      if Test.LR1 then
         Dos2unix (Computed_LR1_El_File);
      end if;

      AUnit.Checks.Text_IO.Check_Files ("LALR", Computed_LALR_El_File, Expected_LALR_El_File);
      if Test.LR1 then
         AUnit.Checks.Text_IO.Check_Files ("LR1", Computed_LR1_El_File, Expected_LR1_El_File);
      end if;
   end Run_Test;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_test'Access, "Run_Test");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("wisi_wy_test.adb " & T.Root_Name.all);
   end Name;

end Wisi_WY_Test;
