--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 Stephen Leake
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

with Ada.Directories;
with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;
package body Test_Generate_Errors is

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;
      Test : Test_Case renames Test_Case (T);

      Success     : Boolean;
      Return_Code : Integer;

      WY_File : constant String_Access := new String'(Test.Root_Name.all & ".wy");

      Computed_LALR_File : constant String := Simple_Name (Test.Root_Name.all) & "-lalr.out";
      Expected_LALR_File : constant String := Test.Root_Name.all & "-lalr.good_out";

      --  LALR reports terminals that are only used in unused
      --  productions as unused; lr1 is not that smart.
      Computed_LR1_File : constant String := Simple_Name (Test.Root_Name.all) & "-lr1.out";
      Expected_LR1_File : constant String := Test.Root_Name.all & "-lr1.good_out";

   begin
      Spawn
        (Program_Name => Locate_Exec_On_Path ("./wisi-generate.exe").all,
         Args         =>
           (1         => new String'("--parser_algorithm"),
            2         => new String'("LALR"),
            3         => WY_File),
         Output_File  => Computed_LALR_File,
         Return_Code  => Return_Code,
         Success      => Success);

      AUnit.Assertions.Assert
        (Success,
         "spawn or execution of 'wisi-generate.exe' LALR " & WY_File.all & "' failed");

      AUnit.Checks.Text_IO.Check_Files ("1", Computed_LALR_File, Expected_LALR_File);

      Spawn
        (Program_Name => Locate_Exec_On_Path ("./wisi-generate.exe").all,
         Args         =>
           (1         => new String'("--parser_algorithm"),
            2         => new String'("LR1"),
            3         => WY_File),
         Output_File  => Computed_LR1_File,
         Return_Code  => Return_Code,
         Success      => Success);

      AUnit.Assertions.Assert
        (Success,
         "spawn or execution of 'wisi-generate.exe' LR1 " & WY_File.all & "' failed");

      AUnit.Checks.Text_IO.Check_Files ("1", Computed_LR1_File, Expected_LR1_File);

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
      return new String'(GNAT.Source_Info.File & " " & T.Root_Name.all);
   end Name;

end Test_Generate_Errors;
