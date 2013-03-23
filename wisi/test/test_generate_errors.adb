--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013 Stephen Leake
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

with Ada.Directories;
with AUnit.Assertions;
with AUnit.Check;
with GNAT.OS_Lib;
package body Test_Generate_Errors is

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use GNAT.OS_Lib;
      Test : Test_Case renames Test_Case (T);

      Success     : Boolean;
      Return_Code : Integer;

      Wy_File             : constant String_Access := new String'(Test.Root_Name.all & ".wy");
      Computed_Error_File : constant String        :=
        Ada.Directories.Simple_Name (Test.Root_Name.all) & ".out";
      Expected_Error_File : constant String        := Test.Root_Name.all & ".good_out";
   begin
      Spawn
        (Program_Name => Locate_Exec_On_Path ("wisi-generate.exe").all,
         Args         =>
           (1         => Wy_File,
            2         => new String'("test")),
         Output_File  => Computed_Error_File,
         Return_Code  => Return_Code,
         Success      => Success);

      AUnit.Assertions.Assert
        (Success,
         "spawn or execution of 'wisi-generate.exe' " & Wy_File.all & "' failed");

      AUnit.Check.Check_Files ("1", Computed_Error_File, Expected_Error_File);
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
      return new String'("../../wisi/test/Test_Generate_Errors.adb " & T.Root_Name.all);
   end Name;

end Test_Generate_Errors;
