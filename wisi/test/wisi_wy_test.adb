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
with GNAT.OS_Lib;
package body Wisi_WY_Test is

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;
      Test : Test_Case renames Test_Case (T);

      Success : Boolean;

      Make_Target : constant access String := new String'(Base_Name (Test.Root_Name.all) & "-wy.diff");
   begin
      --  run 'make' to create wisi-<root_name>-generate.adb, compile and run it.
      Spawn
        (Program_Name => Locate_Exec_On_Path ("make").all,
         Args         =>
           (1         => String_Access (Make_Target)),
         Success      => Success);

      AUnit.Assertions.Assert
        (Success,
         "spawn or execution of 'make " & Make_Target.all & "' failed");

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
      return new String'("../../wisi/test/wisi_wy_test.adb " & T.Root_Name.all);
   end Name;

end Wisi_WY_Test;
