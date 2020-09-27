--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 - 2020 Stephen Leake
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
with GNAT.OS_Lib;
package body BNF_WY_Errors_Test is

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AUnit.Checks.Text_IO;

      Test : Test_Case renames Test_Case (T);

      Input_Name           : constant String := "../test/bnf/" & Test.Input.all;
      Expected_Errors_Name : constant String := "../test/bnf/" & Test.Input.all & ".errors_good";
      Exe                  : constant String := "./wisitoken-bnf-generate.exe";

      Error_Output : constant String := Test.Input.all & ".errors";
      Success      : Boolean;
      Return_Code  : Integer;
   begin
      GNAT.OS_Lib.Spawn
        (Program_Name => Exe,
         Args         => (1 => new String'(Input_Name)),
         Output_File  => Error_Output,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => True);

      Check ("wisitoken-bnf-generate success", Success, True);        -- exe was spawned.
      Check ("wisitoken-bnf-generate return status", Return_Code, 1); -- exe reported error.

      Check_Files ("", Error_Output, Expected_Errors_Name);
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
      return new String'("bnf_wy_errors_test.adb " & T.Input.all);
   end Name;

end BNF_WY_Errors_Test;
