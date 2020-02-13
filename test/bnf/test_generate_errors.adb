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

with Ada.Directories;
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
      use WisiToken.BNF;
      Test : Test_Case renames Test_Case (T);

      Success     : Boolean;
      Return_Code : Integer;
      pragma Unreferenced (Return_Code);

      WY_File : constant String_Access := new String'(Test.Root_Name.all & ".wy");

   begin
      for Alg in Test.Generate_Set.all'Range loop
         if Test.Generate_Set (Alg) then
            declare
               Computed : constant String := Simple_Name (Test.Root_Name.all) & "_" &
                 To_Lower (Generate_Algorithm'Image (Alg)) & ".out";
            begin
               --  We specify Err_To_Out => True, because the error messages normally
               --  go to Standard_Error, but we need to capture them for this test.
               --  That means we don't see any error messages when something
               --  unexpected goes wrong.
               Spawn
                 (Program_Name => Locate_Exec_On_Path ("./wisitoken-bnf-generate.exe").all,
                  Args         =>
                    (1         => new String'("--generate"),
                     2         => new String'(Generate_Algorithm'Image (Alg)),
                     3         => new String'("Ada"), -- actually ignored due to errors
                     4         => WY_File),
                  Output_File  => Computed,
                  Return_Code  => Return_Code,
                  Err_To_Out   => True,
                  Success      => Success);

               --  Note that we don't need dos2unix here; Ada.Text_IO handles that.
               AUnit.Checks.Text_IO.Check_Files ("1", Computed, "../test/bnf/" & Computed & "_good");
            end;
         end if;
      end loop;
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
