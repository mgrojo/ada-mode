--  Abstract:
--
--  See spec
--
--  Copyright (C) 2015 Stephen Leake
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

with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with Wisi.Put_Module_Action_Line;
package body Wisi_Module_Action_Test is

   procedure Delete (Name : in String)
   is
      use Ada.Directories;
   begin
      if Exists (Name) then
         Delete_File (Name);
      end if;
   end Delete;

   ----------
   --  Test procedures

   procedure Nominal (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test);
      use Ada.Text_IO;
      use Wisi;
      Computed_File_Name : constant String := "wisi_module_action_test.out";
      Computed_File      : File_Type;
      Expected_File_Name : constant String := "../../wisi/test/wisi_module_action_test.good_out";
   begin
      Delete (Computed_File_Name);
      Create (Computed_File, Out_File, Computed_File_Name);
      Set_Output (Computed_File);

      --  First line; ignored
      Put_Module_Action_Line ("(progn");

      Put_Module_Action_Line ("(wisi_test_action 1)");
      Put_Module_Action_Line ("(wisi-containing-action 2 3)");
      Put_Module_Action_Line ("(wisi-statement-action [1 function 2 other])");
      Put_Module_Action_Line ("(wisi-face-action [2 font-lock-function-name-face]))");
      Put_Module_Action_Line ("(wisi-motion-action [1 5 [6 block-middle EXCEPTION block-middle WHEN]])");
      Put_Module_Action_Line ("(wisi-motion-action [1 3 [5 statement-other ELSIF block-middle THEN] 6 8])");

      --  Last line; trailing closing paren ignored.
      Put_Module_Action_Line ("(wisi-face-action [2 font-lock-function-name-face 8 font-lock-function-name-face]))");

      Set_Output (Standard_Output);
      Close (Computed_File);

      AUnit.Checks.Text_IO.Check_Files ("1", Computed_File_Name, Expected_File_Name);
   end Nominal;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");

   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../wisi/test/wisi_module_action_test.adb");
   end Name;

end Wisi_Module_Action_Test;
