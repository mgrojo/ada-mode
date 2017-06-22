--  Abstract:
--
--  See spec
--
--  Copyright (C) 2014, 2015, 2017 Stephen Leake
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

with AUnit.Checks;
with Ada.Directories;
with GNAT.OS_Lib;
with WisiToken.Text_Feeder.Counted_GNAT_OS_Lib;
package body Counted_GNAT_OS_Lib_Test is

   ----------
   --  Test procedures

   procedure Discard (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Directories;
      use AUnit.Checks;
      use GNAT.OS_Lib;
      use WisiToken.Text_Feeder.Counted_GNAT_OS_Lib;

      File_Text    : constant String := "a23456 b90123";
      Command_Text : constant String := "04quit";
      File_Name    : constant String := "counted_gnat_os_lib_test.in";
      OS_Lib_File  : File_Descriptor;
      Junk         : Integer;
      pragma Unreferenced (Junk);
      Feeder       : Instance;

      Command        : aliased String (1 .. 20);
      Command_Length : Integer;

      procedure Test_Get
        (Label         : in String;
         Expected_Text : in String)
      is
         Text     : String (1 .. Expected_Text'Length);
         Text_End : Integer;
      begin
         Get (Feeder, Text, Text_End);
         Check (Label & ".end", Text'Last, Text_End);
         Check (Label & ".text", Text (1 .. Text_End), Expected_Text);
      end Test_Get;

   begin
      --  Verify that Counted_GNAT_OS_Lib.Discard_Rest_Of_File works properly.

      if Exists (File_Name) then
         Delete_File (File_Name);
      end if;

      --  We create the file with GNAT.OS_Lib, not Ada.Text_IO, so we
      --  don't get extra EOL and EOF chars in the file.
      OS_Lib_File := Create_File (File_Name, Binary);

      --  Input file contains File_Text to be read via Get, followed
      --  by Command_Text to be read by GNAT_OS_Lib.Read; testing bug
      --  in ada_mode_wisi_parse.adb
      Junk := Write (OS_Lib_File, File_Text'Address, File_Text'Length);
      Junk := Write (OS_Lib_File, Command_Text'Address, Command_Text'Length);
      Close (OS_Lib_File);

      OS_Lib_File := Open_Read (File_Name, Binary);

      Initialize (Feeder, OS_Lib_File);

      Feeder.Reset (File_Text'Length);

      Test_Get ("1", "a23456");

      Feeder.Discard_Rest_Of_Input;

      Command_Length := Read (OS_Lib_File, Command'Address, Command'Length);
      Check ("2.text", Command (1 .. Command_Length), Command_Text);
      Check ("2", Command_Length, Command_Text'Length);

   end Discard;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Discard'Access, "Discard");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../Test/counted_gnat_os_lib_test.adb");
   end Name;

end Counted_GNAT_OS_Lib_Test;
