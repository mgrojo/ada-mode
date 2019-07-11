--  Abstract :
--
--  Utilities for AUnit tests with Ada.Text_IO files
--
--  Separate from parent to allow parent to be Preelaborated.
--
--  Copyright (C) 2004 - 2009, 2015 - 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Directories;
with Ada.Text_IO;
package AUnit.Checks.Text_IO is
   pragma Elaborate_Body; --  Ada.Text_IO

   procedure Check is new Gen_Check_Discrete (Ada.Text_IO.Count);

   procedure Check (File : in Ada.Text_IO.File_Type; Expected : in String);
   --  Read a line from File, compare to Expected. Failure message
   --  label is file name and line number.
   --
   --  File must be Open.

   procedure Check
     (Computed : in String;
      Expected : in Ada.Text_IO.File_Type);
   --  Read a line from Expected, compare to Computed. Failure message
   --  label is file name and line number.
   --
   --  Expected must be Open.

   procedure Check_End (File : in Ada.Text_IO.File_Type);
   --  Check that End_Of_File (File) is True.

   type Line_Number_Array_Type is array (Positive range <>) of Ada.Text_IO.Count;

   procedure Check_Files
     (Label         : in String;
      Computed_Name : in String;
      Expected_Name : in String;
      Skip          : in Line_Number_Array_Type := (1 .. 0 => 1));
   --  Compare files named Computed_Name and Expected_Name
   --
   --  Skip lines in Skip; this allows for lines with time stamps that
   --  are not repeatable.

   ----------
   --  For Ada.Directories

   procedure Check_File_Count (Directory : in String; Expected : in Integer);
   --  Check that Directory is an existing directory containing
   --  exactly Expected regular files.

   procedure Check_File_Exists (Name : in String);
   --  Check that Name is an existing regular file.

   procedure Check is new Gen_Check_Discrete (Ada.Directories.File_Kind);

   procedure Delete_If_Exists (Name : in String);
   --  If Name exists, delete it

end AUnit.Checks.Text_IO;
