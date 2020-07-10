--  Abstract :
--
--  Sum lengths of all .diff files in current directory.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
procedure Sum_Diff_Lengths
is
   File_Count : Integer   := 0;
   Zero_Count : Integer   := 0;
   Total_Size : File_Size := 0;

   procedure Process_File (Item : in Directory_Entry_Type)
   is
      File_Name : constant String := Full_Name (Item);
   begin
      if Extension (File_Name) = "diff" then
         File_Count := @ + 1;
         Total_Size := @ + Size (File_Name);
         if Size (File_Name) = 0 then
            Zero_Count := @ + 1;
         end if;
      end if;
   end Process_File;

begin
   Search
     (Directory        => Current_Directory,
      Pattern          => "*",
      Filter           =>
        (Directory     => False,
         Ordinary_File => True,
         Special_File  => False),
      Process          => Process_File'Access);

   Ada.Text_IO.Put_Line
     ("file_count:" & File_Count'Image &
        " total size:" & Total_Size'Image &
        " zero size:" & Zero_Count'Image);
end Sum_Diff_Lengths;
