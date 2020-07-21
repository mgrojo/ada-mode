--  Abstract :
--
--  Compare lengths of all .diff files in two directories.
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

with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
procedure Compare_Lengths
is
   Dir_1_Name : constant String := Ada.Command_Line.Argument (1);
   Dir_2_Name : constant String := Ada.Command_Line.Argument (2);

   Dir_1_Search : Search_Type;
   Dir_2_Search : Search_Type;

   Dir_1_Larger : Integer := 0;
   Dir_2_Larger : Integer := 0;
   Equal        : Integer := 0;

begin
   Start_Search
     (Dir_1_Search,
      Directory        => Dir_1_Name,
      Pattern          => "*.diff",
      Filter           =>
        (Directory     => False,
         Ordinary_File => True,
         Special_File  => False));

   Start_Search
     (Dir_2_Search,
      Directory        => Dir_2_Name,
      Pattern          => "*.diff",
      Filter           =>
        (Directory     => False,
         Ordinary_File => True,
         Special_File  => False));

   loop
      exit when not More_Entries (Dir_1_Search) or not More_Entries (Dir_2_Search);
      declare
         Dir_1_Entry : Directory_Entry_Type;
         Dir_2_Entry : Directory_Entry_Type;
         Size_1 : File_Size;
         Size_2 : File_Size;
      begin
         Get_Next_Entry (Dir_1_Search, Dir_1_Entry);
         Get_Next_Entry (Dir_2_Search, Dir_2_Entry);

         if Simple_Name (Dir_1_Entry) /= Simple_Name (Dir_2_Entry) then
            raise Ada.Text_IO.Use_Error;
         end if;

         Ada.Text_IO.Put_Line (Simple_Name (Dir_1_Entry));

         Size_1 := Size (Dir_1_Entry);
         Size_2 := Size (Dir_2_Entry);

         if Size_1 > Size_2 then
            Dir_1_Larger := @ + 1;
         elsif Size_1 < Size_2 then
            Dir_2_Larger := @ + 1;
         else
            Equal := @ + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Put_Line (Dir_1_Name & " larger:" & Dir_1_Larger'Image);
   Ada.Text_IO.Put_Line (Dir_2_Name & " larger:" & Dir_2_Larger'Image);
   Ada.Text_IO.Put_Line ("equal:" & Equal'Image);
end Compare_Lengths;
