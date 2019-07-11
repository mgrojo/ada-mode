--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004 - 2008, 2015 - 2019 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Assertions;
with Ada.Exceptions;
with Ada.Strings.Fixed;
package body AUnit.Checks.Text_IO is

   procedure Check (File : in Ada.Text_IO.File_Type; Expected : in String)
   is
      Read_Line : String (1 .. 400);
      Last      : Natural;
   begin
      Ada.Text_IO.Get_Line (File, Read_Line, Last);
      Check
        (Label    => Ada.Text_IO.Name (File) & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (File)),
         Computed => Read_Line (1 .. Last),
         Expected => Expected);
   exception
   when Ada.Text_IO.End_Error =>
      AUnit.Assertions.Assert (False, "got End_Error; expecting " & Expected);
   end Check;

   procedure Check
     (Computed : in String;
      Expected : in Ada.Text_IO.File_Type)
   is
      Read_Line : String (1 .. 400);
      Last      : Natural;
   begin
      Ada.Text_IO.Get_Line (Expected, Read_Line, Last);
      Check
        (Label    => Ada.Text_IO.Name (Expected) & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Expected)),
         Computed => Computed,
         Expected => Read_Line (1 .. Last));
   exception
   when Ada.Text_IO.End_Error =>
      AUnit.Assertions.Assert (False, "got '" & Computed & "'; expecting End_Error");
   end Check;

   procedure Check_End (File : in Ada.Text_IO.File_Type)
   is begin
      AUnit.Assertions.Assert (Ada.Text_IO.End_Of_File (File), Ada.Text_IO.Name (File) & " not at end");
   end Check_End;

   function Not_In (Line : in Ada.Text_IO.Count; Skip : in Line_Number_Array_Type) return Boolean
   is
      use type Ada.Text_IO.Count;
   begin
      for I in Skip'Range loop
         if Line = Skip (I) then
            return False;
         end if;
      end loop;
      return True;
   end Not_In;

   procedure Check_Files
     (Label         : in String;
      Computed_Name : in String;
      Expected_Name : in String;
      Skip          : in Line_Number_Array_Type := (1 .. 0 => 1))
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use AUnit.Assertions;
      Computed : File_Type;
      Expected : File_Type;
   begin
      begin
         Open (Computed, In_File, Computed_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Computed_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E) &
              "; current dir '" & Ada.Directories.Current_Directory & "'");
      end;

      begin
         Open (Expected, In_File, Expected_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Expected_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E));
      end;

      begin
         while not (End_Of_File (Expected) or End_Of_File (Computed)) loop
            declare
               Computed_Line : constant String := Get_Line (Computed);
               Expected_Line : constant String := Get_Line (Expected);
            begin
               --  Get_Line advances the line counter beyond the line of interest
               if Not_In (Line (Computed) - 1, Skip) then
                  Check (Computed_Name & ":" & Trim (Positive_Count'Image (Line (Expected)), Both),
                         Computed_Line,
                         Expected_Line);
               end if;
            end;
         end loop;
         Assert (End_Of_File (Computed), Label & " " & Computed_Name & " longer than " & Expected_Name);
         Assert (End_Of_File (Expected), Label & " " & Expected_Name & " longer than " & Computed_Name);

         Close (Computed);
         Close (Expected);
      exception
      when AUnit.Assertions.Assertion_Error =>
         Close (Computed);
         Close (Expected);
         raise;
      end;
   end Check_Files;

   procedure Check_File_Count (Directory : in String; Expected : in Integer)
   is
      use Ada.Directories;
      Computed : Integer := 0;

      procedure Process_Entry (Dir_Entry : Directory_Entry_Type)
      is
         pragma Unreferenced (Dir_Entry);
      begin
         Computed := Computed + 1;
      end Process_Entry;
   begin
      Check ("'" & Directory & "' exists", Exists (Directory), True);
      Check ("'" & Directory & "' is a directory", Kind (Directory), Ada.Directories.Directory);

      Search
        (Directory,
         Pattern => "*",
         Filter  => (Ordinary_File => True, others => False),
         Process => Process_Entry'Access);

      Check ("'" & Directory & "' file count", Computed, Expected);
   end Check_File_Count;

   procedure Check_File_Exists (Name : in String)
   is
      Result : Boolean;
   begin
      begin
         Result := Ada.Directories.Exists (Name);
      exception
      when Ada.Text_IO.Name_Error =>
         Result := False;
      end;
      Check ("'" & Name & "' exists", Result, True);
   end Check_File_Exists;

   procedure Delete_If_Exists (Name : in String)
   is begin
      if Ada.Directories.Exists (Name) then
         Ada.Directories.Delete_File (Name);
      end if;
   end Delete_If_Exists;

end AUnit.Checks.Text_IO;
