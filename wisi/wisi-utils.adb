--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Directories;
with Ada.Strings.Fixed;
package body Wisi.Utils is

   function Skip_Comments (File : in Standard.Ada.Text_IO.File_Type) return String
   is
      use Standard.Ada.Strings;
      use Standard.Ada.Strings.Fixed;
   begin
      loop
         declare
            Line      : constant String  := Standard.Ada.Text_IO.Get_Line (File);
            Comment   : constant Integer := Standard.Ada.Strings.Fixed.Index (Pattern => ";;", Source => Line);
            Non_Blank : constant Integer := Standard.Ada.Strings.Fixed.Index_Non_Blank (Line);
         begin
            if Non_Blank > 0 then
               if Comment = 0 then
                  return Trim (Line, Both);
               else
                  if Comment = Non_Blank then
                     null;
                  else
                     return Trim (Line (Non_Blank .. Comment - 1), Right);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Skip_Comments;

   procedure Put_Error
     (File_Name : in String;
      File_Line : in Standard.Ada.Text_IO.Positive_Count;
      Message   : in String)
   is
      use Standard.Ada.Directories;
      use Standard.Ada.Strings.Fixed;
      use Standard.Ada.Strings;
      use Standard.Ada.Text_IO;
      Prefix : constant String := Simple_Name (File_Name) & ":" &
        Trim (Positive_Count'Image (File_Line), Left) & ":0: ";
   begin
      Put_Line (Standard_Error, Prefix & Message);
   end Put_Error;

   procedure Put_Error (File : in Standard.Ada.Text_IO.File_Type; Message : in String)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Error (Name (File), Line (File) - 1, Message);
   end Put_Error;

end Wisi.Utils;
