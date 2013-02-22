--  Abstract :
--
--  Utilities for parsing Wisent files
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

with Ada.Text_IO;
package Wisi.Utils is

   function Skip_Comments (File : in Standard.Ada.Text_IO.File_Type) return String;
   --  Return next line that is not a comment, and strip leading
   --  whitespace and trailing comment from line.

   procedure Put_Error
     (File_Name : in String;
      File_Line : in Standard.Ada.Text_IO.Positive_Count;
      Message   : in String);
   --  Output error message on Standard_Error

   procedure Put_Error (File : in Standard.Ada.Text_IO.File_Type; Message : in String);
   --  Output error message on Standard_Error for File

end Wisi.Utils;
