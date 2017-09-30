--  Abstract :
--
--  Utilities for parsing Wisent files
--
--  Copyright (C) 2012, 2013, 2015, 2017 Stephen Leake. All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
package Wisi.Utils is

   Error : Boolean := False;

   function Skip_Comments (File : in Standard.Ada.Text_IO.File_Type) return String;
   --  Return next line that is not a comment, and strip leading
   --  whitespace and trailing comment from line.

   function Error_String
     (File_Name : in String;
      File_Line : in Standard.Ada.Text_IO.Positive_Count;
      Message   : in String)
     return String;

   procedure Put_Error
     (File_Name : in String;
      File_Line : in Standard.Ada.Text_IO.Positive_Count;
      Message   : in String);
   --  Set Error True, output to Standard_Error

   procedure Put_Error (File : in Standard.Ada.Text_IO.File_Type; Message : in String);
   --  Set Error True, output error to Standard_Error

   --  code generation
   Indent : Standard.Ada.Text_IO.Positive_Count := 1;

   procedure Indent_Line (Text : in String);
   --  Put Text indented to Indent to Current_Output, with newline.

   procedure Indent_Start (Text : in String);
   --  Put Text indented to Indent to Current_Output, without newline.
   --  Should be followed by Put_Line, not Indent_Line.

   function Strip_Quotes (Item : in String) return String;
   --  Remove leading and trailing '"', if any.

   function Strip_Parens (Item : in String) return String;
   --  Remove leading and trailing '()', if any.

end Wisi.Utils;
