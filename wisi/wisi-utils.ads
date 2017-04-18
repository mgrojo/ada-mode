--  Abstract :
--
--  Utilities for parsing Wisent files
--
--  Copyright (C) 2012, 2013, 2015 Stephen Leake.  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
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

   --  code generation
   Indent : Ada.Text_IO.Positive_Count := 1;

   procedure Indent_Line (Text : in String);
   --  Put Text indented to Indent to Current_Output, with newline.

   function Elisp_Name_To_Ada (Elisp_Name : in String) return String;

   procedure Indent_Keyword_Table_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Keywords         : in     String_Pair_Lists.List;
      EOI_Name         : in     Ada.Strings.Unbounded.Unbounded_String;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String);
   --  Output via Indent_Line an elisp form named
   --  Output_File_Root-Label-keyword-table declaring a keyword table
   --  for the wisi lexer containing Keywords and EOI_Name, using Image
   --  (Pair.Name) for the elisp symbol.

   procedure Indent_Token_Table_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Tokens           : in     Token_Lists.List;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String);
   --  Output via Indent_Line an elisp form named
   --  Output_File_Root-Label-token-table declaring a token table for
   --  the wisi lexer containing Tokens, using Image (Token.Name) for
   --  the elisp symbol.

   procedure Indent_Names_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Names            : in     String_Lists.List);
   --  Output via Indent_Line an elisp form named
   --  Output_File_Root-Label-names declaring an array of symbols
   --  containing Names.

end Wisi.Utils;
