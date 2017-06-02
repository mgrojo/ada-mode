--  Abstract :
--
--  Subprograms common to Output_Elisp and Output_Ada_Emacs
--
--  Copyright (C) 2012, 2013, 2015, 2017 Stephen Leake. All Rights Reserved.
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

package Wisi.Output_Elisp_Common is

   function Elisp_Name_To_Ada (Elisp_Name : in String) return String;

   procedure Indent_Keyword_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Keywords         : in     String_Pair_Lists.List;
      Image            : access function (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String);

   procedure Indent_Token_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Tokens           : in     Token_Lists.List;
      Image            : access function (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String);

   procedure Indent_Names
     (Output_File_Root : in     String;
      Label            : in     String;
      Names            : in     String_Lists.List);

end Wisi.Output_Elisp_Common;
