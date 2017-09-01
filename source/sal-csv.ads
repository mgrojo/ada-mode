--  Abstract :
--
--  Support for reading CSV (comma separated value) files. Also
--  supports other delimiters, including spaces.
--
--  Copyright (C) 2008 - 2013, 2016 - 2017 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Text_IO;
package SAL.CSV is
   pragma Elaborate_Body;
   --  Almost any package that does file IO will not be preelaborable.

   type File_Type is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out File_Type);
   --  close file, free data structures

   procedure Close (File : in out File_Type) renames Finalize;

   procedure Open
     (File               : in out File_Type;
      Name               : in     String;
      Max_Row_Size       : in     Integer;
      Delimiter          : in     Character := ',';
      Combine_Delimiters : in     Boolean   := False;
      Columns            : in     Integer   := 0);
   --  If Combine_Delimiters is True, consecutive delimeters in a line
   --  are combined into one column separator, so no empty columns are
   --  possible.
   --
   --  If Columns is 0, reads first row of the file to determine how
   --  many columns there are, then resets file.
   --
   --  After any reset, reads first row; Next_Row will read second row.
   --
   --  Raises Initialization_Error if first row character count is
   --  longer than Max_Row_Size.
   --
   --  Raises End_Error if there is no data in File.

   procedure Reset (File : in out File_Type);
   --  Set File to reading first row.

   function Columns (File : in File_Type) return Integer;
   --  number of columns found or specifiedin Open

   function Row (File : in File_Type) return Integer;
   --  Current row; same as Text_IO.Line

   function End_Of_File (File : in File_Type) return Boolean;
   --  True if Next_Row will raise End_Error; no more data.

   procedure Next_Row (File : in out File_Type);
   --  After Open, read will read first row (often containing the row
   --  header names). Next_Row advances internal data so read will
   --  read the next row.
   --
   --  Raises Ada.Text_IO.End_Error if there is no next row.
   --
   --  Raises Initialization_Error if new row is longer than
   --  Max_Line_Size specified in Open.

   procedure Next (File : in out File_Type) renames Next_Row;

   procedure Skip_Lines
     (File : in out File_Type;
      N    : in Integer);
   --  Skip N lines by calling Next_Row N times.

   function Read (File : in File_Type; Column : in Positive) return String;
   --  Return the contents of Column as a string.
   --
   --  Raises Use_Error if Column is not in File.

   generic
      type Float_Type is digits <>;
   function Gen_Read_Float (File : in File_Type; Column : in Positive) return Float_Type;
   --  Allows integer or real number Ada syntax, and empty string returns 0.0.

   generic
      type Integer_Type is range <>;
   function Gen_Read_Integer (File : in File_Type; Column : in Positive) return Integer_Type;
   --  Allows integer Ada syntax, and empty string returns 0.

   function Quote (Item : in String) return String;
   --  Return quoted Item; handles embedded quotes using Ada syntax.

   function Unquote (Item : in String) return String;
   --  Inverse of Quote

   --  For error messages
   function Is_Open (File : in File_Type) return Boolean;
   function Name (File : in File_Type) return String;
   function Line (File : in File_Type) return Ada.Text_IO.Positive_Count;

private
   type Positive_Array_Integer_Type is array (Positive range <>) of Integer;
   type Positive_Array_Integer_Access_Type is access Positive_Array_Integer_Type;

   type File_Type is new Ada.Finalization.Limited_Controlled with record
      Delimiter          : String (1 .. 1);
      Combine_Delimiters : Boolean;

      File        : Ada.Text_IO.File_Type;
      Line        : Ada.Strings.Unbounded.String_Access;
      Last        : Integer;
      Commas      : Positive_Array_Integer_Access_Type;
      Comma_Count : Positive_Array_Integer_Access_Type;
      --  Number of consecutive delimiters at start of each column
      --  (deleted from column data).
   end record;

end SAL.CSV;
