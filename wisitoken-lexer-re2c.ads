--  Abstract:
--
--  WisiToken wrapper around the re2c lexer
--
--  References:
--
--  [1] http://re2c.org/
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL); -- GNATCOLL.Mmap

pragma Warnings (Off, "license of withed unit ""GNATCOLL.Mmap"" may be inconsistent");

pragma Warnings (On);
with Interfaces.C;
with System;
generic
   --  These subprograms are provided by generated source code.

   with function New_Lexer
     (Buffer : in System.Address;
      Length : in Interfaces.C.size_t)
     return System.Address;
   --  Create the re2c lexer object, passing it the full text to process.
   --  Length is buffer length in 8 bit bytes.
   --
   --  The C lexer does not know about Buffer_Nominal_First,
   --  Line_Nominal_First; its buffer positions and lines start at 1.

   with procedure Free_Lexer (Lexer : in out System.Address);
   --  Destruct the re2c lexer object

   with procedure Reset_Lexer (Lexer : in System.Address);
   --  Restart lexing, with previous input buffer.

   with procedure Set_Verbosity
     (Lexer     : in System.Address;
      Verbosity : in Interfaces.C.int);

   with procedure Set_Position
     (Lexer         : in System.Address;
      Byte_Position : in Interfaces.C.size_t;
      Char_Position : in Interfaces.C.size_t;
      Line          : in Interfaces.C.int);

   with function Next_Token
     (Lexer         : in     System.Address;
      ID            :    out Token_ID;
      Byte_Position :    out Interfaces.C.size_t;
      Byte_Length   :    out Interfaces.C.size_t;
      Char_Position :    out Interfaces.C.size_t;
      Char_Length   :    out Interfaces.C.size_t;
      Line_Start    :    out Interfaces.C.int;
      Line_Length   :    out Interfaces.C.int)
     return Interfaces.C.int;
   --  *_Position and *_Length give the position and length in bytes and
   --  characters of the token from the start of the buffer, 0 indexed.
   --
   --  Line_Start gives the line number in the source file that the first
   --  character of the token is in, 1 indexed. Line_Length gives the
   --  number of line ends contained in the token; 0 for a token that is all on
   --  one line, 1 for a new_line, more for a multi-line token.
   --
   --  Result values:
   --
   --  0 - no error
   --  1 - there is an unrecognized character at Position.

   with function Is_Comment (ID : in Token_ID) return Boolean;
   --  Implements WisiToken.Lexer.Is_Comment.

   with function Comment_Start_Length (ID : in Token_ID) return Integer;
   --  Implements WisiToken.Lexer.Comment_Start_Length.

   with function Comment_End_Length (ID : in Token_ID) return Integer;
   --  Implements WisiToken.Lexer.Comment_Start_Length.

   with function Find_Comment_End
     (Source        : in WisiToken.Lexer.Source;
      ID            : in Token_ID;
      Comment_Start : in Buffer_Pos)
     return Buffer_Pos;
   --  Implements WisiToken.Lexer.Find_Comment_End.

   with function Contains_Comment_End
     (Source : in WisiToken.Lexer.Source;
      ID     : in Token_ID;
      Region : in Buffer_Region)
     return Boolean;
   --  Implements WisiToken.Lexer.Contains_Comment_End

   with function Line_Begin_Char_Pos
     (Source : in WisiToken.Lexer.Source;
      Token  : in WisiToken.Lexer.Token;
      Line   : in WisiToken.Line_Number_Type)
     return Buffer_Pos;
   --  Implements WisiToken.Lexer.Line_Begin_Char_Pos, so that
   --  precondition applies.

   with function Line_At_Byte_Pos
     (Source      : in WisiToken.Lexer.Source;
      ID          : in Token_ID;
      Byte_Region : in WisiToken.Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return WisiToken.Line_Number_Type;
   --  Implements WisiToken.Lexer.Line_At_Byte_Pos.

   with function Terminated_By_New_Line (ID : in Token_ID) return Boolean;
   --  Implements WisiToken.Lexer.Terminated_By_New_Line;

package WisiToken.Lexer.re2c is

   Invalid_Input : exception;

   type Instance is new WisiToken.Lexer.Instance with private;

   overriding procedure Finalize (Object : in out Instance);

   function New_Lexer (Descriptor : in WisiToken.Descriptor_Access_Constant) return WisiToken.Lexer.Handle;
   --  If the tokens do not include a reporting New_Line token, set
   --  New_Line_ID to Invalid_Token_ID.

   overriding procedure Set_Verbosity
     (Lexer     : in Instance;
      Verbosity : in Integer);

   overriding function Has_Source (Lexer : access constant Instance) return Boolean;

   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First);
   --  Copies Input to internal buffer.

   overriding procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      Input_Last : in     Integer;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First);

   overriding procedure Reset_With_File
     (Lexer          : in out Instance;
      File_Name      : in     String;
      Begin_Byte_Pos : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte_Pos   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char     : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line     : in     Line_Number_Type := Line_Number_Type'First);
   --  Uses memory mapped file; no copies.

   overriding procedure Discard_Rest_Of_Input (Lexer : in out Instance) is null;

   overriding procedure Reset (Lexer : in out Instance);

   overriding function Buffer_Region_Byte (Lexer : in Instance) return WisiToken.Buffer_Region;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in WisiToken.Buffer_Region) return String;

   overriding
   procedure Set_Position
     (Lexer         : in out Instance;
      Byte_Position : in     Buffer_Pos;
      Char_Position : in     Buffer_Pos;
      Line          : in     Line_Number_Type);

   overriding
   function Find_Next
     (Lexer : in out Instance;
      Token :    out WisiToken.Lexer.Token)
     return Boolean;

   overriding function File_Name (Lexer : in Instance) return String;

   overriding
   procedure Begin_Pos
     (Lexer      : in     Instance;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type);

   overriding
   function Is_Comment
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean;

   overriding
   function Comment_Start_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer;

   overriding
   function Comment_End_Length
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Integer;

   overriding
   function Find_Comment_End
     (Lexer         : in Instance;
      ID            : in Token_ID;
      Comment_Start : in Buffer_Pos)
     return Buffer_Pos;

   overriding
   function Contains_Comment_End
     (Lexer         : in Instance;
      ID            : in Token_ID;
      Region : in Buffer_Region)
     return Boolean;

   overriding
   function Line_Begin_Char_Pos
     (Lexer : in Instance;
      Token : in WisiToken.Lexer.Token;
      Line  : in Line_Number_Type)
     return Base_Buffer_Pos;

   overriding
   function Line_At_Byte_Pos
     (Lexer       : in Instance;
      ID          : in Token_ID;
      Byte_Region : in WisiToken.Buffer_Region;
      Byte_Pos    : in Buffer_Pos;
      First_Line  : in Line_Number_Type)
     return Line_Number_Type;

   overriding
   function Contains_New_Line
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region)
     return Boolean;

   overriding
   function New_Line_Count
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region)
     return Base_Line_Number_Type;

   overriding
   function Terminated_By_New_Line
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean;

private

   type Instance is new WisiToken.Lexer.Instance with
   record
      Lexer  : System.Address := System.Null_Address;
      Source : WisiToken.Lexer.Source;
   end record;

end WisiToken.Lexer.re2c;
