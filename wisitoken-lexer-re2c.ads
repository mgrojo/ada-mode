--  Abstract:
--
--  WisiToken wrapper around the re2c lexer
--
--  Copyright (C) 2017 Stephe Leake
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
     (Buffer      : in System.Address;
      Length      : in Interfaces.C.size_t;
      Verbosity   : in Interfaces.C.int)
     return System.Address;
   --  Create the re2c lexer object, passing it the full text to process.
   --  Length is buffer length in 8 bit bytes.

   with procedure Free_Lexer (Lexer : in out System.Address);
   --  Destruct the re2c lexer object

   with procedure Reset_Lexer (Lexer : in System.Address);
   --  Restart lexing, with previous input buffer.

   with function Next_Token
     (Lexer         : in     System.Address;
      ID            :    out Token_ID;
      Byte_Position :    out Interfaces.C.size_t;
      Byte_Length   :    out Interfaces.C.size_t;
      Char_Position :    out Interfaces.C.size_t;
      Char_Length   :    out Interfaces.C.size_t)
     return Interfaces.C.int;
   --  *_Position and *_Length give the position and length in bytes and
   --  characters of the token from the start of the buffer, 0 indexed.
   --
   --  Line gives the line number in the source file that the token is
   --  in, 1 indexed. Char_Line_Start gives the character position of the
   --  line start relative to the file start.
   --
   --  Result values: (see wisi-gen_output_ada_common.adb create_re2c)
   --
   --  0 - no error
   --  1 - there is an unrecognized character at Position.

package WisiToken.Lexer.re2c is

   Invalid_Input : exception;

   type Instance is new WisiToken.Lexer.Instance with private;

   overriding procedure Finalize (Object : in out Instance);

   function New_Lexer
     (Trace       : not null access WisiToken.Trace'Class;
      New_Line_ID : in              Token_ID)
     return WisiToken.Lexer.Handle;
   --  If the tokens do not include a reporting New_Line token, set
   --  New_Line_ID to Invalid_Token_ID.

   overriding procedure Reset_With_String (Lexer : in out Instance; Input : in String);
   --  Copies Input to internal buffer.

   overriding procedure Reset_With_File (Lexer : in out Instance; File_Name : in String);
   --  Uses memory mapped file; no copies.

   overriding procedure Reset (Lexer : in out Instance);

   overriding function Find_Next (Lexer : in out Instance) return Token_ID;

   overriding
   function Line (Lexer : in Instance) return Ada.Text_IO.Count;

   overriding
   function Column (Lexer : in Instance) return Ada.Text_IO.Count;

   overriding function Char_Region (Lexer : in Instance) return Buffer_Region;
   overriding function Byte_Region (Lexer : in Instance) return Buffer_Region;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in Buffer_Region) return String;

private

   type Instance is new WisiToken.Lexer.Instance with
   record
      New_Line_ID   : Token_ID;
      Lexer         : System.Address := System.Null_Address;
      Source        : WisiToken.Lexer.Source;
      ID            : Token_ID; --  Last token read by find_next
      Byte_Position : Natural;
      Byte_Length   : Natural;
      Char_Position : Natural;
      Char_Length   : Natural;
      --  Position and length in bytes and characters of last token from
      --  start of Managed.Buffer, 1 indexed.

      Line            : Ada.Text_IO.Count; -- 1 indexed, after last New_Line token
      Char_Line_Start : Natural;           -- Character position after last New_Line token
   end record;

end WisiToken.Lexer.re2c;
