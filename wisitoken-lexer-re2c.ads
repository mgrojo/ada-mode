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
     (Buffer    : in System.Address;
      Length    : in Interfaces.C.size_t;
      Verbosity : in Interfaces.C.int)
     return System.Address;
   --  Create the re2c lexer object, passing it the full text to process.
   --  Length is buffer length in 8 bit bytes.

   with procedure Free_Lexer (Lexer : in out System.Address);
   --  Destruct the re2c lexer object

   with procedure Reset_Lexer (Lexer : in System.Address);
   --  Restart lexing, with previous input buffer.

   with function Next_Token
     (Lexer              : in     System.Address;
      ID                 :    out Token_ID;
      Byte_Position      :    out Interfaces.C.size_t;
      Byte_Length        :    out Interfaces.C.size_t;
      Character_Position :    out Interfaces.C.size_t;
      Character_Length   :    out Interfaces.C.size_t)
     return Interfaces.C.int;
   --  *_Position and *_Length give the position and length in bytes and
   --  characters of the token from the start of the buffer, 0 indexed.
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
     (Trace : not null access WisiToken.Trace'Class)
     return WisiToken.Lexer.Handle;

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

   overriding function Lexeme (Lexer : in Instance) return String;
   --  Return UTF-8 encoded text.

   overriding function Bounds (Lexer : in Instance) return Buffer_Region;
   --  Only correct for 8 bit input text.

private

   type Instance is new WisiToken.Lexer.Instance with
   record
      Lexer  : System.Address := System.Null_Address;
      Source : WisiToken.Lexer.Source;
      ID       : Token_ID; --  Last token read by find_next
      Byte_Position      : Natural;
      Byte_Length        : Natural;
      Character_Position : Natural;
      Character_Length   : Natural;
      --  Position and length in bytes and characters of last token from
      --  start of Managed.Buffer, 0 indexed.
   end record;

end WisiToken.Lexer.re2c;
