--  Abstract:
--
--  see spec.
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
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Exceptions;
package body WisiToken.Lexer.re2c is

   function New_Lexer (Trace : not null access WisiToken.Trace'Class) return Handle
   is
      use System;
      New_Lexer : constant access Instance := new Instance (Trace);
   begin
      return Handle (New_Lexer);
   end New_Lexer;

   overriding procedure Reset_With_String (Lexer : in out Instance; Input : in String)
   is begin
      Finalize (Lexer.Managed);

      --  We assume Input is in UTF-8 encoding
      Lexer.Managed.Source :=
        (Label  => String_Label,
         Buffer => new String'(Input));

      Lexer.Managed.Lexer := New_Lexer
        (Buffer    => Lexer.Managed.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

   end Reset_With_String;

   overriding procedure Reset_With_File (Lexer : in out Instance; File_Name : in String)
   is
      use GNATCOLL.Mmap;
   begin
      Finalize (Lexer.Managed);

      --  We assume the file is in UTF-8 encoding
      Lexer.Managed.Source := (File_Label, Open_Read (File_Name), Invalid_Mapped_Region);

      Lexer.Managed.Source.Region := Read (Lexer.Managed.Source.File);

      Lexer.Managed.Lexer := New_Lexer
        (Buffer    => Data (Lexer.Managed.Source.Region).all'Address,
         Length    => Interfaces.C.size_t (Last (Lexer.Managed.Source.Region)),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Instance)
   is begin
      Reset_Lexer (Lexer.Managed.Lexer);
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is
      use Interfaces.C;

      Status : constant int := Next_Token
        (Lexer.Managed.Lexer, Lexer.ID, Interfaces.C.size_t (Lexer.Position), Interfaces.C.size_t (Lexer.Length));
   begin
      case Status is
      when 0 =>
         return Lexer.ID;
      when 1 =>
         raise Syntax_Error with "lexer attempted to read past end of buffer";
      when 2 =>
         raise Syntax_Error with "unrecognized character at " & Natural'Image (Lexer.Position);

      when others =>
         raise Syntax_Error with "lexer returned unrecognized status code" & int'Image (Status);
      end case;
   exception
   when Syntax_Error  =>
      raise;

   when E : others =>
      raise Syntax_Error with
         Error_Message
           ("", Lexer.Line, Lexer.Column,
            "lexer find_next error: " & Ada.Exceptions.Exception_Name (E) &
              " : " & Ada.Exceptions.Exception_Message (E));
   end Find_Next;

   overriding function Line (Lexer : in Instance) return Ada.Text_IO.Count
   is
      pragma Unreferenced (Lexer);
   begin
      return 0;
   end Line;

   overriding function Column (Lexer : in Instance) return Ada.Text_IO.Count
   is begin
      return Ada.Text_IO.Count (Lexer.Position);
   end Column;

   overriding function Lexeme (Lexer : in Instance) return String
   is
      pragma Unreferenced (Lexer);
   begin
      --  FIXME: add C function to use token char pointer, length to fetch
      --  lexeme. Or figure out a portable way to return Offset. Or return
      --  char pointer, do pointer math here; we know the buffer address.
      return "";
   end Lexeme;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region
   is
      pragma Unreferenced (Lexer);
   begin
      --  The result is supposed to be the location in the original input
      --  buffer; Lexer.Token.Offset is the location in the decoded buffer
      --  (of utf8 characters), and there is no simple relation between
      --  the two.
      --
      --  Except in the case where the input buffer is also utf8, which is
      --  most of the time, so we simply assume that here.
      --
      --  FIXME: Offset is not available yet, and Token.Length is not set properly
      return (0, 0);
   end Bounds;

   overriding procedure Finalize (Object : in out Managed_Lexer)
   is begin
      Free_Lexer (Object.Lexer);
      case Object.Source.Label is
      when String_Label =>
         Ada.Strings.Unbounded.Free (Object.Source.Buffer);

      when File_Label =>
         GNATCOLL.Mmap.Free (Object.Source.Region);
         GNATCOLL.Mmap.Close (Object.Source.File);
      end case;
   end Finalize;

end WisiToken.Lexer.re2c;
