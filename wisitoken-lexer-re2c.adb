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
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package body WisiToken.Lexer.re2c is

   overriding procedure Finalize (Object : in out Instance)
   is
      use all type System.Address;
   begin
      if Object.Lexer /= System.Null_Address then
         Free_Lexer (Object.Lexer);
         Object.Lexer := System.Null_Address;
      end if;

      Finalize (Object.Source);
   end Finalize;

   function New_Lexer (Trace : not null access WisiToken.Trace'Class) return Handle
   is
      New_Lexer : constant access Instance := new Instance (Trace);
   begin
      return Handle (New_Lexer);
   end New_Lexer;

   overriding procedure Reset_With_String (Lexer : in out Instance; Input : in String)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label       => String_Label,
         Buffer      => new String'(Input),
         User_Buffer => False);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer : in out Instance;
      Input : in     Ada.Strings.Unbounded.String_Access)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label       => String_Label,
         Buffer      => Input,
         User_Buffer => True);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String_Access;

   overriding procedure Reset_With_File (Lexer : in out Instance; File_Name : in String)
   is
      use GNATCOLL.Mmap;
   begin
      Finalize (Lexer);

      --  We assume the file is in UTF-8 encoding
      Lexer.Source := (File_Label, Open_Read (File_Name), Invalid_Mapped_Region, 1);

      Lexer.Source.Region      := Read (Lexer.Source.File);
      Lexer.Source.Buffer_Last := Last (Lexer.Source.Region);

      if Integer (Length (Lexer.Source.File)) /= Lexer.Source.Buffer_Last then
         raise Programmer_Error with "not all of file is mapped; file length" &
           File_Size'Image (Length (Lexer.Source.File)) & " mapped:" & Integer'Image (Lexer.Source.Buffer_Last);
      end if;

      Lexer.Lexer := New_Lexer
        (Buffer    => Data (Lexer.Source.Region).all'Address,
         Length    => Interfaces.C.size_t (Last (Lexer.Source.Region)),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Instance)
   is begin
      Reset_Lexer (Lexer.Lexer);
      Lexer.Line            := 1;
      Lexer.Char_Line_Start := 1;
      Lexer.ID :=
        --  First token is assumed to be first on a line.
        (if Lexer.Trace.Descriptor.New_Line_ID = Invalid_Token_ID
         then Invalid_Token_ID
         else Lexer.Trace.Descriptor.New_Line_ID);
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is
      use all type Ada.Text_IO.Count;
      use Interfaces.C;
   begin
      Lexer.Prev_ID := Lexer.ID;
      declare
         Status : constant int := Next_Token
           (Lexer.Lexer, Lexer.ID,
            Byte_Position => Interfaces.C.size_t (Lexer.Byte_Position),
            Byte_Length   => Interfaces.C.size_t (Lexer.Byte_Length),
            Char_Position => Interfaces.C.size_t (Lexer.Char_Position),
            Char_Length   => Interfaces.C.size_t (Lexer.Char_Length),
            Line_Start    => Interfaces.C.int (Lexer.Line));
      begin
         case Status is
         when 0 =>
            if Lexer.ID = Lexer.Trace.Descriptor.New_Line_ID then
               Lexer.Char_Line_Start := Lexer.Char_Position + 1;
            end if;
            return Lexer.ID;

         when 1 =>
            declare
               use GNATCOLL.Mmap;
               Buffer        : constant Str_Access := WisiToken.Lexer.Buffer (Lexer.Source);
               Context_First : constant Integer    := Integer'Max (1, Lexer.Byte_Position - 3);
               Context_Last  : constant Integer    := Integer'Min (Lexer.Source.Buffer_Last, Lexer.Byte_Position + 3);

               Context : constant String (Context_First .. Context_Last) := String
                 (Buffer (Context_First .. Context_Last));
            begin
               raise Syntax_Error with " unrecognized character '" & Buffer (Lexer.Byte_Position) &
                 "' (" & Integer'Image (Character'Pos (Buffer (Lexer.Byte_Position))) & ") at character position" &
                 Natural'Image (Lexer.Char_Position) & " in context '" & Context & "'";
            end;

         when others =>
            raise Syntax_Error with " lexer returned unrecognized status code" & int'Image (Status);
         end case;
      end;
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

   overriding function Line (Lexer : in Instance) return Line_Number_Type
   is begin
      return Lexer.Line;
   end Line;

   overriding function Column (Lexer : in Instance) return Ada.Text_IO.Count
   is begin
      if Lexer.ID = Lexer.Trace.Descriptor.New_Line_ID or
        Lexer.ID = Lexer.Trace.Descriptor.EOF_ID
      then
         return 0;
      else
         return Ada.Text_IO.Count (Lexer.Char_Position - Lexer.Char_Line_Start);
      end if;
   end Column;

   overriding function Char_Region (Lexer : in Instance) return Buffer_Region
   is begin
      --  In an empty buffer, char_position = 0 and char_length = 0.
      return
        (Buffer_Pos (Lexer.Char_Position),
         Buffer_Pos (Integer'Max (1, Lexer.Char_Position + Lexer.Char_Length - 1)));
   end Char_Region;

   overriding function Byte_Region (Lexer : in Instance) return Buffer_Region
   is begin
      return (Buffer_Pos (Lexer.Byte_Position), Buffer_Pos (Lexer.Byte_Position + Lexer.Byte_Length - 1));
   end Byte_Region;

   overriding function First (Lexer : in Instance) return Boolean
   is begin
      return Lexer.Trace.Descriptor.New_Line_ID /= Invalid_Token_ID and then
           Lexer.Prev_ID = Lexer.Trace.Descriptor.New_Line_ID;
   end First;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in Buffer_Region) return String
   is begin
      return String (Buffer (Lexer.Source) (Integer (Byte_Bounds.First) .. Integer (Byte_Bounds.Last)));
   end Buffer_Text;

end WisiToken.Lexer.re2c;
