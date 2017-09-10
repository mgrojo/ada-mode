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
with GNAT.Byte_Order_Mark;
package body WisiToken.Lexer.Quex is

   Quex_Leading_Bytes  : constant := 1;
   Quex_Trailing_Bytes : constant := 1;
   Quex_Extra_Bytes    : constant :=
     Quex_Leading_Bytes + Quex_Trailing_Bytes;
   --  Quex requires its input buffer to have two leading reserved
   --  8 bit bytes and one trailing.

   procedure Decode_Buffer
     (Buffer         : in     String;
      Decoded_Buffer :    out Byte_Sequence_Access;
      First_8_Bit    :    out Positive;
      Last_8_Bit     :    out Natural;
      Charset        : in     String  := "";
      Read_BOM       : in     Boolean := False)
   is
      use GNAT.Byte_Order_Mark;
      use GNATCOLL.Iconv;

      State  : Iconv_T;
      Status : Iconv_Result;
      BOM    : BOM_Kind := Unknown;

      Input_Index : Positive;
   begin
      --  In the worst case, the input text has 8 bit characters that each
      --  expand to two utf8 bytes.

      Decoded_Buffer := new Byte_Sequence (1 .. 2 * Buffer'Length + Quex_Extra_Bytes);
      First_8_Bit    := Decoded_Buffer'First + Quex_Leading_Bytes;

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Buffer'Length = 0 then
         Last_8_Bit := First_8_Bit - 1;
         return;
      end if;

      --  If we have a byte order mark, it overrides the specified Charset

      Input_Index := Buffer'First;
      if Read_BOM then
         declare
            Len : Natural;
         begin
            GNAT.Byte_Order_Mark.Read_BOM (Buffer, Len, BOM);
            Input_Index := Input_Index + Len;
         end;
      end if;

      --  Create the Iconv converter. We will notice unknown charsets here

      declare
         use System;

         To_Code : constant String := UTF8;

         BOM_Kind_To_Charset : constant array (UTF8_All .. UTF32_BE) of Ada.Strings.Unbounded.String_Access :=
           (UTF8_All => UTF8'Unrestricted_Access,
            UTF16_LE => UTF16LE'Unrestricted_Access,
            UTF16_BE => UTF16BE'Unrestricted_Access,
            UTF32_LE => UTF32LE'Unrestricted_Access,
            UTF32_BE => UTF32BE'Unrestricted_Access);

         Actual_Charset : constant String :=
           (if BOM in UTF8_All .. UTF32_BE
            then BOM_Kind_To_Charset (BOM).all
            else Charset);
      begin
         State := Iconv_Open (To_Code, Actual_Charset);
      exception
      when Unsupported_Conversion =>
         Free (Decoded_Buffer);
         raise;
      end;

      --  Perform the conversion itself

      Last_8_Bit := First_8_Bit;
      Iconv (State,
             Buffer, Input_Index,
             Decoded_Buffer (Last_8_Bit .. Decoded_Buffer'Last - Quex_Trailing_Bytes), Last_8_Bit,
             Status);

      case Status is
      when Invalid_Multibyte_Sequence | Incomplete_Multibyte_Sequence =>

         Free (Decoded_Buffer);
         Iconv_Close (State);
         raise Invalid_Input;

      when Full_Buffer =>
         --  This is not supposed to happen: we allocated Decoded_buffer to be big
         --  enough in all cases.
         raise Programmer_Error;

      when Success =>
         null;
      end case;

      --  Clear the bytes we left for Quex

      declare
         Nul : constant Character := Character'Val (0);
      begin
         Decoded_Buffer (1 .. Quex_Leading_Bytes) := (others => Nul);
         Decoded_Buffer (Last_8_Bit + 1) := Nul;
      end;

      Iconv_Close (State);
   end Decode_Buffer;

   ----------
   --  Visible subprograms

   function New_Lexer (Trace : not null access WisiToken.Trace'Class) return Handle
   is
      use System;
      New_Lexer : constant access Instance := new Instance (Trace);
   begin
      return Handle (New_Lexer);
   end New_Lexer;

   overriding procedure Reset (Lexer : in out Instance; Input : in String)
   is
      use all type Quex_Aux.Error_Codes;
      use all type Interfaces.C.size_t;
      First_8_Bit : Positive;
      Last_8_Bit  : Natural;
      Status      : Quex_Aux.Error_Codes;
   begin
      Free (Lexer.Managed.Decoded_Buffer);
      Decode_Buffer (Input, Lexer.Managed.Decoded_Buffer, First_8_Bit, Last_8_Bit);

      --  We are telling Quex the buffer is smaller than it actually is, but
      --  that should not matter.
      Lexer.Managed.Lexer := New_Lexer_From_Buffer
        (Lexer.Managed.Decoded_Buffer.all'Address,
         Length_8_Bit => Interfaces.C.size_t (Last_8_Bit - First_8_Bit + 1 + Quex_Extra_Bytes));

      Status := Quex_Aux.Error_Codes'Val (Error_Code (Lexer.Managed.Lexer));
      if Status /= Quex_Aux.E_Error_None then
         raise Programmer_Error with "Quex from_memory returned error code " &
           Quex_Aux.Error_Codes'Image (Status);
      end if;
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is
      use all type Interfaces.Unsigned_16;
      use all type Quex_Aux.Token_Access;
   begin
      Next_Token (Lexer.Managed.Lexer, Lexer.Token);
      if Lexer.Token = null then
         --  Quex screwed up, or programmer screwed up parameter passing.
         raise Programmer_Error with "lexer find_next returned null token pointer";

      elsif Lexer.Token.ID = 0 then
         --  Quex reporting an error
         raise Syntax_Error with "Quex reported error code " &
           Quex_Aux.Error_Codes'Image (Quex_Aux.Error_Codes'Val (Error_Code (Lexer.Managed.Lexer)));

      else
         return Token_ID (Lexer.Token.ID);
      end if;
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
   is begin
      return Ada.Text_IO.Count (Lexer.Token.Line);
   end Line;

   overriding function Column (Lexer : in Instance) return Ada.Text_IO.Count
   is begin
      return Ada.Text_IO.Count (Lexer.Token.Column);
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
      Free (Object.Decoded_Buffer);
   end Finalize;

end WisiToken.Lexer.Quex;
