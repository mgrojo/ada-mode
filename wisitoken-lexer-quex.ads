--  Abstract:
--
--  WisiToken wrapper around the Quex lexer
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

pragma License (Modified_GPL); -- GNATCOLL.Iconv

pragma Warnings (Off, "license of withed unit ""GNATCOLL.Iconv"" may be inconsistent");
with GNATCOLL.Iconv;
pragma Warnings (On);

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with System;
with WisiToken.Lexer.Quex_Aux;
generic
   --  These subprograms are provided by generated source code.

   with function New_Lexer_From_Buffer
     (Buffer       : in System.Address;
      Length_8_Bit : in Interfaces.C.size_t)
     return Quex_Aux.Lexer_Type;
   --  Create the Quex lexer object, passing it the full text to process.
   --  Length_8_Bit is buffer length in 8 bit bytes (as required by Quex
   --  'from_memory').

   with procedure Free_Lexer (Lexer : in Quex_Aux.Lexer_Type);
   --  Destruct the Quex lexer object

   with procedure Next_Token
     (Lexer : in     Quex_Aux.Lexer_Type;
      Token :    out Quex_Aux.Token_Access);
   --  Yes, Quex returns a pointer to a token.
   --
   --  If the pointer is null, some programmer screwed up.
   --  If the token id is 0, there's an error; check Error_Code.

   with function Error_Code (Lexer : in Quex_Aux.Lexer_Type) return Interfaces.Unsigned_16;

package WisiToken.Lexer.Quex is

   Invalid_Input : exception;

   type Instance is new WisiToken.Lexer.Instance with private;

   function New_Lexer
     (Trace : not null access WisiToken.Trace'Class)
     return WisiToken.Lexer.Handle;

   overriding procedure Reset (Lexer : in out Instance; Input : in String);
   --  Raises Invalid_Input if Input is not properly encoded.

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

   type Byte_Sequence_Access is access all GNATCOLL.Iconv.Byte_Sequence;
   procedure Free is new Ada.Unchecked_Deallocation (GNATCOLL.Iconv.Byte_Sequence, Byte_Sequence_Access);
   type Managed_Lexer is new Ada.Finalization.Limited_Controlled with record
      Lexer          : Quex_Aux.Lexer_Type;
      Decoded_Buffer : Byte_Sequence_Access;
      Decoded_Last   : Integer;
   end record;

   overriding procedure Finalize (Object : in out Managed_Lexer);

   type Instance is new WisiToken.Lexer.Instance with
   record
      Managed : Managed_Lexer;
      Token   : aliased Quex_Aux.Token_Access; -- Last token read by find_next
   end record;

end WisiToken.Lexer.Quex;
