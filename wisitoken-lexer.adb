--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with GNAT.Strings;
package body WisiToken.Lexer is

   function Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      ID_Image : constant String := WisiToken.Image (Item.ID, Descriptor);
   begin
      if Item.Char_Region = Null_Buffer_Region then
         return "(" & ID_Image & ")";

      else
         return "(" & ID_Image & ", " & Image (Item.Char_Region) & ")";
      end if;
   end Image;

   function Full_Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      return "(" & Image (Item.ID, Descriptor) & ", " &
        Image (Item.Byte_Region) & ", " &
        Image (Item.Char_Region) & ", " &
        Image (Item.Line_Region) & ")";
   end Full_Image;

   function Column (Token : in Lexer.Token; Line_Begin_Char_Pos : in Buffer_Pos) return Ada.Text_IO.Count
   is begin
      if Token.Line_Region.First = 1 then
         return Ada.Text_IO.Count (Token.Char_Region.First);

      elsif Line_Begin_Char_Pos = Invalid_Buffer_Pos then
         return 0;

      else
         return Ada.Text_IO.Count (Token.Char_Region.First - Line_Begin_Char_Pos);
      end if;
   end Column;

   procedure Begin_Pos
     (Object     : in     Source;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   is begin
      Begin_Byte := Object.Buffer_Nominal_First_Byte;
      Begin_Char := Object.Buffer_Nominal_First_Char;
      Begin_Line := Object.Line_Nominal_First;
   end Begin_Pos;

   procedure Finalize (Object : in out Source)
   is begin
      case Object.Label is
      when String_Label =>
         if not Object.User_Buffer then
            Ada.Strings.Unbounded.Free (Object.Buffer);
         end if;

      when File_Label =>
         GNATCOLL.Mmap.Free (Object.Region);
         GNATCOLL.Mmap.Close (Object.File);
      end case;

      Object.Buffer_Nominal_First_Byte := Invalid_Buffer_Pos;
      Object.Buffer_Nominal_First_Char := Invalid_Buffer_Pos;
      Object.Line_Nominal_First        := Invalid_Line_Number;
   end Finalize;

   function Has_Source (Object : in Source) return Boolean
   is
      use all type Ada.Strings.Unbounded.String_Access;
   begin
      case Object.Label is
      when String_Label =>
         return Object.Buffer /= null;

      when File_Label =>
         --  Mmap doesn't provice "Is_Open".
         return Object.Buffer_Nominal_First_Byte /= Invalid_Buffer_Pos;
      end case;
   end Has_Source;

   function Buffer_Region_Byte (Object : in Source) return Buffer_Region
   is begin
      case Object.Label is
      when String_Label =>
         return (Base_Buffer_Pos (Object.Buffer'First), Base_Buffer_Pos (Object.Buffer'Last));

      when File_Label =>
         return (Object.Buffer_Nominal_First_Byte,
                 Object.Buffer_Nominal_First_Byte + Base_Buffer_Pos (Object.Buffer_Last));
      end case;
   end Buffer_Region_Byte;

   function Buffer (Source : in Lexer.Source) return GNATCOLL.Mmap.Str_Access
   is
      use GNATCOLL.Mmap;
   begin
      case Source.Label is
      when String_Label =>
         return Short.To_Str_Access (GNAT.Strings.String_Access (Source.Buffer));

      when File_Label =>
         return Data (Source.Region);
      end case;

   end Buffer;

   function File_Name (Source : in Lexer.Source) return String
   is begin
      return -Source.File_Name;
   end File_Name;

   function To_Char_Pos (Source : in Lexer.Source; Lexer_Char_Pos : in Integer) return Base_Buffer_Pos
   is begin
      return Base_Buffer_Pos (Lexer_Char_Pos) + Source.Buffer_Nominal_First_Char - Buffer_Pos'First;
   end To_Char_Pos;

end WisiToken.Lexer;
