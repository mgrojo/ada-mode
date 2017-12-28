-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014-2015, 2017 Stephe Leake
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
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
package body WisiToken is

   function Image (Item : in Token_ID; Desc : in Descriptor'Class; Pad : in Boolean := False) return String
   is begin
      if Pad then
         return Ada.Strings.Fixed.Head
           (Desc.Image (Item).all,
            (if Item in Desc.First_Terminal .. Desc.Last_Terminal
             then Desc.Terminal_Image_Width
             else Desc.Image_Width));
      else
         return Desc.Image (Item).all;
      end if;
   end Image;

   function Int_Image (Item : in Token_ID) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Token_ID'Image (Item), Both);
   end Int_Image;

   function Find_ID (Descriptor : in WisiToken.Descriptor'Class; Name : in String) return Token_ID
   is begin
      for I in Descriptor.Image'Range loop
         if Descriptor.Image (I).all = Name then
            return I;
         end if;
      end loop;
      raise Programmer_Error with "token name '" & Name & "' not found in descriptor.image";
   end Find_ID;

   function Any (Item : in Token_ID_Set) return Boolean
   is begin
      for I in Item'Range loop
         if Item (I) then
            return True;
         end if;
      end loop;
      return False;
   end Any;

   function Count (Item : in Token_ID_Set) return Integer
   is
      Result : Integer := 0;
   begin
      for I in Item'Range loop
         if Item (I) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   function Image
     (Item      : in Token_ID_Set;
      Desc      : in Descriptor'Class;
      Max_Count : in Integer := Integer'Last)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
      Count      : Integer := 0;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Need_Comma then
               Result := Result & ", ";
            end if;
            Result     := Result & Image (I, Desc);
            Need_Comma := True;
            Count := Count + 1;
            if Count = Max_Count then
               return To_String (Result);
            end if;
         end if;
      end loop;
      return To_String (Result);
   end Image;

   function To_Lookahead (Item : in Token_ID; Descriptor : in WisiToken.Descriptor) return Token_ID_Set
   is
      Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False);
   begin
      Result (Item) := True;
      return Result;
   end To_Lookahead;

   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in WisiToken.Descriptor) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Length (Result) > 0 then
               Result := Result & "/";
            end if;
            Result := Result & Image (I, Descriptor);
         end if;
      end loop;
      return To_String (Result);
   end Lookahead_Image;

   overriding
   function To_Lookahead (Item : in Token_ID; Descriptor : in LALR_Descriptor) return Token_ID_Set
   is
      Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Propagate_ID => False);
   begin
      Result (Item) := True;
      return Result;
   end To_Lookahead;

   overriding
   function Lookahead_Image (Item : in Token_ID_Set; Descriptor : in LALR_Descriptor) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Length (Result) > 0 then
               Result := Result & "/";
            end if;
            if I = Descriptor.Propagate_ID then
               Result := Result & "#";
            else
               Result := Result & Image (I, Descriptor);
            end if;
         end if;
      end loop;
      return To_String (Result);
   end Lookahead_Image;

   function Slice (Item : in Token_Array_Token_Set; I : in Token_ID) return Token_ID_Set
   is
      Result : Token_ID_Set := (Item'First (2) .. Item'Last (2) => False);
   begin
      for J in Result'Range loop
         Result (J) := Item (I, J);
      end loop;
      return Result;
   end Slice;

   function Any (Item : in Token_Array_Token_Set; I : in Token_ID) return Boolean
   is begin
      for J in Item'Range (2) loop
         if Item (I, J) then
            return True;
         end if;
      end loop;
      return False;
   end Any;

   function Any (Item : in Token_Array_Token_Set) return Boolean
   is begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            if Item (I, J) then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Any;

   procedure Or_Slice (Item : in out Token_Array_Token_Set; I : in Token_ID; Value : in Token_ID_Set)
   is begin
      for J in Item'Range (2) loop
         Item (I, J) := Item (I, J) or Value (J);
      end loop;
   end Or_Slice;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_ID)
   is begin
      Trace.Put (Image (Item, Trace.Descriptor.all));
   end Put;

   function Int_Image (Item : in Integer) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Integer'Image (Item), Both);
   end Int_Image;

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_ID_Set)
   is begin
      Ada.Text_IO.Put (Image (Item, Descriptor));
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_Array_Token_Set)
   is
      use Ada.Text_IO;
      Paren_Done : Boolean := False;
   begin
      if not Any (Item) then
         Put_Line ("(others => (others => False))");
      else
         Put ("(");
         for I in Item'Range (1) loop
            if Any (Item, I) then
               Put_Line (" " & Image (I, Descriptor) & " =>");
               Put ("  (");
               Paren_Done := False;
               for J in Item'Range (2) loop
                  if Item (I, J) then
                     if Paren_Done then
                        Put_Line (" |");
                        Put ("   " & Image (J, Descriptor));
                     else
                        Paren_Done := True;
                        Put (Image (J, Descriptor));
                     end if;
                  end if;
               end loop;
               if Paren_Done then
                  Put_Line (" => True,");
                  Put_Line ("   others => False)");
               else
                  Put_Line (" others => False),");
               end if;
            end if;
         end loop;
         Put_Line ((if Paren_Done then " " else "") & "others => (others => False))");
      end if;
   end Put;

   function Error_Message
     (File_Name : in String;
      Line      : in Line_Number_Type;
      Col       : in Ada.Text_IO.Count;
      Message   : in String)
     return String
   is begin
      return File_Name & ":" &
        Int_Image (if Line = Invalid_Line_Number then Integer'(0) else Integer (Line)) & ":" &
        Int_Image (Integer (Col)) & ": " &
        Message;
   end Error_Message;

   procedure Put_Error (Message : in String)
   is begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Message);
   end Put_Error;

   function Image (Item : in Buffer_Region) return String
   is begin
      return "(" & Int_Image (Integer (Item.First)) & " ." & Buffer_Pos'Image (Item.Last) & ")";
   end Image;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region
   is begin
      return (Buffer_Pos'Min (Left.First, Right.First), Buffer_Pos'Max (Left.Last, Right.Last));
   end "and";

   function Image
     (Item       : in Base_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean := False)
     return String
   is
      ID_Image : constant String := Image (Item.ID, Descriptor);
   begin
      if ID_Only or (Item.Name = Null_Buffer_Region and Item.Byte_Region = Null_Buffer_Region) then
         --  No parens for consistency with existing tests.
         return ID_Image;
      else
         return "(" & ID_Image &
           (if Item.Name = Null_Buffer_Region then "" else ", " & Image (Item.Name)) &
           (if Item.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Item.Byte_Region)) & ")";
      end if;
   end Image;

   function Image
     (Item       : in Base_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Strings.Unbounded;
      --  No outer parens, for compatibility with existing tests.
      Result : Unbounded_String;
   begin
      for I in Item.First_Index .. Item.Last_Index loop
         Result := Result & Image (Item (I), Descriptor);
         if I /= Item.Last_Index then
            Result := Result & ", ";
         end if;
      end loop;
      return -Result;
   end Image;

end WisiToken;
