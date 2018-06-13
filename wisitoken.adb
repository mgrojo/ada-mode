-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014-2015, 2017, 2018 Stephe Leake
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

   function Image (Item : in Unknown_State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return (if Item = Unknown_State then " " else Trim (State_Index'Image (Item), Left));
   end Image;

   function Padded_Image (Item : in Token_ID; Desc : in Descriptor'Class) return String
   is begin
      return Ada.Strings.Fixed.Head
        (Desc.Image (Item).all,
         (if Item in Desc.First_Terminal .. Desc.Last_Terminal
          then Desc.Terminal_Image_Width
          else Desc.Image_Width));
   end Padded_Image;

   function Image (Item : in Token_ID; Desc : in Descriptor'Class) return String
   is begin
      return (if Item = Invalid_Token_ID then "" else Desc.Image (Item).all);
   end Image;

   function Find_ID (Descriptor : in WisiToken.Descriptor'Class; Name : in String) return Token_ID
   is begin
      for I in Descriptor.Image'Range loop
         if Descriptor.Image (I).all = Name then
            return I;
         end if;
      end loop;
      raise Programmer_Error with "token name '" & Name & "' not found in descriptor.image";
   end Find_ID;

   function To_Token_ID_Set (First, Last : in Token_ID; Item : in Token_ID_Array) return Token_ID_Set
   is begin
      return Result : Token_ID_Set := (First .. Last => False)
      do
         for ID of Item loop
            Result (ID) := True;
         end loop;
      end return;
   end To_Token_ID_Set;

   procedure To_Set (Item : in Token_ID_Arrays.Vector; Set : out Token_ID_Set)
   is begin
      for ID of Item loop
         Set (ID) := True;
      end loop;
   end To_Set;

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
      Max_Count : in Integer := Integer'Last;
      Inverted  : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
      Count      : Integer := 0;

      function Include (Item : in Boolean) return Boolean
      is begin
         if not Inverted then
            return Item;

         else
            return not Item;
         end if;
      end Include;

   begin
      for I in Item'Range loop
         if Include (Item (I)) then
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

   function Padded_Image (Item : in Production_ID; Width : in Integer) return String
   is
      use Ada.Strings.Fixed;
   begin
      return Result : String (1 .. Width) do
         Move (Production_ID'Image (Item), Result, Justify => Ada.Strings.Right);
      end return;
   end Padded_Image;

   function To_Vector (Item : in Production_ID_Array) return Production_ID_Arrays.Vector
   is begin
      return Result : Production_ID_Arrays.Vector do
         for I of Item loop
            Result.Append (I);
         end loop;
      end return;
   end To_Vector;

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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_ID_Set)
   is begin
      Ada.Text_IO.Put (Image (Item, Descriptor));
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array_Token_Set)
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
      Column    : in Ada.Text_IO.Count;
      Message   : in String)
     return String
   is begin
      return File_Name & ":" &
        Trimmed_Image (if Line = Invalid_Line_Number then Integer'(0) else Integer (Line)) & ":" &
        Trimmed_Image (Integer (Column)) & ": " &
        Message;
   end Error_Message;

   procedure Put_Error (Message : in String)
   is begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Message);
   end Put_Error;

   function Image (Item : in Buffer_Region) return String
   is begin
      return "(" & Trimmed_Image (Integer (Item.First)) & " ." & Buffer_Pos'Image (Item.Last) & ")";
   end Image;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region
   is begin
      return (Buffer_Pos'Min (Left.First, Right.First), Buffer_Pos'Max (Left.Last, Right.Last));
   end "and";

   function Image
     (Item       : in Base_Token;
      Descriptor : in WisiToken.Descriptor'Class)
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

   function Image
     (Token      : in Base_Token_Index;
      Terminals  : in Base_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is begin
      if Token = Invalid_Token_Index then
         return "<invalid_token_index>";
      else
         return Token_Index'Image (Token) & ":" & Image (Terminals (Token), Descriptor);
      end if;
   end Image;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is begin
      return "(" & Image (Item.ID, Descriptor) &
        (if Item.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Item.Byte_Region)) & ")";
   end Image;

end WisiToken;
