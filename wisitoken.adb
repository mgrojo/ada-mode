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
with Ada.Strings.Unbounded;
package body WisiToken is

   function Image (Desc : in Descriptor'Class; Item : in Token_ID; Pad : in Boolean := False) return String
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
     (Desc      : in Descriptor'Class;
      Item      : in Token_ID_Set;
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
            Result     := Result & Image (Desc, I);
            Need_Comma := True;
            Count := Count + 1;
            if Count = Max_Count then
               return To_String (Result);
            end if;
         end if;
      end loop;
      return To_String (Result);
   end Image;

   function To_Lookahead (Descriptor : in WisiToken.Descriptor; Item : in Token_ID) return Token_ID_Set
   is
      Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False);
   begin
      Result (Item) := True;
      return Result;
   end To_Lookahead;

   function Lookahead_Image (Descriptor : in WisiToken.Descriptor; Item : in Token_ID_Set) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Length (Result) > 0 then
               Result := Result & "/";
            end if;
            Result := Result & Image (Descriptor, I);
         end if;
      end loop;
      return To_String (Result);
   end Lookahead_Image;

   overriding
   function To_Lookahead (Descriptor : in LALR_Descriptor; Item : in Token_ID) return Token_ID_Set
   is
      Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Propagate_ID => False);
   begin
      Result (Item) := True;
      return Result;
   end To_Lookahead;

   overriding
   function Lookahead_Image (Descriptor : in LALR_Descriptor; Item : in Token_ID_Set) return String
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
               Result := Result & Image (Descriptor, I);
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
      Trace.Put (Image (Trace.Descriptor.all, Item));
   end Put;

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array) return String
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("(");
   begin
      for I in Item.First_Index .. Item.Last_Index loop
         Result := Result & Image (Descriptor, Item (I));
         if I /= Item.Last_Index then
            Result := Result & ", ";
         end if;
      end loop;
      Result := Result & ")";
      return To_String (Result);
   end Image;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Token_Array)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
   begin
      Put ("(");
      for I in Item.First_Index .. Item.Last_Index loop
         Put (Image (Descriptor, Item (I)));
         if I /= Item.Last_Index then
            Put (", ");
         end if;
      end loop;
      Put (")");
   end Put;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_Array)
   is
      use all type Ada.Containers.Count_Type;
   begin
      for I in Item.First_Index .. Item.Last_Index loop
         Put (Trace, Item (I));
         if I /= Item.Last_Index then
            Put (Trace, ", ");
         end if;
      end loop;
   end Put;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Token_Queues.Queue_Type)
   is
      use all type SAL.Base_Peek_Type;
   begin
      Trace.Put ("(");
      for I in 1 .. Item.Count loop
         Put (Trace, Item.Peek (I));
         if I < Item.Count then
            Put (Trace, ", ");
         end if;
      end loop;
      Trace.Put (")");
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
      Ada.Text_IO.Put (Image (Descriptor, Item));
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
               Put_Line (" " & Image (Descriptor, I) & " =>");
               Put ("  (");
               Paren_Done := False;
               for J in Item'Range (2) loop
                  if Item (I, J) then
                     if Paren_Done then
                        Put_Line (" |");
                        Put ("   " & Image (Descriptor, J));
                     else
                        Paren_Done := True;
                        Put (Image (Descriptor, J));
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
      Line, Col : in Ada.Text_IO.Count;
      Message   : in String)
     return String
   is begin
      return File_Name & ":" &
        Int_Image (Integer (Line)) & ":" &
        Int_Image (Integer (Col)) & ": " &
        Message;
   end Error_Message;

   function Image (Item : in Buffer_Region) return String
   is begin
      return "(" & Int_Image (Item.Begin_Pos) & " . " & Int_Image (Item.End_Pos) & ")";
   end Image;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region
   is begin
      return (Integer'Min (Left.Begin_Pos, Right.Begin_Pos), Integer'Max (Left.End_Pos, Right.End_Pos));
   end "and";

end WisiToken;
