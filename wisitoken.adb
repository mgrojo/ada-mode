--  Abstract:
--
--  See spec
--
--  Copyright (C) 2009, 2014-2015, 2017 - 2021 Free Software Foundation, Inc.
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
package body WisiToken is

   function Padded_Image (Item : in Token_ID; Desc : in Descriptor) return String
   is begin
      return Ada.Strings.Fixed.Head
        (Desc.Image (Item).all,
         (if Item in Desc.First_Terminal .. Desc.Last_Terminal
          then Desc.Terminal_Image_Width
          else Desc.Image_Width));
   end Padded_Image;

   function Image (Item : in Token_ID; Desc : in Descriptor) return String
   is begin
      return (if Item = Invalid_Token_ID then "-" else Desc.Image (Item).all);
   end Image;

   procedure Put_Tokens (Descriptor : in WisiToken.Descriptor)
   is
      use Ada.Text_IO;
   begin
      for I in Token_ID'First .. Descriptor.Last_Nonterminal loop
         Put_Line (Token_ID'Image (I) & " => " & Descriptor.Image (I).all);
      end loop;
   end Put_Tokens;

   function Find_ID (Descriptor : in WisiToken.Descriptor; Name : in String) return Token_ID
   is begin
      for I in Descriptor.Image'Range loop
         if Descriptor.Image (I).all = Name then
            return I;
         end if;
      end loop;
      raise SAL.Programmer_Error with "token name '" & Name & "' not found in descriptor.image";
   end Find_ID;

   procedure To_Vector (Item : in Token_ID_Array; Vector : in out Token_ID_Arrays.Vector)
   is
      J : Integer := Vector.First_Index;
   begin
      for ID of Item loop
         Vector.Replace_Element (J, ID);
         J := J + 1;
      end loop;
   end To_Vector;

   function To_Vector (Item : in Token_ID_Array) return Token_ID_Arrays.Vector
   is begin
      return Result : Token_ID_Arrays.Vector do
         Result.Set_First_Last (Item'First, Item'Last);
         for I in Item'Range loop
            Result (I) := Item (I);
         end loop;
      end return;
   end To_Vector;

   function Shared_Prefix (A, B : in Token_ID_Arrays.Vector) return Natural
   is
      use all type Ada.Containers.Count_Type;
      I : Natural := A.First_Index;
      J : Natural := B.First_Index;
   begin
      if A.Length = 0 or B.Length = 0 then
         return 0;
      end if;

      loop
         exit when A (I) /= B (I) or I = A.Last_Index or J = B.Last_Index;
         I := I + 1;
         J := J + 1;
      end loop;
      return I - 1;
   end Shared_Prefix;

   function "&" (Left : in Token_ID_Set; Right : in Token_ID) return Token_ID_Set
   is begin
      return Result : Token_ID_Set := Left do
         Result (Right) := True;
      end return;
   end "&";

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

   function To_Array (Item : in Token_ID_Set) return Token_ID_Arrays.Vector
   is begin
      return Result : Token_ID_Arrays.Vector do
         for ID in Item'Range loop
            if Item (ID) then
               Result.Append (ID);
            end if;
         end loop;
      end return;
   end To_Array;

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
      Desc      : in Descriptor;
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

   function Image (Item : in Production_ID) return String
   is begin
      return '(' & Trimmed_Image (Item.LHS) & ',' & Natural'Image (Item.RHS) & ')';
   end Image;

   function Image (Item : in Production_ID; Descriptor : in WisiToken.Descriptor) return String
   is begin
      return Image (Item.LHS, Descriptor) & '_' & Trimmed_Image (Item.RHS);
   end Image;

   function Trimmed_Image (Item : in Production_ID) return String
   is begin
      return Trimmed_Image (Item.LHS) & '.' & Trimmed_Image (Item.RHS);
   end Trimmed_Image;

   function Padded_Image (Item : in Production_ID; Width : in Integer) return String
   is
      use Ada.Strings.Fixed;
   begin
      return Result : String (1 .. Width) do
         Move (Trimmed_Image (Item), Result, Justify => Ada.Strings.Right);
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
      Column    : in Ada.Text_IO.Count;
      Message   : in String)
     return String
   is
      use all type Ada.Text_IO.Count;
   begin
      return File_Name & ":" &
        Trimmed_Image (if Line = Invalid_Line_Number then Integer'(0) else Integer (Line)) & ":" &
        Trimmed_Image (Integer (Column + 1)) & ": " &
        Message;
   end Error_Message;

   function Image (Item : in Buffer_Region) return String
   is begin
      return "(" & Trimmed_Image (Integer (Item.First)) & " ." & Buffer_Pos'Image (Item.Last) & ")";
   end Image;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region
   is begin
      return (Buffer_Pos'Min (Left.First, Right.First), Buffer_Pos'Max (Left.Last, Right.Last));
   end "and";

   function Contains
     (Outer, Inner   : in Buffer_Region;
      First_Boundary : in Boundary := Inclusive;
      Last_Boundary  : in Boundary := Inclusive)
     return Boolean
   is
      Result : Boolean;
   begin
      case First_Boundary is
      when Inclusive =>
         Result := Outer.First <= Inner.First;
      when Exclusive =>
         Result := Outer.First < Inner.First;
      end case;

      case Last_Boundary is
      when Inclusive =>
         Result := @ and  Outer.Last >= Inner.Last;
      when Exclusive =>
         Result := @ and  Outer.Last > Inner.Last;
      end case;
      return Result;
   end Contains;

   function Overlaps (A, B : in Buffer_Region) return Boolean
   is begin
      if Length (A) > 0 and Length (B) > 0 then
         return Inside (A.First, B) or Inside (A.Last, B) or Inside (B.First, A) or Inside (B.Last, A);
      else
         return False;
      end if;
   end Overlaps;

   function Trimmed_Image (Item : in Line_Number_Type) return String
   is
      function Base_Trimmed_Image is new SAL.Gen_Trimmed_Image (Line_Number_Type);
   begin
      if Item = Invalid_Line_Number then
         return "-";
      else
         return Base_Trimmed_Image (Item);
      end if;
   end Trimmed_Image;

   function Column (Token : in Base_Token; Line_Begin_Char_Pos : in Line_Pos_Vectors.Vector) return Ada.Text_IO.Count
   is begin
      --  We should check for Token.ID = EOI, and return column 0, because
      --  there is no actual EOI character in files created by Emacs. But
      --  that would require a Descriptor arg, and might be wrong for other
      --  files.
      if Token.Line = 1 then
         return Ada.Text_IO.Count (Token.Char_Region.First);
      elsif Token.Line in Line_Begin_Char_Pos.First_Index .. Line_Begin_Char_Pos.Last_Index then
         if Line_Begin_Char_Pos (Token.Line) = Invalid_Buffer_Pos then
            return 0;
         else
            return Ada.Text_IO.Count (Token.Char_Region.First - Line_Begin_Char_Pos (Token.Line));
         end if;
      else
         return 0;
      end if;
   end Column;

   function Image
     (Item       : in Base_Token;
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

   procedure Enable_Trace (Config : in String)
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
      Name_First : Integer := Config'First;
      Name_Last  : Integer;

      Value_First : Integer;
      Value_Last  : Integer;
   begin
      loop
         Name_Last := Index (Config, "=", Name_First);
         exit when Name_Last = 0;

         Value_First := Name_Last + 1;
         Name_Last   := Name_Last - 1;
         Value_Last  := Index (Config, " ", Value_First);
         if Value_Last = 0 then
            Value_Last := Config'Last;
         end if;
         declare
            Name : constant String := To_Lower (Config (Name_First .. Name_Last));

            function Get_Value return Integer
            is begin
               return Integer'Value (Config (Value_First .. Value_Last));
            exception
            when Constraint_Error =>
               raise User_Error with "expecting integer trace value, found '" & Config (Value_First .. Value_Last);
            end Get_Value;

            Value : constant Integer := Get_Value;
         begin
            --  Trace var alphabetical order
            if Name = "debug" then
               Debug_Mode := Value > 0;
            elsif Name = "action" then
               Trace_Action := Value;
            elsif Name = "ebnf" or Name = "generate_ebnf" then
               Trace_Generate_EBNF := Value;
            elsif Name = "minimal_complete" or Name = "generate_minimal_complete" then
               Trace_Generate_Minimal_Complete := Value;
            elsif Name = "table" or Name = "generate_table" then
               Trace_Generate_Table := Value;
            elsif Name = "incremental" or Name = "incremental_parse" then
               Trace_Incremental_Parse := Value;
            elsif Name = "lexer" then
               Trace_Lexer := Value;
            elsif Name = "mckenzie" then
               Trace_McKenzie := Value;
            elsif Name = "parse" then
               Trace_Parse := Value;
            elsif Name = "test" then
               Trace_Tests := Value;
            elsif Name = "time" then
               Trace_Time := Value > 0;
            else
               raise User_Error with "expecting trace name, found '" & Config (Name_First .. Name_Last) & "'";
            end if;
         end;

         Name_First := Value_Last + 1;
         exit when Name_First > Config'Last;
      end loop;
   end Enable_Trace;

end WisiToken;
