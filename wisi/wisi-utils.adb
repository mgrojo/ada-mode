--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2013, 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Strings.Fixed;
package body Wisi.Utils is

   procedure Indent_Line (Text : in String)
   is
      use Standard.Ada.Text_IO;
   begin
      Set_Col (Indent);
      Put_Line (Text);
      Line_Count := Line_Count + 1;
   end Indent_Line;

   procedure Indent_Start (Text : in String)
   is
      use Standard.Ada.Text_IO;
   begin
      Set_Col (Indent);
      Put (Text);
   end Indent_Start;

   procedure Indent_Wrap (Text : in String)
   is
      use all type Standard.Ada.Text_IO.Count;
      use Standard.Ada.Strings;
      use Standard.Ada.Strings.Fixed;
      I     : Natural;
      First : Integer := Text'First;
   begin
      if Text'Length + Indent <= Max_Line_Length then
         Indent_Line (Text);
      else
         loop
            I := Text'Last;
            loop
               I := Index (Text (First .. Text'Last), " ", From => I, Going => Backward);
               exit when I - First + Integer (Indent) <= Max_Line_Length;
               I := I - 1;
            end loop;
            Indent_Line (Text (First .. I - 1));
            First := I + 1;
            exit when Text'Last - First + Integer (Indent) <= Max_Line_Length;
         end loop;
         Indent_Line (Text (First .. Text'Last));
      end if;
   end Indent_Wrap;

   procedure Indent_Wrap_Comment (Text : in String; Comment_Syntax : in String_2)
   is
      use all type Standard.Ada.Text_IO.Count;
      use Standard.Ada.Strings;
      use Standard.Ada.Strings.Fixed;
      Prefix : constant String := Comment_Syntax & "  ";
      I      : Natural;
      First  : Integer         := Text'First;
   begin
      if Text'Length + Indent <= Max_Line_Length - 4 then
         Indent_Line (Prefix & Text);
      else
         loop
            I := Text'Last;
            loop
               I := Index (Text (First .. Text'Last), " ", From => I, Going => Backward);
               exit when I - First + Integer (Indent) <= Max_Line_Length - 4;
               I := I - 1;
            end loop;
            Indent_Line (Prefix & Text (First .. I - 1));
            First := I + 1;
            exit when Text'Last - First + Integer (Indent) <= Max_Line_Length - 4;
         end loop;
         Indent_Line (Prefix & Text (First .. Text'Last));
      end if;
   end Indent_Wrap_Comment;

   function Strip_Quotes (Item : in String) return String
   is begin
      if Item'Length < 2 then
         return Item;
      else
         return Item
           ((if Item (Item'First) = '"' then Item'First + 1 else Item'First) ..
              (if Item (Item'Last) = '"' then Item'Last - 1 else Item'Last));
      end if;
   end Strip_Quotes;

   function Strip_Parens (Item : in String) return String
   is begin
      if Item'Length < 2 then
         return Item;
      else
         return Item
           ((if Item (Item'First) = '(' then Item'First + 1 else Item'First) ..
              (if Item (Item'Last) = ')' then Item'Last - 1 else Item'Last));
      end if;
   end Strip_Parens;

end Wisi.Utils;
