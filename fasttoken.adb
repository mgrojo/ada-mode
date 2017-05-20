-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014-2015, 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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
with Ada.Text_IO;
package body FastToken is

   function Int_Image (Item : in Integer) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Integer'Image (Item), Both);
   end Int_Image;

   function Gen_Any_1D (Item : in Array_Type) return Boolean
   is begin
      for I in Item'Range loop
         if Item (I) then
            return True;
         end if;
      end loop;
      return False;
   end Gen_Any_1D;

   function Gen_Any_2D (Item : in Array_2_Type) return Boolean
   is begin
      for I in Item'Range loop
         if Any (Item (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Gen_Any_2D;

   procedure Gen_Put_1D (Item : in Array_Type)
   is
      use Ada.Text_IO;
      Paren_Done : Boolean := False;
   begin
      if not Any (Item) then
         Put_Line ("(others => False));");
      else
         Put ("(");
         for I in Item'Range loop
            if Item (I) then
               if Paren_Done then
                  Put_Line (" |");
                  Put (" " & Image (I));
               else
                  Paren_Done := True;
                  Put (Image (I));
               end if;
            end if;
         end loop;
         Put_Line (" => True,");
         Put_Line (" others => False)");
      end if;
   end Gen_Put_1D;

   procedure Gen_Put_2D (Item : in Array_2_Type)
   is
      use Ada.Text_IO;
      Paren_Done : Boolean := False;
   begin
      if not Any (Item) then
         Put_Line ("(others => (others => False))");
      else
         Put ("(");
         for I in Item'Range loop
            if Any (Item (I)) then
               Put_Line (" " & Image_2 (I) & " =>");
               Put ("  (");
               Paren_Done := False;
               for J in Item (I)'Range loop
                  if Item (I)(J) then
                     if Paren_Done then
                        Put_Line (" |");
                        Put ("   " & Image_1 (J));
                     else
                        Paren_Done := True;
                        Put (Image_1 (J));
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
   end Gen_Put_2D;

end FastToken;
