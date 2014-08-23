--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2010, 2013, 2014 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with AUnit.Assertions;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
package body AUnit.Check is

   procedure Gen_Check_Discrete
     (Label    : in String;
      Computed : in Item_Type;
      Expected : in Item_Type)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Item_Type'Image (Computed) & " expecting " & Item_Type'Image (Expected));
   end Gen_Check_Discrete;

   procedure Check
     (Label    : in String;
      Computed : in Boolean;
      Expected : in Boolean)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Boolean'Image (Computed) & " expecting " & Boolean'Image (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in String)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & ASCII.LF &
           "got       '" & Computed & "'" & ASCII.LF &
           "expecting '" & Expected & "'");
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Integer;
      Expected : in Integer)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Integer'Image (Computed) & " expecting " & Integer'Image (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Ada.Tags.Tag;
      Expected : in Ada.Tags.Tag)
   is
      use Ada.Tags;
   begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & External_Tag (Computed) & " expecting " & External_Tag (Expected));
   end Check;

   function Not_In (Line : in Ada.Text_IO.Count; Skip : in Line_Number_Array_Type) return Boolean
   is
      use type Ada.Text_IO.Count;
   begin
      for I in Skip'Range loop
         if Line = Skip (I) then
            return False;
         end if;
      end loop;
      return True;
   end Not_In;

   function Chomp (Line : String) return String
   is
      use Ada.Strings.Maps;

      EOL_Set : constant Character_Set := To_Set (Character'Val (10) & Character'Val (13));

      function Last_Non_Terminating (P : Natural) return Natural
      is
         --  Return position of the last character in Line that is not one of the line
         --  terminating characters, or 0. Starts looking at position P, backwards.
      begin
         if P >= Line'First then
            if Is_In (Line (P), EOL_Set) then
               return Last_Non_Terminating (P - 1);
            else
               return P;
            end if;
         else
            return 0;
         end if;
      end Last_Non_Terminating;

   begin
      return Line (Line'First .. Last_Non_Terminating (Line'Last));
   end Chomp;

   procedure Check_Files
     (Label         : in String;
      Computed_Name : in String;
      Expected_Name : in String;
      Skip          : in Line_Number_Array_Type := (1 .. 0 => 1))
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use Standard.AUnit.Assertions;
      Computed : File_Type;
      Expected : File_Type;
   begin
      begin
         Open (Computed, In_File, Computed_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Computed_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E));
      end;

      begin
         Open (Expected, In_File, Expected_Name);
      exception
      when E : others =>
         Assert
           (False,
            Label & " file '" & Expected_Name & "' cannot be opened: " & Ada.Exceptions.Exception_Name (E));
      end;

      begin
         while not End_Of_File (Expected) and not End_Of_File (Computed) loop
            declare
               Computed_Line : constant String := Chomp (Get_Line (Computed));
               Expected_Line : constant String := Chomp (Get_Line (Expected));
               Line_Image    : constant String := ":" & Trim (Positive_Count'Image (Line (Expected)), Both);
            begin
               --  Get_Line advances the line counter beyond the line of interest
               if Not_In (Line (Computed) - 1, Skip) then
                  Check
                    (Expected_Name & Line_Image & " " & Computed_Name & Line_Image,
                     Computed_Line,
                     Expected_Line);
               end if;
            end;
         end loop;
         Assert (End_Of_File (Computed), Label & " '" & Computed_Name & "' longer than '" & Expected_Name & "'");
         Assert (End_Of_File (Expected), Label & " '" & Expected_Name & "' longer than '" & Computed_Name & "'");

         Close (Computed);
         Close (Expected);
      exception
      when Standard.AUnit.Assertions.Assertion_Error =>
         Close (Computed);
         Close (Expected);
         raise;
      end;
   end Check_Files;

end AUnit.Check;
