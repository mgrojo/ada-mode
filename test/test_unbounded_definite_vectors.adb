--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with AUnit.Checks.Containers;
with SAL.Gen_Unbounded_Definite_Vectors;
package body Test_Unbounded_Definite_Vectors
is
   type Index_Type is range -10 .. 10;
   package Integer_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (Index_Type   => Index_Type,
      Element_Type => Integer);

   use Integer_Vectors;

   Test_Item : Vector;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Extended_Index);

   type Check_Array_Type is array (Index_Type range <>) of Integer;

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Check_Array_Type)
   is
      use AUnit.Checks;
      use AUnit.Checks.Containers;
   begin
      Check (Label & ".length", Computed.Length, Expected'Length);
      Check (Label & ".first_index", Test_Item.First_Index, Expected'First);
      Check (Label & ".last_index", Test_Item.Last_Index, Expected'Last);
      for I in Expected'Range loop
         Check (Label & "." & Index_Type'Image (I), Computed.Element (I), Expected (I));
      end loop;
   end Check;

   ----------
   --  Tests

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

   begin
      Test_Item.Set_First (1);
      Check ("0", Test_Item.First_Index, No_Index);

      Test_Item.Append (1);
      Test_Item.Append (2);

      Check ("1.capacity", Test_Item.Capacity, 2);
      Check ("1", Test_Item, (1 => 1, 2 => 2));

      Test_Item.Prepend (3);
      Test_Item.Prepend (4);

      Check ("2.capacity", Test_Item.Capacity, 4);
      Check ("2", Test_Item, (-1 => 4, 0 => 3, 1 => 1, 2 => 2));

      Test_Item.Append (5);

      Check ("3.length", Test_Item.Length, 5);
      Check ("3.capacity", Test_Item.Capacity, 8);
      Check ("3.first_index", Test_Item.First_Index, -1);
      Check ("3.last_index", Test_Item.Last_Index, 3);
      Check ("3.data", Test_Item (3), 5);
   end Nominal;

   --  FIXME: more

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_unbounded_definite_vectors.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Unbounded_Definite_Vectors;
