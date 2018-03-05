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
      Check (Label & ".first_index", Computed.First_Index, Expected'First);
      Check (Label & ".last_index", Computed.Last_Index, Expected'Last);
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

      Test_Item : Vector;
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

   procedure Test_Set_Length (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      Test_Item : Vector;
   begin
      Test_Item.Set_Length (0);
      Check ("-1a", Test_Item.First_Index, No_Index);
      Check ("-1b", Test_Item.Last_Index, No_Index);

      Test_Item.Set_Length (3);
      Check ("0a", Test_Item.First_Index, Index_Type'First);
      Check ("0b", Test_Item.Last_Index, Index_Type'First + 2);

      Test_Item (-10) := 2;
      Test_Item (-9)  := 3;
      Test_Item (-8)  := 4;

      Check ("1", Test_Item, (-10 => 2, -9 => 3, -8 => 4));

      Test_Item.Set_Length (6);
      Check ("2.first", Test_Item.First_Index, -10);
      Check ("2.last", Test_Item.Last_Index, -5);

      Test_Item (-5) := 5;
      Check ("2", Test_Item (-5), 5);
   end Test_Set_Length;

   procedure Prepend (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      Shared_Nodes   : Vector;
      Branched_Nodes : Vector;
   begin
      --  This test mimics the use of vectors in WisiToken.Syntax_Trees.Branched.

      Shared_Nodes.Set_First (1);
      Shared_Nodes.Append (1);
      Shared_Nodes.Append (2);
      Shared_Nodes.Append (3);

      Branched_Nodes.Set_First (4);
      Branched_Nodes.Append (4);
      Branched_Nodes.Append (5);

      Branched_Nodes.Prepend (Shared_Nodes, 2, 3);
      Check ("1", Branched_Nodes, (2 => 2, 3 => 3, 4 => 4, 5 => 5));
   end Prepend;

   procedure Test_Grow_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      Nodes : Vector;
   begin
      --  Test loop in Grow when it needs to execute more than once

      Nodes.Set_First (1);
      Nodes.Append (1);

      Nodes.Set_First (-2);
      Check ("1", Nodes.First_Index, -2);
   end Test_Grow_1;

   procedure Test_To_Vector_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      Nodes : constant Vector := To_Vector (1, 1);
   begin
      Check ("1", Nodes.First_Index, Index_Type'First);
      Check ("2", Nodes.Last_Index, Index_Type'First);
      Check ("3", Nodes (Index_Type'First), 1);
   end Test_To_Vector_1;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Set_Length'Access, "Test_Set_Length");
      Register_Routine (T, Prepend'Access, "Prepend");
      Register_Routine (T, Test_Grow_1'Access, "Test_Grow_1");
      Register_Routine (T, Test_To_Vector_1'Access, "Test_To_Vector_1");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_unbounded_definite_vectors.adb");
   end Name;

end Test_Unbounded_Definite_Vectors;
