--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Checks.Containers;
with SAL.Gen_Bounded_Definite_Vectors;
package body Test_Bounded_Definite_Vectors
is
   package Integer_Vectors is new SAL.Gen_Bounded_Definite_Vectors
     (Index_Type   => Positive,
      Element_Type => Integer,
      Capacity     => 5);
   use Integer_Vectors;

   type Check_Array_Type is array (Positive range <>) of Integer;

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Check_Array_Type)
   is
      use AUnit.Checks;
      use AUnit.Checks.Containers;
   begin
      Check (Label & ".length", Computed.Length, Expected'Length);
      for I in Expected'Range loop
         Check (Label & "." & Integer'Image (I), Computed.Element (I), Expected (I));
      end loop;
   end Check;

   Item : Vector;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Append (Item, 1);
      Check ("1", Item, (1 => 1));
      Append (Item, 2);
      Append (Item, 3);
      Append (Item, 4);
      Check ("1234", Item, (1, 2, 3, 4));

      Item.Clear;
      Check ("clear", Item, (1 .. 0 => 0));
   end Nominal;

   procedure Errors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
   begin
      Clear (Item);
      Append (Item, 1);
      Append (Item, 2);
      Append (Item, 3);
      Append (Item, 4);
      Append (Item, 5);
      begin
         Append (Item, 6);
         Assert (False, "didn't get exception for append on full");
      exception
      when Constraint_Error =>
         Assert (True, "");
      end;
   end Errors;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_bounded_definite_vectors.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Errors'Access, "Errors");
   end Register_Tests;

end Test_Bounded_Definite_Vectors;
