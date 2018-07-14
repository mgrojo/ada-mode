--  Abstract:
--
--  Test Sal.Gen_Definite_Doubly_Linked_Lists_Sorted
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks.Containers;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Validate;
package body Test_Definite_Doubly_Linked_Lists_Sorted is

   package Integer_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted (Integer, ">");
   use Integer_Lists;

   package Val is new Integer_Lists.Gen_Validate;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;

      List : Integer_Lists.List;
      Cur : Cursor;
   begin
      Check ("0", List.Length, 0);

      Insert (List, 1);
      Insert (List, 5);
      Insert (List, 3);

      Val.Validate ("0", List);
      Cur := List.First;
      Check ("1a", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("1b", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("1c", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("1d", Has_Element (Cur), False);
      Check ("1e", Cur = No_Element, True);

      Cur := List.First;
      Next (Cur);
      Delete (List, Cur);
      Val.Validate ("2", List);
      Check ("2a", Cur = No_Element, True);
      Check ("2b", List.Length, 2);
      Cur := List.First;
      Check ("2c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("2d", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("2e", Cur = No_Element, True);

      declare
         B : Integer_Lists.List := List;
         Added : Boolean;

         A : Integer_Lists.List renames List;
      begin
         Check ("3a", B.Length, 2);
         Cur := B.First;
         Check ("3b", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("3c", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("3d", Cur = No_Element, True);

         Merge (A, B, Added);

         Check ("4a", A.Length, 2);
         Check ("4b", B.Length, 2);
         Check ("4c", Added, False);

         B.Insert (2);

         Merge (A, B, Added);

         Check ("5a", A.Length, 3);
         Check ("5b", B.Length, 3);
         Check ("5c", Added, True);
         Cur := List.First;
         Check ("5d", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("5e", Constant_Ref (Cur), 2);
         Next (Cur);
         Check ("5f", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("5g", Cur = No_Element, True);

         B.Insert (6);
         B.Insert (4);

         Merge (A, B, Added, Exclude => 6);

         Check ("6a", A.Length, 4);
         Check ("6b", B.Length, 5);
         Check ("6c", Added, True);
         Cur := List.First;
         Check ("6d", Constant_Ref (Cur), 1);
         Next (Cur);
         Check ("6e", Constant_Ref (Cur), 2);
         Next (Cur);
         Check ("6f", Constant_Ref (Cur), 4);
         Next (Cur);
         Check ("6g", Constant_Ref (Cur), 5);
         Next (Cur);
         Check ("6h", Cur = No_Element, True);
      end;
   end Nominal;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_doubly_linked_lists_sorted.adb");
   end Name;

end Test_Definite_Doubly_Linked_Lists_Sorted;
