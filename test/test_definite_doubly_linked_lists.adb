--  Abstract:
--
--  see spec
--
--  Copyright (C) 2017, 2018, 2020, 2021 Stephen Leake.  All Rights Reserved.
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

with Ada.Finalization;
with AUnit.Checks.Containers;
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Validate;
package body Test_Definite_Doubly_Linked_Lists is

   package Integer_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists
     (Element_Type => Integer);

   package Integer_Val is new Integer_Lists.Gen_Validate;

   type Element is new Ada.Finalization.Controlled with record
      A : Integer := Integer'First;
      B : Integer := Integer'Last;
   end record;

   overriding procedure Finalize (Object : in out Element)
   is begin
      Object.A := Integer'Last;
      Object.B := Integer'First;
   end Finalize;

   procedure Check
     (Label : in String;
      Computed : in Element;
      Expected : in Element)
   is
      use AUnit.Checks;
   begin
      Check (Label & ".a", Computed.A, Expected.A);
      Check (Label & ".b", Computed.B, Expected.B);
   end Check;

   type Element_Access is access Element;

   package Element_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists
     (Element_Type => Element_Access);

   package Element_Val is new Element_Lists.Gen_Validate;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Integer_Lists;

      List : Integer_Lists.List;
      Cur : Cursor := List.First;
   begin
      Check ("0", List.Length, 0);

      Append (List, 1);
      Append (List, 3);
      Append (List, 5);

      Integer_Val.Validate ("0", List);
      Cur := List.First;
      Check ("1a", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("1b", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("1c", Constant_Ref (Cur), 5);
      Next (Cur);
      Check ("1d", Has_Element (Cur), False);
      Check ("1e", Cur = No_Element, True);

      List.Prepend (0);
      Integer_Val.Validate ("2", List);
      Cur := List.First;
      Check ("2a", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("2b", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("2c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("2d", Constant_Ref (Cur), 5);
      Check ("2e", List.Length, 4);

      Delete (List, Cur);
      Integer_Val.Validate ("3", List);
      Check ("3a", Cur = No_Element, True);
      Check ("3b", List.Length, 3);
      Cur := List.First;
      Check ("3c", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("3d", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("3e", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("3f", Has_Element (Cur), False);

      Cur := List.First;
      Cur := Next (Cur);
      Delete (List, Cur);
      Integer_Val.Validate ("4", List);
      Check ("4a", List.Length, 2);
      Cur := List.First;
      Check ("4b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("4c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("4d", Has_Element (Cur), False);

      declare
         B : constant Integer_Lists.List := List;
         Cur_B : Cursor := B.First;
      begin
         Check ("4a", B.Length, 2);
         Cur_B := B.First;
         Check ("4b", Constant_Ref (Cur_B), 0);
         Next (Cur_B);
         Check ("4c", Constant_Ref (Cur_B), 3);
         Next (Cur_B);
         Check ("4d", Has_Element (Cur_B), False);
      end;

      Check ("5a", List.Length, 2);
      Cur := List.First;
      Check ("5b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("5c", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("5d", Has_Element (Cur), False);

   end Nominal;

   procedure Test_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Integer_Lists;

      List : Integer_Lists.List;
      Cur : Cursor := No_Element;
   begin
      --  Insert into empty
      List.Insert (List.First, 2);

      Integer_Val.Validate ("1", List);
      Check ("1a", List.Length, 1);
      Cur := List.First;
      Check ("1b", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("1c", Has_Element (Cur), False);

      --  Insert before head
      List.Insert (List.First, 0);
      Integer_Val.Validate ("2", List);
      Check ("2a", List.Length, 2);
      Cur := List.First;
      Check ("2b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("2c", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("2d", Has_Element (Cur), False);

      --  Insert before tail
      List.Insert (List.Last, 1);
      Integer_Val.Validate ("3", List);
      Check ("3a", List.Length, 3);
      Cur := List.First;
      Check ("3b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("3c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("3d", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("3e", Has_Element (Cur), False);

      --  Insert after tail
      List.Insert (No_Element, 3);
      Integer_Val.Validate ("4", List);
      Check ("4a", List.Length, 4);
      Cur := List.First;
      Check ("4b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("4c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("4d", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("4e", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("4f", Has_Element (Cur), False);

      --  Insert in middle
      Cur := List.First;
      Cur := Next (Cur);
      Cur := Next (Cur);
      Check ("5a", Constant_Ref (Cur), 2);

      List.Insert (Cur, -2);
      Integer_Val.Validate ("5", List);
      Check ("5a", List.Length, 5);
      Cur := List.First;
      Check ("5b", Constant_Ref (Cur), 0);
      Next (Cur);
      Check ("5c", Constant_Ref (Cur), 1);
      Next (Cur);
      Check ("5d", Constant_Ref (Cur), -2);
      Next (Cur);
      Check ("5e", Constant_Ref (Cur), 2);
      Next (Cur);
      Check ("5f", Constant_Ref (Cur), 3);
      Next (Cur);
      Check ("5g", Has_Element (Cur), False);

   end Test_Insert;

   procedure Test_Delete (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Integer_Lists;
      use Integer_Val;

      List      : Integer_Lists.List;
      Cur       : Cursor := No_Element;
      To_Delete : Cursor := No_Element;
   begin
      for I in 1 .. 4 loop
         List.Append (I);
      end loop;

      Validate ("1a", List);
      Check ("1b", List.Length, 4);

      Cur := List.First;

      for I in 1 .. 4 loop
         To_Delete := Cur;
         Next (Cur);
         List.Delete (To_Delete);
         Validate ("delete" & I'Image, List);
      end loop;

      Check ("2a", List.Length, 0);
      Check ("2b", List.First, No_Element);
      Check ("2c", List.Last, No_Element);
   end Test_Delete;

   procedure Test_Delete_Access (T : in out AUnit.Test_Cases.Test_Case'Class)
   --  Demonstrate that Element_Type of access value does _not_ deallocate Element.
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Element_Lists;
      use Element_Val;

      Perm_List : Element_Lists.List; --  accidently deleted!
      Temp_List : Element_Lists.List;
      Cur       : Cursor := No_Element;
      To_Delete : Cursor := No_Element;
   begin
      for I in 1 .. 4 loop
         Perm_List.Append (new Element'(Ada.Finalization.Controlled with I, -I));
         Temp_List.Append (Perm_List (Perm_List.Last));
      end loop;

      Validate ("1a", Perm_List);
      Check ("1b", Perm_List.Length, 4);
      Validate ("1c", Temp_List);
      Check ("1d", Temp_List.Length, 4);

      Cur := Temp_List.First;

      for I in 1 .. 4 loop
         To_Delete := Cur;
         Next (Cur);
         Temp_List.Delete (To_Delete);
         Validate ("delete" & I'Image, Temp_List);

         declare
            Check_Cur : Cursor := Perm_List.First;
         begin
            for I in 1 .. 4 loop
               Check ("2" & I'Image, Perm_List (Check_Cur).all, (Ada.Finalization.Controlled with I, -I));
               Next (Check_Cur);
            end loop;
         end;
      end loop;

      Check ("3a", Temp_List.Length, 0);
      Check ("3b", Temp_List.First, No_Element);
      Check ("3c", Temp_List.Last, No_Element);
   end Test_Delete_Access;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Insert'Access, "Test_Insert");
      Register_Routine (T, Test_Delete'Access, "Test_Delete");
      Register_Routine (T, Test_Delete_Access'Access, "Test_Delete_Access");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_doubly_linked_lists.adb");
   end Name;

end Test_Definite_Doubly_Linked_Lists;
