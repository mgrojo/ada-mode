--  Abstract:
--
--  see spec
--
--  Copyright (C) 2017, 2018, 2020 - 2022 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Checks.Containers;
with Ada.Assertions;
with SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count.Gen_Validate;
package body Test_Definite_Doubly_Linked_Lists_Ref_Count is

   package Integer_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count
     (Element_Type => Integer);

   package Integer_Val is new Integer_Lists.Gen_Validate;

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
      Integer_Val.Check_Ref_Counts ("0 ref_counts", List, (0, 0, 0));

      Cur := List.First;
      Integer_Val.Check_Ref_Counts ("1a ref_counts", List, (1, 0, 0));
      Check ("1a", Element (Cur), 1);
      Next (Cur);
      Integer_Val.Check_Ref_Counts ("1b ref_counts", List, (0, 1, 0));
      Check ("1b", Element (Cur), 3);
      Next (Cur);
      Integer_Val.Check_Ref_Counts ("1c ref_counts", List, (0, 0, 1));
      Check ("1c", Element (Cur), 5);
      Next (Cur);
      Integer_Val.Check_Ref_Counts ("1d ref_counts", List, (0, 0, 0));
      Check ("1d", Has_Element (Cur), False);
      Check ("1e", Cur = No_Element, True);

      List.Prepend (0);
      Integer_Val.Validate ("2", List);
      Cur := List.First;
      Check ("2a", Element (Cur), 0);
      Next (Cur);
      Check ("2b", Element (Cur), 1);
      Next (Cur);
      Check ("2c", Element (Cur), 3);
      Next (Cur);
      Check ("2d", Element (Cur), 5);
      Check ("2e", List.Length, 4);

      List.Delete (Cur);
      Integer_Val.Validate ("3", List);
      Check ("3a", Cur = No_Element, True);
      Check ("3b", List.Length, 3);
      Cur := List.First;
      Check ("3c", Element (Cur), 0);
      Next (Cur);
      Check ("3d", Element (Cur), 1);
      Next (Cur);
      Check ("3e", Element (Cur), 3);
      Next (Cur);
      Check ("3f", Has_Element (Cur), False);

      Cur := List.First;
      Cur := Next (Cur);
      List.Delete (Cur);
      Integer_Val.Validate ("4", List);
      Check ("4a", List.Length, 2);
      Cur := List.First;
      Check ("4b", Element (Cur), 0);
      Next (Cur);
      Check ("4c", Element (Cur), 3);
      Next (Cur);
      Check ("4d", Has_Element (Cur), False);

      declare
         B : constant Integer_Lists.List := List;
         Cur_B : Cursor := B.First;
      begin
         Check ("4a", B.Length, 2);
         Cur_B := B.First;
         Check ("4b", Element (Cur_B), 0);
         Next (Cur_B);
         Check ("4c", Element (Cur_B), 3);
         Next (Cur_B);
         Check ("4d", Has_Element (Cur_B), False);
      end;

      Check ("5a", List.Length, 2);
      Cur := List.First;
      Check ("5b", Element (Cur), 0);
      Next (Cur);
      Check ("5c", Element (Cur), 3);
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
      Check ("1b", Element (Cur), 2);
      Next (Cur);
      Check ("1c", Has_Element (Cur), False);

      --  Insert before head
      List.Insert (List.First, 0);
      Integer_Val.Validate ("2", List);
      Check ("2a", List.Length, 2);
      Cur := List.First;
      Check ("2b", Element (Cur), 0);
      Next (Cur);
      Check ("2c", Element (Cur), 2);
      Next (Cur);
      Check ("2d", Has_Element (Cur), False);

      --  Insert before tail
      List.Insert (List.Last, 1);
      Integer_Val.Validate ("3", List);
      Check ("3a", List.Length, 3);
      Cur := List.First;
      Check ("3b", Element (Cur), 0);
      Next (Cur);
      Check ("3c", Element (Cur), 1);
      Next (Cur);
      Check ("3d", Element (Cur), 2);
      Next (Cur);
      Check ("3e", Has_Element (Cur), False);

      --  Insert after tail
      List.Insert (No_Element, 3);
      Integer_Val.Validate ("4", List);
      Check ("4a", List.Length, 4);
      Cur := List.First;
      Check ("4b", Element (Cur), 0);
      Next (Cur);
      Check ("4c", Element (Cur), 1);
      Next (Cur);
      Check ("4d", Element (Cur), 2);
      Next (Cur);
      Check ("4e", Element (Cur), 3);
      Next (Cur);
      Check ("4f", Has_Element (Cur), False);

      --  Insert in middle
      Cur := List.First;
      Cur := Next (Cur);
      Cur := Next (Cur);
      Check ("5a", Element (Cur), 2);

      List.Insert (Cur, -2);
      Integer_Val.Validate ("5", List);
      Check ("5a", List.Length, 5);
      Cur := List.First;
      Check ("5b", Element (Cur), 0);
      Next (Cur);
      Check ("5c", Element (Cur), 1);
      Next (Cur);
      Check ("5d", Element (Cur), -2);
      Next (Cur);
      Check ("5e", Element (Cur), 2);
      Next (Cur);
      Check ("5f", Element (Cur), 3);
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

   procedure Detect_Dangling (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use Integer_Lists;

      List      : Integer_Lists.List;
      Cur_1     : Cursor := No_Element;
      Cur_2     : Cursor := No_Element;
      To_Delete : Cursor := No_Element;
   begin
      for I in 1 .. 4 loop
         List.Append (I);
      end loop;

      --  Typical usage error while maintaining two cursors while deleting
      --  via one of them.

      Cur_1 := List.First;
      Check ("0", Element (Cur_1), 1); -- keep the compiler happy about reading Cur_1.

      Cur_2 := List.First;

      begin
         To_Delete := Cur_2;
         Integer_Val.Check_Ref_Counts ("1a ref_counts", List, (3, 0, 0, 0));
         Cur_2 := Next (Cur_2);
         Integer_Val.Check_Ref_Counts ("1b ref_counts", List, (2, 1, 0, 0));
         List.Delete (To_Delete);
         AUnit.Assertions.Assert (False, "1c exception not raised");
      exception
      when SAL.Invalid_Operation | Ada.Assertions.Assertion_Error =>
         --  We get Assertion_Error from the precondition on Delete if
         --  assertions are enabled.
         Check ("1d", Element (Cur_2), 2);
         Integer_Val.Check_Ref_Counts ("1e ref_counts", List, (2, 1, 0, 0));
      end;

      --  Correct the usage error by changing Cur_1.
      Previous (Cur_1);
      Check ("2a", Has_Element (Cur_1), False); -- keep the compiler happy about unused assignment to Cur_1
      Integer_Val.Check_Ref_Counts ("2b ref_counts", List, (1, 1, 0, 0));
      List.Delete (To_Delete);

      Check ("2c", List.Length, 3);
      Check ("2d", Element (Cur_2), 2);
      Integer_Val.Check_Ref_Counts ("2e ref_counts", List, (1, 0, 0));
   end Detect_Dangling;

   procedure Test_Prev_Next (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Integer_Lists;

      use AUnit.Checks;
      List  : Integer_Lists.List;
      Cur_1 : Cursor := No_Element;
   begin
      for I in 1 .. 4 loop
         List.Append (I);
      end loop;

      Integer_Val.Check_Ref_Counts ("1 ref_counts", List, (0, 0, 0, 0));

      Cur_1 := List.Last;
      Integer_Val.Check_Ref_Counts ("2 ref_counts", List, (0, 0, 0, 1));

      Previous (Cur_1);
      Integer_Val.Check_Ref_Counts ("3 ref_counts", List, (0, 0, 1, 0));

      declare
         --  Mimic wisitoken-syntax_trees.adb Pop
         Cur_2 : Cursor := Cur_1;
      begin
         Integer_Val.Check_Ref_Counts ("4a ref_counts", List, (0, 0, 2, 0));

         Cur_1 := Previous (@);
         List.Delete (Cur_2);
         Integer_Val.Check_Ref_Counts ("4b ref_counts", List, (0, 1, 0));
      end;

      Next (Cur_1);
      Integer_Val.Check_Ref_Counts ("5 ref_counts", List, (0, 0, 1));

      Cur_1 := Next (Cur_1);
      Check ("6a", Has_Element (Cur_1), False);
      Integer_Val.Check_Ref_Counts ("6 ref_counts", List, (0, 0, 0));

   end Test_Prev_Next;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Insert'Access, "Test_Insert");
      Register_Routine (T, Test_Delete'Access, "Test_Delete");
      Register_Routine (T, Detect_Dangling'Access, "Detect_Dangling");
      Register_Routine (T, Test_Prev_Next'Access, "Test_Prev_Next");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_doubly_linked_lists_ref_count.adb");
   end Name;

end Test_Definite_Doubly_Linked_Lists_Ref_Count;
