--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Checks;
with SAL.Gen_Unbounded_Sparse_Ordered_Sets;
package body Test_Sparse_Ordered_Sets is

   function Compare (Left, Right : in Integer) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left < Right then SAL.Less
      else SAL.Equal);

   package Sets is new SAL.Gen_Unbounded_Sparse_Ordered_Sets
     (Index_Type    => Integer,
      Index_Compare => Compare);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use Sets;
      Set : Sets.Set;

      Count : Integer := 0;
   begin
      --  Just check that basic operations work.
      Set.Insert (26);
      Set.Insert (9);
      Set.Insert (61);

      Set.Delete (9);

      --  WisiToken encountered a 'null' value in Next.
      for E of Set loop
         Count := @ + 1;
      end loop;
      Check ("count", Count, 2);

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
      return new String'("test_sparse_ordered_sets.adb");
   end Name;

end Test_Sparse_Ordered_Sets;
