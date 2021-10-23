--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2020, 2021 Stephen Leake.  All Rights Reserved.
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

with SAL.Gen_Unbounded_Definite_Hash_Tables;
package body Test_Unbounded_Definite_Hash_Tables
is

   --  Just check that it compiles
   function Identity (Element : in Integer) return Integer
   is (Element);

   function Twice (Key : in Integer; Rows : in Positive) return Positive
   is (1 + (2 * Key) mod Rows);

   function Integer_Compare is new SAL.Gen_Compare_Integer (Integer);

   package Integer_Hash_Tables is new SAL.Gen_Unbounded_Definite_Hash_Tables
     (Element_Type => Integer,
      Key_Type     => Integer,
      Key          => Identity,
      Key_Compare  => Integer_Compare,
      Hash         => Twice);
   pragma Unreferenced (Integer_Hash_Tables);

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is begin
      null;
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_unbounded_definite_hash_tables.adb");
   end Name;

end Test_Unbounded_Definite_Hash_Tables;
