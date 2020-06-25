--  Abstract:
--
--  Generic Hash Table, using red-black trees for collisions.
--
--  Design
--
--  We assume computing Key from Element is free (for example, Element
--  is (Key, Index to actual store)), and computing Hash from Key is
--  cheap. Hashes are recomputed for all elements when the table is
--  grown.
--
--  References
--
--  [1] Prime numbers http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
--
--  Notice
--
--  Copyright (C) 2020 Free Software Foundation, Inc. All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
private with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
private with SAL.Gen_Unbounded_Definite_Vectors;
generic
   type Element_Type is private;
   type Key_Type (<>) is private;
   with function Key (Element : in Element_Type) return Key_Type is <>;
   with function Key_Compare (Left, Right : in Key_Type) return Compare_Result;

   with function Hash (Key : Key_Type; Rows : Positive) return Positive
     with Post => Hash'Result in 1 .. Rows;
   --  1 + (Some_hash (Key) mod Rows) works.

   Default_Init_Rows : Positive := 113;

package SAL.Gen_Unbounded_Definite_Hash_Table is

   package Pkg renames Gen_Unbounded_Definite_Hash_Table;

   type Hash_Table is tagged private;

   procedure Set_Rows
     (Table : in out Hash_Table;
      Rows  : in     Positive);
   --  Set the hash table size. If Table is not empty, all hashes are
   --  recomputed; this renders any Constant_Refs invalid.

   procedure Insert
     (Table   : in out Hash_Table;
      Element : in     Element_Type);
   --  Raises Duplicate_Key if Key (Element) is already in Table

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Find_Or_Insert
     (Table   : in out Hash_Table;
      Element : in     Element_Type;
      Found   :    out Boolean)
     return Constant_Reference_Type;

   function Constant_Ref
     (Table : aliased in Hash_Table;
      Key   :         in Key_Type)
     return Constant_Reference_Type;
   --  Raises SAL.Not_Found if Key is not found

   procedure Sizes
     (Table             : in     Hash_Table;
      Elements          :    out Ada.Containers.Count_Type;
      Rows              :    out Integer;
      Max_Row_Depth     :    out Ada.Containers.Count_Type;
      Average_Row_Depth :    out Ada.Containers.Count_Type);

private

   package Element_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type, Key_Type, Key, Key_Compare);
   --  Holds elements for a row

   package Hash_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Element_Trees.Tree, Element_Trees.Empty_Tree);

   type Hash_Table is tagged record
      --  Directly deriving Hash_Table from Hash_Arrays.Vector would mean we
      --  have to implement Iterate.
      Table : Hash_Arrays.Vector;
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

end SAL.Gen_Unbounded_Definite_Hash_Table;
