--  Abstract :
--
--  Generic unbounded red-black tree with definite elements.
--
--  References :
--
--  [1] Introduction to Algorithms, Thomas H. Cormen, Charles E.
--  Leiserson, Ronald L. Rivest, Clifford Stein.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
   type Key_Type is private;
   with function Key (Element : in Element_Type) return Key_Type is <>;
   with function "<" (Left, Right : in Key_Type) return Boolean is <>;
package SAL.Gen_Unbounded_Definite_Red_Black_Trees is

   package Pkg renames Gen_Unbounded_Definite_Red_Black_Trees;

   type Tree is new Ada.Finalization.Limited_Controlled with private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Variable_Reference,
     Default_Iterator  => Ascending_Order,
     Iterator_Element  => Element_Type;

   overriding procedure Finalize (Object : in out Tree);

   type Cursor is private;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Constant_Reference_Type;

   function Constant_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Constant_Reference_Type;

   type Variable_Reference_Type (Element : not null access Element_Type) is null record
   with Implicit_Dereference => Element;

   function Variable_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Variable_Reference_Type;

   function Variable_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Variable_Reference_Type;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Ascending_Order (Tree : in Pkg.Tree) return Iterators.Forward_Iterator'Class;

   function Count (Tree : in Pkg.Tree) return Ada.Containers.Count_Type;
   procedure Insert (Tree : in out Pkg.Tree; Element : in Element_Type);

private

   type Node;
   type Node_Access is access Node;

   type Color is (Red, Black);

   type Node is record
      Element : aliased Element_Type;
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Pkg.Color;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   type Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Access;
   end record;

   type Cursor is record
      Node       : Node_Access;
      Left_Done  : Boolean;
      Right_Done : Boolean;
   end record;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees;
