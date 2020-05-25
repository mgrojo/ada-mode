--  Abstract :
--
--  Utilities for navigating syntax trees produced by an LR parser.
--
--  Design :
--
--  It would be safer if Cursor contained a pointer to Iterator; then
--  Copy and Splice could just take Cursor arguments. But that
--  requires mode 'aliased in' for First, Last, which is not
--  conformant with Ada.Iterator_Interfaces.
--
--  Copyright (C) 2019, 2020 Stephen Leake All Rights Reserved.
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

with Ada.Iterator_Interfaces;
with SAL.Gen_Unconstrained_Array_Image_Aux;
package WisiToken.Syntax_Trees.LR_Utils is
   use all type SAL.Base_Peek_Type;

   procedure Raise_Programmer_Error
     (Label      : in String;
      Descriptor : in WisiToken.Descriptor;
      Lexer      : in WisiToken.Lexer.Handle;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Terminals  : in WisiToken.Base_Token_Arrays.Vector;
      Node       : in WisiToken.Node_Index);
   pragma No_Return (Raise_Programmer_Error);

   ----------
   --  List functions
   --
   --  A list has one of the following grammar forms:
   --
   --  list : list element | element ;
   --  list : element | list element ;
   --
   --  list : list separator element | element ;
   --  list : element | list separator element ;
   --
   --  In the syntax tree, this looks like:
   --
   --  list: Root
   --  | list
   --  | | list
   --  | | | element: First
   --  | | separator
   --  | | element: 2
   --  | separator
   --  | element: 3
   --  separator
   --  element: Last

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Cursor : in LR_Utils.Cursor) return Boolean;

   function Node (Cursor : in LR_Utils.Cursor) return Node_Index;
   --  Invalid_Node_Index if not Has_Element (Cursor).

   function Get_Node (Cursor : in LR_Utils.Cursor) return Node_Index
     renames Node;
   --  Useful when Node is hidden by another declaration.

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator (<>) is new Iterator_Interfaces.Reversible_Iterator with private;

   overriding function First (Iter : in Iterator) return Cursor;
   overriding function Last  (Iter : in Iterator) return Cursor;
   overriding function Next     (Iter : in Iterator; Position : Cursor) return Cursor;
   overriding function Previous (Iter : in Iterator; Position : Cursor) return Cursor;

   function Iterate
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Root         :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return Iterator
   with Pre => (Tree.Is_Nonterm (Root) and then Tree.Has_Children (Root)) and Tree.ID (Root) = List_ID;
   --  If there is no separator, set Separator_ID = WisiToken.Invalid_Token_ID
   --  The list cannot be empty; use Empty_Iterator for an empty list.

   function Iterate_Constant
     (Tree       : aliased in out WisiToken.Syntax_Trees.Tree;
      Root       :         in     Valid_Node_Index;
      List_ID    :         in     WisiToken.Token_ID;
      Element_ID :         in     WisiToken.Token_ID)
     return Iterator
   with Pre => (Tree.Is_Nonterm (Root) and then Tree.Has_Children (Root)) and Tree.ID (Root) = List_ID;
   --  The separator is only need when adding new elements. We'd like to
   --  make Tree 'aliased in', but that would violate the discrimant
   --  type.

   function Iterate (Iter : in Iterator; Root : in Valid_Node_Index) return Iterator
   with Pre => (Iter.Tree.Is_Nonterm (Root) and then Iter.Tree.Has_Children (Root)) and
               Iter.Tree.ID (Root) = Iter.List_ID;
   --  Same as Iterate, get all other params from Iter.

   function Iterate_From_Element
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Element      :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return Iterator
   with Pre => Tree.ID (Tree.Parent (Element)) = List_ID and
               Tree.ID (Element) = Element_ID and
               Tree.ID (Tree.Parent (Element)) = List_ID;
   --  Same as Iterate, but it first finds the root as an ancestor of
   --  Element.

   function Iterate_From_Element (Iter : in Iterator; Element : in Valid_Node_Index) return Iterator
   with Pre => Iter.Tree.ID (Iter.Tree.Parent (Element)) = Iter.List_ID and
               Iter.Tree.ID (Element) = Iter.Element_ID and
               Iter.Tree.ID (Iter.Tree.Parent (Element)) = Iter.List_ID;
   --  Same as Iterate_From_Element, get all other params from Iter.

   function Invalid_Iterator (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return Iterator;
   --  First, Last return empty cursor, count returns 0, all other
   --  operations fail a precondition check.
   --
   --  Useful when the result should never be used, but must be present,
   --  as in a conditional expression.

   function Is_Invalid (Iter : in Iterator) return Boolean;

   function Empty_Iterator
     (Tree              : aliased in out WisiToken.Syntax_Trees.Tree;
      List_ID           :         in     WisiToken.Token_ID;
      Multi_Element_RHS :         in     Natural;
      Element_ID        :         in     WisiToken.Token_ID;
      Separator_ID      :         in     WisiToken.Token_ID)
     return Iterator;
   --  Result Root returns Invalid_Node_Index; First, Last return empty
   --  cursor, count returns 0; Append works correctly.

   function Empty_Iterator (Iter : in Iterator) return Iterator;
   --  Same as Empty_Iterator, get all params from Iter.

   function Is_Empty (Iter : in Iterator) return Boolean;
   --  Returns True if Iter is invalid, or if Iter is empty

   function To_Cursor (Iter : in Iterator; Node : in Valid_Node_Index) return Cursor
   with Pre => (not Iter.Is_Invalid) and then (Iter.Contains (Node) and Iter.Tree.ID (Node) = Iter.Element_ID);

   function Tree (Iter : in Iterator) return Tree_Variable_Reference
   with Pre => not Iter.Is_Invalid;

   function Root (Iter : in Iterator) return Node_Index
   with Pre => not Iter.Is_Invalid;

   function List_ID (Iter : in Iterator) return Token_ID
   with Pre => not Iter.Is_Invalid;

   function Element_ID (Iter : in Iterator) return Token_ID
   with Pre => not Iter.Is_Invalid;

   function Separator_ID (Iter : in Iterator) return Token_ID
   with Pre => not Iter.Is_Invalid;

   function Count (Iter : in Iterator) return Ada.Containers.Count_Type
   with Pre => not Iter.Is_Invalid;

   function Compatible (A, B : in Iterator) return Boolean;
   --  True if A and B are not invalid, and all components are the same
   --  except Root.

   function Contains (Iter : in Iterator; Node : in Valid_Node_Index) return Boolean
   with Pre => not Iter.Is_Invalid;

   function Contains (Iter : in Iterator; Item : in Cursor) return Boolean
   with Pre => not Iter.Is_Invalid;

   procedure Append
     (Iter        : in out Iterator;
      New_Element : in     Valid_Node_Index)
   with Pre => not Iter.Is_Invalid and then Iter.Tree.ID (New_Element) = Iter.Element_ID;
   --  Append New_Item to Iter list, including Iter.Separator_ID if it is not
   --  Invalid_Token_Index.
   --
   --  If Iter was Empty, or if Iter.Root has no parent in Tree, or if
   --  the parent has ID of Invalid_Token_ID (meaning Clear_Children was
   --  called on it), or if the parent is not a nonterm (meaning
   --  Set_Node_Identifier was called on it), the new list has no parent.
   --  Otherwise, the parent of Iter.Root is updated to hold the new
   --  Iter.Root.

   procedure Insert
     (Iter        : in out Iterator;
      New_Element : in     Valid_Node_Index;
      After       : in     Cursor)
   with Pre => not Iter.Is_Invalid and then
               (Iter.Tree.ID (New_Element) = Iter.Element_ID and
                Iter.Contains (After));
   --  Insert New_Item into Iter list after Ater, including
   --  Iter.Separator_ID if it is not Invalid_Token_Index.
   --
   --  If Iter was Empty, or if Iter.Root has no parent in Tree, or if
   --  the parent has ID of Invalid_Token_ID (meaning Clear_Children was
   --  called on it), or if the parent is not a nonterm (meaning
   --  Set_Node_Identifier was called on it), the new list has no parent.
   --  Otherwise, if After is Iter.Last, the parent of Iter.Root is
   --  updated to hold the new Iter.Root.

   procedure Copy
     (Source_Iter  : in     Iterator;
      Source_First : in     Cursor := No_Element;
      Source_Last  : in     Cursor := No_Element;
      Dest_Iter    : in out Iterator)
   with Pre => Compatible (Source_Iter, Dest_Iter);
   --  Deep copy slice of Source_Iter, appending to Dest.
   --
   --  If First = No_Element, copy from List.First.
   --  If Last = No_Element, copy thru List.Last.

   type Skip_Label is (Nested, Skip);

   type Skip_Item (Label : Skip_Label := Skip_Label'First) is
   record
      Element : Valid_Node_Index;
      case Label is
      when Nested =>
         --  Element is an element in the list currently being copied
         --  containing a nested list with an element to skip. The nested list
         --  is defined by:
         List_Root    : Valid_Node_Index;
         List_ID      : Token_ID;
         Element_ID   : Token_ID;
         Separator_ID : Token_ID;

      when Skip =>
         --  Element is the element in the current list to skip.
         null;
      end case;
   end record;
   subtype Nested_Skip_Item is Skip_Item (Nested);

   function Image (Item : in Skip_Item; Descriptor : in WisiToken.Descriptor) return String
   is ("(" & Item.Label'Image & ", " & Item.Element'Image &
         (case Item.Label is
          when Nested => "," & Item.List_Root'Image & ", " & Image (Item.List_ID, Descriptor),
          when Skip => "") &
         ")");

   type Skip_Array is array (Positive_Index_Type range <>) of Skip_Item;

   function Image is new SAL.Gen_Unconstrained_Array_Image_Aux
     (Positive_Index_Type, Skip_Item, Skip_Array, WisiToken.Descriptor, Image);

   function Valid_Skip_List (Tree : aliased in out Syntax_Trees.Tree; Skip_List : in Skip_Array) return Boolean;
   --  The last element must be Skip, preceding elements must all be
   --  Nested. The Element in each array element must have ID = preceding
   --  Element_ID, and the list described by each element must have more
   --  than one item (so it is not empty when skipped).

   function Copy_Skip_Nested
     (Source_Iter : in     Iterator;
      Skip_List   : in     Skip_Array;
      Skip_Found  : in out Boolean)
     return Valid_Node_Index
   with Pre => Valid_Skip_List (Source_Iter.Tree, Skip_List);
   --  Copy list in Source_Iter, skipping one element as indicated by
   --  Skip_List. Return root of copied list.

   procedure Splice
     (Left_Iter   : in out Iterator;
      Left_Last   : in     Cursor;
      Right_Iter  : in     Iterator;
      Right_First : in     Cursor)
   with
     Pre  => Compatible (Left_Iter, Right_Iter) and
             Right_Iter.Contains (Right_First) and
             Left_Iter.Contains (Left_Last);
   --  Splice Right_Iter list starting at Right_First onto tail of
   --  Left_Iter list, removing the slice from Right_Iter.
   --  Left_Iter.Root is updated to the new list root.
   --
   --  On return, the slice of Left_Iter after Left_Last is inaccessible;
   --  any cursors to it are invalid (this is not enforced), and it has
   --  no proper list root.

   function List_Root
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Index;
      List_ID : in Token_ID)
     return Valid_Node_Index
   with Pre => Tree.ID (Node) = List_ID;

private
   type Cursor is record
      Node : Node_Index;
   end record;

   No_Element : constant Cursor := (Node => Invalid_Node_Index);

   type Iterator (Tree  : not null access WisiToken.Syntax_Trees.Tree) is new Iterator_Interfaces.Reversible_Iterator
   with record
      Root              : WisiToken.Node_Index;
      List_ID           : WisiToken.Token_ID;
      One_Element_RHS   : Natural;
      Multi_Element_RHS : Natural;
      Element_ID        : WisiToken.Token_ID;
      Separator_ID      : WisiToken.Token_ID;
   end record;

   function Has_Element (Cursor : in LR_Utils.Cursor) return Boolean
   is (Cursor.Node /= Invalid_Node_Index);

   function Node (Cursor : in LR_Utils.Cursor) return Node_Index
   is (Cursor.Node);

   function Is_Invalid (Iter : in Iterator) return Boolean
   is (Iter.List_ID = Invalid_Token_ID);

   function Is_Empty (Iter : in Iterator) return Boolean
   is (Iter.Root = Invalid_Node_Index);

   function Tree (Iter : in Iterator) return Tree_Variable_Reference
   is (Element => Iter.Tree);

   function Root (Iter : in Iterator) return Node_Index
   is (Iter.Root);

   function List_ID (Iter : in Iterator) return Token_ID
   is (Iter.List_ID);

   function Element_ID (Iter : in Iterator) return Token_ID
   is (Iter.Element_ID);

   function Separator_ID (Iter : in Iterator) return Token_ID
   is (Iter.Separator_ID);

   function Compatible (A, B : in Iterator) return Boolean
   is
     (A.Tree = B.Tree and
        A.List_ID /= Invalid_Token_ID and
        B.List_ID /= Invalid_Token_ID and
        A.List_ID = B.List_ID and
        A.One_Element_RHS = B.One_Element_RHS and
        A.Multi_Element_RHS = B.Multi_Element_RHS and
        A.Element_ID = B.Element_ID and
        A.Separator_ID = B.Separator_ID);

   function Contains (Iter : in Iterator; Node : in Valid_Node_Index) return Boolean
   is (for some I in Iter => I.Node = Node);

   function Contains (Iter : in Iterator; Item : in Cursor) return Boolean
   is (Contains (Iter, Item.Node));

end WisiToken.Syntax_Trees.LR_Utils;
