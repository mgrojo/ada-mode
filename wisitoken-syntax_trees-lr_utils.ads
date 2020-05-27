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

   type List (<>) is tagged private with
     Constant_Indexing => List_Constant_Ref,
     Default_Iterator  => Iterate,
     Iterator_Element  => Valid_Node_Index;

   type Constant_List (<>) is tagged private with
     Constant_Indexing => Constant_List_Constant_Ref,
     Default_Iterator  => Iterate_Constant,
     Iterator_Element  => Valid_Node_Index;

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Cursor : in LR_Utils.Cursor) return Boolean;

   function Node (Cursor : in LR_Utils.Cursor) return Node_Index;
   --  Invalid_Node_Index if not Has_Element (Cursor).

   function Get_Node (Cursor : in LR_Utils.Cursor) return Node_Index
     renames Node;
   --  Useful when Node is hidden by another declaration.

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator (Container : not null access constant List) is new Iterator_Interfaces.Reversible_Iterator
     with null record;

   function First (Container : in List) return Cursor;
   function Last  (Container : in List) return Cursor;

   overriding function First (Iter : in Iterator) return Cursor is (Iter.Container.First);
   overriding function Last  (Iter : in Iterator) return Cursor is (Iter.Container.Last);
   overriding function Next     (Iter : in Iterator; Position : Cursor) return Cursor;
   overriding function Previous (Iter : in Iterator; Position : Cursor) return Cursor;

   function List_Constant_Ref (Container : aliased in List; Position : in Cursor) return Valid_Node_Index;

   function Iterate (Container : aliased in List'Class) return Iterator
   is (Iterator_Interfaces.Reversible_Iterator with Container'Access);

   type Constant_Iterator (Container : not null access constant Constant_List) is new
     Iterator_Interfaces.Reversible_Iterator
     with null record;

   function First (Container : in Constant_List) return Cursor;
   function Last  (Container : in Constant_List) return Cursor;

   overriding function First (Iter : in Constant_Iterator) return Cursor is (Iter.Container.First);
   overriding function Last  (Iter : in Constant_Iterator) return Cursor is (Iter.Container.Last);
   overriding function Next     (Iter : in Constant_Iterator; Position : Cursor) return Cursor;
   overriding function Previous (Iter : in Constant_Iterator; Position : Cursor) return Cursor;

   function Constant_List_Constant_Ref
     (Container : aliased in Constant_List;
      Position  :         in Cursor)
     return Valid_Node_Index;
   function Constant_Ref (Container : aliased in Constant_List; Position : in Cursor) return Valid_Node_Index
     renames Constant_List_Constant_Ref;

   function Iterate_Constant (Container : aliased in Constant_List'Class) return Constant_Iterator
   is (Iterator_Interfaces.Reversible_Iterator with Container'Access);

   function Create_List
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Root         :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return List
   with Pre => (Tree.Is_Nonterm (Root) and then Tree.Has_Children (Root)) and Tree.ID (Root) = List_ID;
   --  If there is no separator, set Separator_ID = WisiToken.Invalid_Token_ID
   --  The list cannot be empty; use Empty_List for an empty list.

   function Create_Constant_List
     (Tree       : aliased in WisiToken.Syntax_Trees.Tree;
      Root       :         in Valid_Node_Index;
      List_ID    :         in WisiToken.Token_ID;
      Element_ID :         in WisiToken.Token_ID)
     return Constant_List
   with Pre => (Tree.Is_Nonterm (Root) and then Tree.Has_Children (Root)) and Tree.ID (Root) = List_ID;
   --  The separator is only need when adding new elements.

   function Create_List (Container : in List; Root : in Valid_Node_Index) return List
   with Pre => (Container.Tree.Is_Nonterm (Root) and then Container.Tree.Has_Children (Root)) and
               Container.Tree.ID (Root) = Container.List_ID;
   --  Same as Create_List, get all other params from Container.

   function Create_From_Element
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Element      :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return List
   with Pre => Tree.ID (Tree.Parent (Element)) = List_ID and
               Tree.ID (Element) = Element_ID and
               Tree.ID (Tree.Parent (Element)) = List_ID;
   --  Same as Create_List, but it first finds the root as an ancestor of
   --  Element.

   function Create_From_Element (Container : in List; Element : in Valid_Node_Index) return List
   with Pre => Container.Tree.ID (Container.Tree.Parent (Element)) = Container.List_ID and
               Container.Tree.ID (Element) = Container.Element_ID and
               Container.Tree.ID (Container.Tree.Parent (Element)) = Container.List_ID;
   --  Same as Create_From_Element, get all other params from Container.

   function Create_From_Element
     (Tree       : aliased in WisiToken.Syntax_Trees.Tree;
      Element    :         in Valid_Node_Index;
      List_ID    :         in WisiToken.Token_ID;
      Element_ID :         in WisiToken.Token_ID)
     return Constant_List
   with Pre => Tree.ID (Tree.Parent (Element)) = List_ID and
               Tree.ID (Element) = Element_ID and
               Tree.ID (Tree.Parent (Element)) = List_ID;
   --  Same as Create_List, but it first finds the root as an ancestor of
   --  Element.

   function Invalid_List (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return List;
   function Invalid_List (Tree : aliased in WisiToken.Syntax_Trees.Tree) return Constant_List;
   --  First, Last return empty cursor, count returns 0, all other
   --  operations fail a precondition check.
   --
   --  Useful when the result should never be used, but must be present,
   --  as in a conditional expression.

   function Is_Invalid (Container : in List) return Boolean;
   function Is_Invalid (Container : in Constant_List) return Boolean;

   function Empty_List
     (Tree              : aliased in out WisiToken.Syntax_Trees.Tree;
      List_ID           :         in     WisiToken.Token_ID;
      Multi_Element_RHS :         in     Natural;
      Element_ID        :         in     WisiToken.Token_ID;
      Separator_ID      :         in     WisiToken.Token_ID)
     return List;
   --  Result Root returns Invalid_Node_Index; First, Last return empty
   --  cursor, count returns 0; Append works correctly.

   function Empty_List (Container : in List) return List;
   --  Same as Empty_List, get all params from Container.

   function Is_Empty (Container : in List) return Boolean;
   --  Returns True if Container is invalid, or if Container is empty

   function To_Cursor (Container : in List; Node : in Valid_Node_Index) return Cursor
   with Pre => (not Container.Is_Invalid) and then
               (Container.Contains (Node) and Container.Tree.ID (Node) = Container.Element_ID);

   function To_Cursor (Container : in Constant_List; Node : in Valid_Node_Index) return Cursor
   with Pre => (not Container.Is_Invalid) and then
               (Container.Contains (Node) and Container.Tree.ID (Node) = Container.Element_ID);

   function Tree (Container : in List) return Tree_Variable_Reference
   with Pre => not Container.Is_Invalid;

   function Tree (Container : in Constant_List) return Tree_Constant_Reference
   with Pre => not Container.Is_Invalid;

   function Root (Container : in List) return Node_Index
   with Pre => not Container.Is_Invalid;

   function List_ID (Container : in List) return Token_ID
   with Pre => not Container.Is_Invalid;

   function Element_ID (Container : in List) return Token_ID
   with Pre => not Container.Is_Invalid;

   function Element_ID (Container : in Constant_List) return Token_ID
   with Pre => not Container.Is_Invalid;

   function Separator_ID (Container : in List) return Token_ID
   with Pre => not Container.Is_Invalid;

   function Count (Container : in List) return Ada.Containers.Count_Type
   with Pre => not Container.Is_Invalid;
   function Count (Container : in Constant_List) return Ada.Containers.Count_Type
   with Pre => not Container.Is_Invalid;

   function Compatible (A, B : in List) return Boolean;
   function Compatible (A : in Constant_List; B : in List'Class) return Boolean;
   --  True if A and B are not invalid, and all components are the same
   --  except Root.

   function Contains (Container : in List; Node : in Valid_Node_Index) return Boolean
   with Pre => not Container.Is_Invalid;

   function Contains (Container : in Constant_List; Node : in Valid_Node_Index) return Boolean
   with Pre => not Container.Is_Invalid;

   function Contains (Container : in List; Item : in Cursor) return Boolean
   with Pre => not Container.Is_Invalid;

   procedure Append
     (Container   : in out List;
      New_Element : in     Valid_Node_Index)
   with Pre => not Container.Is_Invalid and then Container.Tree.ID (New_Element) = Container.Element_ID;
   --  Append New_Item to Container, including Container.Separator_ID if
   --  it is not Invalid_Token_Index.
   --
   --  If Container was Empty, or if Container.Root has no parent in
   --  Tree, the modified list has no parent. Otherwise, the parent of
   --  Container.Root is updated to hold the new Container.Root.

   procedure Prepend
     (Container   : in out List;
      New_Element : in     Valid_Node_Index)
   with Pre => not Container.Is_Invalid and then Container.Tree.ID (New_Element) = Container.Element_ID;
   --  Prepend New_Item to Container, including Container.Separator_ID if
   --  it is not Invalid_Token_Index.
   --
   --  Container.Root parent is unchanged.

   procedure Insert
     (Container   : in out List;
      New_Element : in     Valid_Node_Index;
      After       : in     Cursor)
   with Pre => not Container.Is_Invalid and then
               (Container.Tree.ID (New_Element) = Container.Element_ID and
                Container.Contains (After));
   --  Insert New_Item into Container after Ater, including
   --  Container.Separator_ID if it is not Invalid_Token_Index.
   --
   --  If After is No_Element, calls Prepend.
   --
   --  If Container was Empty, or if Container.Root has no parent, the
   --  modified list has no parent. Otherwise, if After is
   --  Container.Last, the parent of Container.Root is updated to hold
   --  the new Container.Root.

   procedure Copy
     (Source_List  : in     Constant_List;
      Source_First : in     Cursor := No_Element;
      Source_Last  : in     Cursor := No_Element;
      Dest_List    : in out List'Class)
   with Pre => Compatible (Source_List, Dest_List);
   --  Deep copy slice of Source_List, appending to Dest_List.
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
         List_Root         : Valid_Node_Index;
         List_ID           : Token_ID;
         Element_ID        : Token_ID;
         Separator_ID      : Token_ID;
         Multi_Element_RHS : Natural;

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

   function Valid_Skip_List (Tree : aliased in Syntax_Trees.Tree; Skip_List : in Skip_Array) return Boolean;
   --  The last element must be Skip, preceding elements must all be
   --  Nested. The Element in each array element must have ID = preceding
   --  Element_ID. The net result of all skips must not be empty, unless
   --  there is only one item (Skip); the containing list may have only
   --  that.

   function Copy_Skip_Nested
     (Source_List       :         in     Constant_List;
      Skip_List         :         in     Skip_Array;
      Tree              : aliased in out Syntax_Trees.Tree;
      Separator_ID      :         in     Token_ID;
      Multi_Element_RHS :         in     Natural)
     return Node_Index
   with Pre => Valid_Skip_List (Source_List.Tree, Skip_List);
   --  Copy list in Source_List, skipping one element as indicated by
   --  Skip_List. Return root of copied list.
   --
   --  Return is Invalid_Node_Index (indicating an empty list) if
   --  Skip_List has only one item (Skip), and Source_List has only that
   --  item.
   --
   --  We need Tree and the following args in addition to Source_List,
   --  because we need write access.
   --
   --  Raises SAL.Programmer_Error if Skip_List is not found.

   procedure Splice
     (Left_List   : in out List;
      Left_Last   : in     Cursor;
      Right_List  : in     List;
      Right_First : in     Cursor)
   with
     Pre  => Compatible (Left_List, Right_List) and
             Right_List.Contains (Right_First) and
             Left_List.Contains (Left_Last);
   --  Splice Right_List starting at Right_First onto tail of
   --  Left_List, removing the slice from Right_List.
   --  Left_List.Root is updated to the new list root.
   --  FIXME: update tree parent
   --
   --  On return, the slice of Left_List after Left_Last is inaccessible;
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

   type List (Tree  : not null access WisiToken.Syntax_Trees.Tree) is tagged
   record
      Root              : WisiToken.Node_Index;
      List_ID           : WisiToken.Token_ID;
      One_Element_RHS   : Natural;
      Multi_Element_RHS : Natural;
      Element_ID        : WisiToken.Token_ID;
      Separator_ID      : WisiToken.Token_ID;
   end record;

   type Constant_List (Tree : not null access constant WisiToken.Syntax_Trees.Tree) is tagged
   record
      Root       : WisiToken.Node_Index;
      List_ID    : WisiToken.Token_ID;
      Element_ID : WisiToken.Token_ID;
   end record;

   function Has_Element (Cursor : in LR_Utils.Cursor) return Boolean
   is (Cursor.Node /= Invalid_Node_Index);

   function Node (Cursor : in LR_Utils.Cursor) return Node_Index
   is (Cursor.Node);

   function Is_Invalid (Container : in List) return Boolean
   is (Container.List_ID = Invalid_Token_ID);

   function Is_Invalid (Container : in Constant_List) return Boolean
   is (Container.List_ID = Invalid_Token_ID);

   function Is_Empty (Container : in List) return Boolean
   is (Container.Root = Invalid_Node_Index);

   function Tree (Container : in List) return Tree_Variable_Reference
   is (Element => Container.Tree);

   function Tree (Container : in Constant_List) return Tree_Constant_Reference
   is (Element => Container.Tree);

   function Root (Container : in List) return Node_Index
   is (Container.Root);

   function List_ID (Container : in List) return Token_ID
   is (Container.List_ID);

   function Element_ID (Container : in List) return Token_ID
   is (Container.Element_ID);

   function Element_ID (Container : in Constant_List) return Token_ID
   is (Container.Element_ID);

   function Separator_ID (Container : in List) return Token_ID
   is (Container.Separator_ID);

   function Compatible (A, B : in List) return Boolean
   is
     (A.Tree = B.Tree and
        A.List_ID /= Invalid_Token_ID and
        B.List_ID /= Invalid_Token_ID and
        A.List_ID = B.List_ID and
        A.One_Element_RHS = B.One_Element_RHS and
        A.Multi_Element_RHS = B.Multi_Element_RHS and
        A.Element_ID = B.Element_ID and
        A.Separator_ID = B.Separator_ID);

   function Compatible (A : in Constant_List; B : in List'Class) return Boolean
   is
     (A.Tree = B.Tree and
        A.List_ID /= Invalid_Token_ID and
        B.List_ID /= Invalid_Token_ID and
        A.List_ID = B.List_ID and
        A.Element_ID = B.Element_ID);

   function Contains (Container : in List; Item : in Cursor) return Boolean
   is (Contains (Container, Item.Node));

end WisiToken.Syntax_Trees.LR_Utils;
