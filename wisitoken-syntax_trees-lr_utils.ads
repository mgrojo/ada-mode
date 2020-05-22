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
package WisiToken.Syntax_Trees.LR_Utils is

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
      Terminals    :         in     WisiToken.Base_Token_Array_Access;
      Lexer        :         in     WisiToken.Lexer.Handle;
      Descriptor   :         in     WisiToken.Descriptor_Access_Constant;
      Root         :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID := WisiToken.Invalid_Token_ID)
     return Iterator
   with Pre => (Tree.Is_Nonterm (Root) and then Tree.Has_Children (Root)) and Tree.ID (Root) = List_ID;
   --  The list cannot be empty.

   function Invalid_Iterator (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return Iterator;
   --  First, Last return empty cursor, count returns 0.

   function Is_Invalid (Iter : in Iterator) return Boolean;

   function To_Cursor (Iter : in Iterator; Node : in Valid_Node_Index) return Cursor;
   --  If assertions enabled, checks that Node is child of Iter.Root, and
   --  Tree.ID (Node) = Iter.Element_ID.

   function Tree (Iter : in Iterator) return Tree_Variable_Reference;
   function Root (Iter : in Iterator) return Node_Index;
   function Count (Iter : in Iterator) return Ada.Containers.Count_Type;

   procedure Delete (Iter : in Iterator; Item : in out Cursor);
   --  Delete Item from Iter list. On return, Item points to next list
   --  element.

   function Compatible (A, B : in Iterator) return Boolean;
   function Contains (Iter : in Iterator; Item : in Cursor) return Boolean;

   procedure Append_Copy
     (Source_Iter : in     Iterator;
      Source_Item : in     Cursor;
      Dest_Iter   : in out Iterator)
   with Pre => Compatible (Source_Iter, Dest_Iter) and Source_Iter.Contains (Source_Item);
   --  Append a copy of Source_Item to Dest_Iter. New node is
   --  Dest_Iter.Root.

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

   function List_Root (Tree : in Syntax_Trees.Tree; Item : in Valid_Node_Index) return Valid_Node_Index;
   --  Return last parent of Item where ID (Parent) = ID (Item).
   --
   --  Using the example list above, List_Root (Tree.Parent (First)) => Root.

private
   use all type WisiToken.Lexer.Handle;

   type Cursor is record
      Node : Node_Index;
   end record;

   No_Element : constant Cursor := (Node => Invalid_Node_Index);

   type Iterator (Tree  : not null access WisiToken.Syntax_Trees.Tree) is new Iterator_Interfaces.Reversible_Iterator
   with record
      Terminals         : WisiToken.Base_Token_Array_Access;
      Lexer             : WisiToken.Lexer.Handle;
      Descriptor        : WisiToken.Descriptor_Access_Constant;
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

   function Tree (Iter : in Iterator) return Tree_Variable_Reference
   is (Element => Iter.Tree);

   function Root (Iter : in Iterator) return Node_Index
   is (Iter.Root);

   function Compatible (A, B : in Iterator) return Boolean
   is --  Either A or B is Invalid_Iterator, or all components are the same except Root
     (A.Tree = B.Tree and
        ((A.Root = Invalid_Node_Index or B.Root = Invalid_Node_Index) or else
           (A.Terminals = B.Terminals and
              A.Lexer = B.Lexer and
              A.Descriptor = B.Descriptor and
              A.List_ID = B.List_ID and
              A.One_Element_RHS = B.One_Element_RHS and
              A.Multi_Element_RHS = B.Multi_Element_RHS and
              A.Element_ID = B.Element_ID and
              A.Separator_ID = B.Separator_ID)));

   function Contains (Iter : in Iterator; Item : in Cursor) return Boolean
   is (for some I in Iter => I.Node = Item.Node);

end WisiToken.Syntax_Trees.LR_Utils;
