--  Abstract :
--
--  Syntax tree type and operations
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.Semantic_Checks;
with WisiToken.Semantic_State;
package WisiToken.Syntax_Trees is

   type Tree is tagged private;

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   No_Node_Index : constant Node_Index := Node_Index'First;

   package Node_Index_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Valid_Node_Index);

   function Add_Nonterm
     (Tree    : aliased in out Syntax_Trees.Tree;
      Nonterm :         in     Token_ID;
      Action  :         in     WisiToken.Semantic_State.Semantic_Action := null;
      Check   :         in     WisiToken.Semantic_Checks.Semantic_Check := null)
   return Valid_Node_Index;
   --  Add a new Nonterm node with no parent. Result points to the added
   --  node.

   function Add_Terminal
     (Tree     : aliased in out Syntax_Trees.Tree;
      Terminal :         in     Positive_Index_Type)
     return Valid_Node_Index;
   --  Add a new Terminal node with no parent. Terminal must be an index
   --  into an external Base_Token_Array. Result points to the added
   --  node.

   procedure Set_Child
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
   with Pre => not Tree.Has_Parent (Child);
   --  Set Child as a child of Parent.
   --  Neither Child nor Parent can be No_Element.
   --  Child.Parent must be unset.

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Node_Index_Arrays.Vector)
   with Pre => not (Tree.Has_Children (Parent) or Tree.Has_Parent (Children));
   --  Set Child as a child of Parent.
   --  Neither Child nor Parent can be No_Element.
   --  Child.Parent must be unset.

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Node_Index_Arrays.Vector) return Boolean;

   ----------
   --  Cursors

   type Cursor is private;

   No_Element : constant Cursor;

private

   type Node is record
      Parent   : Node_Index                               := No_Node_Index;
      Terminal : SAL.Base_Peek_Type                       := Base_Token_Arrays.No_Index;
      Nonterm  : Token_ID                                 := Invalid_Token_ID;
      Children : Node_Index_Arrays.Vector;
      Action   : WisiToken.Semantic_State.Semantic_Action := null;
      Check    : WisiToken.Semantic_Checks.Semantic_Check := null;

      --  Node attributes used in semantic checks
      Name : Buffer_Region := Null_Buffer_Region;
   end record;

   package Node_Arrays is new Ada.Containers.Vectors (Valid_Node_Index, Node);

   type Tree is tagged record
      Root  : Node_Index := No_Node_Index;
      Nodes : Node_Arrays.Vector;
   end record;
   type Tree_Access is access all Tree;

   type Cursor is record
      Tree  : Tree_Access;
      Index : Node_Index;
   end record;

   No_Element : constant Cursor := (Tree => null, Index => No_Node_Index);
end WisiToken.Syntax_Trees;
