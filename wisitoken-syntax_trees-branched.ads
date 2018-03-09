--  Abstract :
--
--  Syntax trees that are branched from a base tree, storing only new
--  data but referencing the old, optimized for using during error
--  recovery.
--
--  Rationale :
--
--  There is one branched tree for each recover configuration. The
--  inserted terminals are stored in the branched tree, since they are
--  not shared with any other configuration.
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

package WisiToken.Syntax_Trees.Branched is

   type Tree is new Abstract_Tree with private;

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Syntax_Trees.Tree_Access);
   --  Set Branched_Tree to refer to Shared_Tree.

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Branched.Tree);
   --  Set Branched_Tree to refer to Shared_Tree.Shared_Tree, copy all
   --  branched nodes to Branched_Tree.

   procedure Flush (Tree : in out Branched.Tree);
   --  Move all nodes in branched part to shared tree.

   function Add_Nonterm
     (Tree         : in out Branched.Tree;
      Nonterm      : in     WisiToken.Token_ID;
      Flush        : in     Boolean;
      Virtual      : in     Boolean         := False;
      Action       : in     Semantic_Action := null;
      Action_Index : in     Natural         := 0)
   return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Nonterm node with no parent. If Flush, add the new node
   --  to the shared tree. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_Index;
      Flush    : in     Boolean := False)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node with no parent. Terminal must be an index
   --  into Tree.Terminals. If Flush, add the new node to the shared
   --  tree. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_ID;
      Flush    : in     Boolean := False)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new virtual terminal node with no parent. If Flush, add the
   --  new node to the shared tree. Result points to the added node.

   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array;
      Flush    : in     Boolean)
   with Pre => not Tree.Traversing and
               (if Flush then not Tree.Has_Branched_Nodes) and
               (Tree.Is_Nonterm (Parent) and then (not (Tree.Has_Children (Parent) or Tree.Has_Parent (Children))));
   --  Set the Children of Parent. If Flush, leave all nodes in the
   --  shared tree. If not Flush, copy nodes to branched tree before
   --  modifying.

   overriding
   function Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Branched_Nodes (Tree : in Branched.Tree) return Boolean;
   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Traversing (Tree : in Branched.Tree) return Boolean;

   overriding
   function Byte_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region;

   overriding
   function Name_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region;

   overriding
   procedure Set_Name_Region
     (Tree   : in out Branched.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function ID
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   overriding
   function Base_Token
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token;

   overriding
   function Augmented_Token_Ref
     (Tree                : in out Branched.Tree;
      Node                : in     Valid_Node_Index;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector)
     return Augmented_Ref;

   overriding
   function Constant_Aug_Token_Ref
     (Tree                : in Branched.Tree;
      Node                : in Valid_Node_Index;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector)
     return Constant_Augmented_Ref;

   overriding
   function Augmented_Token_Array
     (Tree                : in out Branched.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array;

   overriding
   function Virtual
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   overriding
   function Action
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Action_Index
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   procedure Process_Tree
     (Tree         : in out Branched.Tree;
      Process_Node : access procedure
        (Tree : in out Branched.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node.

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index)
   with Pre => not Tree.Traversing and not Tree.Has_Parent (Node);
   --  Delete subtree rooted at Node.
   --
   --  If Node is in shared tree, moves branch point to Node.

   function Get_Terminals (Tree : in Branched.Tree; Node : in Valid_Node_Index) return WisiToken.Base_Token_Array;
   --  Return all terminals in tree rooted at Node, in depth-first order.
   --  FIXME: only need IDs?

private

   --  We use the same Node type as parent, to simplify moving the branch
   --  point, and to support future use of branched trees in shared
   --  parsers. We don't need protected node arrays; these are only
   --  accessed from one task at a time.

   type Tree is new Abstract_Tree with record
      Shared_Tree : Syntax_Trees.Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead.

      Last_Shared_Node : Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
   end record;

end WisiToken.Syntax_Trees.Branched;
