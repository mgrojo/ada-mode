--  Abstract :
--
--  Syntax trees that are branched from a base tree, storing only new
--  data but referencing the old, optimized for using during error
--  recovery.
--
--  All operations are affected by the Flush mode; if Flush mode is
--  True, new nodes are written to Tree.Shared_Tree.Nodes, and nodes
--  in Shared_Tree.Nodes may be modified. If False, new nodes are
--  written to Tree.Branched_Nodes, and nodes in Shared_Tree.Nodes
--  that must be modified are copied to Branched_Nodes first.
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
      Shared_Tree   : in     Syntax_Trees.Tree_Access;
      Flush         : in     Boolean);
   --  Set Branched_Tree to refer to Shared_Tree.

   overriding procedure Clear (Tree : in out Branched.Tree);

   procedure Flush (Tree : in out Branched.Tree);
   --  Move all nodes in branched part to shared tree, set Flush mode
   --  True.

   procedure Set_Flush_False (Tree : in out Branched.Tree);
   --  Set Flush mode False; use Flush to set True.

   function Add_Nonterm
     (Tree       : in out Branched.Tree;
      Nonterm    : in     WisiToken.Token_ID;
      Action     : in     Semantic_Action;
      Production : in     Positive;
      Name_Index : in     Natural;
      Children   : in     Valid_Node_Index_Array)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Nonterm node. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node with no parent. Terminal must be an index
   --  into Tree.Terminals. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new virtual terminal node with no parent. Result points to
   --  the added node.

   procedure Set_State
     (Tree  : in out Branched.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index);

   function State (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Unknown_State_Index;

   overriding function Label (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Node_Label;

   overriding
   function Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Branched_Nodes (Tree : in Branched.Tree) return Boolean;
   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Virtual (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Traversing (Tree : in Branched.Tree) return Boolean;

   overriding function Parent (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Node_Index;

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

   function Same_Token
     (Tree_1  : in Branched.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Branched.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean;
   --  True if the two tokens have the same ID and Byte_Region.

   function Recover_Token
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   function Recover_Token_Array
     (Tree  : in Branched.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   overriding
   function Augmented_Token_Ref
     (Tree                : in out Branched.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref;

   overriding
   function Constant_Aug_Token_Ref
     (Tree                : in Branched.Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
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
   function Name_Index
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Find_Ancestor
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;

   overriding
   function Find_Sibling
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Has_Parent (Node);

   overriding
   function Find_Child
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);

   procedure Process_Tree
     (Tree         : in out Branched.Tree;
      Process_Node : access procedure
        (Tree : in out Branched.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node.

   function Min_Shared_Terminal_Index (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   --  Returns lowest index of shared terminal in subtree under Node. If
   --  result is Invalid_Token_Index, all terminals are virtual.

   overriding
   function Image
     (Tree       : in Branched.Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

private

   --  We use the same Node type as parent, to simplify moving the branch
   --  point, and to support future use of branched trees in shared
   --  parsers. We don't need protected node arrays; these are only
   --  accessed from one task at a time.

   type Tree is new Abstract_Tree with record
      Shared_Tree : Syntax_Trees.Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead, unless Flush = True.

      Last_Shared_Node : Node_Index := No_Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
      Flush            : Boolean    := False;
      --  We maintain Last_Shared_Node when Flush is True, so subprograms
      --  that have no reason to check Flush can rely on Last_Shared_Node.
   end record with
     Type_Invariant => (if Tree.Flush then not Tree.Has_Branched_Nodes);

end WisiToken.Syntax_Trees.Branched;
