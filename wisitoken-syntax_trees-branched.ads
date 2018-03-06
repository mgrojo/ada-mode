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

   type Shared_Tree_Access is access constant Syntax_Trees.Tree;

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Shared_Tree_Access);
   --  Set Branched_Tree to refer to Shared_Tree.

   overriding
   function Add_Nonterm
     (Tree         : in out Branched.Tree;
      Nonterm      : in     WisiToken.Token_ID;
      Virtual      : in     Boolean         := False;
      Action       : in     Semantic_Action := null;
      Action_Index : in     Natural         := 0)
   return Valid_Node_Index;

   overriding
   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index;

   overriding
   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index;

   overriding
   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   with Pre =>
     Tree.Is_Nonterm (Parent) and then
     (not (Tree.Has_Children (Parent) or
             Tree.Has_Parent (Children)));
   --  We only set children on newly added nodes, and we don't add nodes
   --  to the shared tree.

   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;

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
   function Virtual
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   ----------
   --  new operations

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index)
   with Pre => not Tree.Has_Parent (Node);
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
      Shared_Tree : Shared_Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead.

      Last_Shared_Node : Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
   end record;

end WisiToken.Syntax_Trees.Branched;
