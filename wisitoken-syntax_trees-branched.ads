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

   type Access_Constant_Shared_Tree is access constant Syntax_Trees.Tree;

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Access_Constant_Shared_Tree);
   --  Set Branched_Tree to refer to Shared_Tree.

   --  FIXME: operations commented out so we find out which ones we really need.

   overriding
   function Add_Nonterm
     (Tree    : in out Branched.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean                                  := False;
      Action  : in     WisiToken.Semantic_State.Semantic_Action := null)
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

   --  overriding
   --  procedure Set_Child
   --    (Tree   : in out Branched.Tree;
   --     Parent : in     Valid_Node_Index;
   --     Child  : in     Valid_Node_Index)
   --  with Pre => not Tree.Has_Parent (Child);

   overriding
   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   with Pre => not (Tree.Has_Children (Parent) or Tree.Has_Parent (Children));

   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Nonterminal (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean;

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
   with Pre => Tree.Is_Nonterminal (Node);

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
   function Virtual
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   ----------
   --  new operations

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index);
   --  Delete subtree rooted at Node.
   --
   --  If Node is in shared tree, moves branch point to Node.
   --
   --  Raises SAL.Invalid_Operation if
   --  Node.Parent is not No_Node_Index.

   function Get_Terminals (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Base_Token_Array;
   --  Return all terminals in tree rooted at Node, in depth-first order.
   --  FIXME: only need IDs?

private

   type Node_Label is (Shared_Terminal, New_Terminal, New_Nonterm);

   type Node (Label : Node_Label := New_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors.
   record
      Parent : Node_Index := No_Node_Index;

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index;

      when New_Terminal =>
         Terminal_ID : WisiToken.Token_ID;

      when New_Nonterm =>
         Nonterm_ID : WisiToken.Token_ID;
         Children   : Valid_Node_Index_Arrays.Vector;

         --  No need for the other fields in Syntax_Tree.Node; these are all
         --  virtual, so the other fields are null.
      end case;
   end record;

   subtype New_Terminal_Node is Node (New_Terminal);
   subtype New_Nonterm_Node is Node (New_Nonterm);

   package Node_Arrays is new Ada.Containers.Vectors (Valid_Node_Index, Node);

   type Tree is new Abstract_Tree with record
      Shared_Tree      : access constant Syntax_Trees.Tree;
      Last_Shared_Node : Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
   end record;

end WisiToken.Syntax_Trees.Branched;
