--  Abstract :
--
--  Syntax tree type and operations.
--
--  Rationale :
--
--  We provide an abstract root type and two concrete types (one in a
--  child package), because we need different implementations for the
--  trees used in the parser and recovery.
--
--  We define both the abstract root type and the parser concrete type
--  in this package so the auxiliary types (Node_Index etc) are
--  visible when Tree is.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.

--  There is one syntax tree for each parser. There is one shared
--  Terminals array, matching the actual input text.
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

with Ada.Containers.Vectors;
with Ada.Finalization;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Semantic_State;
package WisiToken.Syntax_Trees is

   type Abstract_Tree is abstract new Ada.Finalization.Controlled with null record;

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   No_Node_Index : constant Node_Index := Node_Index'First;

   type Valid_Node_Index_Array is array (Positive_Index_Type range <>) of Valid_Node_Index;
   --  Index matches Base_Token_Array, Augmented_Token_Array

   package Valid_Node_Index_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Valid_Node_Index);
   --  Index matches Valid_Node_Index_Array.

   package Valid_Node_Index_Queues is new SAL.Gen_Unbounded_Definite_Queues (Valid_Node_Index);

   type User_Data_Type is tagged limited null record;

   type Semantic_Action is access procedure
     (User_Data    : in out User_Data_Type'Class;
      State        : in out WisiToken.Semantic_State.Semantic_State;
      Tree         : in out Abstract_Tree'Class;
      Tree_Nonterm : in     Valid_Node_Index;
      Tree_Tokens  : in     Valid_Node_Index_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Semantic_Action := null;

   function Add_Nonterm
     (Tree    : in out Abstract_Tree;
      Nonterm : in     Token_ID;
      Virtual : in     Boolean         := False;
      Action  : in     Semantic_Action := null)
   return Valid_Node_Index is abstract;
   --  Add a new Nonterm node with no parent. Result points to the added
   --  node.

   function Add_Terminal
     (Tree     : in out Abstract_Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index is abstract;
   --  Add a new Terminal node with no parent. Terminal must be an index
   --  into Tree.Terminals. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Abstract_Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index is abstract;
   --  Add a new virtual terminal node with no parent. Result points to
   --  the added node.

   procedure Set_Children
     (Tree     : in out Abstract_Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
     is abstract;
   --  Set the Children of Parent. Children.Parent must be unset.

   function Byte_Region
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region is abstract;

   function Name_Region
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region is abstract;

   procedure Set_Name_Region
     (Tree   : in out Abstract_Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
     is abstract;

   function ID
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID is abstract;

   function Base_Token
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token is abstract;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   function Base_Token_Array
     (Tree  : in Abstract_Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Base_Token_Array;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   type Augmented_Ref (Element : access Semantic_State.Augmented_Token) is null record
   with Implicit_Dereference => Element;

   function Augmented_Token_Ref
     (Tree : in out Abstract_Tree;
      Node : in     Valid_Node_Index)
     return Augmented_Ref is abstract;

   function Augmented_Token_Array
     (Tree                : in out Abstract_Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Array is abstract;
   --  If Nodes (I) is a nonterm, returns Augmented_Token_Ref.
   --
   --  If a virtual terminal, returns (ID => Nodes (I).terminal_id, others => <>).
   --
   --  If a terminal, returns (Terminals (Nodes (I) with others => <>).

   function Virtual
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Boolean is abstract;

   function Image
     (Tree       : in Abstract_Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   function Image
     (Tree       : in Abstract_Tree;
      Nodes      : in Valid_Node_Index_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   function Image
     (Tree       : in Abstract_Tree;
      Item       : in Valid_Node_Index_Queues.Queue_Type;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   --  For debug and error messages.

   ----------
   --  Concrete tree for parsers. See wisitoken-syntax_trees-branched.adb for
   --  recovery tree.

   type Tree (Terminals : not null access Base_Token_Arrays.Vector) is new Abstract_Tree with private;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   overriding procedure Adjust (Tree : in out Syntax_Trees.Tree);
   --  Copy any allocated storage.

   overriding
   function Add_Nonterm
     (Tree    : in out Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean         := False;
      Action  : in     Semantic_Action := null)
   return Valid_Node_Index;

   overriding
   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index;

   overriding
   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index;

   overriding
   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   with Pre => Tree.Is_Nonterm (Parent) and then (not (Tree.Has_Children (Parent) or Tree.Has_Parent (Children)));

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;

   overriding
   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region;

   overriding
   function Name_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region;

   overriding
   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   overriding
   function Base_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token;

   overriding
   function Virtual
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Augmented_Token_Ref
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Index)
     return Augmented_Ref
   with Pre => Tree.Is_Nonterm (Node);
   --  If Set_Augmented (Node) has not yet been called a default
   --  Augmented_Token is provided.

   procedure Set_Augmented
     (Tree      : in out Syntax_Trees.Tree;
      Node      : in     Valid_Node_Index;
      Augmented : in     Semantic_State.Augmented_Token)
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Augmented_Token_Array
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Array;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node.
   --
   --  Raises Program_Error if Process_Node calls Set_Children.

private

   type Node_Label is (Shared_Terminal, Virtual_Terminal, Nonterm);
   --  Recover reads from the lexer when checking that the parse will
   --  succeed; those tokens are stored in Shared_Tree.Terminals. Recover
   --  inserts tokens to fix the error; those are virtual, and stored
   --  only in Tree.

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors; all
   --  entries are the same size.
   record
      Parent : Node_Index := No_Node_Index;

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index;

      when Virtual_Terminal =>
         Terminal_ID : WisiToken.Token_ID;

      when Nonterm =>
         Nonterm_ID  : WisiToken.Token_ID;

         Virtual : Boolean;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         Byte_Region : Buffer_Region := Null_Buffer_Region;
         --  Computed by Set_Children, used in debug messages.
         --
         --  FIXME: duplicates Augmented.Byte_Region; time with and without
         --  this. Maybe make Augmented not derived from Base_Token, change to
         --  Annotations.
         --
         --  Very useful for debugging, not needed until action execute, some
         --  actions don't need it.
         --
         --  Expensive to compute individually after parse time (traverse
         --  entire subtree); ok if traversing entire tree anyway.

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Valid_Node_Index_Arrays.Vector;

         Action : Semantic_Action := null;
         --  FIXME: add Index for action name in trace?

         Augmented : Semantic_State.Augmented_Token_Access := null;
         --  We store Augmented_Token_Access rather than Augmented_Token, to
         --  save memory space and copy time during Recover. Note that
         --  Augmented is null during recover; it is only set after parsing is
         --  complete, while executing actions.
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new Ada.Containers.Vectors (Valid_Node_Index, Node);

   type Tree (Terminals : not null access Base_Token_Arrays.Vector) is new Abstract_Tree with record
      Nodes : Node_Arrays.Vector; -- FIXME: use sal.gen_unbounded_definite_vectors for faster recover

      Augmented_Present : Boolean := False;
      --  True if Set_Augmented has been called on any node.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.

   end record;
   type Tree_Access is access all Tree;

end WisiToken.Syntax_Trees;
