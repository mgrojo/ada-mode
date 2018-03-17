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

with Ada.Finalization;
with SAL.Gen_Unbounded_Definite_Queues;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Semantic_State;
package WisiToken.Syntax_Trees is

   type Abstract_Tree is abstract new Ada.Finalization.Controlled with null record;
   --  WORKAROUND: GNAT GPL 2017 is confused by this:
   --  with Variable_Indexing => Augmented_Token_Ref;

   --  FIXME: move all operations to Branched, unless used by that.

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   No_Node_Index : constant Node_Index := Node_Index'First;

   type Valid_Node_Index_Array is array (Positive_Index_Type range <>) of Valid_Node_Index;
   --  Index matches Base_Token_Array, Augmented_Token_Array

   package Valid_Node_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive_Index_Type, Valid_Node_Index);
   --  Index matches Valid_Node_Index_Array.

   package Valid_Node_Index_Queues is new SAL.Gen_Unbounded_Definite_Queues (Valid_Node_Index);

   type Node_Label is (Shared_Terminal, Virtual_Terminal, Nonterm);

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

   procedure Clear (Tree : in out Abstract_Tree) is abstract;
   --  Delete all Elements and free associated memory; keep results of
   --  Initialize.

   function Label (Tree : in Abstract_Tree; Node : in Valid_Node_Index) return Node_Label is abstract;

   function Children
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Valid_Node_Index_Array is abstract;

   function Parent (Tree : in Abstract_Tree; Node : in Valid_Node_Index) return Node_Index is abstract;

   function ID
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID is abstract;

   type Augmented_Ref (Element : access Semantic_State.Augmented_Token) is null record
   with Implicit_Dereference => Element;

   function Augmented_Token_Ref
     (Tree                : in out Abstract_Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref is abstract;
   --  If Nodes (I) is a nonterm, returns result of Set_Augmented, or
   --  (Nodes (I).terminal_id, Nodes (I).Byte_Region, others => <>)
   --
   --  If a virtual terminal, returns result of Set_Augmented, or
   --  (Nodes (I).terminal_id, others => <>).
   --
   --  If a terminal, returns Augmented_Terminals (I).

   type Constant_Augmented_Ref (Element : access constant Semantic_State.Augmented_Token) is null record
   with Implicit_Dereference => Element;

   function Constant_Aug_Token_Ref
     (Tree                : in Abstract_Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
     return Constant_Augmented_Ref is abstract;
   --  If Nodes (I) is a nonterm or virtual terminal, returns result of
   --  Set_Augmented, which may be null.
   --
   --  If a terminal, returns access to Augmented_Terminals (I).

   function Augmented_Token_Array
     (Tree                : in out Abstract_Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array is abstract;

   function Virtual
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Boolean is abstract;

   function Action
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action is abstract;

   function Name_Index
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index)
     return Natural is abstract;

   function Find_Ancestor
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index is abstract;
   --  Return the ancestor of Node that contains ID, or No_Node_Index if
   --  none match.

   function Find_Sibling
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index is abstract;
   --  Return the sibling of Node that contains ID, or No_Node_Index if
   --  none match.

   function Find_Child
     (Tree : in Abstract_Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index is abstract;
   --  Return the child of Node that contains ID, or No_Node_Index if
   --  none match.

   function Image
     (Tree       : in Abstract_Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String is abstract;
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

   type Tree is new Abstract_Tree with private;

   type Tree_Access is access all Syntax_Trees.Tree;

   procedure Initialize
     (Tree      : in out Syntax_Trees.Tree;
      Terminals : in     Base_Token_Arrays.Vector_Access_Constant);
   --  Set the Terminals array for Tree.

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   overriding procedure Clear (Tree : in out Syntax_Trees.Tree);

   overriding procedure Adjust (Tree : in out Syntax_Trees.Tree);
   --  Copy any allocated storage.

   function Add_Nonterm
     (Tree       : in out Syntax_Trees.Tree;
      Nonterm    : in     WisiToken.Token_ID;
      Action     : in     Semantic_Action;
      Production : in     Positive;
      Name_Index : in     Natural)
   return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Nonterm node with no parent. Result points to the added
   --  node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node with no parent. Terminal must be an index
   --  into Tree.Terminals. Result points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new virtual terminal node with no parent. Result points to
   --  the added node.

   overriding function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label;

   overriding
   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
     is (Tree.Label (Node) = Nonterm);
   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   overriding function Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index;

   overriding
   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   overriding
   function Augmented_Token_Ref
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref;

   overriding
   function Constant_Aug_Token_Ref
     (Tree                : in Syntax_Trees.Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
     return Constant_Augmented_Ref;

   procedure Set_Augmented
     (Tree      : in out Syntax_Trees.Tree;
      Node      : in     Valid_Node_Index;
      Augmented : in     Semantic_State.Augmented_Token)
   with Pre => Tree.Label (Node) in Virtual_Terminal .. Nonterm;

   overriding
   function Augmented_Token_Array
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array;

   overriding
   function Virtual
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   overriding
   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Name_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   overriding
   function Find_Ancestor
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;

   overriding
   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Has_Parent (Node);

   overriding
   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node.

   overriding
   function Image
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

private

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors; all
   --  entries are the same size.
   record
      Parent : Node_Index := No_Node_Index;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that was on stack with this token, to allow undoing a
      --  reduce.

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index;

      when Virtual_Terminal =>
         Terminal_ID : WisiToken.Token_ID;

         Virtual_Augmented : Semantic_State.Augmented_Token_Access := null;

      when Nonterm =>
         Nonterm_ID  : WisiToken.Token_ID := Invalid_Token_ID;

         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         Production : Natural := 0;
         --  Index into Parse_Table.Productions.

         Name_Index : Natural := 0;
         --  Production for nonterm_id used to produce this element, for debug
         --  messages.

         Action : Semantic_Action := null;

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

         Min_Terminal_Index : Base_Token_Index;
         --  Cached for push_back of nonterminals during recovery
         --  FIXME: tree is available; can compute when needed.

         Nonterm_Augmented : Semantic_State.Augmented_Token_Access := null;
         --  We store Augmented_Token_Access rather than Augmented_Token, to
         --  save memory space and copy time during Recover. Note that
         --  Augmented is null during recover; it is only set after parsing is
         --  complete, while executing actions.
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node);

   type Tree is new Abstract_Tree with record
      Terminals : Base_Token_Arrays.Vector_Access_Constant;

      Nodes : Node_Arrays.Vector;
      --  During normal parsing, tokens are added to Nodes by "parallel"
      --  parsers, but they are all run from one Ada task, so there's no
      --  need for Nodes to be Protected.
      --
      --  During McKenzie_Recover, tokens are added to Terminals by parallel
      --  tasks, but not to the shared syntax_tree.

      Augmented_Present : Boolean := False;
      --  True if Set_Augmented has been called on any node.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.

   end record;

   ----------
   --  For child packages

   function Children (N : in Syntax_Trees.Node) return Valid_Node_Index_Array;

   function Get_ID (N : in Node; Terminals : in Base_Token_Arrays.Vector) return Token_ID
   with Inline;

   function Image
     (N          : in Syntax_Trees.Node;
      Terminals  : in Base_Token_Arrays.Vector_Access_Constant;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;

   procedure Set_Children
     (Nodes     : in out Node_Arrays.Vector;
      Terminals : in     Base_Token_Arrays.Vector_Access_Constant;
      Parent    : in     Valid_Node_Index;
      Children  : in     Valid_Node_Index_Array);

end WisiToken.Syntax_Trees;
