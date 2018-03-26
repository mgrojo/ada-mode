--  Abstract :
--
--  Syntax tree type and operations.
--
--  Rationale :
--
--  We provide Base_Tree and Tree in one package, because only Tree
--  needs an API; the only way Base_Tree is accessed is via Tree.
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
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Semantic_State;
package WisiToken.Syntax_Trees is

   type Base_Tree is new Ada.Finalization.Controlled with private;

   type Base_Tree_Access is access all Base_Tree;

   overriding procedure Finalize (Tree : in out Base_Tree);
   --  Free any allocated storage.

   overriding procedure Adjust (Tree : in out Base_Tree);
   --  Copy any allocated storage.

   type Tree is tagged private;

   procedure Initialize
     (Branched_Tree : in out Tree;
      Shared_Tree   : in     Base_Tree_Access;
      Flush         : in     Boolean);
   --  Set Branched_Tree to refer to Shared_Tree.

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   Invalid_Node_Index : constant Node_Index := Node_Index'First;

   type Valid_Node_Index_Array is array (Positive_Index_Type range <>) of Valid_Node_Index;
   --  Index matches Base_Token_Array, Augmented_Token_Array

   package Valid_Node_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive_Index_Type, Valid_Node_Index);
   --  Index matches Valid_Node_Index_Array.

   type Node_Label is (Shared_Terminal, Virtual_Terminal, Nonterm);

   type User_Data_Type is tagged limited null record;

   type Semantic_Action is access procedure
     (User_Data : in out User_Data_Type'Class;
      State     : in out WisiToken.Semantic_State.Semantic_State;
      Tree      : in out Syntax_Trees.Tree;
      Nonterm   : in     Valid_Node_Index;
      Tokens    : in     Valid_Node_Index_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Semantic_Action := null;

   procedure Clear (Tree : in out Syntax_Trees.Base_Tree);
   procedure Clear (Tree : in out Syntax_Trees.Tree);
   --  Delete all Elements and free associated memory; keep results of
   --  Initialize.

   procedure Flush (Tree : in out Syntax_Trees.Tree);
   --  Move all nodes in branched part to shared tree, set Flush mode
   --  True.

   procedure Set_Flush_False (Tree : in out Syntax_Trees.Tree);
   --  Set Flush mode False; use Flush to set True.

   function Add_Nonterm
     (Tree       : in out Syntax_Trees.Tree;
      Nonterm    : in     WisiToken.Token_ID;
      Action     : in     Semantic_Action;
      Production : in     Natural;
      Name_Index : in     Natural;
      Children   : in     Valid_Node_Index_Array)
     return Valid_Node_Index
   with
     Pre  => not Tree.Traversing,
     Post => Tree.Is_Empty (Add_Nonterm'Result) or
             Tree.Min_Terminal_Index (Add_Nonterm'Result) /= Invalid_Token_Index;
   --  Add a new Nonterm node. Result points to the added node.

   function Add_Terminal
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Token_Index;
      Terminals : in     Base_Token_Arrays.Vector)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node. Terminal must be an index into Terminals.
   --  Result points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new virtual terminal node with no parent. Result points to
   --  the added node.

   procedure Set_State
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index);

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Unknown_State_Index;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label;

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Branched_Nodes (Tree : in Syntax_Trees.Tree) return Boolean;
   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Min_Terminal_Index (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   function Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Buffer_Region;

   function Same_Token
     (Tree_1  : in Syntax_Trees.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Syntax_Trees.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean;
   --  True if the two tokens have the same ID and Byte_Region.

   function Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token;

   function Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   type Augmented_Ref (Element : access Semantic_State.Augmented_Token) is null record
   with Implicit_Dereference => Element;

   function Augmented_Token_Ref
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref;
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
     (Tree                : in Syntax_Trees.Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
     return Constant_Augmented_Ref;

   --  If Nodes (I) is a nonterm or virtual terminal, returns result of
   --  Set_Augmented, which may be null.
   --
   --  If a terminal, returns access to Augmented_Terminals (I).

   function Augmented_Token_Array
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array;

   function Virtual
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Boolean;

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   function Name_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Find_Ancestor
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;
   --  Return the ancestor of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Has_Parent (Node);
   --  Return the sibling of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);
   --  Return the child of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node.

   function Min_Shared_Terminal_Index (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   --  Returns lowest index of shared terminal in subtree under Node. If
   --  result is Invalid_Token_Index, all terminals are virtual.

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array;

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID_Array;

   function Image
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   function Image
     (Tree       : in Syntax_Trees.Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor'Class)
     return String;
   --  function Image
   --    (Tree       : in Syntax_Trees.Tree;
   --     Nodes      : in Valid_Node_Index_Arrays.Vector;
   --     Descriptor : in WisiToken.Descriptor)
   --    return String;
   --  For debug and error messages.

private

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors; all
   --  entries are the same size.
   record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Computed by Set_Children, used in Semantic_Check actions and debug
      --  messages.

      Parent : Node_Index := Invalid_Node_Index;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that was on stack with this token, to allow undoing a
      --  reduce.

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index;

      when Virtual_Terminal =>
         Virtual_Augmented : Semantic_State.Augmented_Token_Access := null;

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         Production : Natural := 0;
         --  Index into Parse_Table.Productions.

         Name_Index : Natural := 0;
         --  Production for nonterm_id used to produce this element, for debug
         --  messages.

         Action : Semantic_Action := null;

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Valid_Node_Index_Arrays.Vector;

         Min_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
         --  Cached for push_back of nonterminals during recovery

         Nonterm_Augmented : Semantic_State.Augmented_Token_Access := null;
         --  We store Augmented_Token_Access rather than Augmented_Token, to
         --  save memory space and copy time during Recover. Note that
         --  Augmented is null during recover; it is only set after parsing is
         --  complete, while executing actions.
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node);

   type Base_Tree is new Ada.Finalization.Controlled with record
      --  It is tempting to store an access to Shared_Parser.Terminals here,
      --  and use that to represent some data. However, during
      --  McKenzie_Recover, the tree is read by parallel tasks, and
      --  Shared_Parser.Terminals is written (when reading new terminals
      --  from the Lexer), also by parallel tasks. So we would need a
      --  protected object to control access to Terminals, and that would
      --  largely eliminate the advantage of parallel tasks. So we copy data
      --  from Terminals into Tree nodes.

      Nodes : Node_Arrays.Vector;
      --  During normal parsing, tokens are added to Nodes by "parallel"
      --  parsers, but they are all run from one Ada task, so there's no
      --  need for Nodes to be Protected.
      --
      --  During McKenzie_Recover, tokens are added to Terminals by parallel
      --  tasks, but not to the shared syntax_tree.

      Augmented_Present : Boolean := False;
      --  True if Set_Augmented has been called on any node.
      --  Declared in Base_Tree because used by Base_Tree.Adjust.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

   end record;

   type Tree is tagged record
      Shared_Tree : Base_Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead, unless Flush = True.

      Last_Shared_Node : Node_Index := Invalid_Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
      Flush            : Boolean    := False;
      --  We maintain Last_Shared_Node when Flush is True, so subprograms
      --  that have no reason to check Flush can rely on Last_Shared_Node.
   end record with
     Type_Invariant => (if Tree.Flush then not Tree.Has_Branched_Nodes);

end WisiToken.Syntax_Trees;
