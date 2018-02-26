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
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Semantic_State;
package WisiToken.Syntax_Trees is

   type Abstract_Tree is abstract tagged null record;

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   No_Node_Index : constant Node_Index := Node_Index'First;

   type Valid_Node_Index_Array is array (Ada.Containers.Count_Type range <>) of Valid_Node_Index;

   package Valid_Node_Index_Arrays is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Valid_Node_Index);

   package Valid_Node_Index_Queues is new SAL.Gen_Unbounded_Definite_Queues (Valid_Node_Index);

   --  FIXME: operations commented out so we find out which ones we really need.
   function Add_Nonterm
     (Tree    : in out Abstract_Tree;
      Nonterm : in     Token_ID;
      Virtual : in     Boolean                                  := False;
      Action  : in     WisiToken.Semantic_State.Semantic_Action := null)
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

   --  procedure Set_Child
   --    (Tree   : in out Abstract_Tree;
   --     Parent : in     Valid_Node_Index;
   --     Child  : in     Valid_Node_Index)
   --    is abstract
   --  with Pre'Class => not Tree.Has_Parent (Child);
   --  --  Set Child as a child of Parent.
   --  --  Neither Child nor Parent can be No_Element.
   --  --  Child.Parent must be unset.

   procedure Set_Children
     (Tree     : in out Abstract_Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
     is abstract;
   --  Set Child as a child of Parent.
   --  Neither Child nor Parent can be No_Element.
   --  Child.Parent must be unset.

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
   --  Concrete tree for parsers. See wisitoken-syntax_trees-branched for
   --  recovery tree.

   type Tree (Terminals : not null access Base_Token_Arrays.Vector) is new Abstract_Tree with private;

   overriding
   function Add_Nonterm
     (Tree    : in out Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean                                  := False;
      Action  : in     WisiToken.Semantic_State.Semantic_Action := null)
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

   procedure Set_Child
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
   with Pre => not Tree.Has_Parent (Child);
   --  Set Child as a child of Parent.
   --  Neither Child nor Parent can be No_Element.
   --  Child.Parent must be unset.

   overriding
   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   with Pre => Tree.Is_Nonterm (Parent) and then (not (Tree.Has_Children (Parent) or Tree.Has_Parent (Children)));

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

   ----------
   --  Cursors

   type Cursor is private;

   No_Element : constant Cursor;

private

   type Node_Label is (Shared_Terminal, Virtual_Terminal, Nonterm);
   --  Recover reads from the lexer when checking that the parse will
   --  succeed; those tokens are stored in Shared_Tree.Terminals. Recover
   --  inserts tokens to fix the error; those are virtual, and stored
   --  only in Tree.

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors.
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
         --  set.

         Byte_Region : Buffer_Region := Null_Buffer_Region;

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Valid_Node_Index_Arrays.Vector;
         Action   : WisiToken.Semantic_State.Semantic_Action := null;
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new Ada.Containers.Vectors (Valid_Node_Index, Node);

   type Tree (Terminals : not null access Base_Token_Arrays.Vector) is new Abstract_Tree with record
      Nodes : Node_Arrays.Vector; -- FIXME: use sal.gen_unbounded_definite_vectors for faster recover
   end record;
   type Tree_Access is access all Tree;

   type Cursor is record
      Tree  : Tree_Access;
      Index : Node_Index;
   end record;

   No_Element : constant Cursor := (Tree => null, Index => No_Node_Index);

end WisiToken.Syntax_Trees;
