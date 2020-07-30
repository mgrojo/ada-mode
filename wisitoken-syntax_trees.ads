--  Abstract :
--
--  Syntax tree type and operations, implemented with directly
--  allocated nodes.
--
--  References :
--
--  [1] Elad Lahav 2004, Efficient Semantic Analysis for Text Editors
--
--  Design :
--
--  There is one syntax tree; parallel parsers all add nodes to the
--  same tree, maintaining different roots via Stream_IDs.
--
--  Tree are not limited to allow wisitoken-bnf-generate to copy them
--  in order to translate EBNF to BNF, and other similar operations.
--  No Adjust is needed; Augmented pointers are shared.
--
--  Nodes are never deleted, except when the entire tree is Finalized.
--  Thus all nodes created by parsers that are terminated are still in
--  the tree; to prune them, copy the surviving tree to a new tree,
--  finalize the original.
--
--  We can't traverse Tree.Streams to deallocate; in general both
--  Stream_Elements and Nodes are referenced multiple times in
--  multiple streams. So we keep track of things to deallocate in
--  Tree.Elements and Tree.Nodes.
--
--  Node contains a Parent link, to make it easy to traverse the tree
--  in any direction. However, we do not set the Parent links while
--  parsing, to simplify maintaining the syntax tree while parallel
--  parsing. When a new nonterm is added by a parallel parser, if it
--  set the parent component of its children, it would first have to
--  copy those children into new nodes. That would complicate keeping
--  track of Shared_Terminal nodes for error correction.
--
--  The parent links are set by Set_Parents, which is called by
--  Parser.Execute_Actions before the actions are executed.
--  Fortunately, we don't need the parent links during parsing or
--  error recovery. After calling Set_Parents (ie, while editing the
--  syntax tree after parse), any functions that modify children or
--  parents update the corresponding links, setting them to
--  Invalid_Node_Access as appropriate.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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
with SAL.Gen_Trimmed_Image;
with SAL.Gen_Unbounded_Definite_Vectors;
with SAL.Gen_Unbounded_Sparse_Ordered_Sets;
with SAL.Generic_Decimal_Image;
with WisiToken.Lexer;
package WisiToken.Syntax_Trees is
   use all type SAL.Base_Peek_Type;

   type Node (<>) is private;
   type Node_Access is access Node;
   subtype Valid_Node_Access is not null Node_Access;

   Invalid_Node_Access : constant Node_Access := null;
   Dummy_Node : constant Valid_Node_Access;
   --  Use when you must initialize a Valid_Node_Access before overwritting it.

   type Node_Access_Array is array (Positive_Index_Type range <>) of Node_Access;
   type Valid_Node_Access_Array is array (Positive_Index_Type range <>) of Valid_Node_Access;

   function To_Node_Access (Item : in Valid_Node_Access_Array) return Node_Access_Array;
   function To_Valid_Node_Access (Item : in Node_Access_Array) return Valid_Node_Access_Array;

   type Recover_Token is record
      --  Declared here because it needs Node_Access.

      --  Maintaining a syntax tree during error recovery is too slow, so we
      --  store enough information in the recover stack to perform
      --  Semantic_Checks, Language_Fixes, and Push_Back operations. and to
      --  apply the solution to the main parser state. We make thousands of
      --  copies of the parse stack during recover, so minimizing size and
      --  compute time for this is critical.
      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Byte_Region is used to detect empty tokens, for cost and other issues.

      First_Terminal_Index : Node_Access := Invalid_Node_Access;
      --  For terminals, index of this token in Shared_Parser.Tree. For
      --  nonterminals, first of contained tokens (Invalid_Node_Access if
      --  empty). For virtuals, Invalid_Node_Access. Used for push_back of
      --  nonterminals.

      Name : Buffer_Region := Null_Buffer_Region;
      --  Set and used by semantic_checks.

      Virtual : Boolean := True;
      --  For terminals, True if inserted by recover. For nonterminals, True
      --  if any contained token has Virtual = True.
   end record;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor)
     return String;

   type Recover_Token_Array is array (Positive_Index_Type range <>) of Recover_Token;

   type Tree is new Ada.Finalization.Controlled with private;

   type Tree_Variable_Reference (Element : not null access Tree) is null record with
     Implicit_Dereference => Element;

   type Tree_Constant_Reference (Element : not null access constant Tree) is null record with
     Implicit_Dereference => Element;

   --  We don't declare Is_Empty (Tree) because it is not easy to keep
   --  track of. Root (Tree) = Invalid_Node_Access is correct for most use
   --  cases.

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   procedure Clear (Tree : in out Syntax_Trees.Tree; Free_Memory : in Boolean := False);
   --  Delete all nodes in all streams, reset for new parse. Free_Memory
   --  applies to internal bookkeeping; leaving it False may slightly
   --  speed parsing a similar sized file as the previous one.

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean;

   type Node_Label is
     (Shared_Terminal,    -- text is user input, accessed via Parser.Terminals
      Virtual_Terminal,   -- no text; inserted during error recovery
      Virtual_Identifier, -- text in user data, created during tree rewrite
      Nonterm             -- contains terminals/nonterminals/identifiers
     );
   subtype Terminal_Label is Node_Label range Shared_Terminal .. Virtual_Identifier;
   subtype Virtual_Terminal_Label is Node_Label range Virtual_Terminal .. Virtual_Identifier;

   type Stream_ID is private;
   Invalid_Stream_ID : constant Stream_ID;

   type Stream_Element is private;
   type Stream_Index is access Stream_Element;
   Invalid_Stream_Index : constant Stream_Index := null;

   function Trimmed_Image (Item : in Stream_ID) return String;
   --  Trimmed integer.

   type Stream_Index_Array is array (Positive_Index_Type range <>) of Stream_Index;

   ----------
   --  User_Data_Type

   type User_Data_Type is tagged limited null record;
   --  Many test languages don't need this, so we default the procedures
   --  to null.

   type User_Data_Access is access all User_Data_Type'Class;

   procedure Set_Lexer
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle)
     is null;

   procedure Reset (User_Data : in out User_Data_Type) is null;
   --  Reset to start a new parse.

   procedure Initialize_Actions
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class)
     is null;
   --  Called by Execute_Actions, before processing the tree.

   procedure Lexer_To_Augmented
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Token     : in     Node_Access)
     is null;
   --  Token is a grammar token that was just returned by
   --  User_Data.Lexer; read auxiliary data from User_Data.Lexer, do
   --  something useful with it. Called before parsing, once for each
   --  grammar token in the input stream.
   --
   --  Parser.Line_Begin_Token has already been updated.

   procedure Lexer_To_Augmented
     (User_Data          : in out User_Data_Type;
      Tree               : in out Syntax_Trees.Tree'Class;
      Token              : in     Base_Token;
      Prev_Grammar_Token : in     Node_Access)
     is null;
   --  Token is a non-grammar token that was just returned by
   --  User_Data.Lexer; it has already been added to Prev_Grammar_Token,
   --  or to Parser.Leading_Non_Grammar if Prev_Grammar_Token is
   --  Invalid_Token_Index. Read auxiliary data from User_Data.Lexer, do
   --  something useful with it. Called before parsing, once for each
   --  non-grammar token in the input stream.

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Token                : in     Valid_Node_Access;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean;
   --  Return True if ID should be treated as if inserted after the
   --  previous shared terminal, rather than before the next (which is
   --  the default). This can affect which line it appears on, which
   --  affects indentation. Called from Insert_Token.
   --
   --  The default implementation always returns False.

   procedure Insert_Token
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Token     : in     Valid_Node_Access)
     is null;
   --  Token was inserted in error recovery; update other tokens and Tree
   --  as needed. Called from Execute_Actions for each inserted token,
   --  before processing the syntax tree.

   procedure Delete_Token
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Token     : in     Valid_Node_Access)
     is null;
   --  Token was deleted in error recovery; update
   --  remaining tokens as needed. Called from Execute_Actions for each
   --  deleted token, before processing the syntax tree.

   procedure Reduce
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Nonterm   : in     Valid_Node_Access;
      Tokens    : in     Node_Access_Array)
     is null;
   --  Reduce Tokens to Nonterm. Nonterm.Byte_Region is computed by
   --  caller.

   type Semantic_Action is access procedure
     (User_Data : in out User_Data_Type'Class;
      Tree      : in out Syntax_Trees.Tree;
      Nonterm   : in     Valid_Node_Access;
      Tokens    : in     Valid_Node_Access_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Semantic_Action := null;

   ----------
   --
   --  Parsing operations (including error recovery), Tree and Node
   --  attributes.

   function Leading_Non_Grammar (Tree : aliased in out Syntax_Trees.Tree) return Base_Token_Arrays_Var_Ref;

   function Leading_Non_Grammar_Const (Tree : aliased in Syntax_Trees.Tree) return Base_Token_Arrays_Const_Ref;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID;
   --  Create a new parse stream.

   function Next_Stream_ID (Tree : in Syntax_Trees.Tree) return Stream_ID;

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural;

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there is only one stream, and it has only one element.

   --  FIXME: replace with Tree_Root with pre=> fullY_parsed, or delete
   --  function Stream_Root (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Node_Access;

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   with Pre => Stream /= Invalid_Stream_ID;

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Stream /= Invalid_Stream_ID;

   function Get_Node (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Valid_Node_Access;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Current_Token   : in     Stream_Index;
      Children        : in     Stream_Index_Array;
      Action          : in     Semantic_Action := null;
      State           : in     State_Index;
      Default_Virtual : in     Boolean         := False)
     return Stream_Index
   with Pre => not Tree.Traversing and Children'First = 1 and
               (Stream /= Invalid_Stream_ID or Tree.Fully_Parsed);
   --  Build a new Nonterm node on Stream, replacing Children from
   --  Stream. Result points to the added node. If Children'Length = 0,
   --  set Nonterm.Virtual := Default_Virtual.
   --
   --  If Tree.Parents_Set, then Children.Parent are set to the new node,
   --  and in previous parents of those children (if any), the
   --  corresponding entry in Children is set to null.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Base_Token)
     return Stream_Index
   with Pre => not Tree.Traversing and Stream /= Invalid_Stream_ID;
   --  Add a new Terminal element on Stream. Result points to the added
   --  node.

   function Non_Grammar
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Valid_Node_Access)
     return Base_Token_Arrays_Var_Ref
   with Pre => Tree.Is_Shared_Terminal (Terminal);

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Arrays_Const_Ref
   with Pre => Tree.Is_Shared_Terminal (Terminal);

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID;
      Before   : in     Valid_Node_Access)
     return Stream_Index
   with Pre => not Tree.Traversing and Stream /= Invalid_Stream_ID;
   --  Add a new Virtual_Terminal element on Stream. Before is the node
   --  containing the terminal that this virtual is inserted before
   --  during error correction. Result points to the added node.

   function Before
     (Tree             : in Syntax_Trees.Tree;
      Virtual_Terminal : in Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Is_Virtual_Terminal (Virtual_Terminal);

   procedure Set_State
     (Tree    : in out Syntax_Trees.Tree;
      Element : in     Stream_Index;
      State   : in     State_Index);

   function State (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Unknown_State_Index;
   function ID (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Token_ID
   with Pre => Element /= Invalid_Stream_Index;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label;

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Nonterm (Node);

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access_Array
   with Pre => Tree.Is_Nonterm (Node);
   --  Any children that were cleared by Add_Nonterm are returned as
   --  Invalid_Node_Access.

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Child_Index : in Positive_Index_Type)
     return Node_Access
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Child : in Valid_Node_Access)
     return Boolean
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Access) return Boolean;

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  True if contained buffer region is empty; always the case for
   --  virtual tokens, and for most copied tokens. Use Has_Children or
   --  Child_Count to see if Node has children.

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;

   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  Virtual_Terminal, Virtual_Identifier, or Nonterm that contains some Virtual tokens.

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Token_ID;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Production_ID
   with Pre => Tree.Is_Nonterm (Node);

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Buffer_Region;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token;

   function Get_Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Access_Array)
     return Recover_Token_Array;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Access;
      Value : in     Base_Token_Class_Access);
   --  Value will be deallocated when Tree is finalized.

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Base_Token_Class_Access;
   --  Returns result of Set_Augmented.

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Base_Token_Class_Access_Constant;

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      ID         : in Token_ID;
      Max_Parent : in Boolean := False)
     return Node_Access
   with Pre => Tree.Parents_Set;
   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return the ancestor of Node that contains ID (starting search with
   --  Node.Parent), or Invalid_Node_Access if none match.
   --
   --  If Max_Parent, return max parent found if none match; this will be
   --  Invalid_Node_Access if Node has no parent.

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   with Pre => Tree.Parents_Set and then Tree.Has_Parent (Node);
   --  Return the sibling of Node that contains ID, or Invalid_Node_Access if
   --  none match.

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   with Pre => Tree.Is_Nonterm (Node);
   --  Return the child of Node whose ID is ID, or Invalid_Node_Access if
   --  none match.

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access;
   --  Return the descendant of Node (may be Node) whose ID is ID, or
   --  Invalid_Node_Access if none match.

   function Find_Descendant
     (Tree      : in     Syntax_Trees.Tree;
      Node      : in     Valid_Node_Access;
      Predicate : access function (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean)
     return Node_Access;
   --  Return the descendant of Node (may be Node) for which Predicate
   --  returns True, or Invalid_Node_Access if none do.

   function Is_Descendant_Of
     (Tree       : in Syntax_Trees.Tree;
      Root       : in Valid_Node_Access;
      Descendant : in Valid_Node_Access)
     return Boolean
   with Pre => Tree.Parents_Set and Tree.Is_Nonterm (Root);

   function Sub_Tree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access
   with Pre => Tree.Parents_Set;
   --  Return top ancestor of Node.

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access);
      Root         : in     Node_Access := Invalid_Node_Access)
   with Pre => Root /= Invalid_Node_Access or Tree.Fully_Parsed;
   --  Traverse subtree of Tree rooted at Root (default single remaining
   --  stream element) in depth-first order, calling Process_Node on each
   --  node.

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Base_Identifier_Index
   with Pre => Tree.Is_Virtual_Identifier (Node);

   function Base_Token (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Base_Token
   with Pre => Tree.Is_Shared_Terminal (Node);

   function First_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  Returns first shared terminal node in subtree under Node
   --  (ignoring virtual terminals). If result is Invalid_Node_Access,
   --  all terminals are virtual, or the root nonterm is empty.

   function Last_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  Returns last shared terminal node in subtree under Node (ignoring
   --  virtual terminals). If result is Invalid_Node_Access, all
   --  terminals are virtual, or a nonterm is empty.

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array;
   --  Return sequence of terminals in Node.
   --
   --  "Terminals" can be Shared_Terminal, Virtual_Terminal,
   --  Virtual_Identifier.

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  First of Get_Terminals. Invalid_Node_Access if Node is an empty nonterminal.

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  Last of Get_Terminals. Invalid_Node_Access if Node is an empty nonterminal.

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Parents_Set and Tree.Label (Node) in Shared_Terminal | Virtual_Terminal | Virtual_Identifier;
   --  Return the terminal that is immediately before Node in Tree;
   --  Invalid_Node_Access if Node is the first terminal in Tree.

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Parents_Set and Tree.Label (Node) in Shared_Terminal | Virtual_Terminal | Virtual_Identifier;
   --  Return the terminal that is immediately after Node in Tree;
   --  Invalid_Node_Access if Node is the last terminal in Tree.

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID_Array;
   --  Same as Get_Terminals, but return the IDs.

   function First_Terminal_ID (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID;
   --  First of Get_Terminal_IDs; Invalid_Token_ID if Node is empty.

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Valid_Node_Access_Array;
   --  Return all descendants of Node matching ID.

   ----------
   --
   --  Post-parsing operations; editing the tree. The tree has only one
   --  stream, so these subprograms have no stream argument.

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Set_Parents (Tree : in out Syntax_Trees.Tree)
   with Pre => Tree.Fully_Parsed;

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   with Pre => Tree.Fully_Parsed;

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return Count parent of Node.

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Children'First = 1 and
               Tree.Parents_Set and Tree.Fully_Parsed;
   --  Add a new Nonterm node (not on any stream), containing
   --  Children, with no parent. Result points to the added node. If
   --  Children'Length = 0, set Nonterm.Virtual := Default_Virtual.
   --
   --  Children.Parent are set to the new node,
   --  and in previous parents of those children (if any), the
   --  corresponding entry in Children is set to null.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Base_Token)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Fully_Parsed;
   --  Add a new Terminal node with no parent, on no stream. Result
   --  points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Fully_Parsed;
   --  Add a new Virtual_Terminal node with no parent.
   --  Result points to the added node.

   function Copy_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in     Node_Access)
     return Node_Access
   with Pre => Tree.Parents_Set and Tree.Fully_Parsed;
   --  Deep copy (into Tree) subtree of Tree rooted at Root. Return root
   --  of new subtree; it has no parent.
   --
   --  If Root is Invalid_Node_Access, returns Invalid_Node_Access
   --
   --  Parents of new child nodes are set. References to objects external
   --  to tree are shallow copied (Augmented, Action).

   procedure Delete_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in out Node_Access);
   --  Free all nodes under Root
   --
   --  No precondition; called from Finalize.

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index;
      Byte_Region : in     WisiToken.Buffer_Region)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Fully_Parsed;
   --  Add a new Virtual_Identifier node with no parent. Byte_Region
   --  should point to an area in the source buffer related to the new
   --  identifier, to aid debugging. Result points to the added node.

   function Child_Index
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Access;
      Child  : in     Valid_Node_Access)
     return SAL.Peek_Type
   with Pre => Tree.Has_Child (Parent, Child);

   procedure Replace_Child
     (Tree                 : in out Syntax_Trees.Tree;
      Parent               : in     Valid_Node_Access;
      Child_Index          : in     SAL.Peek_Type;
      Old_Child            : in     Node_Access;
      New_Child            : in     Node_Access;
      Old_Child_New_Parent : in     Node_Access := Invalid_Node_Access)
   with
     Pre => Tree.Parents_Set and (not Tree.Traversing) and Tree.Fully_Parsed and
            (Tree.Is_Nonterm (Parent) and then
             (Tree.Child (Parent, Child_Index) = Old_Child and
              (Old_Child = Invalid_Node_Access or else
               Tree.Parent (Old_Child) = Parent)));
   --  In Parent.Children, replace child at Child_Index with New_Child.
   --  Unless Old_Child is Invalid_Node_Access, set Old_Child.Parent to
   --  Old_Child_New_Parent (may be Invalid_Node_Access). Unless New_Child
   --  is Invalid_Node_Access, set New_Child.Parent to Parent.

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in     Valid_Node_Access;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Node_Access_Array)
   with
     Pre => Tree.Parents_Set and (not Tree.Traversing) and Tree.Fully_Parsed and
            Tree.Is_Nonterm (Node) and (for all C of Children => C /= Invalid_Node_Access);
   --  If parents of current Node.Children are not Invalid_Node_Access,
   --  set corresponding entry in those parents to Invalid_Node_Access,
   --  then set the child parent to Invalid_Node_Access.
   --
   --  Then set ID of Node to New_ID, and Node.Children to Children; set
   --  parents of Children to Node.
   --
   --  If New_ID /= Tree.Production_ID (Node), Node.Action is set
   --  to null, because the old Action probably no longer applies.
   --
   --  We use a precondition on Children, rather than
   --  Valid_Node_Access_Array, so constructs like:
   --
   --     Tree.Set_Children (node, new_id, tree.childrend())
   --
   --  are legal.

   procedure Delete_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
   with
     Pre => Tree.Parents_Set and (not Tree.Traversing) and Tree.Fully_Parsed and
            Tree.Parent (Node) /= Invalid_Node_Access;
   --  Set child in Node.Parent to Invalid_Node_Access. If Node.Parent =
   --  Tree.Root, set Tree.Root to Node. Set Node.Parent to
   --  Invalid_Node_Access.

   ----------
   --  Debug and error message utils

   function Image
     (Tree              : in Syntax_Trees.Tree;
      Element           : in Stream_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String;
   function Image
     (Tree              : in Syntax_Trees.Tree;
      Node              : in Node_Access;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String;
   function Image
     (Tree         : in Syntax_Trees.Tree;
      Nodes        : in Node_Access_Array;
      Descriptor   : in WisiToken.Descriptor;
      Node_Numbers : in Boolean := False)
     return String;
   --  Includes Node.Node_Index, Node.ID

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   function Decimal_Image is new SAL.Generic_Decimal_Image (Node_Index);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Node_Index);

   function Get_Node_Index (Node : in Node_Access) return Node_Index;

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result;

   package Node_Sets is new SAL.Gen_Unbounded_Sparse_Ordered_Sets (Node_Access, Node_Access_Compare);

   function Error_Message
     (Tree      : in Syntax_Trees.Tree;
      Node      : in Valid_Node_Access;
      File_Name : in String;
      Message   : in String)
     return String;

   type Validate_Node is access procedure
     (Tree                : in     Syntax_Trees.Tree;
      Node                : in     Valid_Node_Access;
      Data                : in out User_Data_Type'Class;
      Descriptor          : in     WisiToken.Descriptor;
      File_Name           : in     String;
      Node_Image_Output   : in out Boolean;
      Node_Error_Reported : in out Boolean);
   --  Called by Validate_Tree for each node visited; perform checks
   --  other than parent/child, output to Text_IO.Current_Error.
   --
   --  Don't report errors if Node_Error_Reported is already True.
   --  Set Node_Error_Reported True if any errors are reported.
   --
   --  If Node_Image_Output is False, output Image (Tree, Node,
   --  Descriptor, Node_Numbers => True) once before any error messages.

   procedure Validate_Tree
     (Tree           : in out Syntax_Trees.Tree;
      User_Data      : in out User_Data_Type'Class;
      Descriptor     : in     WisiToken.Descriptor;
      File_Name      : in     String;
      Error_Reported : in out Node_Sets.Set;
      Root           : in     Node_Access                 := Invalid_Node_Access;
      Validate_Node  : in     Syntax_Trees.Validate_Node := null)
   with Pre'Class => Tree.Parents_Set;
   --  Verify child/parent links, and that no children are Deleted_Child.
   --  Call Validate_Node for each visited node. Violations output a
   --  message to Text_IO.Current_Error. Error_Reported is used to avoid
   --  outputing an error for a node more than once.

   type Image_Augmented is access function (Aug : in Base_Token_Class_Access) return String;
   type Image_Action is access function (Action : in Semantic_Action) return String;

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Descriptor      : in WisiToken.Descriptor;
      Root            : in Node_Access                   := Invalid_Node_Access;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null);
   --  Print tree rooted at Root (default Tree.Root) to
   --  Text_IO.Current_Output, for debugging. For each node,
   --  Image_Augmented is called if it is not null and node.augmented is
   --  not null.

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String;
   --  For debugging; node counts.

private
   use all type Ada.Containers.Count_Type;

   type Node
     (Label       : Node_Label;
      Child_Count : SAL.Base_Peek_Type)
     is
   --  Descriminants have no default because allocated nodes are
   --  constrained anyway (ARM 4.8 6/3).
   record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Node_Index : Syntax_Trees.Node_Index;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Computed by Set_Children, used in Semantic_Check actions and debug
      --  messages.

      Parent : Node_Access := Invalid_Node_Access;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that was on stack with this token, to allow
      --  undo_reduce and push_back in error recovery.

      Augmented : Base_Token_Class_Access := null;
      --  IMPROVEME: Augmented should not derive from Base_Token; that
      --  duplicates information. Not changing yet for compatibility with
      --  main devel branch.

      case Label is
      when Shared_Terminal =>
         Line        : Line_Number_Type  := Invalid_Line_Number;
         Column      : Ada.Text_IO.Count := 0;
         Char_Region : Buffer_Region     := Null_Buffer_Region;

         Non_Grammar : aliased Base_Token_Arrays.Vector; -- immediately following Terminal

      when Virtual_Terminal =>
         Before : Node_Access := Invalid_Node_Access;

      when Virtual_Identifier =>
         Identifier : Identifier_Index; -- into user data

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         RHS_Index : Natural;
         --  With ID, index into Productions.
         --  Used for debug output, keep for future use.

         Action : Semantic_Action := null;

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Node_Access_Array (1 .. Child_Count);
         --  We use an explicit array, rather than a pointer to the first
         --  child, to preserve child indices while editing the tree.

         First_Terminal_Index : Node_Access := Invalid_Node_Access;
         --  Cached for push_back of nonterminals during recovery
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   function Get_Node_Index (Node : in Node_Access) return Node_Index
   is (if Node = Invalid_Node_Access then 0 else Node.Node_Index);

   type Parse_Stream is record
      First : Stream_Index := Invalid_Stream_Index;
      Last  : Stream_Index := Invalid_Stream_Index;
   end record;

   type Stream_Element is record
      --  We use a separate set of stream pointers, rather than reusing the
      --  nonterm child pointers as in [1], to simpliy checks in Validate
      --  and during editing, and to preserve Child_Index when children are
      --  deleting during editing.
      Next : Stream_Index := Invalid_Stream_Index;
      Prev : Stream_Index := Invalid_Stream_Index;
      Node : Node_Access  := Invalid_Node_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Stream_Element, Stream_Index);

   type Element_Index is range 0 .. Integer'Last;
   subtype Valid_Element_Index is Element_Index range 1 .. Element_Index'Last;

   type Stream_ID is range 0 .. Integer'Last;
   subtype Valid_Stream_ID is Stream_ID range 1 .. Stream_ID'Last;
   Invalid_Stream_ID : constant Stream_ID := 0;

   function T_Image is new SAL.Gen_Trimmed_Image (Stream_ID);
   function Trimmed_Image (Item : in Stream_ID) return String
   renames T_Image;

   package Parse_Stream_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Valid_Stream_ID, Parse_Stream, (others => <>));

   package Node_Access_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node_Access, null);

   package Element_Access_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Element_Index, Stream_Index, null);

   type Tree is new Ada.Finalization.Controlled with record
      Leading_Non_Grammar : aliased WisiToken.Base_Token_Arrays.Vector;
      --  Non-grammar tokens before first grammar token; leading blank lines
      --  and comments.

      Streams : Parse_Stream_Arrays.Vector;
      --  FIXME: need a free list of streams; otherwise Streams gets
      --  unbounded large and sparse during incremental parse.
      --
      --  FIXME: Nodes gets unbounded large and nominally sparse during
      --  incremental parse.
      --
      --  One solution to both is to prune tree by copy/finalize after N
      --  edits.

      Elements : Element_Access_Arrays.Vector;
      --  For Finalize.

      Nodes : Node_Access_Arrays.Vector;
      --  Stores ref to all nodes, for Finalize. Also provides Node_Index;
      --  we may add access via Node_Index at some point.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

      Parents_Set : Boolean := False;
      --  We don't set Node.Parent until after parse is done; see Design
      --  note above.
   end record;

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 0);

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural
   is (Natural (Tree.Streams.Length));

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 1 and
         Tree.Streams (Tree.Streams.First_Index).First = Tree.Streams (Tree.Streams.First_Index).Last);

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   is (Tree.Streams (Tree.Streams.First_Index).First.Node);

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Parents_Set);

   Dummy_Node : constant Node_Access := new Node'(Label => Virtual_Identifier, Child_Count => 0, others => <>);

end WisiToken.Syntax_Trees;
