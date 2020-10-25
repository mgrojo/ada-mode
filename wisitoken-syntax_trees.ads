--  Abstract :
--
--  Syntax tree type and operations, implemented with directly
--  allocated nodes, providing parse streams as described in [1], but
--  generalized for parallel parsers.
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
--  Each parallel parser uses one stream as the parse stack.
--
--  In all streams, in Shared_Terminal nodes, Node_Index gives token
--  order in source text. In all streams, Stream_Element.Index is
--  unique but arbitrary, for debugging.
--
--  During batch parsing, the "shared stream" holds all of the shared
--  terminals read from the input source; this is populated by
--  Wisitoken.Parse.Lex_All. Nodes
--  are copied from the shared stream to parse streams in order to
--  store state for undo_reduce (as well as the Parent pointer). After
--  error correction, the sequence of terminals in a parse stream is
--  given by Next_Terminal/Prev_Terminal.
--
--  During incremental parse, the shared stream holds the edited parse
--  stream with new shared terminals inserted; Shared_Terminal nodes
--  Node_Index give source text order; Stream_Element.Index are
--  arbitrary. Elements from the shared stream are copied to each
--  parse stream as parsing progresses.
--
--  Tree are limited because a bit-copy is not a good start on copy
--  for assign; use Copy_Tree.
--
--  We can't traverse Tree.Streams to deallocate tree Nodes, either
--  when streams are terminated or during Finalize; in general Nodes
--  are referenced multiple times in multiple streams. So we keep
--  track of nodes to deallocate in Tree.Nodes.
--
--  Therefore Nodes are never deleted, except when the entire tree is
--  Finalized. Thus all nodes created by parsers that are terminated
--  are still in the tree; to prune them, copy the surviving tree to a
--  new tree, finalize the original.
--
--  Node contains a Parent link, to make it easy to traverse the tree
--  in any direction. Any tree editing functions that modify children
--  or parents update the corresponding links, setting them to
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
with Ada.Unchecked_Deallocation;
with SAL.Gen_Definite_Doubly_Linked_Lists;
with SAL.Gen_Trimmed_Image;
with SAL.Gen_Unbounded_Definite_Vectors;
with SAL.Gen_Unbounded_Sparse_Ordered_Sets;
with SAL.Generic_Decimal_Image;
with WisiToken.Lexer;
package WisiToken.Syntax_Trees is
   use all type SAL.Base_Peek_Type;

   type Node (<>) is private;
   type Node_Access is access all Node;
   subtype Valid_Node_Access is not null Node_Access;

   Invalid_Node_Access : constant Node_Access := null;
   Dummy_Node : constant Valid_Node_Access;
   --  Use when you must initialize a Valid_Node_Access before overwritting it.

   type Node_Access_Array is array (Positive_Index_Type range <>) of Node_Access;
   type Valid_Node_Access_Array is array (Positive_Index_Type range <>) of Valid_Node_Access;

   function To_Node_Access (Item : in Valid_Node_Access_Array) return Node_Access_Array;
   function To_Valid_Node_Access (Item : in Node_Access_Array) return Valid_Node_Access_Array;

   package Valid_Node_Access_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Valid_Node_Access);

   type Stream_ID is private;
   Invalid_Stream_ID : constant Stream_ID;

   type Stream_Index is private;
   Invalid_Stream_Index : constant Stream_Index;

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;
   Invalid_Node_Index : constant Node_Index := 0;

   type Tree (Descriptor : Descriptor_Access_Constant) is new Ada.Finalization.Limited_Controlled with private;
   --  Use Copy_Tree to get a copy.

   type Tree_Variable_Reference (Element : not null access Tree) is null record with
     Implicit_Dereference => Element;

   type Tree_Constant_Reference (Element : not null access constant Tree) is null record with
     Implicit_Dereference => Element;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   procedure Clear
     (Tree             : in out Syntax_Trees.Tree;
      Free_Memory      : in     Boolean := False;
      Initialize_Parse : in     Boolean := True);
   --  Delete all nodes in all streams, reset for new lex and parse.
   --  Free_Memory applies to internal bookkeeping; leaving it False may
   --  slightly speed parsing a similar sized file as the previous one.
   --
   --  If Initialize_Parse, create empty shared stream.

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean;

   type Node_Label is
     (Shared_Terminal,    -- text is user input, accessed via Parser.Terminals
      Virtual_Terminal,   -- no text; inserted during error recovery
      Virtual_Identifier, -- text in user data, created during tree rewrite
      Nonterm             -- contains terminals/nonterminals/identifiers
     );
   subtype Terminal_Label is Node_Label range Shared_Terminal .. Virtual_Identifier;
   subtype Virtual_Terminal_Label is Node_Label range Virtual_Terminal .. Virtual_Identifier;

   function Label (Node : in Valid_Node_Access) return Node_Label;

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID;

   type Stream_Node_Ref is record
      Element : Syntax_Trees.Stream_Index := Invalid_Stream_Index;
      Node    : Syntax_Trees.Node_Access  := Invalid_Node_Access;
      --  If not both Invalid, Element contains Node in some stream. If
      --  Element contains an empty nonterm, Node is Invalid_Node_Access. In
      --  post-parse actions, the parse stream is deleted, so Element is
      --  Invalid_Stream_Index.
   end record;

   Invalid_Stream_Node_Ref : constant Stream_Node_Ref;

   function Correct_Stream_Node_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Stream_Node_Ref)
     return Boolean;
   --  True if Ref is Invalid_Terminal_Ref or Element contains Node.

   function Valid_Stream_Node_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Stream_Node_Ref)
     return Boolean;
   --  True if Ref refers to a node.

   subtype Terminal_Ref is Stream_Node_Ref
   with Dynamic_Predicate =>
     Terminal_Ref.Node = Invalid_Node_Access or else
     Label (Terminal_Ref.Node) in Terminal_Label;

   Invalid_Terminal_Ref : constant Terminal_Ref;

   function Valid_Terminal_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Terminal_Ref)
     return Boolean
   is (Valid_Stream_Node_Ref (Tree, Stream, Ref));
   --  True if Ref refers to a Terminal node.

   function Single (Tree : in Syntax_Trees.Tree; Ref : in Terminal_Ref) return Boolean;
   --  True if Ref contains a single node.

   type Recover_Token is record
      --  Declared here because it needs Node_Access

      --  Maintaining a syntax tree during error recovery is too slow, so we
      --  store enough information in the recover stack to perform
      --  Semantic_Checks, Language_Fixes, and Push_Back operations. and to
      --  apply the solution to the main parser state. We make thousands of
      --  copies of the parse stack during recover, so minimizing size and
      --  compute time for this is critical.
      --
      --  Only ID and Byte_Region are set by parse during recover; the other
      --  fields are only set when converting from the main parse stack.
      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Byte_Region is used to detect empty tokens, for cost and other issues.

      First_Shared_Terminal : Stream_Node_Ref := Invalid_Stream_Node_Ref;
      --  For terminals, this token in the shared or parse stream. For
      --  nonterminals, first of contained shared terminals (Node is
      --  Invalid_Node_Access if empty; Element is still valid). Used for
      --  push_back of nonterminals.

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

   function Valid_Element_Node (Token : in Recover_Token) return Boolean;
   --  True if Token.First_Shared_Terminal.Element is valid.

   function Element_Node (Token : in Recover_Token) return Syntax_Trees.Node_Access
   with Pre => Valid_Element_Node (Token);
   --  Return node of Item.First_Shared_Terminal.Element

   function Element_Node_Index (Token : in Recover_Token) return Syntax_Trees.Node_Index;
   --  Returns Invalid_Node_Index if Valid_Element_Node is false;
   --  node_index of Element_Node otherwise.

   type Recover_Token_Array is array (Positive_Index_Type range <>) of Recover_Token;

   ----------
   --  User_Data_Type

   type Base_Augmented is tagged null record;
   type Augmented_Class_Access is access all Base_Augmented'Class;
   type Augmented_Class_Access_Constant is access constant Base_Augmented'Class;

   procedure Free is new Ada.Unchecked_Deallocation (Base_Augmented'Class, Augmented_Class_Access);

   type User_Data_Type is tagged limited null record;
   --  Many test languages don't need this, so we default the procedures
   --  to null.

   type User_Data_Access is access all User_Data_Type'Class;

   procedure Set_Lexer
     (User_Data           : in out User_Data_Type;
      Lexer               : in     WisiToken.Lexer.Handle;
      Line_Begin_Char_Pos : in     WisiToken.Line_Pos_Vector_Access)
     is null;

   procedure Reset (User_Data : in out User_Data_Type) is null;
   --  Reset to start a new parse.

   procedure Initialize_Actions
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class)
     is null;
   --  Called by Execute_Actions, before processing the tree, after
   --  Insert_Token/Delete_Token.

   procedure Lexer_To_Augmented
     (User_Data          : in out User_Data_Type;
      Tree               : in out Syntax_Trees.Tree'Class;
      Token              : in     Base_Token;
      Prev_Grammar_Token : in     Node_Access)
     is null;
   --  Token is a grammar or non-grammar token that was just returned by
   --  User_Data.Lexer. If grammar, it is Prev_Grammar_Token; if
   --  non-grammar, it has already been added to Prev_Grammar_Token, or
   --  to Parser.Leading_Non_Grammar if Prev_Grammar_Token is
   --  Invalid_Token_Index. Read auxiliary data from User_Data.Lexer, do
   --  something useful with it. Called before parsing, once for each
   --  non-grammar token in the input stream.

   function Copy_Augmented
     (User_Data : in User_Data_Type;
      Augmented : in Augmented_Class_Access)
     return Augmented_Class_Access
   with Pre => Augmented /= null;
   --  Default implementation raises SAL.Programmer_Error.

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Insert_Token         : in     Valid_Node_Access;
      Insert_Before_Token  : in     Valid_Node_Access;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean;
   --  Return True if Insert_Token should be treated as if inserted after
   --  the previous shared terminal, rather than before
   --  Insert_Before_Token. This can affect which line it appears on,
   --  which affects indentation. Called from Insert_Token.
   --
   --  If Insert_On_Blank_Line is True, there is at least one blank line
   --  before Insert_Before_Token.
   --
   --  The default implementation always returns False.

   procedure Insert_Token
     (User_Data      : in out User_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   is null
   with Pre'Class => Tree.Is_Virtual_Terminal (Inserted_Token);
   --  Inserted_Token was inserted in error recovery. Update
   --  other tokens as needed. Called from Execute_Actions for each
   --  inserted token, before Initialize_Actions.

   procedure Delete_Token
     (User_Data     : in out User_Data_Type;
      Tree          : in out Syntax_Trees.Tree'Class;
      Deleted_Token : in     Valid_Node_Access;
      Prev_Token    : in     Node_Access)
     is null
     with Pre'Class =>
       Tree.Label (Deleted_Token) = Shared_Terminal and
       (Prev_Token = Invalid_Node_Access or else Tree.Label (Prev_Token) in Shared_Terminal | Virtual_Terminal);
   --  Deleted_Token was deleted in error recovery. Prev_Token is the
   --  previous terminal token in the parse stream. Update remaining
   --  tokens as needed; Deleted_Token.Non_Grammar has _not_ been moved
   --  anywhere. Called from Execute_Actions for each deleted token,
   --  before Initialize_Actions.

   procedure Reduce
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Nonterm   : in     Valid_Node_Access;
      Tokens    : in     Node_Access_Array)
     is null;
   --  Reduce Tokens to Nonterm. Nonterm Base_Token components are
   --  computed by caller. Called by Parser.Execute_Actions, just before
   --  processing Nonterm.

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
   --  Parsing operations (including error recovery and incremental
   --  parse), Tree and Node attributes.

   function Leading_Non_Grammar (Tree : aliased in out Syntax_Trees.Tree) return Base_Token_Array_Var_Ref;

   function Leading_Non_Grammar_Const (Tree : aliased in Syntax_Trees.Tree) return Base_Token_Array_Const_Ref;

   function New_Stream
     (Tree       : in out Syntax_Trees.Tree;
      Old_Stream : in     Stream_ID;
      User_Data  : in     User_Data_Access)
      return Stream_ID
   with
     Pre => (Tree.Stream_Count = 1 and Old_Stream = Invalid_Stream_ID) or else
            (Tree.Stream_Count > 1 and (Tree.Is_Valid (Old_Stream))),
     Post => Tree.Is_Valid (New_Stream'Result);
   --  Create a new parse stream, initially copied from Old_Stream.

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural;
   --  Number of active streams.

   function First_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   with Pre => Tree.Stream_Count >= 2;

   function Stream_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Valid (Stream);
   --  Stack + input

   function Stream_Input_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Valid (Stream);

   function Stack_Depth (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Valid (Stream);

   procedure Delete_Stream (Tree : in out Syntax_Trees.Tree; Stream : in out Stream_ID)
   with Pre => Tree.Is_Valid (Stream);
   --  Must be done when Stream is terminated for error.

   function Is_Valid (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Boolean;
   --  Stream is available for parsing operations.

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean
   with Pre => Tree.Is_Valid (Stream) and Token /= Invalid_Stream_Index;
   --  Token is an element of Stream.

   procedure Set_Terminal_Index
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Valid_Node_Access;
      Index    : in     Node_Index)
   with Pre => Tree.Label (Terminal) = Shared_Terminal;
   --  Set Node_Index of Terminal to Index; must reflect token order
   --  in source buffer.

   function Trimmed_Image (Item : in Stream_Index) return String;
   --  Trimmed_Image of item.node_index.

   function Trimmed_Image (Node : in Node_Access) return String;
   --  Trimmed_Image of item.node_index.

   function Get_Node
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Valid_Node_Access
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);
   --  Parser.Current_Token may be from either stream.

   function Get_Node
     (Element : in Stream_Index)
     return Valid_Node_Access
   with Pre => Element /= Invalid_Stream_Index;
   --  When we don't have the stream.

   procedure Start_Parse
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   with Pre => not Tree.Traversing and (Tree.Is_Valid (Stream) and then Tree.Stream_Length (Stream) = 0);
   --  State must be the parser start state; a new stream element and
   --  tree node are created for it.

   procedure Finish_Parse
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Token     : in     Stream_Index;
      User_Data : in     User_Data_Access)
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream) and Tree.ID (Stream, Token) = Tree.Descriptor.EOI_ID;
   --  If Token is from Shared_Stream, copy it from Shared_Stream to
   --  Parse_Stream, after Stack_Top, to mark end of parse stream.
   --  Otherwise Token is from the parse stream; do nothing.

   procedure Start_Edit (Tree : in out Syntax_Trees.Tree)
   with Pre => Tree.Fully_Parsed,
     Post => Tree.Parseable;
   --  Delete Tree.Shared_Stream, relabel the parse stream as
   --  Shared_Stream, delete first element, ready for Parse.Edit_Tree.
   --  Sets Tree.Incremental_Parse True.

   procedure Update_Cache (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID);
   --  In all nodes in Stream, update cached token positions from terminals.

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there are no parse streams and
   --  Shared_Stream holds a lexed or edited stream.

   function Incremental_Parse (Tree : in Syntax_Trees.Tree) return Boolean;

   function Reduce
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Child_Count     : in     Ada.Containers.Count_Type;
      Action          : in     Semantic_Action := null;
      State           : in     Unknown_State_Index;
      Default_Virtual : in     Boolean         := False)
     return Stream_Node_Ref
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream),
     Post => Tree.Valid_Stream_Node_Ref (Stream, Reduce'Result);
   --  Reduce Child_Count tokens on end of Stream to a new Nonterm node on
   --  Stream. Result points to the new Nonterm node. If Child_Count = 0,
   --  set Nonterm.Virtual := Default_Virtual.
   --
   --  Set Result byte_region, char_region, line, column,
   --  first_terminal_index to min/max of children.

   procedure Undo_Reduce
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream) and Tree.Label (Stream) = Nonterm;
   --  Undo reduction of nonterm at Stream.Stack_Top; Stack_Top is then
   --  the last Child of the nonterm.

   procedure Shift
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      State     : in     Unknown_State_Index;
      Token     : in     Stream_Index;
      User_Data : in     User_Data_Access)
   with Pre => not Tree.Traversing and
               (Tree.Contains (Tree.Shared_Stream, Token) or else
                  Token = Tree.Stream_Next (Stream, Tree.Stack_Top (Stream)));
   --  If Token is in Shared_Stream, copy Token from Shared_Stream to
   --  Stream.Stack_Top; otherwise move from Stream input to
   --  Stream.Stack_Top. Then set State in the Stream token.

   procedure Left_Breakdown
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   with Pre => Valid_Terminal_Ref (Tree, Stream, Ref) and Tree.Label (Ref.Element) = Nonterm and
               Tree.Child_Count (Tree.Get_Node (Stream, Ref.Element)) > 0 and
               Tree.Stack_Top (Stream) /= Ref.Element,
     Post => Correct_Stream_Node_Ref (Tree, Stream, Ref);
   --  [Wagner Graham 1998] Left_Breakdown of Ref for Parse_Incremental;
   --  bring the first terminal in Ref or a following stream element (if
   --  Ref is empty or virtual) to the parse stream. Ref is updated to
   --  the first terminal, or Invalid_Terminal_Ref if Ref is empty and
   --  the last element in Stream.
   --
   --  The stack top is unchanged.

   procedure Right_Breakdown
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   with Pre => Tree.Is_Valid (Stream) and Tree.Label (Tree.Stack_Top (Stream)) = Nonterm;
   --  [Wagner Graham 1998] Right_Breakdown of Stream.Stack_Top; bring
   --  the last terminal in Stream.Stack_Top to Stream.Stack_Top.
   --
   --  FIXME: using Undo_Reduce instead; delete?

   function State (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Unknown_State_Index
   with Pre => Tree.Is_Valid (Stream);
   --  Return State from node at Stream.Stack_Top.

   function State
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Unknown_State_Index
   with Pre => Tree.Contains (Stream, Element);

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Unknown_State_Index;

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Terminal_Ref
   with Pre => Tree.Is_Valid (Stream);

   function Stream_Last
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   function Stack_Top
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   procedure Set_Stack_Top
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in     Stream_Index)
   with Pre => Tree.Contains (Stream, Element);

   function First_Input
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Terminal_Ref
   with Pre => Tree.Is_Valid (Stream);
   --  Return first Shared_Terminal in first stream element after
   --  Stack_Top.

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Element = Invalid_Stream_Index or else
               (Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element));
   --  If Element is Invalid_Stream_Index, result is Stream_First

   function Stream_Prev
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Tree.Contains (Stream, Element);

   function Peek
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Count  : in SAL.Peek_Type := 1)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);
   --  Return Count element before last element in Stream; Count = 1
   --  returns last element (= stack top).

   procedure Pop (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID)
   with Pre => Tree.Is_Valid (Stream);

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Base_Token)
     return Terminal_Ref
   with Pre => not Tree.Traversing and Stream = Tree.Shared_Stream,
     Post => Tree.Valid_Terminal_Ref (Stream, Add_Terminal'Result) and
             Tree.Single (Add_Terminal'Result) and
             Tree.Label (Add_Terminal'Result.Node) = Shared_Terminal;
   --  Add a new Shared_Terminal element in Stream. Result points to the added
   --  node.

   function Copy_Subtree
     (Tree      : in out Syntax_Trees.Tree;
      Root      : in     Node_Access;
      User_Data : in     User_Data_Access)
     return Node_Access;
   --  Deep copy (into Tree) subtree of Tree rooted at Root. Return root
   --  of new subtree; it has no parent.
   --
   --  If Root is Invalid_Node_Access, returns Invalid_Node_Access

   function Non_Grammar_Var
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Valid_Node_Access)
     return Base_Token_Array_Var_Ref
   with Pre => Tree.Label (Terminal) in Shared_Terminal | Virtual_Identifier;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Array_Const_Ref
   with Pre => Tree.Label (Terminal) in Shared_Terminal | Virtual_Identifier;

   function Insert_Shared_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Base_Token;
      Index    : in     Node_Index;
      Before   : in     Stream_Index)
     return Terminal_Ref
   with Pre => not Tree.Traversing and Tree.Contains (Stream, Before),
     Post => Tree.Valid_Terminal_Ref (Stream, Insert_Shared_Terminal'Result) and
             Tree.Single (Insert_Shared_Terminal'Result) and
             Tree.Label (Insert_Shared_Terminal'Result.Node) = Shared_Terminal;
   --  Insert a new Shared_Terminal element on Stream, before Before.
   --  Index should give the source token order. Result points to the
   --  added element.

   function Insert_Virtual_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID;
      Before   : in     Stream_Index)
     return Terminal_Ref
   with
     Pre  => not Tree.Traversing and Tree.Is_Valid (Stream) and Stream /= Tree.Shared_Stream and
             Tree.Contains (Tree.Shared_Stream, Before),
     Post => Tree.Valid_Terminal_Ref (Stream, Insert_Virtual_Terminal'Result) and
             Tree.Single (Insert_Virtual_Terminal'Result) and
             Tree.Label (Insert_Virtual_Terminal'Result.Node) = Virtual_Terminal;
   --  Insert a new Virtual_Terminal element into Stream, after
   --  Stack_Top. Before should be the Shared_Stream token this token is
   --  inserted before; new token Line, Char_Region is set to
   --  Before.Line, Char_Region.First. Result points to the added node.

   procedure Update
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Byte_Region : in Buffer_Region;
      Char_Region : in Buffer_Region;
      Line        : in Line_Number_Type;
      Column      : in Ada.Text_IO.Count)
   with Pre => Tree.Label (Node) in Virtual_Terminal | Virtual_Identifier;

   procedure Shift
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Shift_Bytes : in Base_Buffer_Pos;
      Shift_Chars : in Base_Buffer_Pos;
      Shift_Line  : in Base_Line_Number_Type)
   with Pre => Tree.Label (Node) = Shared_Terminal;
   --  Add Shift_* to token positions.

   procedure Stream_Delete
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in out Stream_Index)
   with
     Pre  => Tree.Contains (Stream, Element),
     Post => Element = Invalid_Stream_Index;
   --  If Element = Stream.Stack_Top, Stack_Top is set to Invalid_Stream_Index.

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);
   --  For example, Parser.Current_Token is either a Shared_Terminal from
   --  Shared_Stream or a Virtual_Terminal on Stream from error
   --  recovery; in incremental parse, it could be a Shared_Terminal on
   --  Stream from breakdown.

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label;
   function Label (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Node_Label
   with Pre => Element /= Invalid_Stream_Index;

   function Label (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Node_Label
   with Pre => Tree.Is_Valid (Stream);
   --  Label of Stream.Last; top of stack

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   with Pre => Tree.Is_Nonterm (Node);

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access_Array
   with Pre => Tree.Is_Nonterm (Node);
   --  Any children that were deleted by tree editing are returned as
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
   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;

   function Is_Virtual
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Boolean
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);
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

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   with Pre => Index /= Invalid_Stream_Index;

   function Byte_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Recover_Token;

   function Children_Recover_Tokens
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Recover_Token_Array
   with Pre => Tree.Contains (Stream, Element) and Tree.Label (Element) = Nonterm;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Access;
      Value : in     Augmented_Class_Access);
   --  Value will be deallocated when Tree is finalized.

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access;
   --  Returns result of Set_Augmented.

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access_Constant;

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
     return Node_Access;
   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Access;
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
   with Pre => Tree.Has_Parent (Node);
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
   with Pre => Tree.Is_Nonterm (Root);

   function Sub_Tree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access;
   --  Return top ancestor of Node.

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access);
      Root         : in     Node_Access := Invalid_Node_Access)
   with Pre => Root /= Invalid_Node_Access or else Tree.Root /= Invalid_Node_Access;
   --  Traverse subtree of Tree rooted at Root (default single remaining
   --  stream element) in depth-first order, calling Process_Node on each
   --  node.

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Base_Identifier_Index
   with Pre => Tree.Is_Virtual_Identifier (Node);

   function Base_Token
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return WisiToken.Base_Token
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);

   function Base_Token (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Base_Token;

   function First_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access;
   --  Returns first shared terminal node (in parse tree) in subtree
   --  under Node (ignoring virtual terminals). If result is
   --  Invalid_Node_Access, all terminals are virtual, or Node is empty.

   function First_Shared_Terminal
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Terminal_Ref
   with Pre => Tree.Contains (Stream, Element),
     Post => Valid_Terminal_Ref (Tree, Stream, First_Shared_Terminal'Result);
   --  Returns first shared terminal in Element (ignoring virtual
   --  terminals). If result is Invalid_Node_Access, all terminals in
   --  Element are virtual, or Element is empty.

   function Last_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access;
   --  Returns last shared terminal in subtree under Element (ignoring
   --  virtual terminals). If result is Invalid_Stream_Index, all
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
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Prev_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately before Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the first terminal
   --  in that subtree.

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Next_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately after Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the last terminal
   --  in that subtree.

   function Next_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Next_Shared_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Shared_Terminal'Result) = Shared_Terminal;
   --  Return the next Shared_Terminal that is after Node in subtree
   --  containing Node; Invalid_Node_Access if Node is last shared
   --  terminal in subtree.

   procedure Next_Shared_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   with Pre => Valid_Terminal_Ref (Tree, Stream, Ref),
     Post => Correct_Stream_Node_Ref (Tree, Stream, Ref) and
             (Ref.Node = Invalid_Node_Access or else Tree.Label (Ref.Node) = Shared_Terminal);
   --  Update Ref to the next Shared_Terminal that is after
   --  Node in Stream; Invalid_Node_Access if Node is EOI (at end of
   --  stream).

   function Next_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Terminal_Ref)
     return Terminal_Ref
   with Pre => Valid_Terminal_Ref (Tree, Stream, Ref),
     Post => Correct_Stream_Node_Ref (Tree, Stream, Next_Shared_Terminal'Result) and
             (Next_Shared_Terminal'Result.Node = Invalid_Node_Access or else
                Tree.Label (Next_Shared_Terminal'Result.Node) = Shared_Terminal);
   --  Same as procedure Next_Shared_Terminal, but returns the updated ref.

   procedure Prev_Shared_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   with Pre => Valid_Terminal_Ref (Tree, Stream, Ref),
     Post => Correct_Stream_Node_Ref (Tree, Stream, Ref) and
             (Ref.Node = Invalid_Node_Access or else Tree.Label (Ref.Node) = Shared_Terminal);
   --  Update Element, Node to the next Shared_Terminal that is before
   --  Node in Stream; Invalid_Node_Access if Node is first
   --  Shared_Terminal in Stream.

   function Prev_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Terminal_Ref)
     return Terminal_Ref
   with Pre => Valid_Terminal_Ref (Tree, Stream, Ref),
     Post => Correct_Stream_Node_Ref (Tree, Stream, Prev_Shared_Terminal'Result) and
             (Prev_Shared_Terminal'Result.Node = Invalid_Node_Access or else
                Tree.Label (Prev_Shared_Terminal'Result.Node) = Shared_Terminal);
   --  Same as procedure Prev_Shared_Terminal, but returns the updated result.

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
   --
   --  Some of these are also used for Packrat parsing, and don't have a
   --  precondition of Fully_Parsed.

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there is only one parse stream, and it has only three elements;
   --  the start state, the tree root, and EOI.

   function Editable (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if Clear_Parse_Streams has been called; the remaining tree
   --  may be arbitrarily edited.

   procedure Copy_Tree
     (Source      : in     Tree;
      Destination :    out Tree;
      User_Data   : in     User_Data_Access)
   with Pre => Fully_Parsed (Source) or Editable (Source);
   --  If Editable, the subtree at Tree.Root is copied; if Fully_Parsed,
   --  the subtree rooted in the single remaining parse stream, the parse
   --  stream and shared stream are copied. All references are deep
   --  copied; Source may be finalized after this operation.

   procedure Clear_Parse_Streams (Tree : in out Syntax_Trees.Tree)
   with Post => Tree.Editable;
   --  If Tree.Root is not set, first set Tree.Root to the root of the
   --  single remaining parse stream. Delete the parse stream and
   --  shared stream, but not the nodes they contain. This allows Tree
   --  to be edited without corrupting the parse stream.
   --
   --  No precondition for Packrat parser.

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access;
   --  Tree.Root, or the root in the single stream if Tree.Root is not
   --  set. No precondition for packrat.

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access);

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access;
   --  Return Count parent of Node.

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Clear_Parents   : in     Boolean;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Children'First = 1;
   --  Add a new Nonterm node (not on any stream), containing
   --  Children, with no parent. Result points to the added node. If
   --  Children'Length = 0, set Nonterm.Virtual := Default_Virtual.
   --
   --  Children.Parent are set to the new node. If a child has a previous
   --  parent, then if Clear_Parents, the corresponding entry in the
   --  parent's Children is set to null; if not Clear_Parents and
   --  assertions are enabled, Assertion_Error is raised.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Base_Token)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Terminal node with no parent, on no stream. Result
   --  points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Virtual_Terminal node with no parent, on no stream.
   --  Result points to the added node.

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
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Virtual_Identifier node with no parent, on no stream.
   --  Byte_Region should point to an area in the source buffer related
   --  to the new identifier, to aid debugging. Result points to the
   --  added node.

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
     Pre => not Tree.Traversing and Tree.Editable and
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
      Node     : in out Valid_Node_Access;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Node_Access_Array)
   with
     Pre => not Tree.Traversing and Tree.Editable and
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
   --
   --  Node is 'in out' because it must be reallocated if Children'length
   --  /= Node.Children'length. If it is reallocated,
   --  Node.Parent.Children is updated; the caller must update any other
   --  copies of Node.

   procedure Delete_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
   with
     Pre => Tree.Editable and (not Tree.Traversing) and
            Tree.Parent (Node) /= Invalid_Node_Access;
   --  Set child in Node.Parent to Invalid_Node_Access. If Node.Parent =
   --  Tree.Root, set Tree.Root to Node. Set Node.Parent to
   --  Invalid_Node_Access.

   ----------
   --  Debug and error message utils.
   --
   --  Typically no preconditions so they help with debugging errors
   --  detected by other preconditions.

   function Trimmed_Image (Tree : in Syntax_Trees.Tree; Item : in Stream_ID) return String;
   function Next_Stream_ID_Trimmed_Image (Tree : in Syntax_Trees.Tree) return String;
   --  Trimmed integer.

   function Image
     (Tree        : in Syntax_Trees.Tree;
      Children    : in Boolean     := False;
      Non_Grammar : in Boolean     := False;
      Root        : in Node_Access := Invalid_Node_Access)
     return String;
   --  Image of all streams, or root node if no streams.
   --  If Children, subtree of each stream element is included.

   function Image
     (Tree        : in Syntax_Trees.Tree;
      Stream      : in Stream_ID;
      Children    : in Boolean := False;
      Non_Grammar : in Boolean := False)
     return String;
   --  Image of each node. If Children, each entire subtree is included,
   --  with newlines, as in Print_Tree.

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False)
     return String;
   --  Element can be from any stream, or Invalid_Stream_Index

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Node                  : in Node_Access;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Non_Grammar           : in Boolean := False)
     return String;
   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Nodes                 : in Node_Access_Array;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Non_Grammar           : in Boolean := False)
     return String;
   --  Includes Node.Node_Index, Node.ID

   function Image (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return String;

   function Decimal_Image is new SAL.Generic_Decimal_Image (Node_Index);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Node_Index);

   function Get_Node_Index (Node : in Node_Access) return Node_Index;
   function Get_Node_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Node_Index;
   function Get_Node_Index (Element : in Stream_Index) return Node_Index;
   function Get_Node_Index
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Node_Index
   with Pre => Tree.Contains (Stream, Element);
   --  Version without Tree requires Syntax_Trees.Get_Node_Index.

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result;

   package Node_Sets is new SAL.Gen_Unbounded_Sparse_Ordered_Sets (Node_Access, Node_Access_Compare);

   function Error_Message
     (Tree                : in Syntax_Trees.Tree;
      Node                : in Valid_Node_Access;
      Line_Begin_Char_Pos : in Line_Pos_Vectors.Vector;
      File_Name           : in String;
      Message             : in String)
     return String;

   type Validate_Node is access procedure
     (Tree                : in     Syntax_Trees.Tree;
      Node                : in     Valid_Node_Access;
      Data                : in out User_Data_Type'Class;
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
   --  Node_Numbers => True) once before any error messages.

   procedure Validate_Tree
     (Tree                : in out Syntax_Trees.Tree;
      User_Data           : in out User_Data_Type'Class;
      Line_Begin_Char_Pos : in     WisiToken.Line_Pos_Vectors.Vector;
      File_Name           : in     String;
      Error_Reported      : in out Node_Sets.Set;
      Root                : in     Node_Access                := Invalid_Node_Access;
      Validate_Node       : in     Syntax_Trees.Validate_Node := null);
   --  Verify that no children are Invalid_Node_Access. Verify
   --  child/parent links. Call Validate_Node for each visited node.
   --  Violations output a message to Text_IO.Current_Error.
   --  Error_Reported is used to avoid outputing an error for a node more
   --  than once.

   type Image_Augmented is access function (Aug : in Augmented_Class_Access_Constant) return String;
   type Image_Action is access function (Action : in Semantic_Action) return String;

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Root            : in Node_Access                  := Invalid_Node_Access;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null;
      Non_Grammar     : in Boolean                      := False);
   --  Print tree rooted at Root (default Tree.Root) to
   --  Text_IO.Current_Output, for debugging. For each node,
   --  Image_Augmented is called if it is not null and node.augmented is
   --  not null.

   procedure Print_Streams (Tree : in Syntax_Trees.Tree; Non_Grammar : in Boolean := False);

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

      Node_Index : Syntax_Trees.Node_Index := 0;
      --  If Shared_Terminal, corresponds to text order. If not
      --  Shared_Terminal, unique and arbitrary, for debugging.

      Byte_Region : aliased Buffer_Region := Null_Buffer_Region;
      Char_Region : aliased Buffer_Region := Null_Buffer_Region;
      --  Computed by Update_Cache, used in Semantic_Check actions and debug
      --  messages.

      Line : Line_Number_Type := Invalid_Line_Number;
      --  For nonterms, computed by Update_Cache, used in debug messages.

      Parent : Node_Access := Invalid_Node_Access;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that is on the parse stack with this token. Required
      --  for error recover Undo_Reduce, Left_, Right_Breakdown.

      Augmented : Augmented_Class_Access := null;
      --  IMPROVEME: Augmented should not derive from Base_Token; that
      --  duplicates information. Not changing yet for compatibility with
      --  main devel branch.

      case Label is
      when Shared_Terminal =>
         Non_Grammar : aliased Base_Token_Arrays.Vector;
         --  Immediately following Node. In initial parse, this can only be in
         --  a Shared_Terminal node. But editing the tree can copy it to a Virtual_Identifier
         --  node.

      when Virtual_Terminal =>
         null;

      when Virtual_Identifier =>
         Identifier : Identifier_Index; -- into user data

         VI_Non_Grammar : aliased Base_Token_Arrays.Vector;
         --  Immediately following Node.

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

         First_Shared_Terminal : Node_Access := Invalid_Node_Access;
         --  Cached for push_back of nonterminals during error recovery
      end case;
   end record;

   type Stream_Label is range -2 .. Integer'Last;
   --  First parser has label 0, for compatibility with tests, and for
   --  general sanity. There is no practical upper limit; parsing a large
   --  file spawns and terminates thousands of parsers.

   Invalid_Stream_Label : constant Stream_Label := -2;
   Shared_Stream_Label  : constant Stream_Label := -1;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Stream_Label);

   type Stream_Element is record
      --  We use separate stream pointers, rather than reusing the nonterm
      --  child pointers as in [1], to allow each parallel parser to have
      --  its own stream. This also preserves Child_Index when children are
      --  deleted during editing.
      Node : Node_Access  := Invalid_Node_Access;

      Label : Stream_Label; -- allows checking if Element is from Shared_Stream or a parse stream.
   end record;

   package Stream_Element_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Stream_Element);

   type Stream_Index is record
      Cur : Stream_Element_Lists.Cursor;
   end record;

   Invalid_Stream_Index    : constant Stream_Index    := (Cur => Stream_Element_Lists.No_Element);
   Invalid_Stream_Node_Ref : constant Stream_Node_Ref := (Invalid_Stream_Index, Invalid_Node_Access);

   type Parse_Stream is record
      Label : Stream_Label := Invalid_Stream_Label;

      Stack_Top : Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element;
      --  The top of the parse stack. The stack is Stack_Top and previous
      --  elements, the input stream is the following elements, or
      --  Shared_Stream if Stack_Top.Next is Invalid_Stream_Index. In
      --  batch parsing with no error correction, this is always Last. In
      --  Shared_Stream, always Invalid_Stream_Index.

      Elements : Stream_Element_Lists.List;
   end record;

   package Parse_Stream_Lists is new SAL.Gen_Definite_Doubly_Linked_lists (Parse_Stream);
   use all type Parse_Stream_Lists.Cursor;

   type Stream_ID is record
      Cur : Parse_Stream_Lists.Cursor;
   end record;

   Invalid_Stream_ID : constant Stream_ID := (Cur => Parse_Stream_Lists.No_Element);

   package Node_Access_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node_Access, null);

   type Tree (Descriptor : Descriptor_Access_Constant) is new Ada.Finalization.Limited_Controlled with record
      Leading_Non_Grammar : aliased WisiToken.Base_Token_Arrays.Vector;
      --  Non-grammar tokens before first grammar token; leading blank lines
      --  and comments.

      Next_Stream_Label : Stream_Label := Shared_Stream_Label + 1;

      Next_Terminal_Node_Index : Node_Index := 1;

      Root : Node_Access := Invalid_Node_Access;

      Streams : Parse_Stream_Lists.List;

      Shared_Stream : Stream_ID;

      Nodes : Node_Access_Arrays.Vector;
      --  Stores ref to all nodes, for Finalize. Also provides Node_Index
      --  for non-Shared_Terminals.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

      Incremental_Parse : Boolean := False;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   function Base_Token
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return WisiToken.Base_Token
   is (Base_Token (Tree, Tree.Streams (Stream.Cur).Elements (Element.Cur).Node));

   function Base_Token (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Base_Token
   is (Node.ID, Node.Byte_Region, Node.Line, Node.Char_Region);

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   is (Stream_Element_Lists.Constant_Ref (Index.Cur).Node.Byte_Region);

   function Byte_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region
   is (Node.Byte_Region);

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean
   is ((Tree.Is_Valid (Stream) and Token /= Invalid_Stream_Index) and then
         Stream_Element_Lists.Constant_Ref (Token.Cur).Label = Tree.Streams (Stream.Cur).Label);

   function Correct_Stream_Node_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Stream_Node_Ref)
     return Boolean
   is (((Ref.Element = Invalid_Stream_Index and Ref.Node = Invalid_Node_Access) or else
          (Ref.Node = Invalid_Node_Access or else
             (Tree.Contains (Stream, Ref.Element) and
                Tree.Sub_Tree_Root (Ref.Node) = Tree.Get_Node (Stream, Ref.Element)))));

   function Editable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 0 and Tree.Shared_Stream.Cur = Parse_Stream_Lists.No_Element);

   function Element_Node (Token : in Recover_Token) return Syntax_Trees.Node_Access
   is (Stream_Element_Lists.Constant_Ref (Token.First_Shared_Terminal.Element.Cur).Node);

   function Element_Node_Index (Token : in Recover_Token) return Syntax_Trees.Node_Index
   is (if Valid_Element_Node (Token) then Get_Node_Index (Element_Node (Token)) else Invalid_Node_Index);

   function First_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is (Cur => Parse_Stream_Lists.Next (Tree.Shared_Stream.Cur));

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 2 and then Tree.Stream_Length ((Cur => Tree.Streams.Last)) = 3);
   --  1 stream for Shared, one for the successful parser. Parse
   --  stream has 3 elements: start state, parse tree, EOI.

   function Get_Node
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Valid_Node_Access
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node);

   function Get_Node
     (Element : in Stream_Index)
     return Valid_Node_Access
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node);

   function Get_Node_Index (Node : in Node_Access) return Node_Index
   is (if Node = Invalid_Node_Access then 0 else Node.Node_Index);

   function Get_Node_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Node_Index
   is (if Node = Invalid_Node_Access then 0 else Node.Node_Index);

   function Get_Node_Index (Element : in Stream_Index) return Node_Index
   is (if Stream_Element_Lists.Has_Element (Element.Cur)
       then Stream_Element_Lists.Constant_Ref (Element.Cur).Node.Node_Index
       else 0);

   function Get_Node_Index
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Node_Index
   is (Get_Node_Index (Element));

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node.ID);

   function Incremental_Parse (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Incremental_Parse);

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 0 and Tree.Root = Invalid_Node_Access);

   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is (Tree.Label (Node) in Terminal_Label);

   function Is_Valid (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Boolean
   is (Parse_Stream_Lists.Has_Element (Stream.Cur));

   function Label (Node : in Valid_Node_Access) return Node_Label
   is (Node.Label);

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label
   is (Node.Label);

   function Label (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Node_Label
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node.Label);

   function Label (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Node_Label
   is (Stream_Element_Lists.Constant_Ref (Tree.Streams (Stream.Cur).Elements.Last).Node.Label);

   function Next_Stream_ID_Trimmed_Image (Tree : in Syntax_Trees.Tree) return String
   is (Trimmed_Image (Tree.Next_Stream_Label));

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 1);

   function Stack_Top
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Stream_Index
   is ((Cur => Tree.Streams (Stream.Cur).Stack_Top));

   function State
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Unknown_State_Index
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node.State);

   function State (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Unknown_State_Index
   is (Stream_Element_Lists.Constant_Ref (Tree.Streams (Stream.Cur).Stack_Top).Node.State);

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Unknown_State_Index
   is (Node.State);

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural
   is (Natural (Tree.Streams.Length));

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   is ((Cur => Tree.Streams (Stream.Cur).Elements.First));

   function Stream_Last
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Stream_Index
   is ((Cur => Tree.Streams (Stream.Cur).Elements.Last));

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is (if Element = Invalid_Stream_Index
       then (Cur => Tree.Streams (Stream.Cur).Elements.First)
       else (Cur => Stream_Element_Lists.Next (Element.Cur)));

   function Stream_Prev
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is ((Cur => Stream_Element_Lists.Previous (Element.Cur)));

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is (Tree.Shared_Stream);

   function Single (Tree : in Syntax_Trees.Tree; Ref : in Terminal_Ref) return Boolean
   is (declare
          Element : Stream_Element renames Stream_Element_Lists.Constant_Ref (Ref.Element.Cur);
       begin
          Element.Node = Ref.Node);

   function Trimmed_Image (Tree : in Syntax_Trees.Tree; Item : in Stream_ID) return String
   is (Trimmed_Image (Tree.Streams (Item.Cur).Label));

   function Trimmed_Image (Item : in Stream_Index) return String
   is (if Item = Invalid_Stream_Index
       then "-"
       else Trimmed_Image (Stream_Element_Lists.Constant_Ref (Item.Cur).Node.Node_Index));

   function Trimmed_Image (Node : in Node_Access) return String
   is (if Node = Invalid_Node_Access
       then "-"
       else Trimmed_Image (Node.Node_Index));

   function Valid_Element_Node (Token : in Recover_Token) return Boolean
   is (Token.First_Shared_Terminal.Element /= Invalid_Stream_Index);

   function Valid_Stream_Node_Ref
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Stream_Node_Ref)
     return Boolean
   is (Tree.Contains (Stream, Ref.Element) and
         Tree.Sub_Tree_Root (Ref.Node) = Tree.Get_Node (Stream, Ref.Element));

   Dummy_Node : constant Node_Access := new Node'(Label => Virtual_Identifier, Child_Count => 0, others => <>);

   Invalid_Terminal_Ref : constant Terminal_Ref := (Invalid_Stream_Index, Invalid_Node_Access);

end WisiToken.Syntax_Trees;
