--  Abstract :
--
--  Syntax tree type and operations, providing parse streams as
--  described in [1], but generalized for parallel parsers.
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
--  The Shared_Stream contains all of the input source text, either as
--  terminal tokens from the lexer, or a mix of terminal and
--  nonterminal tokens from Parse.Edit_Tree. The sequence of tokens is
--  given by Next_Terminal/Prev_Terminal, and Node_Index on terminals
--  is positive and sequential. The shared stream may contain
--  Virtual_Terminals from previous error recovery; they are included
--  in the positive Node_Index order.
--
--  Each parallel parser uses one stream as the parse stack and
--  auxiliarty input stream. The auxiliary input stream contains
--  tokens that are pushed back in error recover, or referenced from
--  Shared_Stream for breakdown in incremental parse.
--
--  Each node contains a Parent link, to make it easy to traverse the
--  tree in any direction. However, we do not set the Parent links in
--  streams other than the shared stream while parsing, to avoid
--  having to copy nodes; that would mean copying the entire tree for
--  each new parallel parser, which is far too slow. Parent links are
--  set in the shared stream, to allow Next_Terminal and similar
--  terminal navigation operations. The parent links in the final
--  parsed tree are set by Set_Parents, which is called by
--  Parser.Execute_Actions before the actions are executed, and by
--  Parse.Edit_Tree before editing. After calling Set_Parents (ie,
--  while editing the syntax tree after parse), any functions that
--  modify children or parents update the corresponding links, setting
--  them to Invalid_Node_Access as appropriate.
--
--  During parsing, nodes are never copied or edited in any way once
--  they are created.
--
--  Incremental parse and error recover need to use terminal
--  navigation operations on nodes that are not from the shared
--  stream; the subprograms use a temporary internal stack to store
--  the parent links needed.
--
--  We don't store the parse State in syntax tree nodes, to avoid
--  having to copy nodes during parsing. State is stored in the parse
--  stream elements. This means Parse.LR.Undo_Reduce has to call
--  Action_For to compute the state for the child nodes.
--
--  After error correction, the sequence of terminals in a parse
--  stream is given by Next_Terminal/Prev_Terminal; Node_Index on
--  inserted virtuals is negative, and has gaps for deleted tokens.
--
--  A terminal is a "shared terminal" if it has a positive Node_Index;
--  it is in the Shared_Stream.
--
--  Type Tree is limited because a bit-copy is not a good start on copy
--  for assign; use Copy_Tree.
--
--  We can't traverse Tree.Streams to deallocate tree Nodes, either
--  when streams are terminated or during Finalize; in general Nodes
--  are referenced multiple times in multiple streams. So we keep
--  track of nodes to deallocate in Tree.Nodes. Therefore Nodes are
--  never deleted, except in Clear_Parse_Streams and when the entire
--  tree is Finalized.
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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
with SAL.Gen_Unbounded_Definite_Stacks;
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
   package Node_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Valid_Node_Access);

   type Stream_ID is private;
   Invalid_Stream_ID : constant Stream_ID;

   type Stream_Index is private;
   Invalid_Stream_Index : constant Stream_Index;

   type Node_Index is new Integer;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;
   Invalid_Node_Index : constant Node_Index := 0;

   type Base_Tree is new Ada.Finalization.Limited_Controlled with record
      --  Visible components of Tree.

      Lexer : WisiToken.Lexer.Handle;
      --  Declared here because it provides access to the source text; any
      --  code that needs access to Tree mostly likely also needs access to
      --  the source text.

      Leading_Non_Grammar : aliased Base_Token_Arrays.Vector;
      --  Non-grammar tokens before first grammar token; leading blank lines
      --  and comments.
   end record;

   type Tree is new Base_Tree with private;
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
     (Source_Terminal,    -- text is user input, accessed via Lexer
      Virtual_Terminal,   -- no text; inserted during error recovery
      Virtual_Identifier, -- text in user data, created during tree rewrite
      Nonterm             -- contains terminals/nonterminals/identifiers
     );
   subtype Terminal_Label is Node_Label range Source_Terminal .. Virtual_Identifier;
   subtype Virtual_Terminal_Label is Node_Label range Virtual_Terminal .. Virtual_Identifier;

   function Label (Node : in Valid_Node_Access) return Node_Label;

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID;

   type Stream_Node_Ref is record
      Stream  : Stream_ID    := Invalid_Stream_ID;
      Element : Stream_Index := Invalid_Stream_Index;
      Node    : Node_Access  := Invalid_Node_Access;
      --  If both valid, Element contains Node in Stream. In some cases,
      --  Element is valid but Node is Invalid_Node_Access (for example, if
      --  the ref is the First_Terminal in an empty nonterm). In post-parse
      --  actions, the parse stream is deleted, so Stream is
      --  Invalid_Stream_Index, Element is Invalid_Stream_Index, but Node is
      --  valid.
   end record;

   Invalid_Stream_Node_Ref : constant Stream_Node_Ref;

   function Correct_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean;
   --  True if Ref = Invalid_Stream_Node_Ref or Ref.Node =
   --  Invalid_Node_Access or Stream contains Element, which contains
   --  Node.
   --
   --  We allow Ref.Node = Invalid_Node_Access so a Stream_Node_Ref can
   --  be First_Shared_Terminal of an empty nonterm, while still allowing
   --  Next_Shared_Terminal (ref).
   --
   --  Note that this is False in post-parse actions.

   function Valid_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean;
   --  True if Ref refers to a node (possibly an empty nonterm).

   subtype Terminal_Ref is Stream_Node_Ref
   with Dynamic_Predicate =>
     Terminal_Ref.Node = Invalid_Node_Access or else
     Label (Terminal_Ref.Node) in Terminal_Label;

   function Valid_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Boolean
   is (Valid_Stream_Node (Tree, Ref));
   --  True if Ref refers to a Terminal node.

   function Single_Terminal (Ref : in Stream_Node_Ref) return Boolean;
   --  True if Ref contains a single terminal node.

   function Valid_Single_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is (Valid_Stream_Node (Tree, Ref) and then Single_Terminal (Ref));

   subtype Single_Terminal_Ref is Stream_Node_Ref
   with Dynamic_Predicate =>
     Single_Terminal_Ref.Node = Invalid_Node_Access or else
     Single_Terminal (Single_Terminal_Ref);

   function Rooted (Ref : in Stream_Node_Ref) return Boolean;
   --  True if Ref.Element.Node = Ref.Node.

   subtype Rooted_Ref is Stream_Node_Ref
   with Dynamic_Predicate =>
     (Rooted_Ref = Invalid_Stream_Node_Ref --  allows initialization
        or else Rooted (Rooted_Ref));

   type Recover_Token (Virtual : Boolean := True) is record
      --  Virtual is True if there is no node in the syntax tree that is
      --  this token; it was created by error recover.

      --  Declared here because it needs Node_Access

      --  Maintaining a syntax tree during error recovery is too slow, so we
      --  store enough information in the recover stack to perform
      --  In_Parse_Actions, Language_Fixes, Push_Back and Undo_Reduce
      --  operations. and to apply the solution to the main parser state. We
      --  make thousands of copies of the parse stack during recover, so
      --  minimizing size and compute time for this is critical.

      case Virtual is
      when True =>
         ID : Token_ID := Invalid_Token_ID;

         Byte_Region : Buffer_Region := Null_Buffer_Region;
         --  Byte_Region is used to detect empty tokens, for cost and other
         --  issues. Set from real children of virtual nonterms.

         First_Terminal : Node_Access := Invalid_Node_Access;
         --  For ID in Nonterminals, first terminal of this token in the Tree
         --  shared or parse stream, cached from children. For terminals,
         --  Invalid_Node_Access.

         Name : Buffer_Region := Null_Buffer_Region;
         --  Set and used by In_Parse_Actions.

         Contains_Virtual_Terminal : Boolean := False;
         --  True if any contained terminal is Virtual.

      when False =>
         Element_Node : Node_Access := Invalid_Node_Access;
         Node         : Node_Access := Invalid_Node_Access;
         --  This token in the Tree shared or parse stream.
         --
         --  This implements a variant of Stream_Node_Ref for recover.
         --
         --  For terminals, Element_Node = First_Terminal.
         --
         --  For nonterminals, First_Terminal = First_Terminal (Element_Node).
         --  Element_Node may not be Subtree_Root (First_Terminal), if it is
         --  from Undo_Reduce
         --
         --  This token can be virtual, if from Shared_Stream of an edited
         --  tree.
         --
         --  In a non-default Recover_Token, Element_Node cannot be
         --  Invalid_Node_Access.
      end case;
   end record;

   subtype Virtual_Recover_Token is Recover_Token (Virtual => True);
   subtype Real_Recover_Token is Recover_Token (Virtual => False);

   Invalid_Recover_Token : constant Recover_Token := (Virtual => True, ID => Invalid_Token_ID, others => <>);

   function ID (Item : in Recover_Token) return Token_ID;

   function Byte_Region (Item : in Recover_Token) return Buffer_Region;

   function Name (Item : in Recover_Token) return Buffer_Region;
   --  If Node.Name = Null_Buffer_Region, return Node.Byte_Region; else return Node.Name.

   procedure Set_Name (Item : in out Recover_Token; Name : in Buffer_Region);

   function Contains_Virtual_Terminal (Item : in Recover_Token) return Boolean;

   function Is_Empty_Nonterm (Item : in Recover_Token; Descriptor : in WisiToken.Descriptor) return Boolean;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access;

   function To_Real_Recover_Token (Item : in Stream_Node_Ref) return Real_Recover_Token
   with Pre => Item.Element /= Invalid_Stream_Index;

   function Image
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return String;

   type Recover_Token_Array is array (Positive_Index_Type range <>) of Recover_Token;

   ----------
   --  User_Data_Type

   type Base_Augmented is tagged null record;

   function Image_Augmented (Aug : in Base_Augmented) return String
   is ("");

   type Augmented_Class_Access is access all Base_Augmented'Class;
   type Augmented_Class_Access_Constant is access constant Base_Augmented'Class;

   procedure Shift
     (Augmented   : in out Base_Augmented;
      Shift_Bytes : in     Base_Buffer_Pos;
      Shift_Chars : in     Base_Buffer_Pos;
      Shift_Line  : in     Base_Line_Number_Type)
   is null;
   --  Add Shift_* to Augmented positions.

   procedure Free is new Ada.Unchecked_Deallocation (Base_Augmented'Class, Augmented_Class_Access);

   type User_Data_Type is tagged limited null record;
   --  Many test languages don't need this, so we default the procedures
   --  to null.

   type User_Data_Access is access all User_Data_Type'Class;

   function New_User_Data (Template : in User_Data_Type) return User_Data_Access
   is (null);
   --  Return a new empty object with the same type as Template.

   procedure Reset (User_Data : in out User_Data_Type) is null;
   --  Reset to start a new parse.

   procedure Initialize_Actions
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class)
   is null;
   --  Called by Execute_Actions, before processing the tree, after
   --  Insert_Token/Delete_Token.

   procedure Lexer_To_Augmented
     (User_Data     : in out User_Data_Type;
      Tree          : in out Syntax_Trees.Tree'Class;
      Token         : in     Base_Token;
      Grammar_Token : in     Node_Access)
   is null;
   --  Token is a grammar or non-grammar token that was just returned by
   --  User_Data.Lexer. If grammar, it is Grammar_Token; if
   --  non-grammar, it has already been added to Grammar_Token, or
   --  to Parser.Leading_Non_Grammar if Grammar_Token is
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
      Comment_Present      : in     Boolean;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean;
   --  Return True if Insert_Token should be treated as if inserted after
   --  the previous shared terminal, rather than before
   --  Insert_Before_Token. This can affect which line it appears on,
   --  which affects indentation. Called from Insert_Token.
   --
   --  If Comment_Present, there is a comment between Tree.Prev_Terminal
   --  (Insert_Before_Token) and Insert_Before_Token.
   --
   --  If Insert_On_Blank_Line, there is at least one blank line
   --  immediately after Tree.Prev_Terminal (Insert_Before_Token) (before
   --  any comment).
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
      Tree          : in     Syntax_Trees.Tree'Class;
      Deleted_Token : in     Valid_Node_Access;
      Prev_Token    : in     Node_Access)
   is null
   with Pre'Class =>
     Tree.Label (Deleted_Token) in Terminal_Label and
     (Prev_Token = Invalid_Node_Access or else Tree.Label (Prev_Token) in Terminal_Label);
   --  Deleted_Token was deleted in error recovery. Prev_Token is the
   --  previous terminal token in the parse stream. Update remaining
   --  tokens as needed; Deleted_Token.Non_Grammar has _not_ been moved
   --  anywhere. Called from Execute_Actions for each deleted token,
   --  before Initialize_Actions.

   procedure Delete_Token
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class;
      To_Delete : in     Syntax_Trees.Terminal_Ref)
   is null;
   --  To_Deleted is about to be deleted in Edit_Tree. Update remaining
   --  tokens as needed.

   procedure Reduce
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class;
      Nonterm   : in     Valid_Node_Access)
   is null;
   --  Called by Parser.Execute_Actions, just before processing Nonterm;
   --  Nonterm was created by a 'reduce' parse action.

   type Post_Parse_Action is access procedure
     (User_Data : in out User_Data_Type'Class;
      Tree      : in out Syntax_Trees.Tree;
      Nonterm   : in     Valid_Node_Access;
      Tokens    : in     Valid_Node_Access_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Post_Parse_Action := null;

   ----------
   --
   --  Parsing operations (including error recovery and incremental
   --  parse), Tree and Node attributes.

   function New_Stream
     (Tree       : in out Syntax_Trees.Tree;
      Old_Stream : in     Stream_ID;
      User_Data  : in     User_Data_Access)
     return Stream_ID
   with
     Pre => not Tree.Parents_Set and
            ((Tree.Stream_Count = 1 and Old_Stream = Invalid_Stream_ID) or else
               (Tree.Stream_Count > 1 and (Tree.Is_Valid (Old_Stream)))),
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
   --  Delete the stream

   function Is_Valid (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Boolean;
   --  Stream is available for parsing operations.

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean;
   --  True if Stream and Token are valid, and Token is an element of Stream.

   procedure Set_Terminal_Index
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Valid_Node_Access;
      Index    : in     Valid_Node_Index)
   with Pre => Tree.Label (Terminal) in Terminal_Label;
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
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream) and Tree.ID (Stream, Token) =
               Tree.Lexer.Descriptor.EOI_ID;
   --  If Token is from Shared_Stream, copy it from Shared_Stream to
   --  Parse_Stream, after Stack_Top, to mark end of parse stream.
   --  Otherwise Token is from the parse stream; do nothing.

   procedure Start_Edit (Tree : in out Syntax_Trees.Tree)
   with Pre => Tree.Fully_Parsed or Tree.Editable,
     Post => Tree.Parseable;
   --  If Fully_Parsed: delete Tree.Shared_Stream, relabel the parse
   --  stream as Shared_Stream, delete first element.
   --
   --  If Editable, construct Tree.Shared_Stream from Tree.Root.
   --
   --  On return, Tree is ready for Parse.Edit_Tree.
   --
   --  Calls Set_Parents on relabeled Shared_Stream, but leaves
   --  Tree.Parents_Set false.

   procedure Update_Cache (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID);
   --  In all nodes in Stream, update cached token positions from terminals.

   procedure Update_Ancestor_Cache (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access);
   --  In all ancestors of Node, update cached token positions from terminals.

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there are no parse streams and
   --  Shared_Stream holds a lexed or edited stream.

   function Reduce
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Child_Count     : in     Ada.Containers.Count_Type;
      Action          : in     Post_Parse_Action := null;
      State           : in     State_Index;
      Default_Virtual : in     Boolean         := False)
     return Rooted_Ref
   with Pre => not Tree.Traversing and not Tree.Parents_Set and Tree.Is_Valid (Stream),
     Post => Reduce'Result.Stream = Stream and Tree.Valid_Stream_Node (Reduce'Result);
   --  Reduce Child_Count tokens on Stream top of stack to a new Nonterm node on
   --  Stream top of stack. Result points to the new Nonterm node. If Child_Count = 0,
   --  set Nonterm.Virtual := Default_Virtual.
   --
   --  Set Result byte_region, char_region, line, column,
   --  first_terminal to min/max of children.

   procedure Shift
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index;
      Token  : in     Stream_Index)
   with Pre => not Tree.Traversing and
               (Tree.Contains (Tree.Shared_Stream, Token) or else
                  Token = Tree.Stream_Next (Stream, Tree.Stack_Top (Stream)));
   --  If Token is in Shared_Stream, push Token on Stream stack;
   --  otherwise move from Stream input to Stream stack. Then set State
   --  in the Stream element.

   function Pop
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream);
   --  Delete Stream stack top, returning its node.

   procedure Push
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     State_Index)
   with Pre => not Tree.Traversing and Tree.Is_Valid (Stream);
   --  State, Node become Stream stack top.

   procedure Push_Back
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID);
   --  Move Stream.Stack_Top to Stream input.

   procedure Insert_Token
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Ref       : in out Rooted_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref) and Stream /= Ref.Stream and Ref.Stream = Tree.Shared_Stream,
     Post => Valid_Stream_Node (Tree, Ref) and Ref.Stream = Stream;
   --  Insert a stream element containing Ref nodes to beginning of
   --  Stream input. Ref is updated to point to the new element.

   procedure Left_Breakdown
     (Tree : in out Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Label (Ref.Element) = Nonterm and
               Ref.Node /= Invalid_Node_Access and
               Tree.Stack_Top (Ref.Stream) /= Ref.Element,
     Post => Valid_Single_Terminal (Tree, Ref);
   --  Bring first terminal of Ref.Element to the parse stream.
   --  Ref.Element is updated to the element containing the first terminal.
   --
   --  The stack top is unchanged. Note that Ref.Node is ignored on input.

   procedure Breakdown
     (Tree : in out Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => Ref.Stream = Tree.Shared_Stream and
               Valid_Stream_Node (Tree, Ref) and Tree.Label (Ref.Element) = Nonterm and
               Ref.Node /= Invalid_Node_Access and
               Tree.Stack_Top (Ref.Stream) /= Ref.Element,
     Post => Ref.Node = Ref'Old.Node and Tree.First_Terminal (Ref).Node = Ref.Node;
   --  Bring descendants of Ref.Element to the parse stream, until
   --  First_Terminal of one of the parse stream elements = Ref.Node.
   --  Ref.Element is updated to the element whose first terminal is
   --  Ref.Node.
   --
   --  The stack top is unchanged.

   function State (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return State_Index
   with Pre => Tree.Is_Valid (Stream);
   --  Return State from Stream.Stack_Top.

   function State
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Unknown_State_Index
   with Pre => Tree.Contains (Stream, Element);
   --  If Element is in input, state may be Unknown_State.

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
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

   function Has_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Boolean
   with Pre => Stream /= Tree.Shared_Stream and Tree.Is_Valid (Stream);
   --  Return True if there is a stream element after Stack_Top.

   function First_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   with Pre => Tree.Has_Input (Stream),
     Post => Correct_Stream_Node (Tree, First_Input'Result);
   --  Return first stream element after Stack_Top.

   function First_Input_Terminal
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Terminal_Ref
   with Pre => Tree.Has_Input (Stream),
     Post => Correct_Stream_Node (Tree, First_Input_Terminal'Result);
   --  Return first terminal in first stream element after
   --  Stack_Top.

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Element = Invalid_Stream_Index or else
               (Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element));
   --  If Element is Invalid_Stream_Index, result is Stream_First

   function Stream_Next
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Stream_Next'Result);
   --  Return stream element after Ref.Element.

   procedure Stream_Next
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and
             (Ref = Invalid_Stream_Node_Ref or else Tree.Get_Node (Ref.Stream, Ref.Element) = Ref.Node);
   --  Update Ref to root of next stream element after Ref.Element.

   procedure Stream_Next_Terminal_Ref
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);
   --  Update Ref to root of next stream element after Ref.Element;
   --  Ref.Node is First_Terminal (Ref.Element).

   function Stream_Prev
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Tree.Contains (Stream, Element);

   function Stream_Prev
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Stream_Prev'Result);
   --  Return stream element before Ref.Element.

   procedure Stream_Prev
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and
             (Ref = Invalid_Stream_Node_Ref or else Tree.Get_Node (Ref.Stream, Ref.Element) = Ref.Node);
   --  Update Ref to root of stream element before Ref.Element.

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
     return Single_Terminal_Ref
   with Pre => not Tree.Traversing and Stream = Tree.Shared_Stream,
     Post => Tree.Label (Add_Terminal'Result.Node) = Source_Terminal;
   --  Add a new Source_Terminal element in Stream. Result points to the added
   --  node.

   function Non_Grammar_Var
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Array_Var_Ref
   with Pre => Tree.Label (Terminal) in Terminal_Label;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Array_Const_Ref
   with Pre => Tree.Label (Terminal) in Terminal_Label;

   function Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Base_Token;
      Index    : in     Node_Index;
      Before   : in     Stream_Index)
     return Single_Terminal_Ref
   with Pre => not Tree.Traversing and (Before = Invalid_Stream_Index or else Tree.Contains (Stream, Before)),
     Post => Tree.Label (Insert_Source_Terminal'Result.Node) = Source_Terminal;
   --  Insert a new Source_Terminal element on Stream, before Before.
   --  Index should give the source token order. Result points to the
   --  added element.

   function Insert_Virtual_Terminal
     (Tree                   : in out Syntax_Trees.Tree;
      Stream                 : in     Stream_ID;
      Terminal               : in     Token_ID;
      Before_Shared_Terminal : in     Valid_Node_Access)
     return Single_Terminal_Ref
   with
     Pre  => not Tree.Traversing and Tree.Is_Valid (Stream) and Stream /= Tree.Shared_Stream and
             Tree.Label (Before_Shared_Terminal) in Terminal_Label and Tree.Is_Shared (Before_Shared_Terminal),
     Post => Tree.Label (Insert_Virtual_Terminal'Result.Node) = Virtual_Terminal;
   --  Insert a new Virtual_Terminal element into Stream, after
   --  Stack_Top. Before_Shared_Terminal should be the token Terminal is
   --  inserted before; Terminal Line, Char_Region are set to
   --  Before_Shared_Terminal.Line (for indent),
   --  Before_Shared_Terminal.Char_Region.First (for indent); Byte_Region
   --  is Null_Buffer_Region (for "is empty). result points to the added
   --  node.

   procedure Update
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Byte_Region : in Buffer_Region;
      Char_Region : in Buffer_Region;
      Line_Region : in WisiToken.Line_Region)
   with Pre => Tree.Label (Node) in Virtual_Terminal | Virtual_Identifier;

   procedure Shift
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Shift_Bytes : in Base_Buffer_Pos;
      Shift_Chars : in Base_Buffer_Pos;
      Shift_Line  : in Base_Line_Number_Type)
   with Pre => Tree.Label (Node) in Terminal_Label;
   --  Add Shift_* to token positions.

   procedure Stream_Delete
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in out Stream_Index)
   with
     Pre  => Tree.Contains (Stream, Element),
     Post => Element = Invalid_Stream_Index;
   --  Delete Element from Stream. If Element = Stream.Stack_Top,
   --  Stack_Top is set to Invalid_Stream_Index.

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);
   --  The precondition allows either stream; Parser.Current_Token is
   --  either a Source_Terminal from Shared_Stream or a Virtual_Terminal
   --  in Stream input from error recovery; in incremental parse, it
   --  could be a Source_Terminal in Stream input from push_back.

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label;
   function Label (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Node_Label
   with Pre => Element /= Invalid_Stream_Index;

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type;

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
   --  virtual tokens. Use First_Terminal = Invalid_Node_Access to see if
   --  Node is an empty nonterm.

   function Is_Shared (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Source_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;

   function Is_Virtual
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Boolean
   with Pre => Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element);
   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  Virtual_Terminal, Virtual_Identifier, or Nonterm that contains some Virtual terminals.

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

   function ID
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return WisiToken.Token_ID;
   --  One of Ref.Node.ID, Ref.Element.Node.ID, Invalid_Token_ID

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Production_ID
   with Pre => Tree.Is_Nonterm (Node);

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   with Pre => Index /= Invalid_Stream_Index;

   function Byte_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region;

   function Name (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Buffer_Region;
   function Name (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Buffer_Region
   with Pre => Valid_Stream_Node (Tree, Ref);
   --  If Ref.Element.Name = Null_Buffer_Region, return
   --  Ref.Element.Byte_Region; else return Ref.Element.Name.

   function Char_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region;

   function Line_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Line_Region;
   --  Lines in Node; Invalid_Line_Region if Node is virtual or empty.

   procedure Set_Line_Last
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      Line : in Line_Number_Type);
   --  Set Node.Line_Region.Last

   function Column (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Ada.Text_IO.Count
   with Pre => Tree.Editable and Tree.Subtree_Root (Node) = Tree.Root;
   --  Column of first char of Node; offset from first character on line,
   --  origin 0 (WisiToken and Emacs standard). If Node is empty or
   --  Virtual, result is 0.

   function Column
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Stream : in Stream_ID)
     return Ada.Text_IO.Count;
   --  Same as Column, but Node must be in Stream or Shared_Stream.

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Recover_Token;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token;
   --  Treat Node as a stream element.

   function Children_Recover_Tokens
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Recover_Token_Array
   with Pre => Tree.Contains (Stream, Element) and Tree.Label (Element) = Nonterm;

   procedure Set_Augmented
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Value : in Augmented_Class_Access);
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
     return Post_Parse_Action
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
   with Pre => Tree.Parents_Set and Tree.Has_Parent (Node);
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
   with Pre => Tree.Parents_Set;

   function Subtree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access
   with Pre => Tree.Parents_Set;
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
   --  under Node. If result is Invalid_Node_Access, all terminals are
   --  non-shared, or Node is an empty nonterm.

   function First_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Same as First_Shared_Terminal (Tree, Node), also initializes
   --  Parents to store path from Node to the first terminal, for
   --  Next_Shared_Terminal in nodes that have unset parent links.

   function First_Shared_Terminal
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Terminal_Ref
   with Pre => Tree.Contains (Stream, Element),
     Post => First_Shared_Terminal'Result = Invalid_Stream_Node_Ref or else
             (First_Shared_Terminal'Result.Stream = Stream and Valid_Terminal (Tree, First_Shared_Terminal'Result));
   --  Returns first shared terminal in Element or a following stream
   --  element, or Invalid_Stream_Node_Ref if there is no such shared terminal.

   function First_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in     Stream_Index;
      Parents : in out Node_Stacks.Stack)
     return Terminal_Ref
   with Pre => not Tree.Parents_Set and Tree.Contains (Stream, Element),
     Post => First_Shared_Terminal'Result = Invalid_Stream_Node_Ref or else
             (First_Shared_Terminal'Result.Stream = Stream and Valid_Terminal (Tree, First_Shared_Terminal'Result));
   --  Same as First_Shared_Terminal (Tree, Stream, Element), but also
   --  initializes Parents for Elements with unset parent links.

   function First_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => First_Shared_Terminal'Result = Invalid_Stream_Node_Ref or else
             (First_Shared_Terminal'Result.Stream = Ref.Stream and
                Valid_Terminal (Tree, First_Shared_Terminal'Result));
   --  Returns first shared terminal in Ref.Element or a following stream
   --  element. Can be Invalid_Stream_Node_Ref if Ref.Stream is the last
   --  element in a parse stream and Ref.Element contains no shared
   --  terminals.

   function First_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in     Rooted_Ref;
      Parents : in out Node_Stacks.Stack)
     return Terminal_Ref
   with Pre => not Tree.Parents_Set and Valid_Stream_Node (Tree, Ref),
     Post => First_Shared_Terminal'Result = Invalid_Stream_Node_Ref or else
             (First_Shared_Terminal'Result.Stream = Ref.Stream and
                Valid_Terminal (Tree, First_Shared_Terminal'Result));
   --  Same as First_Shared_Terminal (Tree, Ref), but also initializes
   --  Parents for Refs with unset parent links.

   function Last_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access;
   --  Returns last shared terminal in subtree under Element. If result
   --  is Invalid_Node_Access, all terminals are non-shared, or Node is
   --  an empty nonterm.

   function Last_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Same as Last_Shared_Terminal (Tree, Node), also initializes
   --  Parents to store path from Node to the first terminal, for
   --  Prev_Shared_Terminal in nodes that have unset parent links.

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   with Post => (for all Node of Get_Terminals'Result => Tree.Label (Node) in Terminal_Label);
   --  Return sequence of terminals in Node.

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  First of Get_Terminals. Invalid_Node_Access if Node is an empty nonterminal.

   function First_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   with Pre => not Tree.Parents_Set;
   --  Same as First_Terminal (Tree, Node), also initializes Parents to
   --  store path from Node to the first terminal, for Next_Terminal in
   --  nodes that have unset parent links.

   function First_Terminal
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Terminal_Ref
   with Pre => Tree.Contains (Stream, Element),
     Post => First_Terminal'Result.Stream = Stream and Valid_Terminal (Tree, First_Terminal'Result);
   --  Return the first terminal in Element or a following Element if
   --  Element is empty.

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   with Pre => Valid_Stream_Node (Tree, Ref);
   --  If Ref.Node is a terminal, return Ref. Else if Ref.Node is a
   --  nonterm, return First_Terminal (Ref.Node). Else Ref.Node is
   --  invalid; return First_Terminal (Ref.Element.Node).
   --
   --  Note that the result can be Invalid_Stream_Node if Ref is an empty
   --  nonterm.

   function First_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in     Stream_Index;
      Parents : in out Node_Stacks.Stack)
     return Terminal_Ref
   with Pre => not Tree.Parents_Set and Tree.Contains (Stream, Element),
     Post => First_Terminal'Result.Stream = Stream and Valid_Terminal (Tree, First_Terminal'Result);
   --  Same as First_Terminal (Tree, Stream, Element), but also
   --  initializes Parents for Element with unset parent links.

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  Last of Get_Terminals. Invalid_Node_Access if Node is an empty nonterminal.

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Prev_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately before Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the first terminal
   --  in that subtree.
   --
   --  Subtree under Node must have parents set; either Tree.Parents_Set
   --  or Node is in Shared_Stream.

   function Prev_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Node    : in     Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Prev_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Terminal'Result) in Terminal_Label;
   --  Same as Prev_Terminal (Tree, Node), for nodes with unset parent
   --  links. Parents is initialized by Last_Terminal.

   function Prev_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Prev_Terminal'Result);

   procedure Prev_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Next_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately after Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the last terminal
   --  in that subtree.
   --
   --  The subtree at Node must have parents set; either Tree.Parents_Set
   --  or Node is in Shared_Stream.

   function Next_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Node    : in     Valid_Node_Access)
     return Node_Access
   with Pre => not Tree.Parents_Set and Tree.Label (Node) in Terminal_Label,
     Post => Next_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Terminal'Result) in Terminal_Label;
   --  Same as Next_Terminal (Tree, Node), for nodes with unset parent
   --  links. Parents is initialized by First_Terminal.

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);
   --  Update Ref to the next terminal that is after Ref.Node in Stream;
   --  Invalid_Stream_Node_Ref if Ref.Node is EOI.

   procedure Next_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Ref     : in out Terminal_Ref)
   with Pre => Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);
   --  Same as Next_Terminal (Tree, Ref), for Refs with unset parent
   --  links. Parents is initialized by First_Terminal.

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Next_Terminal'Result);
   --  Same as procedure Next_Terminal, but return result.

   function Next_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Next_Shared_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Shared_Terminal'Result) in Terminal_Label;
   --  Return the next shared terminal after Node in subtree containing
   --  Node; Invalid_Node_Access if Node is last shared terminal in
   --  subtree.
   --
   --  Subtree under Node must have parents set; either Tree.Parents_Set
   --  or Node is in Shared_Stream.

   function Next_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Node    : in     Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Next_Shared_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Shared_Terminal'Result) in Terminal_Label;
   --  Same as Next_Shared_Terminal, for nodes with unset parents.
   --  Parents is initialized by First_Shared_Terminal.

   procedure Next_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and
             (Ref.Node = Invalid_Node_Access or else Tree.Label (Ref.Node) in Terminal_Label);
   --  Update Ref to the next shared terminal that is after Ref.Node in
   --  Stream; Invalid_Stream_Node_Ref if Ref is EOI.

   function Next_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Next_Shared_Terminal'Result);
   --  Same as procedure Next_Shared_Terminal, but returns the updated ref.

   procedure Next_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Ref     : in out Stream_Node_Ref)
   with Pre => not (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and
             (Ref.Node = Invalid_Node_Access or else Tree.Label (Ref.Node) in Terminal_Label);
   --  Same as Next_Shared_Terminal, for Ref with unset Parent links.

   function Next_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Ref     : in     Stream_Node_Ref)
     return Terminal_Ref
   with Pre => not (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Next_Shared_Terminal'Result);
   --  Same as function Next_Shared_Terminal, but for Ref with unset Parent links.
   --  Parents is initialized by First_Shared_Terminal.

   function Prev_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Prev_Shared_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Shared_Terminal'Result) in Terminal_Label;
   --  Return the previous shared terminal before Node in subtree containing
   --  Node; Invalid_Node_Access if Node is first shared terminal in
   --  subtree.
   --
   --  Subtree under Node must have parents set; either Tree.Parents_Set
   --  or Node is in Shared_Stream.

   function Prev_Shared_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Parents : in out Node_Stacks.Stack;
      Node    : in     Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Label (Node) in Terminal_Label,
     Post => Prev_Shared_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Shared_Terminal'Result) in Terminal_Label;
   --  Same as Prev_Shared_Terminal, for nodes with unset parents.
   --  Parents is initialized by Last_Shared_Terminal.

   procedure Prev_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);
   --  Update Ref to the shared terminal that is before Node in Stream;
   --  Invalid_Stream_Node_Ref if Ref is first shared terminal in Stream.

   function Prev_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   with Pre => (Tree.Parents_Set or Ref.Stream = Tree.Shared_Stream) and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Prev_Shared_Terminal'Result);
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
   --  True if Clear_Parse_Streams and Set_Parents have been called; the
   --  remaining tree may be arbitrarily edited.

   function Copy_Subtree
     (Tree      : in out Syntax_Trees.Tree;
      Root      : in     Node_Access;
      User_Data : in     User_Data_Access)
     return Node_Access
   with Pre => Editable (Tree);
   --  Deep copy (into Tree) subtree of Tree rooted at Root. Return root
   --  of new subtree; it has no parent.
   --
   --  If Root is Invalid_Node_Access, returns Invalid_Node_Access.

   procedure Copy_Tree
     (Source      : in     Tree;
      Destination :    out Tree;
      User_Data   : in     User_Data_Access)
   with Pre => Editable (Source);
   --  The subtree at Tree.Root is copied. Destination parents are set.
   --  All references are deep copied; Source may be finalized after this
   --  operation.

   procedure Clear_Parse_Streams
     (Tree       : in out Syntax_Trees.Tree;
      Keep_Nodes : in     Valid_Node_Access_Lists.List := Valid_Node_Access_Lists.Empty_List)
   with Post => Tree.Editable;
   --  If Tree.Root is not set, set it to the root of the single
   --  remaining parse stream. Delete the parse stream and shared stream.
   --  Delete all nodes not reachable from the root, not Tree.EOI, and
   --  not in Keep_Nodes. Also call Set_Parents if not Tree.Parents_Set.
   --
   --  Keep_Nodes should be set to nodes that occur in errors, or are
   --  deleted by error recovery; they may be referenced by post-parse
   --  actions.
   --
   --  No precondition for Packrat parser.

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Set_Parents
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID := Invalid_Stream_ID);
   --  If Stream is not Invalid_Stream_ID, set parents in all elements of
   --  Stream. Otherwise, if Tree.Root is set, sets parents in tree
   --  rooted at Tree.Root. Otherwise sets parents in all elements of
   --  Tree.Shared_Stream.
   --
   --  No precondition for packrat.

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access;
   --  Tree.Root, or the root in the single stream if Tree.Root is not
   --  set. No precondition for packrat.

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access);

   function EOI (Tree : in Syntax_Trees.Tree) return Node_Access;
   --  Return node representing end of input; Invalid_Node_Access if it
   --  has not yet been seen by the lexer.

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return Count parent of Node.

   function Find_New_Line
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   with Pre => Tree.Editable;
   --  Return node that that ends Line - 1; either EOI or contains the
   --  non-grammar New_Line. Invalid_Node_Access if Line is in
   --  Tree.Leading_Non_Grammar, or outside text spanned by Tree.

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Base_Buffer_Pos
     with Pre => Tree.Editable;
   --  First character on Line in text spanned by tree under Tree.Root;
   --  it may be in no token, or in a grammar or non-grammar token.
   --  Result is Invalid_Buffer_Pos if Line is not in the text spanned by
   --  Tree, or if Line is inside a multi-line token.

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type;
      Stream : in Stream_ID)
     return Base_Buffer_Pos;
   --  Same as other Line_Begin_Token, but searches in Stream instead of
   --  Tree.Root. If not found there, continues searching input in
   --  Shared_Stream.

   function Line_Begin_Token
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access;
   --  Return the node under Root (Tree) of the first terminal token on
   --  line Line; Invalid_Node_Access if there are no grammar tokens on
   --  the line (ie only comment or whitespace).

   function Line_Begin_Token
     (Tree   : in Syntax_Trees.Tree;
      Line   : in Line_Number_Type;
      Stream : in Stream_ID)
     return Node_Access;
   --  Same as other Line_Begin_Token, but searches in Stream instead of
   --  Tree.Root. If not found there, continues searching input in
   --  Shared_Stream.

   function Find_Terminal
     (Tree       : in Syntax_Trees.Tree;
      Char_Point : in Buffer_Pos)
     return Node_Access;
   --  Return the terminal whose char_region contains Char_Point,
   --  Invalid_Node_Access if none.

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Clear_Parents   : in     Boolean;
      Action          : in     Post_Parse_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Children'First = 1;
   --  Add a new Nonterm node (not on any stream), containing
   --  Children, with no parent. Result points to the added node. If
   --  Children'Length = 0, set Nonterm.Virtual := Default_Virtual.
   --
   --  If Parents_Set, Children.Parent are set to the new node. If a
   --  child has a previous parent, then if Clear_Parents, the
   --  corresponding entry in the parent's Children is set to null; if
   --  not Clear_Parents and assertions are enabled, Assertion_Error is
   --  raised.

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

   procedure Clear_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
   with
     Pre => not Tree.Traversing and Tree.Editable and Tree.Parent (Node) /= Invalid_Node_Access;
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

   type Image_Action is access function (Action : in Post_Parse_Action) return String;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Children     : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Root         : in Node_Access               := Invalid_Node_Access;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String;
   --  Image of all streams, or root node if no streams.
   --  If Children, subtree of each stream element is included.

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Stream       : in Stream_ID;
      Stack        : in Boolean                   := True;
      Input        : in Boolean                   := True;
      Children     : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String;
   --  Image of each node. If Stack, includes stack; if Input, includes
   --  input. If Children, each entire subtree is included, with
   --  newlines, as in Print_Tree.

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      Children              : in Boolean                   := False;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String;
   --  Element can be from any stream, or Invalid_Stream_Index

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Node                  : in Node_Access;
      Children              : in Boolean                   := False;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String;
   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Nodes                 : in Node_Access_Array;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String;
   --  Includes Node.Node_Index, Node.ID

   function Image
     (Tree           : in Syntax_Trees.Tree;
      Ref            : in Stream_Node_Ref;
      First_Terminal : in Boolean                   := False;
      Augmented      : in Boolean                   := False;
      Image_Action   : in Syntax_Trees.Image_Action := null)
     return String;
   --  If First_Terminal, show First_Terminal of Ref.Node if Ref is rooted.

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
   with Pre => Element = Invalid_Stream_Index or else Tree.Contains (Stream, Element);
   --  Version without Tree requires Syntax_Trees.Get_Node_Index. Returns
   --  Invalid_Node_Index for Invalid_Node_Access.

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result;

   package Node_Sets is new SAL.Gen_Unbounded_Sparse_Ordered_Sets (Node_Access, Node_Access_Compare);

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Access;
      Message : in String)
     return String;
   --  File_Name from Tree.Lexer

   type Validate_Node is access procedure
     (Tree                : in     Syntax_Trees.Tree;
      Node                : in     Valid_Node_Access;
      Data                : in out User_Data_Type'Class;
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
     (Tree           : in out Syntax_Trees.Tree;
      User_Data      : in out User_Data_Type'Class;
      Error_Reported : in out Node_Sets.Set;
      Root           : in     Node_Access                := Invalid_Node_Access;
      Validate_Node  : in     Syntax_Trees.Validate_Node := null);
   --  Verify that no children are Invalid_Node_Access. Verify
   --  child/parent links. Call Validate_Node for each visited node.
   --  Violations output a message to Text_IO.Current_Error.
   --  Error_Reported is used to avoid outputing an error for a node more
   --  than once.

   procedure Print_Tree
     (Tree         : in Syntax_Trees.Tree;
      Root         : in Node_Access               := Invalid_Node_Access;
      Image_Action : in Syntax_Trees.Image_Action := null;
      Non_Grammar  : in Boolean                   := False);
   --  Print tree rooted at Root (default Tree.Root) to
   --  Text_IO.Current_Output, for debugging.

   procedure Print_Streams (Tree : in Syntax_Trees.Tree; Non_Grammar : in Boolean := False);

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String;
   --  For debugging; node counts.

private
   use all type Ada.Containers.Count_Type;

   type Node
     (Label       : Node_Label;
      Child_Count : SAL.Base_Peek_Type)
   --  Descriminants have no default because allocated nodes are
   --  constrained anyway (ARM 4.8 6/3).
   is record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Node_Index : Syntax_Trees.Node_Index := 0;
      --  If Terminal_Label, and in or copied from Shared_Stream,
      --  corresponds to text order. Otherwise, negative, unique and
      --  arbitrary, for debugging.

      Byte_Region : aliased Buffer_Region := Null_Buffer_Region;
      Char_Region : aliased Buffer_Region := Null_Buffer_Region;
      --  For terminals, from lexer. For nonterms, computed by Update_Cache,
      --  used by Line_Begin_Char_Pos, In_Parse_Actions and debug messages.
      --  Virtual_Terminals, and nonterms containing only Virtual_Terminals
      --  have null Byte_Region but non-null Char_Region; Char_Region is
      --  taken from a nearby Source_Terminal or non_grammar.

      Line_Region : WisiToken.Line_Region := Null_Line_Region;
      --  Lines in token including trailing non_grammar.

      Parent : Node_Access := Invalid_Node_Access;

      Augmented : Augmented_Class_Access := null;
      --  IMPROVEME: Augmented should not derive from Base_Token; that
      --  duplicates information. Not changing yet for compatibility with
      --  main devel branch.

      case Label is
      when Source_Terminal =>
         Non_Grammar : aliased Base_Token_Arrays.Vector;
         --  Immediately following Node. In initial lex, this can only be in a
         --  Source_Terminal node. User Insert_Terminal can move it to a
         --  Virtual_Terminal node, editing the tree can copy it to a
         --  Virtual_Identifier node.

      when Virtual_Terminal =>
         VT_Non_Grammar : aliased Base_Token_Arrays.Vector;
         --  Immediately following Node.

      when Virtual_Identifier =>
         Identifier : Identifier_Index; -- into user data

         VI_Non_Grammar : aliased Base_Token_Arrays.Vector;
         --  Immediately following Node.

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by In_Parse_Actions and error recover.

         RHS_Index : Natural;
         --  With ID, index into Productions.

         Action : Post_Parse_Action := null;

         Name_Offset : Base_Buffer_Pos := 0;
         Name_Length : Base_Buffer_Pos := 0;
         --  Name_* are set and checked by In_Parse_Actions. We use an offset
         --  from Byte_Region.First and length, rather than a Buffer_Region, to
         --  avoid needing to shift it during Edit_Tree for incremental parse.
         --  In_Parse_Actions are not called on a nonterm when it is shifted,
         --  so Name is not recomputed. Name_Offset will only change when the
         --  text in the region Byte_Region.First .. Byte_Region.First +
         --  Name_Offset is edited, in which case the nonterm will be broken
         --  down, and the In_Parse_Actions rerun.

         Children : Node_Access_Array (1 .. Child_Count);
         --  We use an explicit array, rather than a pointer to the first
         --  child, to preserve child indices while editing the tree.

         First_Terminal : Node_Access := Invalid_Node_Access;
         --  Cached for push_back of nonterminals during error recovery, and
         --  fast access in incremental parse.
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

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that is on the parse stack with this token.
      --  Unknown_State in Shared_Stream or a parse stream input.

      Label : Stream_Label; -- allows checking if Element is from Shared_Stream or a parse stream.
   end record;

   package Stream_Element_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Stream_Element);
   use all type Stream_Element_Lists.Cursor;

   type Stream_Index is record
      Cur : Stream_Element_Lists.Cursor;
   end record;

   Invalid_Stream_Index : constant Stream_Index := (Cur => Stream_Element_Lists.No_Element);

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

   Invalid_Stream_Node_Ref : constant Stream_Node_Ref :=
     (Invalid_Stream_ID, Invalid_Stream_Index, Invalid_Node_Access);

   package Node_Access_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node_Access, null);

   type Tree is new Base_Tree with record
      Next_Stream_Label : Stream_Label := Shared_Stream_Label + 1;

      Next_Terminal_Node_Index : Node_Index := 1;

      Root : Node_Access := Invalid_Node_Access;
      EOI  : Node_Access := Invalid_Node_Access; --  Not in Nodes

      Streams : Parse_Stream_Lists.List;

      Shared_Stream : Stream_ID;

      Nodes : Node_Access_Arrays.Vector;
      --  Stores ref to all nodes, for Finalize.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

      Parents_Set : Boolean := False;
      --  We don't set Node.Parent until after parse is done; see Design
      --  note above.
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   function Base_Token
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return WisiToken.Base_Token
   is (Base_Token (Tree, Tree.Streams (Stream.Cur).Elements (Element.Cur).Node));

   function Base_Token (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Base_Token
   is (Node.ID, Node.Byte_Region, Node.Char_Region, Node.Line_Region);

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   is (Stream_Element_Lists.Constant_Ref (Index.Cur).Node.Byte_Region);

   function Byte_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region
   is (Node.Byte_Region);

   function Char_Region (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Buffer_Region
   is (Node.Char_Region);

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   is (Node.Child_Count);

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean
   is ((Tree.Is_Valid (Stream) and Token /= Invalid_Stream_Index) and then
         Stream_Element_Lists.Constant_Ref (Token.Cur).Label = Tree.Streams (Stream.Cur).Label);

   function Correct_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is ((Ref.Stream = Invalid_Stream_ID and Ref.Element = Invalid_Stream_Index and Ref.Node = Invalid_Node_Access)
          or else
          (Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Label = Tree.Streams (Ref.Stream.Cur).Label and
             (Ref.Node = Invalid_Node_Access or else
                (not Tree.Parents_Set or else
                   Tree.Subtree_Root (Ref.Node) = Tree.Get_Node (Ref.Stream, Ref.Element)))));

   function Editable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Parents_Set and Tree.Streams.Length = 0 and Tree.Shared_Stream.Cur = Parse_Stream_Lists.No_Element);

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
   is (if Node = Invalid_Node_Access then Invalid_Node_Index else Node.Node_Index);

   function Get_Node_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Node_Index
   is (if Node = Invalid_Node_Access then Invalid_Node_Index else Node.Node_Index);

   function Get_Node_Index (Element : in Stream_Index) return Node_Index
   is (if Stream_Element_Lists.Has_Element (Element.Cur)
       then Stream_Element_Lists.Constant_Ref (Element.Cur).Node.Node_Index
       else Invalid_Node_Index);

   function Get_Node_Index
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Node_Index
   is (Get_Node_Index (Element));

   function Has_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Boolean
   is (declare Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
       begin Parse_Stream.Stack_Top /= Parse_Stream.Elements.Last);

   function ID (Item : in Recover_Token) return Token_ID
   is
     (if Item.Virtual
      then Item.ID
      elsif Item.Element_Node /= Invalid_Node_Access
      then Item.Element_Node.ID
      else Item.Node.ID);

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node.ID);

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 0 and Tree.Root = Invalid_Node_Access);

   function Is_Empty_Nonterm (Item : in Recover_Token; Descriptor : in WisiToken.Descriptor) return Boolean
   is (case Item.Virtual is
       when True => Is_Nonterminal (Item.ID, Descriptor) and Item.First_Terminal = Invalid_Node_Access,
       when False => Item.Node.Label = Nonterm and Item.Node.Child_Count = 0);

   function Is_Shared (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is (Node.Node_Index > 0);

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

   function Next_Stream_ID_Trimmed_Image (Tree : in Syntax_Trees.Tree) return String
   is (Trimmed_Image (Tree.Next_Stream_Label));

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Parents_Set);

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 1);

   function Rooted (Ref : in Stream_Node_Ref) return Boolean
   is (Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node = Ref.Node);

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
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).State);

   function State (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return State_Index
   is (Stream_Element_Lists.Constant_Ref (Tree.Streams (Stream.Cur).Stack_Top).State);

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

   function Single_Terminal (Ref : in Stream_Node_Ref) return Boolean
   is (declare
          Element : Stream_Element renames Stream_Element_Lists.Constant_Ref (Ref.Element.Cur);
       begin
          Element.Node = Ref.Node and Ref.Node.Label in Terminal_Label);

   function Terminal_Ref_Image (Item : in Syntax_Trees.Terminal_Ref; Aux : in Syntax_Trees.Tree'Class) return String
   is (Aux.Image (Item));

   function To_Real_Recover_Token (Item : in Stream_Node_Ref) return Real_Recover_Token
   is ((Virtual      => False,
        Element_Node => Stream_Element_Lists.Constant_Ref (Item.Element.Cur).Node,
        Node         => Item.Node));

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

   function Valid_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is (Tree.Contains (Ref.Stream, Ref.Element) and
         (Ref.Node = Invalid_Node_Access or else
            (not Tree.Parents_Set or else
               Tree.Subtree_Root (Ref.Node) = Tree.Get_Node (Ref.Stream, Ref.Element))));

   Dummy_Node : constant Node_Access := new Node'(Label => Virtual_Identifier, Child_Count => 0, others => <>);

end WisiToken.Syntax_Trees;
