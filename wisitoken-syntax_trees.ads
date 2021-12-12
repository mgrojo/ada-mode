--  Abstract :
--
--  Syntax tree type and operations, providing parse streams as
--  described in [1], but generalized for parallel parsers. Data
--  structures and operations are optimized for incremental parsing.
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
--  During parsing, the Shared_Stream contains all of the input source
--  text, either as terminal tokens from the lexer in batch parse, or
--  a mix of terminal (possibly virtual) and nonterminal tokens from
--  Parse.Edit_Tree in incremental parse.
--
--  Node_Index is used only for debugging. Node_Index on nonterms is
--  negative. Node_Index on terminal nodes created by the lexer in the
--  shared stream is positive; Nod_Index on virtual nodes inserted by
--  error recover is negative.
--
--  During a batch parse, Node_Index on terminals is sequential, as a
--  consequence of lexing the source code first; Node_Index on
--  nonterms is unique within the tree, and abs Node_Index of a
--  nonterm is greater than abs Node_Index of any of its children.
--  Error recover inserts and deletes terminals, with non-sequential
--  Node_Index.
--
--  In incremental parse, Node_Index on terminals is not sequential,
--  and Node_Index is not unique within the tree.
--
--  Error recover uses Sequential_Index to determine success, and to
--  control where terminals are inserted and deleted. To be
--  incremental, Sequential_Index is only set in a small portion of
--  the shared stream at error recover start, and cleared when error
--  recover completes.
--
--  During and after parsing, the sequence of terminals in the parse
--  stream or syntax tree is given by Next_Terminal/Prev_Terminal.
--
--  Each parallel parser uses one stream as the parse stack and
--  auxiliary input stream. The auxiliary input stream contains tokens
--  that are pushed back in error recovery, or broken down from
--  Shared_Stream in incremental parse.
--
--  Nodes that are deleted from the parse stream during error recover
--  are referenced from the preceding terminal node or SOI, so they
--  may be restored on the next incremental parse if appropriate.
--  Similarly, parse errors are referenced from the error node. In
--  order to avoid editing shared nodes, the nodes that are edited are
--  copied to the parse stream first.
--
--  Each node contains a Parent link, to make it easy to traverse the
--  tree in any direction after parsing is done. We do not set the
--  Parent links while parsing, to avoid having to copy nodes. At the
--  start of incremental parse (during and after Edit_Tree), the
--  shared stream has complete parent links. However, Breakdown clears
--  parent links, so shared nodes may not have complete parent links;
--  error recover must use explicit Parent stack versions of tree
--  routines. All Parent links are set when parse completes; condition
--  Tree.Editable ensures that there is a single fully parsed tree
--  with all parent links set. While editing the syntax tree after
--  parse, any functions that modify children or parent relationships
--  update the corresponding links, setting them to
--  Invalid_Node_Access as appropriate.
--
--  We don't store the parse State in syntax tree nodes, to avoid
--  having to copy nodes during parsing. State is stored in the parse
--  stream elements. This means Parse.LR.Undo_Reduce has to call
--  Action_For to compute the state for the child nodes.
--
--  During parsing, nodes are never deleted, copied or edited in any
--  way once they are created, except for clearing parent links and
--  setting Sequential_Index.
--
--  Type Tree is limited because a bit-copy is not a good start on copy
--  for assign; use Copy_Tree.
--
--  We can't traverse Tree.Streams to deallocate tree Nodes, either
--  when streams are terminated or during Finalize; in general Nodes
--  are referenced multiple times in multiple streams. So we keep
--  track of nodes to deallocate in Tree.Nodes. Nodes are deleted in
--  Clear_Parse_Streams and when the entire tree is Finalized.
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
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Image_Aux;
with SAL.Gen_Indefinite_Doubly_Linked_Lists;
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

   type Stream_ID_Array is array (Positive_Index_Type range <>) of Stream_ID;

   type Stream_Index is private;
   Invalid_Stream_Index : constant Stream_Index;

   type Node_Index is new Integer;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;
   Invalid_Node_Index : constant Node_Index := 0;

   type Base_Sequential_Index is new Integer;
   Invalid_Sequential_Index : constant Base_Sequential_Index := Base_Sequential_Index'Last;
   subtype Sequential_Index is Base_Sequential_Index range Base_Sequential_Index'First .. Invalid_Sequential_Index - 1;
   --  Identifies a sequence of tokens in Shared_Stream during error
   --  recovery. Index 1 is the error token (set in
   --  wisitoken-parse-lr-mckenzie_recover-base.adb Initialize).
   --
   --  We need arbitrarily large negative index for Push_Back and
   --  Undo_Reduce error recover operations, and arbitrarily large
   --  positive index for handling unterminated strings.

   type Base_Tree is new Ada.Finalization.Limited_Controlled with record
      --  Visible components of Tree.

      Lexer : WisiToken.Lexer.Handle;
      --  Declared here because it provides access to the source text; any
      --  code that needs access to Tree mostly likely also needs access to
      --  the source text.
   end record;

   type Tree is new Base_Tree with private;
   --  Use Copy_Tree to get a copy.

   type Tree_Variable_Reference (Element : not null access Tree) is null record with
     Implicit_Dereference => Element;

   type Tree_Constant_Reference (Element : not null access constant Tree) is null record with
     Implicit_Dereference => Element;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   procedure Free_Augmented (Tree : in Syntax_Trees.Tree);
   --  Free Augmented in all nodes.

   procedure Clear
     (Tree        : in out Syntax_Trees.Tree;
      Free_Memory : in     Boolean := False)
   with Post => Tree.Cleared;
   --  Delete all nodes in all streams, reset for new lex and parse.
   --  Free_Memory applies to internal bookkeeping; leaving it False may
   --  slightly speed parsing a similar sized file as the previous one.

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean;

   type Error_Data is abstract tagged null record;
   --  Error_Data must not have Valid_Node_Access components; they would
   --  not be updated properly when the tree is copied. The error node is
   --  the node that contains this data.

   function Dispatch_Equal (Left : in Error_Data; Right : in Error_Data'Class) return Boolean
   is abstract;
   --  True if Left matches Right for purposes of Update_Error and
   --  Delete_Error, below.
   --
   --  Not named "=" because that's always ambiguous with the predefined "=".

   function To_Message
     (Data       : in Error_Data;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Valid_Node_Access)
     return Error_Data'Class
   is abstract;
   --  Convert Data to a simple message; it is being moved to another
   --  node (see wisitoken-parse-lr.adb Undo_Reduce).

   function Image
     (Data       : in Error_Data;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Valid_Node_Access)
     return String
   is abstract;
   --  Should not include file name, line number; a higher level will add
   --  that if desired.

   type Null_Error_Data is new Error_Data with null record;
   --  For Error_Data parameters when there is no error.

   overriding function Dispatch_Equal
     (Left  : in Null_Error_Data;
      Right : in Error_Data'Class)
     return Boolean
   is (Right in Null_Error_Data);
   overriding function To_Message
     (Data       : in Null_Error_Data;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Valid_Node_Access)
     return Error_Data'Class
   is (raise SAL.Programmer_Error);

   overriding function Image
     (Data       : in Null_Error_Data;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Valid_Node_Access)
     return String
   is ("");

   pragma Warnings (Off, """others"" choice is redundant");
   No_Error : constant Null_Error_Data := (others => <>);
   pragma Warnings (On);

   No_Error_Classwide : constant Error_Data'Class := Error_Data'Class (No_Error);

   package Error_Data_Lists is new SAL.Gen_Indefinite_Doubly_Linked_Lists (Error_Data'Class);
   type Error_Data_List_Ref (List : not null access constant Error_Data_Lists.List) is private
   with Implicit_Dereference => List;

   type Node_Label is
     (Source_Terminal,    -- text is user input, accessed via Lexer
      Virtual_Terminal,   -- no text; inserted during error recovery
      Virtual_Identifier, -- text in user data, created during tree rewrite
      Nonterm);           -- nonterminal node.
   subtype Terminal_Label is Node_Label range Source_Terminal .. Virtual_Identifier;
   subtype Virtual_Terminal_Label is Node_Label range Virtual_Terminal .. Virtual_Identifier;

   function Label (Node : in Valid_Node_Access) return Node_Label;

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID;

   type Stream_Node_Ref is record
      Stream  : Stream_ID;
      Element : Stream_Index;
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
   --  be First_Terminal of an empty nonterm, while still allowing
   --  Next_Terminal (Ref).
   --
   --  Note that this is False in post-parse actions; there are no
   --  streams, so Element is Invalid_Stream_Index.

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
   is (Valid_Stream_Node (Tree, Ref) and Ref.Node /= Invalid_Node_Access);
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

   function To_Rooted_Ref
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Rooted_Ref;

   type Stream_Node_Parents is record
      Ref     : Stream_Node_Ref;
      Parents : Node_Stacks.Stack;
      --  Parents stores the path from Ref.Element.Node to Ref.Node. Parents
      --  is empty if Ref is rooted (ie Ref.Element.Node = Ref.Node). If not
      --  rooted, Parents.Peek (1) is Ref.Node parent, and Parents.Peek
      --  (Parents.Depth) is Ref.Element.Node.
   end record;

   Invalid_Stream_Node_Parents : constant Stream_Node_Parents;

   type Stream_Node_Parents_Array is array (Positive_Index_Type range <>) of Stream_Node_Parents;

   function Parents_Valid (Ref : in Stream_Node_Parents) return Boolean;
   --  True if Parents gives the path from Element.Node to Node, or Element or Node is invalid.

   function To_Stream_Node_Parents (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Stream_Node_Parents
   with Pre => Ref = Invalid_Stream_Node_Ref or else Tree.Parents_Set or else
               (Rooted (Ref) or Ref.Node = Tree.First_Terminal (Get_Node (Ref.Element))),
     Post => Parents_Valid (To_Stream_Node_Parents'Result);

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

         First_Terminal : Node_Access := Invalid_Node_Access;
         Last_Terminal  : Node_Access := Invalid_Node_Access;
         --  For ID in Nonterminals, first and last terminal of this token in
         --  the Tree shared or parse stream, cached from children;
         --  Invalid_Node_Access if the children are virtual. For terminals,
         --  Invalid_Node_Access. Used to detect empty nonterm and compute
         --  Name.

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
         --  For terminals, Element_Node = Node.
         --
         --  For nonterminals, Node = some descendant of Element_Node (similar
         --  to a Stream_Node_Ref).
         --
         --  This token can be virtual, if from Shared_Stream of an edited
         --  tree.
         --
         --  In a non-default Recover_Token, Element_Node cannot be
         --  Invalid_Node_Access. Node can be Invalid_Node_Access when it is
         --  nominally a terminal and Element_Node is an empty nonterm.
      end case;
   end record;

   subtype Virtual_Recover_Token is Recover_Token (Virtual => True);
   subtype Real_Recover_Token is Recover_Token (Virtual => False);

   Invalid_Recover_Token : constant Recover_Token := (Virtual => True, ID => Invalid_Token_ID, others => <>);

   function Node_ID (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Token_ID
   with Pre => Item.Virtual = False;
   function Element_ID (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Token_ID;

   function Element_Is_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Boolean;
   --  Virtual ID or Element_Node.

   function Byte_Region (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region;

   function Name (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region;
   --  If Node.Name = Null_Buffer_Region and Is_Terminal (Node.ID),
   --  return Node.Byte_Region; else return Node.Name.

   procedure Set_Name
     (Tree : in     Syntax_Trees.Tree;
      Item : in out Recover_Token;
      Name : in     Buffer_Region);

   function Contains_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Boolean;
   function Contains_Virtual_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean;

   function Is_Empty_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Boolean;
   --  True if node contains no terminals.

   function First_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access;
   function Last_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access;

   function To_Real_Recover_Token (Item : in Stream_Node_Ref) return Real_Recover_Token
   with Pre => Item.Element /= Invalid_Stream_Index;

   function Make_Rooted (Item : in Recover_Token) return Recover_Token;

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
     (Augmented        : in out Base_Augmented;
      Shift_Bytes      : in     Base_Buffer_Pos;
      Shift_Chars      : in     Base_Buffer_Pos;
      Shift_Line       : in     Base_Line_Number_Type;
      Last_Stable_Byte : in     Buffer_Pos)
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
      Token         : in     Lexer.Token;
      Grammar_Token : in     Valid_Node_Access)
   is null;
   --  Token is a grammar or non-grammar token that was just returned by
   --  User_Data.Lexer. If grammar, it is Grammar_Token; if non-grammar,
   --  it has already been added to Grammar_Token (which is SOI if before
   --  first grammar token in input). Called before parsing, once for
   --  each non-grammar token in the input stream.

   function Copy_Augmented
     (User_Data : in User_Data_Type;
      Augmented : in Augmented_Class_Access)
     return Augmented_Class_Access
   with Pre => Augmented /= null;
   --  Default implementation raises SAL.Programmer_Error.

   function Insert_After
     (User_Data           : in out User_Data_Type;
      Tree                : in     Syntax_Trees.Tree'Class;
      Insert_Token        : in     Valid_Node_Access;
      Insert_Before_Token : in     Valid_Node_Access;
      Comment_Present     : in     Boolean;
      Blank_Line_Present  : in     Boolean)
     return WisiToken.Insert_Location
   with Post'Class => (if not (Blank_Line_Present or Comment_Present) then Insert_After'Result /= Between);
   --  Return an insert location for Insert_Token. This can affect which
   --  line it appears on, which affects indentation. Should be called from
   --  user-overridden Insert_Token.
   --
   --  If Comment_Present, there is a comment between Tree.Prev_Terminal
   --  (Insert_Before_Token) and Insert_Before_Token.
   --
   --  If Blank_Line_Present, there is at least one blank line
   --  immediately after Tree.Prev_Terminal (Insert_Before_Token) (before
   --  any comment).
   --
   --  The default implementation always returns Before_Next.

   procedure Insert_Token
     (User_Data      : in out User_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Trace          : in out WisiToken.Trace'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   is null
   with Pre'Class => Tree.Parents_Set and Tree.Is_Virtual_Terminal (Inserted_Token);
   --  Inserted_Token was inserted in error recovery. Move
   --  Inserted_Token.Non_Grammar as needed to control which line the
   --  token is on.
   --
   --  Called from Execute_Actions for each inserted token, before
   --  Initialize_Actions.

   procedure Delete_Token
     (User_Data     : in out User_Data_Type;
      Tree          : in     Syntax_Trees.Tree'Class;
      Trace         : in out WisiToken.Trace'Class;
      Deleted_Token : in     Valid_Node_Access)
   with Pre'Class =>
     Tree.Parents_Set and
     Tree.Label (Deleted_Token) in Terminal_Label;
   --  Deleted_Token was deleted in error recovery; Deleted_Token.Parent
   --  is the non-deleted terminal token before Deleted_Token in the
   --  parse stream.
   --
   --  The default body appends Deleted_Token.Non_Grammar to
   --  Deleted_Token.Parent.Non_Grammar.
   --
   --  Called from Execute_Actions for each deleted token, before
   --  Initialize_Actions.

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
      Nonterm   : in     Valid_Node_Access);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree.

   Null_Action : constant Post_Parse_Action := null;

   ----------
   --  Parsing operations (including error recovery and incremental
   --  parse), Tree and Node attributes.

   function New_Stream
     (Tree       : in out Syntax_Trees.Tree;
      Old_Stream : in     Stream_ID;
      User_Data  : in     User_Data_Access)
     return Stream_ID
   with
     Pre => Old_Stream = Invalid_Stream_ID or else
            (not Tree.Parents_Set and Tree.Stream_Count > 1 and Tree.Is_Valid (Old_Stream)),
     Post => Tree.Is_Valid (New_Stream'Result);
   --  Create a new parse stream, initially copied from Old_Stream.

   function Stream_Count (Tree : in Syntax_Trees.Tree) return Natural;
   --  Number of active streams.

   function First_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   with Pre => Tree.Stream_Count >= 2;

   function Last_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   with Pre => Tree.Stream_Count >= 2;

   procedure Next_Parse_Stream (Tree : in Syntax_Trees.Tree; Stream : in out Stream_ID)
   with Pre => Stream /= Invalid_Stream_ID;

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

   procedure Start_Lex
     (Tree           : in out Syntax_Trees.Tree)
   with Pre => Tree.Cleared,
     Post => Tree.Lexable;
   --  Create empty Tree.Shared_Stream, add SOI node containing
   --  Tree.Lexer.Begin_Pos values to it.

   procedure Start_Parse
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   with Pre => (not Tree.Traversing and Stream /= Tree.Shared_Stream and Tree.Is_Valid (Stream)) and then
               Tree.Stream_Length (Stream) = 0;
   --  State must be the parser start state; it is stored in Tree.SOI.
   --  Sets Tree.Parents_Set False.

   procedure Start_Edit (Tree : in out Syntax_Trees.Tree)
   with Pre => Tree.Editable,
     Post => Tree.Parseable;
   --  Construct Tree.Shared_Stream from Tree.SOI, Tree.Root, Tree.EOI.
   --
   --  On return, Tree is ready for Parse.Edit_Tree.
   --
   --  Parents_Set remains True. Parse.Edit_Tree calls Breakdown on
   --  Shared_Stream Elements, which removes some parent links. However,
   --  the remaining stream elements all have complete parent links; the
   --  links removed point to nodes that are not accessible from the
   --  shared stream.

   function Reduce
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Child_Count     : in     Ada.Containers.Count_Type;
      Action          : in     Post_Parse_Action := null;
      State           : in     State_Index;
      Default_Virtual : in     Boolean         := False)
     return Rooted_Ref
   with Pre => not Tree.Traversing and not Tree.Parents_Set and Tree.Is_Valid (Stream) and Stream /= Tree.Shared_Stream,
     Post => Reduce'Result.Stream = Stream and Tree.Valid_Stream_Node (Reduce'Result);
   --  Reduce Child_Count tokens on Stream top of stack to a new Nonterm
   --  node on Stream top of stack. Result points to the new Nonterm
   --  node. If Child_Count = 0, set Nonterm.Virtual := Default_Virtual.
   --
   --  Set Result byte_region, char_region, line, column,
   --  first_terminal to min/max of children.

   procedure Shift
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index);
   --  Shift Stream current input token onto Stream stack. Then set State
   --  in the Stream element.
   --
   --  Does _not_ clear shifted node Error_List; this shift may be part
   --  of error recover.

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

   procedure Move_Shared_To_Input
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID);
   --  Insert Stream.Shared_Link into Parse_Stream input, increment
   --  Stream.Shared_Link.
   --
   --  This is often needed before Left_Breakdown while parsing.

   procedure Move_Shared_To_Input
     (Tree   : in out Syntax_Trees.Tree;
      First  : in     Stream_Node_Ref;
      Last   : in     Stream_Node_Ref;
      Stream : in     Stream_ID)
   with Pre => Valid_Stream_Node (Tree, First) and Valid_Stream_Node (Tree, Last) and
               First.Stream = Tree.Shared_Stream and Last.Stream = Tree.Shared_Stream and
               Stream /= Tree.Shared_Stream;
   --  Insert Shared_Stream elements First .. Last into Stream input.
   --  Update Parse_Stream.Shared_Link to next Shared_Stream element
   --  after Last.

   procedure Breakdown
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      User_Data : in     Syntax_Trees.User_Data_Access)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref) and
               Tree.Label (Ref.Ref.Element) = Nonterm and Ref.Ref.Node /= Invalid_Node_Access and
               Tree.Stack_Top (Ref.Ref.Stream) /= Ref.Ref.Element,
     Post => Parents_Valid (Ref) and Tree.Valid_Terminal (Ref.Ref) and
             Tree.First_Terminal (Get_Node (Ref.Ref.Element)) = Ref.Ref.Node;
   --  Bring descendants of Ref.Element to the parse stream, until
   --  First_Terminal of one of the parse stream elements = Ref.Node.
   --  Ref.Element is updated to the element whose first terminal is
   --  (a copy of) Ref.Node.
   --
   --  If Ref.Parents nonterms contain any errors, the errors are moved to
   --  the first terminal of that nonterm, copying ancestor nodes, and
   --  Ref.Parents is updated to match. If Ref.Node is one of those first
   --  terminals, it will be copied.
   --
   --  The stack top is unchanged.
   --
   --  Parent/child links are set to Invalid_Node_Access as appropriate.

   procedure Breakdown
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Terminal_Ref;
      User_Data : in     User_Data_Access)
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Label (Ref.Element) = Nonterm and
               Ref.Node /= Invalid_Node_Access and
               Tree.Stack_Top (Ref.Stream) /= Ref.Element,
     Post => Tree.First_Terminal (Get_Node (Ref.Element)) = Ref.Node;
   --  Same as Breakdown (Stream_Node_Parents), but supports not
   --  Tree.Parents_Set, using an internal Stream_Node_Parent.

   procedure Left_Breakdown
     (Tree      : in out Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Ref;
      User_Data : in     Syntax_Trees.User_Data_Access)
   with Pre =>
     Valid_Stream_Node (Tree, Ref) and then
     (Tree.Label (Ref.Element) = Nonterm and
        Tree.First_Terminal (Ref).Node /= Invalid_Node_Access and
        Tree.Stack_Top (Ref.Stream) /= Ref.Element),
     Post => Valid_Single_Terminal (Tree, Ref);
   --  Similar to Breakdown; bring first terminal of Ref.Element to
   --  the stream Ref.Stream. Ref.Node is ignored on input.

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
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean := True)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   function Stream_First
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean := True)
     return Rooted_Ref
   with Pre => Tree.Is_Valid (Stream);

   function Stream_Last
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

   function Stack_Top
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);

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

   function Current_Token
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   with Post => Correct_Stream_Node (Tree, Current_Token'Result);
   --  If Stream has input, then first input of Stream; otherwise
   --  Stream.Shared_Link.

   function Shared_Token
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   with Post => Correct_Stream_Node (Tree, Shared_Token'Result);
   --  Stream.Shared_Link.

   procedure Delete_Current_Token
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID);
   --  User must call Add_Deleted with Deleted_Ref => Tree.Current_Input
   --  before calling Delete_Current_Token.

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   with Pre => Element = Invalid_Stream_Index or else
               (Tree.Contains (Stream, Element) or Tree.Contains (Tree.Shared_Stream, Element));
   --  If Element is Invalid_Stream_Index, result is Stream_First (= SOI).

   function Stream_Next
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Stream_Next'Result);
   --  Return stream element after Ref.Element.

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and then
             (Ref = Invalid_Stream_Node_Ref or else
                (if Rooted
                 then Tree.Get_Node (Ref.Stream, Ref.Element) = Ref.Node
                 else Tree.First_Terminal (Tree.Get_Node (Ref.Stream, Ref.Element)) = Ref.Node));
   --  Update Ref to root or first terminal of next stream element after Ref.Element.

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   with Pre => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref) and
             (Ref.Ref = Invalid_Stream_Node_Ref or else
                (if Rooted
                 then Tree.Get_Node (Ref.Ref.Stream, Ref.Ref.Element) = Ref.Ref.Node
                 else Tree.First_Terminal (Tree.Get_Node (Ref.Ref.Stream, Ref.Ref.Element)) = Ref.Ref.Node));
   --  Update Ref to root or first terminal of next stream element after
   --  Ref.Element.

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
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean := True)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref) and
             (Ref = Invalid_Stream_Node_Ref or else
                (if Rooted
                 then Tree.Get_Node (Ref.Stream, Ref.Element) = Ref.Node
                 else Tree.Last_Terminal (Tree.Get_Node (Ref.Stream, Ref.Element)) = Ref.Node));
   --  Update Ref to root or last terminal of stream element before Ref.Element.

   procedure Stream_Prev
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and
             (Ref.Ref = Invalid_Stream_Node_Ref or else
                (if Rooted
                 then Tree.Get_Node (Ref.Ref.Stream, Ref.Ref.Element) = Ref.Ref.Node
                 else Tree.Last_Terminal (Tree.Get_Node (Ref.Ref.Stream, Ref.Ref.Element)) = Ref.Ref.Node));

   procedure Stream_Insert
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Index)
   with Pre => Tree.Contains (Stream, Before);
   --  Insert a new stream element on Stream containing Node, before Before.

   function Stream_Insert
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Index)
     return Stream_Node_Ref
   with Pre => Tree.Contains (Stream, Before);
   --  Insert a new stream element on Stream containing Node, before Before.
   --  Result references new element.

   function Peek
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Count  : in SAL.Peek_Type := 1)
     return Stream_Index
   with Pre => Tree.Is_Valid (Stream);
   --  Return Count element on stack in Stream; Count = 1
   --  returns stack top.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Lexer.Token;
      Error    : in     Error_Data'Class)
     return Single_Terminal_Ref
   with Pre => not Tree.Traversing and Stream = Tree.Shared_Stream,
     Post => Tree.Label (Add_Terminal'Result.Node) = Source_Terminal;
   --  Append a new Source_Terminal element to Stream. Result points to the added
   --  node.

   type Token_Array_Var_Ref (Element : not null access WisiToken.Lexer.Token_Arrays.Vector) is private
   with Implicit_Dereference => Element;

   type Token_Array_Const_Ref (Element : not null access constant WisiToken.Lexer.Token_Arrays.Vector) is private
   with Implicit_Dereference => Element;

   function Has_Non_Grammar
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Boolean
   with Pre => Tree.Label (Terminal) in Terminal_Label;

   function Non_Grammar_Var
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Token_Array_Var_Ref
   with Pre => Tree.Label (Terminal) in Terminal_Label;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Token_Array_Const_Ref
   with Pre => Tree.Label (Terminal) in Terminal_Label;

   procedure Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Lexer.Token;
      Before   : in     Stream_Index;
      Error    : in     Error_Data'Class)
   with Pre => not Tree.Traversing and (Before = Invalid_Stream_Index or else Tree.Contains (Stream, Before));
   --  Insert a new Source_Terminal element on Stream, before Before.

   function Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Lexer.Token;
      Before   : in     Stream_Index;
      Error    : in     Error_Data'Class)
     return Single_Terminal_Ref
   with Pre => not Tree.Traversing and (Before = Invalid_Stream_Index or else Tree.Contains (Stream, Before)),
     Post => Tree.Label (Insert_Source_Terminal'Result.Node) = Source_Terminal;
   --  Insert a new Source_Terminal element on Stream, before Before.
   --  Result points to the added element.

   function Insert_Virtual_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID)
     return Single_Terminal_Ref
   with
     Pre  => not Tree.Traversing and Tree.Is_Valid (Stream) and Stream /= Tree.Shared_Stream,
     Post => Tree.Label (Insert_Virtual_Terminal'Result.Node) = Virtual_Terminal;
   --  Insert a new Virtual_Terminal element into Stream, after
   --  Stack_Top. Result refers to the added node.

   procedure Shift
     (Tree             : in     Syntax_Trees.Tree;
      Node             : in     Valid_Node_Access;
      Shift_Bytes      : in     Base_Buffer_Pos;
      Shift_Chars      : in     Base_Buffer_Pos;
      Shift_Lines      : in     Base_Line_Number_Type;
      Last_Stable_Byte : in     Buffer_Pos;
      Non_Grammar_Next : in out Lexer.Token_Arrays.Extended_Index)
   with Pre => Tree.Label (Node) in Terminal_Label;
   --  Add Shift_* to token, non_grammar, and augmented corresponding
   --  regions. If a non_grammar is adjacent to or after
   --  Last_Stable_Byte, set Non_Grammar_Next to it, without shifting,
   --  and skip the rest of non_grammar.

   procedure Set_Node_Index
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      Node_Index : in Syntax_Trees.Node_Index);

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
   --  The precondition allows either stream; Current_Token is
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

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Source_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;

   function Is_Empty_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   --  True if Node contains no terminals.

   function Is_Empty_Or_Virtual_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean;
   --  True if Node contains no terminals, or all terminals are virtual,
   --  and thus have an empty Buffer_Region for Byte_ and Char_Region.

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Set_Insert_Location
     (Tree            : in Syntax_Trees.Tree;
      Node            : in Valid_Node_Access;
      Insert_Location : in WisiToken.Insert_Location)
   with Pre => Tree.Is_Virtual_Terminal (Node);

   procedure Set_Name
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Region : in Buffer_Region)
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

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region;
   --  If Trailing_Non_Grammar, any non_grammar attached to last terminal
   --  in Node is included in region.
   --
   --  If Tree.Parents_Set:
   --
   --  Byte_Region of Virtual_Terminal is an empty region with .First
   --  determined by Insert_Location, using previous or next
   --  source_terminal or non_grammar.
   --
   --  Byte_Region of an empty nonterm with Trailing_Non_Grammar False is
   --  an empty region; .First gives nominal location in source text,
   --  using previous or next source_terminal or non_grammar.
   --
   --  If not Tree.Parents_Set, does as much of the above as possible,
   --  returning Null_Buffer_Region if would need Parents_Set.

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region
   with Pre => Tree.Parents_Set and Valid_Stream_Node (Tree, Ref);
   --  Return Byte_Region of Ref.Node, using stream to find prev, next
   --  non_grammar if needed.

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Parents;
      Parse_Stream         : in Stream_ID;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);
   --  Same as Byte_Region (Stream_Node_Ref), for use when parents are
   --  not set. See Prev_Terminal (tree, stream_node_parents) for meaning
   --  of Parse_Stream.

   function Name (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Buffer_Region;
   --  If Node.Label in Terminal_Label, return Node.Byte_Region; else if
   --  Node.Name is not Null_Buffer_Region, return Node.Name; else return
   --  Node.Byte_Region.

   function Name (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Buffer_Region
   with Pre => Valid_Stream_Node (Tree, Ref);
   --  Call Name with Ref.Element.Node.

   function Char_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region;
   --  Similar to Byte_Region.

   function Line_At_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Byte_Pos : in Buffer_Pos)
     return Base_Line_Number_Type
   with Pre => Tree.Editable;
   --  Return line that contains Byte_Pos; Invalid_Line_Number is outside
   --  range of text spanned by Tree.

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   with Pre => Tree.Editable;
   --  Lines of tokens in Node. First is the line started by the first
   --  New_Line or SOI (start of input) before the first terminal in
   --  Node. If Trailing_Non_Grammar, Last is the line ended by the last
   --  New_Line in the first non_grammar array after the last terminal of
   --  Node, or EOI (end of input); if not Trailing_Non_Grammar, Last is
   --  the line ended by the first New_Line or EOI after the last
   --  terminal of Node.

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   with Pre => Tree.Valid_Stream_Node (Ref) and
               (Tree.Parents_Set or Rooted (Ref) or Ref.Node = Tree.First_Terminal (Get_Node (Ref.Element)));
   --  Same as Line_Region (Ref.Node), using Ref.Stream to find
   --  prev/next non_grammar.
   --
   --  If not Tree.Parents_Set, constructs a Stream_Node_Parents
   --  internally.

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Parents;
      Parse_Stream         : in Stream_ID;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   with Pre => Tree.Valid_Stream_Node (Ref.Ref) and Parents_Valid (Ref);
   --  Same as Line_Region (Stream_Node_Ref), for use when parents are
   --  not set. See Prev_Terminal (tree, stream_node_parents) for meaning
   --  of Parse_Stream.

   function Line_Region
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Real_Recover_Token)
     return WisiToken.Line_Region
   with Pre => Ref.Element_Node = Ref.Node or Ref.Node = Tree.First_Terminal (Ref.Element_Node);
   --  Constructs a Stream_Node_Parents from Stream, Ref. Assumes
   --  Trailing_Non_Grammar => True. For use in error recovery.

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
   --  Return the ancestor of Node that contains one of IDs (starting
   --  search with Node.Parent), or Invalid_Node_Access if none match.
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

   function Prev_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return first node before Node that has a non-empty Non_Grammar.
   --  If Node = Tree.Root or Tree.SOI, return Tree.SOI.
   --
   --  Returns Invalid_Node_Access only in broken trees; we tolerate this
   --  here so we can use this in Error_Message.

   procedure Prev_Non_Grammar
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   with Pre => Tree.Valid_Stream_Node (Ref.Ref) and Parents_Valid (Ref),
     Post => Tree.Correct_Stream_Node (Ref.Ref) and Parents_Valid (Ref);

   function First_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access;
   --  Return first node in subtree under Node that has a non-empty
   --  Non_Grammar.
   --  Invalid_Node_Access if none.

   function Last_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access;
   --  Return last node in subtree under Node that has a non-empty
   --  Non_Grammar.
   --  Invalid_Node_Access if none.

   function Next_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Valid_Node_Access
   with Pre => Tree.Parents_Set;
   --  Return first node after Node that has a non-empty Non_Grammar.
   --  If Node = Tree.Root or Tree.EOI, return Tree.EOI.

   procedure Next_Non_Grammar
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Tree.Correct_Stream_Node (Ref.Ref) and Parents_Valid (Ref);

   function First_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean;
      Following            : in Boolean := False)
     return Node_Access
   with Pre => (if Following then Tree.Parents_Set else True);
   --  Return a terminal node that can give byte or char pos.
   --
   --  If Trailing_Non_Grammar, return first terminal in Node that is a
   --  Source_Terminal, or a virtual terminal with non-empty non_grammar.
   --  If not Trailing_Non_Grammar, only return a Source_Terminal.
   --
   --  If Following, return a matching terminal following Node if none
   --  found in Node.

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return the next terminal node after Node that can give byte or
   --  char pos; Invalid_Node_Access if there is no such node.
   --
   --  If Trailing_Non_Grammar, return next terminal after Ref.Ref.Node
   --  that is a Source_Terminal, or a virtual terminal with non-empty
   --  non_grammar. If not Trailing_Non_Grammar, only return a
   --  Source_Terminal.

   procedure Next_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Ref;
      Trailing_Non_Grammar : in     Boolean)
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Parents_Set,
     Post => Tree.Correct_Stream_Node (Ref);
   --  Update Ref to the next terminal node that can give byte or char
   --  pos.
   --
   --  If Trailing_Non_Grammar, return next terminal after Ref.Node
   --  that is a Source_Terminal, or a virtual terminal with non-empty
   --  non_grammar. If not Trailing_Non_Grammar, only return a
   --  Source_Terminal.

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean)
     return Stream_Node_Ref
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Parents_Set,
     Post => Tree.Correct_Stream_Node (Prev_Source_Terminal'Result);
   --  Return the previous terminal node that can give byte or char pos.
   --
   --  If Trailing_Non_Grammar, return prev terminal before Ref.Ref.Node
   --  that is a Source_Terminal, or a virtual terminal with non-empty
   --  non_grammar. If not Trailing_Non_Grammar, only return a
   --  Source_Terminal.

   procedure Prev_Source_Terminal
     (Tree                 : in     Syntax_Trees.Tree;
      Ref                  : in out Stream_Node_Parents;
      Parse_Stream         : in     Stream_ID;
      Trailing_Non_Grammar : in     Boolean)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and not Tree.Parents_Set,
     Post   => Tree.Correct_Stream_Node (Ref.Ref);

   function Get_Virtuals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   with Pre => Tree.Parents_Set,
     Post => (for all Node of Get_Virtuals'Result => Tree.Label (Node) in Virtual_Terminal_Label);
   --  Return list of virtual terminals in Node.

   function Count_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Natural;

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   with Post => (for all Node of Get_Terminals'Result => Tree.Label (Node) in Terminal_Label);
   --  Return sequence of terminals in Node.

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID_Array;
   --  Same as Get_Terminals, but return the IDs.

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  First of Get_Terminals. Invalid_Node_Access if Node is an empty nonterminal.

   function First_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Same as First_Terminal (Tree, Node), also initializes Parents to
   --  store path from Node to the first terminal, for Next_Terminal in
   --  nodes that have unset parent links, or to limit Next_Terminal to
   --  descendants of Node.
   --
   --  We don't have "Pre => Parents.Is_Empty" or "Post => Parents_Valid
   --  (Parents, First_Terminal'Result)", because we call this function
   --  recursively to create Parents.
   --
   --  Visible for use with error recovery Configuration input stream.

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Valid_Stream_Node (Tree, Ref);
   --  Update Ref to first terminal of Ref.Element.Node or a following
   --  stream element. Continues search in Shared_Stream at
   --  Stream.Shared_Link; will always find EOI.

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Valid_Stream_Node (Tree, First_Terminal'Result);
   --  Return first terminal in Ref.Element.Node or a following stream element.
   --  Continues search in Shared_Stream; will always find EOI, so never
   --  Invalid_Stream_Element.
   --
   --  Use First_Terminal_In_Node to not look in following stream
   --  elements.

   function First_Terminal_In_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   with Pre => Valid_Stream_Node (Tree, Ref);
   --  Return first terminal in Ref.Node; Invalid_Node_Access if none.

   procedure First_Terminal
     (Tree      : in     Syntax_Trees.Tree;
      Ref       : in out Stream_Node_Parents;
      Following : in     Boolean)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Parents_Valid (Ref);
   --  Update Ref to first terminal in Ref.Ref.Node or, if Following, a
   --  following stream element - continues search in Shared_Stream.

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Rooted_Ref)
     return Stream_Node_Parents
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Valid_Stream_Node (Tree, First_Terminal'Result.Ref) and
             Label (First_Terminal'Result.Ref.Node) in Terminal_Label and
             Parents_Valid (First_Terminal'Result);
   --  Return first terminal in Ref.Node or a following element.

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access;
   --  Last terminal in subtree under Node. Invalid_Node_Access if none.

   function Last_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Same as Last_Terminal, also initializes Parents.
   --
   --  We don't have "Pre => Parents.Is_Empty" or "Post => Parents_Valid
   --  (Parents, Last_Terminal'Result)", because we call this function
   --  recursively to build Parents.
   --
   --  Visible for use with error recovery Configuration input stream.

   procedure Last_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Ref.Parents.Is_Empty,
     Post => Parents_Valid (Ref);
   --  Update Ref to last terminal of Ref.Ref.Element.Node or preceding
   --  element.

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Parents_Set,
     Post => Prev_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Prev_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately before Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the first terminal
   --  in that subtree.

   function Prev_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   with Pre => Tree.Parents_Set and Tree.Valid_Terminal (Ref),
     Post => Tree.Correct_Stream_Node (Prev_Terminal'Result);

   procedure Prev_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => Tree.Parents_Set and Tree.Valid_Terminal (Ref),
     Post => Tree.Correct_Stream_Node (Ref);

   procedure Prev_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Tree.Correct_Stream_Node (Ref.Ref) and Parents_Valid (Ref);
   --  If Parse_Stream is not Invalid_Stream_ID and Ref.Stream is
   --  Shared_Stream, switches from Shared_Stream to Parse_Stream at
   --  Parse_Stream.Shared_Link.

   procedure Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access)
   with Pre => Tree.Parents_Set,
     Post => Node = Invalid_Node_Access or else
             Tree.Label (Node) in Terminal_Label;

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   with Pre => Tree.Parents_Set,
     Post => Next_Terminal'Result = Invalid_Node_Access or else
             Tree.Label (Next_Terminal'Result) in Terminal_Label;
   --  Return the terminal that is immediately after Node in subtree
   --  containing Node; Invalid_Node_Access if Node is the last terminal
   --  in that subtree.

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   with Pre => Tree.Parents_Set and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Ref);
   --  Update Ref to the next terminal that is after Ref.Node in Stream.
   --  Continues search in Shared_Stream; will always find EOI. Result is
   --  Invalid_Stream_Node_Ref if Ref.Node is EOI.

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);
   --  Same as Next_Terminal (Tree, Ref), using Ref.Parents for parent
   --  links. Ref.Parents is initialized by First_Terminal.

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   with Pre => Tree.Parents_Set and Valid_Terminal (Tree, Ref),
     Post => Correct_Stream_Node (Tree, Next_Terminal'Result);
   --  Same as procedure Next_Terminal, but return result.

   procedure Next_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack);
   --  Same as Next_Terminal, using Parents instead of node parent links.
   --  Parent is initialized by First_Terminal.
   --
   --  Visible for use with error recovery Configuration input stream.

   function First_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Node_Access)
     return Node_Access;
   --  Return first terminal with valid Sequential_Index in Node;
   --  Invalid_Node_Access if none.

   function First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Same as Tree.First_Sequential_Terminal (Node), also initialize
   --  Parents.

   procedure First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack);
   --  Update Node to first terminal with valid Sequential_Index in Node;
   --  Invalid_Node_Access if none. Also initialize Parents.

   procedure First_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Parents)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);
   --  Return first terminal with valid Sequential_Index in Ref.Node or a
   --  following stream element; continues search in Tree.Shared_Stream.
   --  Invalid_Node_Access if none found.

   procedure First_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Parents_Set,
     Post => Correct_Stream_Node (Tree, Ref);
   --  Return first terminal with valid Sequential_Index in Ref.Node or a
   --  following stream element; continues search in Tree.Shared_Stream.
   --  Invalid_Node_Access if none found.

   function First_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Syntax_Trees.Rooted_Ref)
     return Terminal_Ref
   with Pre => Valid_Stream_Node (Tree, Ref),
     Post => Correct_Stream_Node (Tree, First_Sequential_Terminal'Result);
   --   Same as First_Sequential_Terminal, does not require Parents_Set.

   function Last_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access;
   --  Return last terminal in Node that has a valid Sequential_Index,
   --  also initialize Parents.

   function Last_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access)
     return Node_Access;
   --  Return last terminal in Node that has a valid Sequential_Index.
   --  Uses an internal parents stack.

   procedure Last_Sequential_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Syntax_Trees.Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);
   --  Update Ref to last terminal with valid Sequential_Index in
   --  Ref.Node or a preceding stream element; if Ref.Stream is
   --  Tree.Shared_Stream, switches to Parse_Stream at
   --  Parse_Stream.Shared_Link. Invalid_Node_Access if none found.

   procedure Next_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   with Pre => Tree.Label (Node) in Terminal_Label;
   --  Update Node to the first terminal with valid Sequential_Index
   --  following Node. .

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Ref)
   with Pre => Valid_Stream_Node (Tree, Ref) and Tree.Parents_Set,
     Post => Correct_Stream_Node (Tree, Ref);

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Parents)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);
   --  Update Node to the first terminal with valid Sequential_Index
   --  succeeding Node. Can step past EOI.

   procedure Prev_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   with Pre => Tree.Label (Node) in Terminal_Label;
   --  Update Node to the last terminal with valid Sequential_Index
   --  preceding Node. Can step past SOI.

   procedure Prev_Sequential_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID)
   with Pre => Valid_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref),
     Post => Correct_Stream_Node (Tree, Ref.Ref) and Parents_Valid (Ref);

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Valid_Node_Access_Array;
   --  Return all descendants of Node matching ID.

   ----------
   --  Post-parsing operations; editing the tree. The tree has one or
   --  zero streams, so these subprograms have no stream argument.
   --
   --  Some of these are also used for Packrat parsing, and don't have a
   --  precondition of Fully_Parsed.

   function Cleared (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there are no streams and no nodes.

   function Lexable (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there is a shared stream that contains only SOI.

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there are no parse streams and
   --  Shared_Stream holds a lexed or edited stream.

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if there is only one parse stream, and it has only two
   --  elements; SOI with the start state and the tree root (EOI is only
   --  in the shared stream).

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
   with Pre => Tree.Fully_Parsed or Tree.Stream_Count = 1,
     Post => Tree.Editable;
   --  If Tree.Root is not set, set it to the root of the single
   --  remaining parse stream. Delete the parse stream and shared stream.
   --  Delete all nodes not reachable from the root, and not Tree.SOI,
   --  Tree.EOI, or in Keep_Nodes. Also call Set_Parents if not
   --  Tree.Parents_Set.
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
   --  rooted at Tree.Root.
   --
   --  No precondition for packrat.

   function Valid_Root (Tree : in Syntax_Trees.Tree) return Boolean;
   --  True if Tree has a single root.

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   with Pre => Tree.Valid_Root;
   --  Tree.Root, or the root in the last parse stream if Tree.Root is
   --  not set. Can be Invalid_Node_Access if input syntax does not allow
   --  parsing to succeed.

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access)
   with Pre => (Tree.Parseable and Tree.Label (New_Root) = Nonterm) and then Tree.Child_Count (New_Root) > 0;
   --  Set Tree.Root to Root. If New_Root.Children does not start with
   --  Tree.SOI, prepend it. If New_Root.Children does not end with
   --  Tree.EOI, append it.
   --
   --  Precondition matches Packrat parser conditions at end of parse.

   function SOI (Tree : in Syntax_Trees.Tree) return Node_Access;
   --  Return node representing start of input in the shared stream. It
   --  has non_grammar giving the first line number, and all non_grammar
   --  before the first grammar node.
   --
   --  Note that SOI may be copied in a parse stream, when it has
   --  Following_Deleted.

   function EOI (Tree : in Syntax_Trees.Tree) return Node_Access;
   --  Return node representing end of input in the shared stream. It has
   --  non_grammar giving the last line number. Invalid_Node_Access if it
   --  has not yet been seen by the lexer.
   --
   --  Note that EOI may be copied in a parse stream, when it has an error.

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return Count parent of Node.

   function Find_Byte_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Byte_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;
   --  Return the terminal that contains (including non_grammar if
   --  Trailing_Non_Grammar) or is first after Byte_Pos.
   --  Invalid_Node_Access if Byte_Pos is after text spanned by Tree.

   function Find_Byte_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Stream               : in Stream_ID;
      Byte_Pos             : in Buffer_Pos;
      Trailing_Non_Grammar : in Boolean;
      Start_At             : in Terminal_Ref)
     return Terminal_Ref
   with Pre => Start_At = Invalid_Stream_Node_Ref or else Tree.Byte_Region (Start_At.Node).First <= Byte_Pos;
   --  Return the terminal that contains (including non_grammar if
   --  Trailing_Non_Grammar) or is first after Byte_Pos.
   --  Invalid_Stream_Node_Ref if Byte_Pos is after text spanned by
   --  Tree.Stream.
   --
   --  If Start_At is not Invalid_Stream_Node_Ref, start search there.

   function Find_Char_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Char_Pos             : in Buffer_Pos;
      After                : in Boolean;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;
   --  If After, return the first terminal after or containing
   --  Char_Point. Otherwise return the terminal containing Char_Point.
   --  If Include_Non_Grammar, non_grammar is included in token
   --  char_region. Invalid_Node_Access if none.

   function Find_New_Line
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   with Pre => Tree.Editable,
     Post => Find_New_Line'Result = Invalid_Node_Access or else
             (Tree.Is_Terminal (Find_New_Line'Result));
   --  Return the terminal node containing a non_grammar that ends Line -
   --  1. Result is Invalid_Node_Access if Line is outside range spanned
   --  by Tree.

   function Find_New_Line
     (Tree                : in     Syntax_Trees.Tree;
      Line                : in     Line_Number_Type;
      Line_Begin_Char_Pos :    out Buffer_Pos)
     return Node_Access
   with Pre => Tree.Editable,
     Post => Find_New_Line'Result = Invalid_Node_Access or else
             (Tree.Is_Terminal (Find_New_Line'Result));
   --  Same as Find_New_Line, also updates Line_Begin_Char_Pos to first
   --  char pos on Line.

   procedure Next_New_Line
     (Tree               : in     Syntax_Trees.Tree;
      Start_Ref          : in     Terminal_Ref;
      After_Non_Grammar  : in     Positive_Index_Type;
      Result_Ref         :    out Terminal_Ref;
      Result_Non_Grammar :    out Positive_Index_Type)
   with Pre => Tree.Non_Grammar_Const (Start_Ref.Node).Last_Index >= After_Non_Grammar,
     Post => Tree.Non_Grammar_Const (Result_Ref.Node).Last_Index >= Result_Non_Grammar;
   --  Return next New_Line or EOI.

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Buffer_Pos
   with Pre => Tree.Editable;
   --  First character on Line in text spanned by tree under Tree.Root;
   --  it may be in no token, or in a grammar or non-grammar token.
   --  Result is Invalid_Buffer_Pos if Line is not in the text spanned by
   --  Tree, or if Line is inside a multi-line token.

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type;
      Stream : in Stream_ID)
     return Buffer_Pos;
   --  Same as other Line_Begin_Char_Pos, but searches in Stream instead of
   --  Tree.Root. If not found there, continues searching input in
   --  Shared_Stream.

   function Line_Begin_Token
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   with Pre => Tree.Parents_Set;
   --  Return the node under Tree.Root of the first terminal token on
   --  line Line; Invalid_Node_Access if there are no grammar tokens on
   --  the line (ie only comment or whitespace), or the line is outside
   --  the text spanned by Tree.

   function Line_Begin_Token
     (Tree                      : in Syntax_Trees.Tree;
      Line                      : in Line_Number_Type;
      Stream                    : in Stream_ID;
      Following_Source_Terminal : in Boolean)
     return Node_Access;
   --  Same as other Line_Begin_Token, but searches in Stream instead of
   --  Tree.Root. If not found there, continues searching input in
   --  Shared_Stream.
   --
   --  If Following_Source_Terminal, returns next Source_Terminal in
   --  stream if there are no grammar tokens on Line.

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
      Terminal : in     Lexer.Token;
      Error    : in     Error_Data'Class)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Terminal node with no parent, on no stream. Result
   --  points to the added node.

   function Add_Terminal
     (Tree       : in out Syntax_Trees.Tree;
      Terminal   : in     Token_ID)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Virtual_Terminal node with no parent, on no stream.
   --  Result points to the added node.

   procedure Add_Deleted
     (Tree          : in out Syntax_Trees.Tree;
      Deleted_Ref   : in     Stream_Node_Ref;
      Prev_Terminal : in out Stream_Node_Parents;
      User_Data     : in     User_Data_Access)
   with Pre =>
     Tree.Valid_Stream_Node (Deleted_Ref) and
     Tree.Label (Deleted_Ref.Node) in Terminal_Label and
     Tree.Valid_Stream_Node (Prev_Terminal.Ref) and
     Parents_Valid (Prev_Terminal) and
     Prev_Terminal.Ref.Stream /= Tree.Shared_Stream and
     Tree.Label (Prev_Terminal.Ref.Node) = Source_Terminal;
   --  Copy Prev_Terminal.Ref.Node, add Deleted_Node to
   --  Prev_Terminal.Ref.Node.Following_Deleted. Update Prev_Terminal to
   --  point to copied node. Move any non_grammar from Deleted_Node to
   --  Prev_Terminal.Ref.Node.
   --
   --  Note that this does _not_ delete Deleted_Ref from the input; use
   --  Delete_Current_Token for that.

   function Has_Following_Deleted
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
     return Boolean
   with Pre => Tree.Label (Node) = Source_Terminal;

   type Valid_Node_Access_List_Var_Ref (List : not null access Valid_Node_Access_Lists.List) is private
   with Implicit_Dereference => List;

   function Following_Deleted
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
     return Valid_Node_Access_List_Var_Ref
   with Pre => Tree.Label (Node) = Source_Terminal;

   procedure Delete_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in out Node_Access);
   --  Free all nodes under Root
   --
   --  No precondition; called from Finalize.

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index)
     return Valid_Node_Access
   with Pre => not Tree.Traversing and Tree.Editable;
   --  Add a new Virtual_Identifier node with no parent, on no stream.
   --  Result points to the added node.

   function Child_Index
     (Tree   : in Syntax_Trees.Tree;
      Parent : in Valid_Node_Access;
      Child  : in Valid_Node_Access)
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
     (Tree           : in out Syntax_Trees.Tree;
      Node           : in     Valid_Node_Access;
      Clear_Children : in     Boolean)
   with Pre => not Tree.Traversing;
   --  If Clear_Children and Node.Parent /= Invalid_Node_Access, set
   --  child in Node.Parent to Invalid_Node_Access, and if Node.Parent =
   --  Tree.Root, set Tree.Root to Node. Finally, set Node.Parent to
   --  Invalid_Node_Access.
   --
   --  Clear_Children should be False unless Tree is Editable or Node is
   --  in Shared_Stream.

   ----------
   --  Accessing parse errors

   function Contains_Error
     (Tree       : in Syntax_Trees.Tree;
      Error_Node : in Valid_Node_Access;
      Data       : in Error_Data'Class)
     return Boolean;
   --  True if Error_Node's error list contains an element matching Data.

   procedure Add_Error_To_Input
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access);
   --  Copy Stream.Shared_Link.Node to Stream, add Data to its error list.

   procedure Add_Error_To_Stack_Top
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access);
   --  Copy Stream.Stack_Top.Node, add Data to its error list.

   procedure Add_Errors
     (Tree      : in out Syntax_Trees.Tree;
      Error_Ref : in out Stream_Node_Parents;
      Errors    : in     Error_Data_Lists.List;
      User_Data : in     User_Data_Access)
   with Pre => Parents_Valid (Error_Ref) and
     (for all Err of Errors => not Tree.Contains_Error (Error_Ref.Ref.Node, Err));
   --  Copy Error_Ref.Node and parents, add Errors to its error list.
   --  Update Error_Ref to point to copied node.

   type Error_Predicate is access function (Cur : in Error_Data_Lists.Cursor) return Boolean;

   procedure Delete_Errors_In_Input
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Predicate : in     Error_Predicate;
      User_Data : in     User_Data_Access);
   --  Delete errors in Current_Token where Predicate returns True.
   --
   --  If Current_Token is a nonterm, deletes errors the entire subtree.

   function Input_Has_Matching_Error
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Data   : in Error_Data'Class)
     return Boolean;
   --  Return True if Data matches (according to Dispatching_Equal) an
   --  error on the current input node.

   procedure Update_Error
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Error_Ref : in out Stream_Node_Parents;
      Data      : in     Error_Data'Class;
      User_Data : in     User_Data_Access)
   with Pre => Tree.Contains_Error (Error_Ref.Ref.Node, Data),
     Post => Tree.Contains_Error (Error_Ref.Ref.Node, Data);
   --  Move Error_Ref to Stream, update error list element matching Data,
   --  copying Error_Ref.Node and all ancestors. Update Error_Ref to
   --  point to new stream element with copied nodes.

   function Error_List (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Error_Data_List_Ref;
   --  To change the error data, use Add_Error, which will copy Node.
   --
   --  Returns an empty list if Has_Error (Node) is false, so users can
   --  just use 'for Err of Tree.Error_List (Node) loop'

   type Error_Ref is record
      --  Used when tree is fully parsed.
      Node    : Node_Access;
      Deleted : Valid_Node_Access_Lists.Cursor;
      --  If Node = Invalid_Node_Access, no error. If Deleted = No_Element,
      --  Node has an error. If Deleted /= No_Element, any error on Node has
      --  already been visited, Element (Deleted) is a Source_Terminal that
      --  has an error, and Element (Deleted).Parent = Node.

      Error : Error_Data_Lists.Cursor;
      --  Element in error node Error_Data_List.
   end record;

   Invalid_Error_Ref : constant Error_Ref :=
     (Invalid_Node_Access, Valid_Node_Access_Lists.No_Element, Error_Data_Lists.No_Element);

   type Stream_Error_Ref is record
      --  Used while parsing
      Ref     : Stream_Node_Parents;
      Deleted : Valid_Node_Access_Lists.Cursor;
      Error   : Error_Data_Lists.Cursor;
   end record;

   Invalid_Stream_Error_Ref : constant Stream_Error_Ref;

   type Stream_Error_Cursor is private;
   --  We need an extra layer of indirection for this type because
   --  Stream_ID used in Stream_Node_Parents is private.

   function Error (Item : in Stream_Error_Cursor) return Stream_Error_Ref;

   function Error (Item : in Error_Ref) return Error_Data'Class;
   function Error (Item : in Stream_Error_Ref) return Error_Data'Class;

   function Error_Node (Tree : in Syntax_Trees.Tree; Error : in Error_Ref) return Node_Access
   with Pre => Valid_Error_Ref (Error);

   function Error_Node (Tree : in Syntax_Trees.Tree; Error : in Stream_Error_Ref) return Node_Access
   with Pre => Valid_Error_Ref (Error);

   function First_Error (Tree : in Syntax_Trees.Tree) return Error_Ref
   with Pre => Tree.Parents_Set;
   --  Return first error node in Tree.

   function First_Error (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Stream_Error_Ref;
   --  Return first error node in Stream.

   function Valid_Error_Ref (Error : in Error_Ref) return Boolean;
   function Valid_Error_Ref (Error : in Stream_Error_Ref) return Boolean;
   --  True if Error.Node is invalid_Node_Access or
   --  Error.Node.Parse_Error is non-null or Error.Node.Following_Deleted
   --  contains Error.Deleted and that node has an error.

   procedure Next_Error (Tree : in Syntax_Trees.Tree; Error : in out Error_Ref)
   with Pre => Tree.Parents_Set and (Error.Node /= Invalid_Node_Access and then Valid_Error_Ref (Error));
   --  Update Error to next error node.

   procedure Next_Error (Tree : in Syntax_Trees.Tree; Error : in out Stream_Error_Ref)
   with Pre => Error.Ref.Ref.Node /= Invalid_Node_Access and then Valid_Error_Ref (Error);
   --  Update Error to next error node.

   function Error_Count (Tree : in Syntax_Trees.Tree) return Ada.Containers.Count_Type
   with Pre => Tree.Parents_Set;
   function Error_Count (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return Ada.Containers.Count_Type;

   function Has_Error (Error : in Error_Ref) return Boolean;
   function Has_Error (Error : in Stream_Error_Ref) return Boolean;
   function Has_Error (Position : in Stream_Error_Cursor) return Boolean;
   function Has_Error (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean;
   function Has_Error (Node : in Valid_Node_Access) return Boolean;

   package Error_Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor      => Error_Ref,
      Has_Element => Has_Error);

   function Error_Iterate
     (Tree     : aliased in Syntax_Trees.Tree)
     return Error_Iterator_Interfaces.Forward_Iterator'Class;
   --  Iterates over errors.

   package Stream_Error_Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor      => Stream_Error_Cursor,
      Has_Element => Has_Error);

   function Stream_Error_Iterate
     (Tree   : aliased in Syntax_Trees.Tree;
      Stream :         in Stream_ID)
     return Stream_Error_Iterator_Interfaces.Forward_Iterator'Class;
   --  Iterates over errors in Stream.

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
     (Tree          : in Syntax_Trees.Tree;
      Stream        : in Stream_ID;
      Stack         : in Boolean                   := True;
      Input         : in Boolean                   := True;
      Shared        : in Boolean                   := False;
      Children      : in Boolean                   := False;
      Node_Numbers  : in Boolean                   := True;
      Non_Grammar   : in Boolean                   := False;
      Augmented     : in Boolean                   := False;
      Line_Numbers  : in Boolean                   := False;
      Image_Action  : in Syntax_Trees.Image_Action := null;
      State_Numbers : in Boolean                   := True)
     return String;
   --  Image of each node. If Stack, includes stack; if Input, includes
   --  input; if Shared, includes continuation in Shared_Stream. If
   --  Children, each entire subtree is included, with newlines, as in
   --  Print_Tree.

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      State                 : in Boolean                   := False;
      Children              : in Boolean                   := False;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Expecting             : in Boolean                   := False;
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
      Expecting             : in Boolean                   := False;
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

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Ref                   : in Stream_Node_Ref;
      First_Terminal        : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Expecting             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String;
   --  If First_Terminal, show First_Terminal of Ref.Node if Ref is rooted.

   function Image
     (Tree : in Syntax_Trees.Tree;
      List : in Valid_Node_Access_Lists.List)
     return String;

   function Decimal_Image is new SAL.Generic_Decimal_Image (Node_Index);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Node_Index);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Base_Sequential_Index);

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

   function Get_Sequential_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Base_Sequential_Index
   with Pre => Node = Invalid_Node_Access or else Tree.Label (Node) in Terminal_Label;
   --  For convenience, returns Invalid_Sequential_Index if Node =
   --  Invalid_Node_Access.

   procedure Set_Sequential_Index
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Index : in Base_Sequential_Index)
   with Pre => Tree.Label (Node) in Terminal_Label;

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result
   with Pre => Left /= Invalid_Node_Access and Right /= Invalid_Node_Access;
   --  Only valid on a batch parsed tree, where Node_Index is ordered and
   --  unique.
   --
   --  Left, Right can't be Valid_Node access because
   --  SAL.Gen_Unbounded_Sparse_Ordered_Sets requires a valid default
   --  initialization.

   package Node_Sets is new SAL.Gen_Unbounded_Sparse_Ordered_Sets (Node_Access, Node_Access_Compare);

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Access;
      Message : in String)
     return String
   with Pre => Tree.Parents_Set;
   --  File_Name from Tree.Lexer, line, column from Node

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Ref     : in Stream_Node_Ref;
      Message : in String)
     return String;
   --  File_Name from Tree.Lexer, line, column from Node

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
     (Tree             : in out Syntax_Trees.Tree;
      User_Data        : in out User_Data_Type'Class;
      Error_Reported   : in out Node_Sets.Set;
      Node_Index_Order : in     Boolean;
      Root             : in     Node_Access                := Invalid_Node_Access;
      Validate_Node    : in     Syntax_Trees.Validate_Node := null);
   --  Verify that no children are Invalid_Node_Access. Verify
   --  child/parent links. If Node_Index_Order, verify that
   --  Node.Node_Index > Node.Children.Node_Index (which is true in a
   --  batch parse tree). Call Validate_Node for each visited node.
   --  Violations output a message to Text_IO.Current_Error.
   --  Error_Reported is used to avoid outputing an error for a node more
   --  than once.

   procedure Sequential_Index_Cleared (Tree : in Syntax_Trees.Tree);
   --  Raises SAL.Programmer_Error if any node in Tree or Tree.Streams
   --  has Sequential_Index /= Invalid_Sequential_Index.

   procedure Print_Tree
     (Tree         : in     Syntax_Trees.Tree;
      Trace        : in out WisiToken.Trace'Class;
      Root         : in     Node_Access               := Invalid_Node_Access;
      Image_Action : in     Syntax_Trees.Image_Action := null;
      Line_Numbers : in     Boolean                   := False;
      Non_Grammar  : in     Boolean                   := False);
   --  Print tree rooted at Root (default Tree.Root) to
   --  Trace, for debugging.
   --
   --  This is the same as Trace.Put_Line (Tree.Image (..., Children =>
   --  True)), but avoids storing the entire trace image on the stack;
   --  required for large trees.

   procedure Print_Streams
     (Tree        : in     Syntax_Trees.Tree;
      Trace       : in out WisiToken.Trace'Class;
      Children    : in     Boolean := False;
      Non_Grammar : in     Boolean := False);

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String;
   --  For debugging; node counts.

private
   use all type Ada.Containers.Count_Type;
   use all type Valid_Node_Access_Lists.Cursor;

   type Error_List_Access is access all Error_Data_Lists.List;
   procedure Free is new Ada.Unchecked_Deallocation (Error_Data_Lists.List, Error_List_Access);

   type Node
     (Label       : Node_Label;
      Child_Count : SAL.Base_Peek_Type)
      --  Descriminants have no default because allocated nodes are
      --  constrained anyway (ARM 4.8 6/3).
   is record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Node_Index : Syntax_Trees.Node_Index := 0;
      --  Used only for debugging. If Terminal_Label, positive, and
      --  corresponds to text order after initial lex. If Nonterm, negative,
      --  arbitrary. After a batch parse, node indices are unique within the
      --  tree, but after incremental editing, they are reused because nodes
      --  created for unsuccessful parse streams are deleted.

      Parent : Node_Access := Invalid_Node_Access;

      Augmented : Augmented_Class_Access := null;

      Error_List : Error_List_Access;
      --  We store an access to an error list object in each node, rather
      --  than a list object, to reduce the size of a node; almost all nodes
      --  have no errors.

      case Label is
      when Terminal_Label =>
         Non_Grammar : aliased Lexer.Token_Arrays.Vector;
         --  Immediately following Node. In initial lex, this can only be in a
         --  Source_Terminal node. User Insert_Terminal can move it to a
         --  Virtual_Terminal node, editing the tree can copy it to a
         --  Virtual_Identifier node.
         --
         --  Not a pointer, because many nodes have non_grammar, and to
         --  simplify using Tree.Non_Grammar_Var.

         Sequential_Index : Syntax_Trees.Base_Sequential_Index := Invalid_Sequential_Index;

         case Label is
         when Source_Terminal =>
            Byte_Region : Buffer_Region := Null_Buffer_Region;
            Char_Region : Buffer_Region := Null_Buffer_Region;
            --  Data from lexer. We store the absolute buffer region here to avoid
            --  storing all whitespace in the tree. Edit_Tree shifts these for
            --  incremental parse. We don't store Line_Region here, to save space,
            --  to simplify Edit_Tree, and because it changes when Insert_Terminal
            --  moves Non_Grammar.

            Following_Deleted : aliased Valid_Node_Access_Lists.List;
            --  Nodes that follow this terminal that were deleted by error
            --  recovery.
            --  FIXME: change to ptr like error_list, for space saving?

         when Virtual_Terminal_Label =>
            Insert_Location : WisiToken.Insert_Location := Before_Next;
            --  Overridden Insert_Token can change the default.
            --  If the node has non_grammar tokens, Insert_Location must be
            --  Between.

            case Label is
            when Virtual_Terminal =>
               null;
            when Virtual_Identifier =>
               Identifier : Identifier_Index; -- index into user data
            when Source_Terminal | Nonterm =>
               null;
            end case;

         when Nonterm =>
            null;
         end case;

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by In_Parse_Actions and error recover, via
         --  Contains_Virtual_Terminal.

         RHS_Index : Natural;
         --  With ID, index into Productions.

         Action : Post_Parse_Action := null;

         Name_Offset : Base_Buffer_Pos := 0;
         Name_Length : Base_Buffer_Pos := 0;
         --  Name_* are set and checked by In_Parse_Actions. We use an offset
         --  from First_Terminal (Node).Byte_Region.First, rather than a
         --  Buffer_Region, to avoid needing to shift it during Edit_Tree for
         --  incremental parse. FIXME: generalize for other actions;
         --  post_parse_augmented, in_parse_augmented.

         Children : Node_Access_Array (1 .. Child_Count);
         --  We use an explicit array, rather than a pointer to the first
         --  child, to preserve child indices while editing the tree.
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

      Shared_Link : Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element;
      --  A complete parse stream consists of elements in
      --  Parse_Stream.Elements, followed by elements in
      --  Shared_Stream.Elements starting at Shared_Link, terminating in an
      --  EOI element. EOI is never shifted to the parse stream, but it can
      --  be copied to the parse stream to add an error. Then Shared_Link is
      --  No_Element.

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

   Invalid_Stream_Node_Parents : constant Stream_Node_Parents := (Invalid_Stream_Node_Ref, Parents => <>);

   package Node_Access_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node_Access, null);

   type Tree is new Base_Tree with record
      Next_Stream_Label : Stream_Label := Shared_Stream_Label + 1;

      Next_Terminal_Node_Index : Node_Index := 1;

      Root : Node_Access := Invalid_Node_Access;
      SOI  : Node_Access := Invalid_Node_Access;
      EOI  : Node_Access := Invalid_Node_Access;

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

   function Byte_Region (Tree : in Syntax_Trees.Tree; Index : in Stream_Index) return WisiToken.Buffer_Region
   is (Byte_Region (Tree, Stream_Element_Lists.Constant_Ref (Index.Cur).Node));

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   is (Node.Child_Count);

   function Cleared (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 0 and Tree.Nodes.Length = 0);

   function Contains
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Token  : in Stream_Index)
     return Boolean
   is ((Tree.Is_Valid (Stream) and Token /= Invalid_Stream_Index) and then
         (for some Cur in Tree.Streams (Stream.Cur).Elements.Iterate => Cur = Token.Cur));

   function Correct_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is (Ref = Invalid_Stream_Node_Ref or else
         (Ref.Element /= Invalid_Stream_Index and then
            (Ref.Node = Invalid_Node_Access or else
               (not Tree.Parents_Set or else
                  Tree.Subtree_Root (Ref.Node) = Tree.Get_Node (Ref.Stream, Ref.Element)))));

   function Editable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Parents_Set and Tree.Streams.Length = 0 and Tree.Shared_Stream.Cur = Parse_Stream_Lists.No_Element);

   function First_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is (Cur => Parse_Stream_Lists.Next (Tree.Shared_Stream.Cur));

   function Last_Parse_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is (Cur => Parse_Stream_Lists.Last (Tree.Streams));

   function Fully_Parsed (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 2 and then Tree.Stream_Length ((Cur => Tree.Streams.Last)) in 2 .. 3);
   --  1 stream for Shared, one for the successful parser. Parse stream
   --  has SOI with start state, parsed tree root, possibly copied EOI

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

   function ID
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Token_ID
   is (Stream_Element_Lists.Constant_Ref (Element.Cur).Node.ID);

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

   function Lexable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 1 and
         (Tree.Shared_Stream.Cur /= Parse_Stream_Lists.No_Element and then
            Tree.Streams (Tree.Shared_Stream.Cur).Elements.Length = 1));

   function Next_Stream_ID_Trimmed_Image (Tree : in Syntax_Trees.Tree) return String
   is (Trimmed_Image (Tree.Next_Stream_Label));

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Parents_Set);

   function Parents_Valid (Ref : in Stream_Node_Parents) return Boolean
   is ((Ref.Ref.Element = Invalid_Stream_Index and Ref.Ref.Node = Invalid_Node_Access) or else
         ((Stream_Element_Lists.Constant_Ref (Ref.Ref.Element.Cur).Node = Ref.Ref.Node or
             Ref.Ref.Node = Invalid_Node_Access) and Ref.Parents.Is_Empty) or else
         (Ref.Parents.Depth > 0 and then
            (for all Item of Ref.Parents => Item /= Invalid_Node_Access and then Item.Label = Nonterm) and then
            (Ref.Parents.Peek (Ref.Parents.Depth) = Stream_Element_Lists.Constant_Ref (Ref.Ref.Element.Cur).Node and
               --  we don't check the intervening parent items.
               (for some Child of Ref.Parents.Peek.Children => Child = Ref.Ref.Node))));

   function Parseable (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Streams.Length = 1);

   function Rooted (Ref : in Stream_Node_Ref) return Boolean
   is (Stream_Element_Lists.Has_Element (Ref.Element.Cur) and then
         Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node = Ref.Node);

   function Stack_Top
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
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

   function Stream_Last
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   is ((Cur => Tree.Streams (Stream.Cur).Elements.Last));

   function Stream_Prev
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is ((Cur => Stream_Element_Lists.Previous (Element.Cur)));

   function Shared_Stream (Tree : in Syntax_Trees.Tree) return Stream_ID
   is (Tree.Shared_Stream);

   function Single_Terminal (Ref : in Stream_Node_Ref) return Boolean
   is (Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node = Ref.Node and Ref.Node.Label in Terminal_Label);

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

   function Valid_Error_Ref (Error : in Error_Ref) return Boolean
   is (Error.Node = Invalid_Node_Access or else
         (Error.Node.Error_List /= null or
            (Error.Node.Label = Source_Terminal and then
               (for some Cur in Error.Node.Following_Deleted.Iterate => Cur = Error.Deleted))));

   function Valid_Error_Ref (Error : in Stream_Error_Ref) return Boolean
   is (Error.Ref.Ref.Node = Invalid_Node_Access or else
         (Error.Ref.Ref.Node.Error_List /= null or
            (Error.Ref.Ref.Node.Label = Source_Terminal and then
               (for some Cur in Error.Ref.Ref.Node.Following_Deleted.Iterate => Cur = Error.Deleted))));

   function Valid_Root (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Root /= Invalid_Node_Access or Tree.Stream_Count > 0);

   function Valid_Stream_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Boolean
   is (Tree.Contains (Ref.Stream, Ref.Element) and then
         (if Ref.Node = Invalid_Node_Access
          then Ref /= Invalid_Stream_Node_Ref
          else
            (not Tree.Parents_Set or else
               Tree.Is_Descendant_Of (Root => Tree.Get_Node (Ref.Stream, Ref.Element), Descendant => Ref.Node))));

   Dummy_Node : constant Node_Access := new Node'(Label => Virtual_Identifier, Child_Count => 0, others => <>);

   type Token_Array_Var_Ref (Element : not null access WisiToken.Lexer.Token_Arrays.Vector) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Token_Array_Const_Ref (Element : not null access constant WisiToken.Lexer.Token_Arrays.Vector) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Valid_Node_Access_List_Var_Ref (List : not null access Valid_Node_Access_Lists.List) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   function Node_Image
     (Node : in Node_Access;
      Tree : in Syntax_Trees.Tree'Class)
     return String
   is (Tree.Image (Node, Node_Numbers => True));

   function Node_List_Image is new Valid_Node_Access_Lists.Gen_Image_Aux (Tree'Class, Node_Image);

   function Image
     (Tree : in Syntax_Trees.Tree;
      List : in Valid_Node_Access_Lists.List)
     return String
   is (Node_List_Image (List, Tree));

   ----------
   --  Errors

   type Error_Data_List_Ref (List : not null access constant Error_Data_Lists.List) is record
      Dummy : Integer := raise Program_Error;
   end record;

   function Has_Error (Node : in Valid_Node_Access) return Boolean
   is (Node.Error_List /= null);

   Empty_Error_List : aliased constant Error_Data_Lists.List := Error_Data_Lists.Empty_List;
   --  WORKAROUND: with GNAT Community 2021, adding 'aliased' in
   --  sal-gen_indefinite_doubly_linked_lists.ads doesn't work.

   function Error_List (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Error_Data_List_Ref
   is (Error_Data_List_Ref'
         (List  => (if Node.Error_List = null then Empty_Error_List'Access else Node.Error_List),
          Dummy => 1));

   type Error_Data_List_Cursor is record
      Cur : Error_Data_Lists.Cursor;
   end record;

   type Error_Constant_Reference_Type (Element : not null access constant Error_Data'Class) is record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Error_Iterator (Tree : not null access constant Syntax_Trees.Tree)
     is new Error_Iterator_Interfaces.Forward_Iterator with
     null record;

   overriding function First (Object : Error_Iterator) return Error_Ref;

   overriding function Next
     (Object   : Error_Iterator;
      Position : Error_Ref)
     return Error_Ref;

   type Stream_Error_Cursor is record
      SER : Stream_Error_Ref;
   end record;

   Invalid_Stream_Error_Ref : constant Stream_Error_Ref :=
     (Invalid_Stream_Node_Parents, Valid_Node_Access_Lists.No_Element, Error_Data_Lists.No_Element);

   function Error (Item : in Stream_Error_Cursor) return Stream_Error_Ref
   is (Item.SER);

   type Stream_Error_Iterator (Tree : not null access constant Syntax_Trees.Tree)
   is new Stream_Error_Iterator_Interfaces.Forward_Iterator with record
      Stream : Parse_Stream_Lists.Cursor;
   end record;

   overriding function First (Object : Stream_Error_Iterator) return Stream_Error_Cursor;

   overriding function Next
     (Object   : Stream_Error_Iterator;
      Position : Stream_Error_Cursor)
     return Stream_Error_Cursor;

end WisiToken.Syntax_Trees;
