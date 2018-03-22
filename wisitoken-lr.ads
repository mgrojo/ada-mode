--  Abstract :
--
--  Root package of an implementation of an LR (Left-to-right scanning
--  Rightmost-deriving) parser for grammars defined by a production
--  list. It contains types shared by the parse table generators and
--  the parser.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  [info] Docs/wisi-user_guide.texinfo
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 - 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Image_Aux;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Queues.Gen_Image_Aux;
with SAL.Gen_Unbounded_Definite_Stacks.Gen_Image_Aux;
with WisiToken.Lexer;
with WisiToken.Semantic_Checks;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees.Branched;
package WisiToken.LR is

   --  Parser stack type. FIXME: move to parser?
   type Parser_Stack_Item is record
      State : Unknown_State_Index     := Unknown_State;
      Token : Syntax_Trees.Node_Index := Syntax_Trees.Invalid_Node_Index;
   end record;

   package Parser_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Parser_Stack_Item);

   function Image
     (Stack      : in Parser_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor'Class;
      Tree       : in Syntax_Trees.Abstract_Tree'Class;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String;
   --  If Depth = 0, put all of Stack. Otherwise put Min (Depth,
   --  Stack.Depth) items.

   ----------
   --  Following are the types used in the parse table. The parse
   --  table is an array indexed by parse state that where each state
   --  contains a list of parse actions and a list of gotos.
   --
   --  Parse actions are indexed by the terminal they match and are either
   --    o Shift and change to a designated state.
   --    o Reduce by the given production
   --
   --  Gotos are indexed by the nonterminal they match and designate
   --  the state the parser need to change to.

   type All_Parse_Action_Verbs is (Shift_Recover, Shift, Reduce, Accept_It, Error);
   subtype Parse_Action_Verbs is All_Parse_Action_Verbs range Shift .. Error;
   --  Shift_Recover is only used for error recovery.

   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         State : State_Index := State_Index'Last;

      when Reduce | Accept_It =>
         LHS         : Token_ID                                 := Invalid_Token_ID;
         Action      : WisiToken.Syntax_Trees.Semantic_Action   := null;
         Check       : WisiToken.Semantic_Checks.Semantic_Check := null;
         Token_Count : Ada.Containers.Count_Type                := 0;

         Production : Natural := 0;
         --  Index into Parse_Table.Productions, for McKenzie_Recover.

         Name_Index : Natural := 0;
         --  Index of production among productions for a nonterminal,
         --  for generating action names

      when Error =>
         null;
      end case;
   end record;
   subtype Shift_Action_Rec is Parse_Action_Rec (Shift);
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor'Class) return String;
   --  Ada aggregate syntax, leaving out Semantic_Action in reduce.

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec);

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Parse_Action_Node, Parse_Action_Node_Ptr);

   type Action_Node;
   type Action_Node_Ptr is access Action_Node;

   type Action_Node is record
      Symbol : Token_ID; -- ignored if Action is Error
      Action : Parse_Action_Node_Ptr;
      Next   : Action_Node_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Action_Node, Action_Node_Ptr);

   type Goto_Node is private;
   type Goto_Node_Ptr is access Goto_Node;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID;
   function State (List : in Goto_Node_Ptr) return State_Index;
   function Next (List : in Goto_Node_Ptr) return Goto_Node_Ptr;

   type Parse_State is record
      Action_List : Action_Node_Ptr;
      Goto_List   : Goto_Node_Ptr;
   end record;

   type Parse_State_Array is array (State_Index range <>) of Parse_State;

   type Action_List_Iterator is tagged private;
   --  Iterates over all shift/reduce actions for a state, including
   --  conflicts.

   function First (State : in Parse_State) return Action_List_Iterator;
   function Is_Done (Iter : in Action_List_Iterator) return Boolean;
   procedure Next (Iter : in out Action_List_Iterator);

   function Symbol (Iter : in Action_List_Iterator) return Token_ID;
   function Action (Iter : in Action_List_Iterator) return Parse_Action_Rec;

   type Goto_List_Iterator is tagged private;
   --  Iterates over all gotos for a state.

   function First (State : in Parse_State) return Goto_List_Iterator;
   function Is_Done (Iter : in Goto_List_Iterator) return Boolean;
   procedure Next (Iter : in out Goto_List_Iterator);

   function Symbol (Iter : in Goto_List_Iterator) return Token_ID;
   function State (Iter : in Goto_List_Iterator) return State_Index;

   ----------
   --  Run-time parse table construction

   procedure Add_Action
     (State       : in out Parse_State;
      Symbol      : in     Token_ID;
      State_Index : in     WisiToken.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     Parse_Action_Verbs;
      Production      : in     Positive;
      LHS_ID          : in     Token_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Name_Index      : in     Natural;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Reduce or Accept_It action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      State_Index     : in     WisiToken.State_Index;
      Production      : in     Positive;
      LHS_ID          : in     Token_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Name_Index      : in     Natural;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Shift/Reduce conflict to State.

   procedure Add_Action
     (State             : in out Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     Parse_Action_Verbs;
      Production_1      : in     Positive;
      LHS_ID_1          : in     Token_ID;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Name_Index_1      : in     Natural;
      Semantic_Action_1 : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check_1  : in     WisiToken.Semantic_Checks.Semantic_Check;
      Production_2      : in     Positive;
      LHS_ID_2          : in     Token_ID;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Name_Index_2      : in     Natural;
      Semantic_Action_2 : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check_2  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add an Accept/Reduce or Reduce/Reduce conflict action to State.

   procedure Add_Error (State  : in out Parse_State);
   --  Add an Error action to State, at tail of action list.

   procedure Add_Goto
     (State    : in out Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     State_Index);
   --  Add a Goto to State; keep goto list sorted in ascending order on Symbol.

   --  FIXME: delete Pattern, or put back in McKenzie_Recover

   type Pattern is abstract tagged null record;
   --  We don't declare a dispatching operation to implement Pattern
   --  here, because the required types are not visible. See
   --  wisitoken-lr-mckenzie_recover.adb

   function Image (Item : in Pattern) return String is abstract;
   --  Return image of Item, using Token_ID'Image for any Token_IDs,
   --  in Ada aggregate syntax. Used in generated Ada code.

   package Patterns is new Standard.Ada.Containers.Indefinite_Doubly_Linked_Lists (Pattern'Class);

   type Recover_Pattern_1 is new Pattern with record
      --  See [info] node Error Recovery item recover_pattern_1
      Stack     : Token_ID;
      Error     : Token_ID;
      Expecting : Token_ID;
   end record;

   overriding function Image (Item : in Recover_Pattern_1) return String;

   type Recover_Pattern_2 is new Pattern with record
      --  See [info] node Error Recovery item recover_pattern_2
      Stack     : Token_ID;
      Error     : Token_ID;
      Expecting : Token_ID;
      Insert    : Token_ID;
   end record;

   overriding function Image (Item : in Recover_Pattern_2) return String;

   type Recover_End_EOF is new Pattern with record
      --  See [info] node Error Recovery
      Error       : Token_ID;
      Delete_Thru : Token_ID;
   end record;

   overriding function Image (Item : in Recover_End_EOF) return String;

   type Recover_Block_Mismatched_Names is new Pattern with record
      --  See [info] node Error Recovery
      Begin_ID     : Token_ID;
      End_ID       : Token_ID;
      Name_ID      : Token_ID;
      Semicolon_ID : Token_ID;
   end record;

   overriding function Image (Item : in Recover_Block_Mismatched_Names) return String;

   type McKenzie_Param_Type
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
   is record
      Insert      : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      Delete      : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      Push_Back   : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      Undo_Reduce : Token_ID_Array_Natural (First_Nonterminal .. Last_Nonterminal);
      --  Cost of operations on config stack, input.

      Cost_Limit  : Natural; -- max cost of configurations to look at
      Check_Limit : Natural; -- max tokens to parse ahead when checking a configuration.

      --  For special rules
      Patterns : LR.Patterns.List;
   end record;

   Default_McKenzie_Param : constant McKenzie_Param_Type :=
     (First_Terminal    => Token_ID'Last,
      Last_Terminal     => Token_ID'First,
      First_Nonterminal => Token_ID'Last,
      Last_Nonterminal  => Token_ID'First,
      Insert            => (others => 0),
      Delete            => (others => 0),
      Push_Back         => (others => 0),
      Undo_Reduce       => (others => 0),
      Cost_Limit        => Natural'Last,
      Check_Limit       => Natural'Last,
      Patterns          => LR.Patterns.Empty_List);

   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor'Class);
   --  Put Item to Ada.Text_IO.Current_Output

   package Production_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive, Token_ID_Arrays.Vector);

   package Token_Sequence_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Token_ID, Token_ID_Arrays.Vector);

   procedure Set_Token_Sequence (Vector : in out Token_ID_Arrays.Vector; Tokens : in Token_ID_Array);

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
     is
   record
      States             : Parse_State_Array (State_First .. State_Last);
      McKenzie_Param     : McKenzie_Param_Type (First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
      Productions        : Production_Arrays.Vector;     -- Indexed by Production.Index
      Terminal_Sequences : Token_Sequence_Arrays.Vector; -- Indexed by nonterminal Token_ID
   end record;

   type Parse_Table_Ptr is access Parse_Table;
   procedure Free is new Ada.Unchecked_Deallocation (Parse_Table, Parse_Table_Ptr);

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index;
   --  Return next state after reducing stack by nonterminal ID;
   --  Unknown_State if none (only possible during error recovery).

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr;
   --  Return the action for State, terminal ID.

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set;

   function Minimal_Terminal_Sequence
     (Table   : in Parse_Table;
      Nonterm : in Token_ID)
     return Token_ID_Arrays.Vector;
   --  Return the minimal terminal sequence for Nonterm; this can be
   --  inserted as virtual terminals in the input stream when
   --  McKenzie_Recover inserts a nonterm.

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; State : in Parse_State);

   ----------
   --  For McKenzie_Recover. Declared here because Parser_Lists needs
   --  these, Mckenzie_Recover needs Parser_Lists.
   --
   --  We don't maintain a syntax tree during recover; it's too slow, and
   --  not needed for any operations. The parser syntax tree is used for
   --  Undo_Reduce, which is only done on nonterms reduced by the main
   --  parser, not virtual nonterms produced by recover.

   package Fast_Token_ID_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (SAL.Peek_Type, Token_ID, Capacity => 20);

   No_Inserted : constant SAL.Base_Peek_Type := 0;

   function Image
     (Index      : in SAL.Peek_Type;
      Tokens     : in Fast_Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
     is (SAL.Peek_Type'Image (Index) & ":" & SAL.Peek_Type'Image (Tokens.Last_Index) & ":" &
           Image (Tokens (Index), Descriptor));

   type Config_Op_Label is (Fast_Forward, Undo_Reduce, Push_Back, Insert, Delete);
   --  Fast_Forward is a placeholder to mark a fast_forward parse; that
   --  resets what operations are allowed to be done on a config.
   --
   --  Undo_Reduce is the inverse of Reduce.
   --
   --  Push_Back pops the top stack item, and moves the input stream
   --  pointer back to the first shared_terminal contained by that item.
   --
   --  Insert inserts a new token in the token input stream, before the
   --  given point in Terminals. If ID is a nonterm, the minimal terminal token
   --  sequence (from Table.Terminal_Sequences) for that nonterm is
   --  inserted.
   --
   --  Delete deletes one item from the token input stream, at the given
   --  point.

   type Config_Op (Op : Config_Op_Label := Fast_Forward) is record
      --  We store enough information to perform the operation on the main
      --  parser stack and input stream point when the config is the result
      --  of a successful recover.
      --
      --  After a recover, the main parser must reparse any inserted tokens,
      --  and skip any deleted tokens. Therefore, when all the recover ops
      --  are applied, the main parser stack will be the same or shorter
      --  than it was, so we only need to store token counts for stack
      --  operations (Unknown_State is pushed when a state is needed; none
      --  will be left on the main stack). We also store IDs, so we can
      --  check that everything is in sync, and for debugging.

      ID : Token_ID;
      --  For Fast_Forward, ID is EOF.
      --  For Undo_Reduce | Push_Back, ID is the nonterm ID popped off the stack.
      --  For Insert | Delete, ID is the token inserted or deleted.

      case Op is
      when Fast_Forward =>
         null;

      when Undo_Reduce =>
         Token_Count : Ada.Containers.Count_Type;
         --  The number of tokens pushed on the stack.
         --  ID is the nonterminal.

      when Push_Back | Insert | Delete =>
         Token_Index : WisiToken.Token_Index;
         --  The position in the input stream after the operation is done.
         --  Multiple tokens may be pushed/inserted/deleted in one operation;
         --  ID is the first of those.

      end case;
   end record;

   package Config_Op_Queues is new SAL.Gen_Unbounded_Definite_Queues (Config_Op);

   package Config_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Config_Op, Capacity => 80);
   --  Using a fixed size vector significantly speeds up
   --  McKenzie_Recover. The capacity is determined by the maximum number
   --  of repair operations, which is limited by the cost_limit McKenzie
   --  parameter plus an arbitrary number from the language-specific
   --  repairs; in practice, a capacity of 80 is enough so far.

   function Image (Item : in Config_Op; Descriptor : in WisiToken.Descriptor) return String
     is ("(" & Config_Op_Label'Image (Item.Op) & ", " & Image (Item.ID, Descriptor) &
           (case Item.Op is
            when Fast_Forward => "",
            when Undo_Reduce => "," & Ada.Containers.Count_Type'Image (Item.Token_Count),
            when Push_Back | Insert | Delete => "," & WisiToken.Token_Index'Image (Item.Token_Index))
           & ")");

   function Image is new Config_Op_Queues.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Image is new Config_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);

   function None (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is (for all O of Ops => O.Op /= Op);
   --  True if Ops contains no Op.

   function None_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains no Op after the last Fast_Forard (or ops.first, if
   --  no Fast_Forward).

   function Any (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is (for some O of Ops => O.Op = Op);
   --  True if Ops contains at least one Op.

   type Recover_Stack_Item is record
      State      : Unknown_State_Index;
      Tree_Index : Syntax_Trees.Node_Index;
      Token      : Recover_Token;
   end record;

   package Recover_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Recover_Stack_Item);

   function Image (Item : in Recover_Stack_Item; Descriptor : in WisiToken.Descriptor'Class) return String
     is (Image (Item.State) & " : " & Image (Item.Token, Descriptor));

   function Image is new Recover_Stacks.Gen_Image_Aux (WisiToken.Descriptor'Class, Image);

   type Configuration is record
      Stack : Recover_Stacks.Stack;
      --  Initially built from the parser stack, then the stack after the
      --  Ops below have been performed.

      Current_Shared_Token : Token_Index := Token_Index'Last;
      --  Index into Shared_Parser.Terminals for current input token, after
      --  all of Inserted is input. Initially the error token.

      Inserted         : Fast_Token_ID_Arrays.Vector;
      Current_Inserted : SAL.Base_Peek_Type := No_Inserted;
      --  Index of current input token in Inserted. If No_Index, use
      --  Current_Shared_Token.

      Check_Action : Reduce_Action_Rec;
      Check_Status : Semantic_Checks.Check_Status;
      --  If parsing this config ended on a semantic check fail,
      --  Check_Action caused the fail, and Check_Status is the error.

      Ops              : Config_Op_Arrays.Vector;
      Ops_Insert_Point : SAL.Base_Peek_Type := Config_Op_Arrays.No_Index;
      --  If Ops_Insert_Point is not No_Index, fast_forward failed partway
      --  thru Ops, and we are trying to find a fix at that point.

      Cost : Natural := 0;
   end record;
   type Configuration_Access is access all Configuration;
   for Configuration_Access'Storage_Size use 0;

   function Key (A : in Configuration) return Integer is (A.Cost);

   procedure Set_Key (Item : in out Configuration; Key : in Integer);

   package Config_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci
     (Element_Type   => Configuration,
      Element_Access => Configuration_Access,
      Key_Type       => Integer,
      Key            => Key,
      Set_Key        => Set_Key);

   type Semantic_Check_Fixes_Access is access function
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : in     WisiToken.Lexer.Handle;
      Parser_Label      : in     Natural;
      McKenzie_Param    : in     McKenzie_Param_Type;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Branched.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration;
      Nonterm           : in     Recover_Token)
     return Boolean;
   --  A reduce to Nonterm by Action on Config failed a semantic check
   --  returning Status. Config.Stack is in the pre-reduce state. Add to
   --  Local_Config_Heap language-specific fixes for the failure. Return
   --  True if ignoring the error is a viable solution, False otherwise.
   --  Called from McKenzie_Recover.

   type McKenzie_Data is tagged record
      Config_Heap   : Config_Heaps.Heap_Type;
      Enqueue_Count : Integer := 0;
      Check_Count   : Integer := 0;
      Results       : Config_Heaps.Heap_Type;
      Success       : Boolean := False;
   end record;

   type McKenzie_Access is access all McKenzie_Data;

   type Parse_Error_Label is (Action, Check);

   type Parse_Error
     (Label          : Parse_Error_Label;
      First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is record
      Recover : Configuration;

      case Label is
      when Action =>
         Error_Token : Syntax_Trees.Valid_Node_Index; -- index into Parser.Tree
         Expecting   : Token_ID_Set (First_Terminal .. Last_Terminal);

      when Check =>
         Check_Status : Semantic_Checks.Check_Status;
      end case;
   end record;

   package Parse_Error_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Parse_Error);

   procedure Put
     (Source_File_Name : in String;
      Errors           : in Parse_Error_Lists.List;
      Tree             : in Syntax_Trees.Abstract_Tree'Class;
      Descriptor       : in WisiToken.Descriptor);
   --  Put user-friendly error messages to Ada.Text_IO.Current_Output.

private

   --  Private to enforce use of Add; doesn't succeed, since only
   --  children use it.
   type Goto_Node is record
      Symbol : Token_ID;
      State  : State_Index;
      Next   : Goto_Node_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Goto_Node, Goto_Node_Ptr);

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec);
   --  Add action to List, sorted on ascending Symbol.

   type Action_List_Iterator is tagged record
      Node : Action_Node_Ptr;
      Item : Parse_Action_Node_Ptr;
   end record;

   type Goto_List_Iterator is tagged record
      Node : Goto_Node_Ptr;
   end record;

   function Next_Grammar_Token
     (Terminals      : in out          Base_Token_Arrays.Vector;
      Lexer          : not null access WisiToken.Lexer.Instance'Class;
      Semantic_State : in out          WisiToken.Semantic_State.Semantic_State;
      Descriptor     : in              WisiToken.Descriptor'Class)
     return Token_Index;
   --  Get next token from Lexer, call Semantic_State.Lexer_To_Augmented.
   --  If it is a grammar token, store in Terminals and return its ID.
   --  Otherwise, repeat.

   function Reduce_Stack
     (Stack        : in out Parser_Stacks.Stack;
      Tree         : in out Syntax_Trees.Branched.Tree;
      Action       : in     Reduce_Action_Rec;
      Nonterm      :    out Syntax_Trees.Valid_Node_Index;
      Lexer        : in     WisiToken.Lexer.Handle;
      Trace        : in out WisiToken.Trace'Class;
      Trace_Level  : in     Integer;
      Trace_Prefix : in     String := "")
     return WisiToken.Semantic_Checks.Check_Status;
   --  Reduce Stack according to Action, calling Action.Check and
   --  returning result, or Ok if null.

end WisiToken.LR;
