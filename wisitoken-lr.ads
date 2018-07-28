--  Abstract :
--
--  Root package of an implementation of an LR (Left-to-right scanning
--  Rightmost-deriving) parser for grammars defined by a production
--  list. It contains types shared by the parse table generators and
--  the parser.
--
--  References :
--
--  See wisitoken.ads
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
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Image;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Queues.Gen_Image_Aux;
with SAL.Gen_Unbounded_Definite_Stacks.Gen_Image_Aux;
with System.Multiprocessors;
with WisiToken.Productions;
with WisiToken.Semantic_Checks;
with WisiToken.Syntax_Trees;
package WisiToken.LR is

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

   type All_Parse_Action_Verbs is (Pause, Shift_Recover, Shift, Reduce, Accept_It, Error);
   subtype Parse_Action_Verbs is All_Parse_Action_Verbs range Shift .. Error;
   subtype Minimal_Verbs is All_Parse_Action_Verbs range Shift .. Reduce;
   --  Pause, Shift_Recover are only used for error recovery.

   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         Productions : Production_ID_Arrays.Vector;
         --  Index into Parse_Table.Productions, for McKenzie_Recover. A Shift
         --  action for the same token may occur in several productions in one
         --  state (ie push the first token in a statement that has several
         --  variants). FIXME: still needed?

         State : State_Index := State_Index'Last;

      when Reduce | Accept_It =>
         Production : Production_ID;
         --  Index into Parse_Table.Productions, and the result nonterm. FIXME: still needed? put back nonterm here.

         --  FIXME: use Action, Check in table.productions
         Action      : WisiToken.Syntax_Trees.Semantic_Action   := null;
         Check       : WisiToken.Semantic_Checks.Semantic_Check := null;
         Token_Count : Ada.Containers.Count_Type                := 0;

      when Error =>
         null;
      end case;
   end record;
   subtype Shift_Action_Rec is Parse_Action_Rec (Shift);
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor) return String;
   --  Ada aggregate syntax, leaving out Action, Check in reduce; for debug output

   function Equal (Left, Right : in Parse_Action_Rec) return Boolean;
   --  Ignore Action, Check.

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec);
   --  Put a line for Item in parse table output.

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
      Symbol : Token_ID := Invalid_Token_ID; -- ignored if Action is Error
      Action : Parse_Action_Node_Ptr;
      Next   : Action_Node_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Action_Node, Action_Node_Ptr);

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec);
   --  Add action to List, sorted on ascending Symbol.

   type Goto_Node is private;
   type Goto_Node_Ptr is access Goto_Node;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID;
   function State (List : in Goto_Node_Ptr) return State_Index;
   function Next (List : in Goto_Node_Ptr) return Goto_Node_Ptr;

   type Minimal_Action (Verb : Minimal_Verbs := Shift) is record
      case Verb is
      when Shift =>
         ID    : Token_ID;
         State : State_Index;

      when Reduce =>
         Nonterm     : Token_ID;
         Token_Count : Ada.Containers.Count_Type;
      end case;
   end record;

   function Compare_Minimal_Action (Left, Right : in Minimal_Action) return SAL.Compare_Result;

   type Minimal_Action_Array is array (Positive range <>) of Minimal_Action;

   package Minimal_Action_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted
     (Minimal_Action, Compare_Minimal_Action);

   function Strict_Image (Item : in Minimal_Action) return String;
   --  Strict Ada aggregate syntax, for generated code.

   function Image is new Minimal_Action_Lists.Gen_Image (Strict_Image);

   procedure Set_Minimal_Action (List : out Minimal_Action_Lists.List; Actions : in Minimal_Action_Array);

   type Parse_State is record
      Productions : Production_ID_Arrays.Vector;
      --  Used in parse and error recovery. FIXME: still true?
      Action_List : Action_Node_Ptr;
      Goto_List   : Goto_Node_Ptr;

      Minimal_Complete_Actions : Minimal_Action_Lists.List;
      --  Set of parse actions that will most quickly complete the
      --  productions in this state; used in error recovery
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
      Productions : in     Production_ID_Array;
      Symbol      : in     Token_ID;
      State_Index : in     WisiToken.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     Parse_Action_Verbs;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Reduce or Accept_It action to tail of State action list.

   function Duplicate_Reduce (State : in Parse_State) return Boolean;
   --  True if all actions are the same reduce; can use Add_Action (symbols).

   function Actions_Length (State : in Parse_State) return Integer;
   --  Not including Error.

   function Symbols_Image (State : in Parse_State) return String;
   --  Return image of symbols in State actions (assumed to be a
   --  Duplicate_Reduce state), in Ada aggregate syntax.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbols         : in     Token_ID_Array;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add duplicate Reduce actions, and final Error action, to tail of
   --  State action list.

   procedure Add_Action
     (State             : in out Parse_State;
      Shift_Productions : in     Production_ID_Array;
      Symbol            : in     Token_ID;
      State_Index       : in     WisiToken.State_Index;
      Reduce_Production : in     Production_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type;
      Semantic_Action   : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check    : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Shift/Reduce conflict to State.

   procedure Add_Action
     (State             : in out Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     Parse_Action_Verbs;
      Production_1      : in     Production_ID;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check_1  : in     WisiToken.Semantic_Checks.Semantic_Check;
      Production_2      : in     Production_ID;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check_2  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add an Accept/Reduce or Reduce/Reduce conflict action to State.

   procedure Add_Error (State  : in out Parse_State);
   --  Add an Error action to State, at tail of action list.

   procedure Add_Goto
     (State    : in out Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     State_Index);
   --  Add a goto item to State goto list; keep goto list sorted in ascending order on Symbol.

   type McKenzie_Param_Type
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
   is record
      Insert    : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Delete    : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Push_Back : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      --  Cost of operations on config stack, input.

      Task_Count : System.Multiprocessors.CPU_Range;
      --  Number of parallel tasks during recovery. If 0, use
      --  System.Multiprocessors.Number_Of_CPUs - 1.

      Cost_Limit        : Natural;     -- max cost of configurations to look at
      Check_Limit       : Token_Index; -- max tokens to parse ahead when checking a configuration.
      Check_Delta_Limit : Natural;     -- max configs checked, delta over successful parser.
      Enqueue_Limit     : Natural;     -- max configs enqueued.
   end record;

   Default_McKenzie_Param : constant McKenzie_Param_Type :=
     (First_Terminal    => Token_ID'Last,
      Last_Terminal     => Token_ID'First,
      First_Nonterminal => Token_ID'Last,
      Last_Nonterminal  => Token_ID'First,
      Insert            => (others => 0),
      Delete            => (others => 0),
      Push_Back         => (others => 0),
      Task_Count        => System.Multiprocessors.CPU_Range'Last,
      Cost_Limit        => Natural'Last,
      Check_Limit       => Token_Index'Last,
      Check_Delta_Limit => Natural'Last,
      Enqueue_Limit     => Natural'Last);

   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor);
   --  Put Item to Ada.Text_IO.Current_Output

   procedure Set_Token_Sequence (Vector : in out Token_ID_Arrays.Vector; Tokens : in Token_ID_Array);

   procedure Set_Production
     (Prod     : in out Productions.Instance;
      LHS      : in     Token_ID;
      RHS_Last : in     Natural);

   procedure Set_RHS
     (Prod      : in out Productions.Instance;
      RHS_Index : in     Natural;
      Tokens    : in     Token_ID_Array;
      Action    : in     WisiToken.Syntax_Trees.Semantic_Action   := null;
      Check     : in     WisiToken.Semantic_Checks.Semantic_Check := null);

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
     is tagged
   record
      States         : Parse_State_Array (State_First .. State_Last);
      McKenzie_Param : McKenzie_Param_Type (First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
      Productions    : WisiToken.Productions.Prod_Arrays.Vector;
   end record;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index;
   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Goto_Node_Ptr;
   --  Return next state after reducing stack by nonterminal ID;
   --  Unknown_State if none (only possible during error recovery).
   --  Second form allows retrieving Production.

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr;
   --  Return the action for State, terminal ID.

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set;

   type Reduce_Action_Array is array (Positive range <>) of Reduce_Action_Rec;
   function Reductions
     (Table       : in     Parse_Table;
      State       : in     State_Index;
      Shift_Count :    out Natural)
     return Reduce_Action_Array;
   --   FIXME: used?

   type Parse_Table_Ptr is access Parse_Table;
   procedure Free_Table (Table : in out Parse_Table_Ptr);

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor; State : in Parse_State);
   --  In human-readable parse_table format

   function Get_Action
     (Prod        : in Production_ID;
      Productions : in WisiToken.Productions.Prod_Arrays.Vector)
     return WisiToken.Syntax_Trees.Semantic_Action;

   function Get_Check
     (Prod        : in Production_ID;
      Productions : in WisiToken.Productions.Prod_Arrays.Vector)
     return WisiToken.Semantic_Checks.Semantic_Check;

   procedure Put_Text_Rep
     (Table        : in Parse_Table;
      File_Name    : in String;
      Action_Names : in Names_Array_Array;
      Check_Names  : in Names_Array_Array);
   --  Write machine-readable text format of Table.States to a file
   --  File_Name, to be read by the parser executable at startup.

   function Get_Text_Rep
     (File_Name      : in String;
      McKenzie_Param : in McKenzie_Param_Type;
      Productions    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Parse_Table_Ptr;
   --  Read machine-readable text format of states from a file File_Name.
   --  Result has actions, checks from Productions.

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
      Descriptor : in WisiToken.Descriptor)
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
   --  given point in Terminals.
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

      case Op is
      when Fast_Forward =>
         FF_Token_Index : WisiToken.Token_Index;
         --  Config.Current_Shared_Token after the operation is done; the last
         --  token shifted.

      when Undo_Reduce =>
         Nonterm     : Token_ID;
         Token_Count : Ada.Containers.Count_Type;
         --  The number of tokens pushed on the stack.
         --  Nonterm is the nonterminal popped off the stack.

      when Push_Back | Insert | Delete =>
         ID : Token_ID;
         --  For Push_Back, ID is the nonterm ID popped off the stack.
         --  For Insert | Delete, ID is the token inserted or deleted.

         Token_Index : WisiToken.Base_Token_Index;
         --  For Push_Back, Token_Index is Config.Current_Shared_Token after
         --  the operation is done. If the token is empty, Token_Index is
         --  Invalid_Token_Index.
         --
         --  For Insert, ID is inserted before Token_Index.
         --
         --  For Delete, token at Token_Index is deleted.

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

   function Config_Op_Image (Item : in Config_Op; Descriptor : in WisiToken.Descriptor) return String
     is ("(" & Config_Op_Label'Image (Item.Op) & ", " &
           (case Item.Op is
            when Fast_Forward => WisiToken.Token_Index'Image (Item.FF_Token_Index),
            when Undo_Reduce => Image (Item.Nonterm, Descriptor) & "," &
                 Ada.Containers.Count_Type'Image (Item.Token_Count),
            when Push_Back | Insert | Delete => Image (Item.ID, Descriptor) & "," &
                 WisiToken.Token_Index'Image (Item.Token_Index))
           & ")");

   function Image (Item : in Config_Op; Descriptor : in WisiToken.Descriptor) return String
     renames Config_Op_Image;

   function Image is new Config_Op_Queues.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Config_Op_Array_Image is new Config_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Image (Item : in Config_Op_Arrays.Vector; Descriptor : in WisiToken.Descriptor) return String
     renames Config_Op_Array_Image;

   function None (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is (for all O of Ops => O.Op /= Op);
   --  True if Ops contains no Op.

   function None_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains no Op after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward).

   function Match_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op) return Boolean;
   --  True if Ops contains an Op after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward) that equals Op.

   function Any (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is (for some O of Ops => O.Op = Op);
   --  True if Ops contains at least one Op.

   type Recover_Stack_Item is record
      State      : Unknown_State_Index;
      Tree_Index : Syntax_Trees.Node_Index;
      Token      : Recover_Token;
   end record;

   package Recover_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Recover_Stack_Item);

   function Image (Item : in Recover_Stack_Item; Descriptor : in WisiToken.Descriptor) return String
     is ((if Item.State = Unknown_State then " " else Trimmed_Image (Item.State)) & " : " &
           Image (Item.Token, Descriptor));

   function Recover_Stack_Image is new Recover_Stacks.Gen_Image_Aux (WisiToken.Descriptor, Image);
   --  Unique name for calling from debugger

   function Image
     (Stack      : in Recover_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
     renames Recover_Stack_Image;

   function Valid_Tree_Indices (Stack : in Recover_Stacks.Stack; Depth : in SAL.Base_Peek_Type) return Boolean;
   --  Return True if Stack top Depth items have valid Tree_Indices,
   --  which is true if they were copied from the parser stack, and not
   --  pushed by recover.

   type Configuration is record
      Stack : Recover_Stacks.Stack;
      --  Initially built from the parser stack, then the stack after the
      --  Ops below have been performed.

      Resume_Token_Goal : Token_Index := Token_Index'Last;
      --  A successful solution shifts this token. Per-config because it
      --  increases with Delete; we increase Shared_Parser.Resume_Token_Goal
      --  only from successful configs.

      Current_Shared_Token : Token_Index := Token_Index'Last;
      --  Index into Shared_Parser.Terminals for current input token, after
      --  all of Inserted is input. Initially the error token.

      String_Quote_Checked : Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Inserted         : Fast_Token_ID_Arrays.Vector;
      Current_Inserted : SAL.Base_Peek_Type := No_Inserted;
      --  Index of current input token in Inserted. If No_Index, use
      --  Current_Shared_Token.

      Error_Token       : Recover_Token;
      Check_Token_Count : Ada.Containers.Count_Type;
      Check_Status      : Semantic_Checks.Check_Status;
      --  If parsing this config ended with a parse error, Error_Token is
      --  the token that failed to shift, Check_Status.Label is Ok.
      --
      --  If parsing this config ended with a semantic check fail,
      --  Error_Token is the nonterm created by the reduction,
      --  Check_Token_Count the number of tokens in the right hand side, and
      --  Check_Status is the error.
      --
      --  Cleared when Config is parsed successfully, or modified so the
      --  error is no longer meaningful (ie in explore when adding an op, or
      --  in languag_fixes when adding a fix).

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

   type Check_Status is (Success, Abandon, Continue);
   subtype Non_Success_Status is Check_Status range Abandon .. Continue;

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

   function Image
     (Item       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree;
      Descriptor : in WisiToken.Descriptor)
     return String;

private

   --  Private to enforce use of Add; doesn't succeed, since only
   --  children use it.
   type Goto_Node is record
      Symbol     : Token_ID;
      State      : State_Index;
      Next       : Goto_Node_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Goto_Node, Goto_Node_Ptr);

   type Action_List_Iterator is tagged record
      Node : Action_Node_Ptr;
      Item : Parse_Action_Node_Ptr;
   end record;

   type Goto_List_Iterator is tagged record
      Node : Goto_Node_Ptr;
   end record;

end WisiToken.LR;
