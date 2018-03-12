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
with SAL.Gen_Bounded_Definite_Vectors;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Stacks;
with WisiToken.Lexer;
with WisiToken.Semantic_Checks;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees.Branched;
package WisiToken.LR is

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range 0 .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   --  Parser stack type. Visible here for error recovery.
   type Parser_Stack_Item is record
      State : Unknown_State_Index     := Unknown_State;
      Token : Syntax_Trees.Node_Index := Syntax_Trees.No_Node_Index;
   end record;

   package Parser_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Parser_Stack_Item);

   function Image (Item : in Unknown_State_Index) return String;
   --  no leading space; " " for Unknown_State

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

   type All_Parse_Action_Verbs is (Shift_Local_Lookahead, Shift, Reduce, Accept_It, Error);
   subtype Parse_Action_Verbs is All_Parse_Action_Verbs range Shift .. Error;
   --  Shift_Local_Lookahead is only used for error recovery.

   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         State : State_Index;
      when Reduce | Accept_It =>
         LHS         : Token_ID;
         Action      : WisiToken.Syntax_Trees.Semantic_Action;
         Check       : WisiToken.Semantic_Checks.Semantic_Check;
         Token_Count : Ada.Containers.Count_Type;

         Production : Natural := 0;
         --  Index into Parse_Table.Productions, for McKenzie_Recover.

         Name_Index : Natural;
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
   --  Loops over all shift/reduce actions for a state, including
   --  conflicts.

   function First_Action (State : in Parse_State) return Action_List_Iterator;
   function Is_Done (Iter : in Action_List_Iterator) return Boolean;
   procedure Next (Iter : in out Action_List_Iterator);

   function Symbol (Iter : in Action_List_Iterator) return Token_ID;
   function Action (Iter : in Action_List_Iterator) return Parse_Action_Rec;

   ----------
   --  Run-time parse table construction

   procedure Add_Action
     (State       : in out Parse_State;
      Symbol      : in     Token_ID;
      State_Index : in     LR.State_Index);
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
      State_Index     : in     LR.State_Index;
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
      Insert : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Delete : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      --  Cost of inserting or deleting tokens. Insert includes nonterms
      --  pushed onto the parse stack; delete includes nonterms popped off
      --  the parse stack

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
      Cost_Limit        => Natural'Last,
      Check_Limit       => Natural'Last,
      Patterns          => LR.Patterns.Empty_List);

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in McKenzie_Param_Type);
   --  Put Item to Ada.Text_IO.Current_Output

   package Production_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive, Token_ID_Arrays.Vector);
   --  Element is right hand side; left hand side is given by
   --  Parse_Action_Rec.

   procedure Add_Production
     (Vector : in out Token_ID_Arrays.Vector;
      Tokens : in     Token_ID_Array);

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
     is
   record
      States         : Parse_State_Array (State_First .. State_Last);
      McKenzie_Param : McKenzie_Param_Type (First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
      Productions    : Production_Arrays.Vector;
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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; State : in Parse_State);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Table : in Parse_Table);

   ----------
   --  For McKenzie_Recover: Parser_Lists needs these, Mckenzie_Recover
   --  needs Parser_Lists.

   package Fast_Token_ID_Vectors is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Token_ID, Capacity => 20);
   --  Using a fixed size vector significantly speeds up
   --  McKenzie_Recover.
   --
   --  This contains just Token_ID, not Base_Vector,
   --  because during recover we only know the ID of inserted tokens.
   --  Recover can create thousands of copies of Configuration, so saving
   --  space is important.
   --
   --  Capacity is determined by the maximum number of popped, inserted,
   --  or deleted tokens. These are limited by the cost_limit McKenzie
   --  parameter; in practice, a cost of 20 is too high.
   --
   --  Lookahead capacity is determined by tokens inserted during
   --  Apply_Patterns; 20 has proved enough so far.

   function Image (Item : in Fast_Token_ID_Vectors.Vector; Descriptor : in WisiToken.Descriptor'Class) return String;

   type Recover_Stack_Item is record
      State       : Unknown_State_Index;
      ID          : Token_ID;
      Byte_Region : Buffer_Region;
   end record;

   package Recover_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Recover_Stack_Item);

   function Image
     (Stack      : in Recover_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor'Class;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String;

   type Configuration is record
      --  We don't maintain a syntax tree during recover; it's too slow, and
      --  only needed temporarily for a couple of repair strategies.
      --
      --  We keep Byte_Region on the stack to allow detecting empty items,
      --  for 0 cost deletion.

      Stack : Recover_Stacks.Stack;
      --  Initially built from the parser stack, with ID replacing the tree
      --  index in each item. During recover, the stack after Inserted thru
      --  Current_Inserted and then Shared_Parser.Terminals
      --  (Last_Shared_Token + 1 .. Current_Shared_Token) have been parsed.

      Next_Shared_Token : Base_Token_Index := Base_Token_Arrays.No_Index;
      --  Index into Shared_Parser.Terminals for next input token, after
      --  all of Inserted is input. Initially the error token.

      Popped   : Fast_Token_ID_Vectors.Vector;
      Inserted : Fast_Token_ID_Vectors.Vector;
      Deleted  : Fast_Token_ID_Vectors.Vector;
      Cost     : Natural := 0;
   end record;

   function Key (A : in Configuration) return Integer is (A.Cost);

   procedure Set_Key (Item : in out Configuration; Key : in Integer);

   package Config_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci
     (Element_Type => Configuration,
      Key_Type     => Integer,
      Key          => Key,
      Set_Key      => Set_Key);

   type McKenzie_Data is tagged record
      Parser_Label  : Natural; --  For trace.
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
         Code   : Semantic_Checks.Error_Label;
         Tokens : Syntax_Trees.Valid_Node_Index_Arrays.Vector;
      end case;
   end record;

   package Parse_Error_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Parse_Error);

   procedure Put
     (Source_File_Name : in String;
      Errors           : in Parse_Error_Lists.List;
      Syntax_Tree      : in Syntax_Trees.Abstract_Tree'Class;
      Descriptor       : in WisiToken.Descriptor'Class);
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

   function Next_Grammar_Token
     (Terminals      : in out          Protected_Base_Token_Arrays.Vector;
      Lexer          : not null access WisiToken.Lexer.Instance'Class;
      Semantic_State : in out          WisiToken.Semantic_State.Semantic_State;
      Descriptor     : in              WisiToken.Descriptor'Class)
     return Token_Index;
   --  Get next token from Lexer, call Semantic_State.Lexer_To_Augmented.
   --  If it is a grammar token, store in Terminals and return its ID.
   --  Otherwise, repeat.

   function Reduce_Stack
     (Stack        : in out Parser_Stacks.Stack;
      Syntax_Tree  : in out Syntax_Trees.Branched.Tree;
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
