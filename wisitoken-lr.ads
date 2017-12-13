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
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 Stephe Leake
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
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Bounded_Definite_Vectors;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Queues;
with SAL.Gen_Unbounded_Definite_Stacks;
with WisiToken.Lexer;
with WisiToken.Semantic_State;
package WisiToken.LR is

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range 0 .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   --  Parser stack type. Visible here for error recovery.
   type Parser_Stack_Item is record
      State : Unknown_State_Index;
      ID    : Token_ID;
   end record;
   Default_Parser_Stack_Item : constant Parser_Stack_Item := (Unknown_State, Invalid_Token_ID);

   package Parser_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Parser_Stack_Item);

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Stack : in Parser_Stacks.Stack_Type);
   --  Put image of top 10 stack items to Trace.

   type Semantic_Status is (Ok, Error);

   type Semantic_Check is access function
     (Stack   : in Parser_Stacks.Stack_Type;
      Nonterm : in Token_ID;
      Tokens  : in Base_Token_Arrays.Vector)
     return Semantic_Status;
   --  Called during error recovery to implement language-specific
   --  checks, such as block name matching in Ada.

   Null_Check : constant Semantic_Check := null;

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
         LHS    : Token_ID;
         Action : WisiToken.Semantic_State.Semantic_Action;
         Check  : Semantic_Check;
         Index  : Natural;
         --  Index of production among productions for a nonterminal,
         --  for generating action names

         Token_Count : Ada.Containers.Count_Type;
      when Error =>
         null;
      end case;
   end record;
   subtype Shift_Action_Rec is Parse_Action_Rec (Shift);
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   Null_Reduce_Action_Rec : constant Reduce_Action_Rec :=
     (Reduce, Token_ID'First, WisiToken.Semantic_State.Null_Action, Null_Check, 0, 0);

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec) return String;
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

   type Action_List_Iterator is tagged private;
   function First_Action (State : in Parse_State) return Action_List_Iterator;
   function Is_Done (Iter : in Action_List_Iterator) return Boolean;
   procedure Next (Iter : in out Action_List_Iterator);

   function Symbol (Iter : in Action_List_Iterator) return Token_ID;
   function Action (Iter : in Action_List_Iterator) return Parse_Action_Rec;

   ----------
   --  Run-time parse table construction subprograms:

   procedure Add_Action
     (State       : in out Parse_State;
      Symbol      : in     Token_ID;
      State_Index : in     LR.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     Parse_Action_Verbs;
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_State.Semantic_Action;
      Semantic_Check  : in     LR.Semantic_Check);
   --  Add a Reduce or Accept_It action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      State_Index     : in     LR.State_Index;
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_State.Semantic_Action;
      Semantic_Check  : in     LR.Semantic_Check);
   --  Add a Shift/Reduce conflict to State.

   procedure Add_Action
     (State             : in out Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     Parse_Action_Verbs;
      LHS_ID_1          : in     Token_ID;
      Index_1           : in     Integer;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     WisiToken.Semantic_State.Semantic_Action;
      Semantic_Check_1  : in     LR.Semantic_Check;
      LHS_ID_2          : in     Token_ID;
      Index_2           : in     Integer;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     WisiToken.Semantic_State.Semantic_Action;
      Semantic_Check_2  : in     LR.Semantic_Check);
   --  Add an Accept/Reduce or Reduce/Reduce conflict action to State.

   procedure Add_Error (State  : in out Parse_State);
   --  Add an Error action to State, at tail of action list.

   procedure Add_Goto
     (State    : in out Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     LR.State_Index);
   --  Add a Goto to State; keep goto list sorted in ascending order on Symbol.

   type Parse_State_Array is array (State_Index range <>) of Parse_State;

   type Pattern is abstract tagged null record;
   --  We don't declare a dispatching operation to implement Pattern
   --  here, because the required types are not visible. See
   --  wisitoken-parser-lr-mckenzie_recover.adb

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
      Follow         : Token_Array_Token_Set (First_Nonterminal .. Last_Nonterminal, First_Terminal .. Last_Terminal);
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

   package Base_Token_Queues is new SAL.Gen_Unbounded_Definite_Queues (Base_Token);
   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Base_Token_Queues.Queue_Type);

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Lexer                   : WisiToken.Lexer.Handle;
      Table                   : Parse_Table_Ptr;
      Semantic_State          : WisiToken.Semantic_State.Semantic_State_Access;
      Shared_Lookahead        : Base_Token_Queues.Queue_Type;
      Max_Parallel            : Ada.Containers.Count_Type;
      First_Parser_Label      : Integer;
      Terminate_Same_State    : Boolean;
      Enable_McKenzie_Recover : Boolean;
   end record;

   overriding procedure Finalize (Object : in out Instance);
   --  Deep free Object.Table.

   procedure Parse (Shared_Parser : in out Instance);
   --  Attempt a parse. Does _not_ reset Parser.Lexer on each call, to
   --  allow continuing in the same input stream.
   --
   --  Raises Syntax_Error for lexer errors, Parse_Error for
   --  parser errors.
   --
   --  If an error is encountered but a recover strategy succeeds, no
   --  exception is raised. Semantic_State contains information about the
   --  errors that were corrected.

   ----------
   --  Useful text output

   function State_Image (Item : in Unknown_State_Index) return String;
   --  no leading space; " " for Unknown_State

   function Image
     (Descriptor : in WisiToken.Descriptor'Class;
      Stack      : in Parser_Stacks.Stack_Type;
      Depth      : in SAL.Base_Peek_Type := 0;
      Top_First  : in Boolean            := True)
     return String;
   --  If Depth = 0, put all of Stack. Otherwise put Min (Depth,
   --  Stack.Depth) items.

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
   --  Capacity is determined by the maximum number of popped, inserted,
   --  deleted, or lookahead tokens. The first three are limited by the
   --  cost_limit McKenzie parameter; in practice, a cost of 20 is too
   --  high. Lookahead is limited by the check_limit parameter; 20 is
   --  very high.

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Fast_Token_ID_Vectors.Vector) return String;
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Fast_Token_ID_Vectors.Vector);
   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Fast_Token_ID_Vectors.Vector);

   type Configuration is new WisiToken.Semantic_State.Recover_Data with record
      Stack : Parser_Stacks.Stack_Type;
      --  The stack after the operations below have been done; suitable for
      --  the next operation.

      Shared_Lookahead_Index : SAL.Base_Peek_Type; -- index into Parser.Shared_Lookahead for next input token

      Local_Lookahead        : Fast_Token_ID_Vectors.Vector;
      Local_Lookahead_Index  : Ada.Containers.Count_Type;
      --  Local_Lookahead contains tokens inserted by special rules.
      --  It is not a queue type, because we always access it via
      --  Local_Lookahead_Index

      Popped   : Fast_Token_ID_Vectors.Vector;
      Pushed   : Parser_Stacks.Stack_Type;
      Inserted : Fast_Token_ID_Vectors.Vector;
      Deleted  : Fast_Token_ID_Vectors.Vector;
      Cost     : Natural := 0;
   end record;

   overriding
   function Image (Config : in Configuration; Descriptor : in WisiToken.Descriptor'Class) return String;
   --  Aggregate syntax, for sending to IDE.

   function Key (A : in Configuration) return Integer is (A.Cost);

   procedure Set_Key (Item : in out Configuration; Key : in Integer);

   Default_Configuration : constant Configuration :=
     (Stack                  => Parser_Stacks.Empty_Stack,
      Shared_Lookahead_Index => SAL.Base_Peek_Type'First,
      Local_Lookahead        => Fast_Token_ID_Vectors.Empty_Vector,
      Local_Lookahead_Index  => Fast_Token_ID_Vectors.No_Index,
      Popped                 => Fast_Token_ID_Vectors.Empty_Vector,
      Pushed                 => Parser_Stacks.Empty_Stack,
      Inserted               => Fast_Token_ID_Vectors.Empty_Vector,
      Deleted                => Fast_Token_ID_Vectors.Empty_Vector,
      Cost                   => 0);

   package Config_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci
     (Element_Type => Configuration,
      Key_Type     => Integer,
      Key          => Key,
      Set_Key      => Set_Key);

   type McKenzie_Data is tagged record
      Config_Heap   : Config_Heaps.Heap_Type;
      Enqueue_Count : Integer := 0;
      Check_Count   : Integer := 0;
      Results       : Config_Heaps.Heap_Type;
      Success       : Boolean := False;
   end record;

   type McKenzie_Access is access all McKenzie_Data;

   Default_McKenzie : constant McKenzie_Data := (others => <>);
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
     (Lexer          : not null access WisiToken.Lexer.Instance'Class;
      Semantic_State : not null access WisiToken.Semantic_State.Semantic_State'Class)
     return Token_ID;
   --  Get next token from Lexer, call Semantic_State.Lexer_To_Lookahead.
   --  If it is a grammar token, return it. Otherwise, repeat.

end WisiToken.LR;
