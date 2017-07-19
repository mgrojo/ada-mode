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

with Ada.Unchecked_Deallocation;
package WisiToken.Parser.LR is

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

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range 0 .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   type Parse_Action_Verbs is (Shift, Reduce, Accept_It, Error);
   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         State : State_Index;
      when Reduce | Accept_It =>
         LHS    : Token_ID;
         Action : Semantic_Action;
         Index  : Natural;
         --  Index of production among productions for a nonterminal,
         --  for generating action names

         Token_Count : Ada.Containers.Count_Type;
      when Error =>
         null;
      end case;
   end record;
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   Null_Reduce_Action_Rec : constant Reduce_Action_Rec := (Reduce, Token_ID'First, Null_Action, 0, 0);

   procedure Put_Trace (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec);

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;

   type Action_Node;
   type Action_Node_Ptr is access Action_Node;

   type Action_Node is record
      Symbol : Token_ID; -- ignored if Action is Error
      Action : Parse_Action_Node_Ptr;
      Next   : Action_Node_Ptr;
   end record;

   type Goto_Node is private;
   type Goto_Node_Ptr is access Goto_Node;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID;
   function State (List : in Goto_Node_Ptr) return State_Index;
   function Next (List : in Goto_Node_Ptr) return Goto_Node_Ptr;

   type Parse_State is record
      Action_List : Action_Node_Ptr;
      Goto_List   : Goto_Node_Ptr;
   end record;

   ----------
   --  Run-time parse table construction subprograms:

   procedure Add_Action
     (State       : in out LR.Parse_State;
      Symbol      : in     Token_ID;
      State_Index : in     LR.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out LR.Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     LR.Parse_Action_Verbs;
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_Action);
   --  Add a Reduce or Accept_It action to tail of State action list.

   procedure Add_Action
     (State           : in out LR.Parse_State;
      Symbol          : in     Token_ID;
      State_Index     : in     LR.State_Index;
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_Action);
   --  Add a Shift/Reduce conflict to State.

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     LR.Parse_Action_Verbs;
      LHS_ID_1          : in     Token_ID;
      Index_1           : in     Integer;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     Semantic_Action;
      LHS_ID_2          : in     Token_ID;
      Index_2           : in     Integer;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     Semantic_Action);
   --  Add an Accept/Reduce or Reduce/Reduce conflict action to State.

   procedure Add_Error (State  : in out LR.Parse_State);
   --  Add an Error action to State, at tail of action list.

   procedure Add_Goto
     (State    : in out LR.Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     LR.State_Index);
   --  Add a Goto to State; keep goto list sorted in ascending order on Symbol.

   type Parse_State_Array is array (State_Index range <>) of Parse_State;

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
   is record

      States        : Parse_State_Array (State_First .. State_Last);
      Panic_Recover : Token_ID_Set (First_Nonterminal .. Last_Nonterminal);
      Follow        : Token_Array_Token_Set (First_Nonterminal .. Last_Nonterminal, First_Terminal .. Last_Terminal);
   end record;

   Default_Panic_Recover : constant Token_ID_Set := (1 .. 0 => False);

   type Parse_Table_Ptr is access Parse_Table;
   procedure Free is new Ada.Unchecked_Deallocation (Parse_Table, Parse_Table_Ptr);

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index;
   --  Return next state after reducing stack by ID; Unknown_State if
   --  none (only possible during error recovery).

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr;
   --  Return the action for State, ID.

   type Recover_Data is tagged record
      --  Stored with parser state during recovery. Base type is
      --  sufficient for panic mode recovery; other recovery
      --  algorithms may extend this.
      --
      --  Tokens skipped in the input stream are not stored here; they
      --  are reported via Semantic_State.Discard_Token.
      --
      --  Semantic actions are not performed on the popped or skipped
      --  tokens.
      Nonterm       : Token_ID;
      Goto_State    : Unknown_State_Index;
      Popped_Tokens : Token.List.Instance;
      Pushed_Tokens : Token.List.Instance;
   end record;

   type Recover_Data_Access is access Recover_Data'Class;
   procedure Free is new Ada.Unchecked_Deallocation (Recover_Data'Class, Recover_Data_Access);

   Default_Recover : constant Recover_Data :=
     (Invalid_Token, Unknown_State, Token.List.Null_List, Token.List.Null_List);

   type Instance is abstract new WisiToken.Parser.Instance with record
      Table          : Parse_Table_Ptr;
      Semantic_State : access WisiToken.Token.Semantic_State'Class;
   end record;

   ----------
   --  Useful text output

   function State_Image (Item : in State_Index) return String;
   --  no leading space

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; State : in Parse_State);
   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Table : in Parse_Table);

private

   --  Private to enforce use of Add; doesn't succeed, since only
   --  children use it.
   type Goto_Node is record
      Symbol : Token_ID;
      State  : State_Index;
      Next   : Goto_Node_Ptr;
   end record;

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec);
   --  Add action to List, sorted on ascending Symbol.

end WisiToken.Parser.LR;
