--  Abstract :
--
--  Types and operatorion for LR(1) items.
--
--  Copyright (C) 2003, 2008, 2013-2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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
with FastToken.Production;
generic
   type Unknown_State_Index is range <>;
   Unknown_State : in Unknown_State_Index;

   type Semantic_Action is private;
   Null_Semantic_Action : in Semantic_Action;
   with package Production is new FastToken.Production (Token_Pkg, Semantic_Action, Null_Semantic_Action);
package FastToken.Parser.LR1_Items is

   --  We need a special value of Lookahead to indicate '#' in
   --  [dragon] LALR algorithm 4.12. That is implemented by setting
   --  Propagate = True.
   type Lookahead is record
      Propagate : Boolean;
      Tokens    : Token.Terminal_ID_Set;
   end record;
   Null_Lookaheads : constant Lookahead := (False, (others => False));

   function "+" (Item : in Token.Token_ID) return Lookahead;

   type Item_Node is private;
   type Item_Ptr is access Item_Node;

   function Prod (Item : in Item_Ptr) return Production.Instance;
   function LHS (Item : in Item_Ptr) return Token.Nonterminal_ID;
   function RHS (Item : in Item_Ptr) return Production.Right_Hand_Side;
   function Dot (Item : in Item_Ptr) return Token.List.List_Iterator;
   --  Token after Dot.
   function State (Item : in Item_Ptr) return Unknown_State_Index;
   function Lookaheads (Item : in Item_Ptr) return Lookahead;
   function Next (Item : in Item_Ptr) return Item_Ptr;

   function New_Item_Node
     (Prod       : in Production.Instance;
      Dot        : in Token.List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead)
     return Item_Ptr;

   procedure Set
     (Item       : in out Item_Node;
      Prod       : in     Production.Instance;
      Dot        : in     Token.List.List_Iterator;
      State      : in     Unknown_State_Index;
      Lookaheads : in     Lookahead);
   --  Replace all values in Item.

   procedure Add
     (List : in out Item_Ptr;
      Item : in     Item_Ptr);
   --  Add Item to List, in ascending order of Prod.LHS.

   procedure Set_State (List : in Item_Ptr; State : in Unknown_State_Index);
   --  Set State in all items in List.

   procedure Include
     (Item  : in Item_Ptr;
      Value : in Token.Terminal_ID);
   procedure Include
     (Item              : in Item_Ptr;
      Value             : in Lookahead;
      Exclude_Propagate : in Boolean);
   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Exclude_Propagate : in     Boolean);
   --  Add Value to Item.Lookahead

   type Goto_Item is private;
   type Goto_Item_Ptr is access Goto_Item;

   function Symbol (List : in Goto_Item_Ptr) return Token.Token_ID;
   function State (List : in Goto_Item_Ptr) return Unknown_State_Index;
   function Next (List : in Goto_Item_Ptr) return Goto_Item_Ptr;

   type Item_Set;
   type Item_Set_Ptr is access Item_Set;

   function New_Goto_Item
     (Symbol : in     Token.Token_ID;
      Set    : in     Item_Set_Ptr)
     return Goto_Item_Ptr;

   procedure Add
     (List   : in out Goto_Item_Ptr;
      Symbol : in     Token.Token_ID;
      Set    : in     Item_Set_Ptr);
   --  Add an item to List; keep List sorted in ascending order on Symbol.

   type Item_Set is record
      Set       : Item_Ptr;
      Goto_List : Goto_Item_Ptr;
      State     : Unknown_State_Index;
      Next      : Item_Set_Ptr;
   end record;

   Null_Item_Set : constant Item_Set := (null, null, Unknown_State, null);

   function Filter
     (Set     : in     Item_Set;
      Include : access function (Item : in Item_Ptr) return Boolean)
     return Item_Set;
   --  Return a deep copy of Set, including only items for which Include returns True.

   function In_Kernel (Item : in Item_Ptr) return Boolean;
   --  For use with Filter; [dragon] sec 4.7 pg 240

   type Item_Set_List is record
      Head : Item_Set_Ptr;
      Size : Unknown_State_Index := 0;
   end record;

   function Reverse_List (List : in Item_Set_List) return Item_Set_List;
   --  Return a modified List that is in reversed order.

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set);
   --  Add New_Item to Target without checking to see if it is in there already.

   function Find
     (Prod             : in Production.Instance;
      Dot              : in Token.List.List_Iterator;
      Right            : in Item_Set;
      Lookaheads       : in Lookahead := Null_Lookaheads;
      Match_Lookaheads : in Boolean)
     return Item_Ptr;
   --  Return a pointer to an item in Right that matches Prod, Dot,
   --  and Lookaheads if Match_Lookaheads; null if not found.

   function Find
     (Left             : in Item_Set;
      Right            : in Item_Set_List;
      Match_Lookaheads : in Boolean)
     return Item_Set_Ptr;
   --  Return a pointer to Left in Right, null if not found.

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to the set in Sets containing State, null if not found.

   function Is_In
     (Symbol    : in Token.Token_ID;
      Set       : in Item_Set_Ptr;
      Goto_List : in Goto_Item_Ptr)
     return Boolean;

   function Goto_Set
     (From   : in Item_Set;
      Symbol : in Token.Token_ID)
     return Item_Set_Ptr;
   --  Return Item_Set from From.Goto_List where the goto symbol is
   --  Symbol; null if not found.

   function Has_Empty_Production (Grammar : in Production.List.Instance) return Token.Nonterminal_ID_Set;

   function First
     (Grammar              : in Production.List.Instance;
      Has_Empty_Production : in Token.Nonterminal_ID_Set;
      Trace                : in Boolean)
     return Token.Nonterminal_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of tokens
   --  (terminal or nonterminal) that any string derived from it can
   --  start with. Together with Has_Empty_Production, implements
   --  algorithm FIRST from [dragon], augmented with nonterminals.

   function Follow
     (Grammar              : in Production.List.Instance;
      First                : in Token.Nonterminal_Array_Token_Set;
      Has_Empty_Production : in Token.Nonterminal_ID_Set)
     return Token.Nonterminal_Array_Terminal_Set;
   --  For each nonterminal in Grammar, find the set of terminal
   --  tokens that can follow it. Implements algorithm FOLLOW from
   --  [dragon] pg 189.

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Token.Nonterminal_ID_Set;
      First                : in Token.Nonterminal_Array_Token_Set;
      Grammar              : in Production.List.Instance;
      Trace                : in Boolean)
     return Item_Set;
   --  Return the closure of Set over Grammar. First must be the
   --  result of First above. Makes a deep copy of Goto_List.
   --  Implements 'closure' from [dragon] algorithm 4.9 pg 232, but
   --  allows merging lookaheads into one item..

   function Image (Item : in Lookahead) return String;

   procedure Put (Item : in Item_Ptr; Show_Lookaheads : in Boolean);
   --  Ignores Item.Next.

   procedure Put
     (Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False);
   --  Ignores Item.Next.

   procedure Put (Item : in Goto_Item_Ptr);
   procedure Put (Item : in Item_Set_Ptr; Show_Lookaheads : in Boolean := True);
   procedure Put (Item : in Item_Set_List; Show_Lookaheads : in Boolean := True);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

   procedure Free is new Ada.Unchecked_Deallocation (Item_Set, Item_Set_Ptr);

   procedure Free (Item : in out Item_Set);
   procedure Free (Item : in out Item_Set_List);

   ----------
   --  For use in unit tests

   function "&" (Left, Right : in Item_Ptr) return Item_Ptr;
   --  Append Right to Left; does not enforce sort order.

private

   --  Private to force use of Add

   type Item_Node is record
      Prod       : Production.Instance;
      Dot        : Token.List.List_Iterator; -- token after item Dot
      State      : Unknown_State_Index;
      Lookaheads : Lookahead;
      Next       : Item_Ptr;
   end record;

   type Goto_Item is record
      Symbol : Token.Token_ID;
      --  If Symbol is a terminal, this is a shift and goto state action.
      --  If Symbol is a non-terminal, this is a post-reduce goto state action.
      Set    : Item_Set_Ptr;
      Next   : Goto_Item_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Goto_Item, Goto_Item_Ptr);
end FastToken.Parser.LR1_Items;
