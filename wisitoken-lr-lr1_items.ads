--  Abstract :
--
--  Types and operatorion for LR(1) items.
--
--  Copyright (C) 2003, 2008, 2013-2015, 2017, 2018 Stephe Leake
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
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Productions;
package WisiToken.LR.LR1_Items is

   --  We need a special value of Lookahead to indicate '#' in
   --  [dragon] LALR algorithm 4.12. That is implemented by setting
   --  lookahead (Last_Nonterminal + 1) true.
   subtype Lookahead is Token_ID_Set;

   type Item_Node is private;
   type Item_Ptr is access Item_Node;

   function Prod_ID (Item : in Item_Ptr) return WisiToken.Production_ID;
   function Dot (Item : in Item_Ptr) return Token_ID_Arrays.Cursor;
   --  Token after Dot; No_Element if Dot is after last token.
   function State (Item : in Item_Ptr) return Unknown_State_Index;
   function Lookaheads (Item : in Item_Ptr) return Lookahead;
   function Next (Item : in Item_Ptr) return Item_Ptr;

   function New_Item_Node
     (Prod       : in Production_ID;
      Dot        : in Token_ID_Arrays.Cursor;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead)
     return Item_Ptr;

   procedure Set
     (Item       : in out Item_Node;
      Prod       : in     Production_ID;
      Dot        : in     Token_ID_Arrays.Cursor;
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
      Value : in Token_ID);
   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean);
   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean);
   --  Add Value to Item.Lookahead.
   --  Descriptor may be null when Exclude_Propagate is False

   type Goto_Item is private;
   type Goto_Item_Ptr is access Goto_Item;

   function Symbol (List : in Goto_Item_Ptr) return Token_ID;
   function State (List : in Goto_Item_Ptr) return Unknown_State_Index;
   function Next (List : in Goto_Item_Ptr) return Goto_Item_Ptr;

   function New_Goto_Item
     (Symbol : in     Token_ID;
      State  : in     State_Index)
     return Goto_Item_Ptr;
   --  For unit tests

   procedure Add
     (List   : in out Goto_Item_Ptr;
      Symbol : in     Token_ID;
      State  : in     State_Index);
   --  Add an item to List; keep List sorted in ascending order on Symbol.

   type Item_Set is record
      Set       : Item_Ptr;
      Goto_List : Goto_Item_Ptr;
      State     : Unknown_State_Index; --  FIXME: delete, use index in Kernels.
   end record;

   Null_Item_Set : constant Item_Set := (null, null, Unknown_State);

   package Item_Set_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (State_Index, Item_Set);
   subtype Item_Set_List is Item_Set_Arrays.Vector;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor'Class;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor'Class;
         Item       : in Item_Ptr)
        return Boolean)
     return Item_Set;
   --  Return a deep copy of Set, including only items for which Include returns True.

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Item_Ptr)
     return Boolean;
   --  For use with Filter; [dragon] sec 4.7 pg 240

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set);
   --  Add New_Item to Target without checking to see if it is in there already.

   function Find
     (Prod             : in     Production_ID;
      Dot              : in     Token_ID_Arrays.Cursor;
      Right            : in     Item_Set;
      Lookaheads       : access Lookahead := null;
      Match_Lookaheads : in     Boolean)
     return Item_Ptr;
   --  Return a pointer to an item in Right that matches Prod, Dot,
   --  and Lookaheads if Match_Lookaheads; null if not found.

   function Find
     (Left             : in Item_Set;
      Right            : in Item_Set_List;
      Match_Lookaheads : in Boolean)
     return Unknown_State_Index;
   --  Return a index of element matching Left in Right, Unknown_State if
   --  not found.

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Unknown_State_Index;
   --  Return a pointer to the set in Sets containing State, null if not found.
   --  FIXME: not used? just Sets (State)!

   function Is_In
     (Symbol    : in Token_ID;
      State     : in State_Index;
      Goto_List : in Goto_Item_Ptr)
     return Boolean;
   --  Return True if a goto on Symbol to State is found in Goto_List

   function Goto_Set
     (From   : in Item_Set;
      Symbol : in Token_ID)
     return Unknown_State_Index;
   --  Return state from From.Goto_List where the goto symbol is
   --  Symbol; Unknown_State if not found.

   function Follow
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of terminal
   --  tokens that can follow it. Implements algorithm FOLLOW from
   --  [dragon] pg 189.

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class)
     return Item_Set;
   --  Return the closure of Set over Grammar. First must be the
   --  result of First above. Makes a deep copy of Goto_List.
   --  Implements 'closure' from [dragon] algorithm 4.9 pg 232, but
   --  allows merging lookaheads into one item..

   function Productions (Set : in Item_Set) return Production_ID_Arrays.Vector;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Ptr;
      Show_Lookaheads : in Boolean := True);
   --  Ignores Item.Next.

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False);

   procedure Put
     (Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Goto_Item_Ptr);
   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

   procedure Free (Item : in out Item_Set);

   ----------
   --  For use in unit tests

   function "&" (Left, Right : in Item_Ptr) return Item_Ptr;
   --  Append Right to Left; does not enforce sort order.

private

   --  Private to force use of Add

   type Item_Node is record
      Prod       : Production_ID;
      Dot        : Token_ID_Arrays.Cursor; -- token after item Dot
      State      : Unknown_State_Index;
      Lookaheads : access Lookahead;
      Next       : Item_Ptr;
   end record;

   type Goto_Item is record
      Symbol : Token_ID;
      --  If Symbol is a terminal, this is a shift and goto state action.
      --  If Symbol is a non-terminal, this is a post-reduce goto state action.
      State  : State_Index;
      Next   : Goto_Item_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Goto_Item, Goto_Item_Ptr);

end WisiToken.LR.LR1_Items;
