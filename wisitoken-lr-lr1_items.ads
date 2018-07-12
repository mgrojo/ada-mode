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

with SAL.Gen_Definite_Doubly_Linked_Lists;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Productions;
package WisiToken.LR.LR1_Items is

   subtype Lookahead is Token_ID_Set;

   type Item is record
      Prod       : Production_ID;
      Dot        : Token_ID_Arrays.Cursor; -- token after item Dot
      Lookaheads : access Lookahead;
   end record;

   Null_Item : constant Item :=
     (Prod       => <>,
      Dot        => <>,
      Lookaheads => null);

   package Item_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Item);

   procedure Add
     (List : in out Item_Lists.List;
      Item : in     LR1_Items.Item);
   --  Add Item to List, in ascending order of Prod.LHS.

   procedure Include
     (Item  : in LR1_Items.Item;
      Value : in Token_ID);
   procedure Include
     (Item              : in     LR1_Items.Item;
      Value             : in     Lookahead;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean);
   procedure Include
     (Item              : in     LR1_Items.Item;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean);
   --  Add Value to Item.Lookahead.
   --  Descriptor may be null when Exclude_Propagate is False

   type Goto_Item is record
      Symbol : Token_ID;
      --  If Symbol is a terminal, this is a shift and goto state action.
      --  If Symbol is a non-terminal, this is a post-reduce goto state action.
      State  : State_Index;
   end record;

   package Goto_Item_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Goto_Item);

   procedure Add
     (List   : in out Goto_Item_Lists.List;
      Symbol : in     Token_ID;
      State  : in     State_Index);
   --  Add an item to List; keep List sorted in ascending order on Symbol.

   type Item_Set is record
      Set       : Item_Lists.List;
      Goto_List : Goto_Item_Lists.List;
      State     : Unknown_State_Index := Unknown_State;
   end record;

   package Item_Set_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (State_Index, Item_Set);
   subtype Item_Set_List is Item_Set_Arrays.Vector;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor'Class;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor'Class;
         Item       : in LR1_Items.Item)
        return Boolean)
     return Item_Set;
   --  Return a deep copy of Set, including only items for which Include returns True.

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class;
      Item       : in LR1_Items.Item)
     return Boolean;
   --  For use with Filter; [dragon] sec 4.7 pg 240

   function Find
     (Item : in LR1_Items.Item;
      Set  : in Item_Set)
     return Item_Lists.Cursor;
   --  Return an item from Set that matches Item; exclude Lookaheads if
   --  not Match_Lookaheads.
   --
   --  Return No_Element if not found.

   function Find
     (Prod       : in     Production_ID;
      Dot        : in     Token_ID_Arrays.Cursor;
      Right      : in     Item_Set;
      Lookaheads : access Lookahead := null)
     return Item_Lists.Cursor;
   --  Return an item from Right that matches Prod, Dot, and
   --  Lookaheads if non-null.
   --
   --  Return No_Element if not found.
   --
   --  Lookaheads is non-null in LR1_Generate.

   function Find
     (Left             : in Item_Set;
      Right            : in Item_Set_List;
      Match_Lookaheads : in Boolean)
     return Unknown_State_Index;
   --  Return the index into Right of an element matching Left, Unknown_State if
   --  not found.
   --
   --  Match_Lookaheads is True in LR1_Generate.

   function Is_In
     (Item      : in Goto_Item;
      Goto_List : in Goto_Item_Lists.List)
     return Boolean;
   --  Return True if a goto on Symbol to State is found in Goto_List

   function Goto_State
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
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean := True);

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False);

   procedure Put
     (Descriptor : in WisiToken.Descriptor'Class;
      List       : in Goto_Item_Lists.List);
   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

end WisiToken.LR.LR1_Items;
