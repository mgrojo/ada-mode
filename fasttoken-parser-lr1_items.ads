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
with FastToken.Token.Nonterminal;
with FastToken.Production;
generic
   type Unknown_State_Index is range <>;
   Unknown_State : in Unknown_State_Index;

   with package Nonterminal is new Token.Nonterminal;
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
package FastToken.Parser.LR1_Items is

   --  A lookahead list is a linked list of tokens. We also define a
   --  Token_ID_Set below, but that is typically much larger than a
   --  lookahead list, and would be mostly False.
   --
   --  We need a special value of Lookahead to indicate '#' in
   --  [dragon] algorithm 4.12. That is implemented by setting
   --  Propagate = True; all other lookaheads have Propagate = False.
   type Lookahead;
   type Lookahead_Ptr is access Lookahead;
   type Lookahead (Propagate : Boolean) is record

      Next : Lookahead_Ptr;
      case Propagate is
      when False =>
         Lookahead : Token.Terminal_ID;
      when True =>
         null;
      end case;
   end record;

   function Deep_Copy (Item : in Lookahead_Ptr) return Lookahead_Ptr;

   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Token.Terminal_ID);
   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Lookahead_Ptr);
   --  Add Value to Set if it is not already present

   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Token.Terminal_ID;
      Added :    out Boolean);
   --  Add Value to Set if it is not already present. Added will be
   --  true if the item had to be added.

   type Item_Node;
   type Item_Ptr is access Item_Node;
   type Item_Node is record
      Prod       : Production.Instance;
      Dot        : Token.List.List_Iterator; -- token after item Dot
      State      : Unknown_State_Index;
      Lookaheads : Lookahead_Ptr;
      Next       : Item_Ptr;
   end record;

   function Item_Node_Of
     (Prod       : in Production.List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead_Ptr := null)
     return Item_Node;
   --  Return an item node made from Prod, Dot at the start of the
   --  right hand side, State, deep copy of Lookaheads.

   type Goto_Item;
   type Goto_Item_Ptr is access Goto_Item;

   type Item_Set;
   type Item_Set_Ptr is access Item_Set;

   type Goto_Item is record
      Symbol : Token.Token_ID;
      Set    : Item_Set_Ptr;
      Next   : Goto_Item_Ptr;
   end record;

   type Item_Set is record
      Set       : Item_Ptr;
      Goto_List : Goto_Item_Ptr;
      State     : Unknown_State_Index;
      Next      : Item_Set_Ptr;
   end record;

   type Item_Set_List is record
      Head : Item_Set_Ptr;
      Size : Unknown_State_Index := 0;
   end record;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set);
   --  Add New_Item to Target without checking to see if it is in there already.

   function Merge
     (New_Item     : in out Item_Node;
      Existing_Set : in out Item_Set)
     return Boolean;
   --  Merge New_Item into Existing_Set. Return True if Existing_Set
   --  is modified.
   --
   --  New_Item.Lookaheads are moved or deallocated, as appropriate.
   --  Rest of New_Item is copied or deallocated.

   function Find
     (Left  : in Item_Node;
      Right : in Item_Set)
     return Item_Ptr;
   --  Return a pointer to an item in Right that matches Left.Prod,
   --  Left.Dot; null if not found.

   function Find
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to Left in Right, null if not found.

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to the set in Sets containing State, null if not found.

   function Is_In
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Boolean;

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

   type Token_ID_Set is array (Token.Token_ID) of Boolean;

   function Image (Item : in Token_ID_Set) return String;

   type Derivation_Matrix is array (Nonterminal_ID) of Token_ID_Set;

   type Nonterminal_ID_Set is array (Nonterminal_ID) of Boolean;

   function Has_Empty_Production (Grammar : in Production.List.Instance) return Nonterminal_ID_Set;

   function First
     (Grammar              : in Production.List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Trace                : in Boolean)
     return Derivation_Matrix;
   --  For each nonterminal in the given grammar, find the set of
   --  tokens (terminal or nonterminal) that its first term could
   --  start with. Implements algorithm FIRST from [dragon].

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      Trace                : in Boolean)
     return Item_Set;
   --  Return the closure of Set over Grammar. First must be the
   --  result of First above. Implements 'closure' from [dragon]
   --  algorithm 4.9 pg 232.

   function Kernels
     (Grammar           : in Production.List.Instance;
      First             : in Derivation_Matrix;
      EOF_Token         : in Token.Token_ID;
      Trace             : in Boolean;
      First_State_Index : in Unknown_State_Index)
     return Item_Set_List;

   function Print (Item : in Lookahead) return String;
   function Print (Item : in Lookahead_Ptr) return String;

   procedure Put (Item : in Item_Node; Show_Lookaheads : in Boolean);
   procedure Put (Item : in Item_Set; Show_Lookaheads : in Boolean := False);
   procedure Put (Item : in Goto_Item_Ptr);
   procedure Put (Item : in Item_Set_List);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Lookahead, Lookahead_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Item_Set, Item_Set_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation (Goto_Item, Goto_Item_Ptr);

   procedure Free (Item : in out Item_Node);
   procedure Free (Item : in out Item_Set);
   procedure Free (Item : in out Item_Set_List);

   ----------
   --  visible for unit test

   function LALR_Goto_Transitions
     (Kernel  : in Item_Set;
      Symbol  : in Token.Token_ID;
      First   : in Derivation_Matrix;
      Grammar : in Production.List.Instance)
     return Item_Set;
   --  FIXME: _not_ 'goto' from [dragon] algorithm 4.9

   function LR1_Goto_Transitions
     (Set                  : in Item_Set;
      Symbol               : in Token.Token_ID;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      Trace                : in Boolean)
     return Item_Set;
   --  'goto' from [dragon] algorithm 4.9

end FastToken.Parser.LR1_Items;
