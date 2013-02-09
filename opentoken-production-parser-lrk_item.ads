-------------------------------------------------------------------------------
--
-- Copyright (C) 2003, 2008, 2013 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

---------------------------------------------------------------------------
--  This package provides types and operatorion for parsing analysis
--  on grammars and LR(k) items.
---------------------------------------------------------------------------
generic
   --  The number of elements of lookahead to keep in an item
   K : in Natural := 0;
package OpenToken.Production.Parser.LRk_Item is

   --  Lookahead Sets
   type Full_Lookahead_Set is array (1 .. K) of Tokenizer.Terminal_ID;
   type Item_Lookahead;
   type Item_Lookahead_Ptr is access Item_Lookahead;
   type Item_Lookahead is record
      Last       : Natural;
      Lookaheads : Full_Lookahead_Set;
      Next       : Item_Lookahead_Ptr;
   end record;

   ----------------------------------------------------------------------------
   --  Add the given lookahead to the given lookahead set if it is not already
   --  in there
   ----------------------------------------------------------------------------
   procedure Include (Set   : in out Item_Lookahead_Ptr;
                      Value : in     Item_Lookahead
                     );

   ----------------------------------------------------------------------------
   --  Add the given lookahead to the given lookahead set if it is not already
   --  in there. Added will be true if the item had to be added.
   ----------------------------------------------------------------------------
   procedure Include (Set   : in out Item_Lookahead_Ptr;
                      Value : in     Item_Lookahead;
                      Added :    out Boolean
                     );

   --  The structure for actual items
   type Item_Node;
   type Item_Ptr is access Item_Node;
   type Item_Node is record
      Prod          : OpenToken.Production.Instance;
      Pointer       : Token_List.List_Iterator;
      Lookahead_Set : Item_Lookahead_Ptr;
      Next          : Item_Ptr;
   end record;

   ----------------------------------------------------------------------------
   --  Return an item node made from the given production and iterator into
   --  the production's right hand side.
   ----------------------------------------------------------------------------
   function Item_Node_Of
     (Prod      : in OpenToken.Production.Instance;
      Iterator  : in Token_List.List_Iterator;
      Lookahead : in Item_Lookahead_Ptr := null;
      Next      : in Item_Ptr           := null

     ) return Item_Node;

   ----------------------------------------------------------------------------
   --  Return an item node made from the given production the iterator refers
   --  to and the given lookahead. The lookaheads will be copied.
   ----------------------------------------------------------------------------
   function Item_Node_Of (Prod      : in Production_List.List_Iterator;
                          Lookahead : in Item_Lookahead_Ptr := null) return Item_Node;

   ----------------------------------------------------------------------------
   --  Return an item node made from the given production. The pointer will be
   --  set to the start of the right hand side.
   ----------------------------------------------------------------------------
   function Item_Node_Of (Prod : in OpenToken.Production.Instance) return Item_Node;

   type Set_Reference;
   type Set_Reference_Ptr is access Set_Reference;

   type Item_Set;
   type Item_Set_Ptr is access Item_Set;

   type Set_Reference is record
      Set    : Item_Set_Ptr;
      Symbol : Token.Token_ID;
      Next   : Set_Reference_Ptr;
   end record;

   type Item_Set is record
      Set       : Item_Ptr;
      Goto_List : Set_Reference_Ptr;
      Index     : Natural := 0; -- state_index in LALR parser table
      Next      : Item_Set_Ptr;
   end record;

   type Item_Set_List is record
      Head : Item_Set_Ptr;
      Size : Natural := 0;
   end record;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set);
   --  Add an item to the set without checking to see if it is in there already.

   function Find
     (Left  : in Item_Node;
      Right : in Item_Set)
     return Item_Ptr;
   --  Return a pointer to Left in Right, null if not found.

   function Find
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to Left in Right, null if not found.

   function Find
     (Index : in Integer;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr;
   --  Return a pointer to the set in Sets containing Index, null if not found.

   ----------------------------------------------------------------------------
   --  Check to see if the given item set is in the set list.
   ----------------------------------------------------------------------------
   function Is_In (Left  : in Item_Set;
                   Right : in Item_Set_List
                  ) return Boolean;

   ----------------------------------------------------------------------------
   --  Check to see if the given item set is in the given goto list for the
   --  given symbol.
   ----------------------------------------------------------------------------
   function Is_In (Set_Ptr   : in Item_Set_Ptr;
                   Symbol    : in Token.Token_ID;
                   Goto_List : in Set_Reference_Ptr
                  ) return Boolean;

   ----------------------------------------------------------------------------
   --  Return the goto set for the given item set on the given token symbol.
   ----------------------------------------------------------------------------
   function Goto_Set (From   : in Item_Set;
                      Symbol : in Token.Token_ID
                     ) return Item_Set_Ptr;

   --------------------------------------------------------------------------
   --  Merge the new set into an existing item set list. The existing
   --  set will take over control of the dynamicly allocated lrk
   --  components. If the existing set already contains the new set,
   --  it will not be put in again. In this case, this routine
   --  automatilcy deallocates the components of New_Set that were
   --  created within this package.
   --------------------------------------------------------------------------
   procedure Merge (New_Item     : in out Item_Node;
                    Existing_Set : in out Item_Set
                   );

   ------------------------------------------------
   --  Types and operations for computing Item sets
   --

   type Token_ID_Set is array (Token.Token_ID) of Boolean;

   function Image (Item : in Token_ID_Set) return String;

   type Derivation_Matrix is array
     (Token.Token_ID'Succ (Tokenizer.Last_Terminal) .. Token.Token_ID'Last) of
     Token_ID_Set;

   function First_Derivations
     (Grammar : in Production_List.Instance;
      Trace   : in Boolean)
     return Derivation_Matrix;
   --  For each nonterminal in the given grammar, find the set of
   --  tokens that its first term could start with.

   function Closure
     (Set     : in Item_Set;
      First   : in Derivation_Matrix;
      Grammar : in Production_List.Instance)
     return Item_Set;
   --  The result will contain only one item. This is done
   --  so that they each get their own look-aheads.

   ----------------------------------------------------------------------------
   --  Return the set of transitions from the given item kernel set on the
   --  given symbol.
   ----------------------------------------------------------------------------
   function Goto_Transitions
     (Kernel       : in Item_Set;
      Symbol       : in Token.Token_ID;
      First_Tokens : in Derivation_Matrix;
      Grammar      : in Production_List.Instance;
      Trace        : in Boolean)
     return Item_Set;

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Node);

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item set.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Set);

   ----------------------------------------------------------------------------
   --  Release any resources allocated by this package for the given item set
   --  list.
   ----------------------------------------------------------------------------
   procedure Free (Subject : in out Item_Set_List);

   ----------------------------------------------------------------------------
   --  Compute the kernels for the sets of LR(O) items for the given grammar
   ----------------------------------------------------------------------------
   function LR0_Kernels
     (Grammar      : in Production_List.Instance;
      First_Tokens : in Derivation_Matrix;
      Trace        : in Boolean)
     return Item_Set_List;

   ----------------------------------------------------------------------------
   --  Print out the given item. This routine is included as a debugging aid.
   ----------------------------------------------------------------------------
   function Print (Item : in Item_Lookahead) return String;
   function Print (Item : in Item_Lookahead_Ptr) return String;

   procedure Put_Item (Item : in Item_Node; Prefix : in String := "");
   --  Put Item to Ada.Text_IO.Standard_Output, preceded by Prefix. Does not end with New_Line.

   function Image_Item (Item : in Item_Node; Verbose : in Boolean := False) return String;

   ----------------------------------------------------------------------------
   --  Print out the given item set. This routine is included as a debugging
   --  aid.
   ----------------------------------------------------------------------------
   procedure Print_Item_Set (Items : in Item_Set);
   function Image (Items : in Item_Set) return String;

   ----------------------------------------------------------------------------
   --  Print out all the item sets in the given list. This routine is included
   --  as a debugging aid.
   ----------------------------------------------------------------------------
   procedure Print_Item_Set_List (Items : in Item_Set_List);
   function Print_Item_Set_List (Items : in Item_Set_List) return String;

end OpenToken.Production.Parser.LRk_Item;
