--  Abstract :
--
--  AUnit routines useful in WisiToken tests
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with WisiToken.Parser.LR.LR1_Items;
with WisiToken.Production;
package WisiToken_AUnit is

   --  FIXME: move these to appropriate .AUnit

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Production.Instance;
      Expected : in WisiToken.Production.Instance);

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.Parser.LR.LR1_Items.Item_Ptr;
      Expected         : in WisiToken.Parser.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean);

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.Parser.LR.LR1_Items.Item_Set;
      Expected         : in WisiToken.Parser.LR.LR1_Items.Item_Set;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Expected : in WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr);

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Expected         : in WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Parser.LR.LR1_Items.Item_Set_List;
      Expected : in WisiToken.Parser.LR.LR1_Items.Item_Set_List);

   function Get_Production
     (Grammar : in WisiToken.Production.List.Instance;
      Prod    : in Positive)
     return WisiToken.Production.List.List_Iterator;
   function Get_Production
     (Grammar : in WisiToken.Production.List.Instance;
      Prod    : in Positive)
     return WisiToken.Production.Instance;
   --  Return Prod production in Grammar; 1 indexed.

   function Get_Item_Node
     (Grammar    : in WisiToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in WisiToken.Parser.LR.LR1_Items.Lookahead;
      State      : in WisiToken.Parser.LR.Unknown_State_Index := WisiToken.Parser.LR.Unknown_State)
     return WisiToken.Parser.LR.LR1_Items.Item_Ptr;
   --  Construct an LR1_Items item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function Get_Item
     (Grammar    : in WisiToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in WisiToken.Parser.LR.LR1_Items.Lookahead;
      State      : in WisiToken.Parser.LR.Unknown_State_Index := WisiToken.Parser.LR.Unknown_State)
     return WisiToken.Parser.LR.LR1_Items.Item_Ptr
     renames Get_Item_Node;

   function "+" (Item : in WisiToken.Parser.LR.LR1_Items.Item_Ptr) return WisiToken.Parser.LR.LR1_Items.Item_Set;
   function "+" (Item : in WisiToken.Parser.LR.LR1_Items.Item_Ptr) return WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;

   function "+"
     (State : in WisiToken.Parser.LR.Unknown_State_Index;
      Item  : in WisiToken.Parser.LR.LR1_Items.Item_Ptr)
     return WisiToken.Parser.LR.LR1_Items.Item_Set_List;
   function "&"
     (Left  : in WisiToken.Parser.LR.LR1_Items.Item_Set_List;
      Right : in WisiToken.Parser.LR.LR1_Items.Item_Set_List)
     return WisiToken.Parser.LR.LR1_Items.Item_Set_List;

   function Get_Set
     (To_State : in WisiToken.Parser.LR.Unknown_State_Index;
      Set_List : in WisiToken.Parser.LR.LR1_Items.Item_Set_List)
     return WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;

   type AUnit_Goto_Item is record
      Symbol : WisiToken.Token_ID;
      Set    : WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;
   end record;

   function "+" (Right : in AUnit_Goto_Item) return WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
   function "&"
     (Left  : in WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Right : in AUnit_Goto_Item)
     return WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr;

   procedure Add_Gotos
     (List  : in WisiToken.Parser.LR.LR1_Items.Item_Set_List;
      State : in WisiToken.Parser.LR.Unknown_State_Index;
      Gotos : in WisiToken.Parser.LR.LR1_Items.Goto_Item_Ptr);

   function Get_Item_Set
     (Grammar   : in WisiToken.Production.List.Instance;
      Prod      : in Positive;
      Dot       : in Positive;
      Lookahead : in WisiToken.Parser.LR.LR1_Items.Lookahead)
     return WisiToken.Parser.LR.LR1_Items.Item_Set;

end WisiToken_AUnit;
