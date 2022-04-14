--  Abstract :
--
--  AUnit routines useful in WisiToken tests
--
--  Copyright (C) 2013-2015, 2017, 2018, 2020, 2022 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with WisiToken.AUnit;
with WisiToken.Productions;
package WisiToken.Generate.LR1_Items.AUnit is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Lookahead_Index_Type,
      Array_Type  => Lookahead,
      Check_Index => WisiToken.AUnit.Check,
      Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check
     (Label            : in String;
      Computed         : in Item;
      Expected         : in Item;
      Match_Lookaheads : in Boolean);

   procedure Check
     (Label            : in String;
      Computed         : in Item_Set;
      Expected         : in Item_Set;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in Goto_Item_List;
      Expected : in Goto_Item_List);

   procedure Check
     (Label            : in String;
      Computed         : in Item_Set_List;
      Expected         : in Item_Set_List;
      Match_Lookaheads : in Boolean := True);

   function Get_Item
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod       : in WisiToken.Production_ID;
      Dot        : in Positive;
      Lookaheads : in Lookahead)
     return Item;
   --  Construct an LR1_Items item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function "+" (Item : in LR1_Items.Item) return Item_Set;
   function "+" (Item : in LR1_Items.Item) return Item_Lists.List renames Item_Lists.To_List;

   function "&"
     (Left  : in Item;
      Right : in Item)
     return Item_Lists.List;

   function "&"
     (Left  : in Item_Lists.List;
      Right : in Item)
     return Item_Lists.List;

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in LR1_Items.Item)
     return Item_Set;

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in Item_Lists.List)
     return Item_Set;

   function "&"
     (Left  : in Item_Set;
      Right : in Item_Set)
     return Item_Set_List;

   function "&"
     (Left  : in Item_Set_List;
      Right : in Item_Set)
     return Item_Set_List;

   function "+" (Right : in Goto_Item) return Goto_Item_List;
   function "&"
     (Left  : in Goto_Item_List;
      Right : in Goto_Item)
     return Goto_Item_List;

   procedure Add_Gotos
     (List  : in out Item_Set_List;
      State : in     WisiToken.Unknown_State_Index;
      Gotos : in     Goto_Item_List);

   function Get_Item_Set
     (Grammar   : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod      : in WisiToken.Production_ID;
      Dot       : in Positive;
      Lookahead : in LR1_Items.Lookahead)
     return Item_Set;

end WisiToken.Generate.LR1_Items.AUnit;
