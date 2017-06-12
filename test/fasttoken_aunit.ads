--  Abstract :
--
--  AUnit routines useful in FastToken tests
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

with AUnit.Checks;
with FastToken.Parser.LR.LR1_Items;
with FastToken.Production;
with FastToken.Token;
package FastToken_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Token.List.List_Iterator;
      Expected : in FastToken.Token.List.List_Iterator);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Production.Instance;
      Expected : in FastToken.Production.Instance);

   procedure Check
     (Label            : in String;
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Ptr;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean);

   procedure Check
     (Label            : in String;
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Set;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Set;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Expected : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr);

   procedure Check
     (Label            : in String;
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.LR1_Items.Item_Set_List;
      Expected : in FastToken.Parser.LR.LR1_Items.Item_Set_List);

   function Get_Production
     (Grammar : in FastToken.Production.List.Instance;
      Prod    : in Positive)
     return FastToken.Production.List.List_Iterator;
   function Get_Production
     (Grammar : in FastToken.Production.List.Instance;
      Prod    : in Positive)
     return FastToken.Production.Instance;
   --  Return Prod production in Grammar; 1 indexed.

   function Get_Item_Node
     (Grammar    : in FastToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in FastToken.Parser.LR.LR1_Items.Lookahead;
      State      : in FastToken.Parser.LR.Unknown_State_Index := FastToken.Parser.LR.Unknown_State)
     return FastToken.Parser.LR.LR1_Items.Item_Ptr;
   --  Construct an LR1_Items item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function Get_Item
     (Grammar    : in FastToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in FastToken.Parser.LR.LR1_Items.Lookahead;
      State      : in FastToken.Parser.LR.Unknown_State_Index := FastToken.Parser.LR.Unknown_State)
     return FastToken.Parser.LR.LR1_Items.Item_Ptr
     renames Get_Item_Node;

   function "+" (Item : in FastToken.Parser.LR.LR1_Items.Item_Ptr) return FastToken.Parser.LR.LR1_Items.Item_Set;
   function "+" (Item : in FastToken.Parser.LR.LR1_Items.Item_Ptr) return FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;

   function "+"
     (State : in FastToken.Parser.LR.Unknown_State_Index;
      Item  : in FastToken.Parser.LR.LR1_Items.Item_Ptr)
     return FastToken.Parser.LR.LR1_Items.Item_Set_List;
   function "&"
     (Left  : in FastToken.Parser.LR.LR1_Items.Item_Set_List;
      Right : in FastToken.Parser.LR.LR1_Items.Item_Set_List)
     return FastToken.Parser.LR.LR1_Items.Item_Set_List;

   function Get_Set
     (To_State : in FastToken.Parser.LR.Unknown_State_Index;
      Set_List : in FastToken.Parser.LR.LR1_Items.Item_Set_List)
     return FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;

   type AUnit_Goto_Item is record
      Symbol : FastToken.Token_ID;
      Set    : FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
   end record;

   function "+" (Right : in AUnit_Goto_Item) return FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
   function "&"
     (Left  : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Right : in AUnit_Goto_Item)
     return FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;

   procedure Add_Gotos
     (List  : in FastToken.Parser.LR.LR1_Items.Item_Set_List;
      State : in FastToken.Parser.LR.Unknown_State_Index;
      Gotos : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr);

   function Get_Item_Set
     (Grammar   : in FastToken.Production.List.Instance;
      Prod      : in Positive;
      Dot       : in Positive;
      Lookahead : in FastToken.Parser.LR.LR1_Items.Lookahead)
     return FastToken.Parser.LR.LR1_Items.Item_Set;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (FastToken.Parser.LR.Unknown_State_Index);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (FastToken.Parser.LR.Parse_Action_Verbs);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.Parse_State;
      Expected : in FastToken.Parser.LR.Parse_State);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.Parse_Table;
      Expected : in FastToken.Parser.LR.Parse_Table);

end FastToken_AUnit;
