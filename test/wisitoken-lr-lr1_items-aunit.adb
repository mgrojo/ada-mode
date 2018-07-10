--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Checks;
with WisiToken.AUnit; use WisiToken.AUnit;
package body WisiToken.LR.LR1_Items.AUnit is

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Expected         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit.Token_ID_Arrays_AUnit;
      Computed_I : Item_Ptr := Computed;
      Expected_I : Item_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         Standard.AUnit.Assertions.Assert (Computed /= null, Label & " Computed is null");
         Standard.AUnit.Assertions.Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".State", State (Computed_I), State (Expected_I));
         Check (Label & Integer'Image (Index) & ".Prod", Prod_ID (Computed_I), Prod_ID (Expected_I));
         Check (Label & Integer'Image (Index) & ".Dot", Dot (Computed_I), Dot (Expected_I));
         if Match_Lookaheads then
            Check (Label & Integer'Image (Index) & ".Lookaheads", Lookaheads (Computed_I), Lookaheads (Expected_I));
         end if;
         Check (Label & Integer'Image (Index) & ".Next = null", Next (Computed_I) = null, Next (Expected_I) = null);
         Computed_I := Next (Computed_I);
         Expected_I := Next (Expected_I);
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Set;
      Expected         : in WisiToken.LR.LR1_Items.Item_Set;
      Match_Lookaheads : in Boolean := True)
   is begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set, Match_Lookaheads);
      --  ignoring Goto_List;
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Set_List;
      Expected         : in WisiToken.LR.LR1_Items.Item_Set_List;
      Match_Lookaheads : in Boolean := True)
   is begin
      Check (Label & ".first_index", Computed.First_Index, Expected.First_Index);
      Check (Label & ".last_index", Computed.Last_Index, Expected.Last_Index);
      for I in Computed.First_Index .. Computed.Last_Index loop
         Check (Label & State_Index'Image (I), Computed (I), Expected (I), Match_Lookaheads);
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.LR.LR1_Items.Goto_Item_Ptr;
      Expected : in WisiToken.LR.LR1_Items.Goto_Item_Ptr)
   is
      use Standard.AUnit.Checks;
      Computed_I : Goto_Item_Ptr := Computed;
      Expected_I : Goto_Item_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         Standard.AUnit.Assertions.Assert (Computed /= null, Label & " Computed is null");
         Standard.AUnit.Assertions.Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         --  We only check set.state, not set.*, because the full check would be recursive
         Check (Label & Integer'Image (Index) & ".Symbol", Symbol (Computed_I), Symbol (Expected_I));
         Check (Label & Integer'Image (Index) & ".Set.State", State (Computed_I), State (Expected_I));
         Check (Label & Integer'Image (Index) & ".Next = null", Next (Computed_I) = null, Next (Expected_I) = null);
         Computed_I := Next (Computed_I);
         Expected_I := Next (Expected_I);
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   function Get_Item_Node
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod       : in WisiToken.Production_ID;
      Dot        : in Positive;
      Lookaheads : in WisiToken.LR.LR1_Items.Lookahead;
      State      : in WisiToken.Unknown_State_Index := WisiToken.Unknown_State)
     return WisiToken.LR.LR1_Items.Item_Ptr
   is
      Dot_I : constant Token_ID_Arrays.Cursor := Grammar (Prod.Nonterm).RHSs (Prod.RHS).Tokens.To_Cursor (Dot);
   begin
      return WisiToken.LR.LR1_Items.New_Item_Node (Prod, Dot_I, State, Lookaheads);
   end Get_Item_Node;

   function "+" (Item : in WisiToken.LR.LR1_Items.Item_Ptr) return WisiToken.LR.LR1_Items.Item_Set
   is begin
      return WisiToken.LR.LR1_Items.Item_Set'(Item, null, WisiToken.LR.LR1_Items.State (Item));
   end "+";

   function "+" (Item : in WisiToken.LR.LR1_Items.Item_Ptr) return WisiToken.LR.LR1_Items.Item_Set_List
   is begin
      return Result : Item_Set_List do
         Result.Append (Item_Set'(Item, null, State (Item)));
      end return;
   end "+";

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in WisiToken.LR.LR1_Items.Item_Ptr)
     return WisiToken.LR.LR1_Items.Item_Set_List
   is begin
      Set_State (Item, State);
      return Result : Item_Set_List do
         Result.Append (Item_Set'(Item, null, State));
      end return;
   end "+";

   function "&"
     (Left, Right : in WisiToken.LR.LR1_Items.Item_Set_List)
     return WisiToken.LR.LR1_Items.Item_Set_List
   is
   begin
      return Result : Item_Set_List := Left do
         for Element of Right loop
            Result.Append (Element);
         end loop;
      end return;
   end "&";

   function "+" (Right : in AUnit_Goto_Item) return WisiToken.LR.LR1_Items.Goto_Item_Ptr
   is begin
      return WisiToken.LR.LR1_Items.New_Goto_Item (Right.Symbol, Right.State);
   end "+";

   function "&"
     (Left  : in WisiToken.LR.LR1_Items.Goto_Item_Ptr;
      Right : in AUnit_Goto_Item)
     return WisiToken.LR.LR1_Items.Goto_Item_Ptr
   is
      Result : WisiToken.LR.LR1_Items.Goto_Item_Ptr := Left;
   begin
      WisiToken.LR.LR1_Items.Add (Result, Right.Symbol, Right.State);
      return Left;
   end "&";

   procedure Add_Gotos
     (List  : in out WisiToken.LR.LR1_Items.Item_Set_List;
      State : in     WisiToken.Unknown_State_Index;
      Gotos : in     WisiToken.LR.LR1_Items.Goto_Item_Ptr)
   is begin
      List (State).Goto_List := Gotos;
   end Add_Gotos;

   function Get_Item_Set
     (Grammar   : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod      : in WisiToken.Production_ID;
      Dot       : in Positive;
      Lookahead : in WisiToken.LR.LR1_Items.Lookahead)
     return WisiToken.LR.LR1_Items.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Grammar,
            Prod       => Prod,
            Dot        => Dot,
            Lookaheads => Lookahead),
         Goto_List       => null,
         State           => WisiToken.Unknown_State);
   end Get_Item_Set;

end WisiToken.LR.LR1_Items.AUnit;
