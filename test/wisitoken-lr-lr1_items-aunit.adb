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
with WisiToken.LR.AUnit;
package body WisiToken.LR.LR1_Items.AUnit is

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Expected         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean)
   is
      use Standard.AUnit.Checks;
      use WisiToken.LR.AUnit;
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
   is
      use Standard.AUnit.Checks;
      use WisiToken.LR.AUnit;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set, Match_Lookaheads);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.LR.LR1_Items.Goto_Item_Ptr;
      Expected : in WisiToken.LR.LR1_Items.Goto_Item_Ptr)
   is
      use Standard.AUnit.Checks;
      use WisiToken.LR.AUnit;
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

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Set_Ptr;
      Expected         : in WisiToken.LR.LR1_Items.Item_Set_Ptr;
      Match_Lookaheads : in Boolean := True)
   is
      use WisiToken.LR.AUnit;
      Computed_1 : WisiToken.LR.LR1_Items.Item_Set_Ptr := Computed;
      Expected_1 : WisiToken.LR.LR1_Items.Item_Set_Ptr := Expected;
      I          : Integer                           := 1;
   begin
      if Computed_1 = null then
         Standard.AUnit.Assertions.Assert (Expected_1 = null, Label & "expected non-null, got null");
      end if;

      loop
         exit when Computed_1 = null;
         Check (Label & Integer'Image (I) & ".Set", Computed_1.Set, Expected_1.Set, Match_Lookaheads);
         Check (Label & Integer'Image (I) & ".Goto_List", Computed_1.Goto_List, Expected_1.Goto_List);
         Check (Label & Integer'Image (I) & ".State", Computed_1.State, Expected_1.State);
         Computed_1 := Computed_1.Next;
         Expected_1 := Expected_1.Next;
         I := I + 1;
      end loop;

      Standard.AUnit.Assertions.Assert (Expected_1 = null, Label & "expected more items, got" & Integer'Image (I));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.LR.LR1_Items.Item_Set_List;
      Expected : in WisiToken.LR.LR1_Items.Item_Set_List)
   is
      use WisiToken.LR.AUnit;
   begin
      Check (Label & ".Size", Computed.Size, Expected.Size);
      Check (Label & ".Head", Computed.Head, Expected.Head, Match_Lookaheads => True);
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
      return WisiToken.LR.LR1_Items.Item_Set'(Item, null, WisiToken.LR.LR1_Items.State (Item), null);
   end "+";

   function "+" (Item : in WisiToken.LR.LR1_Items.Item_Ptr) return WisiToken.LR.LR1_Items.Item_Set_Ptr
   is begin
      return new WisiToken.LR.LR1_Items.Item_Set'(Item, null, WisiToken.LR.LR1_Items.State (Item), null);
   end "+";

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in WisiToken.LR.LR1_Items.Item_Ptr)
     return WisiToken.LR.LR1_Items.Item_Set_List
   is begin
      WisiToken.LR.LR1_Items.Set_State (Item, State);
      return
        (Head => new WisiToken.LR.LR1_Items.Item_Set'(Item, null, State, null),
         Size => 1);
   end "+";

   function "&"
     (Left, Right : in WisiToken.LR.LR1_Items.Item_Set_List)
     return WisiToken.LR.LR1_Items.Item_Set_List
   is
      I : Item_Set_Ptr;
   begin
      if Left.Head.Next = null then
         Left.Head.Next := Right.Head;
      else
         I := Left.Head.Next;
         while I.Next /= null loop
            I := I.Next;
         end loop;
         I.Next := Right.Head;
      end if;
      return (Head => Left.Head, Size => Left.Size + Right.Size);
   end "&";

   function Get_Set
     (To_State : in WisiToken.Unknown_State_Index;
      Set_List : in WisiToken.LR.LR1_Items.Item_Set_List)
     return WisiToken.LR.LR1_Items.Item_Set_Ptr
   is
      I : Item_Set_Ptr := Set_List.Head;
   begin
      loop
         exit when I.State = To_State;
         I := I.Next;
      end loop;
      return I;
   end Get_Set;

   function "+" (Right : in AUnit_Goto_Item) return WisiToken.LR.LR1_Items.Goto_Item_Ptr
   is begin
      return WisiToken.LR.LR1_Items.New_Goto_Item (Right.Symbol, Right.Set);
   end "+";

   function "&"
     (Left  : in WisiToken.LR.LR1_Items.Goto_Item_Ptr;
      Right : in AUnit_Goto_Item)
     return WisiToken.LR.LR1_Items.Goto_Item_Ptr
   is
      Result : WisiToken.LR.LR1_Items.Goto_Item_Ptr := Left;
   begin
      WisiToken.LR.LR1_Items.Add (Result, Right.Symbol, Right.Set);
      return Left;
   end "&";

   procedure Add_Gotos
     (List  : in WisiToken.LR.LR1_Items.Item_Set_List;
      State : in WisiToken.Unknown_State_Index;
      Gotos : in WisiToken.LR.LR1_Items.Goto_Item_Ptr)
   is
      I : Item_Set_Ptr := List.Head;
   begin
      loop
         exit when I.State = State;
         I := I.Next;
      end loop;
      I.Goto_List := Gotos;
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
         State           => WisiToken.Unknown_State,
         Next            => null);
   end Get_Item_Set;

end WisiToken.LR.LR1_Items.AUnit;
