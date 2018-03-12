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
with WisiToken.Token_ID_Lists.AUnit;
package body WisiToken_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Production.Instance;
      Expected : in WisiToken.Production.Instance)
   is
      use AUnit.Checks;
      use WisiToken.Token_ID_Lists.AUnit;
   begin
      Check (Label & ".Name_Index", Computed.RHS.Name_Index, Expected.RHS.Name_Index);
      Check (Label & ".LHS", Computed.LHS, Expected.LHS);
      Check (Label & ".RHS", Computed.RHS.Tokens.First, Expected.RHS.Tokens.First);
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Expected         : in WisiToken.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean)
   is
      use AUnit.Checks;
      use WisiToken.LR.AUnit;
      use WisiToken.LR.LR1_Items;
      use WisiToken.Token_ID_Lists.AUnit;
      Computed_I : Item_Ptr := Computed;
      Expected_I : Item_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         AUnit.Assertions.Assert (Computed /= null, Label & " Computed is null");
         AUnit.Assertions.Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".State", State (Computed_I), State (Expected_I));
         Check (Label & Integer'Image (Index) & ".Prod", Prod (Computed_I), Prod (Expected_I));
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
      use AUnit.Checks;
      use WisiToken.LR.AUnit;
      use type WisiToken.LR.LR1_Items.Goto_Item_Ptr;
      use type WisiToken.LR.LR1_Items.Item_Set_Ptr;
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
      use AUnit.Checks;
      use WisiToken.LR.AUnit;
      use WisiToken.LR.LR1_Items;
      Computed_I : Goto_Item_Ptr := Computed;
      Expected_I : Goto_Item_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         AUnit.Assertions.Assert (Computed /= null, Label & " Computed is null");
         AUnit.Assertions.Assert (Expected /= null, Label & " Expected is null");
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
      use type WisiToken.LR.LR1_Items.Item_Set_Ptr;
      Computed_1 : WisiToken.LR.LR1_Items.Item_Set_Ptr := Computed;
      Expected_1 : WisiToken.LR.LR1_Items.Item_Set_Ptr := Expected;
      I          : Integer                           := 1;
   begin
      if Computed_1 = null then
         AUnit.Assertions.Assert (Expected_1 = null, Label & "expected non-null, got null");
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

      AUnit.Assertions.Assert (Expected_1 = null, Label & "expected more items, got" & Integer'Image (I));
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

   function Get_Production
     (Grammar : in WisiToken.Production.List.Instance;
      Prod    : in Positive)
     return WisiToken.Production.List.List_Iterator
   is
      Grammar_I : WisiToken.Production.List.List_Iterator := Grammar.First;
   begin
      for I in 2 .. Prod loop
         WisiToken.Production.List.Next (Grammar_I);
      end loop;

      return Grammar_I;
   end Get_Production;

   function Get_Production
     (Grammar : in WisiToken.Production.List.Instance;
      Prod    : in Positive)
     return WisiToken.Production.Instance
   is begin
      return WisiToken.Production.List.Current (Get_Production (Grammar, Prod));
   end Get_Production;

   function Get_Item_Node
     (Grammar    : in WisiToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in WisiToken.LR.LR1_Items.Lookahead;
      State      : in WisiToken.LR.Unknown_State_Index := WisiToken.LR.Unknown_State)
     return WisiToken.LR.LR1_Items.Item_Ptr
   is
      Grammar_I : WisiToken.Production.List.List_Iterator := Grammar.First;

      Dot_I : WisiToken.Token_ID_Lists.Cursor;
   begin
      for I in 2 .. Prod loop
         WisiToken.Production.List.Next (Grammar_I);
      end loop;

      if WisiToken.Production.List.Is_Done (Grammar_I) then
         raise WisiToken.Programmer_Error with Integer'Image (Prod) & " > length (grammar)";
      end if;

      Dot_I := WisiToken.Production.First_Token (WisiToken.Production.List.Current (Grammar_I));
      for I in 2 .. Dot loop
         WisiToken.Token_ID_Lists.Next (Dot_I);
      end loop;

      return WisiToken.LR.LR1_Items.New_Item_Node
        (WisiToken.Production.List.Current (Grammar_I), Dot_I, State, Lookaheads);
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
     (State : in WisiToken.LR.Unknown_State_Index;
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
      use WisiToken.LR.LR1_Items;
      use all type WisiToken.LR.Unknown_State_Index;

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
     (To_State : in WisiToken.LR.Unknown_State_Index;
      Set_List : in WisiToken.LR.LR1_Items.Item_Set_List)
     return WisiToken.LR.LR1_Items.Item_Set_Ptr
   is
      use WisiToken.LR.LR1_Items;
      use all type WisiToken.LR.Unknown_State_Index;

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
      State : in WisiToken.LR.Unknown_State_Index;
      Gotos : in WisiToken.LR.LR1_Items.Goto_Item_Ptr)
   is
      use WisiToken.LR.LR1_Items;
      use all type WisiToken.LR.Unknown_State_Index;
      I : Item_Set_Ptr := List.Head;
   begin
      loop
         exit when I.State = State;
         I := I.Next;
      end loop;
      I.Goto_List := Gotos;
   end Add_Gotos;

   function Get_Item_Set
     (Grammar   : in WisiToken.Production.List.Instance;
      Prod      : in Positive;
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
         State           => WisiToken.LR.Unknown_State,
         Next            => null);
   end Get_Item_Set;

end WisiToken_AUnit;
