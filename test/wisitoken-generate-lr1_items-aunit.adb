--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017, 2018, 2020 Stephen Leake.  All Rights Reserved.
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
package body WisiToken.Generate.LR1_Items.AUnit is

   procedure Check
     (Label            : in String;
      Computed         : in Item;
      Expected         : in Item;
      Match_Lookaheads : in Boolean)
   is
      use Standard.AUnit.Checks;
   begin
      Check (Label & ".Prod", Computed.Prod, Expected.Prod);
      Check (Label & ".Dot", Computed.Dot, Expected.Dot);
      if Match_Lookaheads then
         Check (Label & ".Lookaheads", Computed.Lookaheads.all, Expected.Lookaheads.all);
      end if;
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in Item_Lists.List;
      Expected         : in Item_Lists.List;
      Match_Lookaheads : in Boolean := True)
   is
      use Standard.AUnit.Checks;
      use Item_Lists;
      Computed_I : Cursor := Computed.First;
      Expected_I : Cursor := Expected.First;
      Index      : Integer  := 1;
   begin
      if Computed_I /= No_Element or Expected_I /= No_Element then
         Standard.AUnit.Assertions.Assert (Computed_I /= No_Element, Label & " Computed is empty");
         Standard.AUnit.Assertions.Assert (Expected_I /= No_Element, Label & " Expected is empty");
      else
         --  both are empty
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index), Constant_Ref (Computed_I), Constant_Ref (Expected_I), Match_Lookaheads);
         Check (Label & Integer'Image (Index) & ".Next = null",
                Next (Computed_I) = No_Element, Next (Expected_I) = No_Element);
         Computed_I := Next (Computed_I);
         Expected_I := Next (Expected_I);
         Index      := Index + 1;
         exit when Computed_I = No_Element;
      end loop;
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in Item_Set;
      Expected         : in Item_Set;
      Match_Lookaheads : in Boolean := True)
   is begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set, Match_Lookaheads);
      --  ignoring Goto_List, Dot_IDs
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in Item_Set_List;
      Expected         : in Item_Set_List;
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
      Computed : in Goto_Item_Lists.List;
      Expected : in Goto_Item_Lists.List)
   is
      use Standard.AUnit.Checks;
      use Goto_Item_Lists;
      Computed_I : Cursor  := Computed.First;
      Expected_I : Cursor  := Expected.First;
      Index      : Integer := 1;
   begin
      if Computed_I /= No_Element or Expected_I /= No_Element then
         Standard.AUnit.Assertions.Assert (Computed_I /= No_Element, Label & " Computed is empty");
         Standard.AUnit.Assertions.Assert (Expected_I /= No_Element, Label & " Expected is empty");
      else
         --  both are empty
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".Symbol", Computed (Computed_I).Symbol, Expected (Expected_I).Symbol);
         Check (Label & Integer'Image (Index) & ".State", Computed (Computed_I).State, Expected (Expected_I).State);
         Check (Label & Integer'Image (Index) & ".Next = null",
                Next (Computed_I) = No_Element, Next (Expected_I) = No_Element);
         Computed_I := Next (Computed_I);
         Expected_I := Next (Expected_I);
         Index      := Index + 1;
         exit when Computed_I = No_Element;
      end loop;
   end Check;

   function Get_Item
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod       : in WisiToken.Production_ID;
      Dot        : in Positive;
      Lookaheads : in Lookahead)
     return Item
   is
      Tokens : Token_ID_Arrays.Vector renames WisiToken.Productions.Constant_Ref_RHS (Grammar, Prod).Tokens;
   begin
      return
        (Prod,
         (if Dot > Tokens.Last_Index
          then Token_ID_Arrays.No_Index
          else Dot),
         new Token_ID_Set'(Lookaheads));
   end Get_Item;

   function "+" (Item : in LR1_Items.Item) return Item_Set
   is begin
      return Item_Set'
        (Set       => Item_Lists.To_List (Item),
         Goto_List => <>,
         Dot_IDs   => <>,
         State     => <>);
   end "+";

   function "&"
     (Left  : in Item;
      Right : in Item)
     return Item_Lists.List
   is
   begin
      return Result : Item_Lists.List := Item_Lists.To_List (Left) do
         Result.Insert (Right);
      end return;
   end "&";

   function "&"
     (Left  : in Item_Lists.List;
      Right : in Item)
     return Item_Lists.List
   is
   begin
      return Result : Item_Lists.List := Left do
         Result.Insert (Right);
      end return;
   end "&";

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in LR1_Items.Item)
     return Item_Set
   is begin
      return (Set => Item_Lists.To_List (Item), Goto_List => <>, Dot_IDs => <>, State => State);
   end "+";

   function "+"
     (State : in WisiToken.Unknown_State_Index;
      Item  : in Item_Lists.List)
     return Item_Set
   is begin
      return (Set => Item, Goto_List => <>, Dot_IDs => <>, State => State);
   end "+";

   function "&"
     (Left  : in Item_Set;
      Right : in Item_Set)
     return Item_Set_List
   is
      use Item_Set_Arrays;
   begin
      return Result : Item_Set_List := To_Vector (Left) do
         Result.Append (Right);
      end return;
   end "&";

   function "&"
     (Left  : in Item_Set_List;
      Right : in Item_Set)
     return Item_Set_List
   is begin
      return Result : Item_Set_List := Left do
         Result.Append (Right);
      end return;
   end "&";

   function "+" (Right : in Goto_Item) return Goto_Item_Lists.List
   is begin
      return Result : Goto_Item_Lists.List do
         Result.Insert (Right);
      end return;
   end "+";

   function "&"
     (Left  : in Goto_Item_Lists.List;
      Right : in Goto_Item)
     return Goto_Item_Lists.List
   is
      Result : Goto_Item_Lists.List := Left;
   begin
      Result.Insert (Right);
      return Left;
   end "&";

   procedure Add_Gotos
     (List  : in out Item_Set_List;
      State : in     WisiToken.Unknown_State_Index;
      Gotos : in     Goto_Item_Lists.List)
   is begin
      List (State).Goto_List := Gotos;
   end Add_Gotos;

   function Get_Item_Set
     (Grammar   : in WisiToken.Productions.Prod_Arrays.Vector;
      Prod      : in WisiToken.Production_ID;
      Dot       : in Positive;
      Lookahead : in LR1_Items.Lookahead)
     return Item_Set
   is begin
      return
        (Set           => +Get_Item
           (Grammar,
            Prod       => Prod,
            Dot        => Dot,
            Lookaheads => Lookahead),
         Goto_List     => <>,
         Dot_IDs       => <>,
         State         => WisiToken.Unknown_State);
   end Get_Item_Set;

end WisiToken.Generate.LR1_Items.AUnit;
