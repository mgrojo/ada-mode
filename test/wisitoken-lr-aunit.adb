--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL); --  AUnit

with AUnit.Checks.Containers;
with AUnit.Assertions;
with WisiToken.AUnit;
package body WisiToken.LR.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Rec;
      Expected : in Parse_Action_Rec)
   is
      use Standard.AUnit.Checks.Containers;
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
      use all type Parse_Action_Verbs;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".State", Computed.State, Expected.State);
      when Reduce | Accept_It =>
         Check (Label & ".LHS", Computed.LHS, Expected.LHS);
         --  Ignoring Action
         Check (Label & ".Index", Computed.Index, Expected.Index);
         Check (Label & ".Token_Count", Computed.Token_Count, Expected.Token_Count);
      when Error =>
         null;
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Node_Ptr;
      Expected : in Parse_Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use type Parse_Action_Node_Ptr;
      Computed_I : Parse_Action_Node_Ptr := Computed;
      Expected_I : Parse_Action_Node_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         Assert (Computed /= null, Label & " Computed is null");
         Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".Item", Computed_I.Item, Expected_I.Item);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Action_Node_Ptr;
      Expected : in Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use WisiToken.AUnit;
      use type Action_Node_Ptr;
      Computed_I : Action_Node_Ptr := Computed;
      Expected_I : Action_Node_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         Assert (Computed /= null, Label & " Computed is null");
         Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".Symbol", Computed_I.Symbol, Expected_I.Symbol);
         Check (Label & Integer'Image (Index) & ".Action", Computed_I.Action, Expected_I.Action);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Goto_Node_Ptr;
      Expected : in Goto_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use WisiToken.AUnit;
      use all type Goto_Node_Ptr;
      Computed_I : Goto_Node_Ptr := Computed;
      Expected_I : Goto_Node_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      if Computed /= null or Expected /= null then
         Assert (Computed /= null, Label & " Computed is null");
         Assert (Expected /= null, Label & " Expected is null");
      else
         --  both are null
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".Symbol", Symbol (Computed_I), Symbol (Expected_I));
         Check (Label & Integer'Image (Index) & ".State", State (Computed_I), State (Expected_I));
         Check (Label & Integer'Image (Index) & ".Next = null", Next (Computed_I) = null, Next (Expected_I) = null);
         Computed_I := Next (Computed_I);
         Expected_I := Next (Expected_I);
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_State;
      Expected : in Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_Table;
      Expected : in Parse_Table)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".States'first", Computed.States'First, Expected.States'First);
      Check (Label & ".States'last", Computed.States'Last, Expected.States'Last);
      for I in Computed.States'Range loop
         Check
           (Label & ".States." & State_Index'Image (I), Computed.States (I), Expected.States (I));
      end loop;
      --  We do not check McKenzie, since that is not computed.
      Check (Label & ".Follow", Computed.Follow, Expected.Follow);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parser_Stack_Item;
      Expected : in Parser_Stack_Item)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".ID", Computed.ID, Expected.ID);
   end Check;

   function To_State_Stack (Item : in Parser_Stack_Item_Array) return Parser_Stacks.Stack_Type
   is begin
      return
        Result : Parser_Stacks.Stack_Type
      do
         Result.Set_Depth (Item'Length);
         for I in SAL.Base_Peek_Type'(1) .. Item'Length loop
            Result.Set (I, Item'Length, Item (I));
         end loop;
      end return;
   end To_State_Stack;

end WisiToken.LR.AUnit;
