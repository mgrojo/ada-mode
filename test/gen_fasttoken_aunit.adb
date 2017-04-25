--  Abstract :
--
--  See spec.
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

with AUnit.Assertions;
package body Gen_FastToken_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Token_Pkg.List.List_Iterator;
      Expected : in Token_Pkg.List.List_Iterator)
   is
      use AUnit.Checks;
      use Token_Pkg;
      use Production;
      use Token_Pkg.List;
      Computed_I : List_Iterator := Computed;
      Expected_I : List_Iterator := Expected;
      Index      : Integer       := 1;
   begin
      loop
         if Computed_I = Null_Iterator or Expected_I = Null_Iterator then
            Check (Label & " = null", Computed_I = Null_Iterator and Expected_I = Null_Iterator, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), ID (Computed_I), ID (Expected_I));
         Next_Token (Computed_I);
         Next_Token (Expected_I);
         Index := Index + 1;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Production.Instance;
      Expected : in Production.Instance)
   is
      use AUnit.Checks;
      use Nonterminal;
   begin
      Check (Label & ".Index", Computed.RHS.Index, Expected.RHS.Index);
      Check (Label & ".LHS", ID (Computed.LHS), ID (Expected.LHS));
      Check (Label & ".RHS", Computed.RHS.Tokens.First, Expected.RHS.Tokens.First);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Lookahead_Ptr;
      Expected : in LR1_Items.Lookahead_Ptr)
   is
      use AUnit.Checks;
      use LR1_Items;
      Computed_I : Lookahead_Ptr := Computed;
      Expected_I : Lookahead_Ptr := Expected;
      Index : Integer := 1;
   begin
      loop
         if Computed_I = null then
            Check (Label & " = null", Expected_I = null, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), Computed_I.Lookahead, Expected_I.Lookahead);
         Check
           (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Ptr;
      Expected : in LR1_Items.Item_Ptr)
   is
      use AUnit.Checks;
      use LR1_Items;
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
         Check (Label & Integer'Image (Index) & ".State", Computed_I.State, Expected_I.State);
         Check (Label & Integer'Image (Index) & ".Prod", Computed_I.Prod, Expected_I.Prod);
         Check (Label & Integer'Image (Index) & ".Dot", Computed_I.Dot, Expected_I.Dot);
         Check (Label & Integer'Image (Index) & ".Lookaheads", Computed_I.Lookaheads, Expected_I.Lookaheads);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Set;
      Expected : in LR1_Items.Item_Set)
   is
      use AUnit.Checks;
      use type LR1_Items.Goto_Item_Ptr;
      use type LR1_Items.Item_Set_Ptr;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   function "&" (Left, Right : in LR1_Items.Goto_Item) return LR1_Items.Goto_Item_Ptr
   is
      use LR1_Items;
      Result : constant Goto_Item_Ptr := new Goto_Item'(Left);
   begin
      Result.Next := new Goto_Item'(Right);
      return Result;
   end "&";

   function "&"
     (Left  : in LR1_Items.Goto_Item_Ptr;
      Right : in LR1_Items.Goto_Item)
     return LR1_Items.Goto_Item_Ptr
   is
      use LR1_Items;
      Result : constant Goto_Item_Ptr := Left;
      Tail   : Goto_Item_Ptr := Left.Next;
   begin
      loop
         exit when Tail.Next = null;
         Tail := Tail.Next;
      end loop;
      Tail.Next := new Goto_Item'(Right);
      return Result;
   end "&";

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Goto_Item_Ptr;
      Expected : in LR1_Items.Goto_Item_Ptr)
   is
      use AUnit.Checks;
      use LR1_Items;
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
         --  We only check set.index, not set, because the full check would be recursive
         Check (Label & Integer'Image (Index) & ".Set.State", Computed_I.Set.State, Expected_I.Set.State);
         Check (Label & Integer'Image (Index) & ".Symbol", Computed_I.Symbol, Expected_I.Symbol);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Set_Ptr;
      Expected : in LR1_Items.Item_Set_Ptr)
   is
      use type LR1_Items.Item_Set_Ptr;
      Computed_1 : LR1_Items.Item_Set_Ptr := Computed;
      Expected_1 : LR1_Items.Item_Set_Ptr := Expected;
      I          : Integer                           := 1;
   begin
      if Computed_1 = null then
         AUnit.Assertions.Assert (Expected_1 = null, Label & "expected non-null, got null");
      end if;

      loop
         exit when Computed_1 = null;
         Check (Label & Integer'Image (I) & ".Set", Computed_1.Set, Expected_1.Set);
         Check (Label & Integer'Image (I) & ".Goto_List", Computed_1.Goto_List, Expected_1.Goto_List);
         Check (Label & Integer'Image (I) & ".State", Computed_1.State, Expected_1.State);
         Computed_1 := Computed_1.Next;
         Expected_1 := Expected_1.Next;
         I := I + 1;
      end loop;

      AUnit.Assertions.Assert (Expected_1 = null, Label & "expected more items, got" & Integer'Image (I));
   end Check;

   function Get_Production (Prod : in Integer) return Production.Instance
   is
      Grammar_I : Production.List.List_Iterator := Grammar.First;
   begin
      for I in 2 .. Prod loop
         Production.List.Next (Grammar_I);
      end loop;

      return Production.List.Current (Grammar_I);
   end Get_Production;

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LR1_Items.Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LR1_Items.Item_Ptr         := null;
      State      : in LR.Unknown_State_Index := LR.Unknown_State)
     return LR1_Items.Item_Ptr
   is
      Grammar_I : Production.List.List_Iterator := Grammar.First;

      Dot_I : Token_Pkg.List.List_Iterator;
   begin
      for I in 2 .. Prod loop
         Production.List.Next (Grammar_I);
      end loop;

      Dot_I := Production.First_Token (Production.List.Current (Grammar_I));
      for I in 2 .. Dot loop
         Token_Pkg.List.Next_Token (Dot_I);
      end loop;

      return new LR1_Items.Item_Node'
        (Prod       => Production.List.Current (Grammar_I),
         Dot        => Dot_I,
         State      => State,
         Lookaheads => Lookaheads,
         Next       => Next);
   end Get_Item_Node;

   function "+" (Item : in LR1_Items.Item_Ptr) return LR1_Items.Item_Set_Ptr
   is begin
      return new LR1_Items.Item_Set'(Item, null, Item.State, null);
   end "+";

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LR1_Items.Item_Set_Ptr)
     return LR1_Items.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Prod       => Prod,
            Lookaheads => null,
            Dot        => Dot,
            Next       => null),
         Goto_List       => null,
         State           => LR.Unknown_State,
         Next            => Next);
   end Get_Item_Set;

   function "+" (Item : in Token_ID) return LR1_Items.Lookahead_Ptr
   is begin
      return +(1 => Item);
   end "+";

   function "+" (Item : in Token_Array) return LR1_Items.Lookahead_Ptr
   is
      use LR1_Items;
      Result : Lookahead_Ptr;
   begin
      for I in reverse Item'Range loop
         Result := new Lookahead'
           (Propagate => False,
            Lookahead => Item (I),
            Next      => Result);
      end loop;
      return Result;
   end "+";

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Action_Rec;
      Expected : in LR.Parse_Action_Rec)
   is
      use AUnit.Checks;
      use LR;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".State", Computed.State, Expected.State);
      when Reduce | Accept_It =>
         Check (Label & ".LHS", Token_Pkg.ID (Computed.LHS.all), Token_Pkg.ID (Expected.LHS.all));
         --  Ignoring Action
         Check (Label & ".Index", Computed.Index, Expected.Index);
         Check (Label & ".Token_Count", Computed.Token_Count, Expected.Token_Count);
      when Error =>
         null;
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Action_Node_Ptr;
      Expected : in LR.Parse_Action_Node_Ptr)
   is
      use AUnit.Checks;
      use type LR.Parse_Action_Node_Ptr;
      Computed_I : LR.Parse_Action_Node_Ptr := Computed;
      Expected_I : LR.Parse_Action_Node_Ptr := Expected;
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
         Check (Label & Integer'Image (Index) & ".Item", Computed_I.Item, Expected_I.Item);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check (Label : in String; Computed : in LR.Action_Node_Ptr; Expected : in LR.Action_Node_Ptr)
   is
      use AUnit.Checks;
      use type LR.Action_Node_Ptr;
      Computed_I : LR.Action_Node_Ptr := Computed;
      Expected_I : LR.Action_Node_Ptr := Expected;
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
         Check (Label & Integer'Image (Index) & ".Symbol", Computed_I.Symbol, Expected_I.Symbol);
         Check (Label & Integer'Image (Index) & ".Action", Computed_I.Action, Expected_I.Action);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check (Label : in String; Computed : in LR.Goto_Node_Ptr; Expected : in LR.Goto_Node_Ptr)
   is
      use AUnit.Checks;
      use type LR.Goto_Node_Ptr;
      Computed_I : LR.Goto_Node_Ptr := Computed;
      Expected_I : LR.Goto_Node_Ptr := Expected;
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
         Check (Label & Integer'Image (Index) & ".Symbol", Computed_I.Symbol, Expected_I.Symbol);
         Check (Label & Integer'Image (Index) & ".State", Computed_I.State, Expected_I.State);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_State;
      Expected : in LR.Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Table;
      Expected : in LR.Parse_Table)
   is begin
      Check (Label & ".first", Computed'First, Expected'First);
      Check (Label & ".last", Computed'Last, Expected'Last);
      for I in Computed'Range loop
         Check (Label & "." & LR.State_Index'Image (I), Computed (I), Expected (I));
      end loop;
   end Check;

end Gen_FastToken_AUnit;
