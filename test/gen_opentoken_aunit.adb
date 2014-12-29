--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013, 2014 Stephen Leake.  All Rights Reserved.
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
package body Gen_OpenToken_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Token_Lists.List_Iterator;
      Expected : in Token_Lists.List_Iterator)
   is
      use AUnit.Check;
      use Tokens_Pkg;
      use Productions;
      use Token_Lists;
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
      Computed : in Productions.Instance;
      Expected : in Productions.Instance)
   is
      use AUnit.Check;
      use Productions;
   begin
      Check (Label & ".Index", Index (Computed), Index (Expected));
      Check (Label & ".LHS", LHS_ID (Computed), LHS_ID (Expected));
      Check (Label & ".RHS", First_Token (Computed), First_Token (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generators.LRk.Item_Lookahead_Ptr;
      Expected : in LALR_Generators.LRk.Item_Lookahead_Ptr)
   is
      use AUnit.Check;
      use LALR_Generators.LRk;
      Computed_I : Item_Lookahead_Ptr := Computed;
      Expected_I : Item_Lookahead_Ptr := Expected;
      Index : Integer := 1;
   begin
      loop
         if Computed_I = null then
            Check (Label & " = null", Expected_I = null, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), Computed_I.Lookaheads (1), Expected_I.Lookaheads (1));
         Check
           (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generators.LRk.Item_Ptr;
      Expected : in LALR_Generators.LRk.Item_Ptr)
   is
      use AUnit.Check;
      use LALR_Generators.LRk;
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
      Computed : in LALR_Generators.LRk.Item_Set;
      Expected : in LALR_Generators.LRk.Item_Set)
   is
      use AUnit.Check;
      use type LALR_Generators.LRk.Set_Reference_Ptr;
      use type LALR_Generators.LRk.Item_Set_Ptr;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generators.LRk.Set_Reference_Ptr;
      Expected : in LALR_Generators.LRk.Set_Reference_Ptr)
   is
      use AUnit.Check;
      use LALR_Generators.LRk;
      Computed_I : Set_Reference_Ptr := Computed;
      Expected_I : Set_Reference_Ptr := Expected;
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

   function Get_Production (Prod : in Integer) return Productions.Instance
   is
      Grammar_I : Production_Lists.List_Iterator := Grammar.Initial_Iterator;
   begin
      for I in 2 .. Prod loop
         Production_Lists.Next_Production (Grammar_I);
      end loop;

      return Production_Lists.Get_Production (Grammar_I);
   end Get_Production;

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LALR_Generators.LRk.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LALR_Generators.LRk.Item_Ptr := null;
      State      : in LALRs.Unknown_State_Index    := LALRs.Unknown_State)
     return LALR_Generators.LRk.Item_Ptr
   is
      Grammar_I : Production_Lists.List_Iterator := Grammar.Initial_Iterator;

      Dot_I : Token_Lists.List_Iterator;
   begin
      for I in 2 .. Prod loop
         Production_Lists.Next_Production (Grammar_I);
      end loop;

      Dot_I := Productions.First_Token (Production_Lists.Get_Production (Grammar_I));
      for I in 2 .. Dot loop
         Token_Lists.Next_Token (Dot_I);
      end loop;

      return new LALR_Generators.LRk.Item_Node'
        (Prod       => Production_Lists.Get_Production (Grammar_I),
         Dot        => Dot_I,
         State      => State,
         Lookaheads => Lookaheads,
         Next       => Next);
   end Get_Item_Node;

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LALR_Generators.LRk.Item_Set_Ptr)
     return LALR_Generators.LRk.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Prod       => Prod,
            Lookaheads => null,
            Dot        => Dot,
            Next       => null),
         Goto_List       => null,
         State           => LALRs.Unknown_State,
         Next            => Next);
   end Get_Item_Set;

   function "+" (Item : in Token_Array) return LALR_Generators.LRk.Item_Lookahead_Ptr
   is
      use LALR_Generators.LRk;
      Result : Item_Lookahead_Ptr;
   begin
      for I in reverse Item'Range loop
         Result := new Item_Lookahead'
           (Last       => 1,
            Lookaheads => (1 => Item (I)),
            Next       => Result);
      end loop;
      return Result;
   end "+";

   procedure Check
     (Label    : in String;
      Computed : in LALRs.Parse_Action_Rec;
      Expected : in LALRs.Parse_Action_Rec)
   is
      use AUnit.Check;
      use LALRs;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".State", Computed.State, Expected.State);
      when Reduce | Accept_It =>
         Check (Label & ".LHS", Tokens_Pkg.ID (Computed.LHS.all), Tokens_Pkg.ID (Expected.LHS.all));
         --  Ignoring Action
         Check (Label & ".Index", Computed.Index, Expected.Index);
         Check (Label & ".Token_Count", Computed.Token_Count, Expected.Token_Count);
      when Error =>
         null;
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LALRs.Parse_Action_Node_Ptr;
      Expected : in LALRs.Parse_Action_Node_Ptr)
   is
      use AUnit.Check;
      use type LALRs.Parse_Action_Node_Ptr;
      Computed_I : LALRs.Parse_Action_Node_Ptr := Computed;
      Expected_I : LALRs.Parse_Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LALRs.Action_Node_Ptr; Expected : in LALRs.Action_Node_Ptr)
   is
      use AUnit.Check;
      use type LALRs.Action_Node_Ptr;
      Computed_I : LALRs.Action_Node_Ptr := Computed;
      Expected_I : LALRs.Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LALRs.Goto_Node_Ptr; Expected : in LALRs.Goto_Node_Ptr)
   is
      use AUnit.Check;
      use type LALRs.Goto_Node_Ptr;
      Computed_I : LALRs.Goto_Node_Ptr := Computed;
      Expected_I : LALRs.Goto_Node_Ptr := Expected;
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
      Computed : in LALRs.Parse_State;
      Expected : in LALRs.Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

end Gen_OpenToken_AUnit;
