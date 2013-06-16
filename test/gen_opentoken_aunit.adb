--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013 Stephen Leake.  All Rights Reserved.
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

   procedure Check (Label : in String; Computed : in LALR.LRk.Item_Lookahead_Ptr; Expected : in LALR.LRk.Item_Lookahead_Ptr)
   is
      use AUnit.Check;
      use LALR.LRk;
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

   procedure Check (Label : in String; Computed : in LALR.LRk.Item_Ptr; Expected : in LALR.LRk.Item_Ptr)
   is
      use AUnit.Check;
      use LALR.LRk;
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
         Check (Label & Integer'Image (Index) & ".Index", Computed_I.Index, Expected_I.Index);
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

   procedure Check (Label : in String; Computed : in LALR.LRk.Item_Set; Expected : in LALR.LRk.Item_Set)
   is
      use AUnit.Check;
      use LALR.LRk;
   begin
      Check (Label & ".Index", Computed.Index, Expected.Index);
      Check (Label & ".Set", Computed.Set, Expected.Set);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   procedure Check (Label : in String; Computed : in LALR.LRk.Set_Reference_Ptr; Expected : in LALR.LRk.Set_Reference_Ptr)
   is
      use AUnit.Check;
      use LALR.LRk;
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
         Check (Label & Integer'Image (Index) & ".Set.Index", Computed_I.Set.Index, Expected_I.Set.Index);
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
      Lookaheads : in LALR.LRk.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LALR.LRk.Item_Ptr := null;
      Index      : in Integer           := -1)
     return LALR.LRk.Item_Ptr
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

      return new LALR.LRk.Item_Node'
        (Prod       => Production_Lists.Get_Production (Grammar_I),
         Dot        => Dot_I,
         Index      => Index,
         Lookaheads => Lookaheads,
         Next       => Next);
   end Get_Item_Node;

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LALR.LRk.Item_Set_Ptr)
     return LALR.LRk.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Prod       => Prod,
            Lookaheads => null,
            Dot        => Dot,
            Next       => null),
         Goto_List       => null,
         Index           => -1,
         Next            => Next);
   end Get_Item_Set;

   function "+" (Item : in Token_Array) return LALR.LRk.Item_Lookahead_Ptr
   is
      use LALR.LRk;
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

   procedure Check (Label : in String; Computed : in LALR.Parse_Action_Rec; Expected : in LALR.Parse_Action_Rec)
   is
      use AUnit.Check;
      use LALR;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".State", Computed.State, Expected.State);
      when Reduce | Accept_It =>
         Check (Label & ".Production", Computed.Production, Expected.Production);
         Check (Label & ".Length", Computed.Length, Expected.Length);
      when Error =>
         null;
      end case;
   end Check;

   procedure Check (Label : in String; Computed : in LALR.Parse_Action_Node_Ptr; Expected : in LALR.Parse_Action_Node_Ptr)
   is
      use AUnit.Check;
      use type LALR.Parse_Action_Node_Ptr;
      Computed_I : LALR.Parse_Action_Node_Ptr := Computed;
      Expected_I : LALR.Parse_Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LALR.Action_Node_Ptr; Expected : in LALR.Action_Node_Ptr)
   is
      use AUnit.Check;
      use type LALR.Action_Node_Ptr;
      Computed_I : LALR.Action_Node_Ptr := Computed;
      Expected_I : LALR.Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LALR.Reduction_Node_Ptr; Expected : in LALR.Reduction_Node_Ptr)
   is
      use AUnit.Check;
      use type LALR.Reduction_Node_Ptr;
      Computed_I : LALR.Reduction_Node_Ptr := Computed;
      Expected_I : LALR.Reduction_Node_Ptr := Expected;
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
      Computed : in LALR.Parse_State;
      Expected : in LALR.Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Reduction_List", Computed.Reduction_List, Expected.Reduction_List);
   end Check;

end Gen_OpenToken_AUnit;
