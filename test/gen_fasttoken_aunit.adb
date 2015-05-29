--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015 Stephen Leake.  All Rights Reserved.
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
      Computed : in LALR_Generator.LRk.Item_Lookahead_Ptr;
      Expected : in LALR_Generator.LRk.Item_Lookahead_Ptr)
   is
      use AUnit.Checks;
      use LALR_Generator.LRk;
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
      Computed : in LALR_Generator.LRk.Item_Ptr;
      Expected : in LALR_Generator.LRk.Item_Ptr)
   is
      use AUnit.Checks;
      use LALR_Generator.LRk;
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
      Computed : in LALR_Generator.LRk.Item_Set;
      Expected : in LALR_Generator.LRk.Item_Set)
   is
      use AUnit.Checks;
      use type LALR_Generator.LRk.Set_Reference_Ptr;
      use type LALR_Generator.LRk.Item_Set_Ptr;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generator.LRk.Set_Reference_Ptr;
      Expected : in LALR_Generator.LRk.Set_Reference_Ptr)
   is
      use AUnit.Checks;
      use LALR_Generator.LRk;
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
      Lookaheads : in LALR_Generator.LRk.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LALR_Generator.LRk.Item_Ptr := null;
      State      : in LALR.Unknown_State_Index    := LALR.Unknown_State)
     return LALR_Generator.LRk.Item_Ptr
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

      return new LALR_Generator.LRk.Item_Node'
        (Prod       => Production.List.Current (Grammar_I),
         Dot        => Dot_I,
         State      => State,
         Lookaheads => Lookaheads,
         Next       => Next);
   end Get_Item_Node;

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LALR_Generator.LRk.Item_Set_Ptr)
     return LALR_Generator.LRk.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Prod       => Prod,
            Lookaheads => null,
            Dot        => Dot,
            Next       => null),
         Goto_List       => null,
         State           => LALR.Unknown_State,
         Next            => Next);
   end Get_Item_Set;

   function "+" (Item : in Token_Array) return LALR_Generator.LRk.Item_Lookahead_Ptr
   is
      use LALR_Generator.LRk;
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
      Computed : in LALR.Parse_Action_Rec;
      Expected : in LALR.Parse_Action_Rec)
   is
      use AUnit.Checks;
      use LALR;
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
      Computed : in LALR.Parse_Action_Node_Ptr;
      Expected : in LALR.Parse_Action_Node_Ptr)
   is
      use AUnit.Checks;
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
      use AUnit.Checks;
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

   procedure Check (Label : in String; Computed : in LALR.Goto_Node_Ptr; Expected : in LALR.Goto_Node_Ptr)
   is
      use AUnit.Checks;
      use type LALR.Goto_Node_Ptr;
      Computed_I : LALR.Goto_Node_Ptr := Computed;
      Expected_I : LALR.Goto_Node_Ptr := Expected;
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
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

end Gen_FastToken_AUnit;
