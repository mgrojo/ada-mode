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
with FastToken.AUnit; use FastToken.AUnit;
package body FastToken_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Token.List.List_Iterator;
      Expected : in FastToken.Token.List.List_Iterator)
   is
      use AUnit.Checks;
      use FastToken.Token;
      use FastToken.Production;
      use FastToken.Token.List;
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
         Next (Computed_I);
         Next (Expected_I);
         Index := Index + 1;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Production.Instance;
      Expected : in FastToken.Production.Instance)
   is
      use AUnit.Checks;
      use FastToken.Token;
   begin
      Check (Label & ".Index", Computed.RHS.Index, Expected.RHS.Index);
      Check (Label & ".LHS", Computed.LHS, Expected.LHS);
      Check (Label & ".RHS", Computed.RHS.Tokens.First, Expected.RHS.Tokens.First);
   end Check;

   procedure Check
     (Label            : in String;
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Ptr;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean)
   is
      use AUnit.Checks;
      use FastToken.Parser.LR.LR1_Items;
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
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Set;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Set;
      Match_Lookaheads : in Boolean := True)
   is
      use AUnit.Checks;
      use type FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      use type FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Set", Computed.Set, Expected.Set, Match_Lookaheads);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Expected : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr)
   is
      use AUnit.Checks;
      use FastToken.Parser.LR.LR1_Items;
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
      Computed         : in FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Expected         : in FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Match_Lookaheads : in Boolean := True)
   is
      use type FastToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      Computed_1 : FastToken.Parser.LR.LR1_Items.Item_Set_Ptr := Computed;
      Expected_1 : FastToken.Parser.LR.LR1_Items.Item_Set_Ptr := Expected;
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
      Computed : in FastToken.Parser.LR.LR1_Items.Item_Set_List;
      Expected : in FastToken.Parser.LR.LR1_Items.Item_Set_List)
   is begin
      Check (Label & ".Size", Computed.Size, Expected.Size);
      Check (Label & ".Head", Computed.Head, Expected.Head, Match_Lookaheads => True);
   end Check;

   function Get_Production
     (Grammar : in FastToken.Production.List.Instance;
      Prod    : in Positive)
     return FastToken.Production.List.List_Iterator
   is
      Grammar_I : FastToken.Production.List.List_Iterator := Grammar.First;
   begin
      for I in 2 .. Prod loop
         FastToken.Production.List.Next (Grammar_I);
      end loop;

      return Grammar_I;
   end Get_Production;

   function Get_Production
     (Grammar : in FastToken.Production.List.Instance;
      Prod    : in Positive)
     return FastToken.Production.Instance
   is begin
      return FastToken.Production.List.Current (Get_Production (Grammar, Prod));
   end Get_Production;

   function Get_Item_Node
     (Grammar    : in FastToken.Production.List.Instance;
      Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in FastToken.Parser.LR.LR1_Items.Lookahead;
      State      : in FastToken.Parser.LR.Unknown_State_Index := FastToken.Parser.LR.Unknown_State)
     return FastToken.Parser.LR.LR1_Items.Item_Ptr
   is
      Grammar_I : FastToken.Production.List.List_Iterator := Grammar.First;

      Dot_I : FastToken.Token.List.List_Iterator;
   begin
      for I in 2 .. Prod loop
         FastToken.Production.List.Next (Grammar_I);
      end loop;

      if FastToken.Production.List.Is_Done (Grammar_I) then
         raise FastToken.Programmer_Error with Integer'Image (Prod) & " > length (grammar)";
      end if;

      Dot_I := FastToken.Production.First_Token (FastToken.Production.List.Current (Grammar_I));
      for I in 2 .. Dot loop
         FastToken.Token.List.Next (Dot_I);
      end loop;

      return FastToken.Parser.LR.LR1_Items.New_Item_Node
        (FastToken.Production.List.Current (Grammar_I), Dot_I, State, Lookaheads);
   end Get_Item_Node;

   function "+" (Item : in FastToken.Parser.LR.LR1_Items.Item_Ptr) return FastToken.Parser.LR.LR1_Items.Item_Set
   is begin
      return FastToken.Parser.LR.LR1_Items.Item_Set'(Item, null, FastToken.Parser.LR.LR1_Items.State (Item), null);
   end "+";

   function "+" (Item : in FastToken.Parser.LR.LR1_Items.Item_Ptr) return FastToken.Parser.LR.LR1_Items.Item_Set_Ptr
   is begin
      return new FastToken.Parser.LR.LR1_Items.Item_Set'(Item, null, FastToken.Parser.LR.LR1_Items.State (Item), null);
   end "+";

   function "+"
     (State : in FastToken.Parser.LR.Unknown_State_Index;
      Item  : in FastToken.Parser.LR.LR1_Items.Item_Ptr)
     return FastToken.Parser.LR.LR1_Items.Item_Set_List
   is begin
      FastToken.Parser.LR.LR1_Items.Set_State (Item, State);
      return
        (Head => new FastToken.Parser.LR.LR1_Items.Item_Set'(Item, null, State, null),
         Size => 1);
   end "+";

   function "&"
     (Left, Right : in FastToken.Parser.LR.LR1_Items.Item_Set_List)
     return FastToken.Parser.LR.LR1_Items.Item_Set_List
   is
      use FastToken.Parser.LR.LR1_Items;
      use all type FastToken.Parser.LR.Unknown_State_Index;

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
     (To_State : in FastToken.Parser.LR.Unknown_State_Index;
      Set_List : in FastToken.Parser.LR.LR1_Items.Item_Set_List)
     return FastToken.Parser.LR.LR1_Items.Item_Set_Ptr
   is
      use FastToken.Parser.LR.LR1_Items;
      use all type FastToken.Parser.LR.Unknown_State_Index;

      I : Item_Set_Ptr := Set_List.Head;
   begin
      loop
         exit when I.State = To_State;
         I := I.Next;
      end loop;
      return I;
   end Get_Set;

   function "+" (Right : in AUnit_Goto_Item) return FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr
   is begin
      return FastToken.Parser.LR.LR1_Items.New_Goto_Item (Right.Symbol, Right.Set);
   end "+";

   function "&"
     (Left  : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr;
      Right : in AUnit_Goto_Item)
     return FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr
   is
      Result : FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr := Left;
   begin
      FastToken.Parser.LR.LR1_Items.Add (Result, Right.Symbol, Right.Set);
      return Left;
   end "&";

   procedure Add_Gotos
     (List  : in FastToken.Parser.LR.LR1_Items.Item_Set_List;
      State : in FastToken.Parser.LR.Unknown_State_Index;
      Gotos : in FastToken.Parser.LR.LR1_Items.Goto_Item_Ptr)
   is
      use FastToken.Parser.LR.LR1_Items;
      use all type FastToken.Parser.LR.Unknown_State_Index;
      I : Item_Set_Ptr := List.Head;
   begin
      loop
         exit when I.State = State;
         I := I.Next;
      end loop;
      I.Goto_List := Gotos;
   end Add_Gotos;

   function Get_Item_Set
     (Grammar   : in FastToken.Production.List.Instance;
      Prod      : in Positive;
      Dot       : in Positive;
      Lookahead : in FastToken.Parser.LR.LR1_Items.Lookahead)
     return FastToken.Parser.LR.LR1_Items.Item_Set
   is begin
      return
        (Set => Get_Item_Node
           (Grammar,
            Prod       => Prod,
            Dot        => Dot,
            Lookaheads => Lookahead),
         Goto_List       => null,
         State           => FastToken.Parser.LR.Unknown_State,
         Next            => null);
   end Get_Item_Set;

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.Parse_Action_Rec;
      Expected : in FastToken.Parser.LR.Parse_Action_Rec)
   is
      use Standard.AUnit.Checks;
      use all type FastToken.Parser.LR.Parse_Action_Verbs;
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
      Computed : in FastToken.Parser.LR.Parse_Action_Node_Ptr;
      Expected : in FastToken.Parser.LR.Parse_Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use type FastToken.Parser.LR.Parse_Action_Node_Ptr;
      Computed_I : FastToken.Parser.LR.Parse_Action_Node_Ptr := Computed;
      Expected_I : FastToken.Parser.LR.Parse_Action_Node_Ptr := Expected;
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
      Computed : in FastToken.Parser.LR.Action_Node_Ptr;
      Expected : in FastToken.Parser.LR.Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use type FastToken.Parser.LR.Action_Node_Ptr;
      Computed_I : FastToken.Parser.LR.Action_Node_Ptr := Computed;
      Expected_I : FastToken.Parser.LR.Action_Node_Ptr := Expected;
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
      Computed : in FastToken.Parser.LR.Goto_Node_Ptr;
      Expected : in FastToken.Parser.LR.Goto_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use all type FastToken.Parser.LR.Goto_Node_Ptr;
      Computed_I : FastToken.Parser.LR.Goto_Node_Ptr := Computed;
      Expected_I : FastToken.Parser.LR.Goto_Node_Ptr := Expected;
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
      Computed : in FastToken.Parser.LR.Parse_State;
      Expected : in FastToken.Parser.LR.Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Parser.LR.Parse_Table;
      Expected : in FastToken.Parser.LR.Parse_Table)
   is begin
      Check (Label & ".States'first", Computed.States'First, Expected.States'First);
      Check (Label & ".States'last", Computed.States'Last, Expected.States'Last);
      for I in Computed.States'Range loop
         Check
           (Label & ".States." & FastToken.Parser.LR.State_Index'Image (I), Computed.States (I), Expected.States (I));
      end loop;
      Check (Label & ".Panic_Recover", Computed.Panic_Recover, Expected.Panic_Recover);
      Check (Label & ".Follow", Computed.Follow, Expected.Follow);
   end Check;

end FastToken_AUnit;
