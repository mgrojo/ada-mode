--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2008, 2009, 2012 - 2015, 2017, 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Text_IO;
with Ada.Strings.Unbounded;
package body WisiToken.LR.LR1_Items is
   use type Ada.Strings.Unbounded.Unbounded_String;

   function Reverse_List (List : in Item_Ptr) return Item_Ptr
   is
      I      : Item_Ptr := List;
      Result : Item_Ptr := null;
      Next_1 : Item_Ptr;
      Next_2 : Item_Ptr;
   begin
      while I /= null loop
         Next_1      := Result;
         Next_2      := I.Next;
         Result      := I;
         Result.Next := Next_1;
         I           := Next_2;
      end loop;
      return Result;
   end Reverse_List;

   function Reverse_List (List : in Item_Set_List) return Item_Set_List
   is
      I      : Item_Set_Ptr := List.Head;
      Result : Item_Set_Ptr := null;
      Next_1 : Item_Set_Ptr;
      Next_2 : Item_Set_Ptr;
   begin
      while I /= null loop
         Next_1      := Result;
         Next_2      := I.Next;
         Result      := I;
         Result.Next := Next_1;
         I           := Next_2;
      end loop;
      return (Result, List.Size);
   end Reverse_List;

   function Deep_Copy (List : in Item_Ptr) return Item_Ptr
   is
      I      : Item_Ptr := List;
      Result : Item_Ptr;
   begin
      while I /= null loop
         Result := new Item_Node'(I.Prod, I.Dot, I.State, I.Lookaheads, Result);
         I := I.Next;
      end loop;

      return Reverse_List (Result);
   end Deep_Copy;

   function Deep_Copy (List : in Goto_Item_Ptr) return Goto_Item_Ptr
   is
      I      : Goto_Item_Ptr := List;
      Result : Goto_Item_Ptr;
      Next_1 : Goto_Item_Ptr;
      Next_2 : Goto_Item_Ptr;
   begin
      while I /= null loop
         Result := new Goto_Item'(I.Symbol, I.Set, Result);
         I := I.Next;
      end loop;

      --  Reverse order in Result so original order is preserved
      I      := Result;
      Result := null;

      while I /= null loop
         Next_1      := Result;
         Next_2      := I.Next;
         Result      := I;
         Result.Next := Next_1;
         I           := Next_2;
      end loop;
      return Result;
   end Deep_Copy;

   function First
     (Grammar              : in WisiToken.Productions.Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      Has_Empty_Production : in Token_ID_Set;
      Non_Terminal         : in Token_ID;
      Trace                : in Boolean)
     return Token_ID_Set
   is
      use WisiToken.Productions.Token_ID_Lists;

      Token_Iterator : Cursor;
      Derived_Token  : Token_ID;
      Derivations    : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);
      Added_Tokens   : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);
      Search_Tokens  : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      function Compute_Non_Terminals return Token_ID_Set
      is
         Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);
      begin
         --  Can't use a simple aggregate for this; bounds are non-static.
         Result (Descriptor.First_Terminal .. Descriptor.Last_Terminal) := (others => False);
         Result (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal)   := (others => True);
         return Result;
      end Compute_Non_Terminals;

      Non_Terminals : constant Token_ID_Set := Compute_Non_Terminals;

   begin

      Search_Tokens (Non_Terminal) := True;

      while Any (Search_Tokens) loop

         Added_Tokens := (others => False);

         for Prod of Grammar loop
            if Search_Tokens (Prod.LHS) then
               Token_Iterator := First (Prod.RHS.Tokens);
               loop
                  if Token_Iterator = No_Element then
                     exit;
                  else
                     Derived_Token := Element (Token_Iterator);

                     if not Derivations (Derived_Token) then
                        Added_Tokens (Derived_Token) := True;
                     end if;

                     if (Non_Terminals (Derived_Token) and then Has_Empty_Production (Derived_Token)) and
                       Next (Token_Iterator) /= No_Element
                     then
                        Token_Iterator := Next (Token_Iterator);
                     else
                        exit;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;

         if Trace then
            if Any (Added_Tokens) then
               Ada.Text_IO.Put_Line
                 (Image (Non_Terminal, Descriptor) & ": adding " & Image (Added_Tokens, Descriptor));
            end if;
         end if;

         Derivations   := Derivations or Added_Tokens;
         Search_Tokens := Added_Tokens and Non_Terminals;
      end loop;

      return Derivations;
   end First;

   function First
     (Grammar              : in WisiToken.Productions.Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      Has_Empty_Production : in Token_ID_Set;
      Trace                : in Boolean)
     return Token_Array_Token_Set
   is
      Matrix : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False));

      procedure Set_Slice (Matrix : in out Token_Array_Token_Set; I : Token_ID; Value : in Token_ID_Set)
      is begin
         for J in Matrix'Range (2) loop
            Matrix (I, J) := Value (J);
         end loop;
      end Set_Slice;

   begin
      if Trace then
         Ada.Text_IO.Put_Line ("First Derivations:");
      end if;

      for NT_Index in Matrix'Range loop
         Set_Slice (Matrix, NT_Index, First (Grammar, Descriptor, Has_Empty_Production, NT_Index, Trace));
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Matrix;
   end First;

   function Has_Empty_Production
     (Grammar    : in WisiToken.Productions.Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return Token_ID_Set
   is
      use WisiToken.Productions.Token_ID_Lists;
      subtype Nonterminal is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;

      Result  : Token_ID_Set := (Nonterminal => False);
      RHS_I   : Cursor;
      Changed : Boolean      := True;
   begin
      loop
         exit when not Changed;
         Changed := False;

         for Prod of Grammar loop
            RHS_I := Prod.RHS.Tokens.First;

            if (RHS_I = No_Element or else
                  (Element (RHS_I) in Nonterminal and then Result (Element (RHS_I)))) and
              not Result (Prod.LHS)
            then
               Result (Prod.LHS) := True;
               Changed := True;
            end if;
         end loop;
      end loop;
      return Result;
   end Has_Empty_Production;

   function Follow
     (Grammar              : in WisiToken.Productions.Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set
   is
      use WisiToken.Productions.Token_ID_Lists;

      Prev_Result : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      Result : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      RHS_I : Cursor;
      ID    : Token_ID;
   begin
      --  [dragon] pgp 189:
      --
      --  Rule 1 Follow (S, EOF) = True; EOF is explicit in the
      --  start symbol production, so this is covered by Rule 2.
      --
      --  Rule 2: If A => alpha B Beta, add First (Beta) to Follow (B)
      --
      --  Rule 3; if A => alpha B, or A -> alpha B Beta and Beta
      --  can be null, add Follow (A) to Follow (B)
      --
      --  We don't assume any order in the productions list, so we
      --  have to keep applying rule 3 until nothing changes.

      for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         for A of Grammar loop
            RHS_I := A.RHS.Tokens.First;
            while Has_Element (RHS_I) loop
               if Element (RHS_I) = B then
                  if Has_Element (Next (RHS_I)) then
                     --  Rule 1
                     ID := Element (Next (RHS_I));
                     if ID in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                        Result (B, ID) := True;
                     else
                        Or_Slice (Result, B, Slice (First, ID));
                     end if;
                  end if;
               end if;
               Next (RHS_I);
            end loop;
         end loop;
      end loop;

      Prev_Result := Result;
      loop
         for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
            for A of Grammar loop
               RHS_I := A.RHS.Tokens.First;
               while Has_Element (RHS_I) loop
                  if Element (RHS_I) = B then
                     if not Has_Element (Next (RHS_I)) or else
                       (Element (Next (RHS_I)) in
                          Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                          Has_Empty_Production (Element (Next (RHS_I))))
                     then
                        --  rule 3
                        Or_Slice (Result, B, Slice (Result, A.LHS));
                     end if;
                  end if;
                  Next (RHS_I);
               end loop;
            end loop;
         end loop;

         exit when Prev_Result = Result;
         Prev_Result := Result;
      end loop;
      return Result;
   end Follow;

   function Prod_ID (Item : in Item_Ptr) return WisiToken.Production_ID
   is begin
      return Item.Prod;
   end Prod_ID;

   function Dot (Item : in Item_Ptr) return WisiToken.Productions.Token_ID_Lists.Cursor
   is begin
      return Item.Dot;
   end Dot;

   function State (Item : in Item_Ptr) return Unknown_State_Index
   is begin
      return Item.State;
   end State;

   function Lookaheads (Item : in Item_Ptr) return Lookahead
   is begin
      return Item.Lookaheads.all;
   end Lookaheads;

   function Next (Item : in Item_Ptr) return Item_Ptr
   is begin
      return Item.Next;
   end Next;

   function New_Item_Node
     (Prod       : in Production_ID;
      Dot        : in WisiToken.Productions.Token_ID_Lists.Cursor;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead)
     return Item_Ptr
   is begin
      --  This returns a shallow copy of the production; just the list head.
      return new Item_Node'(Prod, Dot, State, new Lookahead'(Lookaheads), null);
   end New_Item_Node;

   procedure Set
     (Item       : in out Item_Node;
      Prod       : in     Production_ID;
      Dot        : in     WisiToken.Productions.Token_ID_Lists.Cursor;
      State      : in     Unknown_State_Index;
      Lookaheads : in     Lookahead)
   is begin
      Item := (Prod, Dot, State, new Lookahead'(Lookaheads), Item.Next);
   end Set;

   procedure Add
     (List    : in out Item_Ptr;
      Item    : in     Item_Ptr;
      Grammar : in     WisiToken.Productions.Arrays.Vector)
   is
      use all type Token_ID;
      New_Item : Item_Ptr renames Item;
      I        : Item_Ptr;
   begin
      if List = null then
         List := New_Item;
      else
         if Grammar (List.Prod).LHS > Grammar (Item.Prod).LHS then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or else Grammar (I.Next.Prod).LHS > Grammar (Item.Prod).LHS;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add;

   procedure Set_State (List : in Item_Ptr; State : in Unknown_State_Index)
   is
      I : Item_Ptr := List;
   begin
      while I /= null loop
         I.State := State;
         I       := I.Next;
      end loop;
   end Set_State;

   function "&" (Left, Right : in Item_Ptr) return Item_Ptr
   is
      I : Item_Ptr;
   begin
      Right.State := Left.State;
      if Left.Next = null then
         Left.Next := Right;
      else
         I := Left.Next;
         while I.Next /= null loop
            I := I.Next;
         end loop;
         I.Next := Right;
      end if;
      return Left;
   end "&";

   procedure Include
     (Set               : in out Lookahead;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is begin
      --   Descriptor is null when Exclude_Propagate is False
      Added := False;

      for I in Set'Range loop
         if Exclude_Propagate and then
           Descriptor.all in LALR_Descriptor and then
           I = LALR_Descriptor (Descriptor.all).Propagate_ID
         then
            null;
         else
            if Value (I) then
               Added := Added or not Set (I);
               Set (I) := True;
            end if;
         end if;
      end loop;
   end Include;

   procedure Include
     (Set   : in out Lookahead;
      Value : in     Token_ID)
   is begin
      Set (Value) := True;
   end Include;

   procedure Include
     (Set               : in out Lookahead;
      Value             : in     Lookahead;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is
      Added : Boolean;
   begin
      Include (Set, Value, Added, Descriptor, Exclude_Propagate);
   end Include;

   procedure Include
     (Item  : in Item_Ptr;
      Value : in Token_ID)
   is begin
      Include (Item.Lookaheads.all, Value);
   end Include;

   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Descriptor, Exclude_Propagate);
   end Include;

   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Added, Descriptor, Exclude_Propagate);
   end Include;

   function Symbol (List : in Goto_Item_Ptr) return Token_ID
   is begin
      return List.Symbol;
   end Symbol;

   function Prod_ID (List : in Goto_Item_Ptr) return Production_ID
   is begin
      return List.Set.Set.Prod;
   end Prod_ID;

   function State (List : in Goto_Item_Ptr) return Unknown_State_Index
   is begin
      return List.Set.State;
   end State;

   function Next (List : in Goto_Item_Ptr) return Goto_Item_Ptr
   is begin
      return List.Next;
   end Next;

   function New_Goto_Item
     (Symbol : in     Token_ID;
      Set    : in     Item_Set_Ptr)
     return Goto_Item_Ptr
   is begin
      return new Goto_Item'(Symbol, Set, null);
   end New_Goto_Item;

   procedure Add
     (List   : in out Goto_Item_Ptr;
      Symbol : in     Token_ID;
      Set    : in     Item_Set_Ptr)
   is
      use all type Token_ID;
      New_Item : constant Goto_Item_Ptr := new Goto_Item'(Symbol, Set, null);
      I        : Goto_Item_Ptr;
   begin
      if List = null then
         List := New_Item;
      else
         if List.Symbol > Symbol then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or else I.Next.Symbol > Symbol;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set)
   is begin
      Target.Set := new Item_Node'
        (Prod       => New_Item.Prod,
         Dot        => New_Item.Dot,
         State      => Target.State,
         Lookaheads => New_Item.Lookaheads,
         Next       => Target.Set);
   end Add;

   function Find
     (Prod             : in     Production_ID;
      Dot              : in     WisiToken.Productions.Token_ID_Lists.Cursor;
      Right            : in     Item_Set;
      Lookaheads       : access Lookahead := null;
      Match_Lookaheads : in     Boolean)
     return Item_Ptr
   is
      use all type WisiToken.Productions.Token_ID_Lists.Cursor;
      Current : Item_Ptr := Right.Set;
   begin
      while Current /= null loop
         if Prod = Current.Prod and
           Dot = Current.Dot and
           (not Match_Lookaheads or else
              Lookaheads.all = Current.Lookaheads.all)
         then
            return Current;
         end if;
         Current := Current.Next;
      end loop;
      return null;
   end Find;

   function Find
     (Left             : in Item_Set;
      Right            : in Item_Set_List;
      Match_Lookaheads : in Boolean)
     return Item_Set_Ptr
   is
      Right_Set  : Item_Set_Ptr := Right.Head;
      Right_Item : Item_Ptr;
      Left_Size  : Natural      := 0;
      Right_Size : Natural;
   begin
      Right_Item := Left.Set;
      while Right_Item /= null loop
         Left_Size := Left_Size + 1;
         Right_Item := Right_Item.Next;
      end loop;

      while Right_Set /= null loop

         Right_Item := Right_Set.Set;
         Right_Size := 0;
         while Right_Item /= null loop

            if Find (Right_Item.Prod, Right_Item.Dot, Left, Right_Item.Lookaheads, Match_Lookaheads) = null then
               exit;
            end if;

            Right_Size := Right_Size + 1;
            Right_Item := Right_Item.Next;
         end loop;

         if Right_Item = null and Left_Size = Right_Size then
            return Right_Set;
         end if;

         Right_Set := Right_Set.Next;
      end loop;

      return null;
   end Find;

   function Find
     (State : in Unknown_State_Index;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr
   is
      Set : Item_Set_Ptr := Sets.Head;
   begin
      while Set /= null loop
         if Set.State = State then
            return Set;
         end if;

         Set := Set.Next;
      end loop;

      return null;
   end Find;

   function Is_In
     (Symbol    : in Token_ID;
      Set       : in Item_Set_Ptr;
      Goto_List : in Goto_Item_Ptr)
     return Boolean
   is
      Goto_Ptr : Goto_Item_Ptr := Goto_List;
      use type Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Set = Set and Goto_Ptr.Symbol = Symbol then
            return True;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return False;
   end Is_In;

   function Goto_Set
     (From   : in Item_Set;
      Symbol : in Token_ID)
     return Item_Set_Ptr
   is
      Goto_Ptr : Goto_Item_Ptr := From.Goto_List;
      use type Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Symbol = Symbol then
            return Goto_Ptr.Set;
         end if;

         Goto_Ptr := Goto_Ptr.Next;
      end loop;

      return null;
   end Goto_Set;

   function Merge
     (Prod         : in     Production_ID;
      Dot          : in     WisiToken.Productions.Token_ID_Lists.Cursor;
      State        : in     Unknown_State_Index;
      Lookaheads   : in     Lookahead;
      Existing_Set : in out Item_Set;
      Grammar      : in     WisiToken.Productions.Arrays.Vector)
     return Boolean
   is
      --  Merge item into Existing_Set. Return True if Existing_Set
      --  is modified.

      Found    : constant Item_Ptr := Find (Prod, Dot, Existing_Set, Match_Lookaheads => False);
      Modified : Boolean           := False;
   begin
      if Found = null then
         Add
           (Existing_Set.Set,
            New_Item_Node (Prod, Dot, State, Lookaheads),
            Grammar);

         Modified := True;
      else
         Include (Found.Lookaheads.all, Lookaheads, Modified, null, Exclude_Propagate => False);
      end if;

      return Modified;
   end Merge;

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      Trace                : in Boolean)
     return Item_Set
   is
      use all type Token_ID;
      use WisiToken.Productions.Token_ID_Lists;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead.

      I : Item_Set; --  The result.

      Item       : Item_Ptr := Set.Set; -- iterator 'for each item in I'
      Added_Item : Boolean  := False;   -- 'until no more items can be added'

      Beta : Cursor;
   begin
      --  Copy Set into I
      I.State     := Set.State;
      I.Goto_List := Deep_Copy (Set.Goto_List);
      I.Set       := Deep_Copy (Set.Set);

      Item := I.Set;
      For_Each_Item :
      loop
         --  An item has the structure [A -> alpha Dot B Beta, a].
         --
         --  If B is a nonterminal, find its productions and place
         --  them in the set with lookaheads from FIRST(Beta a).
         if Item.Dot /= No_Element and then
           Element (Item.Dot) in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal
         then
            Beta := Next (Item.Dot); -- tokens after nonterminal, possibly null

            For_Each_Production :
            for B in Grammar.First_Index .. Grammar.Last_Index loop
               if Grammar (B).LHS = Element (Item.Dot) then
                  --  Compute FIRST (<tail of right hand side> a); loop
                  --  until find a terminal, a nonterminal that
                  --  cannot be empty, or end of production, adding
                  --  items on the way.
                  First_Tail :
                  loop
                     if Beta = No_Element then
                        --  Use FIRST (a); a = Item.Lookaheads.
                        --  Lookaheads are all terminals, so
                        --  FIRST (a) = a.
                        Added_Item := Added_Item or
                          Merge (B, Grammar (B).RHS.Tokens.First, Set.State, Item.Lookaheads.all, I, Grammar);

                        exit First_Tail;

                     elsif Element (Beta) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                        --  FIRST (Beta) = Beta
                        Added_Item := Added_Item or Merge
                          (B, Grammar (B).RHS.Tokens.First, Set.State,
                           To_Lookahead (Element (Beta), Descriptor),
                           I, Grammar);
                        exit First_Tail;

                     else
                        --  Beta is a nonterminal; use FIRST (Beta)
                        for Terminal in Descriptor.First_Terminal .. Descriptor.Last_Terminal loop
                           if First (Element (Beta), Terminal) then
                              Added_Item := Added_Item or
                                Merge
                                  (B, Grammar (B).RHS.Tokens.First, Set.State,
                                   To_Lookahead (Terminal, Descriptor),
                                   I, Grammar);
                           end if;
                        end loop;

                        if Has_Empty_Production (Element (Beta)) then
                           --  Process the next token in the tail, or a
                           Beta := Next (Beta);
                        else
                           exit First_Tail;
                        end if;
                     end if;
                  end loop First_Tail;

                  Beta := Next (Item.Dot);
               end if;
            end loop For_Each_Production;
         end if; -- Dot is at non-terminal

         if Item.Next = null then
            exit For_Each_Item when not Added_Item;

            Item       := I.Set;
            Added_Item := False;

            if Trace then
               Ada.Text_IO.Put_Line ("I:");
               Put (Grammar, Descriptor, I);
               Ada.Text_IO.New_Line;
            end if;
         else
            Item := Item.Next;
         end if;
      end loop For_Each_Item;

      return I;
   end Closure;

   function Productions (Set : in Item_Set) return Production_ID_Arrays.Vector
   is
      I : Item_Ptr := Set.Set;
   begin
      return Result : Production_ID_Arrays.Vector do
         loop
            exit when I = null;
            Result.Append (I.Prod);
            I := I.Next;
         end loop;
      end return;
   end Productions;

   procedure Free (Item : in out Item_Set)
   is
      I        : Item_Ptr      := Item.Set;
      Goto_Set : Goto_Item_Ptr := Item.Goto_List;
   begin
      while I /= null loop
         Item.Set := I.Next;
         Free (I);
         I := Item.Set;
      end loop;

      while Goto_Set /= null loop
         Item.Goto_List := Goto_Set.Next;
         Free (Goto_Set);
         Goto_Set := Item.Goto_List;
      end loop;

   end Free;

   procedure Free (Item : in out Item_Set_List)
   is
      Set : Item_Set_Ptr := Item.Head;
   begin
      while Set /= null loop
         Item.Head := Set.Next;

         Free (Set.all);
         Free (Set);

         Set := Item.Head;
      end loop;
   end Free;

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Item_Ptr)
     return Boolean
   is
      use WisiToken.Productions.Token_ID_Lists;
      Prod : WisiToken.Productions.Instance renames Grammar (Item.Prod);
   begin
      return
        No_Element /= First (Prod.RHS.Tokens) and
        (Item.Dot = No_Element or else
           ((Prod.LHS = Descriptor.Accept_ID and
               Item.Dot = First (Prod.RHS.Tokens))
              -- Start symbol production with dot before first token.
              or
              Item.Dot /= First (Prod.RHS.Tokens)));
   end In_Kernel;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor'Class;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Arrays.Vector;
         Descriptor : in WisiToken.Descriptor'Class;
         Item       : in Item_Ptr)
        return Boolean)
     return Item_Set
   is
      Result      : Item_Set;
      Result_Tail : Item_Ptr;
      I           : Item_Ptr := Set.Set;
   begin
      Result :=
        (Set       => null,
         Goto_List => Deep_Copy (Set.Goto_List),
         State     => Set.State,
         Next      => null);

      --  Walk I thru Set.Set, copying Include items to Result_Tail.
      while I /= null and then
        (not Include (Grammar, Descriptor, I))
      loop
         I := I.Next;
      end loop;

      if I /= null then
         Result.Set       := new Item_Node'(I.all);
         Result_Tail      := Result.Set;
         Result_Tail.Next := null;

         while I.Next /= null loop
            if Include (Grammar, Descriptor, I.Next) then
               Result_Tail.Next      := new Item_Node'(I.Next.all);
               Result_Tail.Next.Next := null;
            end if;
            I := I.Next;
         end loop;
      end if;
      return Result;
   end Filter;

   function Image
     (Grammar         : in WisiToken.Productions.Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Node;
      Show_State      : in Boolean;
      Show_Lookaheads : in Boolean)
     return String
   is
      use WisiToken.Productions.Token_ID_Lists;

      I : Cursor;

      Prod   : WisiToken.Productions.Instance renames Grammar (Item.Prod);
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        +Padded_Image (Item.Prod, Width => 4) & ":" & Image (Prod.LHS, Descriptor) & " <=";
   begin
      I := First (Prod.RHS.Tokens);

      while I /= No_Element loop
         if I = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Image (Element (I), Descriptor);
         Next (I);
      end loop;

      if Item.Dot = No_Element then
         Result := Result & " ^";
      end if;

      if Show_State and Item.State /= Unknown_State then
         Result := Result & " in " & Unknown_State_Index'Image (Item.State);
      end if;

      if Show_Lookaheads then
         Result := Result & ", " & Lookahead_Image (Item.Lookaheads.all, Descriptor);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   procedure Put
     (Grammar         : in WisiToken.Productions.Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Ptr;
      Show_Lookaheads : in Boolean := True)
   is begin
      Ada.Text_IO.Put (Image (Grammar, Descriptor, Item.all, Show_State => True, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put
     (Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Goto_Item_Ptr)
   is
      use Ada.Text_IO;
      Reference : Goto_Item_Ptr := Item;
   begin
      while Reference /= null loop
         Put_Line
           ("      on " & Image (Reference.Symbol, Descriptor) &
              " => State" & Unknown_State_Index'Image (Reference.Set.State));

         Reference := Reference.Next;
      end loop;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False)
   is
      use Ada.Text_IO;
      Set : Item_Ptr := Item.Set;
   begin
      if Item.State /= Unknown_State then
         Put_Line ("State" & Unknown_State_Index'Image (Item.State) & ":");
      end if;
      while Set /= null loop
         if not Kernel_Only or else
           In_Kernel (Grammar, Descriptor, Set)
         then
            Put_Line
              ("  " & Image (Grammar, Descriptor, Set.all, Show_State => False, Show_Lookaheads => Show_Lookaheads));
         end if;

         Set := Set.Next;
      end loop;

      if Show_Goto_List then
         Put (Descriptor, Item.Goto_List);
      end if;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set_Ptr;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
      Set : Item_Set_Ptr := Item;
   begin
      while Set /= null loop
         Put (Grammar, Descriptor, Set.all, Show_Lookaheads);
         Put_Line ("   Goto:");
         Put (Descriptor, Set.Goto_List);

         Set := Set.Next;
      end loop;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Size :" & Unknown_State_Index'Image (Item.Size));
      Put (Grammar, Descriptor, Item.Head, Show_Lookaheads);
   end Put;

end WisiToken.LR.LR1_Items;
