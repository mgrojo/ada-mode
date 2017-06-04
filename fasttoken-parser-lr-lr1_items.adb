--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2008, 2009, 2012 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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
package body FastToken.Parser.LR.LR1_Items is
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
     (Grammar              : in Production.List.Instance;
      Descriptor           : in FastToken.Descriptor'Class;
      Has_Empty_Production : in Token_ID_Set;
      Non_Terminal         : in Token_ID;
      Trace                : in Boolean)
     return Token_ID_Set
   is
      use Token.List;
      use Token;
      use all type Production.List.List_Iterator;

      Prod_Iterator  : Production.List.List_Iterator;
      Token_Iterator : List_Iterator;
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

         Prod_Iterator := First (Grammar);
         while not Is_Done (Prod_Iterator) loop
            if Search_Tokens (Current (Prod_Iterator).LHS) then
               Token_Iterator := First (Current (Prod_Iterator).RHS.Tokens);
               loop
                  if Token_Iterator = Null_Iterator then
                     exit;
                  else
                     Derived_Token := ID (Token_Iterator);

                     if not Derivations (Derived_Token) then
                        Added_Tokens (Derived_Token) := True;
                     end if;

                     if (Non_Terminals (Derived_Token) and then Has_Empty_Production (Derived_Token)) and
                       Next (Token_Iterator) /= Null_Iterator
                     then
                        Token_Iterator := Next (Token_Iterator);
                     else
                        exit;
                     end if;
                  end if;
               end loop;
            end if;

            Next (Prod_Iterator);
         end loop;

         if Trace then
            if Any (Added_Tokens) then
               Ada.Text_IO.Put_Line
                 (Image (Descriptor, Non_Terminal) & ": adding " & Image (Descriptor, Added_Tokens));
            end if;
         end if;

         Derivations   := Derivations or Added_Tokens;
         Search_Tokens := Added_Tokens and Non_Terminals;
      end loop;

      return Derivations;
   end First;

   function First
     (Grammar              : in Production.List.Instance;
      Descriptor           : in FastToken.Descriptor'Class;
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
     (Grammar    : in Production.List.Instance;
      Descriptor : in FastToken.Descriptor'Class)
     return Token_ID_Set
   is
      use type Token.List.List_Iterator;
      Result : Token_ID_Set := (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => False);
      Prod_I : Production.List.List_Iterator := Production.List.First (Grammar);
      Prod   : Production.Instance;
      RHS_I  : Token.List.List_Iterator;
   begin
      while not Production.List.Is_Done (Prod_I) loop
         Prod  := Production.List.Current (Prod_I);
         RHS_I := Prod.RHS.Tokens.First;

         if RHS_I = Token.List.Null_Iterator then
            Result (Prod.LHS) := True;
         end if;
         Production.List.Next (Prod_I);
      end loop;
      return Result;
   end Has_Empty_Production;

   function Follow
     (Grammar              : in Production.List.Instance;
      Descriptor           : in FastToken.Descriptor'Class;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set
   is
      use Token;
      use all type Production.List.List_Iterator;
      use all type Production.Instance;
      use all type Token.List.List_Iterator;

      Prev_Result : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      Result : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      A           : Production.List.List_Iterator;
      RHS_I       : Token.List.List_Iterator;
      Token       : Token_ID;
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
         A := Production.List.First (Grammar);
         while not Is_Null (A) loop
            RHS_I := Current (A).RHS.Tokens.First;
            while not Is_Null (RHS_I) loop
               if ID (RHS_I) = B then
                  if not Is_Null (Next (RHS_I)) then
                     --  Rule 1
                     Token := ID (Next (RHS_I));
                     if Token in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                        Result (B, Token) := True;
                     else
                        Or_Slice (Result, B, Slice (First, Token));
                     end if;
                  end if;
               end if;
               Next (RHS_I);
            end loop;
            Production.List.Next (A);
         end loop;
      end loop;

      Prev_Result := Result;
      loop
         for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
            A := Production.List.First (Grammar);
            while not Is_Null (A) loop
               RHS_I := Current (A).RHS.Tokens.First;

               while not Is_Null (RHS_I) loop
                  if ID (RHS_I) = B then
                     if Is_Null (Next (RHS_I)) or else
                       (ID (Next (RHS_I)) in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                          Has_Empty_Production (ID (Next (RHS_I))))
                     then
                        --  rule 3
                        Or_Slice (Result, B, Slice (Result, LHS (A)));
                     end if;
                  end if;
                  Next (RHS_I);
               end loop;
               Production.List.Next (A);
            end loop;
         end loop;

         exit when Prev_Result = Result;
         Prev_Result := Result;
      end loop;
      return Result;
   end Follow;

   function Prod (Item : in Item_Ptr) return Production.Instance
   is begin
      return Item.Prod;
   end Prod;

   function LHS (Item : in Item_Ptr) return Token_ID
   is begin
      return Item.Prod.LHS;
   end LHS;

   function RHS (Item : in Item_Ptr) return Production.Right_Hand_Side
   is begin
      return Item.Prod.RHS;
   end RHS;

   function Dot (Item : in Item_Ptr) return Token.List.List_Iterator
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
     (Prod       : in Production.Instance;
      Dot        : in Token.List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead)
     return Item_Ptr
   is begin
      return new Item_Node'(Prod, Dot, State, new Lookahead'(Lookaheads), null);
   end New_Item_Node;

   procedure Set
     (Item       : in out Item_Node;
      Prod       : in     Production.Instance;
      Dot        : in     Token.List.List_Iterator;
      State      : in     Unknown_State_Index;
      Lookaheads : in     Lookahead)
   is begin
      Item := (Prod, Dot, State, new Lookahead'(Lookaheads), Item.Next);
   end Set;

   procedure Add
     (List : in out Item_Ptr;
      Item : in     Item_Ptr)
   is
      use all type Token_ID;
      New_Item : Item_Ptr renames Item;
      I        : Item_Ptr;
   begin
      if List = null then
         List := New_Item;
      else
         if List.Prod.LHS > Item.Prod.LHS then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or else I.Next.Prod.LHS > Item.Prod.LHS;
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
      Exclude_Propagate : in     Boolean)
   is
      --  FIXME: do we need exclude_propagate? not defined for LR1
      pragma Unreferenced (Exclude_Propagate);
   begin
      Added := False;

      for I in Set'Range loop
         if Value (I) then
            Added := Added or not Set (I);
            Set (I) := True;
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
      Exclude_Propagate : in     Boolean)
   is
      Added : Boolean;
   begin
      Include (Set, Value, Added, Exclude_Propagate);
   end Include;

   procedure Include
     (Item  : in Item_Ptr;
      Value : in Token_ID)
   is begin
      Include (Item.Lookaheads.all, Value);
   end Include;

   procedure Include
     (Item              : in Item_Ptr;
      Value             : in Lookahead;
      Exclude_Propagate : in Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Exclude_Propagate);
   end Include;

   procedure Include
     (Item              : in     Item_Ptr;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Exclude_Propagate : in     Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Added, Exclude_Propagate);
   end Include;

   function Symbol (List : in Goto_Item_Ptr) return Token_ID
   is begin
      return List.Symbol;
   end Symbol;

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
     (Prod             : in     Production.Instance;
      Dot              : in     Token.List.List_Iterator;
      Right            : in     Item_Set;
      Lookaheads       : access Lookahead := null;
      Match_Lookaheads : in     Boolean)
     return Item_Ptr
   is
      use type Production.Instance;
      use type Token.List.List_Iterator;
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
     (Prod         : in     Production.Instance;
      Dot          : in     Token.List.List_Iterator;
      State        : in     Unknown_State_Index;
      Lookaheads   : in     Lookahead;
      Existing_Set : in out Item_Set)
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
            New_Item_Node (Prod, Dot, State, Lookaheads));

         Modified := True;
      else
         Include (Found.Lookaheads.all, Lookaheads, Modified, Exclude_Propagate => False);
      end if;

      return Modified;
   end Merge;

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in Production.List.Instance;
      Descriptor           : in FastToken.Descriptor'Class;
      Trace                : in Boolean)
     return Item_Set
   is
      use all type Token_ID;
      use all type Token.List.List_Iterator;
      use all type Production.List.List_Iterator;
      --  Can't 'use' Production.List or Token.List; they hide each other.

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead.

      I : Item_Set; --  The result.

      Item       : Item_Ptr := Set.Set;           -- iterator 'for each item in I'
      B          : Production.List.List_Iterator; -- iterator 'for each production in G'
      Added_Item : Boolean  := False;             -- 'until no more items can be added'

      Beta : Token.List.List_Iterator;
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
         if Item.Dot /= Token.List.Null_Iterator and then
           Token.List.ID (Item.Dot) in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal
         then
            Beta := Next (Item.Dot); -- tokens after nonterminal, possibly null

            B := Production.List.First (Grammar);
            For_Each_Production :
            while not Production.List.Is_Done (B) loop
               if LHS (B) = ID (Item.Dot) then
                  --  Compute FIRST (<tail of right hand side> a); loop
                  --  until find a terminal, a nonterminal that
                  --  cannot be empty, or end of production, adding
                  --  items on the way.
                  First_Tail :
                  loop
                     if Beta = Token.List.Null_Iterator then
                        --  Use FIRST (a); a = Item.Lookaheads.
                        --  Lookaheads are all terminals, so
                        --  FIRST (a) = a.
                        Added_Item := Added_Item or
                          Merge (Current (B), RHS (B).Tokens.First, Set.State, Item.Lookaheads.all, I);

                        exit First_Tail;

                     elsif Token.List.ID (Beta) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                        --  FIRST (Beta) = Beta
                        Added_Item := Added_Item or Merge
                          (Current (B), RHS (B).Tokens.First, Set.State,
                           To_Lookahead (Descriptor, Token.List.ID (Beta)),
                           I);
                        exit First_Tail;

                     else
                        --  Beta is a nonterminal; use FIRST (Beta)
                        for Terminal in Descriptor.First_Terminal .. Descriptor.Last_Terminal loop
                           if First (Token.List.ID (Beta), Terminal) then
                              Added_Item := Added_Item or
                                Merge
                                  (Current (B), RHS (B).Tokens.First, Set.State,
                                   To_Lookahead (Descriptor, Terminal),
                                   I);
                           end if;
                        end loop;

                        if Has_Empty_Production (Token.List.ID (Beta)) then
                           --  Process the next token in the tail, or a
                           Beta := Next (Beta);
                        else
                           exit First_Tail;
                        end if;
                     end if;
                  end loop First_Tail;

                  Beta := Next (Item.Dot);
               end if;

               Production.List.Next (B);
            end loop For_Each_Production;
         end if; -- Dot is at non-terminal

         if Item.Next = null then
            exit For_Each_Item when not Added_Item;

            Item       := I.Set;
            Added_Item := False;

            if Trace then
               Ada.Text_IO.Put_Line ("I:");
               Put (Descriptor, I);
               Ada.Text_IO.New_Line;
            end if;
         else
            Item := Item.Next;
         end if;
      end loop For_Each_Item;

      return I;
   end Closure;

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

   function In_Kernel (Descriptor : in FastToken.Descriptor'Class; Item : in Item_Ptr) return Boolean
   is
      use Token.List;
   begin
      return
        Null_Iterator /= First (Item.Prod.RHS.Tokens) and
        (Item.Dot = Null_Iterator or else
           ((Item.Prod.LHS = Descriptor.Accept_ID and
               Item.Dot = First (Item.Prod.RHS.Tokens))
              -- Start symbol production with dot before first token.
              or
              Item.Dot /= First (Item.Prod.RHS.Tokens)));
   end In_Kernel;

   function Filter
     (Set : in Item_Set;
     Include : access function (Item : in Item_Ptr) return Boolean)
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
        (not Include (I))
      loop
         I := I.Next;
      end loop;

      if I /= null then
         Result.Set       := new Item_Node'(I.all);
         Result_Tail      := Result.Set;
         Result_Tail.Next := null;

         while I.Next /= null loop
            if Include (I.Next) then
               Result_Tail.Next      := new Item_Node'(I.Next.all);
               Result_Tail.Next.Next := null;
            end if;
            I := I.Next;
         end loop;
      end if;
      return Result;
   end Filter;

   function Image
     (Descriptor      : in FastToken.Descriptor'Class;
      Item            : in Item_Node;
      Show_State      : in Boolean;
      Show_Lookaheads : in Boolean)
     return String
   is
      use Token.List;

      I : List_Iterator;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Image (Descriptor, Item.Prod.LHS)) &
          " <=";
   begin
      I := First (Item.Prod.RHS.Tokens);

      while I /= Null_Iterator loop
         if I = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Image (Descriptor, ID (I));
         Next (I);
      end loop;

      if Item.Dot = Null_Iterator then
         Result := Result & " ^";
      end if;

      if Show_State and Item.State /= Unknown_State then
         Result := Result & " in " & Unknown_State_Index'Image (Item.State);
      end if;

      if Show_Lookaheads then
         Result := Result & ", " & Lookahead_Image (Descriptor, Item.Lookaheads.all);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   procedure Put
     (Descriptor      : in FastToken.Descriptor'Class;
      Item            : in Item_Ptr;
      Show_Lookaheads : in Boolean)
   is begin
      Ada.Text_IO.Put (Image (Descriptor, Item.all, Show_State => True, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put
     (Descriptor : in FastToken.Descriptor'Class;
      Item       : in Goto_Item_Ptr)
   is
      use Ada.Text_IO;
      Reference : Goto_Item_Ptr := Item;
   begin
      while Reference /= null loop
         Put_Line
           ("      on " & Image (Descriptor, Reference.Symbol) &
              " => State" & Unknown_State_Index'Image (Reference.Set.State));

         Reference := Reference.Next;
      end loop;
   end Put;

   procedure Put
     (Descriptor      : in FastToken.Descriptor'Class;
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
           In_Kernel (Descriptor, Set)
         then
            Put_Line ("  " & Image (Descriptor, Set.all, Show_State => False, Show_Lookaheads => Show_Lookaheads));
         end if;

         Set := Set.Next;
      end loop;

      if Show_Goto_List then
         Put (Descriptor, Item.Goto_List);
      end if;
   end Put;

   procedure Put
     (Descriptor      : in FastToken.Descriptor'Class;
      Item            : in Item_Set_Ptr;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
      Set : Item_Set_Ptr := Item;
   begin
      while Set /= null loop
         Put (Descriptor, Set.all, Show_Lookaheads);
         Put_Line ("   Goto:");
         Put (Descriptor, Set.Goto_List);

         Set := Set.Next;
      end loop;
   end Put;

   procedure Put
     (Descriptor      : in FastToken.Descriptor'Class;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Size :" & Unknown_State_Index'Image (Item.Size));
      Put (Descriptor, Item.Head, Show_Lookaheads);
   end Put;

end FastToken.Parser.LR.LR1_Items;
--  Local Variables:
--  jit-lock-defer-time: 0.5
--  End:
