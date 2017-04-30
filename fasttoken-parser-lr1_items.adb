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

with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
package body FastToken.Parser.LR1_Items is
   use type Ada.Strings.Unbounded.Unbounded_String;

   function Compute_Non_Terminals return Token_ID_Set
   is
      Result : Token_ID_Set;
   begin
      Result (Token.Token_ID'First .. Token.Terminal_ID'Last)                      := (others => False);
      Result (Token.Token_ID'Succ (Token.Terminal_ID'Last) .. Token.Token_ID'Last) := (others => True);
      return Result;
   end Compute_Non_Terminals;

   Non_Terminals : constant Token_ID_Set := Compute_Non_Terminals;

   function Image (Item : in Token_ID_Set) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Need_Comma then
               Result := Result & ", ";
            end if;
            Result     := Result & Token.Token_Image (I);
            Need_Comma := True;
         end if;
      end loop;
      return To_String (Result);
   end Image;

   function First
     (Grammar              : in Production.List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Non_Terminal         : in Token.Token_ID;
      Trace                : in Boolean)
     return Token_ID_Set
   is
      use Token.List;

      Prod_Iterator  : Production.List.List_Iterator;
      Token_Iterator : List_Iterator;

      Derived_Token : Token.Token_ID;

      Derivations   : Token_ID_Set := (others => False);
      Added_Tokens  : Token_ID_Set;
      Search_Tokens : Token_ID_Set := (others => False);

   begin

      Search_Tokens (Non_Terminal) := True;

      while Search_Tokens /= Token_ID_Set'(others => False) loop

         Added_Tokens := (others => False);

         --  search all productions for rightmost derivations for
         --  tokens we found last time.
         Prod_Iterator := Production.List.First (Grammar);
         while not Production.List.Is_Done (Prod_Iterator) loop
            if Search_Tokens (Token.ID (Production.List.Current (Prod_Iterator).LHS.all)) then
               Token_Iterator := First
                 (Production.List.Current (Prod_Iterator).RHS.Tokens);

               loop
                  if Token_Iterator /= Null_Iterator then
                     Derived_Token := ID (Token_Iterator);

                     if not Derivations (Derived_Token) then
                        Added_Tokens (Derived_Token) := True;
                     end if;

                     if (Derived_Token in Nonterminal_ID and then Has_Empty_Production (Derived_Token)) and
                       Next (Token_Iterator) /= Null_Iterator
                     then
                        Token_Iterator := Next (Token_Iterator);
                     else
                        exit;
                     end if;
                  else
                     exit;
                  end if;
               end loop;
            end if;

            Production.List.Next (Prod_Iterator);
         end loop;

         if Trace then
            if Added_Tokens /= Token_ID_Set'(others => False) then
               Ada.Text_IO.Put_Line (Token.Token_Image (Non_Terminal) & ": adding " & Image (Added_Tokens));
            end if;
         end if;

         Derivations   := Derivations or Added_Tokens;
         Search_Tokens := Added_Tokens and Non_Terminals;
      end loop;

      return Derivations;
   end First;

   function First
     (Grammar              : in Production.List.Instance;
      Has_Empty_Production : in Nonterminal_ID_Set;
      Trace                : in Boolean)
     return Derivation_Matrix
   is
      Matrix : Derivation_Matrix;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("First Derivations:");
      end if;

      for NT_Index in Matrix'Range loop
         Matrix (NT_Index) := First (Grammar, Has_Empty_Production, NT_Index, Trace);
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Matrix;
   end First;

   function Has_Empty_Production (Grammar : in Production.List.Instance) return Nonterminal_ID_Set
   is
      use type Token.List.List_Iterator;
      Result : Nonterminal_ID_Set := (others => False);
      Prod_I : Production.List.List_Iterator;
      Prod   : Production.Instance;
      RHS_I  : Token.List.List_Iterator;
   begin
      Prod_I := Production.List.First (Grammar);
      while not Production.List.Is_Done (Prod_I) loop

         Prod  := Production.List.Current (Prod_I);
         RHS_I := Prod.RHS.Tokens.First;

         if RHS_I = Token.List.Null_Iterator then
            Result (Nonterminal.ID (Prod.LHS)) := True;
         end if;
         Production.List.Next (Prod_I);
      end loop;
      return Result;
   end Has_Empty_Production;

   function Deep_Copy (Item : in Lookahead_Ptr) return Lookahead_Ptr
   is
      --  Copy is in reverse order
      I      : Lookahead_Ptr := Item;
      Result : Lookahead_Ptr;
   begin
      while I /= null loop
         if I.Propagate then
            Result := new Lookahead'
              (Propagate => True,
               Next      => Result);
         else
            Result := new Lookahead'
              (Propagate => False,
               Lookahead => I.Lookahead,
               Next      => Result);
         end if;
         I := I.Next;
      end loop;
      return Result;
   end Deep_Copy;

   function Item_Node_Of
     (Prod       : in Production.List.List_Iterator;
      State      : in Unknown_State_Index;
      Lookaheads : in Lookahead_Ptr := null)
     return Item_Node
   is begin
      return
        (Prod       => Production.List.Current (Prod),
         Dot        => Token.List.First (Production.List.Current (Prod).RHS.Tokens),
         State      => State,
         Lookaheads => Deep_Copy (Lookaheads),
         Next       => null);
   end Item_Node_Of;

   function Item_Node_Of
     (Prod  : in Production.Instance;
      State : in Unknown_State_Index)
     return Item_Node
   is begin
      return
        (Prod       => Prod,
         Dot        => Prod.RHS.Tokens.First,
         State      => State,
         Lookaheads => null,
         Next       => null);
   end Item_Node_Of;

   function Compare (Left : in Lookahead; Right : in Token.Token_ID) return Boolean
   is
      use type Token.Token_ID;
   begin
      return (not Left.Propagate) and then Left.Lookahead = Right;
   end Compare;

   function Compare (Left, Right : in Lookahead) return Boolean
   is
      use type Token.Token_ID;
   begin
      if Left.Propagate or Right.Propagate then
         if Left.Propagate = Right.Propagate then
            return True;
         end if;
      else
         return Left.Lookahead = Right.Lookahead;
      end if;
      return False;
   end Compare;

   function Compare (Left, Right : in Lookahead_Ptr) return Boolean
   is begin
      return Compare (Left.all, Right.all);
   end Compare;

   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Token.Terminal_ID;
      Added :    out Boolean)
   is
      use type Token_Pkg.Token_ID;
      Found_Match : Boolean       := False;
      Match_Set   : Lookahead_Ptr := Set;
   begin
      while Match_Set /= null loop
         if Compare (Match_Set.all, Value) then
            Found_Match := True;
            exit;
         end if;

         Match_Set := Match_Set.Next;
      end loop;

      if not Found_Match then
         Set := new Lookahead'(False, Next => Set, Lookahead => Value);
      end if;

      Added := not Found_Match;
   end Include;

   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Token.Terminal_ID)
   is
      Added : Boolean;
   begin
      Include (Set, Value, Added);
   end Include;

   procedure Include
     (Set   : in out Lookahead_Ptr;
      Value : in     Lookahead_Ptr)
   is
      Found_Match : Boolean       := False;
      Match_Set   : Lookahead_Ptr := Set;
   begin
      while Match_Set /= null loop
         if Compare (Match_Set, Value) then
            Found_Match := True;
            exit;
         end if;

         Match_Set := Match_Set.Next;
      end loop;

      if not Found_Match then
         if Value.Propagate then
            Set := new Lookahead'(True, Next => Set);
         else
            Set := new Lookahead'(False, Next => Set, Lookahead => Value.Lookahead);
         end if;
      end if;
   end Include;

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
     (Left             : in Item_Node;
      Right            : in Item_Set;
      Match_Lookaheads : in Boolean)
     return Item_Ptr
   is
      use type Production.Instance;
      use type Token.List.List_Iterator;
      Current : Item_Ptr := Right.Set;
   begin
      while Current /= null loop
         if Left.Prod = Current.Prod and
           Left.Dot = Current.Dot and
           (not Match_Lookaheads or else
              Compare (Left.Lookaheads, Current.Lookaheads))
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

            if Find (Right_Item.all, Left, Match_Lookaheads) = null then
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
     (Left             : in Item_Set;
      Right            : in Item_Set_List;
      Match_Lookaheads : in Boolean)
     return Boolean
   is begin
      return Find (Left, Right, Match_Lookaheads) /= null;
   end Is_In;

   function Is_In
     (Symbol    : in Token.Token_ID;
      Set       : in Item_Set_Ptr;
      Goto_List : in Goto_Item_Ptr)
     return Boolean
   is
      Goto_Ptr : Goto_Item_Ptr := Goto_List;
      use type Token.Token_ID;
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
      Symbol : in Token.Token_ID)
     return Item_Set_Ptr
   is
      Goto_Ptr : Goto_Item_Ptr := From.Goto_List;
      use type Token.Token_ID;
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
     (New_Item         : in out Item_Node;
      Existing_Set     : in out Item_Set;
      Match_Lookaheads : in     Boolean)
     return Boolean
   is
      use type Token_Pkg.Token_ID;

      Found : constant Item_Ptr := Find (New_Item, Existing_Set, Match_Lookaheads);

      New_Lookahead      : Lookahead_Ptr; --  From New_Item
      Existing_Lookahead : Lookahead_Ptr; --  in Existing_Set
      Temp               : Lookahead_Ptr; --  for moves
      Result_Lookahead   : Lookahead_Ptr; --  add new not in existing
      Found_Match        : Boolean;
      Modified           : Boolean := False;
   begin
      if Found = null then
         Existing_Set.Set := new Item_Node'
           (Prod       => New_Item.Prod,
            Dot        => New_Item.Dot,
            State      => New_Item.State,
            Lookaheads => New_Item.Lookaheads,
            Next       => Existing_Set.Set);

         Modified := True;

      else
         --  Merge their lookaheads.
         Result_Lookahead := Found.Lookaheads;
         New_Lookahead    := New_Item.Lookaheads;

         while New_Lookahead /= null loop
            Existing_Lookahead := Found.Lookaheads;

            Found_Match := False;
            while Existing_Lookahead /= null loop

               if Compare (Existing_Lookahead, New_Lookahead) then
                  Found_Match := True;
                  exit;
               end if;

               Existing_Lookahead := Existing_Lookahead.Next;
            end loop;

            if not Found_Match then
               --  New lookahead not in Existing; move New to Result
               Temp               := New_Lookahead.Next;
               New_Lookahead.Next := Result_Lookahead;
               Result_Lookahead   := New_Lookahead;
               New_Lookahead      := Temp;
               Modified           := True;
            else
               --  New lookahead in Existing; free new
               Temp               := New_Lookahead.Next;
               New_Lookahead.Next := null;
               Free (New_Lookahead);
               New_Lookahead      := Temp;
            end if;
         end loop;

         Found.Lookaheads    := Result_Lookahead;
         New_Item.Lookaheads := null;
         Free (New_Item);
      end if;
      return Modified;
   end Merge;

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      Match_Lookaheads     : in Boolean;
      Trace                : in Boolean)
     return Item_Set
   is
      use type Token.Token_ID;
      use type Token.List.List_Iterator;
      --  Can't 'use' Production.List or Token.List; they hide each other.

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead. We don't copy Goto_List, since we are only
      --  concerned with lookaheads here.

      I : Item_Set; --  The result.

      Item       : Item_Ptr := Set.Set;           -- iterator 'for each item in I'
      B          : Production.List.List_Iterator; -- iterator 'for each production in G'
      Added_Item : Boolean  := False;             -- 'until no more items can be added'

      Beta : Token.List.List_Iterator;
      Merge_From  : Item_Node;
   begin
      --  Copy Set into I; Goto_List not copied
      I.State := Set.State;

      while Item /= null loop
         I.Set := new Item_Node'
           (Prod       => Item.Prod,
            Dot        => Item.Dot,
            State      => Set.State,
            Lookaheads => Deep_Copy (Item.Lookaheads),
            Next       => I.Set);

         Item := Item.Next;
      end loop;

      Item := I.Set;
      For_Each_Item :
      loop
         --  An item has the structure [A -> alpha Dot B Beta, a].
         --
         --  If B is a nonterminal, find its productions and place
         --  them in the set with lookaheads from FIRST(Beta a).
         if Item.Dot /= Token.List.Null_Iterator and then
           Token.List.ID (Item.Dot) in Nonterminal_ID
         then
            Beta := Token.List.Next_Token (Item.Dot); -- token after nonterminal, possibly null

            B := Production.List.First (Grammar);
            For_Each_Production :
            while not Production.List.Is_Done (B) loop
               if Nonterminal.ID (Production.List.Current (B).LHS) = Token.List.ID (Item.Dot) then
                  --  loop until find a terminal, or a nonterminal that cannot be empty, or end of production
                  Empty_Nonterm :
                  loop
                     if Beta = Token.List.Null_Iterator then
                        --  Use FIRST (a); a = Item.Lookaheads.
                        --  Lookaheads are all terminals, so
                        --  FIRST (a) = a.

                        --  Need a variable, because the lookaheads might be freed.
                        Merge_From := Item_Node_Of
                          (B,
                           State      => Set.State,
                           Lookaheads => Item.Lookaheads);

                        Added_Item := Added_Item or Merge (Merge_From, I, Match_Lookaheads);
                        exit Empty_Nonterm;

                     elsif Token.List.ID (Beta) in Token.Terminal_ID then
                        --  FIRST (Beta) = Beta
                        Merge_From := Item_Node_Of
                          (B,
                           State        => Set.State,
                           Lookaheads   => new Lookahead'
                             (Propagate => False,
                              Lookahead => Token.List.ID (Beta),
                              Next      => null));

                        Added_Item := Added_Item or Merge (Merge_From, I, Match_Lookaheads);
                        exit Empty_Nonterm;

                     else
                        --  Beta is a nonterminal; use FIRST (Beta)
                        for Terminal in Token.Terminal_ID loop
                           if First (Token.List.ID (Beta)) (Terminal) then
                              Merge_From := Item_Node_Of
                                (B,
                                 State        => Set.State,
                                 Lookaheads   => new Lookahead'
                                   (Propagate => False,
                                    Lookahead => Terminal,
                                    Next      => null));

                              Added_Item := Added_Item or Merge (Merge_From, I, Match_Lookaheads);
                           end if;
                        end loop;

                        if Has_Empty_Production (Token.List.ID (Beta)) then
                           --  Process the item [A -> alpha Dot Next(Beta), a]
                           Beta := Token.List.Next_Token (Beta);
                        else
                           exit Empty_Nonterm;
                        end if;
                     end if;
                  end loop Empty_Nonterm;

                  Beta := Token.List.Next_Token (Item.Dot);
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
               Put (I, Show_Lookaheads => True);
               Ada.Text_IO.New_Line;
            end if;
         else
            Item := Item.Next;
         end if;
      end loop For_Each_Item;

      return I;
   end Closure;

   function LALR_Goto_Transitions
     (Kernel  : in Item_Set;
      Symbol  : in Token.Token_ID;
      First   : in Derivation_Matrix;
      Grammar : in Production.List.Instance)
     return Item_Set
   is
      use Token.List;
      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;

      Item   : Item_Ptr := Kernel.Set;
      Dot_ID : Token.Token_ID;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop

         if Item.Dot /= Null_Iterator then

            Dot_ID := ID (Item.Dot);
            --  ID of token after Dot

            if Dot_ID = Symbol then
               Goto_Set.Set := new Item_Node'
                 (Prod       => Item.Prod,
                  Dot        => Next_Token (Item.Dot),
                  State      => Unknown_State, -- replaced in Kernels
                  Lookaheads => Item.Lookaheads,
                  Next       => Goto_Set.Set);
            end if;

            if Dot_ID in Nonterminal_ID and then First (Dot_ID)(Symbol) then
               --  Find the production(s) that create Dot_ID
               --  with first token Symbol and put them in.
               --
               --  FIXME: this is _not_ [dragon] fix 4.38 closure; where did it come from?
               declare
                  Prod_I : Production.List.List_Iterator := Production.List.First (Grammar);
                  Prod   : Production.Instance;
                  RHS_I  : Token.List.List_Iterator;
               begin
                  while not Production.List.Is_Done (Prod_I) loop
                     Prod  := Production.List.Current (Prod_I);
                     RHS_I := Prod.RHS.Tokens.First;

                     if (Dot_ID = Nonterminal.ID (Prod.LHS) or First (Dot_ID)(Nonterminal.ID (Prod.LHS))) and
                       (RHS_I /= Null_Iterator and then ID (RHS_I) = Symbol)
                     then
                        declare
                           New_Item : constant Item_Node :=
                             (Prod       => Prod,
                              Dot        => Next_Token (RHS_I),
                              State      => Unknown_State, -- replaced in Kernels
                              Lookaheads => null,
                              Next       => Goto_Set.Set);
                        begin
                           if null = Find (New_Item, Goto_Set, Match_Lookaheads => False) then
                              Goto_Set.Set := new Item_Node'(New_Item);
                              --  else already in goto set
                           end if;
                        end;
                     end if;

                     Production.List.Next (Prod_I);
                  end loop;
               end;
            end if;
         end if; -- item.dot /= null

         Item := Item.Next;
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LR1_Goto_Transitions
     (Set                  : in Item_Set;
      Symbol               : in Token.Token_ID;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      Trace                : in Boolean)
     return Item_Set
   is
      use Token.List;
      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;
      Item     : Item_Ptr := Set.Set;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop
         if Item.Dot /= Null_Iterator then
            if ID (Item.Dot) = Symbol then
               Goto_Set.Set := new Item_Node'
                 (Prod       => Item.Prod,
                  Dot        => Next_Token (Item.Dot),
                  State      => Unknown_State,
                  Lookaheads => Item.Lookaheads,
                  Next       => Goto_Set.Set);
            end if;
         end if;

         Item := Item.Next;
      end loop;

      if Trace then
         Ada.Text_IO.Put_Line ("LR1_Goto_Transitions " & Token.Token_ID'Image (Symbol));
         Put (Goto_Set, Show_Lookaheads => True);
      end if;

      if Goto_Set.Set /= null then
         return Closure (Goto_Set, Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   procedure Free (Item : in out Item_Node)
   is
      Lookahead : Lookahead_Ptr := Item.Lookaheads;
   begin
      while Lookahead /= null loop
         Item.Lookaheads := Lookahead.Next;
         Free (Lookahead);
         Lookahead := Item.Lookaheads;
      end loop;
   end Free;

   procedure Free (Item : in out Item_Set)
   is
      I        : Item_Ptr      := Item.Set;
      Goto_Set : Goto_Item_Ptr := Item.Goto_List;
   begin
      while I /= null loop
         Item.Set := I.Next;
         Free (I.all);
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

   function LALR_Kernels
     (Grammar           : in Production.List.Instance;
      First             : in Derivation_Matrix;
      EOF_Token         : in Token.Token_ID;
      Trace             : in Boolean;
      First_State_Index : in Unknown_State_Index)
     return Item_Set_List
   is
      use type Token.List.List_Iterator;
      use type Token.Token_ID;

      Kernel_List : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of
                 (Production.List.Current (Production.List.First (Grammar)), First_State_Index)),
            Goto_List => null,
            State     => First_State_Index,
            Next      => null),
         Size         => 1);

      New_Items_To_Check   : Boolean      := True;
      Checking_Set         : Item_Set_Ptr;
      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= null loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (Checking_Set.all);
            end if;

            for Symbol in Token.Token_ID loop

               if Checking_Set.Set.Dot /= Token.List.Null_Iterator and then
                (Symbol = EOF_Token and
                   Token.List.ID (Checking_Set.Set.Dot) = EOF_Token)
               then
                  --  This is the start symbol accept production;
                  --  don't need a kernel with dot after EOF.
                  New_Items.Set := null;
               else
                  New_Items := LALR_Goto_Transitions (Checking_Set.all, Symbol, First, Grammar);
               end if;

               --  See if any of the item sets need to be added to our list
               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, Kernel_List, Match_Lookaheads => False);

                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next  := Kernel_List.Head;
                     New_Items.State := Kernel_List.Size + First_State_Index;

                     declare
                        I : Item_Ptr := New_Items.Set;
                     begin
                        while I /= null loop
                           I.State := New_Items.State;
                           I       := I.Next;
                        end loop;
                     end;

                     if Trace then
                        Ada.Text_IO.Put ("  adding new kernel on " & Token.Token_Image (Symbol) & ": ");
                        Put (New_Items);
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Checking_Set.Goto_List := new Goto_Item'
                       (Set    => Kernel_List.Head,
                        Symbol => Symbol,
                        Next   => Checking_Set.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Symbol    => Symbol,
                        Set       => New_Items_Set,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put ("  adding goto on " & Token.Token_Image (Symbol) & ": ");
                           Put (New_Items_Set.all);
                        end if;

                        Checking_Set.Goto_List := new Goto_Item'
                          (Set    => New_Items_Set,
                           Symbol => Symbol,
                           Next   => Checking_Set.Goto_List);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            Checking_Set := Checking_Set.Next;
         end loop;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Kernel_List;
   end LALR_Kernels;

   function LR1_Item_Sets
     (Has_Empty_Production : in LR1_Items.Nonterminal_ID_Set;
      First                : in LR1_Items.Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      EOF_Token            : in Token.Token_ID;
      First_State_Index    : in Unknown_State_Index;
      Trace                : in Boolean)
     return LR1_Items.Item_Set_List
   is
      use type Token.List.List_Iterator;
      use type Token.Token_ID;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

      C : Item_Set_List :=                -- result
        (Head                   => new Item_Set'
           (Closure
              ((Set             => new Item_Node'
                  (Item_Node_Of
                     (Production.List.First (Grammar),
                      First_State_Index,
                      new Lookahead'(False, null, EOF_Token))),
                Goto_List       => null,
                State           => First_State_Index,
                Next            => null),
               Has_Empty_Production, First, Grammar,
               Match_Lookaheads => False, -- allow combining lookaheads within one state.
               Trace            => False)),
         Size                   => 1);

      I         : Item_Set_Ptr; -- iterator 'for each set of items I in C'
      Added_Item           : Boolean      := True; -- 'until no more items can be added'

      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin
      loop
         Added_Item   := False;
         I := C.Head;

         while I /= null loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (I.all, Show_Lookaheads => True);
            end if;

            for Symbol in Token.Token_ID loop -- 'for each grammar symbol X'

               if I.Set.Dot /= Token.List.Null_Iterator and then
                (Symbol = EOF_Token and
                   Token.List.ID (I.Set.Dot) = EOF_Token)
               then
                  --  This is the start symbol accept production;
                  --  don't need a set with dot after EOF.
                  New_Items.Set := null;
               else
                  New_Items := LR1_Goto_Transitions
                    (I.all, Symbol, Has_Empty_Production, First, Grammar, Trace);
               end if;

               if New_Items.Set /= null then -- 'goto (I, X) not empty'

                  New_Items_Set := Find (New_Items, C, Match_Lookaheads => True); -- 'not in C'

                  if New_Items_Set = null then
                     Added_Item := True;

                     New_Items.Next  := C.Head;
                     New_Items.State := C.Size + First_State_Index;

                     declare
                        I : Item_Ptr := New_Items.Set;
                     begin
                        while I /= null loop
                           I.State := New_Items.State;
                           I       := I.Next;
                        end loop;
                     end;

                     if Trace then
                        Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (New_Items.State));
                     end if;

                     C :=
                       (Head => new Item_Set'(New_Items),
                        Size => C.Size + 1);

                     I.Goto_List := new Goto_Item'
                       (Symbol => Symbol,
                        Set    => C.Head,
                        Next   => I.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Symbol    => Symbol,
                        Set       => New_Items_Set,
                        Goto_List => I.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put_Line
                             ("  adding goto on " & Token.Token_Image (Symbol) & " to state" &
                                Unknown_State_Index'Image (New_Items_Set.State));

                        end if;

                        I.Goto_List := new Goto_Item'
                          (Symbol => Symbol,
                           Set    => New_Items_Set,
                           Next   => I.Goto_List);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            I := I.Next;
         end loop;
         exit when not Added_Item;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return C;
   end LR1_Item_Sets;

   function Token_Name (Item : in Token.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Item.all));
   end Token_Name;
   function Token_Name (Item : in Nonterminal.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Item.all));
   end Token_Name;

   function Print (Item : in Lookahead) return String
   is begin
      if Item.Propagate then
         return "#";
      else
         return Token.Token_Image (Item.Lookahead);
      end if;
   end Print;

   function Print (Item : in Lookahead_Ptr) return String
   is
      use Ada.Strings.Unbounded;
      Lookahead : Lookahead_Ptr    := Item;
      Result    : Unbounded_String := Null_Unbounded_String;
   begin
      if Lookahead = null then
         return "";
      else
         Result := Result & ", ";

         loop
            Result := Result & Print (Lookahead.all);

            Lookahead := Lookahead.Next;

            exit when Lookahead = null;

            Result := Result & "/";
         end loop;

         return To_String (Result);
      end if;
   end Print;

   function Image_Item
     (Item            : in Item_Node;
      Show_State      : in Boolean;
      Show_Lookaheads : in Boolean;
      Show_Tag        : in Boolean := False)
     return String
   is
      use Token.List;

      Token_Index : List_Iterator;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Token_Name (Item.Prod.LHS)) &
          (if Show_Tag then "(" & Ada.Tags.Expanded_Name (Item.Prod.LHS.all'Tag) & ")"
           else "") &
          " <=";

   begin
      Token_Index := First (Item.Prod.RHS.Tokens);

      while Token_Index /= Null_Iterator loop
         if Token_Index = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Token_Name (Token_Handle (Token_Index));
         Next_Token (Token_Index);
      end loop;

      if Item.Dot = Null_Iterator then
         Result := Result & " ^";
      end if;

      if Show_State then
         Result := Result & " in " & Unknown_State_Index'Image (Item.State);
      end if;

      if Show_Lookaheads then
         Result := Result & Print (Item.Lookaheads);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image_Item;

   procedure Put (Item : in Item_Node; Show_Lookaheads : in Boolean) is
   begin
      Ada.Text_IO.Put (Image_Item (Item, Show_State => True, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put (Item : in Goto_Item_Ptr)
   is
      use Ada.Text_IO;
      Reference : Goto_Item_Ptr := Item;
   begin
      while Reference /= null loop
         Put_Line
           ("      on " & Token.Token_Image (Reference.Symbol) &
              " => State" & Unknown_State_Index'Image (Reference.Set.State));

         Reference := Reference.Next;
      end loop;
   end Put;

   procedure Put (Item : in Item_Set; Show_Lookaheads : in Boolean := False)
   is
      use Ada.Text_IO;
      Set : Item_Ptr := Item.Set;
   begin
      if Item.State /= Unknown_State then
         Put_Line ("State" & Unknown_State_Index'Image (Item.State) & ":");
      end if;
      while Set /= null loop
         Put_Line ("  " & Image_Item (Set.all, Show_State => False, Show_Lookaheads => Show_Lookaheads));

         Set := Set.Next;
      end loop;
   end Put;

   procedure Put (Item : in Item_Set_List; Show_Lookaheads : in Boolean := False)
   is
      use Ada.Text_IO;
      Set : Item_Set_Ptr := Item.Head;
   begin
      Put_Line ("Size :" & Unknown_State_Index'Image (Item.Size));

      while Set /= null loop
         Put (Set.all, Show_Lookaheads);
         Put_Line ("   Goto:");
         Put (Set.Goto_List);

         Set := Set.Next;
      end loop;
   end Put;

end FastToken.Parser.LR1_Items;
--  Local Variables:
--  jit-lock-defer-time: 0.5
--  End:
