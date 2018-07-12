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

   function Follow
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set
   is
      Prev_Result : Token_Array_Token_Set :=
        --  FIXME: use grammar.first_index .., declare local subtypes
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      Result : Token_Array_Token_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal =>
           (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False));

      ID : Token_ID;
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
         for Prod of Grammar loop
            for A of Prod.RHSs loop
               for I in A.Tokens.First_Index .. A.Tokens.Last_Index loop
                  if A.Tokens (I) = B then
                     if I < A.Tokens.Last_Index then
                        --  Rule 1
                        ID := A.Tokens (1 + I);
                        if ID in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                           Result (B, ID) := True;
                        else
                           Or_Slice (Result, B, Slice (First, ID));
                        end if;
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      Prev_Result := Result;
      loop
         for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
            for Prod of Grammar loop
               for A of Prod.RHSs loop
                  for I in A.Tokens.First_Index .. A.Tokens.Last_Index loop
                     if A.Tokens (I) = B then
                        if I = A.Tokens.Last_Index or else
                          (A.Tokens (1 + I) in
                             Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                             Has_Empty_Production (A.Tokens (1 + I)))
                        then
                           --  rule 3
                           Or_Slice (Result, B, Slice (Result, Prod.LHS));
                        end if;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;

         exit when Prev_Result = Result;
         Prev_Result := Result;
      end loop;
      return Result;
   end Follow;

   procedure Add
     (List : in out Item_Lists.List;
      Item : in     LR1_Items.Item)
   is
      use Item_Lists;
      I : Cursor := List.First;
   begin
      loop
         exit when I = No_Element;
         exit when List (I).Prod.Nonterm > Item.Prod.Nonterm;
         Next (I);
      end loop;
      List.Insert (I, Item);
   end Add;

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
     (Item  : in LR1_Items.Item;
      Value : in Token_ID)
   is begin
      Include (Item.Lookaheads.all, Value);
   end Include;

   procedure Include
     (Item              : in     LR1_Items.Item;
      Value             : in     Lookahead;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Descriptor, Exclude_Propagate);
   end Include;

   procedure Include
     (Item              : in     LR1_Items.Item;
      Value             : in     Lookahead;
      Added             :    out Boolean;
      Descriptor        : access constant WisiToken.Descriptor'Class;
      Exclude_Propagate : in     Boolean)
   is begin
      Include (Item.Lookaheads.all, Value, Added, Descriptor, Exclude_Propagate);
   end Include;

   procedure Add
     (List   : in out Goto_Item_Lists.List;
      Symbol : in     Token_ID;
      State  : in     State_Index)
   is
      use Goto_Item_Lists;
      I : Cursor := List.First;
   begin
      loop
         exit when not Has_Element (I) or else List (I).Symbol > Symbol;
         Next (I);
      end loop;
      List.Insert (Before => I, Element => (Symbol, State));
   end Add;

   function Find
     (Item : in LR1_Items.Item;
      Set  : in Item_Set)
     return Item_Lists.Cursor
   is begin
      return Find (Item.Prod, Item.Dot, Set, null);
   end Find;

   function Find
     (Prod       : in     Production_ID;
      Dot        : in     Token_ID_Arrays.Cursor;
      Right      : in     Item_Set;
      Lookaheads : access Lookahead := null)
     return Item_Lists.Cursor
   is
      use Item_Lists;
      use all type Token_ID_Arrays.Cursor;
   begin
      for Cur in Right.Set.Iterate loop
         declare
            Test_Item : Item renames Constant_Ref (Cur);
         begin
            if Prod.Nonterm < Test_Item.Prod.Nonterm then
               --  Right.Item_Set is sorted on ascending LHS, since it is built with
               --  Add.
               return No_Element;
            elsif Prod = Constant_Ref (Cur).Prod and
              Dot = Constant_Ref (Cur).Dot and
              (Lookaheads = null or else
                 Lookaheads.all = Constant_Ref (Cur).Lookaheads.all)
            then
               return Cur;
            end if;
         end;
      end loop;
      return No_Element;
   end Find;

   function To_Item_Set_Tree_Key (Item_Set : in LR1_Items.Item_Set) return Item_Set_Tree_Key
   is
      Cur : Item_Lists.Cursor := Item_Set.Set.First;
   begin
      return Result : Item_Set_Tree_Key do
         Result.Prod_Count := Integer (Item_Set.Set.Length);
         declare
            Item_1 : Item renames Item_Set.Set (Cur);
         begin
            Result.Prod_1_LHS := Item_1.Prod.Nonterm;
            Result.Prod_1_RHS := Item_1.Prod.RHS;
            Result.Prod_1_Dot := Token_ID_Arrays.To_Index (Item_1.Dot);
         end;

         if Result.Prod_Count = 1 then
            Result.Prod_2_LHS := Invalid_Token_ID;
            Result.Prod_2_RHS := 0;
            Result.Prod_2_Dot := 0;
         else
            Item_Lists.Next (Cur);
            declare
               Item_2 : Item renames Item_Set.Set (Cur);
            begin
               Result.Prod_2_LHS := Item_2.Prod.Nonterm;
               Result.Prod_2_RHS := Item_2.Prod.RHS;
               Result.Prod_2_Dot := Token_ID_Arrays.To_Index (Item_2.Dot);
            end;
         end if;
      end return;
   end To_Item_Set_Tree_Key;

   function Item_Set_Tree_Key_Less (Left, Right : in Item_Set_Tree_Key) return Boolean
   is begin
      if Left.Prod_Count < Right.Prod_Count then
         return True;
      elsif Left.Prod_Count > Right.Prod_Count then
         return False;

      elsif Left.Prod_1_LHS < Right.Prod_1_LHS then
         return True;
      elsif Left.Prod_1_LHS > Right.Prod_1_LHS then
         return False;

      elsif Left.Prod_1_RHS < Right.Prod_1_RHS then
         return True;
      elsif Left.Prod_1_RHS > Right.Prod_1_RHS then
         return False;

      elsif Left.Prod_1_Dot < Right.Prod_1_Dot then
         return True;
      elsif Left.Prod_1_Dot > Right.Prod_1_Dot then
         return False;

      elsif Left.Prod_Count = 1 then
         return True;

      elsif Left.Prod_2_LHS < Right.Prod_2_LHS then
         return True;
      elsif Left.Prod_2_LHS > Right.Prod_2_LHS then
         return False;

      elsif Left.Prod_2_RHS < Right.Prod_2_RHS then
         return True;
      elsif Left.Prod_2_RHS > Right.Prod_2_RHS then
         return False;

      else
         return Left.Prod_2_Dot < Right.Prod_2_Dot;
      end if;
   end Item_Set_Tree_Key_Less;

   function Find
     (New_Item_Set     : in Item_Set;
      Item_Set_Array   : in Item_Set_List;
      Item_Set_Tree    : in Item_Set_Trees.Tree;
      Match_Lookaheads : in Boolean)
     return Unknown_State_Index
   is
      use all type Item_Set_Trees.Cursor;

      Tree_It    : constant Item_Set_Trees.Iterator := Item_Set_Trees.Iterate (Item_Set_Tree);
      Key        : constant Item_Set_Tree_Key       := To_Item_Set_Tree_Key (New_Item_Set);
      Found_Tree : constant Item_Set_Trees.Cursor   := Tree_It.Find (Key);

      function Compare (Known_Item_Set : in Item_Set) return Boolean
      is
         use Item_Lists;

         --  We already know they have the same number of items, and the same
         --  LHS/RHS/Dot for the first two productions.

         Known_Cur : Cursor := Known_Item_Set.Set.First;
         New_Cur   : Cursor := New_Item_Set.Set.First;
      begin
         if Match_Lookaheads then
            for I in 1 .. Key.Prod_Count loop
               declare
                  Known_Item : Item renames Constant_Ref (Known_Cur);
                  New_Item   : Item renames Constant_Ref (New_Cur);
               begin
                  if Known_Item.Lookaheads.all /= New_Item.Lookaheads.all then
                     --  FIXME: different data structure for lookahead so /= is faster?
                     return False;
                  end if;
                  if I >= 3 then
                     if Known_Item.Prod /= New_Item.Prod then
                        return False;
                     elsif Token_ID_Arrays.To_Index (Known_Item.Dot) /= Token_ID_Arrays.To_Index (New_Item.Dot) then
                        --  FIXME: store integer dot, use To_Cursor or () elsewhere; we do this way more often?
                        return False;
                     end if;
                  end if;
               end;
               Item_Lists.Next (Known_Cur);
               Item_Lists.Next (New_Cur);
            end loop;
            return True;
         else
            if Key.Prod_Count <= 2 then
               return True;
            else
               Item_Lists.Next (Known_Cur);
               Item_Lists.Next (New_Cur);
               Item_Lists.Next (Known_Cur);
               Item_Lists.Next (New_Cur);
               for I in 3 .. Key.Prod_Count loop
                  declare
                     Known_Item : Item renames Constant_Ref (Known_Cur);
                     New_Item   : Item renames Constant_Ref (New_Cur);
                  begin
                     if Known_Item.Prod /= New_Item.Prod then
                        return False;
                     elsif Token_ID_Arrays.To_Index (Known_Item.Dot) /= Token_ID_Arrays.To_Index (New_Item.Dot) then
                        return False;
                     end if;
                  end;
                  Item_Lists.Next (Known_Cur);
                  Item_Lists.Next (New_Cur);
               end loop;
               return True;
            end if;
         end if;
      end Compare;

   begin
      if Found_Tree = Item_Set_Trees.No_Element then
         return Unknown_State;
      end if;

      declare
         Node : Item_Set_Tree_Node renames Item_Set_Tree.Constant_Ref (Found_Tree);
      begin
         for State of Node.States loop
            if Compare (Item_Set_Array (State)) then
               return State;
            end if;
         end loop;

         return Unknown_State;
      end;
   end Find;

   procedure Add
     (New_Item_Set    : in     Item_Set;
      Item_Set_Vector : in out Item_Set_List;
      Item_Set_Tree   : in out Item_Set_Trees.Tree)
   is
      use Item_Set_Trees;
      Tree_It : constant Iterator          := Iterate (Item_Set_Tree);
      Key     : constant Item_Set_Tree_Key := To_Item_Set_Tree_Key (New_Item_Set);
      Found   : constant Cursor            := Tree_It.Find (Key);
   begin
      Item_Set_Vector.Append (New_Item_Set);

      if Found /= No_Element then
         declare
            Node : Item_Set_Tree_Node renames Item_Set_Tree.Variable_Ref (Found);
         begin
            Node.States.Append (New_Item_Set.State);
         end;
      else
         --  FIXME: use Found to optimize insert?
         Item_Set_Tree.Insert ((Key, State_Index_Arrays.To_Vector (New_Item_Set.State)));
      end if;
   end Add;

   function Is_In
     (Item      : in Goto_Item;
      Goto_List : in Goto_Item_Lists.List)
     return Boolean
   is begin
      for List_Item of Goto_List loop
         if List_Item = Item then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

   function Goto_State
     (From   : in Item_Set;
      Symbol : in Token_ID)
     return Unknown_State_Index
   is begin
      for Item of From.Goto_List loop
         if Item.Symbol = Symbol then
            return Item.State;
         end if;
      end loop;

      return Unknown_State;
   end Goto_State;

   function Merge
     (Prod         : in     Production_ID;
      Dot          : in     Token_ID_Arrays.Cursor;
      Lookaheads   : in     Lookahead;
      Existing_Set : in out Item_Set)
     return Boolean
   is
      --  Merge item into Existing_Set. Return True if Existing_Set
      --  is modified.

      use Item_Lists;

      Found    : constant Item_Lists.Cursor := Find (Prod, Dot, Existing_Set);
      Modified : Boolean                    := False;
   begin
      if Found = No_Element then
         Add (Existing_Set.Set, (Prod, Dot, new Token_ID_Set'(Lookaheads)));

         Modified := True;
      else
         Include (Ref (Found).Lookaheads.all, Lookaheads, Modified, null, Exclude_Propagate => False);
      end if;

      return Modified;
   end Merge;

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor'Class)
     return Item_Set
   is
      use all type Item_Lists.Cursor;
      use Token_ID_Arrays;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead.

      I : Item_Set; --  The result.

      Item_I     : Item_Lists.Cursor; -- iterator 'for each item in I'
      Added_Item : Boolean := False;  -- 'until no more items can be added'

      Beta : Token_ID_Arrays.Cursor; -- into RHS.Tokens
   begin
      I := Set;

      Item_I := I.Set.First;
      loop
         declare
            Item : LR1_Items.Item renames I.Set (Item_I);
         begin
            --  An item has the structure [A -> alpha Dot B Beta, a].
            --
            --  If B is a nonterminal, find its productions and place
            --  them in the set with lookaheads from FIRST(Beta a).
            if Item.Dot /= No_Element and then
              Element (Item.Dot) in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal
            then
               Beta := Next (Item.Dot); -- tokens after nonterminal, possibly null

               For_Each_Production :
               for Prod of Grammar loop
                  For_Each_RHS :
                  for B in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare
                        RHS_2 : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (B);
                     begin
                        if Prod.LHS = Element (Item.Dot) then
                           --  Compute FIRST (<tail of right hand side> a); loop
                           --  until find a terminal, a nonterminal that
                           --  cannot be empty, or end of production, adding
                           --  items on the way.
                           First_Tail :
                           loop
                              declare
                                 P_ID_2 : constant Production_ID := (Prod.LHS, B);
                              begin
                                 if Beta = No_Element then
                                    --  Use FIRST (a); a = Item.Lookaheads.
                                    --  Lookaheads are all terminals, so
                                    --  FIRST (a) = a.
                                    Added_Item := Added_Item or
                                      Merge (P_ID_2, RHS_2.Tokens.First, Item.Lookaheads.all, I);
                                    exit First_Tail;

                                 elsif Element (Beta) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                                    --  FIRST (Beta) = Beta
                                    Added_Item := Added_Item or Merge
                                      (P_ID_2, RHS_2.Tokens.First,
                                       To_Lookahead (Element (Beta), Descriptor), I);
                                    exit First_Tail;

                                 else
                                    --  Beta is a nonterminal; use FIRST (Beta)
                                    for Terminal in Descriptor.First_Terminal .. Descriptor.Last_Terminal loop
                                       if First (Element (Beta), Terminal) then
                                          Added_Item := Added_Item or
                                            Merge
                                              (P_ID_2, RHS_2.Tokens.First,
                                               To_Lookahead (Terminal, Descriptor), I);
                                       end if;
                                    end loop;

                                    if Has_Empty_Production (Element (Beta)) then
                                       --  Process the next token in the tail, or a
                                       Beta := Next (Beta);
                                    else
                                       exit First_Tail;
                                    end if;
                                 end if;
                              end;
                           end loop First_Tail;

                           Beta := Next (Item.Dot);
                        end if;
                     end;
                  end loop For_Each_RHS;
               end loop For_Each_Production;
            end if; -- Dot is at non-terminal
         end;

         if Item_Lists.Next (Item_I) = Item_Lists.No_Element then
            exit when not Added_Item;

            Item_I := I.Set.First;
            Added_Item := False;

            if Trace_Generate > Extra then
               Ada.Text_IO.Put_Line ("I:");
               Put (Grammar, Descriptor, I);
               Ada.Text_IO.New_Line;
            end if;
         else
            Item_I := Item_Lists.Next (Item_I);
         end if;
      end loop;

      return I;
   end Closure;

   function Productions (Set : in Item_Set) return Production_ID_Arrays.Vector
   is begin
      return Result : Production_ID_Arrays.Vector do
         for Item of Set.Set loop
            Result.Append (Item.Prod);
         end loop;
      end return;
   end Productions;

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class;
      Item       : in LR1_Items.Item)
     return Boolean
   is
      use Token_ID_Arrays;
      Prod : WisiToken.Productions.Instance renames Grammar (Item.Prod.Nonterm);
      RHS  : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (Item.Prod.RHS);
   begin
      return
        No_Element /= RHS.Tokens.First and
        (Item.Dot = No_Element or else
           ((Prod.LHS = Descriptor.Accept_ID and
               Item.Dot = RHS.Tokens.First)
              -- Start symbol production with dot before first token.
              or
              Item.Dot /= RHS.Tokens.First));
   end In_Kernel;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor'Class;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor'Class;
         Item       : in LR1_Items.Item)
        return Boolean)
     return Item_Set
   is begin
      return Result : Item_Set := (Set => <>, Goto_List => Set.Goto_List, State => Set.State)
      do
         for Item of Set.Set loop
            if Include (Grammar, Descriptor, Item) then
               Result.Set.Append (Item);
            end if;
         end loop;
      end return;
   end Filter;

   function Image
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean)
     return String
   is
      use Token_ID_Arrays;

      I : Cursor;

      Prod   : WisiToken.Productions.Instance renames Grammar (Item.Prod.Nonterm);
      RHS    : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (Item.Prod.RHS);
      Result : Ada.Strings.Unbounded.Unbounded_String :=
        +Padded_Image (Item.Prod, Width => Prod_ID_Image_Width) & ":" & Image (Prod.LHS, Descriptor) & " <=";
   begin
      I := RHS.Tokens.First;

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

      if Show_Lookaheads then
         Result := Result & ", " & Lookahead_Image (Item.Lookaheads.all, Descriptor);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean := True)
   is begin
      Ada.Text_IO.Put (Image (Grammar, Descriptor, Item, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put
     (Descriptor : in WisiToken.Descriptor'Class;
      List       : in Goto_Item_Lists.List)
   is
      use Ada.Text_IO;
   begin
      for Item of List loop
         Put_Line
           ("      on " & Image (Item.Symbol, Descriptor) &
              " => State" & Unknown_State_Index'Image (Item.State));
      end loop;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False)
   is
      use Ada.Text_IO;
   begin
      if Item.State /= Unknown_State then
         Put_Line ("State" & Unknown_State_Index'Image (Item.State) & ":");
      end if;

      for It of Item.Set loop
         if not Kernel_Only or else
           In_Kernel (Grammar, Descriptor, It)
         then
            Put_Line
              ("  " & Image (Grammar, Descriptor, It, Show_Lookaheads => Show_Lookaheads));
         end if;
      end loop;

      if Show_Goto_List then
         Put (Descriptor, Item.Goto_List);
      end if;
   end Put;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor'Class;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True)
   is
      use Ada.Text_IO;
   begin
      for Set of Item loop
         Put (Grammar, Descriptor, Set, Show_Lookaheads);
         Put_Line ("   Goto:");
         Put (Descriptor, Set.Goto_List);
      end loop;
   end Put;

end WisiToken.LR.LR1_Items;
