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

   ----------
   --  body subprograms

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
         Existing_Set.Set.Insert ((Prod, Dot, new Token_ID_Set'(Lookaheads)));

         Modified := True;
      else
         Include (Ref (Found), Lookaheads, Modified);
      end if;

      return Modified;
   end Merge;

   ----------
   --  Public subprograms, declaration order

   function To_Lookahead (Item : in Token_ID; Descriptor : in WisiToken.Descriptor) return Lookahead
   is begin
      return Result : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Lookahead => False) do
         Result (Item) := True;
      end return;
   end To_Lookahead;

   function Lookahead_Image (Item : in Lookahead; Descriptor : in WisiToken.Descriptor) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Item'Range loop
         if Item (I) then
            if Length (Result) > 0 then
               Result := Result & "/";
            end if;
            Result := Result & Image (I, Descriptor);
         end if;
      end loop;
      return To_String (Result);
   end Lookahead_Image;

   function Item_Compare (Left, Right : in Item) return SAL.Compare_Result
   is begin
      if Left.Prod.Nonterm > Right.Prod.Nonterm then
         return SAL.Greater;
      elsif Left.Prod.Nonterm < Right.Prod.Nonterm then
         return SAL.Less;

      elsif Left.Prod.RHS > Right.Prod.RHS then
         return SAL.Greater;
      elsif Left.Prod.RHS < Right.Prod.RHS then
         return SAL.Less;

      else
         declare
            Left_Index : Integer renames Token_ID_Arrays.To_Index (Left.Dot);
            Right_Index : Integer renames Token_ID_Arrays.To_Index (Right.Dot);
         begin
            if Left_Index > Right_Index then
               return SAL.Greater;
            elsif Left_Index < Right_Index then
               return SAL.Less;
            else
               return SAL.Equal;
            end if;
         end;
      end if;
   end Item_Compare;

   procedure Include
     (Item  : in out LR1_Items.Item;
      Value : in     Lookahead;
      Added :    out Boolean)
   is begin
      Added := False;

      for I in Item.Lookaheads'Range loop
         if Value (I) then
            Added := Added or not Item.Lookaheads (I);
            Item.Lookaheads (I) := True;
         end if;
      end loop;
   end Include;

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Descriptor : in     WisiToken.Descriptor)
   is
      Added : Boolean;
      pragma Unreferenced (Added);
   begin
      Include (Item, Value, Added, Descriptor);
   end Include;

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Added      :    out Boolean;
      Descriptor : in     WisiToken.Descriptor)
   is begin
      Added := False;

      for I in Item.Lookaheads'Range loop
         if I = Descriptor.Last_Lookahead then
            null;
         else
            if Value (I) then
               Added := Added or not Item.Lookaheads (I);
               Item.Lookaheads (I) := True;
            end if;
         end if;
      end loop;
   end Include;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor;
         Item       : in LR1_Items.Item)
        return Boolean)
     return Item_Set
   is begin
      return Result : Item_Set := (Set => <>, Goto_List => Set.Goto_List, State => Set.State)
      do
         for Item of Set.Set loop
            if Include (Grammar, Descriptor, Item) then
               Result.Set.Insert (Item);
            end if;
         end loop;
      end return;
   end Filter;

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor;
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

   function Find
     (Item : in LR1_Items.Item;
      Set  : in Item_Set)
     return Item_Lists.Cursor
   is begin
      return Find (Item.Prod, Item.Dot, Set);
   end Find;

   function Find
     (Prod  : in Production_ID;
      Dot   : in Token_ID_Arrays.Cursor;
      Right : in Item_Set)
     return Item_Lists.Cursor
   is begin
      return Right.Set.Find ((Prod, Dot, null));
   end Find;

   function Find
     (Prod       : in Production_ID;
      Dot        : in Token_ID_Arrays.Cursor;
      Right      : in Item_Set;
      Lookaheads : in Lookahead)
     return Item_Lists.Cursor
   is
      use Item_Lists;
      Result : constant Cursor := Right.Set.Find ((Prod, Dot, null));
   begin
      --  Item_Equal does not consider lookaheads
      if Result = No_Element then
         return Result;
      elsif Constant_Ref (Result).Lookaheads.all = Lookaheads then
         return Result;
      else
         return No_Element;
      end if;
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

   function Closure
     (Set                     : in Item_Set;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
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
               declare
                  Prod : WisiToken.Productions.Instance renames Grammar (Element (Item.Dot));
               begin

                  For_Each_RHS :
                  for B in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare
                        RHS_2 : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (B); --  FIXME: delete _2
                        P_ID_2 : constant Production_ID := (Prod.LHS, B);
                     begin
                        --  Compute FIRST (<tail of right hand side> a); loop
                        --  until find a terminal, a nonterminal that
                        --  cannot be empty, or end of production, adding
                        --  items on the way.

                        Beta := Next (Item.Dot); -- tokens after nonterminal, possibly null

                        First_Tail :
                        loop
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
                                (P_ID_2, RHS_2.Tokens.First, To_Lookahead (Element (Beta), Descriptor), I);
                              exit First_Tail;

                           else
                              --  Beta is a nonterminal; use FIRST (Beta)
                              for Terminal of First_Terminal_Sequence (Element (Beta)) loop
                                 Added_Item := Added_Item or
                                   Merge (P_ID_2, RHS_2.Tokens.First, To_Lookahead (Terminal, Descriptor), I);
                              end loop;

                              if Has_Empty_Production (Element (Beta)) then
                                 --  Process the next token in the tail, or "a"
                                 Beta := Next (Beta);
                              else
                                 exit First_Tail;
                              end if;
                           end if;
                        end loop First_Tail;
                     end;
                  end loop For_Each_RHS;
               end;
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

   function Image
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
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
      Descriptor      : in WisiToken.Descriptor;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean := True)
   is begin
      Ada.Text_IO.Put (Image (Grammar, Descriptor, Item, Show_Lookaheads => Show_Lookaheads));
   end Put;

   procedure Put
     (Descriptor : in WisiToken.Descriptor;
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
      Descriptor      : in WisiToken.Descriptor;
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
      Descriptor      : in WisiToken.Descriptor;
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
