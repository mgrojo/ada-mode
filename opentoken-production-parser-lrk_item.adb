-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2008, 2009, 2012, 2013 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
package body OpenToken.Production.Parser.LRk_Item is
   use type Ada.Strings.Unbounded.Unbounded_String;

   function Compute_Non_Terminals return Token_ID_Set
   is
      Result : Token_ID_Set;
   begin
      Result (Token.Token_ID'First .. Tokenizer.Terminal_ID'Last)                      := (others => False);
      Result (Token.Token_ID'Succ (Tokenizer.Terminal_ID'Last) .. Token.Token_ID'Last) := (others => True);
      return Result;
   end Compute_Non_Terminals;

   Non_Terminals : constant Token_ID_Set := Compute_Non_Terminals;

   Line_End : constant String := "" & Ada.Characters.Latin_1.LF;

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

   function First_Derivations
     (Grammar      : in Production_List.Instance;
      Non_Terminal : in Token.Token_ID;
      Trace        : in Boolean)
     return Token_ID_Set
   is
      use type Token_List.List_Iterator;

      Prod_Iterator  : Production_List.List_Iterator;
      Token_Iterator : Token_List.List_Iterator;

      Derived_Token : Token.Token_ID;

      Derivations   : Token_ID_Set := (others => False);
      Added_Tokens  : Token_ID_Set := (others => False);
      Search_Tokens : Token_ID_Set := (others => False);

   begin

      Search_Tokens (Non_Terminal) := True;

      while Search_Tokens /= Token_ID_Set'(others => False) loop

         Added_Tokens := (others => False);

         --  search all productions for rightmost derivations for
         --  tokens we found last time.
         Prod_Iterator := Production_List.Initial_Iterator (Grammar);
         while not Production_List.Past_Last (Prod_Iterator) loop
            if
              Search_Tokens (Token.ID (Production_List.Get_Production (Prod_Iterator).LHS.all))
            then
               Token_Iterator := Token_List.Initial_Iterator
                 (Production_List.Get_Production (Prod_Iterator).RHS.Tokens);

               if Token_Iterator /= Token_List.Null_Iterator then
                  Derived_Token := Token.ID (Token_List.Token_Handle (Token_Iterator).all);

                  if not Derivations (Derived_Token) then
                     Added_Tokens (Derived_Token) := True;
                  end if;
               end if;
            end if;

            Production_List.Next_Production (Prod_Iterator);
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
   end First_Derivations;

   function First_Derivations
     (Grammar : in Production_List.Instance;
      Trace   : in Boolean)
     return Derivation_Matrix
   is
      Matrix : Derivation_Matrix;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("First Derivations:");
      end if;

      for NT_Index in Matrix'Range loop
         Matrix (NT_Index) := First_Derivations (Grammar, NT_Index, Trace);
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Matrix;
   end First_Derivations;

   function Deep_Copy (Item : in Item_Lookahead_Ptr) return Item_Lookahead_Ptr
   is
      I      : Item_Lookahead_Ptr := Item;
      Result : Item_Lookahead_Ptr;
   begin
      while I /= null loop
         Result := new Item_Lookahead'
           (Last       => I.Last,
            Lookaheads => I.Lookaheads,
            Next       => Result);
         I := I.Next;
      end loop;
      return Result;
   end Deep_Copy;

   function Item_Node_Of
     (Prod       : in Production_List.List_Iterator;
      Index      : in Integer;
      Lookaheads : in Item_Lookahead_Ptr := null)
     return Item_Node
   is begin
      return
        (Prod       => Production_List.Get_Production (Prod),
         Dot        => Token_List.Initial_Iterator (Production_List.Get_Production (Prod).RHS.Tokens),
         Index      => Index,
         Lookaheads => Deep_Copy (Lookaheads),
         Next       => null);
   end Item_Node_Of;

   function Item_Node_Of
     (Prod  : in OpenToken.Production.Instance;
      Index : in Integer)
     return Item_Node
   is begin
      return
        (Prod       => Prod,
         Dot        => Token_List.Initial_Iterator (Prod.RHS.Tokens),
         Index      => Index,
         Lookaheads => null,
         Next       => null);
   end Item_Node_Of;

   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead;
      Added :    out Boolean)
   is
      Found_Match : Boolean := False;
      Match_Set   : Item_Lookahead_Ptr := Set;
   begin

      --  Look for a lookahead that matches
      while Match_Set /= null loop
         if
           Match_Set.Lookaheads (1 .. Match_Set.Last) =
           Value.Lookaheads (1 .. Value.Last)
         then
            Found_Match := True;
            exit;
         end if;

         Match_Set := Match_Set.Next;
      end loop;

      --  If we didn't find one, add one
      if not Found_Match then
         Set := new Item_Lookahead'(Last       => Value.Last,
                                    Lookaheads => Value.Lookaheads,
                                    Next       => Set
                                   );
      end if;

      Added := not Found_Match;
   end Include;

   procedure Include
     (Set   : in out Item_Lookahead_Ptr;
      Value : in     Item_Lookahead)
   is
      Added : Boolean;
   begin
      Include (Set => Set, Value => Value, Added => Added);
   end Include;

   procedure Add
     (New_Item : in     Item_Node;
      Target   : in out Item_Set)
   is begin
      Target.Set := new Item_Node'
        (Prod       => New_Item.Prod,
         Dot        => New_Item.Dot,
         Index      => Target.Index,
         Lookaheads => New_Item.Lookaheads,
         Next       => Target.Set);
   end Add;

   function Find
     (Left  : in Item_Node;
      Right : in Item_Set)
     return Item_Ptr
   is
      use type Token_List.List_Iterator;
      Current : Item_Ptr := Right.Set;
   begin
      while Current /= null loop
         if Left.Prod = Current.Prod and Left.Dot = Current.Dot then
            return Current;
         end if;
         Current := Current.Next;
      end loop;
      return null;
   end Find;

   function Find
     (Left  : in Item_Set;
      Right : in Item_Set_List)
   return Item_Set_Ptr
   is
      Right_Set  : Item_Set_Ptr := Right.Head;
      Right_Item : Item_Ptr;
      Left_Size  : Natural      := 0;
      Right_Size : Natural;
   begin
      --  Count the number of items in the left set
      Right_Item := Left.Set;
      while Right_Item /= null loop
         Left_Size := Left_Size + 1;

         Right_Item := Right_Item.Next;
      end loop;

      --  Go through the sets in the set list...
      while Right_Set /= null loop

         Right_Item := Right_Set.Set;
         Right_Size := 0;
         while Right_Item /= null loop

            if Find (Right_Item.all, Left) = null then
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
     (Index : in Integer;
      Sets  : in Item_Set_List)
     return Item_Set_Ptr
   is
      Set : Item_Set_Ptr := Sets.Head;
   begin
      while Set /= null loop
         if Set.Index = Index then
            return Set;
         end if;

         Set := Set.Next;
      end loop;

      return null;
   end Find;

   function Is_In
     (Left  : in Item_Set;
      Right : in Item_Set_List)
     return Boolean
   is begin
      return Find (Left, Right) /= null;
   end Is_In;

   function Is_In
     (Set_Ptr   : in Item_Set_Ptr;
      Symbol    : in Token.Token_ID;
      Goto_List : in Set_Reference_Ptr)
     return Boolean
   is
      Goto_Ptr : Set_Reference_Ptr := Goto_List;
      use type Token.Token_ID;
   begin
      while Goto_Ptr /= null loop
         if Goto_Ptr.Set = Set_Ptr and Goto_Ptr.Symbol = Symbol then
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
      Goto_Ptr : Set_Reference_Ptr := From.Goto_List;
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

   procedure Merge
     (New_Item     : in out Item_Node;
      Existing_Set : in out Item_Set)
   is
      Found : constant Item_Ptr := Find (New_Item, Existing_Set);

      Source_Lookahead      : Item_Lookahead_Ptr;
      Previous_Lookahead    : Item_Lookahead_Ptr;
      Destination_Lookahead : Item_Lookahead_Ptr;
      Found_Match           : Boolean;

   begin
      if Found = null then
         Existing_Set.Set := new Item_Node'
           (Prod       => New_Item.Prod,
            Dot        => New_Item.Dot,
            Index      => -1,
            Lookaheads => New_Item.Lookaheads,
            Next       => Existing_Set.Set);
      else
         --  Merge their lookaheads.
         Source_Lookahead := New_Item.Lookaheads;
         while Source_Lookahead /= null loop
            Destination_Lookahead := Found.Lookaheads;

            Found_Match := False;
            while Destination_Lookahead /= null loop

               if
                 Destination_Lookahead.Lookaheads (1 .. Destination_Lookahead.Last) =
                 Source_Lookahead.Lookaheads (1 .. Source_Lookahead.Last)
               then
                  Found_Match := True;
                  exit;
               end if;

               Destination_Lookahead := Destination_Lookahead.Next;
            end loop;

            if not Found_Match then
               if Previous_Lookahead = null then
                  New_Item.Lookaheads := Source_Lookahead.Next;
               else
                  Previous_Lookahead.Next := Source_Lookahead.Next;
               end if;

               Source_Lookahead.Next := Found.Lookaheads;
               Found.Lookaheads := Source_Lookahead;

               if Previous_Lookahead = null then
                  Source_Lookahead := New_Item.Lookaheads;
               else
                  Source_Lookahead := Previous_Lookahead.Next;
               end if;
            else
               Previous_Lookahead := Source_Lookahead;
               Source_Lookahead := Source_Lookahead.Next;
            end if;

         end loop;

         Free (New_Item);
      end if;
   end Merge;

   function Lookahead_Closure
     (Set     : in Item_Set;
      First   : in Derivation_Matrix;
      Grammar : in Production_List.Instance)
     return Item_Set
   is
      use type Token.Token_ID;
      use type Token.Handle;

      Item    : Item_Ptr := Set.Set;
      Current : Item_Ptr;
      Start   : Item_Ptr;
      Finish  : Item_Ptr := null;

      Next_Symbol         : Token_List.List_Iterator;
      Production_Iterator : Production_List.List_Iterator;

      Result : Item_Set;

      Merge_From : Item_Node;
   begin
      --  Put copies of everything in Set into the closure. We don't
      --  copy Goto_List, since we are only concerned about lookaheads
      --  here.

      Result.Index := -1; -- Result does _not_ match any kernel set
      while Item /= null loop
         Result.Set := new Item_Node'
           (Prod       => Item.Prod,
            Dot        => Item.Dot,
            Index      => -1,
            Lookaheads => Deep_Copy (Item.Lookaheads),
            Next       => Result.Set);

         Item := Item.Next;
      end loop;

      Current := Result.Set;
      Start   := Result.Set;
      loop

         --  If the token after Dot is a nonterminal, find its
         --  productions and place them in the set with lookaheads
         --  from the current production.
         if Token_List.Token_Handle (Current.Dot) /= null and then
           Token_List.ID (Current.Dot) in Nonterminal_ID
         then
            declare
               Current_ID : constant Token.Token_ID := Token_List.ID (Current.Dot);
            begin
               Next_Symbol := Current.Dot;
               Token_List.Next_Token (Next_Symbol); -- token after nonterminal, possibly null

               Production_Iterator := Production_List.Initial_Iterator (Grammar);
               while not Production_List.Past_Last (Production_Iterator) loop
                  if LHS_ID (Production_List.Get_Production (Production_Iterator)) = Current_ID then

                     if Token_List.Token_Handle (Next_Symbol) = null then
                        --  Need a variable, because the lookaheads might be freed.
                        Merge_From := Item_Node_Of
                          (Production_Iterator,
                           Index      => -1,
                           Lookaheads => Current.Lookaheads);

                        Merge (Merge_From, Result);

                     elsif Token_List.ID (Next_Symbol) in Tokenizer.Terminal_ID then

                        --  Only necessary if nonterminal can be null,
                        --  but that's expensive to check.
                        Merge_From := Item_Node_Of
                          (Production_Iterator,
                           Index => -1,
                           Lookaheads => new Item_Lookahead'
                             (Last       => 1,
                              Lookaheads => (1 => Token_List.ID (Next_Symbol)),
                              Next       => null));

                        Merge (Merge_From, Result);

                     else
                        --  Next_Symbol is a nonterminal

                        for Terminal in Tokenizer.Terminal_ID loop

                           if First (Token.ID (Token_List.Token_Handle (Next_Symbol).all)) (Terminal) then

                              Merge_From := Item_Node_Of
                                (Production_Iterator,
                                 Index => -1,
                                 Lookaheads => new Item_Lookahead'
                                   (Last       => 1,
                                    Lookaheads => (1 => Terminal),
                                    Next       => null));

                              Merge (Merge_From, Result);

                           end if;
                        end loop;

                     end if;
                  end if;

                  Production_List.Next_Production (Production_Iterator);
               end loop;
            end;
         end if; -- Dot is is at non-terminal

         if Current.Next = Finish then
            exit when Result.Set = Start; -- no new items were added to Result

            Finish  := Start; -- only review the new items
            Start   := Result.Set;
            Current := Result.Set;

         else
            Current := Current.Next;
         end if;

      end loop;

      return Result;
   end Lookahead_Closure;

   function Goto_Transitions
     (Kernel       : in Item_Set;
      Symbol       : in Token.Token_ID;
      First_Tokens : in Derivation_Matrix;
      Grammar      : in Production_List.Instance;
      Trace        : in Boolean)
     return Item_Set
   is

      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;

      Item            : Item_Ptr := Kernel.Set;
      Item_Pointer_ID : Token.Token_ID;
      RHS_Pointer     : Token_List.List_Iterator;
      Prod            : OpenToken.Production.Instance;
      Prod_Index      : Production_List.List_Iterator;
   begin
      Goto_Set.Index := -1;

      while Item /= null loop

         if Token_List.Token_Handle (Item.Dot) /= null then

            Item_Pointer_ID := Token.ID (Token_List.Token_Handle (Item.Dot).all);
            --  ID of token after Item Dot

            if Item_Pointer_ID = Symbol then
               Goto_Set.Set := new Item_Node'
                 (Prod       => Item.Prod,
                  Dot        => Token_List.Next_Token (Item.Dot),
                  Index      => -1, -- replaced in LR0_Kernels
                  Lookaheads => Item.Lookaheads,
                  Next       => Goto_Set.Set);
            end if;

            if Item_Pointer_ID in Nonterminal_ID and then First_Tokens (Item_Pointer_ID)(Symbol) then
               --  Find the production(s) that create that symbol and put
               --  them in
               Prod_Index := Production_List.Initial_Iterator (Grammar);
               while not Production_List.Past_Last (Prod_Index) loop

                  Prod        := Production_List.Get_Production (Prod_Index);
                  RHS_Pointer := Token_List.Initial_Iterator (Prod.RHS.Tokens);

                  if (Token_List.Token_Handle (RHS_Pointer) /= null and then
                        Token.ID (Token_List.Token_Handle (RHS_Pointer).all) = Symbol) and
                    (Item_Pointer_ID = LHS_ID (Prod) or First_Tokens (Item_Pointer_ID)(LHS_ID (Prod)))
                  then
                     declare
                        New_Item : constant Item_Node :=
                          (Prod       => Prod,
                           Dot        => Token_List.Next_Token (RHS_Pointer),
                           Index      => -1, -- replaced in LR0_Kernels
                           Lookaheads => null,
                           Next       => Goto_Set.Set);
                     begin
                        if null = Find (New_Item, Goto_Set) then
                           Goto_Set.Set := new Item_Node'(New_Item);
                        else
                           if Trace then
                              Ada.Text_IO.Put_Line ("... already in goto set");
                           end if;
                        end if;
                     end;
                  end if;

                  Production_List.Next_Production (Prod_Index);
               end loop;
            end if;
         end if;
         Item := Item.Next;
      end loop;

      return Goto_Set;
   end Goto_Transitions;

   procedure Free (Subject : in out Item_Node)
   is
      Lookahead : Item_Lookahead_Ptr := Subject.Lookaheads;
   begin
      while Lookahead /= null loop
         Subject.Lookaheads := Lookahead.Next;
         Free (Lookahead);
         Lookahead := Subject.Lookaheads;
      end loop;

   end Free;

   procedure Free (Subject : in out Item_Set)
   is
      Item     : Item_Ptr          := Subject.Set;
      Goto_Set : Set_Reference_Ptr := Subject.Goto_List;
   begin
      while Item /= null loop
         Subject.Set := Item.Next;
         Free (Item.all);
         Free (Item);
         Item := Subject.Set;
      end loop;

      while Goto_Set /= null loop
         Subject.Goto_List := Goto_Set.Next;
         Free (Goto_Set);
         Goto_Set := Subject.Goto_List;
      end loop;

   end Free;

   procedure Free (Subject : in out Item_Set_List)
   is
      Set : Item_Set_Ptr := Subject.Head;
   begin
      while Set /= null loop
         Subject.Head := Set.Next;

         Free (Set.all);
         Free (Set);

         Set := Subject.Head;
      end loop;
   end Free;

   function LR0_Kernels
     (Grammar      : in Production_List.Instance;
      First_Tokens : in Derivation_Matrix;
      Trace        : in Boolean;
      First_Index  : in Natural)
     return Item_Set_List
   is
      Kernel_List : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of
                 (Production_List.Get_Production (Production_List.Initial_Iterator (Grammar)), First_Index)),
            Goto_List => null,
            Index     => First_Index,
            Next      => null),
         Size         => 1);

      New_Items_To_Check   : Boolean      := True;
      Previous_Kernel_Head : Item_Set_Ptr := null;
      Checking_Set         : Item_Set_Ptr;
      Old_Items            : Item_Set_Ptr := null;
      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;
         Old_Items            := Previous_Kernel_Head;
         Previous_Kernel_Head := Kernel_List.Head;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= Old_Items loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Print_Item_Set (Checking_Set.all);
            end if;

            for Symbol in Token.Token_ID loop

               --  Get the goto items for this kernel and symbol
               New_Items := Goto_Transitions
                 (Kernel       => Checking_Set.all,
                  Symbol       => Symbol,
                  First_Tokens => First_Tokens,
                  Grammar      => Grammar,
                  Trace        => Trace);

               --  See if any of the item sets need to be added to our list
               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, Kernel_List);
                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next  := Kernel_List.Head;
                     New_Items.Index := Kernel_List.Size + First_Index;

                     declare
                        I : Item_Ptr := New_Items.Set;
                     begin
                        while I /= null loop
                           I.Index := New_Items.Index;
                           I       := I.Next;
                        end loop;
                     end;

                     if Trace then
                        Ada.Text_IO.Put ("  adding new kernel on " & Token.Token_Image (Symbol) & ": ");
                        Print_Item_Set (New_Items);
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Checking_Set.Goto_List := new Set_Reference'
                       (Set    => Kernel_List.Head,
                        Symbol => Symbol,
                        Next   => Checking_Set.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Set_Ptr   => New_Items_Set,
                        Symbol    => Symbol,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put ("  adding goto on " & Token.Token_Image (Symbol) & ": ");
                           Print_Item_Set (New_Items_Set.all);
                        end if;

                        Checking_Set.Goto_List := new Set_Reference'
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
   end LR0_Kernels;

   function Token_Name (Subject : in Token.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Subject.all));
   end Token_Name;
   function Token_Name (Subject : in Nonterminal.Handle) return String is
   begin
      return Token.Token_Image (Token.ID (Subject.all));
   end Token_Name;

   function Print (Item : in Item_Lookahead) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if Item.Last = 0 then
         return "#";
      else
         for Index in 1 .. Item.Last loop
            if Index > 1 then
               Result := Result & " ";
            end if;
            Result := Result & Token.Token_Image (Item.Lookaheads (Index));
         end loop;
      end if;

      return To_String (Result);
   end Print;

   function Print (Item : in Item_Lookahead_Ptr) return String
   is
      use Ada.Strings.Unbounded;
      Lookahead : Item_Lookahead_Ptr := Item;
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
      Show_Index      : in Boolean;
      Show_Lookaheads : in Boolean;
      Show_Tag        : in Boolean := False)
     return String
   is
      Token_Index : Token_List.List_Iterator;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String (Token_Name (Item.Prod.LHS)) &
        (if Show_Tag then "(" & Ada.Tags.Expanded_Name (Item.Prod.LHS.all'Tag) & ")"
         else "") &
        " <=";

      use type Token_List.List_Iterator;
      use type Token.Handle;

   begin
      Token_Index := Token_List.Initial_Iterator (Item.Prod.RHS.Tokens);

      while Token_List.Token_Handle (Token_Index) /= null loop
         if Token_Index = Item.Dot then
            Result := Result & " ^ ";
         else
            Result := Result & " ";
         end if;
         Result := Result & Token_Name (Token_List.Token_Handle (Token_Index));
         Token_List.Next_Token (Token_Index);
      end loop;

      if Token_List.Token_Handle (Item.Dot) = null then
         Result := Result & " ^";
      end if;

      if Show_Index then
         Result := Result & " in " & Integer'Image (Item.Index);
      end if;

      if Show_Lookaheads then
         Result := Result & Print (Item.Lookaheads);
      end if;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image_Item;

   procedure Put_Item (Item : in Item_Node; Show_Lookaheads : in Boolean) is
   begin
      Ada.Text_IO.Put (Image_Item (Item, Show_Index => True, Show_Lookaheads => Show_Lookaheads));
   end Put_Item;

   function Image_Set_Reference_List (Reference_List : in Set_Reference_Ptr) return String
   is
      use Ada.Strings.Unbounded;

      Reference : Set_Reference_Ptr := Reference_List;

      Result : Unbounded_String := Null_Unbounded_String;

   begin
      while Reference /= null loop
         Result := Result &
           "      on " & Token.Token_Image (Reference.Symbol) &
           " => Set" & Natural'Image (Reference.Set.Index) & Line_End;

         Reference := Reference.Next;
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end Image_Set_Reference_List;

   procedure Print_Item_Set (Items : in Item_Set)
   is
      use Ada.Text_IO;
      Item : Item_Ptr := Items.Set;
   begin
      Put_Line ("Set" & Integer'Image (Items.Index) & ":");

      while Item /= null loop
         Put_Line ("  " & Image_Item (Item.all, Show_Index => False, Show_Lookaheads => True));

         Item := Item.Next;
      end loop;
   end Print_Item_Set;

   procedure Print_Item_Set_List (Items : in Item_Set_List)
   is
      use Ada.Text_IO;
      Set        : Item_Set_Ptr := Items.Head;
   begin
      Put_Line ("Number of Kernel Sets =" & Integer'Image (Items.Size));

      while Set /= null loop
         Print_Item_Set (Set.all);
         Put_Line ("   Goto:");
         Put (Image_Set_Reference_List (Set.Goto_List));

         Set := Set.Next;
      end loop;
   end Print_Item_Set_List;

end OpenToken.Production.Parser.LRk_Item;
