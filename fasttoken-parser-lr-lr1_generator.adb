--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body FastToken.Parser.LR.LR1_Generator is

   function Closure
     (Set                  : in Item_Set;
      Has_Empty_Production : in Nonterminal_ID_Set;
      First                : in Derivation_Matrix;
      G                    : in Production.List.Instance;
      Trace                : in Boolean)
     return Item_Set
   is
      use type Token.Token_ID;
      use type Token.List.List_Iterator;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "closure"
      --
      --  Taken literally, the algorithm modifies its input; we make a
      --  copy instead. We don't copy Goto_List, since we are only
      --  concerned with lookaheads here.

      I : Item_Set; --  The result.

      Item       : Item_Ptr := Set.Set;           -- iterator 'for each item in I'
      B          : Production.List.List_Iterator; -- iterator 'for each production in G'
      Added_Item : Boolean;                       -- 'until no more items can be added'

      -- Current             : Item_Ptr;
      Next_Symbol         : Token.List.List_Iterator;
      Merge_From          : Item_Node;
   begin
      --  Copy Set into I; Goto_List not copied
      I.State := Set.State;

      while Item /= null loop
         I.Set := new Item_Node'
           (Prod       => Item.Prod,
            Dot        => Item.Dot,
            State      => Unknown_State,
            Lookaheads => Deep_Copy (Item.Lookaheads),
            Next       => I.Set);

         Item := Item.Next;
      end loop;

      Current        := I.Set;
      Added_Item := False;
      loop
         --  If the token after Dot is a nonterminal, find its
         --  productions and place them in the set with lookaheads
         --  from the current production.
         if Current.Dot /= Token.List.Null_Iterator and then
           Token.List.ID (Current.Dot) in Nonterminal_ID
         then
            Next_Symbol := Token.List.Next_Token (Current.Dot); -- token after nonterminal, possibly null

            B := Production.List.First (G);
            while not Production.List.Is_Done (B) loop
               if Nonterminal.ID (Production.List.Current (B).LHS) = Token.List.ID (Current.Dot) then
                  --  loop until find a terminal, or a nonterminal that cannot be empty, or end of production
                  Empty_Nonterm :
                  loop
                     if Next_Symbol = Token.List.Null_Iterator then
                        --  Need a variable, because the lookaheads might be freed.
                        Merge_From := Item_Node_Of
                          (B,
                           State      => Unknown_State,
                           Lookaheads => Current.Lookaheads);

                        Added_Item := Added_Item or Merge (Merge_From, I);
                        exit Empty_Nonterm;

                     elsif Token.List.ID (Next_Symbol) in Token.Terminal_ID then
                        Merge_From := Item_Node_Of
                          (B,
                           State         => Unknown_State,
                           Lookaheads    => new Item_Lookahead'
                             (Last       => 1,
                              Lookaheads => (1 => Token.List.ID (Next_Symbol)),
                              Next       => null));

                        Added_Item := Added_Item or Merge (Merge_From, I);
                        exit Empty_Nonterm;

                     else
                        --  Next_Symbol is a nonterminal
                        for Terminal in Token.Terminal_ID loop
                           if First (Token.List.ID (Next_Symbol)) (Terminal) then
                              Merge_From := Item_Node_Of
                                (B,
                                 State         => Unknown_State,
                                 Lookaheads    => new Item_Lookahead'
                                   (Last       => 1,
                                    Lookaheads => (1 => Terminal),
                                    Next       => null));

                              Added_Item := Added_Item or Merge (Merge_From, I);
                           end if;
                        end loop;

                        if Has_Empty_Production (Token.List.ID (Next_Symbol)) then
                           Next_Symbol := Token.List.Next_Token (Next_Symbol);
                        else
                           exit Empty_Nonterm;
                        end if;
                     end if;
                  end loop Empty_Nonterm;

                  Next_Symbol := Token.List.Next_Token (Current.Dot);
               end if;

               Production.List.Next (B);
            end loop;
         end if; -- Dot is is at non-terminal

         if Current.Next = null then
            exit when not Added_Item;

            --  This used to have logic to "only review new items",
            --  but that missed items that were modified by adding new
            --  lookaheads. We'll come back and find a better
            --  optimization if this proves too slow.
            Current        := I.Set;
            Added_Item := False;

            if Trace then
               Ada.Text_IO.Put_Line ("I:");
               Put (I);
               Ada.Text_IO.New_Line;
            end if;
         else
            Current := Current.Next;
         end if;

      end loop;

      return I;
   end Lookahead_Closure;

   function LR1_Items
     (Grammar           : in Production.List.Instance;
      First             : in Derivation_Matrix;
      EOF_Token         : in Token.Token_ID;
      First_State_Index : in Unknown_State_Index;
      Trace             : in Boolean)
     return Item_Set_List
   is
      use type Token.List.List_Iterator;
      use type Token.Token_ID;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

      C : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of
                 (Production.List.Current (Production.List.First (Grammar)), First_State_Index)),
            Goto_List => null,
            State     => First_State_Index,
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
         Previous_Kernel_Head := C.Head;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := C.Head;
         while Checking_Set /= Old_Items loop
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
                  New_Items := Goto_Transitions (Checking_Set.all, Symbol, First, Grammar);
               end if;

               --  See if any of the item sets need to be added to our list
               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, C);

                  if New_Items_Set = null then
                     New_Items_To_Check := True;

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
                        Ada.Text_IO.Put ("  adding new kernel on " & Token.Token_Image (Symbol) & ": ");
                        Put (New_Items);
                     end if;

                     C :=
                       (Head => new Item_Set'(New_Items),
                        Size => C.Size + 1);

                     Checking_Set.Goto_List := new Set_Reference'
                       (Set    => C.Head,
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
                           Put (New_Items_Set.all);
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

      return C;
   end LR1_Items;

   function Generate
     (Grammar                  : in Production.List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
   begin
      raise Programmer_Error;
      return null;
   end Generate;

end FastToken.Parser.LR.LR1_Generator;
