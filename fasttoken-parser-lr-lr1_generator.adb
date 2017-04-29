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

with Ada.Text_IO;
package body FastToken.Parser.LR.LR1_Generator is

   function LR1_Item_Sets
     (Grammar           : in Production.List.Instance;
      First             : in LR1_Items.Derivation_Matrix;
      EOF_Token         : in Token.Token_ID;
      First_State_Index : in Unknown_State_Index;
      Trace             : in Boolean)
     return LR1_Items.Item_Set_List
   is
      use type Token.List.List_Iterator;
      use type Token.Token_ID;
      use LR1_Items;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

      C : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of (Production.List.First (Grammar), First_State_Index)),
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

                     Checking_Set.Goto_List := new Goto_Item'
                       (Symbol => Symbol,
                        Set    => C.Head,
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
                          (Symbol => Symbol,
                           Set    => New_Items_Set,
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
   end LR1_Item_Sets;

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
