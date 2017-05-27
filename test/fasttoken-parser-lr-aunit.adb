--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (GPL);

with AUnit.Assertions;
package body FastToken.Parser.LR.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Action_Rec;
      Expected : in LR.Parse_Action_Rec)
   is
      use Standard.AUnit.Checks;
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
      Computed : in LR.Parse_Action_Node_Ptr;
      Expected : in LR.Parse_Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use type LR.Parse_Action_Node_Ptr;
      Computed_I : LR.Parse_Action_Node_Ptr := Computed;
      Expected_I : LR.Parse_Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LR.Action_Node_Ptr; Expected : in LR.Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use type LR.Action_Node_Ptr;
      Computed_I : LR.Action_Node_Ptr := Computed;
      Expected_I : LR.Action_Node_Ptr := Expected;
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

   procedure Check (Label : in String; Computed : in LR.Goto_Node_Ptr; Expected : in LR.Goto_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      use all type LR.Goto_Node_Ptr;
      Computed_I : LR.Goto_Node_Ptr := Computed;
      Expected_I : LR.Goto_Node_Ptr := Expected;
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
      Computed : in LR.Parse_State;
      Expected : in LR.Parse_State)
   is begin
      Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Table;
      Expected : in LR.Parse_Table)
   is begin
      Check (Label & ".States'first", Computed.States'First, Expected.States'First);
      Check (Label & ".States'last", Computed.States'Last, Expected.States'Last);
      for I in Computed.States'Range loop
         Check (Label & ".States." & LR.State_Index'Image (I), Computed.States (I), Expected.States (I));
      end loop;
      Check (Label & ".Panic_Recover", Computed.Panic_Recover, Expected.Panic_Recover);
   end Check;

end FastToken.Parser.LR.AUnit;
