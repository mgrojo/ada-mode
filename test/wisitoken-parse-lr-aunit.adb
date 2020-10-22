--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL); --  AUnit

with AUnit.Assertions;
with AUnit.Checks.Containers;
with SAL.AUnit;
with WisiToken.AUnit;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Syntax_Trees.AUnit_Public;
package body WisiToken.Parse.LR.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Minimal_Action;
      Expected : in Minimal_Action)
   is
      use Standard.AUnit.Checks.Containers;
      use WisiToken.AUnit;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      Check (Label & ".Production", Computed.Production, Expected.Production);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".ID", Computed.ID, Expected.ID);
         if Expected.State /= State_Index'Last then
            Check (Label & ".State", Computed.State, Expected.State);
         end if;
      when Reduce =>
         Check (Label & ".Token_count", Computed.Token_Count, Expected.Token_Count);
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Rec;
      Expected : in Parse_Action_Rec)
   is
      use Standard.AUnit.Checks.Containers;
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use WisiToken.Semantic_Checks.AUnit;
   begin
      Check (Label & ".Verb", Computed.Verb, Expected.Verb);
      case Computed.Verb is
      when Shift =>
         Check (Label & ".State", Computed.State, Expected.State);
      when Reduce | Accept_It =>
         Check (Label & ".Production", Computed.Production, Expected.Production);
         if Strict then
            Check (Label & ".Action", Computed.Action, Expected.Action);
            Check (Label & ".Check", Computed.Check, Expected.Check);
         end if;
         Check (Label & ".Token_Count", Computed.Token_Count, Expected.Token_Count);
      when Error =>
         null;
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Node_Ptr;
      Expected : in Parse_Action_Node_Ptr)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Assertions;
      Computed_I : Parse_Action_Node_Ptr := Computed;
      Expected_I : Parse_Action_Node_Ptr := Expected;
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
      Computed : in Action_Node;
      Expected : in Action_Node)
   is begin
      WisiToken.AUnit.Check (Label & ".Symbol", Computed.Symbol, Expected.Symbol);
      Check (Label & ".Actions", Computed.Actions, Expected.Actions);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Goto_Node;
      Expected : in Goto_Node)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".Symbol", Computed.Symbol, Expected.Symbol);
      Check (Label & ".State", Computed.State, Expected.State);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_State;
      Expected : in Parse_State)
   is
   begin
      Action_Arrays_AUnit.Check (Label & ".Action_List", Computed.Action_List, Expected.Action_List);
      Goto_Arrays_AUnit.Check (Label & ".Goto_List", Computed.Goto_List, Expected.Goto_List);
      if Strict then
         raise SAL.Programmer_Error;
         --  Check (Label & ".Kernel", Computed.Kernel, Expected.Kernel);
         --  Check (Label & ".Minimal_Complete_Actions",
         --         Computed.Minimal_Complete_Actions,
         --         Expected.Minimal_Complete_Actions);
      end if;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Parse_Table;
      Expected : in Parse_Table)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".States'first", Computed.States'First, Expected.States'First);
      Check (Label & ".States'last", Computed.States'Last, Expected.States'Last);
      for I in Computed.States'Range loop
         Check
           (Label & ".States." & Trimmed_Image (I), Computed.States (I), Expected.States (I));
      end loop;
      --  Ignoring Production.
      --  We do not check McKenzie, since that is not computed.
   end Check;

   procedure Check
     (Label    : in String;
      Tree     : in Syntax_Trees.Tree;
      Computed : in Config_Op;
      Expected : in Test_Config_Op)
   is
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use Standard.AUnit.Checks.Containers;
   begin
      Check (Label & ".op", Computed.Op, Expected.Op);
      case Computed.Op is
      when Fast_Forward =>
         Check (Label & ".ff_token_index", Tree.Get_Element_Index
                  (Tree.Shared_Stream, Computed.FF_Token_Index), Expected.FF_Token_Index);

      when Undo_Reduce =>
         Check (Label & ".nonterm", Computed.Nonterm, Expected.Nonterm);
         Check (Label & ".id", Computed.Token_Count, Expected.Token_Count);

      when Push_Back =>
         Check (Label & ".id", Computed.PB_ID, Expected.PB_ID);
         Check (Label & ".token_index", Tree.Get_Element_Index
                  (Tree.Shared_Stream, Computed.PB_Token_Index), Expected.PB_Token_Index);

      when Insert =>
         Check (Label & ".id", Computed.Ins_ID, Expected.Ins_ID);

         if Tree.Editable then
            --  Execute_Actions has been run; .ins_before is invalid
            null;
         else
            Check (Label & ".ins_before", Tree.Get_Element_Index
                     (Tree.Shared_Stream, Computed.Ins_Before), Expected.Ins_Before);
         end if;

      when Delete =>
         Check (Label & ".id", Computed.Del_ID, Expected.Del_ID);
         Check (Label & ".token_index", Tree.Get_Element_Index
                  (Tree.Shared_Stream, Computed.Del_Token_Index), Expected.Del_Token_Index);
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Tree     : in Syntax_Trees.Tree;
      Computed : in Config_Op_Arrays.Vector;
      Expected : in Test_Config_Op_Arrays.Vector)
   is
      use SAL.AUnit;
      use Config_Op_Arrays;
      use Test_Config_Op_Arrays;
   begin
      Check (Label & ".First_Index", First_Index (Computed), First_Index (Expected));
      Check (Label & ".Last_Index", Last_Index (Computed), Last_Index (Expected));
      for I in First_Index (Computed) .. Last_Index (Computed) loop
         Check (Label & "." & I'Image, Tree, Element (Computed, I), Element (Expected, I));
      end loop;
   end Check;

end WisiToken.Parse.LR.AUnit;
