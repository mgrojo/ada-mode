--  Abstract :
--
--  AUnit checks for parent
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

pragma License (GPL);

with AUnit.Checks;
with SAL.Gen_Unbounded_Definite_Vectors_Sorted.Gen_AUnit;
package WisiToken.Parse.LR.AUnit is

   Strict : Boolean := False;

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (All_Parse_Action_Verbs);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Rec;
      Expected : in Parse_Action_Rec);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Node_Ptr;
      Expected : in Parse_Action_Node_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in Action_Node;
      Expected : in Action_Node);

   package Action_Arrays_AUnit is new Action_Arrays.Gen_AUnit (Check);

   procedure Check
     (Label    : in String;
      Computed : in Goto_Node;
      Expected : in Goto_Node);

   package Goto_Arrays_AUnit is new Goto_Arrays.Gen_AUnit (Check);

   procedure Check
     (Label    : in String;
      Computed : in Minimal_Action;
      Expected : in Minimal_Action);
   --  If Expected.State is State_Index'Last, ignore it.

   procedure Check
     (Label    : in String;
      Computed : in Parse_State;
      Expected : in Parse_State);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Table;
      Expected : in Parse_Table);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Config_Op_Label);

   type Test_Config_Op (Op : Config_Op_Label := Fast_Forward) is record
      --  Replace Stream_Index with Element_Index

      case Op is
      when Fast_Forward =>
         FF_Token_Index : Syntax_Trees.Element_Index;

      when Undo_Reduce =>
         Nonterm     : Token_ID;
         Token_Count : Ada.Containers.Count_Type;

      when Push_Back =>
         PB_ID          : Token_ID;
         PB_Token_Index : Syntax_Trees.Element_Index;

      when Insert =>
         Ins_ID          : Token_ID;
         Ins_Token_Index : Syntax_Trees.Element_Index;

      when Delete =>
         Del_ID          : Token_ID;
         Del_Token_Index : Syntax_Trees.Element_Index;
      end case;
   end record;

   package Test_Config_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Test_Config_Op, Default_Element => (Fast_Forward, 0), Capacity => 80);

   procedure Check
     (Label    : in String;
      Tree     : in Syntax_Trees.Tree;
      Computed : in Config_Op;
      Expected : in Test_Config_Op);

   procedure Check
     (Label    : in String;
      Tree     : in Syntax_Trees.Tree;
      Computed : in Config_Op_Arrays.Vector;
      Expected : in Test_Config_Op_Arrays.Vector);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Strategies);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Array
     (Item_Type   => Natural,
      Index_Type  => Strategies,
      Array_Type  => Strategy_Counts,
      Check_Index => Check,
      Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Parse_Error_Label);

end WisiToken.Parse.LR.AUnit;
