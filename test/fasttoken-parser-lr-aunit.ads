--  Abstract :
--
--  AUnit Checks for parent
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

with Ada.Containers;
with AUnit.Checks;
generic
package FastToken.Parser.LR.AUnit is

   --  Some of these duplicate Gen_FastToken_AUnit, but GNAT chokes if we try to do it right.

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Token_ID);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Unknown_State_Index);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Parse_Action_Verbs);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Token_Pkg.Nonterminal_ID,
      Array_Type  => Token_Pkg.Nonterminal_ID_Set,
      Check_Index => Check,
      Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check (Label : in String; Computed : in Parse_Action_Rec; Expected : in Parse_Action_Rec);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Action_Node_Ptr;
      Expected : in Parse_Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in Action_Node_Ptr; Expected : in Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in Goto_Node_Ptr; Expected : in Goto_Node_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in Parse_State;
      Expected : in Parse_State);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Table;
      Expected : in Parse_Table);

end FastToken.Parser.LR.AUnit;
