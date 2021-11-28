--  Abstract :
--
--  Public AUnit checks for parent
--
--  Copyright (C) 2018, 2020 - 2021 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks;
package WisiToken.Syntax_Trees.AUnit_Public is

   procedure Check_Address is new Standard.AUnit.Checks.Gen_Check_Access (Node, Node_Access);
   --  Checks that access value is the same

   procedure Check_Content (Label : in String; Computed, Expected : in Node_Access);
   --  Checks that the tokens have the same Label, ID, Byte_Region,
   --  Char_Region, non-grammar.

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Node_Label);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Node_Index);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Base_Sequential_Index);

   procedure Check (Label : in String; Computed, Expected : in Recover_Token);
   --  Not all components checked.

   procedure Check
     (Label    : in String;
      Computed : in Post_Parse_Action;
      Expected : in Post_Parse_Action);

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID;
      Parents         : in Boolean);

   procedure Check
     (Label                 : in String;
      Computed              : in Tree;
      Expected              : in Tree;
      Shared_Stream         : in Boolean;
      Terminal_Node_Numbers : in Boolean);

   procedure Set_Parents_Set (Tree : in out Syntax_Trees.Tree; Parents_Set : in Boolean);

end WisiToken.Syntax_Trees.AUnit_Public;
