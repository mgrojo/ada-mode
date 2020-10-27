--  Abstract :
--
--  Public AUnit checks for parent
--
--  Copyright (C) 2018, 2020 Stephen Leake All Rights Reserved.
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
package WisiToken.Syntax_Trees.AUnit_Public is

   procedure Check is new AUnit.Checks.Gen_Check_Access (Node, Node_Access);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Node_Label);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Node_Index);

   procedure Check (Label : in String; Computed, Expected : in Recover_Token);
   --  Not all components checked.

   procedure Check
     (Label    : in String;
      Computed : in Semantic_Action;
      Expected : in Semantic_Action);

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID;
      Check_Label     : in Boolean);

   procedure Check
     (Label         : in String;
      Computed      : in Tree;
      Expected      : in Tree;
      Shared_Stream : in Boolean);

end WisiToken.Syntax_Trees.AUnit_Public;
