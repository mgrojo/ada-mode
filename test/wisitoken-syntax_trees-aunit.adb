--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.AUnit;
with WisiToken.Semantic_Checks.AUnit;
with WisiToken.Semantic_State.AUnit;
package body WisiToken.Syntax_Trees.AUnit is

   function "+" (Item : in Node_Index_Type) return Node_Index_Arrays.Vector
   is begin
      return Node_Index_Arrays.To_Vector (Item, 1);
   end "+";

   procedure Check
     (Label    : in String;
      Computed : in Node;
      Expected : in Node)
   is
      use SAL.AUnit;
      use WisiToken.AUnit;
      use WisiToken.Semantic_State.AUnit;
      use WisiToken.Semantic_Checks.AUnit;
   begin
      Check (Label & ".parent", Computed.Parent, Expected.Parent);
      Check (Label & ".terminal", Computed.Terminal, Expected.Terminal);
      Check (Label & ".nonterm", Computed.Nonterm, Expected.Nonterm);
      Check (Label & ".children", Computed.Children, Expected.Children);
      Check (Label & ".action", Computed.Action, Expected.Action);
      Check (Label & ".check", Computed.Check, Expected.Check);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Tree;
      Expected : in Tree)
   is begin
      Check (Label & ".root", Computed.Root, Expected.Root);
      Check (Label & ".nodes", Computed.Nodes, Expected.Nodes);
   end Check;

end WisiToken.Syntax_Trees.AUnit;
