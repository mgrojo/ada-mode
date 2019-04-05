--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
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
package body WisiToken.Syntax_Trees.AUnit_Private is

   procedure Check
     (Label    : in String;
      Computed : in Node;
      Expected : in Node)
   is
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use WisiToken.Syntax_Trees.AUnit_Public.Valid_Node_Index_Arrays_AUnit;
   begin
      Check (Label & ".label", Computed.Label, Expected.Label);
      Check (Label & ".parent", Computed.Parent, Expected.Parent);
      Check (Label & ".id", Computed.ID, Expected.ID);

      case Computed.Label is
      when Shared_Terminal =>
         Check (Label & ".terminal", Computed.Terminal, Expected.Terminal);
      when Virtual_Terminal | Virtual_Identifier =>
         null;
      when Nonterm =>
         Check (Label & ".children", Computed.Children, Expected.Children);
         Check (Label & ".action", Computed.Action, Expected.Action);
      end case;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Tree;
      Expected : in Tree)
   is
      use Standard.AUnit.Checks;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use Node_Arrays_AUnit;
   begin
      --  Ignoring shared_tree
      Check (Label & ".last_shared_node", Computed.Last_Shared_Node, Expected.Last_Shared_Node);
      Check (Label & ".branched_nodes", Computed.Branched_Nodes, Expected.Branched_Nodes);
      Check (Label & ".Flush", Computed.Flush, Expected.Flush);
      Check (Label & ".root", Computed.Root, Expected.Root);
   end Check;

end WisiToken.Syntax_Trees.AUnit_Private;
