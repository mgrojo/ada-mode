--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018 - 2020 Stephen Leake All Rights Reserved.
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
with WisiToken.Syntax_Trees.AUnit_Public;
package body WisiToken.Syntax_Trees.AUnit_Private is

   procedure Check
     (Label    : in String;
      Computed : in Node;
      Expected : in Node)
   is
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
   begin
      Check (Label & ".label", Computed.Label, Expected.Label);
      Check (Label & ".parent", Computed.Parent, Expected.Parent);
      Check (Label & ".id", Computed.ID, Expected.ID);
      Check (Label & ".byte_region", Computed.Byte_Region, Expected.Byte_Region);

      case Computed.Label is
      when Shared_Terminal =>
         null;
      when Virtual_Terminal | Virtual_Identifier =>
         null;
      when Nonterm =>
         Check (Label & ".children'length", Computed.Child_Count, Expected.Child_Count);
         for I in Computed.Children'Range loop
            Check (Label & ".child." & I'Image, Computed.Children (I), Expected.Children (I));
         end loop;
         Check (Label & ".action", Computed.Action, Expected.Action);
      end case;
   end Check;

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID)
   is
      Computed_Element : Stream_Index := Computed_Tree.Streams (Computed_Stream.Cur).First;
      Expected_Element : Stream_Index := Expected_Tree.Streams (Expected_Stream.Cur).First;
   begin
      Check (Label & ".length", Computed_Tree.Length (Computed_Stream), Expected_Tree.Length (Expected_Stream));
      loop
         exit when Computed_Element = null or Expeced_Element = null;
         Check (Label & ".state", Computed_Element.State, Expected_Element.State);
         Check (Label & ".node", Computed_Element.Node, Expected_Element.Node);

         Computed_Element := @.Next;
         Expected_Element := @.Next;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Tree;
      Expected : in Tree)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
   begin
      Check (Label & ".leading_non_grammar", Computed.Leading_Non_Grammar, Expected.Leading_Non_Grammar);
      Check (Label & ".streams", Computed.Streams, Expected.Streams);
      Check (Label & ".nodes", Computed.Nodes, Expected.Nodes);
   end Check;

end WisiToken.Syntax_Trees.AUnit_Private;
