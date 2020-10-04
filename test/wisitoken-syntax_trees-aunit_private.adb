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

with SAL.AUnit;
with WisiToken.AUnit;
with WisiToken.Syntax_Trees.AUnit_Public;
package body WisiToken.Syntax_Trees.AUnit_Private is

   procedure Check
     (Label    : in String;
      Computed : in Node;
      Expected : in Node)
   is
      use SAL.AUnit;
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
   begin
      Check (Label & ".label", Computed.Label, Expected.Label);
      Check (Label & ".id", Computed.ID, Expected.ID);
      --  meaningless unless Terminal Check (Label & ".node_index", Computed.Node_Index, Expected.Node_Index);
      Check (Label & ".byte_region", Computed.Byte_Region, Expected.Byte_Region);
      Check (Label & ".line", Computed.Line, Expected.Line);
      --  can't compare parents Check (Label & ".parent", Computed.Parent.Node_Index, Expected.Parent.Node_Index);
      Check (Label & ".state", Computed.State, Expected.State);

      --  Not checking Augmented

      case Computed.Label is
      when Shared_Terminal =>
         Check (Label & ".char_region", Computed.Char_Region, Expected.Char_Region);
         Check (Label & ".Terminal_Index",
                Stream_Element_Lists.Constant_Ref (Computed.Terminal_Index.Cur).Index,
                Stream_Element_Lists.Constant_Ref (Expected.Terminal_Index.Cur).Index);

      when Virtual_Terminal | Virtual_Identifier =>
         Check (Label & ".Identifier", Computed.Identifier, Expected.Identifier);

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
      use SAL.AUnit;
      use Stream_Element_Lists;
      Computed_Element : Cursor := Computed_Tree.Streams (Computed_Stream.Cur).Elements.First;
      Expected_Element : Cursor := Expected_Tree.Streams (Expected_Stream.Cur).Elements.First;
   begin
      Check (Label & ".length",
             Computed_Tree.Stream_Length (Computed_Stream),
             Expected_Tree.Stream_Length (Expected_Stream));
      Check (Label & ".stack_depth",
             Computed_Tree.Stack_Depth (Computed_Stream),
             Expected_Tree.Stack_Depth (Expected_Stream));
      loop
         exit when not (Has_Element (Computed_Element) and Has_Element (Expected_Element));
         Check (Label & ".node", Constant_Ref (Computed_Element).Node, Constant_Ref (Expected_Element).Node);

         Computed_Element := Next (@);
         Expected_Element := Next (@);
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Tree;
      Expected : in Tree)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit.Base_Token_Arrays_AUnit;
      use Parse_Stream_Lists;

      Computed_Stream : Parse_Stream_Lists.Cursor := Computed.Streams.First;
      Expected_Stream : Parse_Stream_Lists.Cursor := Expected.Streams.First;

   begin
      Check (Label & ".leading_non_grammar", Computed.Leading_Non_Grammar, Expected.Leading_Non_Grammar);
      Check (Label & ".stream_count", Computed.Stream_Count, Expected.Stream_Count);
      loop
         exit when not (Has_Element (Computed_Stream) and Has_Element (Expected_Stream));
         Check
           (Label & ".streams" & Computed.Streams (Computed_Stream).Label'Image,
            Computed, (Cur => Computed_Stream),
            Expected, (Cur => Expected_Stream));

         Next (Expected_Stream);
         Next (Computed_Stream);
      end loop;

      --  We can't check Tree.Nodes; that's in arbitrary order, and will
      --  differ between batch parse of edited source and edited tree.
      --  FIXME: need to walk both trees in sync, compare nodes.
      raise SAL.Not_Implemented;
   end Check;

end WisiToken.Syntax_Trees.AUnit_Private;
