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

with AUnit.Assertions;
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
      Check (Label & ".child_count", Computed.Child_Count, Expected.Child_Count);
      Check (Label & ".id", Computed.ID, Expected.ID);
      Check (Label & ".byte_region", Computed.Byte_Region, Expected.Byte_Region);
      Check (Label & ".char_region", Computed.Char_Region, Expected.Char_Region);
      Check (Label & ".line", Computed.Line, Expected.Line);
      if Computed.Parent = null and Expected.Parent = null then
         null;
      elsif Computed.Parent = null then
         Standard.AUnit.Assertions.Assert (False, Label & ".parent null expected set");
      elsif Expected.Parent = null then
         Standard.AUnit.Assertions.Assert (False, Label & ".parent set expected null");
      end if;

      Check (Label & ".state", Computed.State, Expected.State);

      --  Not comparing Augmented; not used in unit tests.

      case Computed.Label is
      when Source_Terminal =>
         Base_Token_Arrays_AUnit.Check (Label & ".non_grammar", Computed.Non_Grammar, Expected.Non_Grammar);

      when Virtual_Terminal =>
         null;

      when Virtual_Identifier =>
         Check (Label & ".identifier", Computed.Identifier, Expected.Identifier);
         Base_Token_Arrays_AUnit.Check (Label & ".non_grammar", Computed.VI_Non_Grammar, Expected.VI_Non_Grammar);

      when Nonterm =>
         for I in Computed.Children'Range loop
            Check (Label & ".child." & Computed.Children (I).Node_Index'Image,
                   Computed.Children (I).all,
                   Expected.Children (I).all);
         end loop;
         Check (Label & ".action", Computed.Action, Expected.Action);
      end case;
   end Check;

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID;
      Check_Label     : in Boolean)
   is
      use SAL.AUnit;
      use Stream_Element_Lists;
      Computed_Parse_Stream  : Parse_Stream renames Computed_Tree.Streams (Computed_Stream.Cur);
      Expected_Parse_Stream  : Parse_Stream renames Expected_Tree.Streams (Expected_Stream.Cur);
      Computed_Element : Cursor  := Computed_Parse_Stream.Elements.First;
      Expected_Element : Cursor  := Expected_Parse_Stream.Elements.First;
      I                : Integer := 1;
   begin
      if Check_Label then
         Check (Label & ".label", Computed_Parse_Stream.Label, Expected_Parse_Stream.Label);
      end if;
      Check (Label & ".length",
             Computed_Tree.Stream_Length (Computed_Stream),
             Expected_Tree.Stream_Length (Expected_Stream));
      Check (Label & ".stack_depth",
             Computed_Tree.Stack_Depth (Computed_Stream),
             Expected_Tree.Stack_Depth (Expected_Stream));
      loop
         exit when not (Has_Element (Computed_Element) and Has_Element (Expected_Element));
         declare
            Computed : Stream_Element renames Constant_Ref (Computed_Element);
            Expected : Stream_Element renames Constant_Ref (Expected_Element);
         begin
            if Check_Label then
               Check (Label & I'Image & ".label", Computed.Label, Expected.Label);
            end if;
            Check (Label & ".node" & Computed.Node.Node_Index'Image,
                   Computed.Node.all,
                   Expected.Node.all);
         end;

         Computed_Element := Next (@);
         Expected_Element := Next (@);
         I := I + 1;
      end loop;
   end Check;

   procedure Check
     (Label         : in String;
      Computed      : in Tree;
      Expected      : in Tree;
      Shared_Stream : in Boolean)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit.Base_Token_Arrays_AUnit;
      use Parse_Stream_Lists;

      Computed_Stream : Parse_Stream_Lists.Cursor := Computed.Streams.First;
      Expected_Stream : Parse_Stream_Lists.Cursor := Expected.Streams.First;

   begin
      Check (Label & ".leading_non_grammar", Computed.Leading_Non_Grammar, Expected.Leading_Non_Grammar);
      Check (Label & ".root set", Computed.Root /= null, Expected.Root /= null);
      Check (Label & ".stream_count", Computed.Stream_Count, Expected.Stream_Count);
      loop
         exit when not (Has_Element (Computed_Stream) and Has_Element (Expected_Stream));
         if Shared_Stream or Computed_Stream /= Computed.Shared_Stream.Cur then
            Check
              (Label & ".streams" & Computed.Streams (Computed_Stream).Label'Image,
               Computed, (Cur => Computed_Stream),
               Expected, (Cur => Expected_Stream),
               Check_Label    => Shared_Stream);
         end if;
         Next (Expected_Stream);
         Next (Computed_Stream);
      end loop;

      if Computed.Stream_Count = 0 and Computed.Root /= null then
         --  If stream_count > 0, root is in one of the streams.
         Check (Label, Computed.Root.all, Expected.Root.all);
      end if;
   end Check;

end WisiToken.Syntax_Trees.AUnit_Private;
