--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018 - 2022 Stephen Leake All Rights Reserved.
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
with WisiToken.Lexer.AUnit;
with WisiToken.Syntax_Trees.AUnit_Public;
package body WisiToken.Syntax_Trees.AUnit_Private is

   procedure Check
     (Label                 : in String;
      Computed              : in Node;
      Expected              : in Node;
      Parents               : in Boolean;
      Terminal_Node_Numbers : in Boolean)
   is
      use Standard.AUnit.Checks;
      use SAL.AUnit;
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
   begin
      Check (Label & ".label", Computed.Label, Expected.Label);
      Check (Label & ".child_count", Computed.Child_Count, Expected.Child_Count);
      Check (Label & ".id", Computed.ID, Expected.ID);
      if Terminal_Node_Numbers and Computed.Node_Index > 0 then
         Check (Label & ".node_index", Computed.Node_Index, Expected.Node_Index);
      end if;
      if Parents then
         if Computed.Parent = null and Expected.Parent = null then
            null;
         elsif Computed.Parent = null then
            Standard.AUnit.Assertions.Assert (False, Label & ".parent null expected set");
         elsif Expected.Parent = null then
            Standard.AUnit.Assertions.Assert (False, Label & ".parent set expected null");
         end if;
      end if;

      --  Not comparing Augmented; not used in unit tests.

      case Computed.Label is
      when Source_Terminal =>
         Check (Label & ".byte_region", Computed.Byte_Region, Expected.Byte_Region);
         Check (Label & ".char_region", Computed.Char_Region, Expected.Char_Region);
         Lexer.AUnit.Token_Arrays_AUnit.Check (Label & ".non_grammar", Computed.Non_Grammar, Expected.Non_Grammar);
         Check (Label & ".sequential_index", Computed.Sequential_Index, Expected.Sequential_Index);

      when Virtual_Terminal =>
         Lexer.AUnit.Token_Arrays_AUnit.Check
           (Label & ".non_grammar", Computed.Non_Grammar, Expected.Non_Grammar);
         Check (Label & ".sequential_index", Computed.Sequential_Index, Expected.Sequential_Index);

      when Virtual_Identifier =>
         Check (Label & ".identifier", Computed.Identifier, Expected.Identifier);
         Lexer.AUnit.Token_Arrays_AUnit.Check
           (Label & ".non_grammar", Computed.Non_Grammar, Expected.Non_Grammar);
         Check (Label & ".sequential_index", Computed.Sequential_Index, Expected.Sequential_Index);

      when Nonterm =>
         Check (Label & ".virtual", Computed.Virtual, Expected.Virtual);
         Check (Label & ".rhs_index", Computed.RHS_Index, Expected.RHS_Index);
         Check (Label & ".action", Computed.Action, Expected.Action);
         Check (Label & ".name_offset", Computed.Name_Offset, Expected.Name_Offset);
         Check (Label & ".name_length", Computed.Name_Length, Expected.Name_Length);
         for I in Computed.Children'Range loop
            Check (Label & ".child." & Computed.Children (I).Node_Index'Image,
                   Computed.Children (I).all,
                   Expected.Children (I).all,
                   Parents, Terminal_Node_Numbers);
         end loop;
      end case;
   end Check;

   procedure Check
     (Label                 : in String;
      Computed_Tree         : in Syntax_Trees.Tree;
      Computed_Stream       : in Stream_ID;
      Expected_Tree         : in Syntax_Trees.Tree;
      Expected_Stream       : in Stream_ID;
      Parents               : in Boolean;
      Terminal_Node_Numbers : in Boolean)
   is
      use SAL.AUnit;
      use Stream_Element_Lists;
      use WisiToken.AUnit;
      Computed_Parse_Stream  : Parse_Stream renames Computed_Tree.Streams (Computed_Stream.Cur);
      Expected_Parse_Stream  : Parse_Stream renames Expected_Tree.Streams (Expected_Stream.Cur);
      Computed_Element : Cursor  := Computed_Parse_Stream.Elements.First;
      Expected_Element : Cursor  := Expected_Parse_Stream.Elements.First;
      I                : Integer := 1;
   begin
      Check (Label & ".length",
             Computed_Tree.Stream_Length (Computed_Stream),
             Expected_Tree.Stream_Length (Expected_Stream));
      Check (Label & ".stack_depth",
             Computed_Tree.Stack_Depth (Computed_Stream),
             Expected_Tree.Stack_Depth (Expected_Stream));
      loop
         exit when not (Has_Element (Computed_Element) and Has_Element (Expected_Element));
         declare
            Computed : constant Stream_Element := Element (Computed_Element);
            Expected : constant Stream_Element := Element (Expected_Element);
         begin
            Check (Label & " (" & Computed.Node.Node_Index'Image & ").node",
                   Computed.Node.all,
                   Expected.Node.all,
                   Parents,
                   Terminal_Node_Numbers);
            Check (Label & " (" & Computed.Node.Node_Index'Image & ").state", Computed.State, Expected.State);
         end;

         Computed_Element := Next (@);
         Expected_Element := Next (@);
         I := I + 1;
      end loop;
   end Check;

   procedure Check
     (Label                 : in String;
      Computed              : in Tree;
      Expected              : in Tree;
      Shared_Stream         : in Boolean;
      Terminal_Node_Numbers : in Boolean)
   is
      use Standard.AUnit.Checks;
      use Parse_Stream_Lists;

      Computed_Stream : Parse_Stream_Lists.Cursor := Computed.Streams.First;
      Expected_Stream : Parse_Stream_Lists.Cursor := Expected.Streams.First;

   begin
      Check (Label & ".root set", Computed.Root /= null, Expected.Root /= null);
      Check (Label & ".stream_count", Computed.Stream_Count, Expected.Stream_Count);
      loop
         exit when not (Has_Element (Computed_Stream) and Has_Element (Expected_Stream));
         if Shared_Stream or Computed_Stream /= Computed.Shared_Stream.Cur then
            Check
              (Label & ".streams" & Computed.Streams (Computed_Stream).Label'Image,
               Computed, (Cur        => Computed_Stream),
               Expected, (Cur        => Expected_Stream),
               Parents               => False,
               Terminal_Node_Numbers => Terminal_Node_Numbers);
         end if;
         Next (Expected_Stream);
         Next (Computed_Stream);
      end loop;

      if Computed.Stream_Count = 0 and Computed.Root /= null then
         Check (Label & ".root", Computed.Root.all, Expected.Root.all,
                Parents => True,
                Terminal_Node_Numbers => Terminal_Node_Numbers);
      end if;
   end Check;

   procedure Set_Parents_Set (Tree : in out Syntax_Trees.Tree; Parents_Set : in Boolean)
   is begin
      Tree.Parents_Set := Parents_Set;
   end Set_Parents_Set;

end WisiToken.Syntax_Trees.AUnit_Private;
