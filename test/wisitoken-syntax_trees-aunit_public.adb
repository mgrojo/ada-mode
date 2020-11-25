--  Abstract :
--
--  See spec
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

with AUnit.Assertions;
with WisiToken.AUnit;
with WisiToken.Syntax_Trees.AUnit_Private;
package body WisiToken.Syntax_Trees.AUnit_Public is

   procedure Check (Label : in String; Computed, Expected : in Recover_Token)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Post_Parse_Action;
      Expected : in Post_Parse_Action)
   is begin
      Standard.AUnit.Assertions.Assert (Computed = Expected, Label & ": access type mismatch");
   end Check;

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID;
      Check_Label     : in Boolean;
      Parents         : in Boolean)
   is begin
      WisiToken.Syntax_Trees.AUnit_Private.Check
        (Label, Computed_Tree, Computed_Stream, Expected_Tree, Expected_Stream, Check_Label, Parents);
   end Check;

   procedure Check
     (Label         : in String;
      Computed      : in Tree;
      Expected      : in Tree;
      Shared_Stream : in Boolean)
   is begin
      WisiToken.Syntax_Trees.AUnit_Private.Check (Label, Computed, Expected, Shared_Stream);
   end Check;

end WisiToken.Syntax_Trees.AUnit_Public;
