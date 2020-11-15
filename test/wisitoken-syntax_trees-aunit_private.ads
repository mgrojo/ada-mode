--  Abstract :
--
--  Private AUnit checks for parent
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
private package WisiToken.Syntax_Trees.AUnit_Private is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Stream_Label);

   procedure Check
     (Label    : in String;
      Computed : in Node;
      Expected : in Node;
      Parents  : in Boolean);
   --  Does not compare Augmented, Node_Index

   procedure Check
     (Label           : in String;
      Computed_Tree   : in Syntax_Trees.Tree;
      Computed_Stream : in Stream_ID;
      Expected_Tree   : in Syntax_Trees.Tree;
      Expected_Stream : in Stream_ID;
      Check_Label     : in Boolean;
      Parents         : in Boolean);

   procedure Check
     (Label         : in String;
      Computed      : in Tree;
      Expected      : in Tree;
      Shared_Stream : in Boolean);

end WisiToken.Syntax_Trees.AUnit_Private;
