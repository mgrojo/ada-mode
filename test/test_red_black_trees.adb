--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks;
with AUnit.Checks.Containers;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
package body Test_Red_Black_Trees is

   type Cache_Type is record
      Pos : Integer;
   end record;

   function Key (Item : in Cache_Type) return Integer
   is begin
      return Item.Pos;
   end Key;

   package Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Cache_Type,
      Key_Type     => Integer,
      Key          => Key);

   package Tree_Test is new Trees.Gen_Test;
   use Tree_Test;

   Tree : Trees.Tree;

   procedure Check
     (Label        : in String;
      Cursor       : in Trees.Cursor;
      Expected_Pos : in Integer)
   is
      use AUnit.Checks;
   begin
      Check (Label, Tree.Constant_Reference (Cursor).Pos, Expected_Pos);
   end Check;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks.Containers;
      I : Trees.Cursor;
   begin
      --  Build and check tree from [1] example 13.1
      Tree.Insert ((Pos => 3));
      Tree.Insert ((Pos => 7));
      Tree.Insert ((Pos => 10));
      Tree.Insert ((Pos => 12));
      Tree.Insert ((Pos => 14));
      Tree.Insert ((Pos => 16));
      Tree.Insert ((Pos => 15));
      Tree.Insert ((Pos => 17));
      Tree.Insert ((Pos => 19));
      Tree.Insert ((Pos => 20));
      Tree.Insert ((Pos => 21));
      Tree.Insert ((Pos => 23));
      Tree.Insert ((Pos => 26));
      Tree.Insert ((Pos => 28));
      Tree.Insert ((Pos => 30));
      Tree.Insert ((Pos => 41));
      Tree.Insert ((Pos => 35));
      Tree.Insert ((Pos => 38));
      Tree.Insert ((Pos => 39));
      Tree.Insert ((Pos => 47));

      Check ("count", Tree.Count, 20);

      I := Root (Tree);
      Check ("root", I, 26);
      I := Left (I);
      Check ("1", I, 17);
      I := Left (I);
      Check ("2", I, 14);
      I := Left (I);
      Check ("3", I, 10);
      I := Left (I);
      Check ("4", I, 7);
      I := Left (I);
      Check ("5", I, 3);
      Check_Null ("5 left", Left (I));
      Check_Null ("5 right", Right (I));
      I := Parent (I);
      Check_Null ("4 right", Right (I));
      I := Parent (I);
      I := Right (I);
      Check ("6", I, 12);
      Check_Null ("6 left", Left (I));
      Check_Null ("6 right", Right (I));
      I := Parent (I);
      I := Parent (I);
      I := Right (I);
      Check ("7", I, 16);
      I := Left (I);
      Check ("8", I, 3);
      Check_Null ("8 left", Left (I));
      Check_Null ("8 right", Right (I));
      I := Parent (I);
      Check_Null ("7 right", Right (I));
      I := Parent (I);
      I := Parent (I);
      I := Right (I);
      Check ("9", I, 21);
      I := Left (I);
      Check ("10", I, 19);
      Check_Null ("10 left", Left (I));
      I := Right (I);
      Check ("11", I, 20);
      Check_Null ("11 left", Left (I));
      Check_Null ("11 right", Right (I));
      I := Parent (I);
      I := Parent (I);
      I := Right (I);
      Check ("12", I, 23);
      Check_Null ("12 left", Left (I));
      Check_Null ("12 right", Right (I));
      I := Parent (I);
      I := Parent (I);
      I := Parent (I);
      I := Right (I);
      Check ("13", I, 41);
      I := Left (I);
      Check ("14", I, 30);
      I := Left (I);
      Check ("15", I, 28);
      Check_Null ("15 left", Left (I));
      Check_Null ("15 right", Right (I));
      I := Parent (I);
      I := Right (I);
      Check ("16", I, 38);
      I := Left (I);
      Check ("17", I, 35);
      Check_Null ("17 left", Left (I));
      Check_Null ("17 right", Right (I));
      I := Parent (I);
      I := Right (I);
      Check ("18", I, 39);
      Check_Null ("18 left", Left (I));
      Check_Null ("18 right", Right (I));
      I := Parent (I);
      I := Parent (I);
      I := Right (I);
      Check ("19", I, 47);
      Check_Null ("19 left", Left (I));
      Check_Null ("19 right", Right (I));

      --  FIXME: more
   end Nominal;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_red_black_trees.adb");
   end Name;

end Test_Red_Black_Trees;
