--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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
with AUnit.Assertions;
package body SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test is

   function Root (Tree : in SAL.Gen_Unbounded_Definite_Red_Black_Trees.Tree) return Cursor
   is begin
      return (Node => Tree.Root);
   end Root;

   function Parent (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return (Node => Cursor.Node.Parent);
   end Parent;

   function Left (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return (Node => Cursor.Node.Left);
   end Left;

   function Right (Cursor : in Pkg.Cursor) return Pkg.Cursor
   is begin
      return (Node => Cursor.Node.Right);
   end Right;

   procedure Check_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor)
   is begin
      AUnit.Assertions.Assert (Cursor.Node = null, Label & " expected null, got non-null");
   end Check_Null;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Color);

   procedure Check_Color
     (Label      : in String;
      Cursor     : in Pkg.Cursor;
      Expect_Red : in Boolean)
   is begin
      Check (Label, Cursor.Node.Color, (if Expect_Red then Red else Black));
   end Check_Color;

   procedure Validate_Children_Of_Red (Label : in String; Node : in Node_Access)
   is
      use AUnit.Assertions;
   begin
      if Node.Color = Red then
         Assert (Node.Left = null or else Node.Left.Color = Black, Label & ".left child of red not black");
         Assert (Node.Right = null or else Node.Right.Color = Black, Label & ".right child of red not black");
      end if;
      if Node.Left /= null then
         Validate_Children_Of_Red (Label, Node.Left);
      end if;
      if Node.Right /= null then
         Validate_Children_Of_Red (Label, Node.Right);
      end if;
   end Validate_Children_Of_Red;

   function Black_Height (Node : in Node_Access) return Integer
   is begin
      if Node.Left = null then
         return 0;
      else
         return Black_Height (Node.Left) + (if Node.Left.Color = Black then 1 else 0);
      end if;
   end Black_Height;

   function Black_Height (Cursor : in Pkg.Cursor) return Integer
   is begin
      if Cursor.Node = null then
         return 0;
      else
         return Black_Height (Cursor.Node);
      end if;
   end Black_Height;

   procedure Validate_Black_Height (Label : in String; Node : in Node_Access)
   is
      use AUnit.Assertions;
   begin
      if Node.Left = null then
         if Node.Right = null then
            null;
         else
            declare
               Right : constant Integer := Black_Height (Node.Right);
            begin
               Assert
                 (Right = 1,
                  Label & "." & Image (Key (Node.Element)) & ".siblings have different black height 1," &
                    Integer'Image (Right));
            end;
         end if;
      elsif Node.Right = null then
         declare
            Left  : constant Integer := Black_Height (Node.Left);
         begin
            Assert
              (Left = 1,
               Label & "." & Image (Key (Node.Element)) & ".siblings have different black height" &
                 Integer'Image (Left) & ", 1");
         end;
      else
         declare
            Left  : constant Integer := Black_Height (Node.Left) + (if Node.Left.Color = Black then 1 else 0);
            Right : constant Integer := Black_Height (Node.Right) + (if Node.Right.Color = Black then 1 else 0);
         begin
            Assert
              (Left = Right,
               Label & "." & Image (Key (Node.Element)) & ".siblings have different black height" &
                 Integer'Image (Left) & "," & Integer'Image (Right));
         end;
      end if;
   end Validate_Black_Height;

   procedure Validate (Label : in String; Tree : in Pkg.Tree)
   is begin
      if Tree.Root = null then
         return;
      end if;

      --  [1] 13.1 properties
      --  1. Every node is red or black - trivially true.
      --  2. The root is black
      Check (Label & ".root = black", Tree.Root.Color, Black);

      --  3. Every leaf is black; trivially true (all leaves are null = black).
      --  4. If a node is red, both its children are black
      Validate_Children_Of_Red (Label, Tree.Root);

      --  5. For each node, all simple paths from the node to descendant
      --  leaves contain the same number of black nodes.
      Validate_Black_Height (Label, Tree.Root);
   end Validate;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
