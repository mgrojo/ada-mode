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

end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
