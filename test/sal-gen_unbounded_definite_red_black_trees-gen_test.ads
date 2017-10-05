--  Abstract :
--
--  Access to tree internals for testing parent.
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

generic
package SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test is

   function Root (Tree : in SAL.Gen_Unbounded_Definite_Red_Black_Trees.Tree) return Cursor;

   function Parent (Cursor : in Pkg.Cursor) return Pkg.Cursor;
   function Left (Cursor : in Pkg.Cursor) return Pkg.Cursor;

   function Right (Cursor : in Pkg.Cursor) return Pkg.Cursor;

   procedure Check_Null
     (Label  : in String;
      Cursor : in Pkg.Cursor);

end SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
