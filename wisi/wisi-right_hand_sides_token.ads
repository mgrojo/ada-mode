--  Abstract :
--
--  OpenToken token for Wisent Right_Hand_Sides declarations.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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

package Wisi.Right_Hand_Sides_Token is

   type Instance is new Nonterminal.Instance with private;

   Right_Hand_Sides : constant Instance;
   Grammar          : constant Production_List.Instance;

private

   type Instance is new Nonterminal.Instance with record
      List : Token_List.Instance;
   end record;

   procedure Add
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token_IDs);

   Right_Hand_Sides : constant Instance :=
     (Nonterminal.Instance (Nonterminal.Get (Right_Hand_Sides_ID)) with Token_List.Null_List);

   Grammar : constant Production_List.Instance :=
     Right_Hand_Sides <= Symbol + Add'Access and
     Right_Hand_Sides <= Right_Hand_Sides & Symbol + Add'Access;

end Wisi.Right_Hand_Sides_Token;
--  Local Variables:
--  ada-indent-opentoken: t
--  End:
