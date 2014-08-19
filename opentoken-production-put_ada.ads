--  Abstract :
--
--  Utilities for generating Ada code to create Productions.
--
--  Copyright (C) 2014  All Rights Reserved.
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
   with function Token_Image (Item : in Token.Token_ID) return String;
   with function Action_Name (LHS : in Token.Token_ID; Index : in Integer) return String;
package OpenToken.Production.Put_Ada is

   procedure Put (Item : in Instance);
   --  Put code to generate Item to Current_Output.
   --
   --  opentoken-production-parser-lalr.adb reduce_stack only uses
   --  lhs, rhs.action, so this generates a null RHS.Tokens.
   --
   --  The action is output as Action_Name (Item.LHS.ID, Item.RHS.Index)
   --
   --  FIXME: indent?

end OpenToken.Production.Put_Ada;
