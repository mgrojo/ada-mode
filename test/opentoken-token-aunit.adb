--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009, 2015 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Assertions;
package body OpenToken.Token.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in OpenToken.Token.List.Instance;
      Expected : in Token_Array)
   is
      use Standard.AUnit.Assertions;
      use OpenToken.Token.List;
      I : List_Iterator := First (Computed);
   begin
      for J in Expected'Range loop
         Assert (I /= Null_Iterator, Label & ".actual length =" & Integer'Image (J - Expected'First));
         Check (Label & Integer'Image (J), ID (Token_Handle (I).all), Expected (J));
         Next_Token (I);
      end loop;
      Assert (I = Null_Iterator, Label & ".list longer than expected");
   end Check;

end OpenToken.Token.AUnit;
