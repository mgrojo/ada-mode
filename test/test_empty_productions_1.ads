--  Abstract :
--
--  Partial test of FastToken.Parser.LRK_Item and LALR handling of
--  empty productions. Similarly named files test similar features:
--
--  1 - empty production in a list
--  4 - empty production in the first two nonterms in a production
--  5 - empty production following a nonterm
--  6 - empty production in a list; same conflict in two states
--  7 - two consecutive empty productions not first
--  8 - two consecutive empty productions, confused with a similar production
--
--  Copyright (C) 2013, 2015, 2017 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Test_Cases;
package Test_Empty_Productions_1 is

   type Test_Case (Debug : Boolean) is new AUnit.Test_Cases.Test_Case with null record;

   type Test_Case_Access is access all Test_Case;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String;

end Test_Empty_Productions_1;
