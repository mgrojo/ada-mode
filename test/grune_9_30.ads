--  Abstract :
--
--  Partial test of WisiToken.Parser.LR1_Items.Closure and
--  LR1_Generator.LR1_Goto_Transistions handling of empty productions,
--  using the grammar in fig 9.30 of [Grune]. See
--  empty_productions_1.ads for list of similar tests.
--
--  References:
--
--  [Grune] Parsing Techniques, A Practical Guide. Dick Grune, Ceriel J.H. Jacobs, second edition.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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
package Grune_9_30 is

   type Test_Case (Debug : Boolean) is new AUnit.Test_Cases.Test_Case with null record;

   type Test_Case_Access is access all Test_Case;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String;

end Grune_9_30;
