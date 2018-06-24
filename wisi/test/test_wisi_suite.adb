--  Abstract :
--
--  Build AUnit test suite containing all Wisi tests
--
--  Copyright (C) 2013-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Strings.Unbounded;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Test_Generate_Errors;
with Wisi_WY_Test;
function Test_Wisi_Suite return Access_Test_Suite
is
   function "+" (Item : in String) return Ada.Strings.Unbounded.String_Access
   is begin
      return Ada.Strings.Unbounded.String_Access'(new String'(Item));
   end "+";

   Suite : constant Access_Test_Suite := new Test_Suite;
begin
   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new Test_Generate_Errors.Test_Case (+"../wisi/test/unused_tokens", LR1 => True));

   --  elisp grammar generate tests; grammar file name order

   --  In body_instantiation_conflict, LALR and LR1 generate different
   --  names for the conflict. In the other files, the conflict names are
   --  the same.
   Add_Test (Suite, new Wisi_WY_Test.Test_Case (+"../wisi/test/body_instantiation_conflict", False));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case (+"../wisi/test/case_expression", True));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case (+"../wisi/test/character_literal", True));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case (+"../wisi/test/identifier_list_name_conflict", True));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case (+"../wisi/test/range_conflict", True));

   --  other *.wy files in ../wisi/test are used in Ada parser
   --  generator/parse tests, not run from here.

   --  end test cases

   return Suite;

end Test_Wisi_Suite;
