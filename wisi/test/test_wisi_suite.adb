--  Abstract :
--
--  Build AUnit test suite containing tests that run wisi-generate
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
with Wisi;
function Test_Wisi_Suite return Access_Test_Suite
is
   use all type Wisi.Generate_Algorithm;
   use all type Wisi.Interface_Type;
   use all type Wisi.Lexer_Type;
   use all type Wisi.Output_Language;

   function "+" (Item : in String) return Ada.Strings.Unbounded.String_Access
   is begin
      return Ada.Strings.Unbounded.String_Access'(new String'(Item));
   end "+";
   function "+" (Item : in Wisi.Generate_Algorithm_Set) return Wisi.Generate_Algorithm_Set_Access
   is begin
      return new Wisi.Generate_Algorithm_Set'(Item);
   end "+";
   function "+" (Item : in Wisi.Generate_Set) return Wisi.Generate_Set_Access
   is begin
      return new Wisi.Generate_Set'(Item);
   end "+";

   Suite : constant Access_Test_Suite := new Test_Suite;
begin
   --  test error handling when generate fails
   Add_Test (Suite, new Test_Generate_Errors.Test_Case
               (+"../wisi/test/unused_tokens", +(LR1 | LALR => True, others => False)));
   Add_Test (Suite, new Test_Generate_Errors.Test_Case
               (+"../wisi/test/triple_conflict", +(LALR => True, others => False)));

   --  generate succeeds; grammar file name order

   --  We don't do LR1 for all test files; not a helpful test. Just keep
   --  enough so we don't break LR1 accidently.

   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"ada_lite", False, +(1 => (LALR, Wisi.Ada, None, None))));
   --  FIXME: support indirect left recursion, add , Packrat_Gen, Packrat_Proc

   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (Root_Name        => +"body_instantiation_conflict",
                If_Lexer_Present => False,
                Generate_Set     => null)); -- specified in .wy file to test that.
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"case_expression", False,
                +((LR1, Wisi.Ada, None, None), (LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None),
                  (LR1, Elisp, None, None), (LALR, Elisp, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"character_literal", True,
                +((LR1, Wisi.Ada, re2c_Lexer, None), (Packrat_Gen, Wisi.Ada, re2c_Lexer, None),
                  (Packrat_Proc, Wisi.Ada, re2c_Lexer, None), (LR1, Elisp, Elisp_Lexer, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"conflict_name", False,
                +(1 => (LALR, Wisi.Ada, None, None))));
   --  FIXME: conflict_name has indirect left recursion (Packrat_Gen, Wisi.Ada), (Packrat_Proc, Wisi.Ada))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"dragon_4_43", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_1", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_2", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_3", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_4", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_5", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_6", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_7", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"empty_production_8", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"identifier_list_name_conflict", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None),
                  (LR1, Elisp, None, None), (LALR, Elisp, None, None))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"range_conflict", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None),
                  (LR1, Elisp, None, None), (LALR, Elisp, None, None))));

   --  skip_to_grammar has AUnit in user actions

   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"subprograms", False,
                +((LALR, Ada_Emacs, re2c_Lexer, Process), (Packrat_Gen, Ada_Emacs, re2c_Lexer, Process),
                  (Packrat_Proc, Ada_Emacs, re2c_Lexer, Process))));
   Add_Test (Suite, new Wisi_WY_Test.Test_Case
               (+"warth_left_recurse_expr_1", False,
                +((LALR, Wisi.Ada, None, None), (Packrat_Gen, Wisi.Ada, None, None),
                  (Packrat_Proc, Wisi.Ada, None, None))));

   --  other *.wy files in ../wisi/test are used in Ada parser
   --  generator/parse tests, not run from here.

   --  end test cases

   return Suite;

end Test_Wisi_Suite;
