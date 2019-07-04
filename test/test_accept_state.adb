--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 - 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Parse.LR.Parser;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
package body Test_Accept_State is

   --  A simple grammar that WisiToken used to get wrong.
   --
   --  set foo = integer;

   type Token_ID is
     (Whitespace_ID,
      Equals_ID,
      Int_ID,
      Set_ID,
      EOF_ID, -- _not_ last_terminal; should be ok
      Identifier_ID,

      --  non-terminals
      Parse_Sequence_ID,
      Statement_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Equals_ID,
      Last_Terminal     => Identifier_ID,
      First_Nonterminal => Parse_Sequence_ID,
      Last_Nonterminal  => Statement_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Parse_Sequence_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Whitespace_ID => Lexer.Get (" ", Report => False),
       Equals_ID     => Lexer.Get ("="),
       Int_ID        => Lexer.Get ("[0-9]+"),
       Set_ID        => Lexer.Get ("set"),
       Identifier_ID => Lexer.Get ("[0-9a-zA-Z_]+"),
       EOF_ID        => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
     --  First production in Grammar must be the terminating
     --  production; it gets the accept action.
     Parse_Sequence_ID <= Statement_ID & EOF_ID + Null_Action and
     Statement_ID <= Set_ID & Identifier_ID & Equals_ID & Int_ID + Null_Action;

   Parser : WisiToken.Parse.LR.Parser.Parser;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  The test is that there are no exceptions.

      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (Trace.Descriptor, Syntax),
         WisiToken.Generate.LR.LALR_Generate.Generate (Grammar, LALR_Descriptor),
         User_Data                      => null,
         Language_Fixes                 => null,
         Language_Matching_Begin_Tokens => null,
         Language_String_ID_Set         => null);

      Parser.Lexer.Reset_With_String ("set A = 2");

      Parser.Parse;

   exception
   when E : others =>
      declare
         use Ada.Exceptions;
      begin
         AUnit.Assertions.Assert (False, Exception_Name (E) & ": " & Exception_Message (E));
      end;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_accept_state.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Accept_State;
