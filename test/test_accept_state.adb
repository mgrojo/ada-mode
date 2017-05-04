--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with FastToken.Production;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Text_Feeder.String;
with FastToken.Lexer.Regexp;
with FastToken.Token.Nonterminal;
package body Test_Accept_State is

   --  A simple grammar that OpenToken used to get wrong.
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

   Token_Image_Width : constant Integer := Token_ID'Width;
   package Token_Pkg is new FastToken.Token (Token_ID, Equals_ID, Identifier_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Lexer is new Lexer_Root.Regexp;
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, Parse_Sequence_ID, Lexer_Root);
   First_Parser_Label : constant := 1;
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR (First_State_Index, Token_Image_Width, Nonterminal);
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package Parsers is new LR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

   Equals : constant Token_Pkg.Class := Token_Pkg.Get (Equals_ID);
   Int    : constant Token_Pkg.Class := Token_Pkg.Get (Int_ID);
   EOF    : constant Token_Pkg.Class := Token_Pkg.Get (EOF_ID);

   Identifier     : constant Token_Pkg.Class   := Token_Pkg.Get (Identifier_ID);
   Parse_Sequence : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
   Statement      : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);

   Syntax : constant Lexer.Syntax :=
     (
      Whitespace_ID => Lexer.Get (" ", Token_Pkg.Get (Whitespace_ID), Report => False),
      Equals_ID     => Lexer.Get ("=", Token_Pkg.Get (Equals_ID)),
      Int_ID        => Lexer.Get ("[0-9]+", Int),
      Set_ID        => Lexer.Get ("set", Token_Pkg.Get (Set_ID)),
      Identifier_ID => Lexer.Get ("[0-9a-zA-Z_]+", Token_Pkg.Get (Identifier_ID)),
      EOF_ID        => Lexer.Get ("" & FastToken.EOF_Character, Token_Pkg.Get (EOF_ID))
     );

   use type Production.Instance;        --  "<="
   use type Production.List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_Pkg.List.Instance; --  "&"

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     --  First production in Grammar must be the terminating
     --  production; it gets the accept action.
     Parse_Sequence <= Statement & EOF + Self and
     Statement <= Token_Pkg.Get (Set_ID) & Identifier & Equals & Int + Self;

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;
   Parser        : Parsers.Instance;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      --  The test is that there are no exceptions.

      Parser := Parsers.Initialize
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         Generators.Generate
           (Grammar,
            Trace           => Test.Debug,
            Put_Parse_Table => Test.Debug));

      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      String_Feeder.Set ("set A = 2");

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
      return new String'("../../Test/test_accept_state.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Accept_State;
