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
with AUnit.Checks;
with Ada.Exceptions;
with Ada.Text_IO;
with FastToken.Lexer.Regexp;
with FastToken.Production;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Text_Feeder.String;
with FastToken.Token;
with FastToken.Token_Plain;
package body Test_LR_Expecting is

   --  A simple grammar for testing the Expecting function for generating nice error messages.
   --
   --  legal statments:
   --
   --  set foo = integer;
   --  verify foo = integer +- integer;
   --
   --  Nice errors:
   --
   --  foo; => expecting 'set, verify'
   --  set foo bar; => "expecting '='
   --  etc

   type Token_ID is
     (Whitespace_ID,
      Equals_ID,
      Int_ID,
      Plus_Minus_ID,
      Semicolon_ID,
      Set_ID,
      Verify_ID,

      --  Identifier must be after keywords, so they are recognized instead
      Identifier_ID,

      EOF_ID,

      --  non-terminals
      Statement_ID,
      Parse_Sequence_ID);

   package Token_Pkg is new FastToken.Token (Token_ID, Equals_ID, EOF_ID, Token_ID'Image);
   package Lexer_Root is new FastToken.Lexer (Token_ID);
   package Token_Aug is new FastToken.Token_Plain (Token_Pkg, Lexer_Root);
   package Production is new FastToken.Production (Token_Pkg, Token_Aug.Semantic_Action, Token_Aug.Null_Action);
   package Lexer is new Lexer_Root.Regexp (EOF_ID);
   package Parser_Root is new FastToken.Parser
     (Token_ID, Equals_ID, EOF_ID, EOF_ID, Parse_Sequence_ID, Token_ID'Image, Ada.Text_IO.Put, Token_Pkg, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR
     (First_State_Index, Token_ID'Width, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Token_Aug.State_Type,
      Token_Aug.Input_Token);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

   use type Production.Instance;        --  "<="
   use type Production.List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_Pkg.List.Instance; --  "&"

   package Set_Statement is

      Grammar : constant Production.List.Instance :=
        --  set symbol = value
        Production.List.Only
        (Statement_ID <= Set_ID & Identifier_ID & Equals_ID & Int_ID + Token_Aug.Null_Action);

   end Set_Statement;

   package Verify_Statement is

      Grammar : constant Production.List.Instance :=
        --  verify symbol = value +- tolerance
        Production.List.Only
        (Statement_ID  <= Verify_ID & Equals_ID & Int_ID & Plus_Minus_ID & Int_ID + Token_Aug.Null_Action);
   end Verify_Statement;

   Syntax : constant Lexer.Syntax :=
     (
      Whitespace_ID => Lexer.Get (" ", Report => False),
      Equals_ID     => Lexer.Get ("="),
      Int_ID        => Lexer.Get ("[0-9]+"),
      Plus_Minus_ID => Lexer.Get ("\+-"),
      Semicolon_ID  => Lexer.Get (";"),
      Set_ID        => Lexer.Get ("set"),
      Verify_ID     => Lexer.Get ("verify"),
      Identifier_ID => Lexer.Get ("[0-9a-zA-Z_]+"),
      EOF_ID        => Lexer.Get ("" & FastToken.EOF_Character)
     );

   Grammar : constant Production.List.Instance :=
     Parse_Sequence_ID <= Statement_ID & Semicolon_ID & EOF_ID + Token_Aug.Null_Action and
     Set_Statement.Grammar and
     Verify_Statement.Grammar;

   First_Parser_Label : constant := 1;
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package Panic_Mode is new LR.Panic_Mode (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LR_Parser is new LR.Parser
     (First_Parser_Label, Ada.Text_IO.Put, Ada.Text_IO.Put_Line, Parser_Lists, Panic_Mode,
      Token_Aug.Reset, Token_Aug.Push_Token, Token_Aug.Merge_Tokens, Token_Aug.Recover);

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;
   Parser        : LR_Parser.Instance;

   procedure Execute
     (Command          : in String;
      Expected_Message : in String)
   is begin
      FastToken.Text_Feeder.String.Set (String_Feeder, Command);

      Parser.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

      Parser.Parse;
      AUnit.Assertions.Assert (False, Command & "; no exception");
   exception
   when E : FastToken.Syntax_Error =>
      AUnit.Checks.Check (Command, Ada.Exceptions.Exception_Message (E), Expected_Message);
   end Execute;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
   begin
      Parser := LR_Parser.New_Parser
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         Generators.Generate
           (Grammar,
            Trace           => Test.Debug,
            Put_Parse_Table => Test.Debug),
         Token_Aug.State);

      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute ("set A = 2", "1:10: Syntax error; expecting one of SEMICOLON_ID; found EOF_ID '" & ASCII.EOT & "'");

      Execute ("set A = ", "1:9: Syntax error; expecting one of INT_ID; found EOF_ID '" & ASCII.EOT & "'");

      Execute ("set A", "1:6: Syntax error; expecting one of EQUALS_ID; found EOF_ID '" & ASCII.EOT & "'");

      Execute ("set", "1:4: Syntax error; expecting one of IDENTIFIER_ID; found EOF_ID '" & ASCII.EOT & "'");

      Execute ("2", "1:1: Syntax error; expecting one of SET_ID, VERIFY_ID; found INT_ID '2'");
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_lr_expecting.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR_Expecting;
