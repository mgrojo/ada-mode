--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015 Stephen Leake.  All Rights Reserved.
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
with FastToken.Lexer.Regexp;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token.Nonterminal;
package body Test_Statement_Actions is

   type Token_ID is
     (Whitespace_ID,
      Plus_Minus_ID,
      Semicolon_ID,
      Set_ID,
      Verify_ID,
      Int_ID,
      EOF_ID,

      --  non-terminals
      Statement_ID,
      Statement_Semi_ID,
      Statement_Sequence_ID,
      Parse_Sequence_ID);

   package Token_Pkg is new FastToken.Token (Token_ID, Plus_Minus_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Lexer is new Lexer_Root.Regexp;
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, Parse_Sequence_ID, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package LR_Parser is new LR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

   use type Production.Instance;        --  "<="
   use type Production.List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_Pkg.List.Instance;    --  "&"

   package Tokens is
      --  For use in right hand sides.
      --  Terminals
      EOF            : constant Token_Pkg.Class := Token_Pkg.Get (EOF_ID);
      Integer        : constant Token_Pkg.Class := Token_Pkg.Get (Int_ID);
      Plus_Minus     : constant Token_Pkg.Class := Token_Pkg.Get (Plus_Minus_ID);
      Semicolon      : constant Token_Pkg.Class := Token_Pkg.Get (Semicolon_ID);

      --  Nonterminals
      Parse_Sequence     : constant Nonterminal.Class := Nonterminal.Get (Parse_Sequence_ID);
      Statement          : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
      Statement_Semi     : constant Nonterminal.Class := Nonterminal.Get (Statement_Semi_ID);
      Statement_Sequence : constant Nonterminal.Class := Nonterminal.Get (Statement_Sequence_ID);
   end Tokens;

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   package Set_Statement is

      Set_Statement : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);

      Grammar : constant Production.List.Instance :=
        Production.List.Only
        (Set_Statement <= Nonterminal.Get (Set_ID) & Tokens.Integer + Self);

   end Set_Statement;

   package Verify_Statement is

      Verify_Statement : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);

      Grammar : constant Production.List.Instance :=
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Tokens.Integer + Self
        and
        Verify_Statement  <= Nonterminal.Get (Verify_ID) & Tokens.Integer &
        Tokens.Plus_Minus + Self;
   end Verify_Statement;

   Syntax : constant Lexer.Syntax :=
     (
      Whitespace_ID => Lexer.Get (" ", Token_Pkg.Get (Whitespace_ID), Report => False),
      Plus_Minus_ID => Lexer.Get ("\+-", Tokens.Plus_Minus),
      Semicolon_ID  => Lexer.Get (";", Tokens.Semicolon),
      Set_ID        => Lexer.Get ("set", Token_Pkg.Get (Set_ID)),
      Verify_ID     => Lexer.Get ("verify", Token_Pkg.Get (Verify_ID)),
      Int_ID        => Lexer.Get ("[0-9]+", Tokens.Integer),
      EOF_ID        => Lexer.Get ("" & FastToken.EOF_Character, Tokens.EOF)
     );

   Action_Count : Integer := 0;

   procedure Statement_Semi_Action
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_Pkg.List.Instance'Class;
      To_ID     : in     Token_ID)
   is
      pragma Unreferenced (To_ID);
      pragma Unreferenced (Source);
   begin
      New_Token := Nonterminal.Get (Statement_Semi_ID);
      Action_Count := Action_Count + 1;
   end Statement_Semi_Action;

   Grammar : constant Production.List.Instance :=
     Tokens.Parse_Sequence     <= Tokens.Statement_Sequence & Tokens.EOF + Self and
     Tokens.Statement_Sequence <= Tokens.Statement_Semi & Tokens.Statement_Sequence + Self and
     Tokens.Statement_Sequence <= Tokens.Statement_Semi + Self and
     Tokens.Statement_Semi     <= Tokens.Statement & Tokens.Semicolon + Statement_Semi_Action'Access and

     Set_Statement.Grammar and
     Verify_Statement.Grammar;

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;
   Parser        : LR_Parser.Instance;

   procedure Execute_Command (Command : in String)
   is begin
      FastToken.Text_Feeder.String.Set (String_Feeder, Command);

      Parser.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

      Parser.Parse;
   exception
   when E : others =>
      AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
   end Execute_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use AUnit.Assertions;
   begin
      Parser := LR_Parser.Initialize
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         Generators.Generate (Grammar, Trace => Test.Debug));

      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute_Command ("set 2;");

      Assert (Action_Count = 1, "1 statement");

      Execute_Command ("set 2; verify 3;");

      Assert (Action_Count = 3, "2 more statements");

   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_statement_actions.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Statement_Actions;
