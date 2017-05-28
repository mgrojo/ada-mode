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
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token;
with FastToken.Token_Plain;
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
   package Lexer_Root is new FastToken.Lexer (Token_ID);
   package Token_Aug is new FastToken.Token_Plain (Token_Pkg, Lexer_Root);
   package Production is new FastToken.Production (Token_Pkg, Token_Aug.Semantic_Action, Token_Aug.Null_Action);
   package Lexer is new Lexer_Root.Regexp (EOF_ID);
   package Parser_Root is new FastToken.Parser
     (Token_ID, Plus_Minus_ID, EOF_ID, EOF_ID, Parse_Sequence_ID, Token_ID'Image, Ada.Text_IO.Put,
      Token_Pkg, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR
     (First_State_Index, Token_ID'Width, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Token_Aug.State_Type,
      Token_Aug.Input_Token);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package Panic_Mode is new LR.Panic_Mode (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LR_Parser is new LR.Parser
     (First_Parser_Label, Ada.Text_IO.Put, Ada.Text_IO.Put_Line, Parser_Lists, Panic_Mode,
      Token_Aug.Reset, Token_Aug.Push_Token, Token_Aug.Merge_Tokens, Token_Aug.Recover);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

   use all type Production.Instance;        --  "<="
   use all type Production.List.Instance;   --  "and"
   use all type Production.Right_Hand_Side; --  "+"
   use all type Token_Pkg.List.Instance;    --  "&"

   Null_Action : Token_Aug.Semantic_Action renames Token_Aug.Null_Action;

   package Set_Statement is

      Grammar : constant Production.List.Instance :=
        Production.List.Only
        (Statement_ID <= Set_ID & Int_ID + Null_Action);

   end Set_Statement;

   package Verify_Statement is

      Grammar : constant Production.List.Instance :=
        Statement_ID <= Verify_ID & Int_ID + Null_Action
        and
        Statement_ID <= Verify_ID & Int_ID & Plus_Minus_ID + Null_Action;
   end Verify_Statement;

   Syntax : constant Lexer.Syntax :=
     (
      Whitespace_ID => Lexer.Get (" ", Report => False),
      Plus_Minus_ID => Lexer.Get ("\+-"),
      Semicolon_ID  => Lexer.Get (";"),
      Set_ID        => Lexer.Get ("set"),
      Verify_ID     => Lexer.Get ("verify"),
      Int_ID        => Lexer.Get ("[0-9]+"),
      EOF_ID        => Lexer.Get ("" & FastToken.EOF_Character)
     );

   Action_Count : Integer := 0;

   procedure Statement_Semi_Action
     (Nonterm : in Token_Pkg.Nonterminal_ID;
      Index   : in Natural;
      Source  : in Token_Pkg.List.Instance)
   is
      pragma Unreferenced (Nonterm);
      pragma Unreferenced (Source);
      pragma Unreferenced (Index);
   begin
      Action_Count := Action_Count + 1;
   end Statement_Semi_Action;

   Grammar : constant Production.List.Instance :=
     Parse_Sequence_ID     <= Statement_Sequence_ID & EOF_ID + Null_Action and
     Statement_Sequence_ID <= Statement_Semi_ID & Statement_Sequence_ID + Null_Action and
     Statement_Sequence_ID <= Statement_Semi_ID + Null_Action and
     Statement_Semi_ID     <= Statement_ID & Semicolon_ID + Statement_Semi_Action'Access and

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

      use AUnit.Checks;
   begin
      Parser := LR_Parser.New_Parser
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         Generators.Generate (Grammar, Trace => Test.Debug),
        Token_Aug.State);

      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute_Command ("set 2;");

      Check ("1 statement", Action_Count, 1);

      Execute_Command ("set 2; verify 3;");

      Check ("2 more statements", Action_Count, 3);

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
