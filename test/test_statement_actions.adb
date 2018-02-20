--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with WisiToken.Gen_Token_Enum;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.Parser;
with WisiToken.Lexer.Regexp;
with WisiToken.Production;
with WisiToken.Semantic_State;
with WisiToken.Text_IO_Trace;
with WisiToken.Token_ID_Lists;
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

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Plus_Minus_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Statement_ID,
      Last_Nonterminal  => Parse_Sequence_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Parse_Sequence_ID);
   use Token_Enum;

   First_State_Index  : constant := 1;
   First_Parser_Label : constant := 1;

   use all type WisiToken.Production.List.Instance;   --  "and"
   use all type WisiToken.Production.Right_Hand_Side; --  "+"

   Null_Action : WisiToken.Semantic_State.Semantic_Action renames WisiToken.Semantic_State.Null_Action;

   package Set_Statement is

      Grammar : constant WisiToken.Production.List.Instance :=
        WisiToken.Production.List.Only
        (Statement_ID <= Set_ID & Int_ID + Null_Action);

   end Set_Statement;

   package Verify_Statement is

      Grammar : constant WisiToken.Production.List.Instance :=
        Statement_ID <= Verify_ID & Int_ID + Null_Action
        and
        Statement_ID <= Verify_ID & Int_ID & Plus_Minus_ID + Null_Action;
   end Verify_Statement;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Whitespace_ID => Lexer.Get (" ", Report => False),
       Plus_Minus_ID => Lexer.Get ("\+-"),
       Semicolon_ID  => Lexer.Get (";"),
       Set_ID        => Lexer.Get ("set"),
       Verify_ID     => Lexer.Get ("verify"),
       Int_ID        => Lexer.Get ("[0-9]+"),
       EOF_ID        => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   Action_Count : Integer := 0;

   procedure Statement_Semi_Action
     (Nonterm : in WisiToken.Semantic_State.Augmented_Token;
      Source  : in WisiToken.Semantic_State.Augmented_Token_Arrays.Vector)
   is
      pragma Unreferenced (Nonterm);
      pragma Unreferenced (Source);
   begin
      Action_Count := Action_Count + 1;
   end Statement_Semi_Action;

   Grammar : constant WisiToken.Production.List.Instance :=
     Parse_Sequence_ID     <= Statement_Sequence_ID & EOF_ID + Null_Action and
     Statement_Sequence_ID <= Statement_Semi_ID & Statement_Sequence_ID + Null_Action and
     Statement_Sequence_ID <= Statement_Semi_ID + Null_Action and
     Statement_Semi_ID     <= WisiToken.Token_ID_Lists.List'
       (Statement_ID & Semicolon_ID) + Statement_Semi_Action'Access and

     Set_Statement.Grammar and
     Verify_Statement.Grammar;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);
   State : aliased WisiToken.Semantic_State.Semantic_State (Trace'Access);

   Parser : WisiToken.LR.Instance;

   procedure Execute_Command (Command : in String)
   is begin
      Parser.Lexer.Reset_With_String (Command);
      State.Reset;

      WisiToken.LR.Parser.Parse (Parser);
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
      WisiToken.LR.Parser.New_Parser
        (Parser,
         Lexer.New_Lexer (Trace'Access, State.Lexer_Errors'Access, Syntax),
         WisiToken.LR.LALR_Generator.Generate
           (Grammar, LALR_Descriptor, First_State_Index, Trace => Test.Debug > 0),
         State'Access,
         First_Parser_Label);

      WisiToken.Trace_Parse := Test.Debug;

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
      return new String'("test_statement_actions.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Statement_Actions;
