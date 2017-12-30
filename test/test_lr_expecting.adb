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
with WisiToken.AUnit;
with Ada.Characters.Latin_1;
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.Parser;
with WisiToken.Production;
with WisiToken.Semantic_State;
with WisiToken.Text_IO_Trace;
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

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Equals_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Statement_ID,
      Last_Nonterminal  => Parse_Sequence_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Parse_Sequence_ID);
   use Token_Enum;

   First_State_Index  : constant := 1;
   First_Parser_Label : constant := 1;

   use type WisiToken.Production.List.Instance;   --  "and"
   use type WisiToken.Production.Right_Hand_Side; --  "+"

   package Set_Statement is

      Grammar : constant WisiToken.Production.List.Instance :=
        --  set symbol = value
        WisiToken.Production.List.Only
        (Statement_ID <= Set_ID & Identifier_ID & Equals_ID & Int_ID + WisiToken.Semantic_State.Null_Action);

   end Set_Statement;

   package Verify_Statement is

      Grammar : constant WisiToken.Production.List.Instance :=
        --  verify symbol = value +- tolerance
        WisiToken.Production.List.Only
          (Statement_ID  <= Verify_ID & Equals_ID & Int_ID & Plus_Minus_ID & Int_ID +
             WisiToken.Semantic_State.Null_Action);
   end Verify_Statement;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Whitespace_ID => Lexer.Get (" ", Report => False),
       Equals_ID     => Lexer.Get ("="),
       Int_ID        => Lexer.Get ("[0-9]+"),
       Plus_Minus_ID => Lexer.Get ("\+-"),
       Semicolon_ID  => Lexer.Get (";"),
       Set_ID        => Lexer.Get ("set"),
       Verify_ID     => Lexer.Get ("verify"),
       Identifier_ID => Lexer.Get ("[0-9a-zA-Z_]+"),
       EOF_ID        => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   Grammar : constant WisiToken.Production.List.Instance :=
     Parse_Sequence_ID <= Statement_ID & Semicolon_ID & EOF_ID + WisiToken.Semantic_State.Null_Action and
     Set_Statement.Grammar and
     Verify_Statement.Grammar;

   Parser : WisiToken.LR.Instance;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);
   State : aliased WisiToken.Semantic_State.Semantic_State (Trace'Access);

   procedure Execute
     (Command  : in String;
      Expected : in WisiToken.Token_ID_Set)
   is begin
      Parser.Lexer.Reset_With_String (Command);
      State.Reset;

      Parser.Parse;
      AUnit.Assertions.Assert (False, Command & "; no exception");
   exception
   when WisiToken.Syntax_Error =>
      declare
         use WisiToken.Semantic_State;
         List  : Error_Data_Lists.List renames State.Active_Error_List;
         Error : Error_Data renames List.Constant_Reference (List.First);
      begin
         WisiToken.AUnit.Check (Command, Error.Expecting, Expected);
      end;
   end Execute;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use WisiToken.AUnit;
   begin
      WisiToken.LR.Parser.New_Parser
        (Parser,
         Lexer.New_Lexer (Trace'Access, Syntax),
         WisiToken.LR.LALR_Generator.Generate
           (Grammar,
            LALR_Descriptor,
            First_State_Index,
            Trace           => Test.Debug > 0,
            Put_Parse_Table => Test.Debug > 0),
         State'Access,
         First_Parser_Label);

      WisiToken.Trace_Parse := Test.Debug;

      Execute
        ("set A = 2",
         To_Token_ID_Set (LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal, (1 => +Semicolon_ID)));

      Execute
        ("set A = ",
         To_Token_ID_Set (LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal, (1 => +Int_ID)));

      Execute
        ("set A",
         To_Token_ID_Set (LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal, (1 => +Equals_ID)));

      Execute
        ("set",
         To_Token_ID_Set (LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal, (1 => +Identifier_ID)));

      Execute
        ("2",
         To_Token_ID_Set (LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal, (+Set_ID, +Verify_ID)));
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_lr_expecting.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR_Expecting;
