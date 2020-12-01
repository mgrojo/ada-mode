--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017 - 2020 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Lexer.Regexp;
with WisiToken.Parse.LR.Parser;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada;
package body Test_LR_Expecting_Terminal_Sequence is

   package Simple is
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
         Parse_Sequence_ID,
         Statement_ID);

      package Token_Enum is new WisiToken.Gen_Token_Enum
        (Token_Enum_ID     => Token_ID,
         First_Terminal    => Equals_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => Parse_Sequence_ID,
         Last_Nonterminal  => Statement_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => Parse_Sequence_ID,
         Case_Insensitive  => False);
      use Token_Enum;

      package Set_Statement is
         use WisiToken.Wisi_Ada;

         Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
           --  set symbol = value
           +(Statement_ID <= Set_ID & Identifier_ID & Equals_ID & Int_ID + WisiToken.Syntax_Trees.Null_Action);

      end Set_Statement;

      package Verify_Statement is
         use WisiToken.Wisi_Ada;

         Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
           --  verify symbol = value +- tolerance
           +(Statement_ID  <= Verify_ID & Identifier_ID & Equals_ID & Int_ID & Plus_Minus_ID & Int_ID +
               WisiToken.Syntax_Trees.Null_Action);
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

      package Top_Level is
         use WisiToken.Wisi_Ada;

         Grammar : WisiToken.Productions.Prod_Arrays.Vector :=
           +(Parse_Sequence_ID <= Statement_ID & Semicolon_ID & EOF_ID + WisiToken.Syntax_Trees.Null_Action) and
           Set_Statement.Grammar and
           Verify_Statement.Grammar;
      end Top_Level;

      Parser : WisiToken.Parse.LR.Parser.Parser;

      Trace : aliased WisiToken.Text_IO_Trace.Trace;
      Log_File : Ada.Text_IO.File_Type;

      procedure Execute
        (Command  : in String;
         Expected : in WisiToken.Token_ID_Set);
   end Simple;

   package body Simple is
      procedure Execute
        (Command  : in String;
         Expected : in WisiToken.Token_ID_Set)
      is begin
         Parser.Tree.Lexer.Reset_With_String (Command);
         Parser.Parse (Log_File);
         AUnit.Assertions.Assert (False, Command & "; no exception");
      exception
      when WisiToken.Syntax_Error =>
         declare
            use WisiToken.Parse.LR;
            List  : Parse_Error_Lists.List renames Parser.Parsers.First.State_Ref.Errors;
            Error : Parse_Error renames List.Constant_Reference (List.First);
         begin
            WisiToken.AUnit.Check (Command, Error.Expecting, Expected);
         end;
      end Execute;
   end Simple;

   ----------
   --  Test procedures

   procedure Test_Expecting (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Simple;
      use Simple.Token_Enum;

      First : WisiToken.Token_ID renames LR1_Descriptor.First_Terminal;
      Last  : WisiToken.Token_ID renames LR1_Descriptor.Last_Terminal;

      Recursions : WisiToken.Generate.Recursions := WisiToken.Generate.Empty_Recursions;
   begin
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (LALR_Descriptor'Access, Syntax),
         WisiToken.Generate.LR.LALR_Generate.Generate
           (Top_Level.Grammar, LALR_Descriptor, Grammar_File_Name => "", Recursions => Recursions),
         User_Data                      => null,
         Language_Fixes                 => null,
         Language_Matching_Begin_Tokens => null,
         Language_String_ID_Set         => null);

      Execute
        ("set A = 2",
         WisiToken.To_Token_ID_Set (First, Last, (1 => +Semicolon_ID)));

      Execute
        ("set A = ",
         WisiToken.To_Token_ID_Set (First, Last, (1 => +Int_ID)));

      Execute
        ("set A",
         WisiToken.To_Token_ID_Set (First, Last, (1 => +Equals_ID)));

      Execute
        ("set",
         WisiToken.To_Token_ID_Set
           (First, Last, (1 => +Identifier_ID)));

      Execute
        ("2",
         WisiToken.To_Token_ID_Set
           (First, Last, (+Set_ID, +Verify_ID)));
   end Test_Expecting;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Expecting'Access, "Test_Expecting");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_lr_expecting_terminal_sequence.adb");
   end Name;

end Test_LR_Expecting_Terminal_Sequence;
