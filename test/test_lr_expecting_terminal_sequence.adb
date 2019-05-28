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
with Ada.Text_IO;
with Java_Expressions_Antlr_Actions;
with WisiToken.AUnit;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate.LR.AUnit;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Lexer.Regexp;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
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

         Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
           +(Parse_Sequence_ID <= Statement_ID & Semicolon_ID & EOF_ID + WisiToken.Syntax_Trees.Null_Action) and
           Set_Statement.Grammar and
           Verify_Statement.Grammar;
      end Top_Level;

      Parser : WisiToken.Parse.LR.Parser.Parser;

      Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

      procedure Execute
        (Command  : in String;
         Expected : in WisiToken.Token_ID_Set);
   end Simple;

   package body Simple is
      procedure Execute
        (Command  : in String;
         Expected : in WisiToken.Token_ID_Set)
      is begin
         Parser.Lexer.Reset_With_String (Command);
         Parser.Parse;
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
   begin
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (Trace.Descriptor, Syntax),
         WisiToken.Generate.LR.LALR_Generate.Generate (Top_Level.Grammar, LALR_Descriptor),
         User_Data                             => null,
         Language_Fixes                        => null,
         Language_Use_Minimal_Complete_Actions => null,
         Language_String_ID_Set                => null);

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

   procedure Test_Terminal_Sequence_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Simple;
      use Simple.Token_Enum;

      use WisiToken.Generate.LR.AUnit;
      use WisiToken.Generate.LR.RHS_Sequence_Arrays;
      use WisiToken.Token_ID_Arrays;

      Computed : constant WisiToken.Generate.LR.Minimal_Sequence_Array :=
        WisiToken.Generate.LR.Compute_Minimal_Terminal_Sequences (LALR_Descriptor, Top_Level.Grammar);

      Expected : constant WisiToken.Generate.LR.Minimal_Sequence_Array (+Parse_Sequence_ID .. +Statement_ID) :=
        (To_Vector ((False, +Set_ID & (+Identifier_ID) & (+Equals_ID) & (+Int_ID) & (+Semicolon_ID) & (+EOF_ID))),
         ((False, +Set_ID & (+Identifier_ID) & (+Equals_ID) & (+Int_ID)) &
          (False, +Verify_ID & (+Identifier_ID) & (+Equals_ID) & (+Int_ID) & (+Plus_Minus_ID) & (+Int_ID))));
   begin
      if WisiToken.Trace_Generate > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("Computed:");
         for S of Computed loop
            Ada.Text_IO.Put_Line (WisiToken.Generate.LR.Image (S, LALR_Descriptor));
         end loop;
         Ada.Text_IO.Put_Line ("Expected:");
         for S of Expected loop
            Ada.Text_IO.Put_Line (WisiToken.Generate.LR.Image (S, LALR_Descriptor));
         end loop;
      end if;
      Check ("1", Computed, Expected);
   end Test_Terminal_Sequence_1;

   procedure Test_Terminal_Sequence_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken;
      use WisiToken.Generate.LR.AUnit;

      Input_File_Name : constant String := "../Test/bnf/java_expressions_antlr.wy";
      Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
      Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
      Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;
   begin
      Wisitoken_Grammar_Main.Create_Parser (Grammar_Parser, Trace'Unchecked_Access, Input_Data'Unchecked_Access);
      Grammar_Parser.Lexer.Reset_With_File (Input_File_Name);
      Grammar_Parser.Parse;

      Input_Data.Phase       := WisiToken_Grammar_Runtime.Meta;
      Input_Data.User_Parser := WisiToken.BNF.LR1;
      Input_Data.User_Lexer  := WisiToken.BNF.re2c_Lexer;
      Grammar_Parser.Execute_Actions;
      declare
         Tree  : WisiToken.Syntax_Trees.Tree renames Grammar_Parser.Parsers.First_State_Ref.Tree;
      begin
         WisiToken_Grammar_Runtime.Translate_EBNF_To_BNF (Tree, Input_Data);
      end;
      Input_Data.Phase       := WisiToken_Grammar_Runtime.Other;
      Grammar_Parser.Execute_Actions;

      declare
         use all type Java_Expressions_Antlr_Actions.Token_Enum_ID;
         use all type WisiToken.Generate.LR.RHS_Sequence_Arrays.Vector;

         Generate_Data  : constant WisiToken.BNF.Generate_Utils.Generate_Data :=
           WisiToken.BNF.Generate_Utils.Initialize (Input_Data);

         Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

         function "+" (Item : in Java_Expressions_Antlr_Actions.Token_Enum_ID) return Token_ID_Arrays.Vector
         is begin
            return Token_ID_Arrays.To_Vector (Java_Expressions_Antlr_Actions."+" (Item));
         end "+";

         function "&"
           (Left : in Token_ID_Arrays.Vector;
            Right : in Java_Expressions_Antlr_Actions.Token_Enum_ID)
           return Token_ID_Arrays.Vector
         is begin
            return Token_ID_Arrays."&" (Left, Java_Expressions_Antlr_Actions."+" (Right));
         end "&";

         Computed : constant WisiToken.Generate.LR.Minimal_Sequence_Array :=
           WisiToken.Generate.LR.Compute_Minimal_Terminal_Sequences (Descriptor, Generate_Data.Grammar);

         Expected : WisiToken.Generate.LR.Minimal_Sequence_Array
           (+wisitoken_accept_ID .. +nonterminal_033_list_ID);

      begin
         Expected (+wisitoken_accept_ID) := +(False, +THIS_ID & Wisi_EOI_ID);
         Expected (+BOOL_LITERAL_ID)     := +(False, +TRUE_ID) & (False, +FALSE_ID);
         Expected (+literal_ID)          := +(False, +DECIMAL_LITERAL_ID) & (False, +TRUE_ID);

         Expected (+expressionList_ID) := +(False, +THIS_ID) & (True, +THIS_ID & COMMA_ID & THIS_ID);
         Expected (+methodCall_ID)     :=
           +(False, +IDENTIFIER_ID & LPAREN_ID & THIS_ID & RPAREN_ID) &
             (False, +IDENTIFIER_ID & LPAREN_ID & RPAREN_ID);
         Expected (+expression_ID)     :=
           +(False, +THIS_ID) &
             (True, +THIS_ID & DOT_ID & IDENTIFIER_ID) &
             (True, +THIS_ID & LBRACK_ID & THIS_ID & RBRACK_ID) &
             (False, +IDENTIFIER_ID & LPAREN_ID & RPAREN_ID) &
             (False, +LPAREN_ID & BOOLEAN_ID & RPAREN_ID & THIS_ID) &
             (True, +THIS_ID & INC_ID) &
             (False, +ADD_ID & THIS_ID) &
             (True, +THIS_ID & MUL_ID & THIS_ID) &
             (True, +THIS_ID & ADD_ID & THIS_ID);
         Expected (+primary_ID)         :=
           +(False, +LPAREN_ID & THIS_ID & RPAREN_ID) &
             (False, +THIS_ID) &
             (False, +DECIMAL_LITERAL_ID) &
             (False, +IDENTIFIER_ID) &
             (False, +BOOLEAN_ID & DOT_ID & CLASS_ID);
         Expected (+typeType_ID)        :=
           +(False, +BOOLEAN_ID & LBRACK_ID & RBRACK_ID) &
             (False, +BOOLEAN_ID);
         Expected (+primitiveType_ID)   := +(False, +BOOLEAN_ID) & (False, +INT_ID);
         Expected (+nonterminal_008_ID) :=
           +(False, +IDENTIFIER_ID) &
             (False, +IDENTIFIER_ID & LPAREN_ID & RPAREN_ID) &
             (False, +THIS_ID);
         Expected (+nonterminal_015_ID) := +(False, +INC_ID) & (False, +DEC_ID);
         Expected (+nonterminal_020_ID) :=
           +(False, +ADD_ID) &
             (False, +SUB_ID) &
             (False, +INC_ID) &
             (False, +DEC_ID);

         Expected (+nonterminal_024_ID) := +(False, +MUL_ID) & (False, +DIV_ID) & (False, +MOD_ID);
         Expected (+nonterminal_027_ID) := +(False, +ADD_ID) & (False, +SUB_ID);
         Expected (+nonterminal_034_ID) := +(False, +LBRACK_ID & RBRACK_ID);

         Expected (+nonterminal_033_list_ID) :=
           +(False, +LBRACK_ID & RBRACK_ID) & (True, +LBRACK_ID & RBRACK_ID & LBRACK_ID & RBRACK_ID);

         if WisiToken.Trace_Generate > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("Computed:");
            for S of Computed loop
               Ada.Text_IO.Put_Line (WisiToken.Generate.LR.Image (S, Descriptor));
            end loop;
            Ada.Text_IO.Put_Line ("Expected:");
            for S of Expected loop
               Ada.Text_IO.Put_Line (WisiToken.Generate.LR.Image (S, Descriptor));
            end loop;
         end if;
         Check ("1", Computed, Expected);
      end;
   end Test_Terminal_Sequence_2;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Expecting'Access, "Test_Expecting");
      Register_Routine (T, Test_Terminal_Sequence_1'Access, "Test_Terminal_Sequence_1");
      Register_Routine (T, Test_Terminal_Sequence_2'Access, "Test_Terminal_Sequence_2");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_lr_expecting_terminal_sequence.adb");
   end Name;

end Test_LR_Expecting_Terminal_Sequence;
