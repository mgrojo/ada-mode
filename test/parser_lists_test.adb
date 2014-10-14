-------------------------------------------------------------------------------
--
--  Copyright (C) 2014 Stephen Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Check;
with Ada.Exceptions;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Parser_Lists_Test is

   --  we need an instantiation of OpenToken.Production.Parser.LALR.Parser to test

   type Token_ID is (Identifier_ID, If_ID, Then_ID, Else_ID, End_ID, EOF_ID, Statement_ID, Procedure_ID);

   package Token is new OpenToken.Token.Enumerated (Token_ID, If_ID, EOF_ID, Token_ID'Image);
   package Token_List is new Token.List;
   package Nonterminal is new Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Token, Token_List, Nonterminal);
   package Tokenizer is new Token.Analyzer;
   package Parser is new Production.Parser (Tokenizer);
   package LALRs is new Parser.LALR (First_State_Index => 1);
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label => 0);

   --  These duplicate gen_opentoken_aunit.ads, but we don't need all of that.
   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_ID);
   procedure Check is new AUnit.Check.Gen_Check_Discrete (LALRs.Parse_Action_Verbs);
   procedure Check is new AUnit.Check.Gen_Check_Discrete (LALRs.Unknown_State_Index);
   procedure Check is new AUnit.Check.Gen_Check_Access (Token.Class, Token.Handle);

   procedure Check
     (Label    : in String;
      Computed : in Parser_Lists.Stack_Item;
      Expected : in Parser_Lists.Stack_Item)
   is
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Token", Computed.Token, Expected.Token);
   end Check;

   ----------
   --  Test procedures

   procedure Init (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use LALRs;
      use Parser_Lists;
      use AUnit.Check;

      Parsers : List := Initialize;
      Cursor  : constant Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("1: Count", Parsers.Count, 1);
      Check ("1: Is_Done", Cursor.Is_Done, False);
      Check ("1: Label", Cursor.Label, 0);
      Check ("1: Verb", Cursor.Verb, Shift);
      Check ("1: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("1: Peek.State", Cursor.Peek.State, State_Index'First);
      Check ("1: Peek.Token", Cursor.Peek.Token, null);
      Check ("1: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, True);
   end Init;

   procedure Stack (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use LALRs;
      use Parser_Lists;
      use AUnit.Check;

      Parsers : List := Initialize;
      Cursor  : constant Parser_Lists.Cursor := Parsers.First;

      Item_1 : constant Stack_Item := (2, new Token.Class'(Token.Get (If_ID)));
      Item_2 : constant Stack_Item := (3, new Token.Class'(Token.Get (Then_ID)));
   begin
      Check ("1: Pop", Cursor.Pop, (State_Index'First, null));
      Check ("1: Stack_Empty", Cursor.Stack_Empty, True);
      Check ("1: Stack_Free_Count", Parsers.Stack_Free_Count, 1);

      Cursor.Push (Item_1);
      Check ("2a: Stack_Free_Count", Parsers.Stack_Free_Count, 0);
      Check ("2a: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("2: Pop", Cursor.Pop, Item_1);
      Check ("2b: Stack_Free_Count", Parsers.Stack_Free_Count, 1);
      Check ("2b: Stack_Empty", Cursor.Stack_Empty, True);

      Cursor.Push (Item_1);
      Cursor.Push (Item_2);
      Check ("3a: Stack_Free_Count", Parsers.Stack_Free_Count, 0);
      Check ("3a: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("3b: Pop", Cursor.Pop, Item_2);
      Check ("3b: Peek.State", Cursor.Peek.State, Item_1.State);
      Check ("3b: Stack_Free_Count", Parsers.Stack_Free_Count, 1);
      Check ("3b: Stack_Empty", Cursor.Stack_Empty, False);

      Check ("3c: Pop", Cursor.Pop, Item_1);
      Check ("3c: Stack_Free_Count", Parsers.Stack_Free_Count, 2);
      Check ("3c: Stack_Empty", Cursor.Stack_Empty, True);
   end Stack;

   procedure Parser_List (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use LALRs;
      use Parser_Lists;
      use AUnit.Check;

      Parsers : List := Initialize;
      Cursor  : Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("0: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("0a: Label", Cursor.Label, 0);

      Prepend_Copy (Parsers, Cursor);
      Check ("0b: Label", Cursor.Label, 0);

      Cursor := Parsers.First;
      Check ("1: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("1: Count", Parsers.Count, 2);
      Check ("1: Label", Cursor.Label, 1);

      Cursor.Next;
      Check ("2: Label", Cursor.Label, 0);

      --  Delete last parser
      Free (Cursor);
      Check ("3: Parser_Free_Count", Parsers.Parser_Free_Count, 1);
      Check ("3: Count", Parsers.Count, 1);
      Check ("3: Is_Done", Cursor.Is_Done, True);

      Cursor := Parsers.First;
      Prepend_Copy (Parsers, Cursor);
      Cursor := Parsers.First;
      Check ("4: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("4: Count", Parsers.Count, 2);
      Check ("4: Label", Cursor.Label, 2);

      --  Delete first parser; cursor advances to next
      Free (Cursor);
      Check ("5: Parser_Free_Count", Parsers.Parser_Free_Count, 1);
      Check ("5: Count", Parsers.Count, 1);
      Check ("5: Is_Done", Cursor.Is_Done, False);
      Check ("5: Label", Cursor.Label, 1);

      Free (Cursor);
      Check ("6: Parser_Free_Count", Parsers.Parser_Free_Count, 2);
      Check ("6: Count", Parsers.Count, 0);
      Check ("6: Is_Done", Cursor.Is_Done, True);
   end Parser_List;

   procedure Stack_Equal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use LALRs;
      use Parser_Lists;
      use AUnit.Check;

      Parsers : List                := Initialize;
      Item_1  : constant Stack_Item := (2, new Token.Class'(Token.Get (If_ID)));
      Item_2  : constant Stack_Item := (3, new Token.Class'(Token.Get (Then_ID)));

      Cursor_1 : constant Parser_Lists.Cursor := Parsers.First;
      Cursor_2 : Parser_Lists.Cursor;
   begin
      Prepend_Copy (Parsers, Cursor_1);
      Check ("0a: Label", Cursor_1.Label, 0);
      Cursor_1.Push (Item_1);
      Cursor_1.Push (Item_2);

      Cursor_2 := Parsers.First;
      Check ("0b: Label", Cursor_2.Label, 1);
      Cursor_2.Push (Item_1);
      Cursor_2.Push (Item_2);

      Check ("1", Stack_Equal (Cursor_1, Cursor_2), True);

      declare
         Junk : Stack_Item := Pop (Cursor_2);
         pragma Unreferenced (Junk);
      begin
         Check ("2", Stack_Equal (Cursor_1, Cursor_2), False);
      end;
   end Stack_Equal;

   procedure Pending (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use LALRs;
      use Parser_Lists;
      use AUnit.Check;

      Parsers : List                  := Initialize;
      Item_1  : constant Action_Token :=
        (Null_Reduce_Action_Rec, new Nonterminal.Class'(Nonterminal.Get (Statement_ID)), Token_List.Null_List);
      Item_2  : constant Action_Token :=
        (Null_Reduce_Action_Rec, new Nonterminal.Class'(Nonterminal.Get (Procedure_ID)), Token_List.Null_List);

      Cursor : constant Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("0: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, True);
      Check ("0: action_token_count", Cursor.Action_Token_Count, 0);
      Cursor.Enqueue (Item_1);
      Check ("0a: action_token_count", Cursor.Action_Token_Count, 1);
      Check ("0a: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, False);
      Cursor.Enqueue (Item_2);
      Check ("0b: action_token_count", Cursor.Action_Token_Count, 2);
      Check ("0b: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, False);
      Check ("0: free count", Parsers.Action_Token_Free_Count, 0);

      Check ("1 dequeue", Token.ID (Dequeue (Cursor).New_Token.all), Statement_ID);
      Check ("1: action_token_count", Cursor.Action_Token_Count, 1);
      Check ("1: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, False);
      Check ("1: free count", Parsers.Action_Token_Free_Count, 1);

      Check ("2 dequeue", Token.ID (Dequeue (Cursor).New_Token.all), Procedure_ID);
      Check ("2: action_token_count", Cursor.Action_Token_Count, 0);
      Check ("2: Action_Tokens_Empty", Cursor.Action_Tokens_Empty, True);
      Check ("2: free count", Parsers.Action_Token_Free_Count, 2);

   end Pending;

   procedure Copy (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use AUnit.Check;
      use Parser_Lists;
      use LALRs;
      use Token_List;
      use Parser_Lists_Test;

      Parsers : List := Initialize;

      If_1    : constant Token.Handle := new Token.Class'(Token.Get (If_ID));
      Then_1  : constant Token.Handle := new Token.Class'(Token.Get (Then_ID));
      Else_1  : constant Token.Handle := new Token.Class'(Token.Get (Else_ID));
      Ident_A : constant Token.Handle := new Token.Class'(Token.Get (Identifier_ID));
      If_2    : constant Token.Handle := new Token.Class'(Token.Get (If_ID));
      Then_2  : constant Token.Handle := new Token.Class'(Token.Get (Then_ID));
      Ident_B : constant Token.Handle := new Token.Class'(Token.Get (Identifier_ID));
      End_2   : constant Token.Handle := new Token.Class'(Token.Get (End_ID));
      End_1   : constant Token.Handle := new Token.Class'(Token.Get (End_ID));

      Statement_A : constant Nonterminal.Handle := new Nonterminal.Class'(Nonterminal.Get (Statement_ID, "A"));
      Statement_B : constant Nonterminal.Handle := new Nonterminal.Class'(Nonterminal.Get (Statement_ID, "B"));
      Statement_1 : constant Nonterminal.Handle := new Nonterminal.Class'(Nonterminal.Get (Statement_ID, "1"));
      Statement_2 : constant Nonterminal.Handle := new Nonterminal.Class'(Nonterminal.Get (Statement_ID, "2"));

      Action_A  : constant Action_Token := (Null_Reduce_Action_Rec, Statement_A, Only (Ident_A));
      Action_B  : constant Action_Token := (Null_Reduce_Action_Rec, Statement_B, Only (Ident_B));
      Action_2  : constant Action_Token :=
        (Null_Reduce_Action_Rec, Statement_2, If_2 & Then_2 & Token.Handle (Statement_B) & End_2);
      Action_1  : constant Action_Token :=
        (Null_Reduce_Action_Rec,
         Statement_1,
         If_1 & Then_1 & Token.Handle (Statement_A) & Else_1 & Token.Handle (Statement_2) & End_1);

      Cursor : constant Parser_Lists.Cursor := Parsers.First;

      Junk : Stack_Item;
      pragma Unreferenced (Junk);
   begin
      --  All Action_Token.New_Token must point either to a token on
      --  Stack or to tokens in later Action_Token.Tokens; all
      --  nonterminals in Stack and Action_Token.Tokens must be pointed to by
      --  New_Tokens in previous actions.
      --
      --  Verify that is preserved in Prepend_Copy
      --
      --  To test this, we copy the parser state produced by a typical
      --  parse. Given the above tokens, we assume the following
      --  productions:
      --
      --     statement <= IDENTIFIER
      --     statement <= if then statement else statement end
      --     procedure <= statement <eof>
      --
      --  and the following input:
      --
      --     if then Statement_A else if then Statement_B end end <eof>

      --  We use sequential state numbers, for debugging in this test
      Cursor.Push ((1, If_1));
      Cursor.Push ((2, Then_1));
      Cursor.Push ((3, Ident_A));
      Junk := Cursor.Pop; -- Ident_A
      Cursor.Push ((4, Token.Handle (Statement_A)));
      Cursor.Enqueue (Action_A);
      Cursor.Push ((5, Else_1));

      Cursor.Push ((6, If_2));
      Cursor.Push ((7, Then_2));
      Cursor.Push ((8, Ident_B));
      Junk := Cursor.Pop; -- Ident_B
      Cursor.Push ((9, Token.Handle (Statement_B)));
      Cursor.Enqueue (Action_B);
      Cursor.Push ((10, End_2));

      if Test.Debug then Cursor.Put_Top_10; end if;
      Check_Action_Stack ("1a", Cursor);
      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then Parsers.First.Put_Top_10; end if;
      Check_Action_Stack ("1b", Parsers.First);

      Check ("1", Stack_Equal (Cursor, Parsers.First), True);

      Junk := Cursor.Pop; -- end_2
      Junk := Cursor.Pop; -- statement_B
      Junk := Cursor.Pop; -- then_2
      Junk := Cursor.Pop; -- if_2
      Cursor.Push ((11, Token.Handle (Statement_2)));
      Cursor.Enqueue (Action_2);

      if Test.Debug then Cursor.Put_Top_10; end if;
      Check_Action_Stack ("2a", Cursor);
      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then Parsers.First.Put_Top_10; end if;
      Check_Action_Stack ("2b", Parsers.First);
      Check ("2c", Stack_Equal (Cursor, Parsers.First), True);

      Cursor.Push ((12, End_1));
      Junk := Cursor.Pop; -- end_1
      Junk := Cursor.Pop; -- statement_2
      Junk := Cursor.Pop; -- else_1
      Junk := Cursor.Pop; -- statement_A
      Junk := Cursor.Pop; -- then_1
      Junk := Cursor.Pop; -- if_1
      Cursor.Push ((13, Token.Handle (Statement_1)));
      Cursor.Enqueue (Action_1);

      if Test.Debug then
         Cursor.Put_Top_10;
         Put_Action_Tokens (Cursor);
      end if;
      Check_Action_Stack ("3a", Cursor);

      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then
         Parsers.First.Put_Top_10;
         Put_Action_Tokens (Parsers.First);
      end if;

      Check ("3b", Stack_Equal (Cursor, Parsers.First), True);
      Check_Action_Stack ("3c", Parsers.First);

   exception
   when E : OpenToken.Programmer_Error =>
      AUnit.Assertions.Assert (False, Ada.Exceptions.Exception_Message (E));
   end Copy;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Init'Access, "Init");
      Register_Routine (T, Stack'Access, "Stack");
      Register_Routine (T, Parser_List'Access, "Parser_List");
      Register_Routine (T, Stack_Equal'Access, "Stack_Equal");
      Register_Routine (T, Pending'Access, "Pending");
      Register_Routine (T, Copy'Access, "Copy");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/parser_lists_test.adb");
   end Name;

end Parser_Lists_Test;
