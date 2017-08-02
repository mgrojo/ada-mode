--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2014, 2015, 2017 Stephen Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Exceptions;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Parser.LR.Parser_Lists;
with WisiToken.Text_IO_Trace;
with WisiToken.Token;
with WisiToken_AUnit; use WisiToken_AUnit;
package body Parser_Lists_Test is

   type Token_Enum_ID is (Identifier_ID, If_ID, Then_ID, Else_ID, End_ID, EOF_ID, Statement_ID, Procedure_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_Enum_ID,
      First_Terminal    => Identifier_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Statement_ID,
      Last_Nonterminal  => Procedure_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Statement_ID);
   use Token_Enum;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Parser.LR.Parser_Lists.Stack_Item;
      Expected : in WisiToken.Parser.LR.Parser_Lists.Stack_Item)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".State", Computed.State, Expected.State);
      Check (Label & ".Token", Computed.Token, Expected.Token);
   end Check;

   ----------
   --  Test procedures

   procedure Init (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.Parser_Lists;
      use AUnit.Checks;

      Parsers : List := New_List (First_State_Index => 1, First_Parser_Label => 1);
      Cursor  : constant Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("1: Count", Parsers.Count, 1);
      Check ("1: Is_Done", Cursor.Is_Done, False);
      Check ("1: Label", Cursor.Label, 1);
      Check ("1: Verb", Cursor.Verb, Shift);
      Check ("1: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("1: Peek", Cursor.Peek, (1, WisiToken.Invalid_Token));
      Check ("1: Action_Tokens_Empty", Cursor.Pending_Actions_Empty, True);
   end Init;

   procedure Stack (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.Parser_Lists;

      Parsers : List := New_List (First_State_Index => 0, First_Parser_Label => 0);
      Cursor  : constant Parser_Lists.Cursor := Parsers.First;

      Item_1 : constant Stack_Item := (2, +If_ID);
      Item_2 : constant Stack_Item := (3, +Then_ID);
   begin
      Check ("1: Pop", Cursor.Pop, (0, WisiToken.Invalid_Token));
      Check ("1: Stack_Empty", Cursor.Stack_Empty, True);

      Cursor.Push (Item_1);
      Check ("2a: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("2: Pop", Cursor.Pop, Item_1);
      Check ("2b: Stack_Empty", Cursor.Stack_Empty, True);

      Cursor.Push (Item_1);
      Cursor.Push (Item_2);
      Check ("3a: Stack_Empty", Cursor.Stack_Empty, False);
      Check ("3b: Pop", Cursor.Pop, Item_2);
      Check ("3b: Peek.State", Cursor.Peek.State, Item_1.State);
      Check ("3b: Stack_Empty", Cursor.Stack_Empty, False);

      Check ("3c: Pop", Cursor.Pop, Item_1);
      Check ("3c: Stack_Empty", Cursor.Stack_Empty, True);
   end Stack;

   procedure Parser_List (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.Parser_Lists;
      use AUnit.Checks;

      Parsers : List := New_List (First_State_Index => 1, First_Parser_Label => 1);
      Cursor  : Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("0: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("0a: Label", Cursor.Label, 1);

      Prepend_Copy (Parsers, Cursor);
      --  Cursor still points to original
      Check ("0b: Label", Cursor.Label, 1);

      Cursor := Parsers.First;
      Check ("1: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("1: Count", Parsers.Count, 2);
      Check ("1: Label", Cursor.Label, 2);

      Cursor.Next;
      Check ("2: Label", Cursor.Label, 1);

      --  Delete last parser (label 1)
      Free (Cursor);
      Check ("3: Parser_Free_Count", Parsers.Parser_Free_Count, 1);
      Check ("3: Count", Parsers.Count, 1);
      Check ("3: Is_Done", Cursor.Is_Done, True);

      Cursor := Parsers.First; -- label 2
      Prepend_Copy (Parsers, Cursor);
      Cursor := Parsers.First; -- label 3
      Check ("4: Parser_Free_Count", Parsers.Parser_Free_Count, 0);
      Check ("4: Count", Parsers.Count, 2);
      Check ("4: Label", Cursor.Label, 3);

      --  Delete first parser (label 3); cursor advances to next
      Free (Cursor);
      Check ("5: Parser_Free_Count", Parsers.Parser_Free_Count, 1);
      Check ("5: Count", Parsers.Count, 1);
      Check ("5: Is_Done", Cursor.Is_Done, False);
      Check ("5: Label", Cursor.Label, 2);

      Free (Cursor);
      Check ("6: Parser_Free_Count", Parsers.Parser_Free_Count, 2);
      Check ("6: Count", Parsers.Count, 0);
      Check ("6: Is_Done", Cursor.Is_Done, True);
   end Parser_List;

   procedure Stack_Equal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.Parser_Lists;
      use AUnit.Checks;

      Parsers : List := New_List (First_State_Index => 0, First_Parser_Label => 0);

      Item_1  : constant Stack_Item := (2, +If_ID);
      Item_2  : constant Stack_Item := (3, +Then_ID);

      Cursor_1 : constant Cursor := Parsers.First;
      Cursor_2 : Cursor;
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
      use WisiToken.AUnit;
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.Parser_Lists;
      use AUnit.Checks;

      Parsers : List := New_List (First_State_Index => 1, First_Parser_Label => 1);

      Item_1  : constant Action_Token :=
        ((Reduce, +Statement_ID, null, 0, 0), WisiToken.Token.List.Null_List);
      Item_2  : constant Action_Token :=
        ((Reduce, +Procedure_ID, null, 0, 0), WisiToken.Token.List.Null_List);

      Cursor : constant Parser_Lists.Cursor := Parsers.First;
   begin
      Check ("0: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, True);
      Check ("0: action_token_count", Cursor.Pending_Actions_Count, 0);
      Cursor.Enqueue (Item_1);
      Check ("0a: action_token_count", Cursor.Pending_Actions_Count, 1);
      Check ("0a: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, False);
      Cursor.Enqueue (Item_2);
      Check ("0b: action_token_count", Cursor.Pending_Actions_Count, 2);
      Check ("0b: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, False);
      Check ("0: free count", Parsers.Action_Token_Free_Count, 0);

      Check ("1 dequeue", Dequeue (Cursor).Action.LHS, +Statement_ID);
      Check ("1: action_token_count", Cursor.Pending_Actions_Count, 1);
      Check ("1: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, False);
      Check ("1: free count", Parsers.Action_Token_Free_Count, 1);

      Check ("2 dequeue", Dequeue (Cursor).Action.LHS, +Procedure_ID);
      Check ("2: action_token_count", Cursor.Pending_Actions_Count, 0);
      Check ("2: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, True);
      Check ("2: free count", Parsers.Action_Token_Free_Count, 2);

      --  Enqueue using free list
      Cursor.Enqueue (Item_1);
      Check ("3a: action_token_count", Cursor.Pending_Actions_Count, 1);
      Check ("3a: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, False);
      Cursor.Enqueue (Item_2);
      Check ("3b: action_token_count", Cursor.Pending_Actions_Count, 2);
      Check ("3b: Pending_Actions_Empty", Cursor.Pending_Actions_Empty, False);
      Check ("0: free count", Parsers.Action_Token_Free_Count, 0);

   end Pending;

   procedure Copy (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use AUnit.Checks;
      use WisiToken.Parser.LR.Parser_Lists;
      use WisiToken.Parser.LR;
      use WisiToken;
      use all type WisiToken.Token.List.Instance;

      Parsers : List := New_List (First_State_Index => 1, First_Parser_Label => 1);

      If_1        : constant Token_ID := +If_ID;
      Then_1      : constant Token_ID := +Then_ID;
      Ident_A     : constant Token_ID := +Identifier_ID; --  reduces to Statement_A
      Statement_A : constant Token_ID := +Statement_ID;
      Else_1      : constant Token_ID := +Else_ID;

      If_2        : constant Token_ID := +If_ID;
      Then_2      : constant Token_ID := +Then_ID;
      Ident_B     : constant Token_ID := +Identifier_ID; --  reduces to Statement_B
      Statement_B : constant Token_ID := +Statement_ID;
      End_2       : constant Token_ID := +End_ID;

      Statement_1 : constant Token_ID := +Statement_ID; -- all of if_2 .. end_2
      End_1       : constant Token_ID := +End_ID;

      Statement_2 : constant Token_ID := +Statement_ID; -- all of if_1 .. end_1

      Action_A  : constant Action_Token := ((Reduce, +Statement_ID, null, 1, 1), Only (Ident_A));
      Action_B  : constant Action_Token := ((Reduce, +Statement_ID, null, 2, 1), Only (Ident_B));
      Action_2  : constant Action_Token :=
        ((Reduce, +Statement_ID, null, 3, 4), Only (If_2) & Then_2 & Statement_B & End_2);
      Action_1  : constant Action_Token :=
        ((Reduce, +Statement_ID, null, 4, 6),
         Only (If_1) & Then_1 & Statement_A & Else_1 & Statement_2 & End_1);

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
      Cursor.Push ((4, Statement_A));
      Cursor.Enqueue (Action_A);
      Cursor.Push ((5, Else_1));

      Cursor.Push ((6, If_2));
      Cursor.Push ((7, Then_2));
      Cursor.Push ((8, Ident_B));
      Junk := Cursor.Pop; -- Ident_B
      Cursor.Push ((9, Statement_B));
      Cursor.Enqueue (Action_B);
      Cursor.Push ((10, End_2));

      if Test.Debug then Put_Top_10 (Trace, Cursor); end if;
      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then Put_Top_10 (Trace, Parsers.First); end if;

      Check ("1", Stack_Equal (Cursor, Parsers.First), True);

      Junk := Cursor.Pop; -- end_2
      Junk := Cursor.Pop; -- statement_B
      Junk := Cursor.Pop; -- then_2
      Junk := Cursor.Pop; -- if_2
      Cursor.Push ((11, Statement_2));
      Cursor.Enqueue (Action_2);

      if Test.Debug then Put_Top_10 (Trace, Cursor); end if;
      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then Put_Top_10 (Trace, Parsers.First); end if;
      Check ("2c", Stack_Equal (Cursor, Parsers.First), True);

      Cursor.Push ((12, End_1));
      Junk := Cursor.Pop; -- end_1
      Junk := Cursor.Pop; -- statement_2
      Junk := Cursor.Pop; -- else_1
      Junk := Cursor.Pop; -- statement_A
      Junk := Cursor.Pop; -- then_1
      Junk := Cursor.Pop; -- if_1
      Cursor.Push ((13, Statement_1));
      Cursor.Enqueue (Action_1);

      if Test.Debug then
         Put_Top_10 (Trace, Cursor);
         Put_Pending_Actions (Trace, Cursor);
      end if;

      Parsers.Prepend_Copy (Cursor);
      if Test.Debug then
         Put_Top_10 (Trace, Parsers.First);
         Put_Pending_Actions (Trace, Parsers.First);
      end if;

      Check ("3b", Stack_Equal (Cursor, Parsers.First), True);

   exception
   when E : WisiToken.Programmer_Error =>
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
      return new String'("../Test/parser_lists_test.adb");
   end Name;

end Parser_Lists_Test;
