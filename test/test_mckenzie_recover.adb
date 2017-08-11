--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Lite;
with WisiToken.AUnit;
with WisiToken.Parser.LR.AUnit;
with WisiToken.Parser.LR.McKenzie_Recover.AUnit;
with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder.String;
with WisiToken.Text_Feeder.Text_IO;
with WisiToken.Token_Region.AUnit;
with ada_lite_dfa;
package body Test_McKenzie_Recover is

   String_Feeder : aliased WisiToken.Text_Feeder.String.Instance;
   Parser        : WisiToken.Parser.LR.Parser.Instance := Ada_Lite.Create_Parser
     (WisiToken.LALR,
      Text_Feeder => String_Feeder'Access);

   procedure Parse_Text (Text : in String; Debug : in Integer)
   is begin
      Parser.Enable_McKenzie_Recover := True;

      ada_lite_dfa.aflex_debug := Debug > 3;
      WisiToken.Trace_Parse    := Debug;

      Ada_Lite.Action_Count := (others => 0);

      if Debug > 0 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      String_Feeder.Set (Text & "   ");
      --  Trailing spaces so final token has proper region;
      --  otherwise it is wrapped to 1.

      Parser.Lexer.Reset (Buffer_Size => Text'Length);
      Parser.Parse;
   end Parse_Text;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Buffer_Region;
      Expected : in WisiToken.Buffer_Region)
   is
      use AUnit.Checks;
   begin
      Check (Label & ".Begin_Pos", Computed.Begin_Pos, Expected.Begin_Pos);
      Check (Label & ".End_Pos", Computed.End_Pos, Expected.End_Pos);
   end Check;

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Exceptions;
      use Ada_Lite;

      File_Name : constant String := "../wisi/test/ada_lite.input";
      Parser : WisiToken.Parser.LR.Parser.Instance := Create_Parser
        (WisiToken.LALR,
         Text_Feeder => WisiToken.Text_Feeder.Text_IO.Create (File_Name));
   begin
      --  The test is that there is no exception.

      ada_lite_dfa.aflex_debug := False; -- keep for future debugging
      WisiToken.Trace_Parse := Test.Debug;

      Parser.Parse;
   exception
   when E : WisiToken.Syntax_Error =>
      Ada.Text_IO.Put_Line (File_Name & ":" & Exception_Message (E));
      AUnit.Assertions.Assert (False, "syntax error");

   when E : others =>
      AUnit.Assertions.Assert (False, "parser raised exception: " & Exception_Name (E) & ": " & Exception_Message (E));
   end No_Error;

   procedure Error_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;", Test.Debug);
      --                1        |10       |20       |30       |40
      --  Missing "if" in "end if;"
      --
      --  error 1 at ';' 39. Inserts 'if', succeeds.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "exception: got syntax error exception.");
   end Error_1;

   procedure Error_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;

      use all type WisiToken.Region_Lists.Cursor;
   begin
      Parse_Text
        ("procedure Proc is begin Block_1: begin end; if A = 2 then end Block_2; end Proc_1; ", Test.Debug);
      --  |1       |10       |20       |30       |40       |50       |60       |70
      --  Missing "begin" in Block_2, but McKenzie won't find that.
      --
      --  Parser errors at Block_2, expecting "if". McKenzie inserts
      --  "if;", leaving  "Block_2;" as a procedure call.

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);

      Check ("errors.length", Ada_Lite.State.Errors.Length, 1);
      Check ("errors.invalid_region 1",
             WisiToken.Token_Region.Error_Data_Lists.Element (Ada_Lite.State.Errors.First).Invalid_Region,
             WisiToken.Null_Buffer_Region);
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "exception: got Syntax_Error");
   end Error_2;

   procedure Error_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text
        ("procedure Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; end Water; ",
         --        |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
         Test.Debug);
      --  Missing "end if" at 67.

      --  Enters recover at ';' 83.
      --  Inserts 'if'. Continues to 'loop' 90, error expecting block label or ';'.
      --  Inserts ';'. Continues to ';' 94, expecting statement or 'end loop'.
      --  Inserts 'end loop'. Continues to 'Water' 100, expecting 'loop'.
      --  Inserts 'loop'. Continues to EOF (treating 'Water' as loop label), expecting
      --  statement or 'end <procedure>;' .
      --  Inserts 'end;', succeeds
      Check ("errors.length", State.Errors.Length, 5);
      declare
         use WisiToken.AUnit;
         use WisiToken.Token_Region;
         use WisiToken.Token_Region.AUnit;
         use WisiToken.Token_Region.Error_Data_Lists;
         Cursor : Error_Data_Lists.Cursor := State.Errors.First;
      begin
         Check
           ("1", Element (Cursor),
            (First_Terminal    => Descriptor.First_Terminal,
             Last_Terminal     => Descriptor.Last_Terminal,
             Error_Token       => (+SEMICOLON_ID, (84, 84)),
             Expecting         => To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (1 => +IF_ID)),
             Invalid_Region    => WisiToken.Null_Buffer_Region,
             Recover           => null),
            Check_Recover_Data => null);

         Next (Cursor);
         Check
           ("2", Element (Cursor),
            (Descriptor.First_Terminal, Descriptor.Last_Terminal,
             (+LOOP_ID, (90, 93)),
             To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (+SEMICOLON_ID, +IDENTIFIER_ID)),
             WisiToken.Null_Buffer_Region,
             Recover => null),
            Check_Recover_Data => null);

         Next (Cursor);
         Check
           ("3", Element (Cursor),
            (Descriptor.First_Terminal, Descriptor.Last_Terminal,
             (+SEMICOLON_ID, (94, 94)),
             To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (+BEGIN_ID, +CASE_ID, +DECLARE_ID, +END_ID, +EXIT_ID, +IF_ID, +LOOP_ID, +RETURN_ID, +IDENTIFIER_ID)),
             WisiToken.Null_Buffer_Region,
             Recover => null),
            Check_Recover_Data => null);

         Next (Cursor);
         Check
           ("4", Element (Cursor),
            (Descriptor.First_Terminal, Descriptor.Last_Terminal,
             (+IDENTIFIER_ID, (100, 104)),
             To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (1 => +LOOP_ID)),
             WisiToken.Null_Buffer_Region,
             Recover => null),
            Check_Recover_Data => null);

         Next (Cursor);
         Check
           ("5", Element (Cursor),
            (Descriptor.First_Terminal, Descriptor.Last_Terminal,
             (+Wisi_EOI_ID, (1, 1)),
             To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (+BEGIN_ID, +CASE_ID, +DECLARE_ID, +ELSE_ID, +ELSIF_ID, +END_ID, +EXIT_ID, +IF_ID, +LOOP_ID,
                 +RETURN_ID, +WHEN_ID, +IDENTIFIER_ID))
             , WisiToken.Null_Buffer_Region,
             Recover => null),
            Check_Recover_Data => null);

         Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      end;
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Error_3;

   procedure Dotted_Name (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parser.Table.McKenzie.Enqueue_Limit := 35; -- test that the special rule reduces enqueues.

      Parse_Text
        ("procedure Parent.Water is begin loop begin D; if A then if B then end if; exit when C; end; end loop; " &
           --      |10       |20       |30       |40       |50       |60       |70       |80       |90       |100
           "end Parent.Water; ",
         --  |104  |110      |120
         Test.Debug);
      --  Missing "end if" at 67. Same as Error_3, but procedure name
      --  is dotted, so it can't be a loop label.
      --
      --  Test special rule for dotted names.

      --  error 1: at ';' 91, expecting 'if'.
      --  Inserts 'if'. Continues to error 2: at 'loop' 97, expecting block label or ';'.
      --  Inserts ';'. Continues to error 3: at ';' 101, expecting statement or 'end loop'.
      --  Inserts 'end loop'. Continues to error 4: at 'Parent' 107, expecting 'loop'.
      --  Inserts 'loop'. Continues to error 5: at '.' 113, expecting ';'
      --  Inserts '; IDENTIFIER'. Continues to error 6: at EOF, expecting statement or 'end;'
      --  Inserts 'end ;', succeeds.
      --
      --  With the full Ada language, finding '; IDENTIFIER' for error 5 takes too
      --  long, so we introduce the Dotted_Name special rule to shortcut it; that
      --  cuts the enqueued configs to 4.

      Check ("errors.length", State.Errors.Length, 6);
      Check ("action_count", Action_Count (+subprogram_body_ID), 1);

      declare
         use WisiToken.Parser.LR.AUnit;
         use WisiToken.Parser.LR.McKenzie_Recover.AUnit;
         use WisiToken.Token_Region.AUnit;
         use WisiToken.Token_Region.Error_Data_Lists;
         use WisiToken.Token_Region;
         use WisiToken.AUnit;
         Cursor : Error_Data_Lists.Cursor := State.Errors.First;
      begin
         for I in 2 .. 5 loop
            Next (Cursor);
         end loop;

         if WisiToken.Trace_Parse > 0 then
            Ada.Text_IO.Put ("Config: ");
            WisiToken.Parser.LR.McKenzie_Recover.Put
              (State.Trace.Descriptor.all,
               WisiToken.Parser.LR.McKenzie_Recover.Configuration (Element (Cursor).Recover.all));
            Ada.Text_IO.New_Line;
         end if;

         Check
           ("errors.5.recover (Dotted_Name)",
            WisiToken.Parser.LR.McKenzie_Recover.Configuration (Element (Cursor).Recover.all),
            WisiToken.Parser.LR.McKenzie_Recover.Configuration'
              (Stack                  => To_State_Stack
                 --  FIXME: two consecutive semicolons?
                 (((189, +SEMICOLON_ID), (188, +SEMICOLON_ID), (167, +LOOP_ID), (160, +END_ID),
                   (127, +sequence_of_statements_opt_ID), (101, +LOOP_ID), (35, +BEGIN_ID),
                   (30, +declarative_part_opt_ID), (11, +IS_ID), (10, +subprogram_specification_ID),
                   (0, WisiToken.Invalid_Token_ID))),
               Shared_Lookahead_Index => 1,
               Local_Lookahead        => WisiToken.Empty_Token_Array,
               Local_Lookahead_Index  => 0,
               Pushed                 => WisiToken.Empty_Token_Array,
               Popped                 => WisiToken.Empty_Token_Array,
               Inserted               => To_Token_Array ((1 => +SEMICOLON_ID)),
               --  FIXME: IDENTIFIER inserted by special rule does not show up in recover data for reuse
               Deleted                => WisiToken.Empty_Token_Array,
               Cost                   => 1.0));
      end;
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Dotted_Name;

   procedure Error_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("else then ", Test.Debug);
      --  Bogus syntax; test no exceptions due to empty stack etc.

      Assert (False, "1.exception: did not get Syntax_Error");
   exception
   when WisiToken.Syntax_Error =>
      Check ("error.length", State.Errors.Length, 1);
   end Error_4;

   procedure Error_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Debug is begin B; elsif then else end if; end; ", Test.Debug);
      --  Deleted "if then" (to move it elsewhere).

      Check ("error.length", State.Errors.Length, 1);

      Parse_Text ("procedure Debug is begin elsif then else end if; end; ", Test.Debug);
      --  Same, no 'B;'

      Check ("error.length", State.Errors.Length, 1);
   exception
   when WisiToken.Syntax_Error =>
      Assert (True, "exception: got Syntax_Error");
   end Error_5;

   procedure Check_Accept (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parse_Text ("procedure Debug is begin loop B; end; ", Test.Debug);
      --  Missing "end loop"
      --
      --  Inserts 'loop', continues to EOF, inserts 'end;', succeeds
      Check ("errors.length", State.Errors.Length, 2);

   exception
   when WisiToken.Syntax_Error =>
      Assert (True, "1.exception: got Syntax_Error");
      Check ("error.length", State.Errors.Length, 2);
   end Check_Accept;

   procedure Extra_Begin (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;
   begin
      Parser.Table.McKenzie.Enqueue_Limit := 100; -- needed for this test

      Parse_Text
        ("procedure Debug is begin procedure Put_Top_10 is begin end Put_Top_10; begin end Debug; ",
         --        |10       |20       |30       |40       |50       |60       |70       |80
         Test.Debug);
      --  Added 'begin' at end, intending to delete first 'begin'
      --
      --  Pop 'begin 20', reduced declarative_part_opt. Continue to EOF.
      Check ("errors.length", State.Errors.Length, 1);
      declare
         use WisiToken.AUnit;
         use WisiToken.Parser.LR.AUnit;
         use WisiToken.Token_Region.AUnit;
         use WisiToken.Token_Region.Error_Data_Lists;
         Cursor : constant WisiToken.Token_Region.Error_Data_Lists.Cursor := State.Errors.First;
      begin

         if WisiToken.Trace_Parse > 0 then
            Ada.Text_IO.Put ("Config: ");
            WisiToken.Parser.LR.McKenzie_Recover.Put
              (State.Trace.Descriptor.all,
               WisiToken.Parser.LR.McKenzie_Recover.Configuration (Element (Cursor).Recover.all));
            Ada.Text_IO.New_Line;
         end if;

         Check
           ("errors.1",
            Element (Cursor),
            (First_Terminal            => Descriptor.First_Terminal,
             Last_Terminal             => Descriptor.Last_Terminal,
             Error_Token               => (+PROCEDURE_ID, (26, 34)),
             Expecting                 => To_Token_ID_Set
               (Descriptor.First_Terminal,
                Descriptor.Last_Terminal,
                (+BEGIN_ID, +CASE_ID, +DECLARE_ID, +END_ID, +EXIT_ID, +IF_ID, +LOOP_ID, +RETURN_ID, +IDENTIFIER_ID)),
             Invalid_Region            => (20, 24),
             Recover                   => new WisiToken.Parser.LR.McKenzie_Recover.Configuration'
               (Stack                  => To_State_Stack
                  (((11, +IS_ID), (10, +subprogram_specification_ID), (0, WisiToken.Invalid_Token_ID))),
                Shared_Lookahead_Index => 1,
                Local_Lookahead        => WisiToken.Empty_Token_Array,
                Local_Lookahead_Index  => 0,
                Pushed                 => WisiToken.Empty_Token_Array,
                Popped                 => To_Token_Array ((+BEGIN_ID, +declarative_part_opt_ID)),
                Inserted               => WisiToken.Empty_Token_Array,
                Deleted                => WisiToken.Empty_Token_Array,
                Cost                   => 2.0)),
            Check_Recover_Data         => WisiToken.Parser.LR.McKenzie_Recover.AUnit.Check'Access);
      end;

   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Extra_Begin;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_mckenzie_recover.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug > 1 then
         Register_Routine (T, Error_1'Access, "debug");
      else
         Register_Routine (T, No_Error'Access, "No_Error");
         Register_Routine (T, Error_1'Access, "Error_1");
         Register_Routine (T, Error_2'Access, "Error_2");
         Register_Routine (T, Error_3'Access, "Error_3");
         Register_Routine (T, Dotted_Name'Access, "Dotted_Name");
         Register_Routine (T, Error_4'Access, "Error_4");
         Register_Routine (T, Error_5'Access, "Error_5");
         Register_Routine (T, Check_Accept'Access, "Check_Accept");
         Register_Routine (T, Extra_Begin'Access, "Extra_Begin");
      end if;
   end Register_Tests;

begin
   Parser.Table.Panic_Recover := (others => False);
end Test_McKenzie_Recover;
