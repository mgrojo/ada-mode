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
with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder.String;
with WisiToken.Text_Feeder.Text_IO;
with WisiToken.Token_Region.AUnit;
with ada_lite_dfa;
package body Test_Panic_Mode is

   String_Feeder : aliased WisiToken.Text_Feeder.String.Instance;
   Parser        : WisiToken.Parser.LR.Parser.Instance := Ada_Lite.Create_Parser
     (WisiToken.LALR,
      Text_Feeder => String_Feeder'Access);

   procedure Parse_Text (Text : in String; Debug : in Integer)
   is begin
      Parser.Enable_Panic_Recover := True;

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
      --  panic mode deletes 'if ..then', keeps 'end;', succeeds

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
      --  Missing "begin" in Block_2
      --  FIXME: failing

      Check ("action_count", Action_Count (+subprogram_body_ID), 1);

      Check ("errors.length", Ada_Lite.State.Errors.Length, 3);
      Check ("errors.invalid_region 1",
             WisiToken.Token_Region.Error_Data_Lists.Element (Ada_Lite.State.Errors.First).Invalid_Region,
             (25, 38));
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
      --  Missing "end if"; previous version reported mismatch between
      --  parser and augmented.

      --  Enters recover at ';' 83; pops stack to
      --  handled_sequence_of_statements 36, keeps end 80,
      --  FIXME: update description, add tests for rest of errors, or delete.
      --  FIXME: failing
      Check ("errors.length", State.Errors.Length, 5);
      declare
         use WisiToken.AUnit;
         use WisiToken.Token_Region.AUnit;
         use WisiToken.Token_Region;
         Temp : Error_Data renames Error_Data_Lists.Element (State.Errors.First);

         Expecting : WisiToken.Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Terminal) :=
           (others => False);
      begin
         Expecting (+IF_ID) := True;

         Check ("errors.error_token", Temp.Error_Token, (+SEMICOLON_ID, (84, 84)));
         Check ("errors.expecting", Temp.Expecting, Expecting);
         Check ("errors.invalid_region", Temp.Invalid_Region, (37, 83));
         Check ("action_count", Action_Count (+subprogram_body_ID), 1);
      end;
   exception
   when WisiToken.Syntax_Error =>
      Assert (False, "1.exception: got Syntax_Error");
   end Error_3;

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
      Parse_Text ("procedure Debug is begin elsif then else end if; end; ", Test.Debug);
      --  Deleted "if then" (to move it elsewhere).
      --  Caused "recover: non-shift action not supported" in earlier version.
      --  Now discards 'elsif then else', fails on 'end if; end'.
      --  FIXME: fails with mismatched token id in discard

      Assert (False, "1.exception: did not get Syntax_Error");
   exception
   when WisiToken.Syntax_Error =>
      if Test.Debug > 0 then
         for Data of State.Errors loop
            Ada.Text_IO.Put_Line
              ("error token: " & WisiToken.Token_Region.Image (Descriptor, Data.Error_Token, ID_Only => False) &
                 " expecting: " & WisiToken.Image (Descriptor, Data.Expecting));
         end loop;
      end if;
      Check ("error.length", State.Errors.Length, 2);
   end Error_5;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_panic_mode.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug > 0 then
         Register_Routine (T, Error_1'Access, "debug");
      else
         Register_Routine (T, No_Error'Access, "No_Error");
         Register_Routine (T, Error_1'Access, "Error_1");
         Register_Routine (T, Error_2'Access, "Error_2");
         Register_Routine (T, Error_3'Access, "Error_3");
         Register_Routine (T, Error_4'Access, "Error_4");
         Register_Routine (T, Error_5'Access, "Error_5");
      end if;
   end Register_Tests;

end Test_Panic_Mode;
