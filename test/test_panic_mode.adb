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
with FastToken.Text_Feeder.String;
with FastToken.Text_Feeder.Text_IO;
with ada_lite_dfa;
package body Test_Panic_Mode is

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   procedure Check
     (Label    : in String;
      Computed : in FastToken.Buffer_Region;
      Expected : in FastToken.Buffer_Region)
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

      File_Name : constant String := "../../wisi/test/ada_lite.input";
      Parser : LR_Parser.Instance := Create_Parser
        (FastToken.LALR,
         Text_Feeder => FastToken.Text_Feeder.Text_IO.Create (File_Name));
   begin
      --  The test is that there is no exception.

      ada_lite_dfa.aflex_debug := False; -- keep for future debugging
      FastToken.Trace_Parse := Test.Debug;

      Parser.Parse;
   exception
   when E : FastToken.Syntax_Error =>
      Ada.Text_IO.Put_Line (File_Name & ":" & Exception_Message (E));
      AUnit.Assertions.Assert (False, "syntax error");

   when E : others =>
      AUnit.Assertions.Assert (False, "parser raised exception: " & Exception_Name (E) & ": " & Exception_Message (E));
   end No_Error;

   procedure Errors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Exceptions;
      use Ada_Lite;
      use AUnit.Assertions;
      use AUnit.Checks;

      Parser : LR_Parser.Instance := Create_Parser
        (FastToken.LALR,
         Text_Feeder => String_Feeder'Access);

      procedure Parse_Text (Text : in String)
      is begin
         if Test.Debug > 0 then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("input: '" & Text & "'");
         end if;

         String_Feeder.Set (Text & "   ");
         --  Trailing spaces so final token has proper region;
         --  otherwise it is wrapped to 1.

         Parser.Reset (Buffer_Size => Text'Length);
         Parser.Parse;
      end Parse_Text;
   begin
      ada_lite_dfa.aflex_debug := Test.Debug > 3;
      FastToken.Trace_Parse := Test.Debug;

      Action_Count (subprogram_body_ID) := 0; -- incremented in No_Error

      begin
         Parse_Text ("procedure Proc_1 is begin if A = 2 then end; end;");
         --                1        |10       |20       |30       |40
         --  Missing "if" in "end if;"
         --
         --  panic mode encounters EOF and bottom of stack and gives up.

         Assert (False, "1.exception: did not get syntax error exception.");
      exception
      when FastToken.Syntax_Error =>
         Check ("1.action_count", Action_Count (subprogram_body_ID), 0);
      end;

      declare
         use all type FastToken.Region_Lists.Cursor;
      begin
         Parse_Text
           ("procedure Proc_1 is begin end Proc_1; procedure Proc_2 is if A = 2 then end;");
         --  |1       |10       |20       |30       |40       |50       |60       |70
         --  Missing "begin" in Proc_2
         --
         --  panic mode encounters EOF and accepts Proc_1.

         Check ("2.action_count", Action_Count (subprogram_body_ID), 1);

         --  We don't have a Check for Region_Lists.
         Check ("2.error_status.invalid_regions.length", State_Aug.Invalid_Regions.Length, 1);
         Check ("2.error_status.invalid_regions.first", Element (State_Aug.Invalid_Regions.First), (39, 76));
      exception
      when FastToken.Syntax_Error =>
         Assert (False, "2.exception: got Syntax_Error");
      end;

   end Errors;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_panic_mode.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug > 0 then
         Register_Routine (T, Errors'Access, "debug");
      else
         Register_Routine (T, No_Error'Access, "No_Error");
         Register_Routine (T, Errors'Access, "Errors");
      end if;
   end Register_Tests;

end Test_Panic_Mode;
