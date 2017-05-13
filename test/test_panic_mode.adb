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
with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Lite_Process;
with FastToken.Text_Feeder.String;
with FastToken.Text_Feeder.Text_IO;
with ada_lite_process_dfa;
package body Test_Panic_Mode is

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   ----------
   --  Test procedures

   procedure No_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use Ada.Exceptions;
      use Ada_Lite_Process;

      File_Name : constant String := "../../wisi/test/ada_lite.input";
      Parser : LR_Parser.Instance := Create_Parser
        (FastToken.LALR,
         Text_Feeder => FastToken.Text_Feeder.Text_IO.Create (File_Name));
   begin
      --  The test is that there is no exception.

      ada_lite_process_dfa.aflex_debug := False; -- keep for future debugging
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
      use Ada_Lite_Process;

      Parser : LR_Parser.Instance := Create_Parser
        (FastToken.LALR,
         Text_Feeder => String_Feeder'Access);

      procedure Parse_Text (Label : in String; Text : in String)
      is begin
         String_Feeder.Set (Text);

         Parser.Reset (Buffer_Size => Text'Length + 1); -- +1 for EOF

         Parser.Parse;
      exception
      when E : FastToken.Syntax_Error =>
         Ada.Text_IO.Put_Line (Label & ":" & Exception_Message (E));
         AUnit.Assertions.Assert (False, "syntax error");
      when E : others =>
         AUnit.Assertions.Assert (False, Label & ": " & Exception_Message (E));
      end Parse_Text;
   begin
      ada_lite_process_dfa.aflex_debug := False; -- keep this here for future debugging
      FastToken.Trace_Parse := Test.Debug;

      Parse_Text
        ("1", "procedure Proc_1 is begin if A = 2 then end;");
      --  Enters recovery at final semicolon, expecting "if; end;". It
      --  pops the parse stack to "begin", where a "statement" can
      --  follow, and "end" can follow that.

      --   FIXME: check that subprogram_body:0, if_statement:* got
      --  executed for Proc_1, nothing else; => array of list of regions?
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
