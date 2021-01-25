--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2021 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Warth_Left_Recurse_Expr_1_Actions;
with Warth_Left_Recurse_Expr_1_Packrat_Gen_Main;
with Warth_Left_Recurse_Expr_1_Packrat_Proc_Main;
with Warth_Left_Recurse_Expr_1_Runtime;
with WisiToken.AUnit;
with WisiToken.Parse.Packrat.Generated;
with WisiToken.Parse.Packrat.Procedural;
with WisiToken.Text_IO_Trace;
package body Warth_Left_Recurse_Expr_1 is

   User_Data : aliased Warth_Left_Recurse_Expr_1_Runtime.User_Data_Type;
   Trace : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File : Ada.Text_IO.File_Type;

   ----------
   --  Test procedures

   procedure Test_Parse_Gen (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Parse.Packrat.Generated;

      Parser : aliased WisiToken.Parse.Base_Parser'Class := Warth_Left_Recurse_Expr_1_Packrat_Gen_Main.Create_Parser
        (Trace'Access, User_Data'Access);

      procedure Execute_Parse
        (Input           : in String;
         Expected_State  : in WisiToken.Parse.Packrat.Generated.Result_States;
         Expected_Result : in Integer)
      is
         use AUnit.Checks;
      begin
         Parser.Tree.Lexer.Reset_With_String (Input);
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("input: '" & Input & "'");
         end if;

         Parser.Parse (Log_File);

         AUnit.Assertions.Assert (Expected_State = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

         Parser.Execute_Actions
           (Action_Region_Bytes => (WisiToken.Buffer_Pos (Input'First), WisiToken.Buffer_Pos (Input'Last)));
         Check ("result", User_Data.Stack.Pop, Expected_Result);

      exception
      when WisiToken.Syntax_Error =>
         AUnit.Assertions.Assert (Expected_State = Failure, "'" & Input & "': expected success; got Syntax_Error");

      when AUnit.Assertions.Assertion_Error =>
         raise;

      when E : others =>
         Ada.Text_IO.Put_Line
           ("'" & Input & "': exception " & Ada.Exceptions.Exception_Name (E) & " : " &
              Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         AUnit.Assertions.Assert (False, "'" & Input & "': " & Ada.Exceptions.Exception_Name (E));
      end Execute_Parse;

   begin
      Execute_Parse ("1 - 3", Success, -2);
      Execute_Parse ("1", Success, 1);
      Execute_Parse ("3 - 2 - 1", Success, 0);
      Execute_Parse ("3 -", Failure, 0);
   end Test_Parse_Gen;

   procedure Test_Parse_Proc (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Warth_Left_Recurse_Expr_1_Actions;
      use WisiToken.AUnit;
      use all type WisiToken.Parse.Packrat.Procedural.Result_States;

      Parser : aliased WisiToken.Parse.Base_Parser'Class :=
        Warth_Left_Recurse_Expr_1_Packrat_Proc_Main.Create_Parser
          (Trace'Access, User_Data'Access);

      procedure Execute_Parse
        (Input           : in String;
         Expected_State  : in WisiToken.Parse.Packrat.Procedural.Result_States;
         Expected_Result : in Integer)
      is
         use AUnit.Checks;
      begin
         Parser.Tree.Lexer.Reset_With_String (Input);
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("input: '" & Input & "'");
         end if;

         Parser.Parse (Log_File);

         AUnit.Assertions.Assert
           (Expected_State = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

         if WisiToken.Trace_Tests > WisiToken.Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("parse tree:");
            Parser.Tree.Print_Tree (Parser.Tree.Root, null);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root));
         end if;

         Parser.Execute_Actions
           (Action_Region_Bytes => (WisiToken.Buffer_Pos (Input'First), WisiToken.Buffer_Pos (Input'Last)));
         Check ("result", User_Data.Stack.Pop, Expected_Result);

      exception
      when WisiToken.Syntax_Error =>
         AUnit.Assertions.Assert (Expected_State = Failure, "'" & Input & "': expected success; got Syntax_Error");

      when AUnit.Assertions.Assertion_Error =>
         raise;

      when E : others =>
         Ada.Text_IO.Put_Line
           ("'" & Input & "': exception " & Ada.Exceptions.Exception_Name (E) & " : " &
              Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         AUnit.Assertions.Assert (False, "'" & Input & "': " & Ada.Exceptions.Exception_Name (E));
      end Execute_Parse;

   begin
      declare
         Expected : WisiToken.Token_ID_Set (+wisitoken_accept_ID .. +expr_ID) := (others => False);
      begin
         Expected (+expr_ID) := True;

         Check ("direct_left_recursive",
                WisiToken.Parse.Packrat.Procedural.Parser (Parser).Direct_Left_Recursive,
                Expected);
      end;

      Execute_Parse ("1 - 3", Success, -2);
      Execute_Parse ("1", Success, 1);
      Execute_Parse ("3 - 2 - 1", Success, 0);
      Execute_Parse ("3 -", Failure, 0);
   end Test_Parse_Proc;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("warth_left_recurse_expr_1.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse_Gen'Access, "Test_Parse_Gen");
      Register_Routine (T, Test_Parse_Proc'Access, "Test_Parse_Proc");
   end Register_Tests;

end Warth_Left_Recurse_Expr_1;
