--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
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
with Warth_Left_Recurse_Expr_1_Main;
with Warth_Left_Recurse_Expr_1_Runtime;
with WisiToken.Parse.Packrat.AUnit;
with WisiToken.Text_IO_Trace;
package body Warth_Left_Recurse_Expr_1 is

   User_Data : aliased Warth_Left_Recurse_Expr_1_Runtime.User_Data_Type;

   Parser : aliased Warth_Left_Recurse_Expr_1_Main.Parser_Type;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Warth_Left_Recurse_Expr_1_Actions.Descriptor'Access);

   ----------
   --  Test procedures

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Parse.Packrat;

      procedure Execute_Parse
        (Input           : in String;
         Expected_State  : in WisiToken.Parse.Packrat.Result_States;
         Expected_Result : in Integer)
      is
         use AUnit.Checks;
         use WisiToken.Parse.Packrat.AUnit;
      begin
         Parser.Lexer.Reset_With_String (Input);
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("input: '" & Input & "'");
         end if;

         declare
            Result : constant Result_Type := Warth_Left_Recurse_Expr_1_Main.Parse (Parser);
         begin
            Check (Input, Result.State, Expected_State);
            if Expected_State = Success then
               Execute_Actions (Parser.Tree, Parser.User_Data, Parser.Trace);
               Check ("result", User_Data.Stack.Pop, Expected_Result);
            end if;
         end;
      exception
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
   end Test_Parse;

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
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Warth_Left_Recurse_Expr_1_Main.Create_Parser
        (Parser,
         Trace     => Trace'Access,
         User_Data => User_Data'Access);

      User_Data.Set_Lexer_Terminals (Parser.Lexer, Parser.Terminals'Access);
   end Set_Up_Case;

end Warth_Left_Recurse_Expr_1;
