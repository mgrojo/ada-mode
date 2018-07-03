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
with Warth_Left_Recurse_Expr_1_Packrat_Actions;
with Warth_Left_Recurse_Expr_1_Packrat_Main;
with Warth_Left_Recurse_Expr_1_Runtime;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Parse.Packrat.Generated;
with WisiToken.Parse.Packrat.Procedural;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada;
package body Warth_Left_Recurse_Expr_1 is

   User_Data : aliased Warth_Left_Recurse_Expr_1_Runtime.User_Data_Type;

   Parser_Gen : aliased WisiToken.Parse.Packrat.Generated.Parser;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Warth_Left_Recurse_Expr_1_Packrat_Actions.Descriptor'Access);

   ----------
   --  Test procedures

   procedure Test_Parse_Gen (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Parse.Packrat.Generated;

      procedure Execute_Parse
        (Input           : in String;
         Expected_State  : in WisiToken.Parse.Packrat.Generated.Result_States;
         Expected_Result : in Integer)
      is
         use AUnit.Checks;
      begin
         Parser_Gen.Lexer.Reset_With_String (Input);
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("input: '" & Input & "'");
         end if;

         Parser_Gen.Parse;

         AUnit.Assertions.Assert (Expected_State = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

         Parser_Gen.Execute_Actions;
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
      User_Data.Set_Lexer_Terminals (Parser_Gen.Lexer, Parser_Gen.Terminals'Access);

      Execute_Parse ("1 - 3", Success, -2);
      Execute_Parse ("1", Success, 1);
      Execute_Parse ("3 - 2 - 1", Success, 0);
      Execute_Parse ("3 -", Failure, 0);
   end Test_Parse_Gen;

   procedure Test_Parse_Proc (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use WisiToken.Wisi_Ada;
      use all type WisiToken.Parse.Packrat.Procedural.Result_States;

      type Token_Enum_ID is (Whitespace_ID, Minus_ID, Numeric_Literal_ID, EOF_ID, Expr_ID, Accept_ID);

      package Tokens is new WisiToken.Gen_Token_Enum
        (Token_Enum_ID     => Token_Enum_ID,
         First_Terminal    => Minus_ID,
         Last_Terminal     => EOF_ID,
         First_Nonterminal => Expr_ID,
         Last_Nonterminal  => Accept_ID,
         EOF_ID            => EOF_ID,
         Accept_ID         => Accept_ID,
         Case_Insensitive  => False);
      use Tokens;

      Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
        Accept_ID <= Expr_ID & EOF_ID + WisiToken.Syntax_Trees.Null_Action
        and
        (Expr_ID <=
           Expr_ID & Minus_ID & Numeric_Literal_ID + Warth_Left_Recurse_Expr_1_Packrat_Actions.expr_0'Access
           or
           Numeric_Literal_ID + Warth_Left_Recurse_Expr_1_Packrat_Actions.expr_1'Access);

      Parser : WisiToken.Parse.Packrat.Procedural.Parser
        (First_Nonterminal => +Expr_ID,
         Last_Nonterminal  => +Accept_ID);

      procedure Execute_Parse
        (Input           : in String;
         Expected_State  : in WisiToken.Parse.Packrat.Procedural.Result_States;
         Expected_Result : in Integer)
      is
         use AUnit.Checks;
      begin
         Parser.Lexer.Reset_With_String (Input);
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("input: '" & Input & "'");
         end if;

         Parser.Parse;

         AUnit.Assertions.Assert (Expected_State = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

         Parser.Execute_Actions;
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
      Parser.Create
        (Trace     => Trace'Access,
         Lexer     => Parser_Gen.Lexer,
         User_Data => User_Data'Access,
         Grammar   => Grammar,
         Start_ID  => +Accept_ID);

      User_Data.Set_Lexer_Terminals (Parser.Lexer, Parser.Terminals'Unchecked_Access);

      declare
         Expected : WisiToken.Token_ID_Set (+Expr_ID .. +Accept_ID) := (others => False);
      begin
         Expected (+Expr_ID) := True;

         Check ("direct_left_recursive", Parser.Direct_Left_Recursive, Expected);
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

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register; Parser_Gen.Lexer is also used by Test_Parse_Proc
      Warth_Left_Recurse_Expr_1_Packrat_Main.Create_Parser
        (Parser_Gen,
         Trace     => Trace'Access,
         User_Data => User_Data'Access);
   end Set_Up_Case;

end Warth_Left_Recurse_Expr_1;
