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
with WisiToken.AUnit;
with WisiToken.Generate.Packrat;
with WisiToken.Parse.Packrat.Generated;
with WisiToken.Parse.Packrat.Procedural;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Warth_Left_Recurse_Expr_1 is

   User_Data : aliased Warth_Left_Recurse_Expr_1_Runtime.User_Data_Type;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Warth_Left_Recurse_Expr_1_Actions.Descriptor'Access);

   Parser_Gen : aliased WisiToken.Parse.Base_Parser'Class := Warth_Left_Recurse_Expr_1_Main.Create_Parser
     (Trace'Access, User_Data'Access);

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
      use Warth_Left_Recurse_Expr_1_Actions;
      use WisiToken.AUnit;
      use WisiToken.Productions;
      use all type WisiToken.Parse.Packrat.Procedural.Result_States;

      Grammar : WisiToken.Productions.Prod_Arrays.Vector;
   begin
      --  Build Grammar using container operations, to prototype wisi-generate_grammar.adb

      Grammar.Set_First (Descriptor.First_Nonterminal);
      Grammar.Set_Last (Descriptor.Last_Nonterminal);
      declare
         Prod : Instance;
      begin
         Prod.LHS := +wisitoken_accept_ID;
         Prod.RHSs.Set_First (0);
         Prod.RHSs.Set_Last (0);

         --  Accept_ID <= Expr_ID & EOF_ID + WisiToken.Syntax_Trees.Null_Action
         declare
            RHS : Right_Hand_Side;
         begin
            RHS.Tokens.Set_First (1);
            RHS.Tokens.Set_Last (2);
            RHS.Tokens (1) := +expr_ID;
            RHS.Tokens (2) := +Wisi_EOI_ID;
            --  leave RHS.action, check null

            Prod.RHSs (0) := RHS;
         end;

         Grammar (+wisitoken_accept_ID) := Prod;
      end;

      declare
         Prod : Instance;
      begin
         Prod.LHS := +expr_ID;
         Prod.RHSs.Set_First (0);
         Prod.RHSs.Set_Last (1);

         --  Expr_ID <= Expr_ID & Minus_ID & Numeric_Literal_ID + expr_0
         declare
            RHS : Right_Hand_Side;
         begin
            RHS.Tokens.Set_First (1);
            RHS.Tokens.Set_Last (3);
            RHS.Tokens (1) := +expr_ID;
            RHS.Tokens (2) := +MINUS_ID;
            RHS.Tokens (3) := +NUMERIC_LITERAL_ID;
            RHS.Action     := expr_0'Access;
            --  leave RHS.action, check null

            Prod.RHSs (0) := RHS;
         end;

         --  Expr_ID <= Numeric_Literal_ID + Warth_Left_Recurse_Expr_1_Packrat_Actions.expr_1'Access
         declare
            RHS : Right_Hand_Side;
         begin
            RHS.Tokens.Set_First (1);
            RHS.Tokens.Set_Last (1);
            RHS.Tokens (1) := +NUMERIC_LITERAL_ID;
            RHS.Action     := expr_1'Access;
            --  leave check null

            Prod.RHSs (1) := RHS;
         end;

         Grammar (+expr_ID) := Prod;
      end;

      declare
         Parser : WisiToken.Parse.Packrat.Procedural.Parser := WisiToken.Parse.Packrat.Procedural.Create
           (Trace     => Trace'Access,
            Lexer     => Parser_Gen.Lexer,
            User_Data => User_Data'Access,
            Grammar   => Grammar,
            Direct_Left_Recursive => WisiToken.Generate.Packrat.Potential_Direct_Left_Recursive
              (Grammar, WisiToken.Generate.Has_Empty_Production (Grammar)),
            Start_ID  => +wisitoken_accept_ID);

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

            AUnit.Assertions.Assert
              (Expected_State = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

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
         User_Data.Set_Lexer_Terminals (Parser.Lexer, Parser.Terminals'Unchecked_Access);

         declare
            Expected : WisiToken.Token_ID_Set (+wisitoken_accept_ID .. +expr_ID) := (others => False);
         begin
            Expected (+expr_ID) := True;

            Check ("direct_left_recursive", Parser.Direct_Left_Recursive, Expected);
         end;

         Execute_Parse ("1 - 3", Success, -2);
         Execute_Parse ("1", Success, 1);
         Execute_Parse ("3 - 2 - 1", Success, 0);
         Execute_Parse ("3 -", Failure, 0);
      end;
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
