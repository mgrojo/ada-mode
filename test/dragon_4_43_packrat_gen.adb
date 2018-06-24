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
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Dragon_4_43_Actions;
with Dragon_4_43_Main; --  FIXME: add 'packrat'? also produce LALR?
with WisiToken.Packrat.AUnit;
with WisiToken.Text_IO_Trace;
package body Dragon_4_43_Packrat_Gen is

   Parser : aliased Dragon_4_43_Main.Parser_Type;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Dragon_4_43_Actions.Descriptor'Access);
   --  FIXME: why does Trace need a Descriptor?

   ----------
   --  Test procedures

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Packrat;

      procedure Execute_Parse
        (Input    : in String;
         Expected : in WisiToken.Packrat.Result_States)
      is
         use WisiToken.Packrat.AUnit;
      begin
         Parser.Lexer.Reset_With_String (Input);
         declare
            Result : constant Result_Type := Dragon_4_43_Main.Parse (Parser, Parser.Terminals.First_Index);
         begin
            Check (Input, Result.State, Expected);
            --  FIXME: check syntax_tree, actions?
         end;
      exception
      when E : others =>
         Ada.Text_IO.Put (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         AUnit.Assertions.Assert (False, "'" & Input & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Parse;

   begin
      Execute_Parse ("cdcd", Success);
      Execute_Parse ("ccd", Failure);
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("dragon_4_43_packrat_gen.adb");
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
      Dragon_4_43_Main.Create_Parser
        (Parser,
         Trace     => Trace'Access,
         User_Data => null);
   end Set_Up_Case;

end Dragon_4_43_Packrat_Gen;
