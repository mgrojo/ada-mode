--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020, 2022 Stephen Leake.  All Rights Reserved.
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
with Dragon_4_43_Packrat_Gen_Main;
with WisiToken.Parse.Packrat;
with WisiToken.Text_IO_Trace;
package body Dragon_4_43_Packrat_Gen is

   Trace : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File : Ada.Text_IO.File_Type;

   Parser : aliased WisiToken.Parse.Base_Parser'Class := Dragon_4_43_Packrat_Gen_Main.Create_Parser
     (Trace'Access, User_Data => null);

   ----------
   --  Test procedures

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Parse.Packrat;

      procedure Execute_Parse
        (Input    : in String;
         Expected : in Result_States)
      is begin
         Parser.Tree.Lexer.Reset_With_String (Input);
         Parser.Parse (Log_File);

         if WisiToken.Trace_Tests > WisiToken.Detail then
            Trace.Put_Line ("ref_counts:");
            Parser.Tree.Print_Ref_Counts;
         end if;
         AUnit.Assertions.Assert (Expected = Success, "'" & Input & "': expected fail; did not get Syntax_Error");

      exception
      when WisiToken.Syntax_Error =>
         AUnit.Assertions.Assert (Expected = Failure, "'" & Input & "': expected success; got Syntax_Error");

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
      Execute_Parse ("cdcd", Success);
      Execute_Parse ("ccd", Failure);
      Execute_Parse ("ccdccd", Success);
      Execute_Parse ("d", Failure);
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

end Dragon_4_43_Packrat_Gen;
