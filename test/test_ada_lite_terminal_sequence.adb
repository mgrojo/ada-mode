--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009-2010, 2012-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks.Containers;
with Ada.Text_IO;
with Wisi.Generate_Utils;
with WisiToken.AUnit;
with WisiToken.Generate.LR;
with WisiToken.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Grammar_Runtime;
with Wisi_Grammar_Actions;
with Wisi_Grammar_Main;
package body Test_Ada_Lite_Terminal_Sequence is

   ----------
   --  Test procedures

   procedure Test_Terminal_Sequence (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use WisiToken.AUnit.Token_ID_Arrays_AUnit;
      use AUnit.Checks.Containers;

      Input_File_Name  : constant String := "../wisi/test/ada_lite.wy";

      Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisi_Grammar_Actions.Descriptor'Access);
      Input_Data     : aliased WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Grammar_Parser : WisiToken.LR.Parser_No_Recover.Parser;
   begin
      Wisi_Grammar_Main.Create_Parser (Grammar_Parser, Trace'Unchecked_Access, Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (Input_File_Name);
      Grammar_Parser.Parse;
      Input_Data.User_Parser := Wisi.LALR;
      Grammar_Parser.Execute_Actions;

      declare
         use Wisi.Generate_Utils;

         Generate_Data : aliased constant Wisi.Generate_Utils.Generate_Data := Initialize (Input_Data);

         Computed : WisiToken.Token_Sequence_Arrays.Vector;
         Sequence : WisiToken.Token_ID_Arrays.Vector;
      begin
         WisiToken.Generate.LR.Compute_Minimal_Terminal_Sequences
           (Generate_Data.Grammar, Generate_Data.Descriptor.all, Computed);

         if WisiToken.Trace_Generate > WisiToken.Detail then
            Ada.Text_IO.New_Line;
            for I in Computed.First_Index .. Computed.Last_Index loop
               Ada.Text_IO.Put_Line
                 (WisiToken.Image (I, Generate_Data.Descriptor.all) & " => " &
                    WisiToken.Image (Computed (I), Generate_Data.Descriptor.all));
            end loop;
         end if;

         --  We only check a couple things; the main test is that this runs in
         --  a reasonable time, and there are no exceptions.
         Check ("first", Computed.First_Index, Find_Token_ID (Generate_Data, "wisitoken_accept"));
         Check ("last", Computed.Last_Index, Find_Token_ID (Generate_Data, "unary_adding_operator"));

         Check ("empty 1", Computed (Find_Token_ID (Generate_Data, "aspect_specification_opt")).Length, 0);

         Sequence.Append (Find_Token_ID (Generate_Data, "IDENTIFIER"));
         Sequence.Append (Find_Token_ID (Generate_Data, "COLON_EQUAL"));
         Sequence.Append (Find_Token_ID (Generate_Data, "SEMICOLON"));
         Check ("assignment_statement", Computed (Find_Token_ID (Generate_Data, "assignment_statement")), Sequence);
      end;
   end Test_Terminal_Sequence;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Terminal_Sequence'Access, "Test_Terminal_Sequence");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_ada_lite_terminal_sequence.adb");
   end Name;

end Test_Ada_Lite_Terminal_Sequence;
