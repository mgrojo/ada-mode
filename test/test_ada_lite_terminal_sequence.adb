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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Wisi.Declarations;
with Wisi.Gen_Generate_Utils;
with Wisi.Prologue;
with Wisi.Rules;
with WisiToken.AUnit;
with WisiToken.LR.Generator_Utils;
with WisiToken.Production;
with WisiToken.Syntax_Trees;
package body Test_Ada_Lite_Terminal_Sequence is

   ----------
   --  Test procedures

   procedure Test_Terminal_Sequence (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use AUnit.Checks.Containers;

      use Ada.Text_IO;
      Input_File_Name  : constant String := "../wisi/test/ada_lite.wy";
      Input_File       : File_Type;
      Prologues        : Wisi.Prologues;
      pragma Unreferenced (Prologues); -- They must be read before the rest of the file.
      Generate_Params  : Wisi.Generate_Param_Type;
      Tokens           : Wisi.Tokens;
      Conflicts        : Wisi.Conflict_Lists.List;
      McKenzie_Recover : Wisi.McKenzie_Recover_Param_Type;
      Elisp_Names      : Wisi.Elisp_Names;
      Rule_Count       : Integer;
      Action_Count     : Integer;
      Check_Count      : Integer;

      EOI_Name : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("Wisi_EOI");
      WisiToken_Accept_Name : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("wisitoken_accept");

   begin
      Open (Input_File, In_File, Input_File_Name);

      Wisi.Prologue (Input_File, Prologues);
      Wisi.Declarations (Input_File, Generate_Params, Tokens, Elisp_Names, Conflicts, McKenzie_Recover);
      Wisi.Rules
        (Input_File, Generate_Params.Output_Language, Generate_Params.Lexer, Tokens.Rules,
         Rule_Count, Action_Count, Check_Count);

      declare
         package Generate_Utils is new Wisi.Gen_Generate_Utils (Tokens, Conflicts, EOI_Name, WisiToken_Accept_Name);
         use Generate_Utils;

         Grammar : constant WisiToken.Production.List.Instance := To_Grammar
           (LALR_Descriptor, Input_File_Name, Start_Token => "compilation_unit");

         Computed : WisiToken.LR.Token_Sequence_Arrays.Vector;
         Sequence : WisiToken.Token_ID_Arrays.Vector;
      begin
         WisiToken.LR.Generator_Utils.Compute_Terminal_Sequences (Grammar, LALR_Descriptor, Computed);

         if WisiToken.Trace_Generate > WisiToken.Detail then
            Ada.Text_IO.New_Line;
            for I in Computed.First_Index .. Computed.Last_Index loop
               Ada.Text_IO.Put_Line
                 (WisiToken.Image (I, LALR_Descriptor) & " => " & WisiToken.Image (Computed (I), LALR_Descriptor));
            end loop;
         end if;

         --  We only check a couple things; the main test is that this runs in
         --  a reasonable time, and there are no exceptions.
         Check ("first", Computed.First_Index, Find_Token_ID ("wisitoken_accept"));
         Check ("last", Computed.Last_Index, Find_Token_ID ("unary_adding_operator"));

         Check ("empty 1", Computed (Find_Token_ID ("aspect_specification_opt")).Length, 0);

         Sequence.Append (Find_Token_ID ("IDENTIFIER"));
         Sequence.Append (Find_Token_ID ("COLON_EQUAL"));
         Sequence.Append (Find_Token_ID ("SEMICOLON"));
         Check ("assignment_statement", Computed (Find_Token_ID ("assignment_statement")), Sequence);
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
