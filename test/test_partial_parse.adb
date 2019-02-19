--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with Ada.Text_IO;
with Ada_Lite_Actions;   use Ada_Lite_Actions;
with Ada_Lite_LALR_Main; use Ada_Lite_LALR_Main;
with GNATCOLL.Mmap;
with WisiToken.AUnit;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
package body Test_Partial_Parse is


   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser;

   procedure Run_Parse
     (Label      : in String;
      Begin_Pos  : in WisiToken.Buffer_Pos;
      End_Pos    : in WisiToken.Buffer_Pos;
      Begin_Char : in WisiToken.Buffer_Pos;
      Begin_Line : in WisiToken.Line_Number_Type;
      Action_ID  : in WisiToken.Token_ID)
   is
      use AUnit.Checks;
      use WisiToken.AUnit;

      procedure Finish
      is
         Node : WisiToken.Syntax_Trees.Valid_Node_Index;
      begin
         Parser.Execute_Actions;

         Node := Parser.Tree.Root;
         Check (Label & ".root", Parser.Tree.ID (Node), +compilation_unit_list_ID);

         Node := Parser.Tree.Children (Node)(1); -- First child is compilation_unit
         Check (Label & ".compilation_unit", Parser.Tree.ID (Node), +compilation_unit_ID);

         Node := Parser.Tree.Children (Node)(1);
         Check (Label & ".parsed ID", Parser.Tree.ID (Node), Action_ID);

         Check
           (Label & ".begin_pos",
            Parser.Terminals (Parser.Tree.Min_Terminal_Index (Node)).Byte_Region.First,
            Begin_Pos);

         Check
           (Label & ".end_pos",
            Parser.Terminals (Parser.Tree.Max_Terminal_Index (Node)).Byte_Region.Last,
            End_Pos);

         Check
           (Label & ".begin_char",
            Parser.Terminals (Parser.Tree.Min_Terminal_Index (Node)).Char_Region.First,
            Begin_Char);

         Check
           (Label & ".begin_line",
            Parser.Terminals (Parser.Tree.Min_Terminal_Index (Node)).Line,
            Begin_Line);

         Check (Label & ".action_count", Action_Count (Action_ID), 1);

         if WisiToken.Trace_Action > WisiToken.Outline then
            Parser.Put_Errors;
         end if;
      end Finish;

   begin
      Action_Count := (others => 0);

      Partial_Parse_Active := True;

      Parser.Parse;

      --  If the partial parse reaches the end of the input, a normal Accept
      --  occurs; no Partial_Parse exception.
      Finish;
   exception
   when WisiToken.Partial_Parse =>
      Finish;

   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors;
      end if;
      raise;
   end Run_Parse;

   procedure Parse_Text
     (Label      : in String;
      Text       : in String;
      Begin_Pos  : in Integer;
      End_Pos    : in Integer;
      Begin_Char : in WisiToken.Buffer_Pos;
      Begin_Line : in WisiToken.Line_Number_Type;
      Action_ID  : in WisiToken.Token_ID)
   is
      use WisiToken;
      Partial_Text : String renames Text (Begin_Pos .. End_Pos);
   begin
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input: '" & Partial_Text & "'," &
              Integer'Image (Begin_Pos) & " .." & Integer'Image (End_Pos));
      end if;

      Parser.Lexer.Reset_With_String (Partial_Text, Begin_Char, Begin_Line);
      Run_Parse (Label, Buffer_Pos (Begin_Pos), Buffer_Pos (End_Pos), Begin_Char, Begin_Line, Action_ID);
   end Parse_Text;

   procedure Parse_File
     (Label      : in String;
      File_Name  : in String;
      Begin_Pos  : in WisiToken.Buffer_Pos;
      End_Pos    : in WisiToken.Buffer_Pos;
      Begin_Char : in WisiToken.Buffer_Pos;
      Begin_Line : in WisiToken.Line_Number_Type;
      Action_ID  : in WisiToken.Token_ID)
   is
      use WisiToken;
   begin
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input file: " & File_Name & Buffer_Pos'Image (Begin_Pos) & " .." & Buffer_Pos'Image (End_Pos));
      end if;

      Parser.Lexer.Reset_With_File (File_Name, Begin_Pos, End_Pos, Begin_Char, Begin_Line);
      Run_Parse (Label, Begin_Pos, End_Pos, Begin_Char, Begin_Line, Action_ID);
   end Parse_File;

   procedure Parse_String_Access
     (Label      : in String;
      File_Name  : in String;
      Begin_Pos  : in WisiToken.Buffer_Pos;
      End_Pos    : in WisiToken.Buffer_Pos;
      Begin_Char : in WisiToken.Buffer_Pos;
      Begin_Line : in WisiToken.Line_Number_Type;
      Action_ID  : in WisiToken.Token_ID)
   is
      use GNATCOLL.Mmap;
      use WisiToken;
      File   : constant Mapped_File   := Open_Read (File_Name);
      Region : constant Mapped_Region := Read (File);

      Buffer : aliased String := Data (Region)(Integer (Begin_Pos) .. Integer (End_Pos));
   begin
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input file: " & File_Name & Buffer_Pos'Image (Begin_Pos) & " .." & Buffer_Pos'Image (End_Pos));
      end if;

      Parser.Lexer.Reset_With_String_Access (Buffer'Unchecked_Access, +File_Name, Begin_Char, Begin_Line);
      Run_Parse (Label, Begin_Pos, End_Pos, Begin_Char, Begin_Line, Action_ID);
   end Parse_String_Access;

   type Test_Choice is (File, String_Access);

   procedure Parse_Choice
     (Choice     : in Test_Choice;
      Label      : in String;
      File_Name  : in String;
      Begin_Pos  : in WisiToken.Buffer_Pos;
      End_Pos    : in WisiToken.Buffer_Pos;
      Begin_Char : in WisiToken.Buffer_Pos;
      Begin_Line : in WisiToken.Line_Number_Type;
      Action_ID  : in WisiToken.Token_ID)
   is begin
      case Choice is
      when File =>
         Parse_File (Label, File_Name, Begin_Pos, End_Pos, Begin_Char, Begin_Line, Action_ID);
      when String_Access =>
         Parse_String_Access (Label, File_Name, Begin_Pos, End_Pos, Begin_Char, Begin_Line, Action_ID);
      end case;
   end Parse_Choice;

   ----------
   --  Test procedures

   procedure Plain_String (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Source : constant String := "A := 1; π := 2; C := 3;";
      --  char                     1        |10       |20
      --  byte                     1        |11       |21
   begin
      Parse_Text ("1", Source, 1, 7, 1, 1, +statement_ID);

      Parse_Text ("2", Source, 9, 16, 9, 2, +statement_ID);

      Parse_Text ("3", Source, 18, 24, 17, 3, +statement_ID);
   end Plain_String;

   procedure File_String_Access (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Comments not included in terminal tokens, so can't start at 1.

      for Choice in Test_Choice loop
         Parse_Choice (Choice, "1", "../Test/bnf/ada_lite.input", 161, 178, 161, 4, +subprogram_declaration_ID);

         Parse_Choice (Choice, "2", "../Test/bnf/ada_lite.input", 181, 252, 180, 6, +subprogram_body_ID);
      end loop;
   end File_String_Access;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Plain_String'Access, "Plain_String");
      Register_Routine (T, File_String_Access'Access, "File_String_Access");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_partial_parse.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Create_Parser
        (Parser,
         Language_Fixes                        => null,
         Language_Use_Minimal_Complete_Actions => null,
         Language_String_ID_Set                => null,
         Trace                                 => Trace'Access,
         User_Data                             => User_Data'Access);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Partial_Parse_Active := False;
   end Tear_Down_Case;

end Test_Partial_Parse;
