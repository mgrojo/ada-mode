--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 - 2022 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with Ada_Lite_Actions;   use Ada_Lite_Actions;
with Ada_Lite_LALR_Main;
with GNATCOLL.Mmap;
with WisiToken.AUnit;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Test_Partial_Parse is

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;
   Trace : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File : Ada.Text_IO.File_Type;

   Parser : WisiToken.Parse.Parser.Parser'Class :=
     WisiToken.Parse.LR.Parser.New_Parser
       (Ada_Lite_LALR_Main.Create_Lexer (Trace'Access),
        Ada_Lite_LALR_Main.Create_Parse_Table,
        Ada_Lite_LALR_Main.Create_Productions,
        Language_Fixes                 => null,
        Language_Matching_Begin_Tokens => null,
        Language_String_ID_Set         => null,
        User_Data                      => User_Data'Access);

   procedure Run_Parse
     (Label              : in String;
      Begin_Byte_Pos     : in WisiToken.Buffer_Pos;
      Goal_Byte_Pos      : in WisiToken.Buffer_Pos;
      Begin_Char_Pos     : in WisiToken.Buffer_Pos;
      Begin_Line         : in WisiToken.Line_Number_Type;
      Parse_End_Byte_Pos : in WisiToken.Buffer_Pos;
      Action_ID          : in WisiToken.Token_ID)
   is
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees;

      procedure Finish
      is
         use all type WisiToken.Token_ID;
         Node  : Valid_Node_Access := Parser.Tree.Root;
      begin
         if WisiToken.Trace_Tests > WisiToken.Detail then
            Parser.Tree.Print_Tree (Line_Numbers => True);
         end if;

         Parser.Execute_Actions (Action_Region_Bytes => (Begin_Byte_Pos, Parse_End_Byte_Pos));

         if Action_ID = WisiToken.Invalid_Token_ID then
            --  Only parsed comments, no user compilation units. Recover provided
            --  code to reduce to Accept.

            Check (Label & ".root", Parser.Tree.ID (Node), Descriptor.Accept_ID);

         else
            Check (Label & ".root", Parser.Tree.ID (Node), +compilation_unit_list_ID);

            Node := Parser.Tree.Children (Node)(2); -- First child is SOI, second is compilation_unit
            Check (Label & ".compilation_unit", Parser.Tree.ID (Node), +compilation_unit_ID);

            Node := Parser.Tree.Children (Node)(1);
            Check (Label & ".action ID", Parser.Tree.ID (Node), Action_ID);

            Check (Label & ".parse begin byte",
                   Parser.Tree.Byte_Region (Node, Trailing_Non_Grammar => False).First,
                   Begin_Byte_Pos);
            Check (Label & ".parse begin char",
                   Parser.Tree.Char_Region (Node, Trailing_Non_Grammar => False).First,
                   Begin_Char_Pos);
            Check (Label & ".parse begin line",
                   Parser.Tree.Line_Region (Node, Trailing_Non_Grammar => True).First,
                   Begin_Line);

            Check
              (Label & ".parse end byte",
               Parser.Tree.Byte_Region (Parser.Tree.Last_Terminal (Node), Trailing_Non_Grammar => False).Last,
               Parse_End_Byte_Pos);
         end if;

         if WisiToken.Trace_Action > WisiToken.Detail then
            Parser.Put_Errors;
         end if;
      end Finish;

   begin
      Partial_Parse_Active    := True;
      Partial_Parse_Byte_Goal := Goal_Byte_Pos;

      Parser.LR_Parse (Log_File);

      --  If the partial parse reaches the end of the input, a normal Accept
      --  occurs; no Partial_Parse exception.
      Finish;
   exception
   when WisiToken.Partial_Parse =>
      Finish;

   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Action > WisiToken.Detail then
         Parser.Put_Errors;
      end if;
      raise;
   end Run_Parse;

   procedure Parse_Text
     (Label              : in String;
      Text               : in String;
      Begin_Byte_Pos     : in Integer;
      End_Byte_Pos       : in Integer;
      Goal_Byte_Pos      : in WisiToken.Buffer_Pos;
      Begin_Char_Pos     : in WisiToken.Buffer_Pos;
      Begin_Line         : in WisiToken.Line_Number_Type;
      Parse_End_Byte_Pos : in WisiToken.Buffer_Pos;
      Action_ID          : in WisiToken.Token_ID)
   is
      use WisiToken;
      Partial_Text : String renames Text (Begin_Byte_Pos .. End_Byte_Pos);
   begin
      if WisiToken.Trace_Tests > WisiToken.Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input: '" & Partial_Text & "'," &
              Integer'Image (Begin_Byte_Pos) & " .." & Integer'Image (End_Byte_Pos));
      end if;

      Parser.Tree.Lexer.Reset_With_String (Partial_Text, Begin_Char_Pos, Begin_Line);
      Run_Parse
        (Label, Buffer_Pos (Begin_Byte_Pos), Goal_Byte_Pos, Begin_Char_Pos, Begin_Line, Parse_End_Byte_Pos, Action_ID);
   end Parse_Text;

   procedure Parse_File
     (Label              : in String;
      File_Name          : in String;
      Begin_Byte_Pos     : in WisiToken.Buffer_Pos;
      End_Byte_Pos       : in WisiToken.Buffer_Pos;
      Goal_Byte_Pos      : in WisiToken.Buffer_Pos;
      Begin_Char_Pos     : in WisiToken.Buffer_Pos;
      Begin_Line         : in WisiToken.Line_Number_Type;
      Parse_End_Byte_Pos : in WisiToken.Buffer_Pos;
      Action_ID          : in WisiToken.Token_ID)
   is
      use WisiToken;
   begin
      if WisiToken.Trace_Parse > WisiToken.Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input file: " & File_Name & Buffer_Pos'Image (Begin_Byte_Pos) & " .." & Buffer_Pos'Image (End_Byte_Pos));
      end if;

      Parser.Tree.Lexer.Reset_With_File (File_Name, Begin_Byte_Pos, End_Byte_Pos, Begin_Char_Pos, Begin_Line);
      Run_Parse (Label, Begin_Byte_Pos, Goal_Byte_Pos, Begin_Char_Pos, Begin_Line, Parse_End_Byte_Pos, Action_ID);
   end Parse_File;

   procedure Parse_String_Access
     (Label              : in String;
      File_Name          : in String;
      Begin_Byte_Pos     : in WisiToken.Buffer_Pos;
      End_Byte_Pos       : in WisiToken.Buffer_Pos;
      Goal_Byte_Pos      : in WisiToken.Buffer_Pos;
      Begin_Char_Pos     : in WisiToken.Buffer_Pos;
      Begin_Line         : in WisiToken.Line_Number_Type;
      Parse_End_Byte_Pos : in WisiToken.Buffer_Pos;
      Action_ID          : in WisiToken.Token_ID)
   is
      use GNATCOLL.Mmap;
      use WisiToken;
      File   : constant Mapped_File   := Open_Read (File_Name);
      Region : constant Mapped_Region := Read (File);

      Buffer : aliased String := Data (Region)(Integer (Begin_Byte_Pos) .. Integer (End_Byte_Pos));
   begin
      if WisiToken.Trace_Parse > WisiToken.Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("input file: " & File_Name & Buffer_Pos'Image (Begin_Byte_Pos) & " .." & Buffer_Pos'Image (End_Byte_Pos));
      end if;

      Parser.Tree.Lexer.Reset_With_String_Access
        (Buffer'Unchecked_Access, Buffer'Last, +File_Name, Begin_Char_Pos, Begin_Line);
      Run_Parse (Label, Begin_Byte_Pos, Goal_Byte_Pos, Begin_Char_Pos, Begin_Line, Parse_End_Byte_Pos, Action_ID);
   end Parse_String_Access;

   type Test_Choice is (File, String_Access);

   procedure Parse_Choice
     (Choice             : in Test_Choice;
      Label              : in String;
      File_Name          : in String;
      Begin_Byte_Pos     : in WisiToken.Buffer_Pos;
      End_Byte_Pos       : in WisiToken.Buffer_Pos;
      Goal_Byte_Pos      : in WisiToken.Buffer_Pos;
      Begin_Char_Pos     : in WisiToken.Buffer_Pos;
      Begin_Line         : in WisiToken.Line_Number_Type;
      Parse_End_Byte_Pos : in WisiToken.Buffer_Pos;
      Action_ID          : in WisiToken.Token_ID)
   is begin
      case Choice is
      when File =>
         Parse_File
           (Label & ".File", File_Name, Begin_Byte_Pos, End_Byte_Pos, Goal_Byte_Pos, Begin_Char_Pos, Begin_Line,
            Parse_End_Byte_Pos, Action_ID);
      when String_Access =>
         Parse_String_Access
           (Label & ".String_Access", File_Name, Begin_Byte_Pos, End_Byte_Pos, Goal_Byte_Pos,
            Begin_Char_Pos, Begin_Line, Parse_End_Byte_Pos, Action_ID);
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
      Parse_Text
        (Label              => "1",
         Text               => Source,
         Begin_Byte_Pos     => 1,
         End_Byte_Pos       => 16,
         Goal_Byte_Pos      => 5,
         Begin_Char_Pos     => 1,
         Begin_Line         => 1,
         Parse_End_Byte_Pos => 7,
         Action_ID          => +statement_ID);

      Parse_Text ("2", Source, 9, 24, 14, 9, 2, 16, +statement_ID);

      Parse_Text ("3", Source, 18, 24, 21, 17, 3, 24, +statement_ID);
   end Plain_String;

   procedure File_String_Access (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      for Choice in Test_Choice loop
         --  Header comment
         Parse_Choice (Choice, "0", "../test/bnf/ada_lite.input", 1, 160, 160, 1, 1, 160, WisiToken.Invalid_Token_ID);

         --  πroc_1 spec
         Parse_Choice
           (Choice, "1", "../test/bnf/ada_lite.input", 161, 200, 175, 161, 4, 178, +subprogram_declaration_ID);

         --  πroc_1 body and Func_1 spec
         Parse_Choice (Choice, "2", "../test/bnf/ada_lite.input", 181, 350, 201, 180, 6, 252, +subprogram_body_ID);
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

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Partial_Parse_Active := False;
   end Tear_Down_Case;

end Test_Partial_Parse;
