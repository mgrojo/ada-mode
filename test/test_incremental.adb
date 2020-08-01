--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with WisiToken.Parse.LR;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Syntax_Trees.AUnit_Public;
package body Test_Incremental is
   use Ada_Lite_Actions;

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser;

   Expected_User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;
   Expected_Parser    : WisiToken.Parse.LR.Parser.Parser;

   procedure Parse_Text
     (Initial   : in String;
      Delete_At : in Integer;
      Delete    : in String;
      Insert_At : in Integer;
      Insert    : in String)
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use WisiToken.Syntax_Trees.AUnit_Public;

      Initial_First  : Integer := Initial'First;
      Initial_Last   : Integer := Initial'First - 1;

      Expected : String (Initial'First .. Initial'Last + Insert'Length);
      Expected_First  : Integer := Expected'First;
      Expected_Last   : Integer := Expected'First - 1;

   begin
      if Delete_At in Initial'Range then
         Expected_Last  := Delete_At - 1;
         Initial_Last := Expected_Last;

         Expected (Expected_First .. Expected_Last) := Initial (Initial_First .. Initial_Last);

         Initial_Last := Delete_At + Delete'Length - 1;

         if Initial (Delete_At .. Initial_Last) /= Delete then
            AUnit.Assertions.Assert (False, "invalid delete");
         end if;
      end if;

      if Insert_At in Initial'Range then
         Expected_First := Expected_Last + 1;
         Expected_Last := Expected_Last + Insert'Length;

         Expected (Expected_First .. Expected_Last) := Insert;
      end if;

      Initial_First := Initial_Last + 1;
      Initial_Last := Initial'Last;
      if Initial_First <= Initial_Last then
         Expected_First := Expected_Last + 1;
         Expected_Last  := Expected_Last + Initial_Last - Initial_First + 1;

         Expected (Expected_First .. Expected_Last) := Initial (Initial_First .. Initial_Last);
      end if;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("initial: '" & Initial & "'");
         Ada.Text_IO.Put_Line ("edited : '" & Expected (Expected'First .. Expected_Last) & "'");
      end if;

      Parser.Lexer.Reset_With_String (Initial);

      Parser.Parse;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors;

         Put_Line ("initial terminals:" & Parser.Tree.Image (Parser.Tree.Terminal_Stream, Descriptor));
      end if;

      --  FIXME: apply edit to Parser

      Expected_Parser.Lexer.Reset_With_String (Expected (Expected'First .. Expected_Last));

      Expected_Parser.Parse;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Expected_Parser.Put_Errors;

         Put_Line ("edited terminals  :" & Parser.Tree.Image (Parser.Tree.Terminal_Stream, Descriptor));
         Put_Line
           ("expected terminals:" & Expected_Parser.Tree.Image (Expected_Parser.Tree.Terminal_Stream, Descriptor));
      end if;

      Check ("terminals", Parser.Tree, Parser.Tree.Terminal_Stream, Expected_Parser.Tree,
             Expected_Parser.Tree.Terminal_Stream);
      Check ("syntax_tree", Parser.Tree, Expected_Parser.Tree);

   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", True, False);
   end Parse_Text;

   ----------
   --  Test procedures

   procedure No_Change (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial   => "A := B + C;",
         Delete_At => 0,
         Delete    => "",
         Insert_At => 0,
         Insert    => "");
   end No_Change;

   procedure Edit_Comment (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial => "A := B + C; --  A comment",
         --          1        |10       |20
         Delete_At => 19,
         Delete  => "comment",
         Insert_At => 19,
         Insert  => "cool explanation");
   end Edit_Comment;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Edit_Comment'Access, "Edit_Comment");

   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_incremental.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      Ada_Lite_LR1_T1_Main.Create_Parser
        (Parser,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         Trace                          => Trace'Access,
         User_Data                      => User_Data'Access,
         Text_Rep_File_Name             => "ada_lite_lr1_t1_re2c_parse_table.txt");

      Ada_Lite_LR1_T1_Main.Create_Parser
        (Expected_Parser,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         Trace                          => Trace'Access,
         User_Data                      => Expected_User_Data'Access,
         Text_Rep_File_Name             => "ada_lite_lr1_t1_re2c_parse_table.txt");
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      null;
   end Set_Up;

end Test_Incremental;
--  Local Variables:
--  ada-case-strict: nil
--  End:
