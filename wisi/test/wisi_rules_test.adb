--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks;
with Ada.Containers;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Rules;
package body Wisi_Rules_Test is

   File : File_Type;

   procedure Delete (Name : in String)
   is
      use Ada.Directories;
   begin
      if Is_Open (File) then
         Close (File);
      end if;
      if Exists (Name) then
         Delete_File (Name);
      end if;
   end Delete;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Containers.Count_Type);

   procedure Check
     (Label    : in String;
      Computed : in Wisi.String_Lists.List;
      Expected : in Wisi.String_Lists.List)
   is
      use Wisi.String_Lists;
      use AUnit.Checks;
      Computed_I : Cursor := Computed.First;
      Expected_I : Cursor := Expected.First;
   begin
      Check (Label & ".length", Computed.Length, Expected.Length);

      for I in 1 .. Computed.Length loop
         declare
            Label_I : constant String := Label & " (" & Ada.Containers.Count_Type'Image (I) & ")";
         begin
            Check
              (Label_I,
               Computed.Constant_Reference (Computed_I),
               Expected.Constant_Reference (Expected_I));
         end;
         Next (Computed_I);
         Next (Expected_I);
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Wisi.RHS_Lists.List;
      Expected : in Wisi.RHS_Lists.List)
   is
      use Wisi.RHS_Lists;
      Computed_I : Cursor := Computed.First;
      Expected_I : Cursor := Expected.First;
   begin
      Check (Label & ".length", Computed.Length, Expected.Length);

      for I in 1 .. Computed.Length loop
         declare
            Label_I : constant String := Label & " (" & Ada.Containers.Count_Type'Image (I) & ")";
         begin
            Check
              (Label_I & ".Production",
               Computed.Constant_Reference (Computed_I).Production,
               Expected.Constant_Reference (Expected_I).Production);
            Check
              (Label_I & ".Action",
               Computed.Constant_Reference (Computed_I).Action,
               Expected.Constant_Reference (Expected_I).Action);
         end;
         Next (Computed_I);
         Next (Expected_I);
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Wisi.Rule_Lists.List;
      Expected : in Wisi.Rule_Lists.List)
   is
      use AUnit.Checks;
      use Wisi;
      use Wisi.Rule_Lists;
      Computed_I : Cursor := Computed.First;
      Expected_I : Cursor := Expected.First;
   begin
      Check (Label & ".length", Computed.Length, Expected.Length);

      for I in 1 .. Computed.Length loop
         declare
            Label_I : constant String := Label & " (" & Ada.Containers.Count_Type'Image (I) & ")";
         begin
            Check
              (Label_I & ".LHS",
               -Computed.Constant_Reference (Computed_I).Left_Hand_Side,
               -Expected.Constant_Reference (Expected_I).Left_Hand_Side);
            Check
              (Label_I & ".RHS",
               Computed.Constant_Reference (Computed_I).Right_Hand_Sides,
               Expected.Constant_Reference (Expected_I).Right_Hand_Sides);
         end;
         Next (Computed_I);
         Next (Expected_I);
      end loop;
   end Check;

   ----------
   --  Test procedures

   procedure Nominal_Elisp (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test);
      use Wisi;

      File_Name : constant String := "wisi_rules_test-nominal_elisp.wy";

      Computed     : Wisi.Rule_Lists.List;
      Expected     : Wisi.Rule_Lists.List;
      Rule_Count   : Integer;
      Action_Count : Integer;
      Check_Count  : Integer;
   begin
      Delete (File_Name);
      Create (File, Out_File, File_Name);
      Put_Line (File, "declarations");
      Put_Line (File, "  : declaration");
      Put_Line (File, "  | declarations declaration");
      Put_Line (File, "  ;");
      New_Line (File);
      Put_Line (File, "subprogram");
      Put_Line (File, "  : FUNCTION parameter_list SYMBOL");
      Put_Line (File, "    (wisi-statement-action [1 function 2 other])");
      Put_Line (File, "  | PROCEDURE parameter_list SYMBOL");
      Put_Line (File, "    (wisi-cache-action");
      Put_Line (File, "       1 'procedure");
      Put_Line (File, "       2 'other)");
      Put_Line (File, "  ;");
      Put_Line (File, "%%");
      Close (File);

      Open (File, In_File, File_Name);
      Wisi.Rules (File, Wisi.Elisp, Wisi.Elisp_Lexer, Computed, Rule_Count, Action_Count, Check_Count);
      Close (File);

      Expected.Append
        ((Left_Hand_Side   => +"declarations",
          Right_Hand_Sides =>
            +(Production   => +"declaration",
              Action       => String_Lists.Empty_List,
              Check        => String_Lists.Empty_List,
              Source_Line  => 1) +
              (+"declarations" + "declaration",
               String_Lists.Empty_List, String_Lists.Empty_List, 2)));

      Expected.Append
        ((+"subprogram",
          +(+"FUNCTION" + "parameter_list" + "SYMBOL",
            +"(wisi-statement-action [1 function 2 other])", String_Lists.Empty_List, 1) +
            (+"PROCEDURE" + "parameter_list" + "SYMBOL",
             +"(wisi-cache-action" +
               "1 'procedure" +
               "2 'other)",
             String_Lists.Empty_List, 2)));

      Check ("1", Computed, Expected);

   end Nominal_Elisp;

   procedure Nominal_Ada (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test);
      use Wisi;

      File_Name : constant String := "wisi_rules_test-nominal_ada.wy";

      Computed     : Wisi.Rule_Lists.List;
      Expected     : Wisi.Rule_Lists.List;
      Rule_Count   : Integer;
      Action_Count : Integer;
      Check_Count  : Integer;
   begin
      Delete (File_Name);
      Create (File, Out_File, File_Name);
      Put_Line (File, ";; empty production, comments");
      Put_Line (File, "parameter_list");
      Put_Line (File, "  : LEFT_PAREN RIGHT_PAREN ;; c-like no parameters");
      Put_Line (File, "   (c_null_params := ");
      Put_Line (File, "      c_null_params + 1;)");
      Put_Line (File, "  | ;; ada-like no parameters");
      Put_Line (File, "   (ada_null_params := c_null_params + 1;)");
      Put_Line (File, "  | LEFT_PAREN SYMBOL RIGHT_PAREN");
      Put_Line (File, "  ;");
      Put_Line (File, "%%");
      Close (File);

      Open (File, In_File, File_Name);
      Wisi.Rules (File, Wisi.Ada, Wisi.re2c_Lexer, Computed, Rule_Count, Action_Count, Check_Count);
      Close (File);

      Wisi.Rule_Lists.Append
        (Expected,
         (Left_Hand_Side   => +"parameter_list",
          Right_Hand_Sides =>
            +(+"LEFT_PAREN" + "RIGHT_PAREN", +"c_null_params :=" + "c_null_params + 1;", String_Lists.Empty_List, 1) +
            (String_Lists.Empty_List, +"ada_null_params := c_null_params + 1;", String_Lists.Empty_List, 2) +
            (+"LEFT_PAREN" + "SYMBOL" + "RIGHT_PAREN", String_Lists.Empty_List, String_Lists.Empty_List, 3)));

      Check ("1", Computed, Expected);

   end Nominal_Ada;

   procedure Single_Line (Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Test);
      use Wisi;

      File_Name : constant String := "wisi_rules_test-single_line.wy";

      Computed     : Wisi.Rule_Lists.List;
      Expected     : Wisi.Rule_Lists.List;
      Rule_Count   : Integer;
      Action_Count : Integer;
      Check_Count  : Integer;
   begin
      Delete (File_Name);
      Create (File, Out_File, File_Name);
      Put_Line (File, "library_item : subprogram_body | generic_instantiation  ;");
      Put_Line (File, ";; some on one line");
      Put_Line (File, "subprogram_specification");
      Put_Line (File, "  : PROCEDURE IDENTIFIER parameter_list  ;");
      New_Line (File);
      Put_Line (File, ";; all on one line, empty production");
      Put_Line (File, "parameter_list : | LEFT_PAREN IDENTIFIER RIGHT_PAREN ;");
      Put_Line (File, "%%");
      Close (File);

      Open (File, In_File, File_Name);
      Wisi.Rules (File, Wisi.Elisp, Wisi.Elisp_Lexer, Computed, Rule_Count, Action_Count, Check_Count);
      Close (File);

      Wisi.Rule_Lists.Append
        (Expected,
         (Left_Hand_Side   => +"library_item",
          Right_Hand_Sides =>
            +(Production   => +"subprogram_body",
              Action       => String_Lists.Empty_List,
              Check        => String_Lists.Empty_List,
              Source_Line  => 1) +
              (+"generic_instantiation",
               String_Lists.Empty_List,
               String_Lists.Empty_List,
               2)));

      Wisi.Rule_Lists.Append
        (Expected,
         (+"subprogram_specification",
          +(+"PROCEDURE" + "IDENTIFIER" + "parameter_list", String_Lists.Empty_List, String_Lists.Empty_List, 1)));

      Wisi.Rule_Lists.Append
        (Expected,
         (Left_Hand_Side   => +"parameter_list",
          Right_Hand_Sides =>
            +(String_Lists.Empty_List, String_Lists.Empty_List, String_Lists.Empty_List, 1) +
            (+"LEFT_PAREN" + "IDENTIFIER" + "RIGHT_PAREN", String_Lists.Empty_List, String_Lists.Empty_List, 2)));

      Check ("1", Computed, Expected);

   end Single_Line;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Nominal_Ada'Access, "debug");
      else
         Register_Routine (T, Nominal_Elisp'Access, "Nominal_Elisp");
         Register_Routine (T, Nominal_Ada'Access, "Nominal_Ada");
         Register_Routine (T, Single_Line'Access, "Single_Line");
      end if;
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../wisi/test/wisi_rules_test.adb");
   end Name;

end Wisi_Rules_Test;
