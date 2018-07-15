--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017, 2018 Stephen Leake
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

with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with WisiToken.LR.Parser_No_Recover;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Grammar_Runtime;
with Wisi_Grammar_Actions;
with Wisi_Grammar_Main;
package body Wisi_WY_Test is

   procedure Spawn
     (Program     : in String;
      Args        : in GNAT.OS_Lib.String_List;
      Output_File : in String := "")
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use GNAT.OS_Lib;
      Success     : Boolean;
      Return_Code : Integer;
      pragma Unreferenced (Return_Code);
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Put (Standard_Error, Program);
         for Str_Acc of Args loop
            Put (Standard_Error, " ");
            Put (Standard_Error, Str_Acc.all);
         end loop;
         New_Line;
      end if;

      if Output_File = "" then
         Spawn
           (Program_Name => Locate_Exec_On_Path (Program).all,
            Args         => Args,
            Success      => Success);
      else
         Spawn
           (Program_Name => Locate_Exec_On_Path (Program).all,
            Args         => Args,
            Output_File  => Output_File,
            Err_To_Out   => False,
            Return_Code  => Return_Code,
            Success      => Success);
      end if;

      Check (Program, Success, True);
   end Spawn;

   procedure Generate
     (Root_Name    : in     String;
      Generate_Set : in out Wisi.Generate_Set_Access)
   is
      use GNAT.OS_Lib;
      use Wisi;
      Args : String_List (1 .. 6 + 5 * (if Generate_Set = null then 0 else Generate_Set'Length));
      Last : Integer := 0;

      Gen_Alg_Arg : constant String_Access := new String'("--generate");
      Do_re2c     : Boolean                := False;
   begin
      Last     := Last + 1;
      Args (1) := new String'("--test_main");

      if Generate_Set = null then
         declare
            Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisi_Grammar_Actions.Descriptor'Access);
            Input_Data     : aliased WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
            Grammar_Parser : WisiToken.LR.Parser_No_Recover.Parser;
         begin
            Wisi_Grammar_Main.Create_Parser
              (Parser    => Grammar_Parser,
               Trace     => Trace'Unchecked_Access,
               User_Data => Input_Data'Unchecked_Access);

            Grammar_Parser.Lexer.Reset_With_File ("../../wisi/test/" & Root_Name & ".wy");
            Grammar_Parser.Parse;
            Grammar_Parser.Execute_Actions;

            Generate_Set := Input_Data.Generate_Set;
         exception
         when WisiToken.Syntax_Error =>
            Grammar_Parser.Put_Errors (Input_Data.Grammar_Lexer.File_Name);
            raise;
         end;

         Last := Last + 1;
         Args (Last) := new String'("../../wisi/test/" & Root_Name & ".wy");
         Spawn ("../wisi-generate.exe", Args (1 .. Last));
      else
         Args (1) := new String'("--test_main");

         for Quad of Generate_Set.all loop
            Last := Last + 1;
            Args (Last) := Gen_Alg_Arg;
            Last := Last + 1;
            Args (Last) := new String'(Generate_Algorithm'Image (Quad.Gen_Alg));
            Last := Last + 1;
            Args (Last) := new String'(Wisi.Output_Language'Image (Quad.Out_Lang));

            if Quad.Lexer /= None then
               Last := Last + 1;
               Args (Last) := new String'(Wisi.Lexer_Image (Quad.Lexer).all);
            end if;

            case Quad.Lexer is
            when re2c_Lexer =>
               Do_re2c := True;
            when Elisp_Lexer =>
               null;

            when None =>
               if Quad.Out_Lang in Ada_Output_Language then
                  Do_re2c := True;
               end if;
            end case;

            if Quad.Interface_Kind /= None then
               Last := Last + 1;
               Args (Last) := new String'(Wisi.Interface_Type'Image (Quad.Interface_Kind));
            end if;
         end loop;

         Last := Last + 1;
         Args (Last) := new String'("../../wisi/test/" & Root_Name & ".wy");
         Spawn ("../wisi-generate.exe", Args (1 .. Last));
      end if;

      if Do_re2c then
         Spawn
           (Program => "re2c",
            Args    =>
              (1    => new String'("--no-generation-date"),
               2    => new String'("--debug-output"),
               3    => new String'("--input"),
               4    => new String'("custom"),
               5    => new String'("-W"),
               6    => new String'("-Werror"),
               7    => new String'("--utf-8"),
               8    => new String'("-o"),
               9    => new String'(Root_Name & "_re2c.c"),
               10   => new String'(Root_Name & ".re2c")));
      end if;
   end Generate;

   procedure Diff_Gen
     (Root_Name        : in String;
      Quad             : in Wisi.Generate_Quad;
      If_Lexer_Present : in Boolean)
   is
      use Wisi;

      Gen_Alg  : constant String := "_" & To_Lower (Generate_Algorithm'Image (Quad.Gen_Alg));
      Int_Kind : constant String := "_" & To_Lower (Interface_Type'Image (Quad.Interface_Kind));

      procedure Diff_One
        (Computed : in String;
         Skip : in AUnit.Checks.Text_IO.Line_Number_Array_Type := (1 .. 0 => 1))
      is begin
         AUnit.Checks.Text_IO.Check_Files ("", Computed, "../../wisi/test/" & Computed & "_good", Skip);
      end Diff_One;

   begin
      case Quad.Gen_Alg is
      when LR_Generate_Algorithm =>
         Diff_One
           (Root_Name & Gen_Alg &
              (if If_Lexer_Present
               then "_" & Lexer_Image (Quad.Lexer).all
               else "") &
              ".parse_table");

      when Packrat_Generate_Algorithm =>
         null;
      end case;

      case Quad.Out_Lang is
      when Wisi.Ada =>
         --  Not useful to diff the generated Ada source here; the fact that
         --  the parse succeeds is enough.
         null;

      when Ada_Emacs =>
         Diff_One (Root_Name & Int_Kind & "_actions.adb", Skip => (1 => 2));
         Diff_One (Root_Name & Int_Kind & Gen_Alg  & "_main.adb");

      when Elisp =>
         Diff_One (Root_Name & "-" & To_Lower (Generate_Algorithm'Image (Quad.Gen_Alg)) & "-elisp.el");
      end case;
   end Diff_Gen;

   procedure Compile
     (Root_Name    : in String;
      Generate_Alg : in Wisi.Generate_Algorithm)
   is
      use Wisi;

      --  We know Output_Language is Ada

      Main : constant String := Root_Name & "_" &
        To_Lower (Generate_Algorithm'Image (Generate_Alg)) & "_run";
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "compile " & Root_Name);
      end if;

      Spawn
        (Program => "gprbuild",
         Args    =>
           (1    => new String'("-p"),
            2    => new String'("-q"), -- quiet; no [compile] in test_all_harness.out
            3    => new String'("-P"),
            4    => new String'("wisi_test.gpr"),
            5    => new String'(Main)));
   end Compile;

   procedure Execute_Parse
     (Root_Name    : in String;
      Generate_Alg : in Wisi.Generate_Algorithm)
   is
      use Wisi;

      --  We know Output_Language is Ada

      Exe : constant String := Root_Name & "_" & To_Lower (Generate_Algorithm'Image (Generate_Alg)) & "_run.exe";

      Output : constant String := Root_Name & "_" &
        To_Lower (Generate_Algorithm'Image (Generate_Alg)) & ".parse";
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "parse " & Exe);
      end if;

      Spawn
        (Program     => Exe,
         Args        =>
           (1        => new String'("-v"),
            2        => new String'("2"),
            3        => new String'("../../wisi/test/" & Root_Name & ".input")),
         Output_File => Output);

      AUnit.Checks.Text_IO.Check_Files ("", Output, "../../wisi/test/" & Output & "_good");
   end Execute_Parse;

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Gen_Set     : Wisi.Generate_Set_Access := Test.Generate_Set;
      Simple_Name : constant String          := Ada.Directories.Simple_Name (Test.Root_Name.all);
   begin
      Generate (Simple_Name, Gen_Set);
      for Quad of Gen_Set.all loop
         case Quad.Out_Lang is
         when Wisi.Ada =>
            Compile (Simple_Name, Quad.Gen_Alg);
            Execute_Parse (Simple_Name, Quad.Gen_Alg);

         when Wisi.Ada_Emacs | Wisi.Elisp =>
            null;
         end case;

         --  Do Diff_Gen after compile and execute, so we know the code is
         --  correct before we update _good.
         Diff_Gen (Simple_Name, Quad, Test.If_Lexer_Present);
      end loop;
   end Run_Test;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_Test'Access, "Run_Test");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("wisi_wy_test.adb-" & T.Root_Name.all);
   end Name;

   Orig_Dir : constant String := Ada.Directories.Current_Directory;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Ada.Directories.Set_Directory (Orig_Dir & "/wisi");
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Ada.Directories.Set_Directory (Orig_Dir);
   end Tear_Down_Case;

end Wisi_WY_Test;
