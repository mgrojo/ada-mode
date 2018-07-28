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

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Wisi;
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
      Exe         : constant String_Access := Locate_Exec_On_Path (Program);
      Success     : Boolean;
      Return_Code : Integer;
      pragma Unreferenced (Return_Code);
   begin
      if Exe = null then
         AUnit.Assertions.Assert (False, "'" & Program & "' not found on path");
      end if;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Put (Standard_Error, Program);
         for Str_Acc of Args loop
            Put (Standard_Error, " ");
            Put (Standard_Error, Str_Acc.all);
         end loop;
         New_Line (Standard_Error);
      end if;

      if Output_File = "" then
         Spawn
           (Program_Name => Exe.all,
            Args         => Args,
            Success      => Success);
      else
         Spawn
           (Program_Name => Exe.all,
            Args         => Args,
            Output_File  => Output_File,
            Err_To_Out   => True,
            Return_Code  => Return_Code,
            Success      => Success);
      end if;

      Check (Program, Success, True);
   end Spawn;

   procedure Get_Gen_Set
     (Root_Name        : in     String;
      Generate_Set     :    out Wisi.Generate_Set_Access;
      If_Lexer_Present :    out Boolean)
   is
      Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisi_Grammar_Actions.Descriptor'Access);
      Input_Data     : aliased WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Grammar_Parser : WisiToken.LR.Parser_No_Recover.Parser;
   begin
      Wisi_Grammar_Main.Create_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         User_Data => Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File ("../wisi/test/" & Root_Name & ".wy");
      Grammar_Parser.Parse;
      Grammar_Parser.Execute_Actions;

      Generate_Set     := Input_Data.Generate_Set;
      If_Lexer_Present := Input_Data.If_Lexer_Present;
   exception
   when WisiToken.Syntax_Error =>
      Grammar_Parser.Put_Errors (Input_Data.Grammar_Lexer.File_Name);
      raise;
   end Get_Gen_Set;

   procedure Diff_Gen
     (Root_Name        : in String;
      Tuple            : in Wisi.Generate_Tuple;
      If_Lexer_Present : in Boolean)
   is
      use Wisi;

      Gen_Alg  : constant String := "_" & To_Lower (Generate_Algorithm'Image (Tuple.Gen_Alg));
      Int_Kind : constant String := "_" & To_Lower (Interface_Type'Image (Tuple.Interface_Kind));

      procedure Diff_One
        (Computed : in String;
         Skip     : in AUnit.Checks.Text_IO.Line_Number_Array_Type := (1 .. 0 => 1))
      is begin
         AUnit.Checks.Text_IO.Check_Files ("", Computed, "../wisi/test/" & Computed & "_good", Skip);
      end Diff_One;

   begin
      case Tuple.Gen_Alg is
      when LR_Generate_Algorithm =>
         Diff_One
           (Root_Name & Gen_Alg &
              (if If_Lexer_Present
               then "_" & Lexer_Image
                 ((if Tuple.Lexer = None
                   then
                     (case Tuple.Out_Lang is
                      when Wisi.Ada | Ada_Emacs => re2c_Lexer,
                      when Elisp => Elisp_Lexer)
                   else Tuple.Lexer)).all
               else "") &
              ".parse_table");

      when Packrat_Generate_Algorithm =>
         null;
      end case;

      case Tuple.Out_Lang is
      when Wisi.Ada =>
         --  Not useful to diff the generated Ada source here; the fact that
         --  the parse succeeds is enough.
         null;

      when Ada_Emacs =>
         Diff_One (Root_Name & Int_Kind & "_actions.adb", Skip => (1 => 2));
         Diff_One (Root_Name & Int_Kind & Gen_Alg  & "_main.adb");
         Diff_One (Root_Name & "-process.el");

      when Elisp =>
         Diff_One (Root_Name & "-" & To_Lower (Generate_Algorithm'Image (Tuple.Gen_Alg)) & "-elisp.el");
      end case;

      if Tuple.Text_Rep then
         Diff_One (Root_Name & "_" & To_Lower (Generate_Algorithm'Image (Tuple.Gen_Alg)) & "_parse_table.txt");
      end if;
   end Diff_Gen;

   procedure Execute_Parse
     (Root_Name    : in String;
      Input_Name   : in String;
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
            3        => new String'("../wisi/test/" & Root_Name & ".input")),
         Output_File => Output);

      AUnit.Checks.Text_IO.Check_Files ("", Output, "../wisi/test/" & Output & "_good");

      if Input_Name'Length > 0 then
         declare
            Output : constant String := Input_Name & "_" &
              To_Lower (Generate_Algorithm'Image (Generate_Alg)) & ".parse";
         begin
            Spawn
              (Program     => Exe,
               Args        =>
                 (1        => new String'("-v"),
                  2        => new String'("2"),
                  3        => new String'("../wisi/test/" & Input_Name & ".input")),
               Output_File => Output);

            AUnit.Checks.Text_IO.Check_Files ("", Output, "../wisi/test/" & Output & "_good");
         end;
      end if;
   end Execute_Parse;

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Simple_Name      : constant String := Ada.Directories.Simple_Name (Test.Root_Name.all);
      Gen_Set          : Wisi.Generate_Set_Access;
      If_Lexer_Present : Boolean;
   begin
      --  wisi-generate, re2c, gprbuild are run from the Makefile, since
      --  some of the generated files are shared with other tests.

      Get_Gen_Set (Simple_Name, Gen_Set, If_Lexer_Present);

      for Tuple of Gen_Set.all loop
         case Tuple.Out_Lang is
         when Wisi.Ada =>
            Execute_Parse (Simple_Name, (if Test.Input_Name = null then "" else Test.Input_Name.all), Tuple.Gen_Alg);

         when Wisi.Ada_Emacs | Wisi.Elisp =>
            null;
         end case;

         --  Do Diff_Gen after compile and execute, so we know the code is
         --  correct before we update _good.
         Diff_Gen (Simple_Name, Tuple, If_Lexer_Present);
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

end Wisi_WY_Test;
