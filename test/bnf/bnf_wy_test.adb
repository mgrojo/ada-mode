--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 - 2020 Stephen Leake
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
with WisiToken.Generate;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Editing;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
package body BNF_WY_Test is

   function BNF_File_Name (Root_Name : in String) return String is (Root_Name & "_bnf.wy");

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

      if WisiToken.Trace_Parse > WisiToken.Outline then
         Put (Standard_Error, Program);
         for Str_Acc of Args loop
            Put (Standard_Error, " ");
            Put (Standard_Error, Str_Acc.all);
         end loop;
         if Output_File /= "" then
            Put (Standard_Error, " > " & Output_File);
         end if;

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

   procedure Dos2unix (File_Name : in String)
   is
      use GNAT.OS_Lib;
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then
         declare
            Exe : constant String_Access := Locate_Exec_On_Path ("dos2unix.exe");
            Success : Boolean;
            pragma Unreferenced (Success);
         begin
            Spawn
              (Program_Name => Exe.all,
               Args         =>
                 (1         => new String'("-q"),
                  2         => new String'(File_Name)),
               Success      => Success);
         end;
      end if;
   end Dos2unix;

   procedure Get_Gen_Set
     (Root_Name        : in     String;
      Limit_Gen_Alg    : in     WisiToken.BNF.Generate_Algorithm;
      Generate_Set     :    out WisiToken.BNF.Generate_Set_Access;
      If_Lexer_Present :    out Boolean;
      McKenzie_Recover :    out Boolean;
      Meta_Syntax      :    out WisiToken_Grammar_Runtime.Meta_Syntax)
   is
      use AUnit.Checks;
      use all type WisiToken.Line_Number_Type;
      use all type WisiToken_Grammar_Runtime.Meta_Syntax;
      use all type WisiToken.BNF.Generate_Algorithm;

      Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
      Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
      Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

      Save_Trace_Parse : constant Integer := WisiToken.Trace_Parse;
   begin
      WisiToken.Trace_Parse := 0; --  user does not want to see a trace of the grammar parser.

      WisiToken.Generate.Error := False;

      Wisitoken_Grammar_Main.Create_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         User_Data => Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File ("../test/bnf/" & Root_Name & ".wy");
      Grammar_Parser.Parse;
      Input_Data.Phase := WisiToken_Grammar_Runtime.Meta;
      Grammar_Parser.Execute_Actions;

      Check ("grammar parse meta error", WisiToken.Generate.Error, False);

      if Limit_Gen_Alg = None then
         Generate_Set     := Input_Data.Generate_Set;
      else
         Generate_Set := null;

         for Tuple of Input_Data.Generate_Set.all loop
            if Tuple.Gen_Alg = Limit_Gen_Alg then
               Generate_Set := new WisiToken.BNF.Generate_Set'(1 => Tuple);
            end if;
         end loop;
      end if;

      If_Lexer_Present := Input_Data.If_Lexer_Present;
      Meta_Syntax      := Input_Data.Meta_Syntax;

      case Input_Data.Meta_Syntax is
      when Unknown | BNF_Syntax =>
         null;

      when EBNF_Syntax =>
         --  The BNF file is output by rules.make, and debugged separately. We
         --  need to translate this here in order to set McKenzie_Recover.
         Grammar_Parser.Tree.Clear_Parse_Streams;
         WisiToken_Grammar_Editing.Translate_EBNF_To_BNF
           (Grammar_Parser.Tree, Input_Data);
      end case;

      Input_Data.Phase := WisiToken_Grammar_Runtime.Other;
      Grammar_Parser.Execute_Actions;

      Check ("grammar parse other error", WisiToken.Generate.Error, False);

      McKenzie_Recover := Input_Data.McKenzie_Recover.Source_Line /= WisiToken.Invalid_Line_Number;

      WisiToken.Trace_Parse := Save_Trace_Parse;
   exception
   when AUnit.Assertions.Assertion_Error =>
      WisiToken.Trace_Parse := Save_Trace_Parse;
      raise;

   when WisiToken.Syntax_Error =>
      WisiToken.Trace_Parse := Save_Trace_Parse;
      Grammar_Parser.Put_Errors;
      raise;
   end Get_Gen_Set;

   procedure Diff_One
     (Computed : in String;
      Skip     : in AUnit.Checks.Text_IO.Line_Number_Array_Type := (1 .. 0 => 1))
   is begin
      AUnit.Checks.Text_IO.Check_Files ("", Computed, "../test/bnf/" & Computed & "_good", Skip);
   end Diff_One;

   procedure Diff_Gen
     (Root_Name        : in String;
      Tuple            : in WisiToken.BNF.Generate_Tuple;
      If_Lexer_Present : in Boolean)
   is
      use WisiToken.BNF;

      Gen_Alg  : constant String := "_" & To_Lower (Tuple.Gen_Alg'Image);
      Int_Kind : constant String := "_" & To_Lower (Interface_Type'Image (Tuple.Interface_Kind));

   begin
      case Tuple.Gen_Alg is
      when LR_Generate_Algorithm =>
         Diff_One
           (Root_Name & Gen_Alg &
              (if Tuple.Gen_Alg = LR1 then "_t1" else "") &
              (if If_Lexer_Present
               then "_" & Lexer_Image (Tuple.Lexer).all
               else "") &
              ".parse_table");

      when Tree_Sitter =>
         Diff_One (Root_Name & ".js");

      when None | Packrat_Generate_Algorithm | External =>
         null;
      end case;

      if Tuple.Gen_Alg /= None then
         case Tuple.Out_Lang is
         when WisiToken.BNF.Ada_Lang =>
            --  Not useful to diff the generated Ada source here; the fact that
            --  the parse succeeds is enough.
            null;

         when Ada_Emacs_Lang =>
            Diff_One (Root_Name & Int_Kind & "_actions.ads", Skip => (1 => 2));
            Diff_One (Root_Name & Int_Kind & "_actions.adb", Skip => (1 => 2));
            Diff_One (Root_Name & Int_Kind & Gen_Alg  & "_main.adb");
            Diff_One (Root_Name & "-process.el");

         end case;

         if Tuple.Text_Rep then
            Diff_One (Text_Rep_File_Name (Root_Name, Tuple, 1, If_Lexer_Present));
         end if;
      end if;
   end Diff_Gen;

   procedure Execute_Parse
     (Root_Name        : in String;
      Input_Name       : in String;
      Generate_Alg     : in WisiToken.BNF.Generate_Algorithm;
      McKenzie_Recover : in Boolean)
   is
      use WisiToken.BNF;

      --  We know Output_Language is Ada

      Exe : constant String := "./" & Root_Name & "_" & To_Lower (Generate_Alg'Image) &
        (if Generate_Alg = LR1 then "_t1" else "") &
        "_run.exe";

      Args : GNAT.OS_Lib.String_List (1 .. 9) :=
        (1      => new String'("-v"),
         2      => new String'("2"),
         others => null);

      Last : Integer := 2;
   begin
      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "parse " & Exe);
      end if;

      if Generate_Alg in LR_Generate_Algorithm and McKenzie_Recover then
         --  One task in McKenzie_Recover for repeatable results. Verbosity 1
         --  to show algorithm errors.
         Last := Last + 1;
         Args (Last) := new String'("-t");
         Last := Last + 1;
         Args (Last) := new String'("1");
         Last := Last + 1;
         Args (Last) := new String'("-m");
         Last := Last + 1;
         Args (Last) := new String'("1");
      end if;

      Last := Last + 1; -- For input file name

      declare
         Default_Input_Name : constant String := "../test/bnf/" & Root_Name & ".input";

         Output : constant String := Root_Name & "_" &
           To_Lower (Generate_Algorithm'Image (Generate_Alg)) &
           (if Generate_Alg = LR1 then "_t1" else "") &
           ".parse";
      begin
         if Ada.Directories.Exists (Default_Input_Name) then
            Args (Last) := new String'(Default_Input_Name);

            Spawn (Exe, Args (1 .. Last), Output);
            Dos2unix (Output);

            AUnit.Checks.Text_IO.Check_Files ("", Output, "../test/bnf/" & Output & "_good");
         end if;
      end;

      if Input_Name'Length > 0 then
         declare
            Output : constant String := Root_Name & "-" & To_Lower (Generate_Algorithm'Image (Generate_Alg)) &
              (if Generate_Alg = LR1 then "_t1" else "") &
              "-" & Input_Name & ".parse";
         begin
            Args (Last) := new String'("../test/bnf/" & Input_Name & ".input");
            Spawn (Exe, Args (1 .. Last), Output);
            Dos2unix (Output);

            AUnit.Checks.Text_IO.Check_Files ("", Output, "../test/bnf/" & Output & "_good");
         end;
      end if;
   end Execute_Parse;

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use all type WisiToken.BNF.Generate_Algorithm;
      use all type WisiToken.BNF.Generate_Set_Access;
      use all type WisiToken_Grammar_Runtime.Meta_Syntax;

      Test : Test_Case renames Test_Case (T);

      Simple_Name      : constant String := Ada.Directories.Simple_Name (Test.Root_Name.all);
      Gen_Set          : WisiToken.BNF.Generate_Set_Access;
      If_Lexer_Present : Boolean;
      McKenzie_Recover : Boolean;
      Meta_Syntax      : WisiToken_Grammar_Runtime.Meta_Syntax;
   begin
      --  wisi-generate, re2c, gprbuild are run from the Makefile, since
      --  some of the generated files are shared with other tests.

      Get_Gen_Set (Simple_Name, Test.Gen_Alg, Gen_Set, If_Lexer_Present, McKenzie_Recover, Meta_Syntax);

      if Meta_Syntax = EBNF_Syntax and
        (Gen_Set /= null and then (for some Gen of Gen_Set.all => Gen.Gen_Alg /= Tree_Sitter))
      then
         Diff_One (BNF_File_Name (Simple_Name));
      end if;

      if Gen_Set /= null then
         for Tuple of Gen_Set.all loop
            case Tuple.Out_Lang is
            when WisiToken.BNF.Ada_Lang =>
               Execute_Parse
                 (Simple_Name,
                  (if Test.Input_Name = null then "" else Test.Input_Name.all),
                  Tuple.Gen_Alg, McKenzie_Recover);

            when WisiToken.BNF.Ada_Emacs_Lang =>
               null;
            end case;

            --  Do Diff_Gen after compile and execute, so we know the code is
            --  correct before we update _good.
            Diff_Gen (Simple_Name, Tuple, If_Lexer_Present);
         end loop;
      end if;

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
      return new String'
        ("bnf_wy_test.adb " & T.Root_Name.all &
           (if T.Input_Name = null then "" else " " & T.Input_Name.all));
   end Name;

end BNF_WY_Test;
