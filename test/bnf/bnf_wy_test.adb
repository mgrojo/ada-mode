--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 - 2021 Stephen Leake
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
with WisiToken.BNF;
with WisiToken.Generate;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Test_Util;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Editing;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Main;
package body BNF_WY_Test is

   function BNF_File_Name (Root_Name : in String) return String is (Root_Name & "_bnf.wy");

   procedure Get_Gen_Set
     (Root_Name        : in     String;
      Generate_Set     :    out WisiToken.BNF.Generate_Set_Access;
      If_Lexer_Present :    out Boolean;
      McKenzie_Recover :    out Boolean;
      Meta_Syntax      :    out WisiToken_Grammar_Runtime.Meta_Syntax)
   is
      use AUnit.Checks;
      use all type WisiToken_Grammar_Runtime.Meta_Syntax;

      Gen_Alg_Set : constant WisiToken.BNF.Generate_Algorithm_Set := WisiToken.BNF.From_Generate_Env_Var;

      Trace          : aliased WisiToken.Text_IO_Trace.Trace;
      Log_File       : Ada.Text_IO.File_Type;
      Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
      Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

      Save_Trace_Parse : constant Integer := WisiToken.Trace_Parse;
   begin
      WisiToken.Trace_Parse := 0; --  user does not want to see a trace of the grammar parser.

      WisiToken.Generate.Error := False;

      WisiToken.Parse.LR.Parser_No_Recover.New_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         Lexer     => Wisitoken_Grammar_Main.Create_Lexer,
         Table     => Wisitoken_Grammar_Main.Create_Parse_Table,
         User_Data => Input_Data'Unchecked_Access);

      Grammar_Parser.Tree.Lexer.Reset_With_File ("../test/bnf/" & Root_Name & ".wy");
      Grammar_Parser.Parse (Log_File);
      Input_Data.Phase := WisiToken_Grammar_Runtime.Meta;
      Grammar_Parser.Execute_Actions;

      Check ("grammar parse meta error", WisiToken.Generate.Error, False);

      declare
         Keep : array (Input_Data.Generate_Set'First .. Input_Data.Generate_Set'Last) of Boolean := (others => False);
         Count : Integer := 0;
         Last : Integer := Input_Data.Generate_Set'First - 1;
      begin
         for I in Input_Data.Generate_Set'Range loop
            Keep (I) := Gen_Alg_Set (Input_Data.Generate_Set (I).Gen_Alg);
         end loop;

         for B of Keep loop
            if B then
               Count := @ + 1;
            end if;
         end loop;

         Generate_Set := new WisiToken.BNF.Generate_Set (1 .. Count);
         for I in Input_Data.Generate_Set'Range loop
            if Keep (I) then
               Last := @ + 1;
               Generate_Set (Last) := Input_Data.Generate_Set (I);
            end if;
         end loop;
      end;

      If_Lexer_Present := Input_Data.If_Lexer_Present;
      Meta_Syntax      := Input_Data.Meta_Syntax;

      case Input_Data.Meta_Syntax is
      when Unknown | BNF_Syntax =>
         null;

      when EBNF_Syntax =>
         --  The BNF file is output by rules.make, and debugged separately. We
         --  need to translate this here in order to set McKenzie_Recover.
         WisiToken_Grammar_Editing.Translate_EBNF_To_BNF
           (Grammar_Parser.Tree, Input_Data, Trace);
      end case;

      Input_Data.Phase := WisiToken_Grammar_Runtime.Other;
      Grammar_Parser.Execute_Actions;

      Check ("grammar parse other error", WisiToken.Generate.Error, False);

      --  FIXME: need clear Boolean McKenzie_Specified
      McKenzie_Recover := Input_Data.McKenzie_Recover.Default_Insert /= 0;

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
      WisiToken.Test_Util.Dos2unix (Computed);
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
            Diff_One (Text_Rep_File_Name (Root_Name, Tuple, 1, If_Lexer_Present, Test_Main => True));
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
        (1      => new String'("--verbosity"),
         2      => new String'
           ("""debug=1 parse=2" &
              (if Generate_Alg in LR_Generate_Algorithm and McKenzie_Recover
               then " mckenzie=1""" else """")),
         others => null);

      Last : Integer := 2;
   begin
      if WisiToken.Trace_Tests > WisiToken.Outline then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "parse " & Exe);
      end if;

      if Generate_Alg in LR_Generate_Algorithm and McKenzie_Recover then
         --  One task in McKenzie_Recover for repeatable results. Verbosity 1
         --  to show algorithm errors.
         Last := Last + 1;
         Args (Last) := new String'("-t");
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

            WisiToken.Test_Util.Spawn (Exe, Args (1 .. Last), Output);
            WisiToken.Test_Util.Dos2unix (Output);

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
            WisiToken.Test_Util.Spawn (Exe, Args (1 .. Last), Output);
            WisiToken.Test_Util.Dos2unix (Output);

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

      Get_Gen_Set (Simple_Name, Gen_Set, If_Lexer_Present, McKenzie_Recover, Meta_Syntax);

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
