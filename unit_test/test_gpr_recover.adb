--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
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

with Gpr_Process_Actions; use Gpr_Process_Actions;
with Gpr_Process_Main;
with WisiToken.LR.Parser.Gen_AUnit;
with WisiToken.Semantic_Checks;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Test_Gpr_Recover is
   use WisiToken.LR.Config_Op_Arrays;
   use WisiToken.LR;

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;
   Trace     : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);
   Parser    : WisiToken.LR.Parser.Parser;

   Orig_Params : WisiToken.LR.McKenzie_Param_Type
     (First_Terminal    => Descriptor.First_Terminal,
      Last_Terminal     => Descriptor.Last_Terminal,
      First_Nonterminal => Descriptor.First_Nonterminal,
      Last_Nonterminal  => Descriptor.Last_Nonterminal);

   Empty_Token_ID_Set : constant WisiToken.Token_ID_Set :=
     WisiToken.To_Token_ID_Set
       (Descriptor.First_Terminal,
        Descriptor.Last_Terminal,
        (1 .. 0 => WisiToken.Invalid_Token_ID));

   package Parser_AUnit is new WisiToken.LR.Parser.Gen_AUnit (Descriptor, Empty_Token_ID_Set);
   use Parser_AUnit;

   ----------
   --  Test procedures

   procedure Renaming_Project (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Parser,
         "project Ada_Mode_Unit_Test is end Ada_Mode_Wisi_Parse;");
      --           |10       |20       |30       |40       |50

      --  Renaming project

      Check_Recover
        (Parser.Parsers.First.State_Ref,
         Errors_Length           => 1,
         Error_Token_ID          => +identifier_opt_ID,
         Error_Token_Byte_Region => (35, 53),
         Code                    => WisiToken.Semantic_Checks.Match_Names_Error,
         Enqueue_Low             => 1,
         Enqueue_High            => 1,
         Check_Low               => 1,
         Check_High              => 1,
         Cost                    => 0);

   end Renaming_Project;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Renaming_Project'Access, "Renaming_Project");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_gpr_recover.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Gpr_Process_Main.Create_Parser
        (Parser,
         Language_Fixes               => null, -- WisiToken.LR.McKenzie_Recover.Gpr.Language_Fixes'Access,
         Language_Constrain_Terminals => null,
         Language_String_ID_Set       => null,
         Algorithm                    => WisiToken.LALR,
         Trace                        => Trace'Access,
         User_Data                    => User_Data'Access);

      Orig_Params := Parser.Table.McKenzie_Param;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is
      use all type System.Multiprocessors.CPU_Range;
   begin
      --  Run before each test
      Parser.Table.McKenzie_Param := Orig_Params;

      if T.Task_Count /= 0 then
         Parser.Table.McKenzie_Param.Task_Count := T.Task_Count;
      end if;

      if T.Cost_Limit /= Natural'Last then
         Parser.Table.McKenzie_Param.Cost_Limit := T.Cost_Limit;
      end if;

      Parser.Post_Recover := null;
   end Set_Up;

end Test_Gpr_Recover;
