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

with Ada_Process; use Ada_Process;
with SAL;
with WisiToken.LR.McKenzie_Recover.Ada;
with WisiToken.LR.Parser.Gen_AUnit;
with WisiToken.LR.Parser_Lists;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Test_Ada_Recover is
   use WisiToken.LR.Config_Op_Arrays;
   use WisiToken.LR;

   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;
   Trace     : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);
   Parser    : WisiToken.LR.Parser.Parser;

   Orig_Params : WisiToken.LR.McKenzie_Param_Type
     (First_Terminal    => Ada_Process.Descriptor.First_Terminal,
      Last_Terminal     => Ada_Process.Descriptor.Last_Terminal,
      First_Nonterminal => Ada_Process.Descriptor.First_Nonterminal,
      Last_Nonterminal  => Ada_Process.Descriptor.Last_Nonterminal);

   Empty_Token_ID_Set : constant WisiToken.Token_ID_Set :=
     WisiToken.To_Token_ID_Set
       (Descriptor.First_Terminal,
        Descriptor.Last_Terminal,
        (1 .. 0 => WisiToken.Invalid_Token_ID));

   package Parser_AUnit is new WisiToken.LR.Parser.Gen_AUnit (Descriptor, Empty_Token_ID_Set);
   use Parser_AUnit;

   type Parser_State_Array is array (Integer range 1 .. 15) of WisiToken.LR.Parser_Lists.Parser_State;
   Saved_Data      : Parser_State_Array;
   Saved_Data_Last : Integer := Saved_Data'First - 1;

   procedure Save_Recover
   is begin
      for Parser_State of Parser.Parsers loop
         Saved_Data_Last := Saved_Data_Last + 1;
         Saved_Data (Saved_Data_Last) := Parser_State;
      end loop;
   end Save_Recover;

   ----------
   --  Test procedures

   procedure Kill_Slow_Parser_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use all type SAL.Base_Peek_Type;

      Delete_When_Index : Integer;
      Insert_Case_Index : Integer;
   begin
      --  Simplified from ../test/slow_recover_4.adb; used to be really
      --  slow, now we kill the slow parser sooner.

      Parser.Post_Recover := Save_Recover'Access;
      Saved_Data_Last := Saved_Data'First - 1;

      Parse_Text
        (Parser,
         "procedure Slow_Recover_4 is begin case Data (1) is when Text => begin if (for some C of Content => " &
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90      |99
           "C = Ascii.Nul) then return (Header, To_String_List ( when Text => raise SAL.Not_Implemented with " &
      --    |100      |110      |120      |130      |140      |150      |160      |170      |180      |190
           """UTF-16""; end case; end Slow_Recover_4;");
      --     |196     |204  |210      |220      |230
      --
      --  Enters recovery 1 with 1 parser at 'when' 152, expecting ')'.
      --  Finds two solutions quickly:
      --  (delete 'when' 152)
      --  (insert 'case is')
      --
      --  Enters recovery 2 with 2 parsers:
      --  parser 3 at 'with' 192, expecting ',' | ')'
      --  parser 2 at ';' 204, expecting ',' | ')'
      --  Parser 2 finds one solution, fairly quickly:
      --
      --    2: succeed: enqueue 5689, check 432: 8, (1106 : (END))|
      --  47:(CASE, (211 . 214))|((INSERT, RIGHT_PAREN, 45), (INSERT,
      --  RIGHT_PAREN, 45), (FAST_FORWARD, 47), (INSERT, IF, 47), (INSERT,
      --  SEMICOLON, 47), (INSERT, END, 47), (INSERT, SEMICOLON, 47),
      --  (INSERT, END, 47))
      --
      --  Parser 3 had already checked 1048 when 2 finds a solution; killed
      --  immediately.

      --  It's a race condition which solution is found first in recover 1.
      if WisiToken.LR.Parse_Error_Lists.Constant_Reference
        (Saved_Data (1).Errors, Saved_Data (1).Errors.First).Recover.Ops.Last_Index = 1
      then
         Delete_When_Index := 1;
         Insert_Case_Index := 2;
      else
         Delete_When_Index := 2;
         Insert_Case_Index := 1;
      end if;

      Check_Recover
        (Saved_Data (Delete_When_Index), "recover 1 parser 2",
         Parser_Label            => 2,
         Error_Token_ID          => +WHEN_ID,
         Error_Token_Byte_Region => (153, 156),
         Ops                     => +(Delete, +WHEN_ID, 36),
         Enqueue_Low             => 210,
         Enqueue_High            => 230,
         Check_Low               => 30,
         Check_High              => 40,
         Cost                    => 4);

      Check_Recover
        (Saved_Data (Insert_Case_Index), "recover 1 parser 1",
         Parser_Label            => 1,
         Error_Token_ID          => +WHEN_ID,
         Error_Token_Byte_Region => (153, 156),
         Ops                     => +(Insert, +CASE_ID, 36) & (Insert, +IS_ID, 36),
         Enqueue_Low             => 210,
         Enqueue_High            => 230,
         Check_Low               => 30,
         Check_High              => 40,
         Cost                    => 4);

      Check_Recover
        (Saved_Data (3), "recover 2 parser 3",
         Parser_Label            => 3,
         Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +WITH_ID,
         Error_Token_Byte_Region => (192, 195),
         Success                 => False,
         Check_Low               => 1000,
         Check_High              => 1200);

      Check_Recover
        (Saved_Data (4), "recover 2 parser 2",
         Parser_Label            => 2,
         Errors_Length           => 2,
         Checking_Error          => 2,
         Error_Token_ID          => +SEMICOLON_ID,
         Error_Token_Byte_Region => (205, 205),
         Ops                     => +(Insert, +RIGHT_PAREN_ID, 45) & (Insert, +RIGHT_PAREN_ID, 45) &
           (Fast_Forward, 47) & (Insert, +IF_ID, 47) & (Insert, +SEMICOLON_ID, 47) & (Insert, +END_ID, 47) &
           (Insert, +SEMICOLON_ID, 47) & (Insert, +END_ID, 47),
         Enqueue_Low             => 6100,
         Enqueue_High            => 6300,
         Check_Low               => 450,
         Check_High              => 470,
         Cost                    => 8);
   end Kill_Slow_Parser_2;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Kill_Slow_Parser_2'Access, "Kill_Slow_Parser_2");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_ada_recover.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Run before all tests in register
      Create_Parser
        (Parser,
         Language_Fixes               => WisiToken.LR.McKenzie_Recover.Ada.Language_Fixes'Access,
         Language_Constrain_Terminals => WisiToken.LR.McKenzie_Recover.Ada.Constrain_Terminals'Access,
         Language_String_ID_Set       => WisiToken.LR.McKenzie_Recover.Ada.String_ID_Set'Access,
         Algorithm                    => WisiToken.LALR,
         Trace                        => Trace'Access,
         User_Data                    => User_Data'Access);

      Orig_Params := Parser.Table.McKenzie_Param;
   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is
      use all type System.Multiprocessors.CPU_Range;
   begin
      Parser.Table.McKenzie_Param := Orig_Params;

      if T.Task_Count /= 0 then
         Parser.Table.McKenzie_Param.Task_Count := T.Task_Count;
      end if;

      if T.Cost_Limit /= Natural'Last then
         Parser.Table.McKenzie_Param.Cost_Limit := T.Cost_Limit;
      end if;

      --  Run before each test
      Parser.Post_Recover := null;
   end Set_Up;

end Test_Ada_Recover;
