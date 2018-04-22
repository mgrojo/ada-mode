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
with AUnit.Checks;
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

   procedure Kill_Slow_Parser_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Standard.AUnit.Checks;
   begin
      --  From ../test/slow_recover_1.adb; enters recovery with 3 parsers,
      --  one succeeds, one fails quickly, the other used to take a long
      --  time to fail. Now we kill that one sooner.
      --
      --  Parsing normally discards all recover data from the failed
      --  parsers; here we preserve it via Post_Recover.
      Parser.Post_Recover := Save_Recover'Access;
      Saved_Data_Last := Saved_Data'First - 1;

      Parse_Text
        (Parser,
         "procedure Slow_Recover_1 is begin if Indenting_Token.ID = -Expression_Opt_ID and (Prev_1 " &
      --           |10       |20       |30       |40       |50       |60       |70       |80       |90
           "= -With_ID and (Prev_3 = Invalid_Token_ID or Prev_3 /= Left_Paren_ID)) or  () then end Slow_Recover_1;");
      --    |90       |100      |110      |120      |130      |140      |150      |160      |170

      --  Missing paren around '... or ...' 132, and missing 'end if;' 173.

      Check_Recover
        (Saved_Data (1), "recover 1 parser 6",
         Parser_Label => 6,
         Error_Token_ID          => +RIGHT_PAREN_ID,
         Error_Token_Byte_Region => (159, 159),
         Ops                     => +(Push_Back, +expression_opt_ID, 14) & (Insert, +LEFT_PAREN_ID, 14) &
           (Fast_Forward,  32) & (Insert, +RIGHT_PAREN_ID, 32),
         Enqueue_Low             => 1000,
         Enqueue_High            => 1200,
         Check_Low               => 80,
         Check_High              => 100,
         Cost                    => 7);

      Check ("recover 1 parser 5.label", Saved_Data (2).Label, 5);
      Check ("recover 1 parser 5.success", Saved_Data (2).Recover.Success, False);
      Check_Range ("recover 1 parser 5.Check_Count", Saved_Data (2).Recover.Check_Count, 247, 300);

      Check_Recover
        (Saved_Data (3), "recover 1 parser 1",
         Parser_Label            => 1,
         Error_Token_ID          => +OR_ID,
         Error_Token_Byte_Region => (161, 162),
         Ops                     => +(Push_Back, +expression_opt_ID, 6) & (Insert, +LEFT_PAREN_ID, 6) &
           (Fast_Forward,  29) & (Insert, +RIGHT_PAREN_ID, 29),
         Enqueue_Low             => 900,
         Enqueue_High            => 1000,
         Check_Low               => 80,
         Check_High              => 100,
         Cost                    => 7);

   end Kill_Slow_Parser_1;

   procedure Kill_Slow_Parser_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Standard.AUnit.Checks;
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
      --  Parser 2 finds two solutions, fairly quickly:
      --    2: succeed 2, enqueue 16001, check  1252, cost:  9
      --    ((insert ') ) ;')(delete 'with')(fast_forward '""; end')(insert 'if')(delete 'case'))
      --    ((insert ') ) ; end if ; end'))
      --
      --  Parser 3 killed after checking 2209

      Check_Recover
        (Saved_Data (1), "recover 1 parser 6",
         Parser_Label => 6,
         Error_Token_ID          => +RIGHT_PAREN_ID,
         Error_Token_Byte_Region => (159, 159),
         Ops                     => +(Push_Back, +expression_opt_ID, 14) & (Insert, +LEFT_PAREN_ID, 14) &
           (Fast_Forward,  32) & (Insert, +RIGHT_PAREN_ID, 32),
         Enqueue_Low             => 1000,
         Enqueue_High            => 1200,
         Check_Low               => 80,
         Check_High              => 100,
         Cost                    => 7);

   end Kill_Slow_Parser_2;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Kill_Slow_Parser_1'Access, "Kill_Slow_Parser_1");
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
