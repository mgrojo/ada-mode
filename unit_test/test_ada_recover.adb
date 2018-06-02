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

with AUnit.Checks;
with Ada_Process_Actions; use Ada_Process_Actions;
with Ada_Process_Main;
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

      use AUnit.Checks;
      use all type SAL.Base_Peek_Type;
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
      --  Enters recovery 1 with 2 parsers, finds solutions quickly:
      --  (insert 'case is')
      --  (delete 'when' 36)
      --  (push_back 'is')
      --
      --  Enters recovery 2 with 5 parsers (1 .. 5). One finds a solution
      --  very quickly (enqueue 219, check 34, cost: 4), one less quickly
      --  (enqueue 3474, check 254, cost: 8), the others are killed with
      --  ~222 checks.

      --  It's tedious and error prone to fully check each solution; we just
      --  verify the max checks in recover 2. Note that the max actual
      --  checks can be larger than the McKenzie_Param Check_Delta_Limit;
      --  the extra checks are done before the first solution is found (a
      --  race condition).

      for I in Saved_Data'First .. Saved_Data_Last loop
         Check_Range (Integer'Image (I), Saved_Data (I).Recover.Check_Count, 0, 1500);
      end loop;
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
      Ada_Process_Main.Create_Parser
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
