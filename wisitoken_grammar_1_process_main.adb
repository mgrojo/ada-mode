--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy
--

--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with Wisitoken_Grammar_1_Process_Actions; use Wisitoken_Grammar_1_Process_Actions;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_1_re2c_c;
package body Wisitoken_Grammar_1_Process_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_1_re2c_c.New_Lexer,
      wisitoken_grammar_1_re2c_c.Free_Lexer,
      wisitoken_grammar_1_re2c_c.Reset_Lexer,
      wisitoken_grammar_1_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                         :    out WisiToken.Parse.LR.Parser.Parser;
      Language_Fixes                 : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set       : in     WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 38,
         First_Nonterminal => 39,
         Last_Nonterminal  => 60,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2,
            2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Minimal_Complete_Cost_Delta => -1,
         Fast_Forward =>  0,
         Matching_Begin =>  0,
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Check_Limit => 3,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 233,
         First_Terminal    => 3,
         Last_Terminal     => 38,
         First_Nonterminal => 39,
         Last_Nonterminal  => 60);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 25, (40, 0), 1);
            Add_Action (Table.States (0), 35, (46, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 40, 3);
            Add_Goto (Table.States (0), 46, 4);
            Add_Goto (Table.States (0), 58, 5);
            Add_Goto (Table.States (0), 59, 6);
            Table.States (1).Action_List.Set_Capacity (8);
            Add_Action (Table.States (1), 3, (40, 1), 7);
            Add_Action (Table.States (1), 4, (40, 8), 8);
            Add_Action (Table.States (1), 5, (40, 6), 9);
            Add_Action (Table.States (1), 6, (40, 4), 10);
            Add_Action (Table.States (1), 8, (41, 0), 11);
            Add_Action (Table.States (1), 9, (41, 1), 12);
            Add_Action (Table.States (1), 10, (41, 2), 13);
            Add_Action (Table.States (1), 35, (40, 2), 14);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 41, 15);
            Table.States (1).Kernel := To_Vector ((((40, 0),  25,  3, (2147483647, 0),  0), ((40, 1),  25,  3,
            (2147483647, 0),  0), ((40, 2),  25,  2, (2147483647, 0),  0), ((40, 3),  25,  1, (2147483647, 0),  0),
            ((40, 4),  25,  4, (2147483647, 0),  0), ((40, 5),  25,  4, (2147483647, 0),  0), ((40, 6),  25,  4,
            (2147483647, 0),  0), ((40, 7),  25,  4, (2147483647, 0),  0), ((40, 8),  25,  2, (2147483647, 0),  0)));
            Table.States (1).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 2),  35, 14)));
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 15, (60, 0), 16);
            Add_Action (Table.States (2), 16, (60, 1), 17);
            Table.States (2).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (2), 60, 18);
            Table.States (2).Kernel := To_Vector ((0 => ((46, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (2).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (60, 0),  15, 16)));
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (25, 35, 38), (58, 0),  1, null, null);
            Table.States (3).Kernel := To_Vector ((0 => ((58, 0),  40,  0, (58, 0),  1)));
            Table.States (3).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (25, 35, 38), (58, 1),  1, null, null);
            Table.States (4).Kernel := To_Vector ((0 => ((58, 1),  46,  0, (58, 1),  1)));
            Table.States (4).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  1)));
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (25, 35, 38), (59, 0),  1, compilation_unit_list_0'Access, null);
            Table.States (5).Kernel := To_Vector ((0 => ((59, 0),  58,  0, (59, 0),  1)));
            Table.States (5).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 25, (40, 0), 1);
            Add_Action (Table.States (6), 35, (46, 0), 2);
            Add_Action (Table.States (6), 38, Accept_It, (39, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 40, 3);
            Add_Goto (Table.States (6), 46, 4);
            Add_Goto (Table.States (6), 58, 19);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 35, (42, 0), 20);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 42, 21);
            Table.States (7).Kernel := To_Vector ((0 => ((40, 1),  3,  2, (2147483647, 0),  0)));
            Table.States (7).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 0),  35, 20)));
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 6, (40, 8), 22);
            Table.States (8).Kernel := To_Vector ((0 => ((40, 8),  4,  1, (2147483647, 0),  0)));
            Table.States (8).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 8),  6, 22)));
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 35, (40, 6), 23);
            Table.States (9).Kernel := To_Vector ((((40, 6),  5,  3, (2147483647, 0),  0), ((40, 7),  5,  3,
            (2147483647, 0),  0)));
            Table.States (9).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 6),  35, 23)));
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 35, (40, 4), 24);
            Table.States (10).Kernel := To_Vector ((((40, 4),  6,  3, (2147483647, 0),  0), ((40, 5),  6,  3,
            (2147483647, 0),  0)));
            Table.States (10).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 4),  35, 24)));
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), (1 =>  35), (41, 0),  1, null, null);
            Table.States (11).Kernel := To_Vector ((0 => ((41, 0),  8,  0, (41, 0),  1)));
            Table.States (11).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 0),  1)));
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 23, (41, 1), 25);
            Table.States (12).Kernel := To_Vector ((0 => ((41, 1),  9,  3, (2147483647, 0),  0)));
            Table.States (12).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 1),  23, 25)));
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 23, (41, 2), 26);
            Table.States (13).Kernel := To_Vector ((0 => ((41, 2),  10,  3, (2147483647, 0),  0)));
            Table.States (13).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 2),  23, 26)));
            Table.States (14).Action_List.Set_Capacity (15);
            Add_Action (Table.States (14), 7, (45, 3), 27);
            Add_Action (Table.States (14), 10, (45, 12), 28);
            Add_Action (Table.States (14), 12, (45, 7), 29);
            Add_Action (Table.States (14), 14, (45, 0), 30);
            Add_Action (Table.States (14), 17, (45, 1), 31);
            Add_Action (Table.States (14), 18, (45, 4), 32);
            Add_Action (Table.States (14), 22, (45, 5), 33);
            Add_Action (Table.States (14), 25, Reduce, (40, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (14), 30, (45, 8), 34);
            Add_Action (Table.States (14), 32, (45, 9), 35);
            Add_Action (Table.States (14), 34, (45, 6), 36);
            Add_Action (Table.States (14), 35, (45, 2), 37);
            Add_Conflict (Table.States (14), 35, (40, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (14), 36, (45, 10), 38);
            Add_Action (Table.States (14), 37, (45, 11), 39);
            Add_Action (Table.States (14), 38, Reduce, (40, 3),  2, declaration_3'Access, null);
            Table.States (14).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (14), 44, 40);
            Add_Goto (Table.States (14), 45, 41);
            Table.States (14).Kernel := To_Vector ((((40, 2),  35,  1, (2147483647, 0),  0), ((40, 3),  35,  0, (40,
            3),  2)));
            Table.States (14).Minimal_Complete_Actions := To_Vector (((Shift, (45, 0),  14, 30), (Reduce, (40, 3),
            2)));
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 35, (40, 0), 42);
            Table.States (15).Kernel := To_Vector ((0 => ((40, 0),  41,  2, (2147483647, 0),  0)));
            Table.States (15).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 0),  35, 42)));
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), (14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (60, 0),  1,
            nonterminal_009_0'Access, null);
            Table.States (16).Kernel := To_Vector ((0 => ((60, 0),  15,  0, (60, 0),  1)));
            Table.States (16).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 0),  1)));
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), (14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (60, 1),  1,
            nonterminal_009_1'Access, null);
            Table.States (17).Kernel := To_Vector ((0 => ((60, 1),  16,  0, (60, 1),  1)));
            Table.States (17).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 1),  1)));
            Table.States (18).Action_List.Set_Capacity (10);
            Add_Action (Table.States (18), 14, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (18), 20, (56, 0), 43);
            Add_Action (Table.States (18), 21, (55, 0), 44);
            Add_Action (Table.States (18), 22, (54, 0), 45);
            Add_Action (Table.States (18), 23, (50, 0), 46);
            Add_Action (Table.States (18), 25, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (18), 31, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (18), 35, (51, 1), 47);
            Add_Conflict (Table.States (18), 35, (49, 0),  0, null, null);
            Add_Action (Table.States (18), 37, (53, 1), 48);
            Add_Action (Table.States (18), 38, Reduce, (49, 0),  0, null, null);
            Table.States (18).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (18), 48, 49);
            Add_Goto (Table.States (18), 49, 50);
            Add_Goto (Table.States (18), 50, 51);
            Add_Goto (Table.States (18), 51, 52);
            Add_Goto (Table.States (18), 52, 53);
            Add_Goto (Table.States (18), 53, 54);
            Add_Goto (Table.States (18), 54, 55);
            Add_Goto (Table.States (18), 55, 56);
            Add_Goto (Table.States (18), 56, 57);
            Table.States (18).Kernel := To_Vector ((0 => ((46, 0),  60,  0, (48, 6),  4)));
            Table.States (18).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 6),  4)));
            Table.States (19).Action_List.Set_Capacity (3);
            Add_Action (Table.States (19), (25, 35, 38), (59, 1),  2, compilation_unit_list_1'Access, null);
            Table.States (19).Kernel := To_Vector ((0 => ((59, 1),  58,  0, (59, 1),  2)));
            Table.States (19).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  2)));
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), (11, 35), (42, 0),  1, null, null);
            Table.States (20).Kernel := To_Vector ((0 => ((42, 0),  35,  0, (42, 0),  1)));
            Table.States (20).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 0),  1)));
            Table.States (21).Action_List.Set_Capacity (2);
            Add_Action (Table.States (21), 11, (40, 1), 58);
            Add_Action (Table.States (21), 35, (42, 1), 59);
            Table.States (21).Kernel := To_Vector ((((40, 1),  42,  1, (2147483647, 0),  0), ((42, 1),  42,  1,
            (2147483647, 0),  0)));
            Table.States (21).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 1),  11, 58)));
            Table.States (22).Action_List.Set_Capacity (3);
            Add_Action (Table.States (22), (25, 35, 38), (40, 8),  3, declaration_8'Access, null);
            Table.States (22).Kernel := To_Vector ((0 => ((40, 8),  6,  0, (40, 8),  3)));
            Table.States (22).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 8),  3)));
            Table.States (23).Action_List.Set_Capacity (2);
            Add_Action (Table.States (23), 7, (40, 7), 60);
            Add_Action (Table.States (23), 18, (40, 6), 61);
            Table.States (23).Kernel := To_Vector ((((40, 6),  35,  2, (2147483647, 0),  0), ((40, 7),  35,  2,
            (2147483647, 0),  0)));
            Table.States (23).Minimal_Complete_Actions := To_Vector (((Shift, (40, 6),  18, 61), (Shift, (40, 7),  7,
            60)));
            Table.States (24).Action_List.Set_Capacity (2);
            Add_Action (Table.States (24), 7, (40, 5), 62);
            Add_Action (Table.States (24), 18, (40, 4), 63);
            Table.States (24).Kernel := To_Vector ((((40, 4),  35,  2, (2147483647, 0),  0), ((40, 5),  35,  2,
            (2147483647, 0),  0)));
            Table.States (24).Minimal_Complete_Actions := To_Vector (((Shift, (40, 4),  18, 63), (Shift, (40, 5),  7,
            62)));
            Table.States (25).Action_List.Set_Capacity (1);
            Add_Action (Table.States (25), 35, (41, 1), 64);
            Table.States (25).Kernel := To_Vector ((0 => ((41, 1),  23,  2, (2147483647, 0),  0)));
            Table.States (25).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 1),  35, 64)));
            Table.States (26).Action_List.Set_Capacity (1);
            Add_Action (Table.States (26), 35, (41, 2), 65);
            Table.States (26).Kernel := To_Vector ((0 => ((41, 2),  23,  2, (2147483647, 0),  0)));
            Table.States (26).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 2),  35, 65)));
            Table.States (27).Action_List.Set_Capacity (15);
            Add_Action (Table.States (27), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 3),  1,
            null, null);
            Table.States (27).Kernel := To_Vector ((0 => ((45, 3),  7,  0, (45, 3),  1)));
            Table.States (27).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 3),  1)));
            Table.States (28).Action_List.Set_Capacity (15);
            Add_Action (Table.States (28), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 12),  1,
            null, null);
            Table.States (28).Kernel := To_Vector ((0 => ((45, 12),  10,  0, (45, 12),  1)));
            Table.States (28).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 12),  1)));
            Table.States (29).Action_List.Set_Capacity (15);
            Add_Action (Table.States (29), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 7),  1,
            declaration_item_7'Access, null);
            Table.States (29).Kernel := To_Vector ((0 => ((45, 7),  12,  0, (45, 7),  1)));
            Table.States (29).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 7),  1)));
            Table.States (30).Action_List.Set_Capacity (15);
            Add_Action (Table.States (30), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 0),  1,
            null, null);
            Table.States (30).Kernel := To_Vector ((0 => ((45, 0),  14,  0, (45, 0),  1)));
            Table.States (30).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 0),  1)));
            Table.States (31).Action_List.Set_Capacity (15);
            Add_Action (Table.States (31), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 1),  1,
            null, null);
            Table.States (31).Kernel := To_Vector ((0 => ((45, 1),  17,  0, (45, 1),  1)));
            Table.States (31).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 1),  1)));
            Table.States (32).Action_List.Set_Capacity (15);
            Add_Action (Table.States (32), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 4),  1,
            null, null);
            Table.States (32).Kernel := To_Vector ((0 => ((45, 4),  18,  0, (45, 4),  1)));
            Table.States (32).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 4),  1)));
            Table.States (33).Action_List.Set_Capacity (15);
            Add_Action (Table.States (33), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 5),  1,
            null, null);
            Table.States (33).Kernel := To_Vector ((0 => ((45, 5),  22,  0, (45, 5),  1)));
            Table.States (33).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 5),  1)));
            Table.States (34).Action_List.Set_Capacity (15);
            Add_Action (Table.States (34), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 8),  1,
            null, null);
            Table.States (34).Kernel := To_Vector ((0 => ((45, 8),  30,  0, (45, 8),  1)));
            Table.States (34).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 8),  1)));
            Table.States (35).Action_List.Set_Capacity (15);
            Add_Action (Table.States (35), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 9),  1,
            null, null);
            Table.States (35).Kernel := To_Vector ((0 => ((45, 9),  32,  0, (45, 9),  1)));
            Table.States (35).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 9),  1)));
            Table.States (36).Action_List.Set_Capacity (15);
            Add_Action (Table.States (36), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 6),  1,
            null, null);
            Table.States (36).Kernel := To_Vector ((0 => ((45, 6),  34,  0, (45, 6),  1)));
            Table.States (36).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 6),  1)));
            Table.States (37).Action_List.Set_Capacity (15);
            Add_Action (Table.States (37), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 2),  1,
            null, null);
            Table.States (37).Kernel := To_Vector ((0 => ((45, 2),  35,  0, (45, 2),  1)));
            Table.States (37).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 2),  1)));
            Table.States (38).Action_List.Set_Capacity (15);
            Add_Action (Table.States (38), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 10),  1,
            declaration_item_10'Access, null);
            Table.States (38).Kernel := To_Vector ((0 => ((45, 10),  36,  0, (45, 10),  1)));
            Table.States (38).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 10),  1)));
            Table.States (39).Action_List.Set_Capacity (15);
            Add_Action (Table.States (39), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 11),  1,
            declaration_item_11'Access, null);
            Table.States (39).Kernel := To_Vector ((0 => ((45, 11),  37,  0, (45, 11),  1)));
            Table.States (39).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 11),  1)));
            Table.States (40).Action_List.Set_Capacity (15);
            Add_Action (Table.States (40), 7, (45, 3), 27);
            Add_Action (Table.States (40), 10, (45, 12), 28);
            Add_Action (Table.States (40), 12, (45, 7), 29);
            Add_Action (Table.States (40), 14, (45, 0), 30);
            Add_Action (Table.States (40), 17, (45, 1), 31);
            Add_Action (Table.States (40), 18, (45, 4), 32);
            Add_Action (Table.States (40), 22, (45, 5), 33);
            Add_Action (Table.States (40), 25, Reduce, (40, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (40), 30, (45, 8), 34);
            Add_Action (Table.States (40), 32, (45, 9), 35);
            Add_Action (Table.States (40), 34, (45, 6), 36);
            Add_Action (Table.States (40), 35, (45, 2), 37);
            Add_Conflict (Table.States (40), 35, (40, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (40), 36, (45, 10), 38);
            Add_Action (Table.States (40), 37, (45, 11), 39);
            Add_Action (Table.States (40), 38, Reduce, (40, 2),  3, declaration_2'Access, null);
            Table.States (40).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (40), 45, 66);
            Table.States (40).Kernel := To_Vector ((((40, 2),  44,  0, (40, 2),  3), ((44, 1),  44,  1, (2147483647,
            0),  0)));
            Table.States (40).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 2),  3)));
            Table.States (41).Action_List.Set_Capacity (15);
            Add_Action (Table.States (41), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (44, 0),  1,
            null, null);
            Table.States (41).Kernel := To_Vector ((0 => ((44, 0),  45,  0, (44, 0),  1)));
            Table.States (41).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (42).Action_List.Set_Capacity (13);
            Add_Action (Table.States (42), 7, (45, 3), 27);
            Add_Action (Table.States (42), 10, (45, 12), 28);
            Add_Action (Table.States (42), 12, (45, 7), 29);
            Add_Action (Table.States (42), 14, (45, 0), 30);
            Add_Action (Table.States (42), 17, (45, 1), 31);
            Add_Action (Table.States (42), 18, (45, 4), 32);
            Add_Action (Table.States (42), 22, (45, 5), 33);
            Add_Action (Table.States (42), 30, (45, 8), 34);
            Add_Action (Table.States (42), 32, (45, 9), 35);
            Add_Action (Table.States (42), 34, (45, 6), 36);
            Add_Action (Table.States (42), 35, (45, 2), 37);
            Add_Action (Table.States (42), 36, (45, 10), 38);
            Add_Action (Table.States (42), 37, (45, 11), 39);
            Table.States (42).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (42), 44, 67);
            Add_Goto (Table.States (42), 45, 41);
            Table.States (42).Kernel := To_Vector ((0 => ((40, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (42).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 0),  14, 30)));
            Table.States (43).Action_List.Set_Capacity (6);
            Add_Action (Table.States (43), 20, (56, 0), 68);
            Add_Action (Table.States (43), 21, (55, 0), 69);
            Add_Action (Table.States (43), 22, (54, 0), 70);
            Add_Action (Table.States (43), 23, (50, 0), 71);
            Add_Action (Table.States (43), 35, (51, 1), 72);
            Add_Action (Table.States (43), 37, (53, 1), 73);
            Table.States (43).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (43), 50, 74);
            Add_Goto (Table.States (43), 51, 75);
            Add_Goto (Table.States (43), 52, 76);
            Add_Goto (Table.States (43), 53, 77);
            Add_Goto (Table.States (43), 54, 78);
            Add_Goto (Table.States (43), 55, 79);
            Add_Goto (Table.States (43), 56, 80);
            Add_Goto (Table.States (43), 57, 81);
            Table.States (43).Kernel := To_Vector ((((56, 0),  20,  2, (2147483647, 0),  0), ((56, 1),  20,  3,
            (2147483647, 0),  0)));
            Table.States (43).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 72)));
            Table.States (44).Action_List.Set_Capacity (6);
            Add_Action (Table.States (44), 20, (56, 0), 82);
            Add_Action (Table.States (44), 21, (55, 0), 83);
            Add_Action (Table.States (44), 22, (54, 0), 84);
            Add_Action (Table.States (44), 23, (50, 0), 85);
            Add_Action (Table.States (44), 35, (51, 1), 86);
            Add_Action (Table.States (44), 37, (53, 1), 87);
            Table.States (44).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (44), 50, 88);
            Add_Goto (Table.States (44), 51, 89);
            Add_Goto (Table.States (44), 52, 90);
            Add_Goto (Table.States (44), 53, 91);
            Add_Goto (Table.States (44), 54, 92);
            Add_Goto (Table.States (44), 55, 93);
            Add_Goto (Table.States (44), 56, 94);
            Add_Goto (Table.States (44), 57, 95);
            Table.States (44).Kernel := To_Vector ((0 => ((55, 0),  21,  2, (2147483647, 0),  0)));
            Table.States (44).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 86)));
            Table.States (45).Action_List.Set_Capacity (6);
            Add_Action (Table.States (45), 20, (56, 0), 96);
            Add_Action (Table.States (45), 21, (55, 0), 97);
            Add_Action (Table.States (45), 22, (54, 0), 98);
            Add_Action (Table.States (45), 23, (50, 0), 99);
            Add_Action (Table.States (45), 35, (51, 1), 100);
            Add_Action (Table.States (45), 37, (53, 1), 101);
            Table.States (45).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (45), 50, 102);
            Add_Goto (Table.States (45), 51, 103);
            Add_Goto (Table.States (45), 52, 104);
            Add_Goto (Table.States (45), 53, 105);
            Add_Goto (Table.States (45), 54, 106);
            Add_Goto (Table.States (45), 55, 107);
            Add_Goto (Table.States (45), 56, 108);
            Add_Goto (Table.States (45), 57, 109);
            Table.States (45).Kernel := To_Vector ((((54, 0),  22,  2, (2147483647, 0),  0), ((55, 1),  22,  3,
            (2147483647, 0),  0), ((56, 2),  22,  3, (2147483647, 0),  0), ((56, 3),  22,  3, (2147483647, 0),  0)));
            Table.States (45).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 100)));
            Table.States (46).Action_List.Set_Capacity (1);
            Add_Action (Table.States (46), 35, (50, 0), 110);
            Table.States (46).Kernel := To_Vector ((0 => ((50, 0),  23,  4, (2147483647, 0),  0)));
            Table.States (46).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 110)));
            Table.States (47).Action_List.Set_Capacity (15);
            Add_Action (Table.States (47), 13, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 18, (51, 1), 111);
            Add_Action (Table.States (47), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 25, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 26, (56, 4), 112);
            Add_Action (Table.States (47), 27, (55, 2), 113);
            Add_Action (Table.States (47), 31, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 33, (56, 5), 114);
            Add_Action (Table.States (47), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 37, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (47), 38, Reduce, (53, 0),  1, null, null);
            Table.States (47).Kernel := To_Vector ((((51, 1),  35,  2, (2147483647, 0),  0), ((53, 0),  35,  0, (53,
            0),  1), ((55, 2),  35,  1, (2147483647, 0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,
            1, (2147483647, 0),  0)));
            Table.States (47).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            113), (Shift, (56, 4),  26, 112), (Shift, (56, 5),  33, 114)));
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (48).Action_List.Set_Capacity (12);
            Add_Action (Table.States (48), 13, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 14, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 20, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 21, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 22, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 23, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 25, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 27, (55, 3), 115);
            Add_Action (Table.States (48), 31, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 35, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 37, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (48), 38, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Table.States (48).Kernel := To_Vector ((((53, 1),  37,  0, (53, 1),  1), ((55, 3),  37,  1, (2147483647,
            0),  0)));
            Table.States (48).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 1),  1), (Shift, (55, 3),  27,
            115)));
            Table.States (49).Action_List.Set_Capacity (5);
            Add_Action (Table.States (49), 14, (48, 1), 116);
            Add_Action (Table.States (49), 25, (48, 2), 117);
            Add_Conflict (Table.States (49), 25, (47, 1),  0, null, null);
            Add_Action (Table.States (49), 31, (47, 0), 118);
            Add_Action (Table.States (49), 35, Reduce, (47, 1),  0, null, null);
            Add_Action (Table.States (49), 38, Reduce, (47, 1),  0, null, null);
            Table.States (49).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (49), 47, 119);
            Table.States (49).Kernel := To_Vector ((((46, 0),  48,  0, (47, 1),  0), ((48, 1),  48,  1, (2147483647,
            0),  0), ((48, 2),  48,  5, (2147483647, 0),  0), ((48, 3),  48,  5, (2147483647, 0),  0), ((48, 4),  48,
            5, (2147483647, 0),  0), ((48, 5),  48,  5, (2147483647, 0),  0), ((48, 6),  48,  3, (2147483647, 0),  0)));
            Table.States (49).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 1),  0)));
            Table.States (50).Action_List.Set_Capacity (5);
            Add_Action (Table.States (50), (14, 25, 31, 35, 38), (48, 0),  1, rhs_list_0'Access, null);
            Table.States (50).Kernel := To_Vector ((0 => ((48, 0),  49,  0, (48, 0),  1)));
            Table.States (50).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (51).Action_List.Set_Capacity (11);
            Add_Action (Table.States (51), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (53, 2),  1, null, null);
            Table.States (51).Kernel := To_Vector ((0 => ((53, 2),  50,  0, (53, 2),  1)));
            Table.States (51).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  1)));
            Table.States (52).Action_List.Set_Capacity (11);
            Add_Action (Table.States (52), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (52, 0),  1, null, null);
            Table.States (52).Kernel := To_Vector ((0 => ((52, 0),  51,  0, (52, 0),  1)));
            Table.States (52).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (53).Action_List.Set_Capacity (11);
            Add_Action (Table.States (53), 13, (49, 2), 120);
            Add_Action (Table.States (53), 14, Reduce, (49, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (53), 20, (56, 0), 43);
            Add_Action (Table.States (53), 21, (55, 0), 44);
            Add_Action (Table.States (53), 22, (54, 0), 45);
            Add_Action (Table.States (53), 23, (50, 0), 46);
            Add_Action (Table.States (53), 25, Reduce, (49, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (53), 31, Reduce, (49, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (53), 35, (51, 1), 47);
            Add_Conflict (Table.States (53), 35, (49, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (53), 37, (53, 1), 48);
            Add_Action (Table.States (53), 38, Reduce, (49, 1),  1, rhs_1'Access, null);
            Table.States (53).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (53), 50, 51);
            Add_Goto (Table.States (53), 51, 121);
            Add_Goto (Table.States (53), 53, 54);
            Add_Goto (Table.States (53), 54, 55);
            Add_Goto (Table.States (53), 55, 56);
            Add_Goto (Table.States (53), 56, 57);
            Table.States (53).Kernel := To_Vector ((((49, 1),  52,  0, (49, 1),  1), ((49, 2),  52,  1, (2147483647,
            0),  0), ((49, 3),  52,  2, (2147483647, 0),  0), ((52, 1),  52,  1, (2147483647, 0),  0)));
            Table.States (53).Minimal_Complete_Actions := To_Vector (((Reduce, (49, 1),  1), (Shift, (49, 2),  13,
            120)));
            Table.States (54).Action_List.Set_Capacity (11);
            Add_Action (Table.States (54), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (51, 0),  1, null, null);
            Table.States (54).Kernel := To_Vector ((0 => ((51, 0),  53,  0, (51, 0),  1)));
            Table.States (54).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  1)));
            Table.States (55).Action_List.Set_Capacity (11);
            Add_Action (Table.States (55), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (53, 5),  1, null, null);
            Table.States (55).Kernel := To_Vector ((0 => ((53, 5),  54,  0, (53, 5),  1)));
            Table.States (55).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  1)));
            Table.States (56).Action_List.Set_Capacity (11);
            Add_Action (Table.States (56), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (53, 3),  1, null, null);
            Table.States (56).Kernel := To_Vector ((0 => ((53, 3),  55,  0, (53, 3),  1)));
            Table.States (56).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  1)));
            Table.States (57).Action_List.Set_Capacity (11);
            Add_Action (Table.States (57), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (53, 4),  1, null, null);
            Table.States (57).Kernel := To_Vector ((0 => ((53, 4),  56,  0, (53, 4),  1)));
            Table.States (57).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  1)));
            Table.States (58).Action_List.Set_Capacity (3);
            Add_Action (Table.States (58), (25, 35, 38), (40, 1),  4, declaration_1'Access, null);
            Table.States (58).Kernel := To_Vector ((0 => ((40, 1),  11,  0, (40, 1),  4)));
            Table.States (58).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 1),  4)));
            Table.States (59).Action_List.Set_Capacity (2);
            Add_Action (Table.States (59), (11, 35), (42, 1),  2, null, null);
            Table.States (59).Kernel := To_Vector ((0 => ((42, 1),  35,  0, (42, 1),  2)));
            Table.States (59).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 1),  2)));
            Table.States (60).Action_List.Set_Capacity (1);
            Add_Action (Table.States (60), 35, (43, 0), 122);
            Table.States (60).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (60), 43, 123);
            Table.States (60).Kernel := To_Vector ((0 => ((40, 7),  7,  1, (2147483647, 0),  0)));
            Table.States (60).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 0),  35, 122)));
            Table.States (61).Action_List.Set_Capacity (1);
            Add_Action (Table.States (61), 35, (40, 6), 124);
            Table.States (61).Kernel := To_Vector ((0 => ((40, 6),  18,  1, (2147483647, 0),  0)));
            Table.States (61).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 6),  35, 124)));
            Table.States (62).Action_List.Set_Capacity (1);
            Add_Action (Table.States (62), 35, (43, 0), 122);
            Table.States (62).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (62), 43, 125);
            Table.States (62).Kernel := To_Vector ((0 => ((40, 5),  7,  1, (2147483647, 0),  0)));
            Table.States (62).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 0),  35, 122)));
            Table.States (63).Action_List.Set_Capacity (1);
            Add_Action (Table.States (63), 35, (40, 4), 126);
            Table.States (63).Kernel := To_Vector ((0 => ((40, 4),  18,  1, (2147483647, 0),  0)));
            Table.States (63).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 4),  35, 126)));
            Table.States (64).Action_List.Set_Capacity (1);
            Add_Action (Table.States (64), 19, (41, 1), 127);
            Table.States (64).Kernel := To_Vector ((0 => ((41, 1),  35,  1, (2147483647, 0),  0)));
            Table.States (64).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 1),  19, 127)));
            Table.States (65).Action_List.Set_Capacity (1);
            Add_Action (Table.States (65), 19, (41, 2), 128);
            Table.States (65).Kernel := To_Vector ((0 => ((41, 2),  35,  1, (2147483647, 0),  0)));
            Table.States (65).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 2),  19, 128)));
            Table.States (66).Action_List.Set_Capacity (15);
            Add_Action (Table.States (66), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (44, 1),  2,
            null, null);
            Table.States (66).Kernel := To_Vector ((0 => ((44, 1),  45,  0, (44, 1),  2)));
            Table.States (66).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  2)));
            Table.States (67).Action_List.Set_Capacity (15);
            Add_Action (Table.States (67), 7, (45, 3), 27);
            Add_Action (Table.States (67), 10, (45, 12), 28);
            Add_Action (Table.States (67), 12, (45, 7), 29);
            Add_Action (Table.States (67), 14, (45, 0), 30);
            Add_Action (Table.States (67), 17, (45, 1), 31);
            Add_Action (Table.States (67), 18, (45, 4), 32);
            Add_Action (Table.States (67), 22, (45, 5), 33);
            Add_Action (Table.States (67), 25, Reduce, (40, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (67), 30, (45, 8), 34);
            Add_Action (Table.States (67), 32, (45, 9), 35);
            Add_Action (Table.States (67), 34, (45, 6), 36);
            Add_Action (Table.States (67), 35, (45, 2), 37);
            Add_Conflict (Table.States (67), 35, (40, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (67), 36, (45, 10), 38);
            Add_Action (Table.States (67), 37, (45, 11), 39);
            Add_Action (Table.States (67), 38, Reduce, (40, 0),  4, declaration_0'Access, null);
            Table.States (67).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (67), 45, 66);
            Table.States (67).Kernel := To_Vector ((((40, 0),  44,  0, (40, 0),  4), ((44, 1),  44,  1, (2147483647,
            0),  0)));
            Table.States (67).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 0),  4)));
            Table.States (68).Action_List.Set_Capacity (6);
            Add_Action (Table.States (68), 20, (56, 0), 68);
            Add_Action (Table.States (68), 21, (55, 0), 69);
            Add_Action (Table.States (68), 22, (54, 0), 70);
            Add_Action (Table.States (68), 23, (50, 0), 71);
            Add_Action (Table.States (68), 35, (51, 1), 72);
            Add_Action (Table.States (68), 37, (53, 1), 73);
            Table.States (68).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (68), 50, 74);
            Add_Goto (Table.States (68), 51, 75);
            Add_Goto (Table.States (68), 52, 76);
            Add_Goto (Table.States (68), 53, 77);
            Add_Goto (Table.States (68), 54, 78);
            Add_Goto (Table.States (68), 55, 79);
            Add_Goto (Table.States (68), 56, 80);
            Add_Goto (Table.States (68), 57, 129);
            Table.States (68).Kernel := To_Vector ((((56, 0),  20,  2, (2147483647, 0),  0), ((56, 1),  20,  3,
            (2147483647, 0),  0)));
            Table.States (68).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 72)));
            Table.States (69).Action_List.Set_Capacity (6);
            Add_Action (Table.States (69), 20, (56, 0), 82);
            Add_Action (Table.States (69), 21, (55, 0), 83);
            Add_Action (Table.States (69), 22, (54, 0), 84);
            Add_Action (Table.States (69), 23, (50, 0), 85);
            Add_Action (Table.States (69), 35, (51, 1), 86);
            Add_Action (Table.States (69), 37, (53, 1), 87);
            Table.States (69).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (69), 50, 88);
            Add_Goto (Table.States (69), 51, 89);
            Add_Goto (Table.States (69), 52, 90);
            Add_Goto (Table.States (69), 53, 91);
            Add_Goto (Table.States (69), 54, 92);
            Add_Goto (Table.States (69), 55, 93);
            Add_Goto (Table.States (69), 56, 94);
            Add_Goto (Table.States (69), 57, 130);
            Table.States (69).Kernel := To_Vector ((0 => ((55, 0),  21,  2, (2147483647, 0),  0)));
            Table.States (69).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 86)));
            Table.States (70).Action_List.Set_Capacity (6);
            Add_Action (Table.States (70), 20, (56, 0), 96);
            Add_Action (Table.States (70), 21, (55, 0), 97);
            Add_Action (Table.States (70), 22, (54, 0), 98);
            Add_Action (Table.States (70), 23, (50, 0), 99);
            Add_Action (Table.States (70), 35, (51, 1), 100);
            Add_Action (Table.States (70), 37, (53, 1), 101);
            Table.States (70).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (70), 50, 102);
            Add_Goto (Table.States (70), 51, 103);
            Add_Goto (Table.States (70), 52, 104);
            Add_Goto (Table.States (70), 53, 105);
            Add_Goto (Table.States (70), 54, 106);
            Add_Goto (Table.States (70), 55, 107);
            Add_Goto (Table.States (70), 56, 108);
            Add_Goto (Table.States (70), 57, 131);
            Table.States (70).Kernel := To_Vector ((((54, 0),  22,  2, (2147483647, 0),  0), ((55, 1),  22,  3,
            (2147483647, 0),  0), ((56, 2),  22,  3, (2147483647, 0),  0), ((56, 3),  22,  3, (2147483647, 0),  0)));
            Table.States (70).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 100)));
            Table.States (71).Action_List.Set_Capacity (1);
            Add_Action (Table.States (71), 35, (50, 0), 132);
            Table.States (71).Kernel := To_Vector ((0 => ((50, 0),  23,  4, (2147483647, 0),  0)));
            Table.States (71).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 132)));
            Table.States (72).Action_List.Set_Capacity (12);
            Add_Action (Table.States (72), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 18, (51, 1), 133);
            Add_Action (Table.States (72), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 26, (56, 4), 134);
            Add_Action (Table.States (72), 27, (55, 2), 135);
            Add_Action (Table.States (72), 28, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 33, (56, 5), 136);
            Add_Action (Table.States (72), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (72), 37, Reduce, (53, 0),  1, null, null);
            Table.States (72).Kernel := To_Vector ((((51, 1),  35,  2, (2147483647, 0),  0), ((53, 0),  35,  0, (53,
            0),  1), ((55, 2),  35,  1, (2147483647, 0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,
            1, (2147483647, 0),  0)));
            Table.States (72).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            135), (Shift, (56, 4),  26, 134), (Shift, (56, 5),  33, 136)));
            Table.States (73).Action_List.Set_Capacity (9);
            Add_Action (Table.States (73), 14, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 20, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 21, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 22, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 23, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 27, (55, 3), 137);
            Add_Action (Table.States (73), 28, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 35, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (73), 37, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Table.States (73).Kernel := To_Vector ((((53, 1),  37,  0, (53, 1),  1), ((55, 3),  37,  1, (2147483647,
            0),  0)));
            Table.States (73).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 1),  1), (Shift, (55, 3),  27,
            137)));
            Table.States (74).Action_List.Set_Capacity (8);
            Add_Action (Table.States (74), (14, 20, 21, 22, 23, 28, 35, 37), (53, 2),  1, null, null);
            Table.States (74).Kernel := To_Vector ((0 => ((53, 2),  50,  0, (53, 2),  1)));
            Table.States (74).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  1)));
            Table.States (75).Action_List.Set_Capacity (8);
            Add_Action (Table.States (75), (14, 20, 21, 22, 23, 28, 35, 37), (52, 0),  1, null, null);
            Table.States (75).Kernel := To_Vector ((0 => ((52, 0),  51,  0, (52, 0),  1)));
            Table.States (75).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (76).Action_List.Set_Capacity (8);
            Add_Action (Table.States (76), 14, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (76), 20, (56, 0), 68);
            Add_Action (Table.States (76), 21, (55, 0), 69);
            Add_Action (Table.States (76), 22, (54, 0), 70);
            Add_Action (Table.States (76), 23, (50, 0), 71);
            Add_Action (Table.States (76), 28, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (76), 35, (51, 1), 72);
            Add_Action (Table.States (76), 37, (53, 1), 73);
            Table.States (76).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (76), 50, 74);
            Add_Goto (Table.States (76), 51, 138);
            Add_Goto (Table.States (76), 53, 77);
            Add_Goto (Table.States (76), 54, 78);
            Add_Goto (Table.States (76), 55, 79);
            Add_Goto (Table.States (76), 56, 80);
            Table.States (76).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 0),  52,  0, (57,
            0),  1)));
            Table.States (76).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  1)));
            Table.States (77).Action_List.Set_Capacity (8);
            Add_Action (Table.States (77), (14, 20, 21, 22, 23, 28, 35, 37), (51, 0),  1, null, null);
            Table.States (77).Kernel := To_Vector ((0 => ((51, 0),  53,  0, (51, 0),  1)));
            Table.States (77).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  1)));
            Table.States (78).Action_List.Set_Capacity (8);
            Add_Action (Table.States (78), (14, 20, 21, 22, 23, 28, 35, 37), (53, 5),  1, null, null);
            Table.States (78).Kernel := To_Vector ((0 => ((53, 5),  54,  0, (53, 5),  1)));
            Table.States (78).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  1)));
            Table.States (79).Action_List.Set_Capacity (8);
            Add_Action (Table.States (79), (14, 20, 21, 22, 23, 28, 35, 37), (53, 3),  1, null, null);
            Table.States (79).Kernel := To_Vector ((0 => ((53, 3),  55,  0, (53, 3),  1)));
            Table.States (79).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  1)));
            Table.States (80).Action_List.Set_Capacity (8);
            Add_Action (Table.States (80), (14, 20, 21, 22, 23, 28, 35, 37), (53, 4),  1, null, null);
            Table.States (80).Kernel := To_Vector ((0 => ((53, 4),  56,  0, (53, 4),  1)));
            Table.States (80).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  1)));
            Table.States (81).Action_List.Set_Capacity (2);
            Add_Action (Table.States (81), 14, (57, 1), 139);
            Add_Action (Table.States (81), 28, (56, 0), 140);
            Table.States (81).Kernel := To_Vector ((((56, 0),  57,  1, (2147483647, 0),  0), ((56, 1),  57,  2,
            (2147483647, 0),  0), ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (81).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  28, 140)));
            Table.States (82).Action_List.Set_Capacity (6);
            Add_Action (Table.States (82), 20, (56, 0), 68);
            Add_Action (Table.States (82), 21, (55, 0), 69);
            Add_Action (Table.States (82), 22, (54, 0), 70);
            Add_Action (Table.States (82), 23, (50, 0), 71);
            Add_Action (Table.States (82), 35, (51, 1), 72);
            Add_Action (Table.States (82), 37, (53, 1), 73);
            Table.States (82).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (82), 50, 74);
            Add_Goto (Table.States (82), 51, 75);
            Add_Goto (Table.States (82), 52, 76);
            Add_Goto (Table.States (82), 53, 77);
            Add_Goto (Table.States (82), 54, 78);
            Add_Goto (Table.States (82), 55, 79);
            Add_Goto (Table.States (82), 56, 80);
            Add_Goto (Table.States (82), 57, 141);
            Table.States (82).Kernel := To_Vector ((((56, 0),  20,  2, (2147483647, 0),  0), ((56, 1),  20,  3,
            (2147483647, 0),  0)));
            Table.States (82).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 72)));
            Table.States (83).Action_List.Set_Capacity (6);
            Add_Action (Table.States (83), 20, (56, 0), 82);
            Add_Action (Table.States (83), 21, (55, 0), 83);
            Add_Action (Table.States (83), 22, (54, 0), 84);
            Add_Action (Table.States (83), 23, (50, 0), 85);
            Add_Action (Table.States (83), 35, (51, 1), 86);
            Add_Action (Table.States (83), 37, (53, 1), 87);
            Table.States (83).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (83), 50, 88);
            Add_Goto (Table.States (83), 51, 89);
            Add_Goto (Table.States (83), 52, 90);
            Add_Goto (Table.States (83), 53, 91);
            Add_Goto (Table.States (83), 54, 92);
            Add_Goto (Table.States (83), 55, 93);
            Add_Goto (Table.States (83), 56, 94);
            Add_Goto (Table.States (83), 57, 142);
            Table.States (83).Kernel := To_Vector ((0 => ((55, 0),  21,  2, (2147483647, 0),  0)));
            Table.States (83).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 86)));
            Table.States (84).Action_List.Set_Capacity (6);
            Add_Action (Table.States (84), 20, (56, 0), 96);
            Add_Action (Table.States (84), 21, (55, 0), 97);
            Add_Action (Table.States (84), 22, (54, 0), 98);
            Add_Action (Table.States (84), 23, (50, 0), 99);
            Add_Action (Table.States (84), 35, (51, 1), 100);
            Add_Action (Table.States (84), 37, (53, 1), 101);
            Table.States (84).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (84), 50, 102);
            Add_Goto (Table.States (84), 51, 103);
            Add_Goto (Table.States (84), 52, 104);
            Add_Goto (Table.States (84), 53, 105);
            Add_Goto (Table.States (84), 54, 106);
            Add_Goto (Table.States (84), 55, 107);
            Add_Goto (Table.States (84), 56, 108);
            Add_Goto (Table.States (84), 57, 143);
            Table.States (84).Kernel := To_Vector ((((54, 0),  22,  2, (2147483647, 0),  0), ((55, 1),  22,  3,
            (2147483647, 0),  0), ((56, 2),  22,  3, (2147483647, 0),  0), ((56, 3),  22,  3, (2147483647, 0),  0)));
            Table.States (84).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 100)));
            Table.States (85).Action_List.Set_Capacity (1);
            Add_Action (Table.States (85), 35, (50, 0), 144);
            Table.States (85).Kernel := To_Vector ((0 => ((50, 0),  23,  4, (2147483647, 0),  0)));
            Table.States (85).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 144)));
            Table.States (86).Action_List.Set_Capacity (12);
            Add_Action (Table.States (86), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 18, (51, 1), 145);
            Add_Action (Table.States (86), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 26, (56, 4), 146);
            Add_Action (Table.States (86), 27, (55, 2), 147);
            Add_Action (Table.States (86), 29, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 33, (56, 5), 148);
            Add_Action (Table.States (86), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (86), 37, Reduce, (53, 0),  1, null, null);
            Table.States (86).Kernel := To_Vector ((((51, 1),  35,  2, (2147483647, 0),  0), ((53, 0),  35,  0, (53,
            0),  1), ((55, 2),  35,  1, (2147483647, 0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,
            1, (2147483647, 0),  0)));
            Table.States (86).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            147), (Shift, (56, 4),  26, 146), (Shift, (56, 5),  33, 148)));
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (87).Action_List.Set_Capacity (9);
            Add_Action (Table.States (87), 14, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 20, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 21, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 22, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 23, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 27, (55, 3), 149);
            Add_Action (Table.States (87), 29, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 35, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (87), 37, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Table.States (87).Kernel := To_Vector ((((53, 1),  37,  0, (53, 1),  1), ((55, 3),  37,  1, (2147483647,
            0),  0)));
            Table.States (87).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 1),  1), (Shift, (55, 3),  27,
            149)));
            Table.States (88).Action_List.Set_Capacity (8);
            Add_Action (Table.States (88), (14, 20, 21, 22, 23, 29, 35, 37), (53, 2),  1, null, null);
            Table.States (88).Kernel := To_Vector ((0 => ((53, 2),  50,  0, (53, 2),  1)));
            Table.States (88).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  1)));
            Table.States (89).Action_List.Set_Capacity (8);
            Add_Action (Table.States (89), (14, 20, 21, 22, 23, 29, 35, 37), (52, 0),  1, null, null);
            Table.States (89).Kernel := To_Vector ((0 => ((52, 0),  51,  0, (52, 0),  1)));
            Table.States (89).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (90).Action_List.Set_Capacity (8);
            Add_Action (Table.States (90), 14, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (90), 20, (56, 0), 82);
            Add_Action (Table.States (90), 21, (55, 0), 83);
            Add_Action (Table.States (90), 22, (54, 0), 84);
            Add_Action (Table.States (90), 23, (50, 0), 85);
            Add_Action (Table.States (90), 29, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (90), 35, (51, 1), 86);
            Add_Action (Table.States (90), 37, (53, 1), 87);
            Table.States (90).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (90), 50, 88);
            Add_Goto (Table.States (90), 51, 150);
            Add_Goto (Table.States (90), 53, 91);
            Add_Goto (Table.States (90), 54, 92);
            Add_Goto (Table.States (90), 55, 93);
            Add_Goto (Table.States (90), 56, 94);
            Table.States (90).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 0),  52,  0, (57,
            0),  1)));
            Table.States (90).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  1)));
            Table.States (91).Action_List.Set_Capacity (8);
            Add_Action (Table.States (91), (14, 20, 21, 22, 23, 29, 35, 37), (51, 0),  1, null, null);
            Table.States (91).Kernel := To_Vector ((0 => ((51, 0),  53,  0, (51, 0),  1)));
            Table.States (91).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  1)));
            Table.States (92).Action_List.Set_Capacity (8);
            Add_Action (Table.States (92), (14, 20, 21, 22, 23, 29, 35, 37), (53, 5),  1, null, null);
            Table.States (92).Kernel := To_Vector ((0 => ((53, 5),  54,  0, (53, 5),  1)));
            Table.States (92).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  1)));
            Table.States (93).Action_List.Set_Capacity (8);
            Add_Action (Table.States (93), (14, 20, 21, 22, 23, 29, 35, 37), (53, 3),  1, null, null);
            Table.States (93).Kernel := To_Vector ((0 => ((53, 3),  55,  0, (53, 3),  1)));
            Table.States (93).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  1)));
            Table.States (94).Action_List.Set_Capacity (8);
            Add_Action (Table.States (94), (14, 20, 21, 22, 23, 29, 35, 37), (53, 4),  1, null, null);
            Table.States (94).Kernel := To_Vector ((0 => ((53, 4),  56,  0, (53, 4),  1)));
            Table.States (94).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  1)));
            Table.States (95).Action_List.Set_Capacity (2);
            Add_Action (Table.States (95), 14, (57, 1), 151);
            Add_Action (Table.States (95), 29, (55, 0), 152);
            Table.States (95).Kernel := To_Vector ((((55, 0),  57,  1, (2147483647, 0),  0), ((57, 1),  57,  2,
            (2147483647, 0),  0)));
            Table.States (95).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  29, 152)));
            Table.States (96).Action_List.Set_Capacity (6);
            Add_Action (Table.States (96), 20, (56, 0), 68);
            Add_Action (Table.States (96), 21, (55, 0), 69);
            Add_Action (Table.States (96), 22, (54, 0), 70);
            Add_Action (Table.States (96), 23, (50, 0), 71);
            Add_Action (Table.States (96), 35, (51, 1), 72);
            Add_Action (Table.States (96), 37, (53, 1), 73);
            Table.States (96).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (96), 50, 74);
            Add_Goto (Table.States (96), 51, 75);
            Add_Goto (Table.States (96), 52, 76);
            Add_Goto (Table.States (96), 53, 77);
            Add_Goto (Table.States (96), 54, 78);
            Add_Goto (Table.States (96), 55, 79);
            Add_Goto (Table.States (96), 56, 80);
            Add_Goto (Table.States (96), 57, 153);
            Table.States (96).Kernel := To_Vector ((((56, 0),  20,  2, (2147483647, 0),  0), ((56, 1),  20,  3,
            (2147483647, 0),  0)));
            Table.States (96).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 72)));
            Table.States (97).Action_List.Set_Capacity (6);
            Add_Action (Table.States (97), 20, (56, 0), 82);
            Add_Action (Table.States (97), 21, (55, 0), 83);
            Add_Action (Table.States (97), 22, (54, 0), 84);
            Add_Action (Table.States (97), 23, (50, 0), 85);
            Add_Action (Table.States (97), 35, (51, 1), 86);
            Add_Action (Table.States (97), 37, (53, 1), 87);
            Table.States (97).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (97), 50, 88);
            Add_Goto (Table.States (97), 51, 89);
            Add_Goto (Table.States (97), 52, 90);
            Add_Goto (Table.States (97), 53, 91);
            Add_Goto (Table.States (97), 54, 92);
            Add_Goto (Table.States (97), 55, 93);
            Add_Goto (Table.States (97), 56, 94);
            Add_Goto (Table.States (97), 57, 154);
            Table.States (97).Kernel := To_Vector ((0 => ((55, 0),  21,  2, (2147483647, 0),  0)));
            Table.States (97).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 86)));
            Table.States (98).Action_List.Set_Capacity (6);
            Add_Action (Table.States (98), 20, (56, 0), 96);
            Add_Action (Table.States (98), 21, (55, 0), 97);
            Add_Action (Table.States (98), 22, (54, 0), 98);
            Add_Action (Table.States (98), 23, (50, 0), 99);
            Add_Action (Table.States (98), 35, (51, 1), 100);
            Add_Action (Table.States (98), 37, (53, 1), 101);
            Table.States (98).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (98), 50, 102);
            Add_Goto (Table.States (98), 51, 103);
            Add_Goto (Table.States (98), 52, 104);
            Add_Goto (Table.States (98), 53, 105);
            Add_Goto (Table.States (98), 54, 106);
            Add_Goto (Table.States (98), 55, 107);
            Add_Goto (Table.States (98), 56, 108);
            Add_Goto (Table.States (98), 57, 155);
            Table.States (98).Kernel := To_Vector ((((54, 0),  22,  2, (2147483647, 0),  0), ((55, 1),  22,  3,
            (2147483647, 0),  0), ((56, 2),  22,  3, (2147483647, 0),  0), ((56, 3),  22,  3, (2147483647, 0),  0)));
            Table.States (98).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 100)));
            Table.States (99).Action_List.Set_Capacity (1);
            Add_Action (Table.States (99), 35, (50, 0), 156);
            Table.States (99).Kernel := To_Vector ((0 => ((50, 0),  23,  4, (2147483647, 0),  0)));
            Table.States (99).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 156)));
            Table.States (100).Action_List.Set_Capacity (12);
            Add_Action (Table.States (100), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 18, (51, 1), 157);
            Add_Action (Table.States (100), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 26, (56, 4), 158);
            Add_Action (Table.States (100), 27, (55, 2), 159);
            Add_Action (Table.States (100), 30, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 33, (56, 5), 160);
            Add_Action (Table.States (100), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (100), 37, Reduce, (53, 0),  1, null, null);
            Table.States (100).Kernel := To_Vector ((((51, 1),  35,  2, (2147483647, 0),  0), ((53, 0),  35,  0, (53,
            0),  1), ((55, 2),  35,  1, (2147483647, 0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,
            1, (2147483647, 0),  0)));
            Table.States (100).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            159), (Shift, (56, 4),  26, 158), (Shift, (56, 5),  33, 160)));
            Table.States (101).Action_List.Set_Capacity (9);
            Add_Action (Table.States (101), 14, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 20, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 21, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 22, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 23, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 27, (55, 3), 161);
            Add_Action (Table.States (101), 30, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 35, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (101), 37, Reduce, (53, 1),  1, rhs_item_1'Access, null);
            Table.States (101).Kernel := To_Vector ((((53, 1),  37,  0, (53, 1),  1), ((55, 3),  37,  1, (2147483647,
            0),  0)));
            Table.States (101).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 1),  1), (Shift, (55, 3),  27,
            161)));
            Table.States (102).Action_List.Set_Capacity (8);
            Add_Action (Table.States (102), (14, 20, 21, 22, 23, 30, 35, 37), (53, 2),  1, null, null);
            Table.States (102).Kernel := To_Vector ((0 => ((53, 2),  50,  0, (53, 2),  1)));
            Table.States (102).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  1)));
            Table.States (103).Action_List.Set_Capacity (8);
            Add_Action (Table.States (103), (14, 20, 21, 22, 23, 30, 35, 37), (52, 0),  1, null, null);
            Table.States (103).Kernel := To_Vector ((0 => ((52, 0),  51,  0, (52, 0),  1)));
            Table.States (103).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (104).Action_List.Set_Capacity (8);
            Add_Action (Table.States (104), 14, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (104), 20, (56, 0), 96);
            Add_Action (Table.States (104), 21, (55, 0), 97);
            Add_Action (Table.States (104), 22, (54, 0), 98);
            Add_Action (Table.States (104), 23, (50, 0), 99);
            Add_Action (Table.States (104), 30, Reduce, (57, 0),  1, null, null);
            Add_Action (Table.States (104), 35, (51, 1), 100);
            Add_Action (Table.States (104), 37, (53, 1), 101);
            Table.States (104).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (104), 50, 102);
            Add_Goto (Table.States (104), 51, 162);
            Add_Goto (Table.States (104), 53, 105);
            Add_Goto (Table.States (104), 54, 106);
            Add_Goto (Table.States (104), 55, 107);
            Add_Goto (Table.States (104), 56, 108);
            Table.States (104).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 0),  52,  0, (57,
            0),  1)));
            Table.States (104).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  1)));
            Table.States (105).Action_List.Set_Capacity (8);
            Add_Action (Table.States (105), (14, 20, 21, 22, 23, 30, 35, 37), (51, 0),  1, null, null);
            Table.States (105).Kernel := To_Vector ((0 => ((51, 0),  53,  0, (51, 0),  1)));
            Table.States (105).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  1)));
            Table.States (106).Action_List.Set_Capacity (8);
            Add_Action (Table.States (106), (14, 20, 21, 22, 23, 30, 35, 37), (53, 5),  1, null, null);
            Table.States (106).Kernel := To_Vector ((0 => ((53, 5),  54,  0, (53, 5),  1)));
            Table.States (106).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  1)));
            Table.States (107).Action_List.Set_Capacity (8);
            Add_Action (Table.States (107), (14, 20, 21, 22, 23, 30, 35, 37), (53, 3),  1, null, null);
            Table.States (107).Kernel := To_Vector ((0 => ((53, 3),  55,  0, (53, 3),  1)));
            Table.States (107).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  1)));
            Table.States (108).Action_List.Set_Capacity (8);
            Add_Action (Table.States (108), (14, 20, 21, 22, 23, 30, 35, 37), (53, 4),  1, null, null);
            Table.States (108).Kernel := To_Vector ((0 => ((53, 4),  56,  0, (53, 4),  1)));
            Table.States (108).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  1)));
            Table.States (109).Action_List.Set_Capacity (2);
            Add_Action (Table.States (109), 14, (57, 1), 163);
            Add_Action (Table.States (109), 30, (54, 0), 164);
            Table.States (109).Kernel := To_Vector ((((54, 0),  57,  1, (2147483647, 0),  0), ((55, 1),  57,  2,
            (2147483647, 0),  0), ((56, 2),  57,  2, (2147483647, 0),  0), ((56, 3),  57,  2, (2147483647, 0),  0),
            ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (109).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  30, 164)));
            Table.States (110).Action_List.Set_Capacity (1);
            Add_Action (Table.States (110), 18, (50, 0), 165);
            Table.States (110).Kernel := To_Vector ((0 => ((50, 0),  35,  3, (2147483647, 0),  0)));
            Table.States (110).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  18, 165)));
            Table.States (111).Action_List.Set_Capacity (6);
            Add_Action (Table.States (111), 20, (56, 0), 43);
            Add_Action (Table.States (111), 21, (55, 0), 44);
            Add_Action (Table.States (111), 22, (54, 0), 45);
            Add_Action (Table.States (111), 23, (50, 0), 46);
            Add_Action (Table.States (111), 35, (53, 0), 166);
            Add_Action (Table.States (111), 37, (53, 1), 48);
            Table.States (111).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (111), 50, 51);
            Add_Goto (Table.States (111), 53, 167);
            Add_Goto (Table.States (111), 54, 55);
            Add_Goto (Table.States (111), 55, 56);
            Add_Goto (Table.States (111), 56, 57);
            Table.States (111).Kernel := To_Vector ((0 => ((51, 1),  18,  1, (2147483647, 0),  0)));
            Table.States (111).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  35, 166)));
            Table.States (112).Action_List.Set_Capacity (11);
            Add_Action (Table.States (112), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (56, 4),  2, null, null);
            Table.States (112).Kernel := To_Vector ((0 => ((56, 4),  26,  0, (56, 4),  2)));
            Table.States (112).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 4),  2)));
            Table.States (113).Action_List.Set_Capacity (11);
            Add_Action (Table.States (113), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (55, 2),  2, null, null);
            Table.States (113).Kernel := To_Vector ((0 => ((55, 2),  27,  0, (55, 2),  2)));
            Table.States (113).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 2),  2)));
            Table.States (114).Action_List.Set_Capacity (11);
            Add_Action (Table.States (114), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (56, 5),  2, null, null);
            Table.States (114).Kernel := To_Vector ((0 => ((56, 5),  33,  0, (56, 5),  2)));
            Table.States (114).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 5),  2)));
            Table.States (115).Action_List.Set_Capacity (11);
            Add_Action (Table.States (115), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (55, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (115).Kernel := To_Vector ((0 => ((55, 3),  27,  0, (55, 3),  2)));
            Table.States (115).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 3),  2)));
            Table.States (116).Action_List.Set_Capacity (10);
            Add_Action (Table.States (116), 14, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (116), 20, (56, 0), 43);
            Add_Action (Table.States (116), 21, (55, 0), 44);
            Add_Action (Table.States (116), 22, (54, 0), 45);
            Add_Action (Table.States (116), 23, (50, 0), 46);
            Add_Action (Table.States (116), 25, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (116), 31, Reduce, (49, 0),  0, null, null);
            Add_Action (Table.States (116), 35, (51, 1), 47);
            Add_Conflict (Table.States (116), 35, (49, 0),  0, null, null);
            Add_Action (Table.States (116), 37, (53, 1), 48);
            Add_Action (Table.States (116), 38, Reduce, (49, 0),  0, null, null);
            Table.States (116).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (116), 49, 168);
            Add_Goto (Table.States (116), 50, 51);
            Add_Goto (Table.States (116), 51, 52);
            Add_Goto (Table.States (116), 52, 53);
            Add_Goto (Table.States (116), 53, 54);
            Add_Goto (Table.States (116), 54, 55);
            Add_Goto (Table.States (116), 55, 56);
            Add_Goto (Table.States (116), 56, 57);
            Table.States (116).Kernel := To_Vector ((0 => ((48, 1),  14,  0, (49, 0),  0)));
            Table.States (116).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  0)));
            Table.States (117).Action_List.Set_Capacity (3);
            Add_Action (Table.States (117), 4, (48, 6), 169);
            Add_Action (Table.States (117), 5, (48, 4), 170);
            Add_Action (Table.States (117), 6, (48, 2), 171);
            Table.States (117).Kernel := To_Vector ((((48, 2),  25,  4, (2147483647, 0),  0), ((48, 3),  25,  4,
            (2147483647, 0),  0), ((48, 4),  25,  4, (2147483647, 0),  0), ((48, 5),  25,  4, (2147483647, 0),  0),
            ((48, 6),  25,  2, (2147483647, 0),  0)));
            Table.States (118).Action_List.Set_Capacity (3);
            Add_Action (Table.States (118), (25, 35, 38), (47, 0),  1, null, null);
            Table.States (118).Kernel := To_Vector ((0 => ((47, 0),  31,  0, (47, 0),  1)));
            Table.States (118).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  1)));
            Table.States (119).Action_List.Set_Capacity (3);
            Add_Action (Table.States (119), (25, 35, 38), (46, 0),  4, nonterminal_0'Access, null);
            Table.States (119).Kernel := To_Vector ((0 => ((46, 0),  47,  0, (46, 0),  4)));
            Table.States (119).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 0),  4)));
            Table.States (120).Action_List.Set_Capacity (6);
            Add_Action (Table.States (120), 13, (49, 3), 172);
            Add_Action (Table.States (120), 14, Reduce, (49, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (120), 25, Reduce, (49, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (120), 31, Reduce, (49, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (120), 35, Reduce, (49, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (120), 38, Reduce, (49, 2),  2, rhs_2'Access, null);
            Table.States (120).Kernel := To_Vector ((((49, 2),  13,  0, (49, 2),  2), ((49, 3),  13,  1, (2147483647,
            0),  0)));
            Table.States (120).Minimal_Complete_Actions := To_Vector (((Reduce, (49, 2),  2), (Shift, (49, 3),  13,
            172)));
            Table.States (121).Action_List.Set_Capacity (11);
            Add_Action (Table.States (121), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (52, 1),  2, null, null);
            Table.States (121).Kernel := To_Vector ((0 => ((52, 1),  51,  0, (52, 1),  2)));
            Table.States (121).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  2)));
            Table.States (122).Action_List.Set_Capacity (4);
            Add_Action (Table.States (122), (14, 25, 35, 38), (43, 0),  1, null, null);
            Table.States (122).Kernel := To_Vector ((0 => ((43, 0),  35,  0, (43, 0),  1)));
            Table.States (122).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 0),  1)));
            Table.States (123).Action_List.Set_Capacity (4);
            Add_Action (Table.States (123), 14, (43, 1), 173);
            Add_Action (Table.States (123), 25, Reduce, (40, 7),  5, declaration_7'Access, null);
            Add_Action (Table.States (123), 35, Reduce, (40, 7),  5, declaration_7'Access, null);
            Add_Action (Table.States (123), 38, Reduce, (40, 7),  5, declaration_7'Access, null);
            Table.States (123).Kernel := To_Vector ((((40, 7),  43,  0, (40, 7),  5), ((43, 1),  43,  2, (2147483647,
            0),  0)));
            Table.States (123).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 7),  5)));
            Table.States (124).Action_List.Set_Capacity (3);
            Add_Action (Table.States (124), (25, 35, 38), (40, 6),  5, declaration_6'Access, null);
            Table.States (124).Kernel := To_Vector ((0 => ((40, 6),  35,  0, (40, 6),  5)));
            Table.States (124).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 6),  5)));
            Table.States (125).Action_List.Set_Capacity (4);
            Add_Action (Table.States (125), 14, (43, 1), 173);
            Add_Action (Table.States (125), 25, Reduce, (40, 5),  5, declaration_5'Access, null);
            Add_Action (Table.States (125), 35, Reduce, (40, 5),  5, declaration_5'Access, null);
            Add_Action (Table.States (125), 38, Reduce, (40, 5),  5, declaration_5'Access, null);
            Table.States (125).Kernel := To_Vector ((((40, 5),  43,  0, (40, 5),  5), ((43, 1),  43,  2, (2147483647,
            0),  0)));
            Table.States (125).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 5),  5)));
            Table.States (126).Action_List.Set_Capacity (3);
            Add_Action (Table.States (126), (25, 35, 38), (40, 4),  5, declaration_4'Access, null);
            Table.States (126).Kernel := To_Vector ((0 => ((40, 4),  35,  0, (40, 4),  5)));
            Table.States (126).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 4),  5)));
            Table.States (127).Action_List.Set_Capacity (1);
            Add_Action (Table.States (127), (1 =>  35), (41, 1),  4, token_keyword_non_grammar_1'Access, null);
            Table.States (127).Kernel := To_Vector ((0 => ((41, 1),  19,  0, (41, 1),  4)));
            Table.States (127).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 1),  4)));
            Table.States (128).Action_List.Set_Capacity (1);
            Add_Action (Table.States (128), (1 =>  35), (41, 2),  4, token_keyword_non_grammar_2'Access, null);
            Table.States (128).Kernel := To_Vector ((0 => ((41, 2),  19,  0, (41, 2),  4)));
            Table.States (128).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 2),  4)));
            Table.States (129).Action_List.Set_Capacity (2);
            Add_Action (Table.States (129), 14, (57, 1), 139);
            Add_Action (Table.States (129), 28, (56, 0), 174);
            Table.States (129).Kernel := To_Vector ((((56, 0),  57,  1, (2147483647, 0),  0), ((56, 1),  57,  2,
            (2147483647, 0),  0), ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (129).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  28, 174)));
            Table.States (130).Action_List.Set_Capacity (2);
            Add_Action (Table.States (130), 14, (57, 1), 151);
            Add_Action (Table.States (130), 29, (55, 0), 175);
            Table.States (130).Kernel := To_Vector ((((55, 0),  57,  1, (2147483647, 0),  0), ((57, 1),  57,  2,
            (2147483647, 0),  0)));
            Table.States (130).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  29, 175)));
            Table.States (131).Action_List.Set_Capacity (2);
            Add_Action (Table.States (131), 14, (57, 1), 163);
            Add_Action (Table.States (131), 30, (54, 0), 176);
            Table.States (131).Kernel := To_Vector ((((54, 0),  57,  1, (2147483647, 0),  0), ((55, 1),  57,  2,
            (2147483647, 0),  0), ((56, 2),  57,  2, (2147483647, 0),  0), ((56, 3),  57,  2, (2147483647, 0),  0),
            ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (131).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  30, 176)));
            Table.States (132).Action_List.Set_Capacity (1);
            Add_Action (Table.States (132), 18, (50, 0), 177);
            Table.States (132).Kernel := To_Vector ((0 => ((50, 0),  35,  3, (2147483647, 0),  0)));
            Table.States (132).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  18, 177)));
            Table.States (133).Action_List.Set_Capacity (6);
            Add_Action (Table.States (133), 20, (56, 0), 68);
            Add_Action (Table.States (133), 21, (55, 0), 69);
            Add_Action (Table.States (133), 22, (54, 0), 70);
            Add_Action (Table.States (133), 23, (50, 0), 71);
            Add_Action (Table.States (133), 35, (53, 0), 178);
            Add_Action (Table.States (133), 37, (53, 1), 73);
            Table.States (133).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (133), 50, 74);
            Add_Goto (Table.States (133), 53, 179);
            Add_Goto (Table.States (133), 54, 78);
            Add_Goto (Table.States (133), 55, 79);
            Add_Goto (Table.States (133), 56, 80);
            Table.States (133).Kernel := To_Vector ((0 => ((51, 1),  18,  1, (2147483647, 0),  0)));
            Table.States (133).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  35, 178)));
         end Subr_3;
         procedure Subr_4
         is begin
            Table.States (134).Action_List.Set_Capacity (8);
            Add_Action (Table.States (134), (14, 20, 21, 22, 23, 28, 35, 37), (56, 4),  2, null, null);
            Table.States (134).Kernel := To_Vector ((0 => ((56, 4),  26,  0, (56, 4),  2)));
            Table.States (134).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 4),  2)));
            Table.States (135).Action_List.Set_Capacity (8);
            Add_Action (Table.States (135), (14, 20, 21, 22, 23, 28, 35, 37), (55, 2),  2, null, null);
            Table.States (135).Kernel := To_Vector ((0 => ((55, 2),  27,  0, (55, 2),  2)));
            Table.States (135).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 2),  2)));
            Table.States (136).Action_List.Set_Capacity (8);
            Add_Action (Table.States (136), (14, 20, 21, 22, 23, 28, 35, 37), (56, 5),  2, null, null);
            Table.States (136).Kernel := To_Vector ((0 => ((56, 5),  33,  0, (56, 5),  2)));
            Table.States (136).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 5),  2)));
            Table.States (137).Action_List.Set_Capacity (8);
            Add_Action (Table.States (137), (14, 20, 21, 22, 23, 28, 35, 37), (55, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (137).Kernel := To_Vector ((0 => ((55, 3),  27,  0, (55, 3),  2)));
            Table.States (137).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 3),  2)));
            Table.States (138).Action_List.Set_Capacity (8);
            Add_Action (Table.States (138), (14, 20, 21, 22, 23, 28, 35, 37), (52, 1),  2, null, null);
            Table.States (138).Kernel := To_Vector ((0 => ((52, 1),  51,  0, (52, 1),  2)));
            Table.States (138).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  2)));
            Table.States (139).Action_List.Set_Capacity (6);
            Add_Action (Table.States (139), 20, (56, 0), 68);
            Add_Action (Table.States (139), 21, (55, 0), 69);
            Add_Action (Table.States (139), 22, (54, 0), 70);
            Add_Action (Table.States (139), 23, (50, 0), 71);
            Add_Action (Table.States (139), 35, (51, 1), 72);
            Add_Action (Table.States (139), 37, (53, 1), 73);
            Table.States (139).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (139), 50, 74);
            Add_Goto (Table.States (139), 51, 75);
            Add_Goto (Table.States (139), 52, 180);
            Add_Goto (Table.States (139), 53, 77);
            Add_Goto (Table.States (139), 54, 78);
            Add_Goto (Table.States (139), 55, 79);
            Add_Goto (Table.States (139), 56, 80);
            Table.States (139).Kernel := To_Vector ((0 => ((57, 1),  14,  1, (2147483647, 0),  0)));
            Table.States (139).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 72)));
            Table.States (140).Action_List.Set_Capacity (12);
            Add_Action (Table.States (140), 13, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 14, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 20, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 21, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 22, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 23, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 24, (56, 1), 181);
            Add_Action (Table.States (140), 25, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 31, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 35, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 37, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (140), 38, Reduce, (56, 0),  3, null, null);
            Table.States (140).Kernel := To_Vector ((((56, 0),  28,  0, (56, 0),  3), ((56, 1),  28,  1, (2147483647,
            0),  0)));
            Table.States (140).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 0),  3), (Shift, (56, 1),  24,
            181)));
            Table.States (141).Action_List.Set_Capacity (2);
            Add_Action (Table.States (141), 14, (57, 1), 139);
            Add_Action (Table.States (141), 28, (56, 0), 182);
            Table.States (141).Kernel := To_Vector ((((56, 0),  57,  1, (2147483647, 0),  0), ((56, 1),  57,  2,
            (2147483647, 0),  0), ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (141).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  28, 182)));
            Table.States (142).Action_List.Set_Capacity (2);
            Add_Action (Table.States (142), 14, (57, 1), 151);
            Add_Action (Table.States (142), 29, (55, 0), 183);
            Table.States (142).Kernel := To_Vector ((((55, 0),  57,  1, (2147483647, 0),  0), ((57, 1),  57,  2,
            (2147483647, 0),  0)));
            Table.States (142).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  29, 183)));
            Table.States (143).Action_List.Set_Capacity (2);
            Add_Action (Table.States (143), 14, (57, 1), 163);
            Add_Action (Table.States (143), 30, (54, 0), 184);
            Table.States (143).Kernel := To_Vector ((((54, 0),  57,  1, (2147483647, 0),  0), ((55, 1),  57,  2,
            (2147483647, 0),  0), ((56, 2),  57,  2, (2147483647, 0),  0), ((56, 3),  57,  2, (2147483647, 0),  0),
            ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (143).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  30, 184)));
            Table.States (144).Action_List.Set_Capacity (1);
            Add_Action (Table.States (144), 18, (50, 0), 185);
            Table.States (144).Kernel := To_Vector ((0 => ((50, 0),  35,  3, (2147483647, 0),  0)));
            Table.States (144).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  18, 185)));
            Table.States (145).Action_List.Set_Capacity (6);
            Add_Action (Table.States (145), 20, (56, 0), 82);
            Add_Action (Table.States (145), 21, (55, 0), 83);
            Add_Action (Table.States (145), 22, (54, 0), 84);
            Add_Action (Table.States (145), 23, (50, 0), 85);
            Add_Action (Table.States (145), 35, (53, 0), 186);
            Add_Action (Table.States (145), 37, (53, 1), 87);
            Table.States (145).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (145), 50, 88);
            Add_Goto (Table.States (145), 53, 187);
            Add_Goto (Table.States (145), 54, 92);
            Add_Goto (Table.States (145), 55, 93);
            Add_Goto (Table.States (145), 56, 94);
            Table.States (145).Kernel := To_Vector ((0 => ((51, 1),  18,  1, (2147483647, 0),  0)));
            Table.States (145).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  35, 186)));
            Table.States (146).Action_List.Set_Capacity (8);
            Add_Action (Table.States (146), (14, 20, 21, 22, 23, 29, 35, 37), (56, 4),  2, null, null);
            Table.States (146).Kernel := To_Vector ((0 => ((56, 4),  26,  0, (56, 4),  2)));
            Table.States (146).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 4),  2)));
            Table.States (147).Action_List.Set_Capacity (8);
            Add_Action (Table.States (147), (14, 20, 21, 22, 23, 29, 35, 37), (55, 2),  2, null, null);
            Table.States (147).Kernel := To_Vector ((0 => ((55, 2),  27,  0, (55, 2),  2)));
            Table.States (147).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 2),  2)));
            Table.States (148).Action_List.Set_Capacity (8);
            Add_Action (Table.States (148), (14, 20, 21, 22, 23, 29, 35, 37), (56, 5),  2, null, null);
            Table.States (148).Kernel := To_Vector ((0 => ((56, 5),  33,  0, (56, 5),  2)));
            Table.States (148).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 5),  2)));
            Table.States (149).Action_List.Set_Capacity (8);
            Add_Action (Table.States (149), (14, 20, 21, 22, 23, 29, 35, 37), (55, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (149).Kernel := To_Vector ((0 => ((55, 3),  27,  0, (55, 3),  2)));
            Table.States (149).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 3),  2)));
            Table.States (150).Action_List.Set_Capacity (8);
            Add_Action (Table.States (150), (14, 20, 21, 22, 23, 29, 35, 37), (52, 1),  2, null, null);
            Table.States (150).Kernel := To_Vector ((0 => ((52, 1),  51,  0, (52, 1),  2)));
            Table.States (150).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  2)));
            Table.States (151).Action_List.Set_Capacity (6);
            Add_Action (Table.States (151), 20, (56, 0), 82);
            Add_Action (Table.States (151), 21, (55, 0), 83);
            Add_Action (Table.States (151), 22, (54, 0), 84);
            Add_Action (Table.States (151), 23, (50, 0), 85);
            Add_Action (Table.States (151), 35, (51, 1), 86);
            Add_Action (Table.States (151), 37, (53, 1), 87);
            Table.States (151).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (151), 50, 88);
            Add_Goto (Table.States (151), 51, 89);
            Add_Goto (Table.States (151), 52, 188);
            Add_Goto (Table.States (151), 53, 91);
            Add_Goto (Table.States (151), 54, 92);
            Add_Goto (Table.States (151), 55, 93);
            Add_Goto (Table.States (151), 56, 94);
            Table.States (151).Kernel := To_Vector ((0 => ((57, 1),  14,  1, (2147483647, 0),  0)));
            Table.States (151).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 86)));
            Table.States (152).Action_List.Set_Capacity (11);
            Add_Action (Table.States (152), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (55, 0),  3, null, null);
            Table.States (152).Kernel := To_Vector ((0 => ((55, 0),  29,  0, (55, 0),  3)));
            Table.States (152).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  3)));
            Table.States (153).Action_List.Set_Capacity (2);
            Add_Action (Table.States (153), 14, (57, 1), 139);
            Add_Action (Table.States (153), 28, (56, 0), 189);
            Table.States (153).Kernel := To_Vector ((((56, 0),  57,  1, (2147483647, 0),  0), ((56, 1),  57,  2,
            (2147483647, 0),  0), ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (153).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  28, 189)));
            Table.States (154).Action_List.Set_Capacity (2);
            Add_Action (Table.States (154), 14, (57, 1), 151);
            Add_Action (Table.States (154), 29, (55, 0), 190);
            Table.States (154).Kernel := To_Vector ((((55, 0),  57,  1, (2147483647, 0),  0), ((57, 1),  57,  2,
            (2147483647, 0),  0)));
            Table.States (154).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  29, 190)));
            Table.States (155).Action_List.Set_Capacity (2);
            Add_Action (Table.States (155), 14, (57, 1), 163);
            Add_Action (Table.States (155), 30, (54, 0), 191);
            Table.States (155).Kernel := To_Vector ((((54, 0),  57,  1, (2147483647, 0),  0), ((55, 1),  57,  2,
            (2147483647, 0),  0), ((56, 2),  57,  2, (2147483647, 0),  0), ((56, 3),  57,  2, (2147483647, 0),  0),
            ((57, 1),  57,  2, (2147483647, 0),  0)));
            Table.States (155).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  30, 191)));
            Table.States (156).Action_List.Set_Capacity (1);
            Add_Action (Table.States (156), 18, (50, 0), 192);
            Table.States (156).Kernel := To_Vector ((0 => ((50, 0),  35,  3, (2147483647, 0),  0)));
            Table.States (156).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  18, 192)));
            Table.States (157).Action_List.Set_Capacity (6);
            Add_Action (Table.States (157), 20, (56, 0), 96);
            Add_Action (Table.States (157), 21, (55, 0), 97);
            Add_Action (Table.States (157), 22, (54, 0), 98);
            Add_Action (Table.States (157), 23, (50, 0), 99);
            Add_Action (Table.States (157), 35, (53, 0), 193);
            Add_Action (Table.States (157), 37, (53, 1), 101);
            Table.States (157).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (157), 50, 102);
            Add_Goto (Table.States (157), 53, 194);
            Add_Goto (Table.States (157), 54, 106);
            Add_Goto (Table.States (157), 55, 107);
            Add_Goto (Table.States (157), 56, 108);
            Table.States (157).Kernel := To_Vector ((0 => ((51, 1),  18,  1, (2147483647, 0),  0)));
            Table.States (157).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  35, 193)));
            Table.States (158).Action_List.Set_Capacity (8);
            Add_Action (Table.States (158), (14, 20, 21, 22, 23, 30, 35, 37), (56, 4),  2, null, null);
            Table.States (158).Kernel := To_Vector ((0 => ((56, 4),  26,  0, (56, 4),  2)));
            Table.States (158).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 4),  2)));
            Table.States (159).Action_List.Set_Capacity (8);
            Add_Action (Table.States (159), (14, 20, 21, 22, 23, 30, 35, 37), (55, 2),  2, null, null);
            Table.States (159).Kernel := To_Vector ((0 => ((55, 2),  27,  0, (55, 2),  2)));
            Table.States (159).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 2),  2)));
            Table.States (160).Action_List.Set_Capacity (8);
            Add_Action (Table.States (160), (14, 20, 21, 22, 23, 30, 35, 37), (56, 5),  2, null, null);
            Table.States (160).Kernel := To_Vector ((0 => ((56, 5),  33,  0, (56, 5),  2)));
            Table.States (160).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 5),  2)));
            Table.States (161).Action_List.Set_Capacity (8);
            Add_Action (Table.States (161), (14, 20, 21, 22, 23, 30, 35, 37), (55, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (161).Kernel := To_Vector ((0 => ((55, 3),  27,  0, (55, 3),  2)));
            Table.States (161).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 3),  2)));
            Table.States (162).Action_List.Set_Capacity (8);
            Add_Action (Table.States (162), (14, 20, 21, 22, 23, 30, 35, 37), (52, 1),  2, null, null);
            Table.States (162).Kernel := To_Vector ((0 => ((52, 1),  51,  0, (52, 1),  2)));
            Table.States (162).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  2)));
            Table.States (163).Action_List.Set_Capacity (6);
            Add_Action (Table.States (163), 20, (56, 0), 96);
            Add_Action (Table.States (163), 21, (55, 0), 97);
            Add_Action (Table.States (163), 22, (54, 0), 98);
            Add_Action (Table.States (163), 23, (50, 0), 99);
            Add_Action (Table.States (163), 35, (51, 1), 100);
            Add_Action (Table.States (163), 37, (53, 1), 101);
            Table.States (163).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (163), 50, 102);
            Add_Goto (Table.States (163), 51, 103);
            Add_Goto (Table.States (163), 52, 195);
            Add_Goto (Table.States (163), 53, 105);
            Add_Goto (Table.States (163), 54, 106);
            Add_Goto (Table.States (163), 55, 107);
            Add_Goto (Table.States (163), 56, 108);
            Table.States (163).Kernel := To_Vector ((0 => ((57, 1),  14,  1, (2147483647, 0),  0)));
            Table.States (163).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 1),  35, 100)));
            Table.States (164).Action_List.Set_Capacity (14);
            Add_Action (Table.States (164), 13, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 14, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 20, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 21, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 22, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 23, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 25, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 26, (56, 2), 196);
            Add_Action (Table.States (164), 27, (55, 1), 197);
            Add_Action (Table.States (164), 31, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 33, (56, 3), 198);
            Add_Action (Table.States (164), 35, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 37, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (164), 38, Reduce, (54, 0),  3, null, null);
            Table.States (164).Kernel := To_Vector ((((54, 0),  30,  0, (54, 0),  3), ((55, 1),  30,  1, (2147483647,
            0),  0), ((56, 2),  30,  1, (2147483647, 0),  0), ((56, 3),  30,  1, (2147483647, 0),  0)));
            Table.States (164).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  3), (Shift, (55, 1),  27,
            197), (Shift, (56, 2),  26, 196), (Shift, (56, 3),  33, 198)));
            Table.States (165).Action_List.Set_Capacity (1);
            Add_Action (Table.States (165), 35, (50, 0), 199);
            Table.States (165).Kernel := To_Vector ((0 => ((50, 0),  18,  2, (2147483647, 0),  0)));
            Table.States (165).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 199)));
            Table.States (166).Action_List.Set_Capacity (14);
            Add_Action (Table.States (166), 13, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 25, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 26, (56, 4), 112);
            Add_Action (Table.States (166), 27, (55, 2), 113);
            Add_Action (Table.States (166), 31, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 33, (56, 5), 114);
            Add_Action (Table.States (166), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 37, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (166), 38, Reduce, (53, 0),  1, null, null);
            Table.States (166).Kernel := To_Vector ((((53, 0),  35,  0, (53, 0),  1), ((55, 2),  35,  1, (2147483647,
            0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,  1, (2147483647, 0),  0)));
            Table.States (166).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            113), (Shift, (56, 4),  26, 112), (Shift, (56, 5),  33, 114)));
            Table.States (167).Action_List.Set_Capacity (11);
            Add_Action (Table.States (167), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (51, 1),  3, null, null);
            Table.States (167).Kernel := To_Vector ((0 => ((51, 1),  53,  0, (51, 1),  3)));
            Table.States (167).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 1),  3)));
            Table.States (168).Action_List.Set_Capacity (5);
            Add_Action (Table.States (168), (14, 25, 31, 35, 38), (48, 1),  3, rhs_list_1'Access, null);
            Table.States (168).Kernel := To_Vector ((0 => ((48, 1),  49,  0, (48, 1),  3)));
            Table.States (168).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  3)));
            Table.States (169).Action_List.Set_Capacity (1);
            Add_Action (Table.States (169), 6, (48, 6), 200);
            Table.States (169).Kernel := To_Vector ((0 => ((48, 6),  4,  1, (2147483647, 0),  0)));
            Table.States (169).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 6),  6, 200)));
            Table.States (170).Action_List.Set_Capacity (1);
            Add_Action (Table.States (170), 35, (48, 4), 201);
            Table.States (170).Kernel := To_Vector ((((48, 4),  5,  3, (2147483647, 0),  0), ((48, 5),  5,  3,
            (2147483647, 0),  0)));
            Table.States (171).Action_List.Set_Capacity (1);
            Add_Action (Table.States (171), 35, (48, 2), 202);
            Table.States (171).Kernel := To_Vector ((((48, 2),  6,  3, (2147483647, 0),  0), ((48, 3),  6,  3,
            (2147483647, 0),  0)));
            Table.States (172).Action_List.Set_Capacity (5);
            Add_Action (Table.States (172), (14, 25, 31, 35, 38), (49, 3),  3, rhs_3'Access, null);
            Table.States (172).Kernel := To_Vector ((0 => ((49, 3),  13,  0, (49, 3),  3)));
            Table.States (172).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 3),  3)));
            Table.States (173).Action_List.Set_Capacity (1);
            Add_Action (Table.States (173), 35, (43, 1), 203);
            Table.States (173).Kernel := To_Vector ((0 => ((43, 1),  14,  1, (2147483647, 0),  0)));
            Table.States (173).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 1),  35, 203)));
            Table.States (174).Action_List.Set_Capacity (9);
            Add_Action (Table.States (174), 14, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 20, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 21, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 22, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 23, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 24, (56, 1), 204);
            Add_Action (Table.States (174), 28, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 35, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (174), 37, Reduce, (56, 0),  3, null, null);
            Table.States (174).Kernel := To_Vector ((((56, 0),  28,  0, (56, 0),  3), ((56, 1),  28,  1, (2147483647,
            0),  0)));
            Table.States (174).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 0),  3), (Shift, (56, 1),  24,
            204)));
            Table.States (175).Action_List.Set_Capacity (8);
            Add_Action (Table.States (175), (14, 20, 21, 22, 23, 28, 35, 37), (55, 0),  3, null, null);
            Table.States (175).Kernel := To_Vector ((0 => ((55, 0),  29,  0, (55, 0),  3)));
            Table.States (175).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  3)));
            Table.States (176).Action_List.Set_Capacity (11);
            Add_Action (Table.States (176), 14, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 20, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 21, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 22, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 23, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 26, (56, 2), 205);
            Add_Action (Table.States (176), 27, (55, 1), 206);
            Add_Action (Table.States (176), 28, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 33, (56, 3), 207);
            Add_Action (Table.States (176), 35, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (176), 37, Reduce, (54, 0),  3, null, null);
            Table.States (176).Kernel := To_Vector ((((54, 0),  30,  0, (54, 0),  3), ((55, 1),  30,  1, (2147483647,
            0),  0), ((56, 2),  30,  1, (2147483647, 0),  0), ((56, 3),  30,  1, (2147483647, 0),  0)));
            Table.States (176).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  3), (Shift, (55, 1),  27,
            206), (Shift, (56, 2),  26, 205), (Shift, (56, 3),  33, 207)));
            Table.States (177).Action_List.Set_Capacity (1);
            Add_Action (Table.States (177), 35, (50, 0), 208);
            Table.States (177).Kernel := To_Vector ((0 => ((50, 0),  18,  2, (2147483647, 0),  0)));
            Table.States (177).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 208)));
            Table.States (178).Action_List.Set_Capacity (11);
            Add_Action (Table.States (178), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 26, (56, 4), 134);
            Add_Action (Table.States (178), 27, (55, 2), 135);
            Add_Action (Table.States (178), 28, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 33, (56, 5), 136);
            Add_Action (Table.States (178), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (178), 37, Reduce, (53, 0),  1, null, null);
            Table.States (178).Kernel := To_Vector ((((53, 0),  35,  0, (53, 0),  1), ((55, 2),  35,  1, (2147483647,
            0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,  1, (2147483647, 0),  0)));
            Table.States (178).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            135), (Shift, (56, 4),  26, 134), (Shift, (56, 5),  33, 136)));
            Table.States (179).Action_List.Set_Capacity (8);
            Add_Action (Table.States (179), (14, 20, 21, 22, 23, 28, 35, 37), (51, 1),  3, null, null);
            Table.States (179).Kernel := To_Vector ((0 => ((51, 1),  53,  0, (51, 1),  3)));
            Table.States (179).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 1),  3)));
            Table.States (180).Action_List.Set_Capacity (8);
            Add_Action (Table.States (180), 14, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (180), 20, (56, 0), 68);
            Add_Action (Table.States (180), 21, (55, 0), 69);
            Add_Action (Table.States (180), 22, (54, 0), 70);
            Add_Action (Table.States (180), 23, (50, 0), 71);
            Add_Action (Table.States (180), 28, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (180), 35, (51, 1), 72);
            Add_Action (Table.States (180), 37, (53, 1), 73);
            Table.States (180).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (180), 50, 74);
            Add_Goto (Table.States (180), 51, 138);
            Add_Goto (Table.States (180), 53, 77);
            Add_Goto (Table.States (180), 54, 78);
            Add_Goto (Table.States (180), 55, 79);
            Add_Goto (Table.States (180), 56, 80);
            Table.States (180).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 1),  52,  0, (57,
            1),  3)));
            Table.States (180).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  3)));
         end Subr_4;
         procedure Subr_5
         is begin
            Table.States (181).Action_List.Set_Capacity (11);
            Add_Action (Table.States (181), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (56, 1),  4, null, null);
            Table.States (181).Kernel := To_Vector ((0 => ((56, 1),  24,  0, (56, 1),  4)));
            Table.States (181).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (182).Action_List.Set_Capacity (9);
            Add_Action (Table.States (182), 14, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 20, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 21, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 22, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 23, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 24, (56, 1), 209);
            Add_Action (Table.States (182), 29, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 35, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (182), 37, Reduce, (56, 0),  3, null, null);
            Table.States (182).Kernel := To_Vector ((((56, 0),  28,  0, (56, 0),  3), ((56, 1),  28,  1, (2147483647,
            0),  0)));
            Table.States (182).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 0),  3), (Shift, (56, 1),  24,
            209)));
            Table.States (183).Action_List.Set_Capacity (8);
            Add_Action (Table.States (183), (14, 20, 21, 22, 23, 29, 35, 37), (55, 0),  3, null, null);
            Table.States (183).Kernel := To_Vector ((0 => ((55, 0),  29,  0, (55, 0),  3)));
            Table.States (183).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  3)));
            Table.States (184).Action_List.Set_Capacity (11);
            Add_Action (Table.States (184), 14, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 20, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 21, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 22, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 23, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 26, (56, 2), 210);
            Add_Action (Table.States (184), 27, (55, 1), 211);
            Add_Action (Table.States (184), 29, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 33, (56, 3), 212);
            Add_Action (Table.States (184), 35, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (184), 37, Reduce, (54, 0),  3, null, null);
            Table.States (184).Kernel := To_Vector ((((54, 0),  30,  0, (54, 0),  3), ((55, 1),  30,  1, (2147483647,
            0),  0), ((56, 2),  30,  1, (2147483647, 0),  0), ((56, 3),  30,  1, (2147483647, 0),  0)));
            Table.States (184).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  3), (Shift, (55, 1),  27,
            211), (Shift, (56, 2),  26, 210), (Shift, (56, 3),  33, 212)));
            Table.States (185).Action_List.Set_Capacity (1);
            Add_Action (Table.States (185), 35, (50, 0), 213);
            Table.States (185).Kernel := To_Vector ((0 => ((50, 0),  18,  2, (2147483647, 0),  0)));
            Table.States (185).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 213)));
            Table.States (186).Action_List.Set_Capacity (11);
            Add_Action (Table.States (186), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 26, (56, 4), 146);
            Add_Action (Table.States (186), 27, (55, 2), 147);
            Add_Action (Table.States (186), 29, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 33, (56, 5), 148);
            Add_Action (Table.States (186), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (186), 37, Reduce, (53, 0),  1, null, null);
            Table.States (186).Kernel := To_Vector ((((53, 0),  35,  0, (53, 0),  1), ((55, 2),  35,  1, (2147483647,
            0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,  1, (2147483647, 0),  0)));
            Table.States (186).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            147), (Shift, (56, 4),  26, 146), (Shift, (56, 5),  33, 148)));
            Table.States (187).Action_List.Set_Capacity (8);
            Add_Action (Table.States (187), (14, 20, 21, 22, 23, 29, 35, 37), (51, 1),  3, null, null);
            Table.States (187).Kernel := To_Vector ((0 => ((51, 1),  53,  0, (51, 1),  3)));
            Table.States (187).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 1),  3)));
            Table.States (188).Action_List.Set_Capacity (8);
            Add_Action (Table.States (188), 14, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (188), 20, (56, 0), 82);
            Add_Action (Table.States (188), 21, (55, 0), 83);
            Add_Action (Table.States (188), 22, (54, 0), 84);
            Add_Action (Table.States (188), 23, (50, 0), 85);
            Add_Action (Table.States (188), 29, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (188), 35, (51, 1), 86);
            Add_Action (Table.States (188), 37, (53, 1), 87);
            Table.States (188).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (188), 50, 88);
            Add_Goto (Table.States (188), 51, 150);
            Add_Goto (Table.States (188), 53, 91);
            Add_Goto (Table.States (188), 54, 92);
            Add_Goto (Table.States (188), 55, 93);
            Add_Goto (Table.States (188), 56, 94);
            Table.States (188).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 1),  52,  0, (57,
            1),  3)));
            Table.States (188).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  3)));
            Table.States (189).Action_List.Set_Capacity (9);
            Add_Action (Table.States (189), 14, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 20, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 21, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 22, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 23, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 24, (56, 1), 214);
            Add_Action (Table.States (189), 30, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 35, Reduce, (56, 0),  3, null, null);
            Add_Action (Table.States (189), 37, Reduce, (56, 0),  3, null, null);
            Table.States (189).Kernel := To_Vector ((((56, 0),  28,  0, (56, 0),  3), ((56, 1),  28,  1, (2147483647,
            0),  0)));
            Table.States (189).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 0),  3), (Shift, (56, 1),  24,
            214)));
            Table.States (190).Action_List.Set_Capacity (8);
            Add_Action (Table.States (190), (14, 20, 21, 22, 23, 30, 35, 37), (55, 0),  3, null, null);
            Table.States (190).Kernel := To_Vector ((0 => ((55, 0),  29,  0, (55, 0),  3)));
            Table.States (190).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  3)));
            Table.States (191).Action_List.Set_Capacity (11);
            Add_Action (Table.States (191), 14, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 20, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 21, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 22, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 23, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 26, (56, 2), 215);
            Add_Action (Table.States (191), 27, (55, 1), 216);
            Add_Action (Table.States (191), 30, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 33, (56, 3), 217);
            Add_Action (Table.States (191), 35, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (191), 37, Reduce, (54, 0),  3, null, null);
            Table.States (191).Kernel := To_Vector ((((54, 0),  30,  0, (54, 0),  3), ((55, 1),  30,  1, (2147483647,
            0),  0), ((56, 2),  30,  1, (2147483647, 0),  0), ((56, 3),  30,  1, (2147483647, 0),  0)));
            Table.States (191).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  3), (Shift, (55, 1),  27,
            216), (Shift, (56, 2),  26, 215), (Shift, (56, 3),  33, 217)));
            Table.States (192).Action_List.Set_Capacity (1);
            Add_Action (Table.States (192), 35, (50, 0), 218);
            Table.States (192).Kernel := To_Vector ((0 => ((50, 0),  18,  2, (2147483647, 0),  0)));
            Table.States (192).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  35, 218)));
            Table.States (193).Action_List.Set_Capacity (11);
            Add_Action (Table.States (193), 14, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 20, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 21, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 22, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 23, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 26, (56, 4), 158);
            Add_Action (Table.States (193), 27, (55, 2), 159);
            Add_Action (Table.States (193), 30, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 33, (56, 5), 160);
            Add_Action (Table.States (193), 35, Reduce, (53, 0),  1, null, null);
            Add_Action (Table.States (193), 37, Reduce, (53, 0),  1, null, null);
            Table.States (193).Kernel := To_Vector ((((53, 0),  35,  0, (53, 0),  1), ((55, 2),  35,  1, (2147483647,
            0),  0), ((56, 4),  35,  1, (2147483647, 0),  0), ((56, 5),  35,  1, (2147483647, 0),  0)));
            Table.States (193).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  1), (Shift, (55, 2),  27,
            159), (Shift, (56, 4),  26, 158), (Shift, (56, 5),  33, 160)));
            Table.States (194).Action_List.Set_Capacity (8);
            Add_Action (Table.States (194), (14, 20, 21, 22, 23, 30, 35, 37), (51, 1),  3, null, null);
            Table.States (194).Kernel := To_Vector ((0 => ((51, 1),  53,  0, (51, 1),  3)));
            Table.States (194).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 1),  3)));
            Table.States (195).Action_List.Set_Capacity (8);
            Add_Action (Table.States (195), 14, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (195), 20, (56, 0), 96);
            Add_Action (Table.States (195), 21, (55, 0), 97);
            Add_Action (Table.States (195), 22, (54, 0), 98);
            Add_Action (Table.States (195), 23, (50, 0), 99);
            Add_Action (Table.States (195), 30, Reduce, (57, 1),  3, null, null);
            Add_Action (Table.States (195), 35, (51, 1), 100);
            Add_Action (Table.States (195), 37, (53, 1), 101);
            Table.States (195).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (195), 50, 102);
            Add_Goto (Table.States (195), 51, 162);
            Add_Goto (Table.States (195), 53, 105);
            Add_Goto (Table.States (195), 54, 106);
            Add_Goto (Table.States (195), 55, 107);
            Add_Goto (Table.States (195), 56, 108);
            Table.States (195).Kernel := To_Vector ((((52, 1),  52,  1, (2147483647, 0),  0), ((57, 1),  52,  0, (57,
            1),  3)));
            Table.States (195).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  3)));
            Table.States (196).Action_List.Set_Capacity (11);
            Add_Action (Table.States (196), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (56, 2),  4, null, null);
            Table.States (196).Kernel := To_Vector ((0 => ((56, 2),  26,  0, (56, 2),  4)));
            Table.States (196).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  4)));
            Table.States (197).Action_List.Set_Capacity (11);
            Add_Action (Table.States (197), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (55, 1),  4, null, null);
            Table.States (197).Kernel := To_Vector ((0 => ((55, 1),  27,  0, (55, 1),  4)));
            Table.States (197).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  4)));
            Table.States (198).Action_List.Set_Capacity (11);
            Add_Action (Table.States (198), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (56, 3),  4, null, null);
            Table.States (198).Kernel := To_Vector ((0 => ((56, 3),  33,  0, (56, 3),  4)));
            Table.States (198).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  4)));
            Table.States (199).Action_List.Set_Capacity (1);
            Add_Action (Table.States (199), 19, (50, 0), 219);
            Table.States (199).Kernel := To_Vector ((0 => ((50, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (199).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  19, 219)));
            Table.States (200).Action_List.Set_Capacity (5);
            Add_Action (Table.States (200), (14, 25, 31, 35, 38), (48, 6),  4, rhs_list_6'Access, null);
            Table.States (200).Kernel := To_Vector ((0 => ((48, 6),  6,  0, (48, 6),  4)));
            Table.States (200).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 6),  4)));
            Table.States (201).Action_List.Set_Capacity (2);
            Add_Action (Table.States (201), 7, (48, 5), 220);
            Add_Action (Table.States (201), 18, (48, 4), 221);
            Table.States (201).Kernel := To_Vector ((((48, 4),  35,  2, (2147483647, 0),  0), ((48, 5),  35,  2,
            (2147483647, 0),  0)));
            Table.States (202).Action_List.Set_Capacity (2);
            Add_Action (Table.States (202), 7, (48, 3), 222);
            Add_Action (Table.States (202), 18, (48, 2), 223);
            Table.States (202).Kernel := To_Vector ((((48, 2),  35,  2, (2147483647, 0),  0), ((48, 3),  35,  2,
            (2147483647, 0),  0)));
            Table.States (203).Action_List.Set_Capacity (4);
            Add_Action (Table.States (203), (14, 25, 35, 38), (43, 1),  3, null, null);
            Table.States (203).Kernel := To_Vector ((0 => ((43, 1),  35,  0, (43, 1),  3)));
            Table.States (203).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 1),  3)));
            Table.States (204).Action_List.Set_Capacity (8);
            Add_Action (Table.States (204), (14, 20, 21, 22, 23, 28, 35, 37), (56, 1),  4, null, null);
            Table.States (204).Kernel := To_Vector ((0 => ((56, 1),  24,  0, (56, 1),  4)));
            Table.States (204).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (205).Action_List.Set_Capacity (8);
            Add_Action (Table.States (205), (14, 20, 21, 22, 23, 28, 35, 37), (56, 2),  4, null, null);
            Table.States (205).Kernel := To_Vector ((0 => ((56, 2),  26,  0, (56, 2),  4)));
            Table.States (205).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  4)));
            Table.States (206).Action_List.Set_Capacity (8);
            Add_Action (Table.States (206), (14, 20, 21, 22, 23, 28, 35, 37), (55, 1),  4, null, null);
            Table.States (206).Kernel := To_Vector ((0 => ((55, 1),  27,  0, (55, 1),  4)));
            Table.States (206).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  4)));
            Table.States (207).Action_List.Set_Capacity (8);
            Add_Action (Table.States (207), (14, 20, 21, 22, 23, 28, 35, 37), (56, 3),  4, null, null);
            Table.States (207).Kernel := To_Vector ((0 => ((56, 3),  33,  0, (56, 3),  4)));
            Table.States (207).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  4)));
            Table.States (208).Action_List.Set_Capacity (1);
            Add_Action (Table.States (208), 19, (50, 0), 224);
            Table.States (208).Kernel := To_Vector ((0 => ((50, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (208).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  19, 224)));
            Table.States (209).Action_List.Set_Capacity (8);
            Add_Action (Table.States (209), (14, 20, 21, 22, 23, 29, 35, 37), (56, 1),  4, null, null);
            Table.States (209).Kernel := To_Vector ((0 => ((56, 1),  24,  0, (56, 1),  4)));
            Table.States (209).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (210).Action_List.Set_Capacity (8);
            Add_Action (Table.States (210), (14, 20, 21, 22, 23, 29, 35, 37), (56, 2),  4, null, null);
            Table.States (210).Kernel := To_Vector ((0 => ((56, 2),  26,  0, (56, 2),  4)));
            Table.States (210).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  4)));
            Table.States (211).Action_List.Set_Capacity (8);
            Add_Action (Table.States (211), (14, 20, 21, 22, 23, 29, 35, 37), (55, 1),  4, null, null);
            Table.States (211).Kernel := To_Vector ((0 => ((55, 1),  27,  0, (55, 1),  4)));
            Table.States (211).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  4)));
            Table.States (212).Action_List.Set_Capacity (8);
            Add_Action (Table.States (212), (14, 20, 21, 22, 23, 29, 35, 37), (56, 3),  4, null, null);
            Table.States (212).Kernel := To_Vector ((0 => ((56, 3),  33,  0, (56, 3),  4)));
            Table.States (212).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  4)));
            Table.States (213).Action_List.Set_Capacity (1);
            Add_Action (Table.States (213), 19, (50, 0), 225);
            Table.States (213).Kernel := To_Vector ((0 => ((50, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (213).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  19, 225)));
            Table.States (214).Action_List.Set_Capacity (8);
            Add_Action (Table.States (214), (14, 20, 21, 22, 23, 30, 35, 37), (56, 1),  4, null, null);
            Table.States (214).Kernel := To_Vector ((0 => ((56, 1),  24,  0, (56, 1),  4)));
            Table.States (214).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (215).Action_List.Set_Capacity (8);
            Add_Action (Table.States (215), (14, 20, 21, 22, 23, 30, 35, 37), (56, 2),  4, null, null);
            Table.States (215).Kernel := To_Vector ((0 => ((56, 2),  26,  0, (56, 2),  4)));
            Table.States (215).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  4)));
            Table.States (216).Action_List.Set_Capacity (8);
            Add_Action (Table.States (216), (14, 20, 21, 22, 23, 30, 35, 37), (55, 1),  4, null, null);
            Table.States (216).Kernel := To_Vector ((0 => ((55, 1),  27,  0, (55, 1),  4)));
            Table.States (216).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  4)));
            Table.States (217).Action_List.Set_Capacity (8);
            Add_Action (Table.States (217), (14, 20, 21, 22, 23, 30, 35, 37), (56, 3),  4, null, null);
            Table.States (217).Kernel := To_Vector ((0 => ((56, 3),  33,  0, (56, 3),  4)));
            Table.States (217).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  4)));
            Table.States (218).Action_List.Set_Capacity (1);
            Add_Action (Table.States (218), 19, (50, 0), 226);
            Table.States (218).Kernel := To_Vector ((0 => ((50, 0),  35,  1, (2147483647, 0),  0)));
            Table.States (218).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  19, 226)));
            Table.States (219).Action_List.Set_Capacity (11);
            Add_Action (Table.States (219), (13, 14, 20, 21, 22, 23, 25, 31, 35, 37, 38), (50, 0),  5, null, null);
            Table.States (219).Kernel := To_Vector ((0 => ((50, 0),  19,  0, (50, 0),  5)));
            Table.States (219).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  5)));
            Table.States (220).Action_List.Set_Capacity (1);
            Add_Action (Table.States (220), 35, (43, 0), 227);
            Table.States (220).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (220), 43, 228);
            Table.States (220).Kernel := To_Vector ((0 => ((48, 5),  7,  1, (2147483647, 0),  0)));
            Table.States (220).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 0),  35, 227)));
            Table.States (221).Action_List.Set_Capacity (1);
            Add_Action (Table.States (221), 35, (48, 4), 229);
            Table.States (221).Kernel := To_Vector ((0 => ((48, 4),  18,  1, (2147483647, 0),  0)));
            Table.States (221).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 4),  35, 229)));
            Table.States (222).Action_List.Set_Capacity (1);
            Add_Action (Table.States (222), 35, (43, 0), 227);
            Table.States (222).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (222), 43, 230);
            Table.States (222).Kernel := To_Vector ((0 => ((48, 3),  7,  1, (2147483647, 0),  0)));
            Table.States (222).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 0),  35, 227)));
            Table.States (223).Action_List.Set_Capacity (1);
            Add_Action (Table.States (223), 35, (48, 2), 231);
            Table.States (223).Kernel := To_Vector ((0 => ((48, 2),  18,  1, (2147483647, 0),  0)));
            Table.States (223).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 2),  35, 231)));
            Table.States (224).Action_List.Set_Capacity (8);
            Add_Action (Table.States (224), (14, 20, 21, 22, 23, 28, 35, 37), (50, 0),  5, null, null);
            Table.States (224).Kernel := To_Vector ((0 => ((50, 0),  19,  0, (50, 0),  5)));
            Table.States (224).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  5)));
            Table.States (225).Action_List.Set_Capacity (8);
            Add_Action (Table.States (225), (14, 20, 21, 22, 23, 29, 35, 37), (50, 0),  5, null, null);
            Table.States (225).Kernel := To_Vector ((0 => ((50, 0),  19,  0, (50, 0),  5)));
            Table.States (225).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  5)));
            Table.States (226).Action_List.Set_Capacity (8);
            Add_Action (Table.States (226), (14, 20, 21, 22, 23, 30, 35, 37), (50, 0),  5, null, null);
            Table.States (226).Kernel := To_Vector ((0 => ((50, 0),  19,  0, (50, 0),  5)));
            Table.States (226).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  5)));
            Table.States (227).Action_List.Set_Capacity (5);
            Add_Action (Table.States (227), (14, 25, 31, 35, 38), (43, 0),  1, null, null);
            Table.States (227).Kernel := To_Vector ((0 => ((43, 0),  35,  0, (43, 0),  1)));
            Table.States (227).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 0),  1)));
            Table.States (228).Action_List.Set_Capacity (5);
            Add_Action (Table.States (228), 14, (43, 1), 232);
            Add_Conflict (Table.States (228), 14, (48, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (228), 25, Reduce, (48, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (228), 31, Reduce, (48, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (228), 35, Reduce, (48, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (228), 38, Reduce, (48, 5),  6, rhs_list_5'Access, null);
            Table.States (228).Kernel := To_Vector ((((43, 1),  43,  2, (2147483647, 0),  0), ((48, 5),  43,  0, (48,
            5),  6)));
            Table.States (228).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 5),  6)));
            Table.States (229).Action_List.Set_Capacity (5);
            Add_Action (Table.States (229), (14, 25, 31, 35, 38), (48, 4),  6, rhs_list_4'Access, null);
            Table.States (229).Kernel := To_Vector ((0 => ((48, 4),  35,  0, (48, 4),  6)));
            Table.States (229).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 4),  6)));
            Table.States (230).Action_List.Set_Capacity (5);
            Add_Action (Table.States (230), 14, (43, 1), 232);
            Add_Conflict (Table.States (230), 14, (48, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (230), 25, Reduce, (48, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (230), 31, Reduce, (48, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (230), 35, Reduce, (48, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (230), 38, Reduce, (48, 3),  6, rhs_list_3'Access, null);
            Table.States (230).Kernel := To_Vector ((((43, 1),  43,  2, (2147483647, 0),  0), ((48, 3),  43,  0, (48,
            3),  6)));
            Table.States (230).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 3),  6)));
            Table.States (231).Action_List.Set_Capacity (5);
            Add_Action (Table.States (231), (14, 25, 31, 35, 38), (48, 2),  6, rhs_list_2'Access, null);
            Table.States (231).Kernel := To_Vector ((0 => ((48, 2),  35,  0, (48, 2),  6)));
            Table.States (231).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 2),  6)));
            Table.States (232).Action_List.Set_Capacity (1);
            Add_Action (Table.States (232), 35, (43, 1), 233);
            Table.States (232).Kernel := To_Vector ((0 => ((43, 1),  14,  1, (2147483647, 0),  0)));
            Table.States (232).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 1),  35, 233)));
            Table.States (233).Action_List.Set_Capacity (5);
            Add_Action (Table.States (233), (14, 25, 31, 35, 38), (43, 1),  3, null, null);
            Table.States (233).Kernel := To_Vector ((0 => ((43, 1),  35,  0, (43, 1),  3)));
            Table.States (233).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 1),  3)));
         end Subr_5;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Subr_4;
         Subr_5;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         Language_Fixes,
         Language_Matching_Begin_Tokens,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_1_Process_Main;
