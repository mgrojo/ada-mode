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
         Last_Terminal     => 39,
         First_Nonterminal => 40,
         Last_Nonterminal  => 61,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2,
            2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Minimal_Complete_Cost_Delta => -1,
         Fast_Forward =>  0,
         Matching_Begin =>  0,
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Check_Limit => 4,
         Zombie_Limit => 1,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 235,
         First_Terminal    => 3,
         Last_Terminal     => 39,
         First_Nonterminal => 40,
         Last_Nonterminal  => 61);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 26, (41, 0), 1);
            Add_Action (Table.States (0), 36, (47, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 41, 3);
            Add_Goto (Table.States (0), 47, 4);
            Add_Goto (Table.States (0), 59, 5);
            Add_Goto (Table.States (0), 60, 6);
            Table.States (1).Action_List.Set_Capacity (9);
            Add_Action (Table.States (1), 3, (41, 2), 7);
            Add_Action (Table.States (1), 4, (41, 1), 8);
            Add_Action (Table.States (1), 5, (41, 9), 9);
            Add_Action (Table.States (1), 6, (41, 7), 10);
            Add_Action (Table.States (1), 7, (41, 5), 11);
            Add_Action (Table.States (1), 9, (42, 0), 12);
            Add_Action (Table.States (1), 10, (42, 1), 13);
            Add_Action (Table.States (1), 11, (42, 2), 14);
            Add_Action (Table.States (1), 36, (41, 3), 15);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 42, 16);
            Table.States (1).Kernel := To_Vector ((((41, 0),  26,  3, (2147483647, 0),  0), ((41, 1),  26,  2,
            (2147483647, 0),  0), ((41, 2),  26,  3, (2147483647, 0),  0), ((41, 3),  26,  2, (2147483647, 0),  0),
            ((41, 4),  26,  1, (2147483647, 0),  0), ((41, 5),  26,  4, (2147483647, 0),  0), ((41, 6),  26,  4,
            (2147483647, 0),  0), ((41, 7),  26,  4, (2147483647, 0),  0), ((41, 8),  26,  4, (2147483647, 0),  0),
            ((41, 9),  26,  2, (2147483647, 0),  0)));
            Table.States (1).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 3),  36, 15)));
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 16, (61, 0), 17);
            Add_Action (Table.States (2), 17, (61, 1), 18);
            Table.States (2).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (2), 61, 19);
            Table.States (2).Kernel := To_Vector ((0 => ((47, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (2).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (61, 0),  16, 17)));
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (26, 36, 39), (59, 0),  1, null, null);
            Table.States (3).Kernel := To_Vector ((0 => ((59, 0),  41,  0, (59, 0),  1)));
            Table.States (3).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (26, 36, 39), (59, 1),  1, null, null);
            Table.States (4).Kernel := To_Vector ((0 => ((59, 1),  47,  0, (59, 1),  1)));
            Table.States (4).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  1)));
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (26, 36, 39), (60, 0),  1, compilation_unit_list_0'Access, null);
            Table.States (5).Kernel := To_Vector ((0 => ((60, 0),  59,  0, (60, 0),  1)));
            Table.States (5).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 0),  1)));
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 26, (41, 0), 1);
            Add_Action (Table.States (6), 36, (47, 0), 2);
            Add_Action (Table.States (6), 39, Accept_It, (40, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 41, 3);
            Add_Goto (Table.States (6), 47, 4);
            Add_Goto (Table.States (6), 59, 20);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 36, (43, 0), 21);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 43, 22);
            Table.States (7).Kernel := To_Vector ((0 => ((41, 2),  3,  2, (2147483647, 0),  0)));
            Table.States (7).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (43, 0),  36, 21)));
            Table.States (8).Action_List.Set_Capacity (13);
            Add_Action (Table.States (8), 8, (46, 3), 23);
            Add_Action (Table.States (8), 11, (46, 12), 24);
            Add_Action (Table.States (8), 13, (46, 7), 25);
            Add_Action (Table.States (8), 15, (46, 0), 26);
            Add_Action (Table.States (8), 18, (46, 1), 27);
            Add_Action (Table.States (8), 19, (46, 4), 28);
            Add_Action (Table.States (8), 23, (46, 5), 29);
            Add_Action (Table.States (8), 31, (46, 8), 30);
            Add_Action (Table.States (8), 33, (46, 9), 31);
            Add_Action (Table.States (8), 35, (46, 6), 32);
            Add_Action (Table.States (8), 36, (46, 2), 33);
            Add_Action (Table.States (8), 37, (46, 10), 34);
            Add_Action (Table.States (8), 38, (46, 11), 35);
            Table.States (8).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (8), 45, 36);
            Add_Goto (Table.States (8), 46, 37);
            Table.States (8).Kernel := To_Vector ((0 => ((41, 1),  4,  1, (2147483647, 0),  0)));
            Table.States (8).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (46, 0),  15, 26)));
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 7, (41, 9), 38);
            Table.States (9).Kernel := To_Vector ((0 => ((41, 9),  5,  1, (2147483647, 0),  0)));
            Table.States (9).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 9),  7, 38)));
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 36, (41, 7), 39);
            Table.States (10).Kernel := To_Vector ((((41, 7),  6,  3, (2147483647, 0),  0), ((41, 8),  6,  3,
            (2147483647, 0),  0)));
            Table.States (10).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 7),  36, 39)));
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 36, (41, 5), 40);
            Table.States (11).Kernel := To_Vector ((((41, 5),  7,  3, (2147483647, 0),  0), ((41, 6),  7,  3,
            (2147483647, 0),  0)));
            Table.States (11).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 5),  36, 40)));
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), (1 =>  36), (42, 0),  1, null, null);
            Table.States (12).Kernel := To_Vector ((0 => ((42, 0),  9,  0, (42, 0),  1)));
            Table.States (12).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 0),  1)));
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 24, (42, 1), 41);
            Table.States (13).Kernel := To_Vector ((0 => ((42, 1),  10,  3, (2147483647, 0),  0)));
            Table.States (13).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 1),  24, 41)));
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 24, (42, 2), 42);
            Table.States (14).Kernel := To_Vector ((0 => ((42, 2),  11,  3, (2147483647, 0),  0)));
            Table.States (14).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 2),  24, 42)));
            Table.States (15).Action_List.Set_Capacity (15);
            Add_Action (Table.States (15), 8, (46, 3), 23);
            Add_Action (Table.States (15), 11, (46, 12), 24);
            Add_Action (Table.States (15), 13, (46, 7), 25);
            Add_Action (Table.States (15), 15, (46, 0), 26);
            Add_Action (Table.States (15), 18, (46, 1), 27);
            Add_Action (Table.States (15), 19, (46, 4), 28);
            Add_Action (Table.States (15), 23, (46, 5), 29);
            Add_Action (Table.States (15), 26, Reduce, (41, 4),  2, declaration_4'Access, null);
            Add_Action (Table.States (15), 31, (46, 8), 30);
            Add_Action (Table.States (15), 33, (46, 9), 31);
            Add_Action (Table.States (15), 35, (46, 6), 32);
            Add_Action (Table.States (15), 36, (46, 2), 33);
            Add_Conflict (Table.States (15), 36, (41, 4),  2, declaration_4'Access, null);
            Add_Action (Table.States (15), 37, (46, 10), 34);
            Add_Action (Table.States (15), 38, (46, 11), 35);
            Add_Action (Table.States (15), 39, Reduce, (41, 4),  2, declaration_4'Access, null);
            Table.States (15).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (15), 45, 43);
            Add_Goto (Table.States (15), 46, 37);
            Table.States (15).Kernel := To_Vector ((((41, 3),  36,  1, (2147483647, 0),  0), ((41, 4),  36,  0, (41,
            4),  2)));
            Table.States (15).Minimal_Complete_Actions := To_Vector (((Shift, (46, 0),  15, 26), (Reduce, (41, 4),
            2)));
            Table.States (16).Action_List.Set_Capacity (1);
            Add_Action (Table.States (16), 36, (41, 0), 44);
            Table.States (16).Kernel := To_Vector ((0 => ((41, 0),  42,  2, (2147483647, 0),  0)));
            Table.States (16).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 0),  36, 44)));
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), (15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (61, 0),  1,
            nonterminal_009_0'Access, null);
            Table.States (17).Kernel := To_Vector ((0 => ((61, 0),  16,  0, (61, 0),  1)));
            Table.States (17).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (61, 0),  1)));
            Table.States (18).Action_List.Set_Capacity (10);
            Add_Action (Table.States (18), (15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (61, 1),  1,
            nonterminal_009_1'Access, null);
            Table.States (18).Kernel := To_Vector ((0 => ((61, 1),  17,  0, (61, 1),  1)));
            Table.States (18).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (61, 1),  1)));
            Table.States (19).Action_List.Set_Capacity (10);
            Add_Action (Table.States (19), 15, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (19), 21, (57, 0), 45);
            Add_Action (Table.States (19), 22, (56, 0), 46);
            Add_Action (Table.States (19), 23, (55, 0), 47);
            Add_Action (Table.States (19), 24, (51, 0), 48);
            Add_Action (Table.States (19), 26, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (19), 32, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (19), 36, (52, 1), 49);
            Add_Conflict (Table.States (19), 36, (50, 0),  0, null, null);
            Add_Action (Table.States (19), 38, (54, 1), 50);
            Add_Action (Table.States (19), 39, Reduce, (50, 0),  0, null, null);
            Table.States (19).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (19), 49, 51);
            Add_Goto (Table.States (19), 50, 52);
            Add_Goto (Table.States (19), 51, 53);
            Add_Goto (Table.States (19), 52, 54);
            Add_Goto (Table.States (19), 53, 55);
            Add_Goto (Table.States (19), 54, 56);
            Add_Goto (Table.States (19), 55, 57);
            Add_Goto (Table.States (19), 56, 58);
            Add_Goto (Table.States (19), 57, 59);
            Table.States (19).Kernel := To_Vector ((0 => ((47, 0),  61,  0, (49, 6),  4)));
            Table.States (19).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 6),  4)));
            Table.States (20).Action_List.Set_Capacity (3);
            Add_Action (Table.States (20), (26, 36, 39), (60, 1),  2, compilation_unit_list_1'Access, null);
            Table.States (20).Kernel := To_Vector ((0 => ((60, 1),  59,  0, (60, 1),  2)));
            Table.States (20).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 1),  2)));
            Table.States (21).Action_List.Set_Capacity (2);
            Add_Action (Table.States (21), (12, 36), (43, 0),  1, null, null);
            Table.States (21).Kernel := To_Vector ((0 => ((43, 0),  36,  0, (43, 0),  1)));
            Table.States (21).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 0),  1)));
            Table.States (22).Action_List.Set_Capacity (2);
            Add_Action (Table.States (22), 12, (41, 2), 60);
            Add_Action (Table.States (22), 36, (43, 1), 61);
            Table.States (22).Kernel := To_Vector ((((41, 2),  43,  1, (2147483647, 0),  0), ((43, 1),  43,  1,
            (2147483647, 0),  0)));
            Table.States (22).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 2),  12, 60)));
            Table.States (23).Action_List.Set_Capacity (15);
            Add_Action (Table.States (23), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 3),  1,
            null, null);
            Table.States (23).Kernel := To_Vector ((0 => ((46, 3),  8,  0, (46, 3),  1)));
            Table.States (23).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 3),  1)));
            Table.States (24).Action_List.Set_Capacity (15);
            Add_Action (Table.States (24), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 12),  1,
            null, null);
            Table.States (24).Kernel := To_Vector ((0 => ((46, 12),  11,  0, (46, 12),  1)));
            Table.States (24).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 12),  1)));
            Table.States (25).Action_List.Set_Capacity (15);
            Add_Action (Table.States (25), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 7),  1,
            declaration_item_7'Access, null);
            Table.States (25).Kernel := To_Vector ((0 => ((46, 7),  13,  0, (46, 7),  1)));
            Table.States (25).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 7),  1)));
            Table.States (26).Action_List.Set_Capacity (15);
            Add_Action (Table.States (26), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 0),  1,
            null, null);
            Table.States (26).Kernel := To_Vector ((0 => ((46, 0),  15,  0, (46, 0),  1)));
            Table.States (26).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 0),  1)));
            Table.States (27).Action_List.Set_Capacity (15);
            Add_Action (Table.States (27), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 1),  1,
            null, null);
            Table.States (27).Kernel := To_Vector ((0 => ((46, 1),  18,  0, (46, 1),  1)));
            Table.States (27).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 1),  1)));
            Table.States (28).Action_List.Set_Capacity (15);
            Add_Action (Table.States (28), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 4),  1,
            null, null);
            Table.States (28).Kernel := To_Vector ((0 => ((46, 4),  19,  0, (46, 4),  1)));
            Table.States (28).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 4),  1)));
            Table.States (29).Action_List.Set_Capacity (15);
            Add_Action (Table.States (29), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 5),  1,
            null, null);
            Table.States (29).Kernel := To_Vector ((0 => ((46, 5),  23,  0, (46, 5),  1)));
            Table.States (29).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 5),  1)));
            Table.States (30).Action_List.Set_Capacity (15);
            Add_Action (Table.States (30), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 8),  1,
            null, null);
            Table.States (30).Kernel := To_Vector ((0 => ((46, 8),  31,  0, (46, 8),  1)));
            Table.States (30).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 8),  1)));
            Table.States (31).Action_List.Set_Capacity (15);
            Add_Action (Table.States (31), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 9),  1,
            null, null);
            Table.States (31).Kernel := To_Vector ((0 => ((46, 9),  33,  0, (46, 9),  1)));
            Table.States (31).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 9),  1)));
            Table.States (32).Action_List.Set_Capacity (15);
            Add_Action (Table.States (32), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 6),  1,
            null, null);
            Table.States (32).Kernel := To_Vector ((0 => ((46, 6),  35,  0, (46, 6),  1)));
            Table.States (32).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 6),  1)));
            Table.States (33).Action_List.Set_Capacity (15);
            Add_Action (Table.States (33), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 2),  1,
            null, null);
            Table.States (33).Kernel := To_Vector ((0 => ((46, 2),  36,  0, (46, 2),  1)));
            Table.States (33).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 2),  1)));
            Table.States (34).Action_List.Set_Capacity (15);
            Add_Action (Table.States (34), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 10),  1,
            declaration_item_10'Access, null);
            Table.States (34).Kernel := To_Vector ((0 => ((46, 10),  37,  0, (46, 10),  1)));
            Table.States (34).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 10),  1)));
            Table.States (35).Action_List.Set_Capacity (15);
            Add_Action (Table.States (35), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (46, 11),  1,
            declaration_item_11'Access, null);
            Table.States (35).Kernel := To_Vector ((0 => ((46, 11),  38,  0, (46, 11),  1)));
            Table.States (35).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 11),  1)));
            Table.States (36).Action_List.Set_Capacity (15);
            Add_Action (Table.States (36), 8, (46, 3), 23);
            Add_Action (Table.States (36), 11, (46, 12), 24);
            Add_Action (Table.States (36), 13, (46, 7), 25);
            Add_Action (Table.States (36), 15, (46, 0), 26);
            Add_Action (Table.States (36), 18, (46, 1), 27);
            Add_Action (Table.States (36), 19, (46, 4), 28);
            Add_Action (Table.States (36), 23, (46, 5), 29);
            Add_Action (Table.States (36), 26, Reduce, (41, 1),  3, declaration_1'Access, null);
            Add_Action (Table.States (36), 31, (46, 8), 30);
            Add_Action (Table.States (36), 33, (46, 9), 31);
            Add_Action (Table.States (36), 35, (46, 6), 32);
            Add_Action (Table.States (36), 36, (46, 2), 33);
            Add_Conflict (Table.States (36), 36, (41, 1),  3, declaration_1'Access, null);
            Add_Action (Table.States (36), 37, (46, 10), 34);
            Add_Action (Table.States (36), 38, (46, 11), 35);
            Add_Action (Table.States (36), 39, Reduce, (41, 1),  3, declaration_1'Access, null);
            Table.States (36).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (36), 46, 62);
            Table.States (36).Kernel := To_Vector ((((41, 1),  45,  0, (41, 1),  3), ((45, 1),  45,  1, (2147483647,
            0),  0)));
            Table.States (36).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 1),  3)));
            Table.States (37).Action_List.Set_Capacity (15);
            Add_Action (Table.States (37), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (45, 0),  1,
            null, null);
            Table.States (37).Kernel := To_Vector ((0 => ((45, 0),  46,  0, (45, 0),  1)));
            Table.States (37).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 0),  1)));
            Table.States (38).Action_List.Set_Capacity (3);
            Add_Action (Table.States (38), (26, 36, 39), (41, 9),  3, declaration_9'Access, null);
            Table.States (38).Kernel := To_Vector ((0 => ((41, 9),  7,  0, (41, 9),  3)));
            Table.States (38).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 9),  3)));
            Table.States (39).Action_List.Set_Capacity (2);
            Add_Action (Table.States (39), 8, (41, 8), 63);
            Add_Action (Table.States (39), 19, (41, 7), 64);
            Table.States (39).Kernel := To_Vector ((((41, 7),  36,  2, (2147483647, 0),  0), ((41, 8),  36,  2,
            (2147483647, 0),  0)));
            Table.States (39).Minimal_Complete_Actions := To_Vector (((Shift, (41, 7),  19, 64), (Shift, (41, 8),  8,
            63)));
            Table.States (40).Action_List.Set_Capacity (2);
            Add_Action (Table.States (40), 8, (41, 6), 65);
            Add_Action (Table.States (40), 19, (41, 5), 66);
            Table.States (40).Kernel := To_Vector ((((41, 5),  36,  2, (2147483647, 0),  0), ((41, 6),  36,  2,
            (2147483647, 0),  0)));
            Table.States (40).Minimal_Complete_Actions := To_Vector (((Shift, (41, 5),  19, 66), (Shift, (41, 6),  8,
            65)));
            Table.States (41).Action_List.Set_Capacity (1);
            Add_Action (Table.States (41), 36, (42, 1), 67);
            Table.States (41).Kernel := To_Vector ((0 => ((42, 1),  24,  2, (2147483647, 0),  0)));
            Table.States (41).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 1),  36, 67)));
            Table.States (42).Action_List.Set_Capacity (1);
            Add_Action (Table.States (42), 36, (42, 2), 68);
            Table.States (42).Kernel := To_Vector ((0 => ((42, 2),  24,  2, (2147483647, 0),  0)));
            Table.States (42).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 2),  36, 68)));
            Table.States (43).Action_List.Set_Capacity (15);
            Add_Action (Table.States (43), 8, (46, 3), 23);
            Add_Action (Table.States (43), 11, (46, 12), 24);
            Add_Action (Table.States (43), 13, (46, 7), 25);
            Add_Action (Table.States (43), 15, (46, 0), 26);
            Add_Action (Table.States (43), 18, (46, 1), 27);
            Add_Action (Table.States (43), 19, (46, 4), 28);
            Add_Action (Table.States (43), 23, (46, 5), 29);
            Add_Action (Table.States (43), 26, Reduce, (41, 3),  3, declaration_3'Access, null);
            Add_Action (Table.States (43), 31, (46, 8), 30);
            Add_Action (Table.States (43), 33, (46, 9), 31);
            Add_Action (Table.States (43), 35, (46, 6), 32);
            Add_Action (Table.States (43), 36, (46, 2), 33);
            Add_Conflict (Table.States (43), 36, (41, 3),  3, declaration_3'Access, null);
            Add_Action (Table.States (43), 37, (46, 10), 34);
            Add_Action (Table.States (43), 38, (46, 11), 35);
            Add_Action (Table.States (43), 39, Reduce, (41, 3),  3, declaration_3'Access, null);
            Table.States (43).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (43), 46, 62);
            Table.States (43).Kernel := To_Vector ((((41, 3),  45,  0, (41, 3),  3), ((45, 1),  45,  1, (2147483647,
            0),  0)));
            Table.States (43).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 3),  3)));
            Table.States (44).Action_List.Set_Capacity (13);
            Add_Action (Table.States (44), 8, (46, 3), 23);
            Add_Action (Table.States (44), 11, (46, 12), 24);
            Add_Action (Table.States (44), 13, (46, 7), 25);
            Add_Action (Table.States (44), 15, (46, 0), 26);
            Add_Action (Table.States (44), 18, (46, 1), 27);
            Add_Action (Table.States (44), 19, (46, 4), 28);
            Add_Action (Table.States (44), 23, (46, 5), 29);
            Add_Action (Table.States (44), 31, (46, 8), 30);
            Add_Action (Table.States (44), 33, (46, 9), 31);
            Add_Action (Table.States (44), 35, (46, 6), 32);
            Add_Action (Table.States (44), 36, (46, 2), 33);
            Add_Action (Table.States (44), 37, (46, 10), 34);
            Add_Action (Table.States (44), 38, (46, 11), 35);
            Table.States (44).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (44), 45, 69);
            Add_Goto (Table.States (44), 46, 37);
            Table.States (44).Kernel := To_Vector ((0 => ((41, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (44).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (46, 0),  15, 26)));
            Table.States (45).Action_List.Set_Capacity (6);
            Add_Action (Table.States (45), 21, (57, 0), 70);
            Add_Action (Table.States (45), 22, (56, 0), 71);
            Add_Action (Table.States (45), 23, (55, 0), 72);
            Add_Action (Table.States (45), 24, (51, 0), 73);
            Add_Action (Table.States (45), 36, (52, 1), 74);
            Add_Action (Table.States (45), 38, (54, 1), 75);
            Table.States (45).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (45), 51, 76);
            Add_Goto (Table.States (45), 52, 77);
            Add_Goto (Table.States (45), 53, 78);
            Add_Goto (Table.States (45), 54, 79);
            Add_Goto (Table.States (45), 55, 80);
            Add_Goto (Table.States (45), 56, 81);
            Add_Goto (Table.States (45), 57, 82);
            Add_Goto (Table.States (45), 58, 83);
            Table.States (45).Kernel := To_Vector ((((57, 0),  21,  2, (2147483647, 0),  0), ((57, 1),  21,  3,
            (2147483647, 0),  0)));
            Table.States (45).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 74)));
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (46).Action_List.Set_Capacity (6);
            Add_Action (Table.States (46), 21, (57, 0), 84);
            Add_Action (Table.States (46), 22, (56, 0), 85);
            Add_Action (Table.States (46), 23, (55, 0), 86);
            Add_Action (Table.States (46), 24, (51, 0), 87);
            Add_Action (Table.States (46), 36, (52, 1), 88);
            Add_Action (Table.States (46), 38, (54, 1), 89);
            Table.States (46).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (46), 51, 90);
            Add_Goto (Table.States (46), 52, 91);
            Add_Goto (Table.States (46), 53, 92);
            Add_Goto (Table.States (46), 54, 93);
            Add_Goto (Table.States (46), 55, 94);
            Add_Goto (Table.States (46), 56, 95);
            Add_Goto (Table.States (46), 57, 96);
            Add_Goto (Table.States (46), 58, 97);
            Table.States (46).Kernel := To_Vector ((0 => ((56, 0),  22,  2, (2147483647, 0),  0)));
            Table.States (46).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 88)));
            Table.States (47).Action_List.Set_Capacity (6);
            Add_Action (Table.States (47), 21, (57, 0), 98);
            Add_Action (Table.States (47), 22, (56, 0), 99);
            Add_Action (Table.States (47), 23, (55, 0), 100);
            Add_Action (Table.States (47), 24, (51, 0), 101);
            Add_Action (Table.States (47), 36, (52, 1), 102);
            Add_Action (Table.States (47), 38, (54, 1), 103);
            Table.States (47).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (47), 51, 104);
            Add_Goto (Table.States (47), 52, 105);
            Add_Goto (Table.States (47), 53, 106);
            Add_Goto (Table.States (47), 54, 107);
            Add_Goto (Table.States (47), 55, 108);
            Add_Goto (Table.States (47), 56, 109);
            Add_Goto (Table.States (47), 57, 110);
            Add_Goto (Table.States (47), 58, 111);
            Table.States (47).Kernel := To_Vector ((((55, 0),  23,  2, (2147483647, 0),  0), ((56, 1),  23,  3,
            (2147483647, 0),  0), ((57, 2),  23,  3, (2147483647, 0),  0), ((57, 3),  23,  3, (2147483647, 0),  0)));
            Table.States (47).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 102)));
            Table.States (48).Action_List.Set_Capacity (1);
            Add_Action (Table.States (48), 36, (51, 0), 112);
            Table.States (48).Kernel := To_Vector ((0 => ((51, 0),  24,  4, (2147483647, 0),  0)));
            Table.States (48).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 112)));
            Table.States (49).Action_List.Set_Capacity (15);
            Add_Action (Table.States (49), 14, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 19, (52, 1), 113);
            Add_Action (Table.States (49), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 26, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 27, (57, 4), 114);
            Add_Action (Table.States (49), 28, (56, 2), 115);
            Add_Action (Table.States (49), 32, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 34, (57, 5), 116);
            Add_Action (Table.States (49), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 38, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (49), 39, Reduce, (54, 0),  1, null, null);
            Table.States (49).Kernel := To_Vector ((((52, 1),  36,  2, (2147483647, 0),  0), ((54, 0),  36,  0, (54,
            0),  1), ((56, 2),  36,  1, (2147483647, 0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,
            1, (2147483647, 0),  0)));
            Table.States (49).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            115), (Shift, (57, 4),  27, 114), (Shift, (57, 5),  34, 116)));
            Table.States (50).Action_List.Set_Capacity (12);
            Add_Action (Table.States (50), 14, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 15, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 21, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 22, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 23, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 24, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 26, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 28, (56, 3), 117);
            Add_Action (Table.States (50), 32, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 36, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 38, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (50), 39, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Table.States (50).Kernel := To_Vector ((((54, 1),  38,  0, (54, 1),  1), ((56, 3),  38,  1, (2147483647,
            0),  0)));
            Table.States (50).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 1),  1), (Shift, (56, 3),  28,
            117)));
            Table.States (51).Action_List.Set_Capacity (5);
            Add_Action (Table.States (51), 15, (49, 1), 118);
            Add_Action (Table.States (51), 26, (49, 2), 119);
            Add_Conflict (Table.States (51), 26, (48, 1),  0, null, null);
            Add_Action (Table.States (51), 32, (48, 0), 120);
            Add_Action (Table.States (51), 36, Reduce, (48, 1),  0, null, null);
            Add_Action (Table.States (51), 39, Reduce, (48, 1),  0, null, null);
            Table.States (51).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (51), 48, 121);
            Table.States (51).Kernel := To_Vector ((((47, 0),  49,  0, (48, 1),  0), ((49, 1),  49,  1, (2147483647,
            0),  0), ((49, 2),  49,  5, (2147483647, 0),  0), ((49, 3),  49,  5, (2147483647, 0),  0), ((49, 4),  49,
            5, (2147483647, 0),  0), ((49, 5),  49,  5, (2147483647, 0),  0), ((49, 6),  49,  3, (2147483647, 0),  0)));
            Table.States (51).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  0)));
            Table.States (52).Action_List.Set_Capacity (5);
            Add_Action (Table.States (52), (15, 26, 32, 36, 39), (49, 0),  1, rhs_list_0'Access, null);
            Table.States (52).Kernel := To_Vector ((0 => ((49, 0),  50,  0, (49, 0),  1)));
            Table.States (52).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (53).Action_List.Set_Capacity (11);
            Add_Action (Table.States (53), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (54, 2),  1, null, null);
            Table.States (53).Kernel := To_Vector ((0 => ((54, 2),  51,  0, (54, 2),  1)));
            Table.States (53).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 2),  1)));
            Table.States (54).Action_List.Set_Capacity (11);
            Add_Action (Table.States (54), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (53, 0),  1, null, null);
            Table.States (54).Kernel := To_Vector ((0 => ((53, 0),  52,  0, (53, 0),  1)));
            Table.States (54).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 0),  1)));
            Table.States (55).Action_List.Set_Capacity (11);
            Add_Action (Table.States (55), 14, (50, 2), 122);
            Add_Action (Table.States (55), 15, Reduce, (50, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (55), 21, (57, 0), 45);
            Add_Action (Table.States (55), 22, (56, 0), 46);
            Add_Action (Table.States (55), 23, (55, 0), 47);
            Add_Action (Table.States (55), 24, (51, 0), 48);
            Add_Action (Table.States (55), 26, Reduce, (50, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (55), 32, Reduce, (50, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (55), 36, (52, 1), 49);
            Add_Conflict (Table.States (55), 36, (50, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (55), 38, (54, 1), 50);
            Add_Action (Table.States (55), 39, Reduce, (50, 1),  1, rhs_1'Access, null);
            Table.States (55).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (55), 51, 53);
            Add_Goto (Table.States (55), 52, 123);
            Add_Goto (Table.States (55), 54, 56);
            Add_Goto (Table.States (55), 55, 57);
            Add_Goto (Table.States (55), 56, 58);
            Add_Goto (Table.States (55), 57, 59);
            Table.States (55).Kernel := To_Vector ((((50, 1),  53,  0, (50, 1),  1), ((50, 2),  53,  1, (2147483647,
            0),  0), ((50, 3),  53,  2, (2147483647, 0),  0), ((53, 1),  53,  1, (2147483647, 0),  0)));
            Table.States (55).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 1),  1), (Shift, (50, 2),  14,
            122)));
            Table.States (56).Action_List.Set_Capacity (11);
            Add_Action (Table.States (56), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (52, 0),  1, null, null);
            Table.States (56).Kernel := To_Vector ((0 => ((52, 0),  54,  0, (52, 0),  1)));
            Table.States (56).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (57).Action_List.Set_Capacity (11);
            Add_Action (Table.States (57), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (54, 5),  1, null, null);
            Table.States (57).Kernel := To_Vector ((0 => ((54, 5),  55,  0, (54, 5),  1)));
            Table.States (57).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 5),  1)));
            Table.States (58).Action_List.Set_Capacity (11);
            Add_Action (Table.States (58), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (54, 3),  1, null, null);
            Table.States (58).Kernel := To_Vector ((0 => ((54, 3),  56,  0, (54, 3),  1)));
            Table.States (58).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 3),  1)));
            Table.States (59).Action_List.Set_Capacity (11);
            Add_Action (Table.States (59), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (54, 4),  1, null, null);
            Table.States (59).Kernel := To_Vector ((0 => ((54, 4),  57,  0, (54, 4),  1)));
            Table.States (59).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 4),  1)));
            Table.States (60).Action_List.Set_Capacity (3);
            Add_Action (Table.States (60), (26, 36, 39), (41, 2),  4, declaration_2'Access, null);
            Table.States (60).Kernel := To_Vector ((0 => ((41, 2),  12,  0, (41, 2),  4)));
            Table.States (60).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 2),  4)));
            Table.States (61).Action_List.Set_Capacity (2);
            Add_Action (Table.States (61), (12, 36), (43, 1),  2, null, null);
            Table.States (61).Kernel := To_Vector ((0 => ((43, 1),  36,  0, (43, 1),  2)));
            Table.States (61).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 1),  2)));
            Table.States (62).Action_List.Set_Capacity (15);
            Add_Action (Table.States (62), (8, 11, 13, 15, 18, 19, 23, 26, 31, 33, 35, 36, 37, 38, 39), (45, 1),  2,
            null, null);
            Table.States (62).Kernel := To_Vector ((0 => ((45, 1),  46,  0, (45, 1),  2)));
            Table.States (62).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 1),  2)));
            Table.States (63).Action_List.Set_Capacity (1);
            Add_Action (Table.States (63), 36, (44, 0), 124);
            Table.States (63).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (63), 44, 125);
            Table.States (63).Kernel := To_Vector ((0 => ((41, 8),  8,  1, (2147483647, 0),  0)));
            Table.States (63).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 0),  36, 124)));
            Table.States (64).Action_List.Set_Capacity (1);
            Add_Action (Table.States (64), 36, (41, 7), 126);
            Table.States (64).Kernel := To_Vector ((0 => ((41, 7),  19,  1, (2147483647, 0),  0)));
            Table.States (64).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 7),  36, 126)));
            Table.States (65).Action_List.Set_Capacity (1);
            Add_Action (Table.States (65), 36, (44, 0), 124);
            Table.States (65).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (65), 44, 127);
            Table.States (65).Kernel := To_Vector ((0 => ((41, 6),  8,  1, (2147483647, 0),  0)));
            Table.States (65).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 0),  36, 124)));
            Table.States (66).Action_List.Set_Capacity (1);
            Add_Action (Table.States (66), 36, (41, 5), 128);
            Table.States (66).Kernel := To_Vector ((0 => ((41, 5),  19,  1, (2147483647, 0),  0)));
            Table.States (66).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (41, 5),  36, 128)));
            Table.States (67).Action_List.Set_Capacity (1);
            Add_Action (Table.States (67), 20, (42, 1), 129);
            Table.States (67).Kernel := To_Vector ((0 => ((42, 1),  36,  1, (2147483647, 0),  0)));
            Table.States (67).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 1),  20, 129)));
            Table.States (68).Action_List.Set_Capacity (1);
            Add_Action (Table.States (68), 20, (42, 2), 130);
            Table.States (68).Kernel := To_Vector ((0 => ((42, 2),  36,  1, (2147483647, 0),  0)));
            Table.States (68).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 2),  20, 130)));
            Table.States (69).Action_List.Set_Capacity (15);
            Add_Action (Table.States (69), 8, (46, 3), 23);
            Add_Action (Table.States (69), 11, (46, 12), 24);
            Add_Action (Table.States (69), 13, (46, 7), 25);
            Add_Action (Table.States (69), 15, (46, 0), 26);
            Add_Action (Table.States (69), 18, (46, 1), 27);
            Add_Action (Table.States (69), 19, (46, 4), 28);
            Add_Action (Table.States (69), 23, (46, 5), 29);
            Add_Action (Table.States (69), 26, Reduce, (41, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (69), 31, (46, 8), 30);
            Add_Action (Table.States (69), 33, (46, 9), 31);
            Add_Action (Table.States (69), 35, (46, 6), 32);
            Add_Action (Table.States (69), 36, (46, 2), 33);
            Add_Conflict (Table.States (69), 36, (41, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (69), 37, (46, 10), 34);
            Add_Action (Table.States (69), 38, (46, 11), 35);
            Add_Action (Table.States (69), 39, Reduce, (41, 0),  4, declaration_0'Access, null);
            Table.States (69).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (69), 46, 62);
            Table.States (69).Kernel := To_Vector ((((41, 0),  45,  0, (41, 0),  4), ((45, 1),  45,  1, (2147483647,
            0),  0)));
            Table.States (69).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 0),  4)));
            Table.States (70).Action_List.Set_Capacity (6);
            Add_Action (Table.States (70), 21, (57, 0), 70);
            Add_Action (Table.States (70), 22, (56, 0), 71);
            Add_Action (Table.States (70), 23, (55, 0), 72);
            Add_Action (Table.States (70), 24, (51, 0), 73);
            Add_Action (Table.States (70), 36, (52, 1), 74);
            Add_Action (Table.States (70), 38, (54, 1), 75);
            Table.States (70).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (70), 51, 76);
            Add_Goto (Table.States (70), 52, 77);
            Add_Goto (Table.States (70), 53, 78);
            Add_Goto (Table.States (70), 54, 79);
            Add_Goto (Table.States (70), 55, 80);
            Add_Goto (Table.States (70), 56, 81);
            Add_Goto (Table.States (70), 57, 82);
            Add_Goto (Table.States (70), 58, 131);
            Table.States (70).Kernel := To_Vector ((((57, 0),  21,  2, (2147483647, 0),  0), ((57, 1),  21,  3,
            (2147483647, 0),  0)));
            Table.States (70).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 74)));
            Table.States (71).Action_List.Set_Capacity (6);
            Add_Action (Table.States (71), 21, (57, 0), 84);
            Add_Action (Table.States (71), 22, (56, 0), 85);
            Add_Action (Table.States (71), 23, (55, 0), 86);
            Add_Action (Table.States (71), 24, (51, 0), 87);
            Add_Action (Table.States (71), 36, (52, 1), 88);
            Add_Action (Table.States (71), 38, (54, 1), 89);
            Table.States (71).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (71), 51, 90);
            Add_Goto (Table.States (71), 52, 91);
            Add_Goto (Table.States (71), 53, 92);
            Add_Goto (Table.States (71), 54, 93);
            Add_Goto (Table.States (71), 55, 94);
            Add_Goto (Table.States (71), 56, 95);
            Add_Goto (Table.States (71), 57, 96);
            Add_Goto (Table.States (71), 58, 132);
            Table.States (71).Kernel := To_Vector ((0 => ((56, 0),  22,  2, (2147483647, 0),  0)));
            Table.States (71).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 88)));
            Table.States (72).Action_List.Set_Capacity (6);
            Add_Action (Table.States (72), 21, (57, 0), 98);
            Add_Action (Table.States (72), 22, (56, 0), 99);
            Add_Action (Table.States (72), 23, (55, 0), 100);
            Add_Action (Table.States (72), 24, (51, 0), 101);
            Add_Action (Table.States (72), 36, (52, 1), 102);
            Add_Action (Table.States (72), 38, (54, 1), 103);
            Table.States (72).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (72), 51, 104);
            Add_Goto (Table.States (72), 52, 105);
            Add_Goto (Table.States (72), 53, 106);
            Add_Goto (Table.States (72), 54, 107);
            Add_Goto (Table.States (72), 55, 108);
            Add_Goto (Table.States (72), 56, 109);
            Add_Goto (Table.States (72), 57, 110);
            Add_Goto (Table.States (72), 58, 133);
            Table.States (72).Kernel := To_Vector ((((55, 0),  23,  2, (2147483647, 0),  0), ((56, 1),  23,  3,
            (2147483647, 0),  0), ((57, 2),  23,  3, (2147483647, 0),  0), ((57, 3),  23,  3, (2147483647, 0),  0)));
            Table.States (72).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 102)));
            Table.States (73).Action_List.Set_Capacity (1);
            Add_Action (Table.States (73), 36, (51, 0), 134);
            Table.States (73).Kernel := To_Vector ((0 => ((51, 0),  24,  4, (2147483647, 0),  0)));
            Table.States (73).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 134)));
            Table.States (74).Action_List.Set_Capacity (12);
            Add_Action (Table.States (74), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 19, (52, 1), 135);
            Add_Action (Table.States (74), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 27, (57, 4), 136);
            Add_Action (Table.States (74), 28, (56, 2), 137);
            Add_Action (Table.States (74), 29, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 34, (57, 5), 138);
            Add_Action (Table.States (74), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (74), 38, Reduce, (54, 0),  1, null, null);
            Table.States (74).Kernel := To_Vector ((((52, 1),  36,  2, (2147483647, 0),  0), ((54, 0),  36,  0, (54,
            0),  1), ((56, 2),  36,  1, (2147483647, 0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,
            1, (2147483647, 0),  0)));
            Table.States (74).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            137), (Shift, (57, 4),  27, 136), (Shift, (57, 5),  34, 138)));
            Table.States (75).Action_List.Set_Capacity (9);
            Add_Action (Table.States (75), 15, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 21, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 22, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 23, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 24, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 28, (56, 3), 139);
            Add_Action (Table.States (75), 29, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 36, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (75), 38, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Table.States (75).Kernel := To_Vector ((((54, 1),  38,  0, (54, 1),  1), ((56, 3),  38,  1, (2147483647,
            0),  0)));
            Table.States (75).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 1),  1), (Shift, (56, 3),  28,
            139)));
            Table.States (76).Action_List.Set_Capacity (8);
            Add_Action (Table.States (76), (15, 21, 22, 23, 24, 29, 36, 38), (54, 2),  1, null, null);
            Table.States (76).Kernel := To_Vector ((0 => ((54, 2),  51,  0, (54, 2),  1)));
            Table.States (76).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 2),  1)));
            Table.States (77).Action_List.Set_Capacity (8);
            Add_Action (Table.States (77), (15, 21, 22, 23, 24, 29, 36, 38), (53, 0),  1, null, null);
            Table.States (77).Kernel := To_Vector ((0 => ((53, 0),  52,  0, (53, 0),  1)));
            Table.States (77).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 0),  1)));
            Table.States (78).Action_List.Set_Capacity (8);
            Add_Action (Table.States (78), 15, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (78), 21, (57, 0), 70);
            Add_Action (Table.States (78), 22, (56, 0), 71);
            Add_Action (Table.States (78), 23, (55, 0), 72);
            Add_Action (Table.States (78), 24, (51, 0), 73);
            Add_Action (Table.States (78), 29, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (78), 36, (52, 1), 74);
            Add_Action (Table.States (78), 38, (54, 1), 75);
            Table.States (78).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (78), 51, 76);
            Add_Goto (Table.States (78), 52, 140);
            Add_Goto (Table.States (78), 54, 79);
            Add_Goto (Table.States (78), 55, 80);
            Add_Goto (Table.States (78), 56, 81);
            Add_Goto (Table.States (78), 57, 82);
            Table.States (78).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 0),  53,  0, (58,
            0),  1)));
            Table.States (78).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (79).Action_List.Set_Capacity (8);
            Add_Action (Table.States (79), (15, 21, 22, 23, 24, 29, 36, 38), (52, 0),  1, null, null);
            Table.States (79).Kernel := To_Vector ((0 => ((52, 0),  54,  0, (52, 0),  1)));
            Table.States (79).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (80).Action_List.Set_Capacity (8);
            Add_Action (Table.States (80), (15, 21, 22, 23, 24, 29, 36, 38), (54, 5),  1, null, null);
            Table.States (80).Kernel := To_Vector ((0 => ((54, 5),  55,  0, (54, 5),  1)));
            Table.States (80).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 5),  1)));
            Table.States (81).Action_List.Set_Capacity (8);
            Add_Action (Table.States (81), (15, 21, 22, 23, 24, 29, 36, 38), (54, 3),  1, null, null);
            Table.States (81).Kernel := To_Vector ((0 => ((54, 3),  56,  0, (54, 3),  1)));
            Table.States (81).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 3),  1)));
            Table.States (82).Action_List.Set_Capacity (8);
            Add_Action (Table.States (82), (15, 21, 22, 23, 24, 29, 36, 38), (54, 4),  1, null, null);
            Table.States (82).Kernel := To_Vector ((0 => ((54, 4),  57,  0, (54, 4),  1)));
            Table.States (82).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 4),  1)));
            Table.States (83).Action_List.Set_Capacity (2);
            Add_Action (Table.States (83), 15, (58, 1), 141);
            Add_Action (Table.States (83), 29, (57, 0), 142);
            Table.States (83).Kernel := To_Vector ((((57, 0),  58,  1, (2147483647, 0),  0), ((57, 1),  58,  2,
            (2147483647, 0),  0), ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (83).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  29, 142)));
            Table.States (84).Action_List.Set_Capacity (6);
            Add_Action (Table.States (84), 21, (57, 0), 70);
            Add_Action (Table.States (84), 22, (56, 0), 71);
            Add_Action (Table.States (84), 23, (55, 0), 72);
            Add_Action (Table.States (84), 24, (51, 0), 73);
            Add_Action (Table.States (84), 36, (52, 1), 74);
            Add_Action (Table.States (84), 38, (54, 1), 75);
            Table.States (84).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (84), 51, 76);
            Add_Goto (Table.States (84), 52, 77);
            Add_Goto (Table.States (84), 53, 78);
            Add_Goto (Table.States (84), 54, 79);
            Add_Goto (Table.States (84), 55, 80);
            Add_Goto (Table.States (84), 56, 81);
            Add_Goto (Table.States (84), 57, 82);
            Add_Goto (Table.States (84), 58, 143);
            Table.States (84).Kernel := To_Vector ((((57, 0),  21,  2, (2147483647, 0),  0), ((57, 1),  21,  3,
            (2147483647, 0),  0)));
            Table.States (84).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 74)));
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (85).Action_List.Set_Capacity (6);
            Add_Action (Table.States (85), 21, (57, 0), 84);
            Add_Action (Table.States (85), 22, (56, 0), 85);
            Add_Action (Table.States (85), 23, (55, 0), 86);
            Add_Action (Table.States (85), 24, (51, 0), 87);
            Add_Action (Table.States (85), 36, (52, 1), 88);
            Add_Action (Table.States (85), 38, (54, 1), 89);
            Table.States (85).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (85), 51, 90);
            Add_Goto (Table.States (85), 52, 91);
            Add_Goto (Table.States (85), 53, 92);
            Add_Goto (Table.States (85), 54, 93);
            Add_Goto (Table.States (85), 55, 94);
            Add_Goto (Table.States (85), 56, 95);
            Add_Goto (Table.States (85), 57, 96);
            Add_Goto (Table.States (85), 58, 144);
            Table.States (85).Kernel := To_Vector ((0 => ((56, 0),  22,  2, (2147483647, 0),  0)));
            Table.States (85).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 88)));
            Table.States (86).Action_List.Set_Capacity (6);
            Add_Action (Table.States (86), 21, (57, 0), 98);
            Add_Action (Table.States (86), 22, (56, 0), 99);
            Add_Action (Table.States (86), 23, (55, 0), 100);
            Add_Action (Table.States (86), 24, (51, 0), 101);
            Add_Action (Table.States (86), 36, (52, 1), 102);
            Add_Action (Table.States (86), 38, (54, 1), 103);
            Table.States (86).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (86), 51, 104);
            Add_Goto (Table.States (86), 52, 105);
            Add_Goto (Table.States (86), 53, 106);
            Add_Goto (Table.States (86), 54, 107);
            Add_Goto (Table.States (86), 55, 108);
            Add_Goto (Table.States (86), 56, 109);
            Add_Goto (Table.States (86), 57, 110);
            Add_Goto (Table.States (86), 58, 145);
            Table.States (86).Kernel := To_Vector ((((55, 0),  23,  2, (2147483647, 0),  0), ((56, 1),  23,  3,
            (2147483647, 0),  0), ((57, 2),  23,  3, (2147483647, 0),  0), ((57, 3),  23,  3, (2147483647, 0),  0)));
            Table.States (86).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 102)));
            Table.States (87).Action_List.Set_Capacity (1);
            Add_Action (Table.States (87), 36, (51, 0), 146);
            Table.States (87).Kernel := To_Vector ((0 => ((51, 0),  24,  4, (2147483647, 0),  0)));
            Table.States (87).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 146)));
            Table.States (88).Action_List.Set_Capacity (12);
            Add_Action (Table.States (88), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 19, (52, 1), 147);
            Add_Action (Table.States (88), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 27, (57, 4), 148);
            Add_Action (Table.States (88), 28, (56, 2), 149);
            Add_Action (Table.States (88), 30, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 34, (57, 5), 150);
            Add_Action (Table.States (88), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (88), 38, Reduce, (54, 0),  1, null, null);
            Table.States (88).Kernel := To_Vector ((((52, 1),  36,  2, (2147483647, 0),  0), ((54, 0),  36,  0, (54,
            0),  1), ((56, 2),  36,  1, (2147483647, 0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,
            1, (2147483647, 0),  0)));
            Table.States (88).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            149), (Shift, (57, 4),  27, 148), (Shift, (57, 5),  34, 150)));
            Table.States (89).Action_List.Set_Capacity (9);
            Add_Action (Table.States (89), 15, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 21, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 22, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 23, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 24, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 28, (56, 3), 151);
            Add_Action (Table.States (89), 30, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 36, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (89), 38, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Table.States (89).Kernel := To_Vector ((((54, 1),  38,  0, (54, 1),  1), ((56, 3),  38,  1, (2147483647,
            0),  0)));
            Table.States (89).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 1),  1), (Shift, (56, 3),  28,
            151)));
            Table.States (90).Action_List.Set_Capacity (8);
            Add_Action (Table.States (90), (15, 21, 22, 23, 24, 30, 36, 38), (54, 2),  1, null, null);
            Table.States (90).Kernel := To_Vector ((0 => ((54, 2),  51,  0, (54, 2),  1)));
            Table.States (90).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 2),  1)));
            Table.States (91).Action_List.Set_Capacity (8);
            Add_Action (Table.States (91), (15, 21, 22, 23, 24, 30, 36, 38), (53, 0),  1, null, null);
            Table.States (91).Kernel := To_Vector ((0 => ((53, 0),  52,  0, (53, 0),  1)));
            Table.States (91).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 0),  1)));
            Table.States (92).Action_List.Set_Capacity (8);
            Add_Action (Table.States (92), 15, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (92), 21, (57, 0), 84);
            Add_Action (Table.States (92), 22, (56, 0), 85);
            Add_Action (Table.States (92), 23, (55, 0), 86);
            Add_Action (Table.States (92), 24, (51, 0), 87);
            Add_Action (Table.States (92), 30, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (92), 36, (52, 1), 88);
            Add_Action (Table.States (92), 38, (54, 1), 89);
            Table.States (92).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (92), 51, 90);
            Add_Goto (Table.States (92), 52, 152);
            Add_Goto (Table.States (92), 54, 93);
            Add_Goto (Table.States (92), 55, 94);
            Add_Goto (Table.States (92), 56, 95);
            Add_Goto (Table.States (92), 57, 96);
            Table.States (92).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 0),  53,  0, (58,
            0),  1)));
            Table.States (92).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (93).Action_List.Set_Capacity (8);
            Add_Action (Table.States (93), (15, 21, 22, 23, 24, 30, 36, 38), (52, 0),  1, null, null);
            Table.States (93).Kernel := To_Vector ((0 => ((52, 0),  54,  0, (52, 0),  1)));
            Table.States (93).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (94).Action_List.Set_Capacity (8);
            Add_Action (Table.States (94), (15, 21, 22, 23, 24, 30, 36, 38), (54, 5),  1, null, null);
            Table.States (94).Kernel := To_Vector ((0 => ((54, 5),  55,  0, (54, 5),  1)));
            Table.States (94).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 5),  1)));
            Table.States (95).Action_List.Set_Capacity (8);
            Add_Action (Table.States (95), (15, 21, 22, 23, 24, 30, 36, 38), (54, 3),  1, null, null);
            Table.States (95).Kernel := To_Vector ((0 => ((54, 3),  56,  0, (54, 3),  1)));
            Table.States (95).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 3),  1)));
            Table.States (96).Action_List.Set_Capacity (8);
            Add_Action (Table.States (96), (15, 21, 22, 23, 24, 30, 36, 38), (54, 4),  1, null, null);
            Table.States (96).Kernel := To_Vector ((0 => ((54, 4),  57,  0, (54, 4),  1)));
            Table.States (96).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 4),  1)));
            Table.States (97).Action_List.Set_Capacity (2);
            Add_Action (Table.States (97), 15, (58, 1), 153);
            Add_Action (Table.States (97), 30, (56, 0), 154);
            Table.States (97).Kernel := To_Vector ((((56, 0),  58,  1, (2147483647, 0),  0), ((58, 1),  58,  2,
            (2147483647, 0),  0)));
            Table.States (97).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  30, 154)));
            Table.States (98).Action_List.Set_Capacity (6);
            Add_Action (Table.States (98), 21, (57, 0), 70);
            Add_Action (Table.States (98), 22, (56, 0), 71);
            Add_Action (Table.States (98), 23, (55, 0), 72);
            Add_Action (Table.States (98), 24, (51, 0), 73);
            Add_Action (Table.States (98), 36, (52, 1), 74);
            Add_Action (Table.States (98), 38, (54, 1), 75);
            Table.States (98).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (98), 51, 76);
            Add_Goto (Table.States (98), 52, 77);
            Add_Goto (Table.States (98), 53, 78);
            Add_Goto (Table.States (98), 54, 79);
            Add_Goto (Table.States (98), 55, 80);
            Add_Goto (Table.States (98), 56, 81);
            Add_Goto (Table.States (98), 57, 82);
            Add_Goto (Table.States (98), 58, 155);
            Table.States (98).Kernel := To_Vector ((((57, 0),  21,  2, (2147483647, 0),  0), ((57, 1),  21,  3,
            (2147483647, 0),  0)));
            Table.States (98).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 74)));
            Table.States (99).Action_List.Set_Capacity (6);
            Add_Action (Table.States (99), 21, (57, 0), 84);
            Add_Action (Table.States (99), 22, (56, 0), 85);
            Add_Action (Table.States (99), 23, (55, 0), 86);
            Add_Action (Table.States (99), 24, (51, 0), 87);
            Add_Action (Table.States (99), 36, (52, 1), 88);
            Add_Action (Table.States (99), 38, (54, 1), 89);
            Table.States (99).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (99), 51, 90);
            Add_Goto (Table.States (99), 52, 91);
            Add_Goto (Table.States (99), 53, 92);
            Add_Goto (Table.States (99), 54, 93);
            Add_Goto (Table.States (99), 55, 94);
            Add_Goto (Table.States (99), 56, 95);
            Add_Goto (Table.States (99), 57, 96);
            Add_Goto (Table.States (99), 58, 156);
            Table.States (99).Kernel := To_Vector ((0 => ((56, 0),  22,  2, (2147483647, 0),  0)));
            Table.States (99).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 88)));
            Table.States (100).Action_List.Set_Capacity (6);
            Add_Action (Table.States (100), 21, (57, 0), 98);
            Add_Action (Table.States (100), 22, (56, 0), 99);
            Add_Action (Table.States (100), 23, (55, 0), 100);
            Add_Action (Table.States (100), 24, (51, 0), 101);
            Add_Action (Table.States (100), 36, (52, 1), 102);
            Add_Action (Table.States (100), 38, (54, 1), 103);
            Table.States (100).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (100), 51, 104);
            Add_Goto (Table.States (100), 52, 105);
            Add_Goto (Table.States (100), 53, 106);
            Add_Goto (Table.States (100), 54, 107);
            Add_Goto (Table.States (100), 55, 108);
            Add_Goto (Table.States (100), 56, 109);
            Add_Goto (Table.States (100), 57, 110);
            Add_Goto (Table.States (100), 58, 157);
            Table.States (100).Kernel := To_Vector ((((55, 0),  23,  2, (2147483647, 0),  0), ((56, 1),  23,  3,
            (2147483647, 0),  0), ((57, 2),  23,  3, (2147483647, 0),  0), ((57, 3),  23,  3, (2147483647, 0),  0)));
            Table.States (100).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 102)));
            Table.States (101).Action_List.Set_Capacity (1);
            Add_Action (Table.States (101), 36, (51, 0), 158);
            Table.States (101).Kernel := To_Vector ((0 => ((51, 0),  24,  4, (2147483647, 0),  0)));
            Table.States (101).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 158)));
            Table.States (102).Action_List.Set_Capacity (12);
            Add_Action (Table.States (102), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 19, (52, 1), 159);
            Add_Action (Table.States (102), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 27, (57, 4), 160);
            Add_Action (Table.States (102), 28, (56, 2), 161);
            Add_Action (Table.States (102), 31, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 34, (57, 5), 162);
            Add_Action (Table.States (102), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (102), 38, Reduce, (54, 0),  1, null, null);
            Table.States (102).Kernel := To_Vector ((((52, 1),  36,  2, (2147483647, 0),  0), ((54, 0),  36,  0, (54,
            0),  1), ((56, 2),  36,  1, (2147483647, 0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,
            1, (2147483647, 0),  0)));
            Table.States (102).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            161), (Shift, (57, 4),  27, 160), (Shift, (57, 5),  34, 162)));
            Table.States (103).Action_List.Set_Capacity (9);
            Add_Action (Table.States (103), 15, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 21, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 22, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 23, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 24, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 28, (56, 3), 163);
            Add_Action (Table.States (103), 31, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 36, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (103), 38, Reduce, (54, 1),  1, rhs_item_1'Access, null);
            Table.States (103).Kernel := To_Vector ((((54, 1),  38,  0, (54, 1),  1), ((56, 3),  38,  1, (2147483647,
            0),  0)));
            Table.States (103).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 1),  1), (Shift, (56, 3),  28,
            163)));
            Table.States (104).Action_List.Set_Capacity (8);
            Add_Action (Table.States (104), (15, 21, 22, 23, 24, 31, 36, 38), (54, 2),  1, null, null);
            Table.States (104).Kernel := To_Vector ((0 => ((54, 2),  51,  0, (54, 2),  1)));
            Table.States (104).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 2),  1)));
            Table.States (105).Action_List.Set_Capacity (8);
            Add_Action (Table.States (105), (15, 21, 22, 23, 24, 31, 36, 38), (53, 0),  1, null, null);
            Table.States (105).Kernel := To_Vector ((0 => ((53, 0),  52,  0, (53, 0),  1)));
            Table.States (105).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 0),  1)));
            Table.States (106).Action_List.Set_Capacity (8);
            Add_Action (Table.States (106), 15, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (106), 21, (57, 0), 98);
            Add_Action (Table.States (106), 22, (56, 0), 99);
            Add_Action (Table.States (106), 23, (55, 0), 100);
            Add_Action (Table.States (106), 24, (51, 0), 101);
            Add_Action (Table.States (106), 31, Reduce, (58, 0),  1, null, null);
            Add_Action (Table.States (106), 36, (52, 1), 102);
            Add_Action (Table.States (106), 38, (54, 1), 103);
            Table.States (106).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (106), 51, 104);
            Add_Goto (Table.States (106), 52, 164);
            Add_Goto (Table.States (106), 54, 107);
            Add_Goto (Table.States (106), 55, 108);
            Add_Goto (Table.States (106), 56, 109);
            Add_Goto (Table.States (106), 57, 110);
            Table.States (106).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 0),  53,  0, (58,
            0),  1)));
            Table.States (106).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (107).Action_List.Set_Capacity (8);
            Add_Action (Table.States (107), (15, 21, 22, 23, 24, 31, 36, 38), (52, 0),  1, null, null);
            Table.States (107).Kernel := To_Vector ((0 => ((52, 0),  54,  0, (52, 0),  1)));
            Table.States (107).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (108).Action_List.Set_Capacity (8);
            Add_Action (Table.States (108), (15, 21, 22, 23, 24, 31, 36, 38), (54, 5),  1, null, null);
            Table.States (108).Kernel := To_Vector ((0 => ((54, 5),  55,  0, (54, 5),  1)));
            Table.States (108).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 5),  1)));
            Table.States (109).Action_List.Set_Capacity (8);
            Add_Action (Table.States (109), (15, 21, 22, 23, 24, 31, 36, 38), (54, 3),  1, null, null);
            Table.States (109).Kernel := To_Vector ((0 => ((54, 3),  56,  0, (54, 3),  1)));
            Table.States (109).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 3),  1)));
            Table.States (110).Action_List.Set_Capacity (8);
            Add_Action (Table.States (110), (15, 21, 22, 23, 24, 31, 36, 38), (54, 4),  1, null, null);
            Table.States (110).Kernel := To_Vector ((0 => ((54, 4),  57,  0, (54, 4),  1)));
            Table.States (110).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 4),  1)));
            Table.States (111).Action_List.Set_Capacity (2);
            Add_Action (Table.States (111), 15, (58, 1), 165);
            Add_Action (Table.States (111), 31, (55, 0), 166);
            Table.States (111).Kernel := To_Vector ((((55, 0),  58,  1, (2147483647, 0),  0), ((56, 1),  58,  2,
            (2147483647, 0),  0), ((57, 2),  58,  2, (2147483647, 0),  0), ((57, 3),  58,  2, (2147483647, 0),  0),
            ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (111).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  31, 166)));
            Table.States (112).Action_List.Set_Capacity (1);
            Add_Action (Table.States (112), 19, (51, 0), 167);
            Table.States (112).Kernel := To_Vector ((0 => ((51, 0),  36,  3, (2147483647, 0),  0)));
            Table.States (112).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  19, 167)));
            Table.States (113).Action_List.Set_Capacity (6);
            Add_Action (Table.States (113), 21, (57, 0), 45);
            Add_Action (Table.States (113), 22, (56, 0), 46);
            Add_Action (Table.States (113), 23, (55, 0), 47);
            Add_Action (Table.States (113), 24, (51, 0), 48);
            Add_Action (Table.States (113), 36, (54, 0), 168);
            Add_Action (Table.States (113), 38, (54, 1), 50);
            Table.States (113).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (113), 51, 53);
            Add_Goto (Table.States (113), 54, 169);
            Add_Goto (Table.States (113), 55, 57);
            Add_Goto (Table.States (113), 56, 58);
            Add_Goto (Table.States (113), 57, 59);
            Table.States (113).Kernel := To_Vector ((0 => ((52, 1),  19,  1, (2147483647, 0),  0)));
            Table.States (113).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  36, 168)));
            Table.States (114).Action_List.Set_Capacity (11);
            Add_Action (Table.States (114), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (57, 4),  2, null, null);
            Table.States (114).Kernel := To_Vector ((0 => ((57, 4),  27,  0, (57, 4),  2)));
            Table.States (114).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 4),  2)));
            Table.States (115).Action_List.Set_Capacity (11);
            Add_Action (Table.States (115), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (56, 2),  2, null, null);
            Table.States (115).Kernel := To_Vector ((0 => ((56, 2),  28,  0, (56, 2),  2)));
            Table.States (115).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  2)));
            Table.States (116).Action_List.Set_Capacity (11);
            Add_Action (Table.States (116), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (57, 5),  2, null, null);
            Table.States (116).Kernel := To_Vector ((0 => ((57, 5),  34,  0, (57, 5),  2)));
            Table.States (116).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 5),  2)));
            Table.States (117).Action_List.Set_Capacity (11);
            Add_Action (Table.States (117), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (56, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (117).Kernel := To_Vector ((0 => ((56, 3),  28,  0, (56, 3),  2)));
            Table.States (117).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  2)));
            Table.States (118).Action_List.Set_Capacity (10);
            Add_Action (Table.States (118), 15, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (118), 21, (57, 0), 45);
            Add_Action (Table.States (118), 22, (56, 0), 46);
            Add_Action (Table.States (118), 23, (55, 0), 47);
            Add_Action (Table.States (118), 24, (51, 0), 48);
            Add_Action (Table.States (118), 26, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (118), 32, Reduce, (50, 0),  0, null, null);
            Add_Action (Table.States (118), 36, (52, 1), 49);
            Add_Conflict (Table.States (118), 36, (50, 0),  0, null, null);
            Add_Action (Table.States (118), 38, (54, 1), 50);
            Add_Action (Table.States (118), 39, Reduce, (50, 0),  0, null, null);
            Table.States (118).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (118), 50, 170);
            Add_Goto (Table.States (118), 51, 53);
            Add_Goto (Table.States (118), 52, 54);
            Add_Goto (Table.States (118), 53, 55);
            Add_Goto (Table.States (118), 54, 56);
            Add_Goto (Table.States (118), 55, 57);
            Add_Goto (Table.States (118), 56, 58);
            Add_Goto (Table.States (118), 57, 59);
            Table.States (118).Kernel := To_Vector ((0 => ((49, 1),  15,  0, (50, 0),  0)));
            Table.States (118).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  0)));
            Table.States (119).Action_List.Set_Capacity (3);
            Add_Action (Table.States (119), 5, (49, 6), 171);
            Add_Action (Table.States (119), 6, (49, 4), 172);
            Add_Action (Table.States (119), 7, (49, 2), 173);
            Table.States (119).Kernel := To_Vector ((((49, 2),  26,  4, (2147483647, 0),  0), ((49, 3),  26,  4,
            (2147483647, 0),  0), ((49, 4),  26,  4, (2147483647, 0),  0), ((49, 5),  26,  4, (2147483647, 0),  0),
            ((49, 6),  26,  2, (2147483647, 0),  0)));
            Table.States (120).Action_List.Set_Capacity (3);
            Add_Action (Table.States (120), (26, 36, 39), (48, 0),  1, null, null);
            Table.States (120).Kernel := To_Vector ((0 => ((48, 0),  32,  0, (48, 0),  1)));
            Table.States (120).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (121).Action_List.Set_Capacity (3);
            Add_Action (Table.States (121), (26, 36, 39), (47, 0),  4, nonterminal_0'Access, null);
            Table.States (121).Kernel := To_Vector ((0 => ((47, 0),  48,  0, (47, 0),  4)));
            Table.States (121).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  4)));
            Table.States (122).Action_List.Set_Capacity (6);
            Add_Action (Table.States (122), 14, (50, 3), 174);
            Add_Action (Table.States (122), 15, Reduce, (50, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (122), 26, Reduce, (50, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (122), 32, Reduce, (50, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (122), 36, Reduce, (50, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (122), 39, Reduce, (50, 2),  2, rhs_2'Access, null);
            Table.States (122).Kernel := To_Vector ((((50, 2),  14,  0, (50, 2),  2), ((50, 3),  14,  1, (2147483647,
            0),  0)));
            Table.States (122).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 2),  2), (Shift, (50, 3),  14,
            174)));
            Table.States (123).Action_List.Set_Capacity (11);
            Add_Action (Table.States (123), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (53, 1),  2, null, null);
            Table.States (123).Kernel := To_Vector ((0 => ((53, 1),  52,  0, (53, 1),  2)));
            Table.States (123).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  2)));
            Table.States (124).Action_List.Set_Capacity (4);
            Add_Action (Table.States (124), (15, 26, 36, 39), (44, 0),  1, null, null);
            Table.States (124).Kernel := To_Vector ((0 => ((44, 0),  36,  0, (44, 0),  1)));
            Table.States (124).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
         end Subr_3;
         procedure Subr_4
         is begin
            Table.States (125).Action_List.Set_Capacity (4);
            Add_Action (Table.States (125), 15, (44, 1), 175);
            Add_Action (Table.States (125), 26, Reduce, (41, 8),  5, declaration_8'Access, null);
            Add_Action (Table.States (125), 36, Reduce, (41, 8),  5, declaration_8'Access, null);
            Add_Action (Table.States (125), 39, Reduce, (41, 8),  5, declaration_8'Access, null);
            Table.States (125).Kernel := To_Vector ((((41, 8),  44,  0, (41, 8),  5), ((44, 1),  44,  2, (2147483647,
            0),  0)));
            Table.States (125).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 8),  5)));
            Table.States (126).Action_List.Set_Capacity (3);
            Add_Action (Table.States (126), (26, 36, 39), (41, 7),  5, declaration_7'Access, null);
            Table.States (126).Kernel := To_Vector ((0 => ((41, 7),  36,  0, (41, 7),  5)));
            Table.States (126).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 7),  5)));
            Table.States (127).Action_List.Set_Capacity (4);
            Add_Action (Table.States (127), 15, (44, 1), 175);
            Add_Action (Table.States (127), 26, Reduce, (41, 6),  5, declaration_6'Access, null);
            Add_Action (Table.States (127), 36, Reduce, (41, 6),  5, declaration_6'Access, null);
            Add_Action (Table.States (127), 39, Reduce, (41, 6),  5, declaration_6'Access, null);
            Table.States (127).Kernel := To_Vector ((((41, 6),  44,  0, (41, 6),  5), ((44, 1),  44,  2, (2147483647,
            0),  0)));
            Table.States (127).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 6),  5)));
            Table.States (128).Action_List.Set_Capacity (3);
            Add_Action (Table.States (128), (26, 36, 39), (41, 5),  5, declaration_5'Access, null);
            Table.States (128).Kernel := To_Vector ((0 => ((41, 5),  36,  0, (41, 5),  5)));
            Table.States (128).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 5),  5)));
            Table.States (129).Action_List.Set_Capacity (1);
            Add_Action (Table.States (129), (1 =>  36), (42, 1),  4, token_keyword_non_grammar_1'Access, null);
            Table.States (129).Kernel := To_Vector ((0 => ((42, 1),  20,  0, (42, 1),  4)));
            Table.States (129).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 1),  4)));
            Table.States (130).Action_List.Set_Capacity (1);
            Add_Action (Table.States (130), (1 =>  36), (42, 2),  4, token_keyword_non_grammar_2'Access, null);
            Table.States (130).Kernel := To_Vector ((0 => ((42, 2),  20,  0, (42, 2),  4)));
            Table.States (130).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 2),  4)));
            Table.States (131).Action_List.Set_Capacity (2);
            Add_Action (Table.States (131), 15, (58, 1), 141);
            Add_Action (Table.States (131), 29, (57, 0), 176);
            Table.States (131).Kernel := To_Vector ((((57, 0),  58,  1, (2147483647, 0),  0), ((57, 1),  58,  2,
            (2147483647, 0),  0), ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (131).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  29, 176)));
            Table.States (132).Action_List.Set_Capacity (2);
            Add_Action (Table.States (132), 15, (58, 1), 153);
            Add_Action (Table.States (132), 30, (56, 0), 177);
            Table.States (132).Kernel := To_Vector ((((56, 0),  58,  1, (2147483647, 0),  0), ((58, 1),  58,  2,
            (2147483647, 0),  0)));
            Table.States (132).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  30, 177)));
            Table.States (133).Action_List.Set_Capacity (2);
            Add_Action (Table.States (133), 15, (58, 1), 165);
            Add_Action (Table.States (133), 31, (55, 0), 178);
            Table.States (133).Kernel := To_Vector ((((55, 0),  58,  1, (2147483647, 0),  0), ((56, 1),  58,  2,
            (2147483647, 0),  0), ((57, 2),  58,  2, (2147483647, 0),  0), ((57, 3),  58,  2, (2147483647, 0),  0),
            ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (133).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  31, 178)));
            Table.States (134).Action_List.Set_Capacity (1);
            Add_Action (Table.States (134), 19, (51, 0), 179);
            Table.States (134).Kernel := To_Vector ((0 => ((51, 0),  36,  3, (2147483647, 0),  0)));
            Table.States (134).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  19, 179)));
            Table.States (135).Action_List.Set_Capacity (6);
            Add_Action (Table.States (135), 21, (57, 0), 70);
            Add_Action (Table.States (135), 22, (56, 0), 71);
            Add_Action (Table.States (135), 23, (55, 0), 72);
            Add_Action (Table.States (135), 24, (51, 0), 73);
            Add_Action (Table.States (135), 36, (54, 0), 180);
            Add_Action (Table.States (135), 38, (54, 1), 75);
            Table.States (135).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (135), 51, 76);
            Add_Goto (Table.States (135), 54, 181);
            Add_Goto (Table.States (135), 55, 80);
            Add_Goto (Table.States (135), 56, 81);
            Add_Goto (Table.States (135), 57, 82);
            Table.States (135).Kernel := To_Vector ((0 => ((52, 1),  19,  1, (2147483647, 0),  0)));
            Table.States (135).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  36, 180)));
            Table.States (136).Action_List.Set_Capacity (8);
            Add_Action (Table.States (136), (15, 21, 22, 23, 24, 29, 36, 38), (57, 4),  2, null, null);
            Table.States (136).Kernel := To_Vector ((0 => ((57, 4),  27,  0, (57, 4),  2)));
            Table.States (136).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 4),  2)));
            Table.States (137).Action_List.Set_Capacity (8);
            Add_Action (Table.States (137), (15, 21, 22, 23, 24, 29, 36, 38), (56, 2),  2, null, null);
            Table.States (137).Kernel := To_Vector ((0 => ((56, 2),  28,  0, (56, 2),  2)));
            Table.States (137).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  2)));
            Table.States (138).Action_List.Set_Capacity (8);
            Add_Action (Table.States (138), (15, 21, 22, 23, 24, 29, 36, 38), (57, 5),  2, null, null);
            Table.States (138).Kernel := To_Vector ((0 => ((57, 5),  34,  0, (57, 5),  2)));
            Table.States (138).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 5),  2)));
            Table.States (139).Action_List.Set_Capacity (8);
            Add_Action (Table.States (139), (15, 21, 22, 23, 24, 29, 36, 38), (56, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (139).Kernel := To_Vector ((0 => ((56, 3),  28,  0, (56, 3),  2)));
            Table.States (139).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  2)));
            Table.States (140).Action_List.Set_Capacity (8);
            Add_Action (Table.States (140), (15, 21, 22, 23, 24, 29, 36, 38), (53, 1),  2, null, null);
            Table.States (140).Kernel := To_Vector ((0 => ((53, 1),  52,  0, (53, 1),  2)));
            Table.States (140).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  2)));
            Table.States (141).Action_List.Set_Capacity (6);
            Add_Action (Table.States (141), 21, (57, 0), 70);
            Add_Action (Table.States (141), 22, (56, 0), 71);
            Add_Action (Table.States (141), 23, (55, 0), 72);
            Add_Action (Table.States (141), 24, (51, 0), 73);
            Add_Action (Table.States (141), 36, (52, 1), 74);
            Add_Action (Table.States (141), 38, (54, 1), 75);
            Table.States (141).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (141), 51, 76);
            Add_Goto (Table.States (141), 52, 77);
            Add_Goto (Table.States (141), 53, 182);
            Add_Goto (Table.States (141), 54, 79);
            Add_Goto (Table.States (141), 55, 80);
            Add_Goto (Table.States (141), 56, 81);
            Add_Goto (Table.States (141), 57, 82);
            Table.States (141).Kernel := To_Vector ((0 => ((58, 1),  15,  1, (2147483647, 0),  0)));
            Table.States (141).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 74)));
            Table.States (142).Action_List.Set_Capacity (12);
            Add_Action (Table.States (142), 14, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 15, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 21, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 22, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 23, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 24, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 25, (57, 1), 183);
            Add_Action (Table.States (142), 26, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 32, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 36, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 38, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (142), 39, Reduce, (57, 0),  3, null, null);
            Table.States (142).Kernel := To_Vector ((((57, 0),  29,  0, (57, 0),  3), ((57, 1),  29,  1, (2147483647,
            0),  0)));
            Table.States (142).Minimal_Complete_Actions := To_Vector (((Reduce, (57, 0),  3), (Shift, (57, 1),  25,
            183)));
            Table.States (143).Action_List.Set_Capacity (2);
            Add_Action (Table.States (143), 15, (58, 1), 141);
            Add_Action (Table.States (143), 29, (57, 0), 184);
            Table.States (143).Kernel := To_Vector ((((57, 0),  58,  1, (2147483647, 0),  0), ((57, 1),  58,  2,
            (2147483647, 0),  0), ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (143).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  29, 184)));
            Table.States (144).Action_List.Set_Capacity (2);
            Add_Action (Table.States (144), 15, (58, 1), 153);
            Add_Action (Table.States (144), 30, (56, 0), 185);
            Table.States (144).Kernel := To_Vector ((((56, 0),  58,  1, (2147483647, 0),  0), ((58, 1),  58,  2,
            (2147483647, 0),  0)));
            Table.States (144).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  30, 185)));
            Table.States (145).Action_List.Set_Capacity (2);
            Add_Action (Table.States (145), 15, (58, 1), 165);
            Add_Action (Table.States (145), 31, (55, 0), 186);
            Table.States (145).Kernel := To_Vector ((((55, 0),  58,  1, (2147483647, 0),  0), ((56, 1),  58,  2,
            (2147483647, 0),  0), ((57, 2),  58,  2, (2147483647, 0),  0), ((57, 3),  58,  2, (2147483647, 0),  0),
            ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (145).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  31, 186)));
            Table.States (146).Action_List.Set_Capacity (1);
            Add_Action (Table.States (146), 19, (51, 0), 187);
            Table.States (146).Kernel := To_Vector ((0 => ((51, 0),  36,  3, (2147483647, 0),  0)));
            Table.States (146).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  19, 187)));
            Table.States (147).Action_List.Set_Capacity (6);
            Add_Action (Table.States (147), 21, (57, 0), 84);
            Add_Action (Table.States (147), 22, (56, 0), 85);
            Add_Action (Table.States (147), 23, (55, 0), 86);
            Add_Action (Table.States (147), 24, (51, 0), 87);
            Add_Action (Table.States (147), 36, (54, 0), 188);
            Add_Action (Table.States (147), 38, (54, 1), 89);
            Table.States (147).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (147), 51, 90);
            Add_Goto (Table.States (147), 54, 189);
            Add_Goto (Table.States (147), 55, 94);
            Add_Goto (Table.States (147), 56, 95);
            Add_Goto (Table.States (147), 57, 96);
            Table.States (147).Kernel := To_Vector ((0 => ((52, 1),  19,  1, (2147483647, 0),  0)));
            Table.States (147).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  36, 188)));
            Table.States (148).Action_List.Set_Capacity (8);
            Add_Action (Table.States (148), (15, 21, 22, 23, 24, 30, 36, 38), (57, 4),  2, null, null);
            Table.States (148).Kernel := To_Vector ((0 => ((57, 4),  27,  0, (57, 4),  2)));
            Table.States (148).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 4),  2)));
            Table.States (149).Action_List.Set_Capacity (8);
            Add_Action (Table.States (149), (15, 21, 22, 23, 24, 30, 36, 38), (56, 2),  2, null, null);
            Table.States (149).Kernel := To_Vector ((0 => ((56, 2),  28,  0, (56, 2),  2)));
            Table.States (149).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  2)));
            Table.States (150).Action_List.Set_Capacity (8);
            Add_Action (Table.States (150), (15, 21, 22, 23, 24, 30, 36, 38), (57, 5),  2, null, null);
            Table.States (150).Kernel := To_Vector ((0 => ((57, 5),  34,  0, (57, 5),  2)));
            Table.States (150).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 5),  2)));
            Table.States (151).Action_List.Set_Capacity (8);
            Add_Action (Table.States (151), (15, 21, 22, 23, 24, 30, 36, 38), (56, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (151).Kernel := To_Vector ((0 => ((56, 3),  28,  0, (56, 3),  2)));
            Table.States (151).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  2)));
            Table.States (152).Action_List.Set_Capacity (8);
            Add_Action (Table.States (152), (15, 21, 22, 23, 24, 30, 36, 38), (53, 1),  2, null, null);
            Table.States (152).Kernel := To_Vector ((0 => ((53, 1),  52,  0, (53, 1),  2)));
            Table.States (152).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  2)));
            Table.States (153).Action_List.Set_Capacity (6);
            Add_Action (Table.States (153), 21, (57, 0), 84);
            Add_Action (Table.States (153), 22, (56, 0), 85);
            Add_Action (Table.States (153), 23, (55, 0), 86);
            Add_Action (Table.States (153), 24, (51, 0), 87);
            Add_Action (Table.States (153), 36, (52, 1), 88);
            Add_Action (Table.States (153), 38, (54, 1), 89);
            Table.States (153).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (153), 51, 90);
            Add_Goto (Table.States (153), 52, 91);
            Add_Goto (Table.States (153), 53, 190);
            Add_Goto (Table.States (153), 54, 93);
            Add_Goto (Table.States (153), 55, 94);
            Add_Goto (Table.States (153), 56, 95);
            Add_Goto (Table.States (153), 57, 96);
            Table.States (153).Kernel := To_Vector ((0 => ((58, 1),  15,  1, (2147483647, 0),  0)));
            Table.States (153).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 88)));
            Table.States (154).Action_List.Set_Capacity (11);
            Add_Action (Table.States (154), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (56, 0),  3, null, null);
            Table.States (154).Kernel := To_Vector ((0 => ((56, 0),  30,  0, (56, 0),  3)));
            Table.States (154).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 0),  3)));
            Table.States (155).Action_List.Set_Capacity (2);
            Add_Action (Table.States (155), 15, (58, 1), 141);
            Add_Action (Table.States (155), 29, (57, 0), 191);
            Table.States (155).Kernel := To_Vector ((((57, 0),  58,  1, (2147483647, 0),  0), ((57, 1),  58,  2,
            (2147483647, 0),  0), ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (155).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  29, 191)));
            Table.States (156).Action_List.Set_Capacity (2);
            Add_Action (Table.States (156), 15, (58, 1), 153);
            Add_Action (Table.States (156), 30, (56, 0), 192);
            Table.States (156).Kernel := To_Vector ((((56, 0),  58,  1, (2147483647, 0),  0), ((58, 1),  58,  2,
            (2147483647, 0),  0)));
            Table.States (156).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (56, 0),  30, 192)));
            Table.States (157).Action_List.Set_Capacity (2);
            Add_Action (Table.States (157), 15, (58, 1), 165);
            Add_Action (Table.States (157), 31, (55, 0), 193);
            Table.States (157).Kernel := To_Vector ((((55, 0),  58,  1, (2147483647, 0),  0), ((56, 1),  58,  2,
            (2147483647, 0),  0), ((57, 2),  58,  2, (2147483647, 0),  0), ((57, 3),  58,  2, (2147483647, 0),  0),
            ((58, 1),  58,  2, (2147483647, 0),  0)));
            Table.States (157).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 0),  31, 193)));
            Table.States (158).Action_List.Set_Capacity (1);
            Add_Action (Table.States (158), 19, (51, 0), 194);
            Table.States (158).Kernel := To_Vector ((0 => ((51, 0),  36,  3, (2147483647, 0),  0)));
            Table.States (158).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  19, 194)));
            Table.States (159).Action_List.Set_Capacity (6);
            Add_Action (Table.States (159), 21, (57, 0), 98);
            Add_Action (Table.States (159), 22, (56, 0), 99);
            Add_Action (Table.States (159), 23, (55, 0), 100);
            Add_Action (Table.States (159), 24, (51, 0), 101);
            Add_Action (Table.States (159), 36, (54, 0), 195);
            Add_Action (Table.States (159), 38, (54, 1), 103);
            Table.States (159).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (159), 51, 104);
            Add_Goto (Table.States (159), 54, 196);
            Add_Goto (Table.States (159), 55, 108);
            Add_Goto (Table.States (159), 56, 109);
            Add_Goto (Table.States (159), 57, 110);
            Table.States (159).Kernel := To_Vector ((0 => ((52, 1),  19,  1, (2147483647, 0),  0)));
            Table.States (159).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  36, 195)));
            Table.States (160).Action_List.Set_Capacity (8);
            Add_Action (Table.States (160), (15, 21, 22, 23, 24, 31, 36, 38), (57, 4),  2, null, null);
            Table.States (160).Kernel := To_Vector ((0 => ((57, 4),  27,  0, (57, 4),  2)));
            Table.States (160).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 4),  2)));
            Table.States (161).Action_List.Set_Capacity (8);
            Add_Action (Table.States (161), (15, 21, 22, 23, 24, 31, 36, 38), (56, 2),  2, null, null);
            Table.States (161).Kernel := To_Vector ((0 => ((56, 2),  28,  0, (56, 2),  2)));
            Table.States (161).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  2)));
            Table.States (162).Action_List.Set_Capacity (8);
            Add_Action (Table.States (162), (15, 21, 22, 23, 24, 31, 36, 38), (57, 5),  2, null, null);
            Table.States (162).Kernel := To_Vector ((0 => ((57, 5),  34,  0, (57, 5),  2)));
            Table.States (162).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 5),  2)));
            Table.States (163).Action_List.Set_Capacity (8);
            Add_Action (Table.States (163), (15, 21, 22, 23, 24, 31, 36, 38), (56, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (163).Kernel := To_Vector ((0 => ((56, 3),  28,  0, (56, 3),  2)));
            Table.States (163).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 3),  2)));
            Table.States (164).Action_List.Set_Capacity (8);
            Add_Action (Table.States (164), (15, 21, 22, 23, 24, 31, 36, 38), (53, 1),  2, null, null);
            Table.States (164).Kernel := To_Vector ((0 => ((53, 1),  52,  0, (53, 1),  2)));
            Table.States (164).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  2)));
            Table.States (165).Action_List.Set_Capacity (6);
            Add_Action (Table.States (165), 21, (57, 0), 98);
            Add_Action (Table.States (165), 22, (56, 0), 99);
            Add_Action (Table.States (165), 23, (55, 0), 100);
            Add_Action (Table.States (165), 24, (51, 0), 101);
            Add_Action (Table.States (165), 36, (52, 1), 102);
            Add_Action (Table.States (165), 38, (54, 1), 103);
            Table.States (165).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (165), 51, 104);
            Add_Goto (Table.States (165), 52, 105);
            Add_Goto (Table.States (165), 53, 197);
            Add_Goto (Table.States (165), 54, 107);
            Add_Goto (Table.States (165), 55, 108);
            Add_Goto (Table.States (165), 56, 109);
            Add_Goto (Table.States (165), 57, 110);
            Table.States (165).Kernel := To_Vector ((0 => ((58, 1),  15,  1, (2147483647, 0),  0)));
            Table.States (165).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 1),  36, 102)));
            Table.States (166).Action_List.Set_Capacity (14);
            Add_Action (Table.States (166), 14, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 15, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 21, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 22, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 23, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 24, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 26, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 27, (57, 2), 198);
            Add_Action (Table.States (166), 28, (56, 1), 199);
            Add_Action (Table.States (166), 32, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 34, (57, 3), 200);
            Add_Action (Table.States (166), 36, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 38, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (166), 39, Reduce, (55, 0),  3, null, null);
            Table.States (166).Kernel := To_Vector ((((55, 0),  31,  0, (55, 0),  3), ((56, 1),  31,  1, (2147483647,
            0),  0), ((57, 2),  31,  1, (2147483647, 0),  0), ((57, 3),  31,  1, (2147483647, 0),  0)));
            Table.States (166).Minimal_Complete_Actions := To_Vector (((Reduce, (55, 0),  3), (Shift, (56, 1),  28,
            199), (Shift, (57, 2),  27, 198), (Shift, (57, 3),  34, 200)));
            Table.States (167).Action_List.Set_Capacity (1);
            Add_Action (Table.States (167), 36, (51, 0), 201);
            Table.States (167).Kernel := To_Vector ((0 => ((51, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (167).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 201)));
            Table.States (168).Action_List.Set_Capacity (14);
            Add_Action (Table.States (168), 14, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 26, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 27, (57, 4), 114);
            Add_Action (Table.States (168), 28, (56, 2), 115);
            Add_Action (Table.States (168), 32, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 34, (57, 5), 116);
            Add_Action (Table.States (168), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 38, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (168), 39, Reduce, (54, 0),  1, null, null);
            Table.States (168).Kernel := To_Vector ((((54, 0),  36,  0, (54, 0),  1), ((56, 2),  36,  1, (2147483647,
            0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,  1, (2147483647, 0),  0)));
            Table.States (168).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            115), (Shift, (57, 4),  27, 114), (Shift, (57, 5),  34, 116)));
            Table.States (169).Action_List.Set_Capacity (11);
            Add_Action (Table.States (169), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (52, 1),  3, null, null);
            Table.States (169).Kernel := To_Vector ((0 => ((52, 1),  54,  0, (52, 1),  3)));
            Table.States (169).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  3)));
            Table.States (170).Action_List.Set_Capacity (5);
            Add_Action (Table.States (170), (15, 26, 32, 36, 39), (49, 1),  3, rhs_list_1'Access, null);
            Table.States (170).Kernel := To_Vector ((0 => ((49, 1),  50,  0, (49, 1),  3)));
            Table.States (170).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  3)));
            Table.States (171).Action_List.Set_Capacity (1);
            Add_Action (Table.States (171), 7, (49, 6), 202);
            Table.States (171).Kernel := To_Vector ((0 => ((49, 6),  5,  1, (2147483647, 0),  0)));
            Table.States (171).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (49, 6),  7, 202)));
            Table.States (172).Action_List.Set_Capacity (1);
            Add_Action (Table.States (172), 36, (49, 4), 203);
            Table.States (172).Kernel := To_Vector ((((49, 4),  6,  3, (2147483647, 0),  0), ((49, 5),  6,  3,
            (2147483647, 0),  0)));
            Table.States (173).Action_List.Set_Capacity (1);
            Add_Action (Table.States (173), 36, (49, 2), 204);
            Table.States (173).Kernel := To_Vector ((((49, 2),  7,  3, (2147483647, 0),  0), ((49, 3),  7,  3,
            (2147483647, 0),  0)));
            Table.States (174).Action_List.Set_Capacity (5);
            Add_Action (Table.States (174), (15, 26, 32, 36, 39), (50, 3),  3, rhs_3'Access, null);
            Table.States (174).Kernel := To_Vector ((0 => ((50, 3),  14,  0, (50, 3),  3)));
            Table.States (174).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 3),  3)));
            Table.States (175).Action_List.Set_Capacity (1);
            Add_Action (Table.States (175), 36, (44, 1), 205);
            Table.States (175).Kernel := To_Vector ((0 => ((44, 1),  15,  1, (2147483647, 0),  0)));
            Table.States (175).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 1),  36, 205)));
            Table.States (176).Action_List.Set_Capacity (9);
            Add_Action (Table.States (176), 15, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 21, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 22, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 23, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 24, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 25, (57, 1), 206);
            Add_Action (Table.States (176), 29, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 36, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (176), 38, Reduce, (57, 0),  3, null, null);
            Table.States (176).Kernel := To_Vector ((((57, 0),  29,  0, (57, 0),  3), ((57, 1),  29,  1, (2147483647,
            0),  0)));
            Table.States (176).Minimal_Complete_Actions := To_Vector (((Reduce, (57, 0),  3), (Shift, (57, 1),  25,
            206)));
         end Subr_4;
         procedure Subr_5
         is begin
            Table.States (177).Action_List.Set_Capacity (8);
            Add_Action (Table.States (177), (15, 21, 22, 23, 24, 29, 36, 38), (56, 0),  3, null, null);
            Table.States (177).Kernel := To_Vector ((0 => ((56, 0),  30,  0, (56, 0),  3)));
            Table.States (177).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 0),  3)));
            Table.States (178).Action_List.Set_Capacity (11);
            Add_Action (Table.States (178), 15, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 21, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 22, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 23, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 24, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 27, (57, 2), 207);
            Add_Action (Table.States (178), 28, (56, 1), 208);
            Add_Action (Table.States (178), 29, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 34, (57, 3), 209);
            Add_Action (Table.States (178), 36, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (178), 38, Reduce, (55, 0),  3, null, null);
            Table.States (178).Kernel := To_Vector ((((55, 0),  31,  0, (55, 0),  3), ((56, 1),  31,  1, (2147483647,
            0),  0), ((57, 2),  31,  1, (2147483647, 0),  0), ((57, 3),  31,  1, (2147483647, 0),  0)));
            Table.States (178).Minimal_Complete_Actions := To_Vector (((Reduce, (55, 0),  3), (Shift, (56, 1),  28,
            208), (Shift, (57, 2),  27, 207), (Shift, (57, 3),  34, 209)));
            Table.States (179).Action_List.Set_Capacity (1);
            Add_Action (Table.States (179), 36, (51, 0), 210);
            Table.States (179).Kernel := To_Vector ((0 => ((51, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (179).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 210)));
            Table.States (180).Action_List.Set_Capacity (11);
            Add_Action (Table.States (180), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 27, (57, 4), 136);
            Add_Action (Table.States (180), 28, (56, 2), 137);
            Add_Action (Table.States (180), 29, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 34, (57, 5), 138);
            Add_Action (Table.States (180), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (180), 38, Reduce, (54, 0),  1, null, null);
            Table.States (180).Kernel := To_Vector ((((54, 0),  36,  0, (54, 0),  1), ((56, 2),  36,  1, (2147483647,
            0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,  1, (2147483647, 0),  0)));
            Table.States (180).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            137), (Shift, (57, 4),  27, 136), (Shift, (57, 5),  34, 138)));
            Table.States (181).Action_List.Set_Capacity (8);
            Add_Action (Table.States (181), (15, 21, 22, 23, 24, 29, 36, 38), (52, 1),  3, null, null);
            Table.States (181).Kernel := To_Vector ((0 => ((52, 1),  54,  0, (52, 1),  3)));
            Table.States (181).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  3)));
            Table.States (182).Action_List.Set_Capacity (8);
            Add_Action (Table.States (182), 15, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (182), 21, (57, 0), 70);
            Add_Action (Table.States (182), 22, (56, 0), 71);
            Add_Action (Table.States (182), 23, (55, 0), 72);
            Add_Action (Table.States (182), 24, (51, 0), 73);
            Add_Action (Table.States (182), 29, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (182), 36, (52, 1), 74);
            Add_Action (Table.States (182), 38, (54, 1), 75);
            Table.States (182).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (182), 51, 76);
            Add_Goto (Table.States (182), 52, 140);
            Add_Goto (Table.States (182), 54, 79);
            Add_Goto (Table.States (182), 55, 80);
            Add_Goto (Table.States (182), 56, 81);
            Add_Goto (Table.States (182), 57, 82);
            Table.States (182).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 1),  53,  0, (58,
            1),  3)));
            Table.States (182).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (183).Action_List.Set_Capacity (11);
            Add_Action (Table.States (183), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (57, 1),  4, null, null);
            Table.States (183).Kernel := To_Vector ((0 => ((57, 1),  25,  0, (57, 1),  4)));
            Table.States (183).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  4)));
            Table.States (184).Action_List.Set_Capacity (9);
            Add_Action (Table.States (184), 15, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 21, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 22, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 23, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 24, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 25, (57, 1), 211);
            Add_Action (Table.States (184), 30, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 36, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (184), 38, Reduce, (57, 0),  3, null, null);
            Table.States (184).Kernel := To_Vector ((((57, 0),  29,  0, (57, 0),  3), ((57, 1),  29,  1, (2147483647,
            0),  0)));
            Table.States (184).Minimal_Complete_Actions := To_Vector (((Reduce, (57, 0),  3), (Shift, (57, 1),  25,
            211)));
            Table.States (185).Action_List.Set_Capacity (8);
            Add_Action (Table.States (185), (15, 21, 22, 23, 24, 30, 36, 38), (56, 0),  3, null, null);
            Table.States (185).Kernel := To_Vector ((0 => ((56, 0),  30,  0, (56, 0),  3)));
            Table.States (185).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 0),  3)));
            Table.States (186).Action_List.Set_Capacity (11);
            Add_Action (Table.States (186), 15, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 21, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 22, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 23, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 24, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 27, (57, 2), 212);
            Add_Action (Table.States (186), 28, (56, 1), 213);
            Add_Action (Table.States (186), 30, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 34, (57, 3), 214);
            Add_Action (Table.States (186), 36, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (186), 38, Reduce, (55, 0),  3, null, null);
            Table.States (186).Kernel := To_Vector ((((55, 0),  31,  0, (55, 0),  3), ((56, 1),  31,  1, (2147483647,
            0),  0), ((57, 2),  31,  1, (2147483647, 0),  0), ((57, 3),  31,  1, (2147483647, 0),  0)));
            Table.States (186).Minimal_Complete_Actions := To_Vector (((Reduce, (55, 0),  3), (Shift, (56, 1),  28,
            213), (Shift, (57, 2),  27, 212), (Shift, (57, 3),  34, 214)));
            Table.States (187).Action_List.Set_Capacity (1);
            Add_Action (Table.States (187), 36, (51, 0), 215);
            Table.States (187).Kernel := To_Vector ((0 => ((51, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (187).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 215)));
            Table.States (188).Action_List.Set_Capacity (11);
            Add_Action (Table.States (188), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 27, (57, 4), 148);
            Add_Action (Table.States (188), 28, (56, 2), 149);
            Add_Action (Table.States (188), 30, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 34, (57, 5), 150);
            Add_Action (Table.States (188), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (188), 38, Reduce, (54, 0),  1, null, null);
            Table.States (188).Kernel := To_Vector ((((54, 0),  36,  0, (54, 0),  1), ((56, 2),  36,  1, (2147483647,
            0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,  1, (2147483647, 0),  0)));
            Table.States (188).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            149), (Shift, (57, 4),  27, 148), (Shift, (57, 5),  34, 150)));
            Table.States (189).Action_List.Set_Capacity (8);
            Add_Action (Table.States (189), (15, 21, 22, 23, 24, 30, 36, 38), (52, 1),  3, null, null);
            Table.States (189).Kernel := To_Vector ((0 => ((52, 1),  54,  0, (52, 1),  3)));
            Table.States (189).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  3)));
            Table.States (190).Action_List.Set_Capacity (8);
            Add_Action (Table.States (190), 15, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (190), 21, (57, 0), 84);
            Add_Action (Table.States (190), 22, (56, 0), 85);
            Add_Action (Table.States (190), 23, (55, 0), 86);
            Add_Action (Table.States (190), 24, (51, 0), 87);
            Add_Action (Table.States (190), 30, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (190), 36, (52, 1), 88);
            Add_Action (Table.States (190), 38, (54, 1), 89);
            Table.States (190).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (190), 51, 90);
            Add_Goto (Table.States (190), 52, 152);
            Add_Goto (Table.States (190), 54, 93);
            Add_Goto (Table.States (190), 55, 94);
            Add_Goto (Table.States (190), 56, 95);
            Add_Goto (Table.States (190), 57, 96);
            Table.States (190).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 1),  53,  0, (58,
            1),  3)));
            Table.States (190).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (191).Action_List.Set_Capacity (9);
            Add_Action (Table.States (191), 15, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 21, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 22, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 23, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 24, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 25, (57, 1), 216);
            Add_Action (Table.States (191), 31, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 36, Reduce, (57, 0),  3, null, null);
            Add_Action (Table.States (191), 38, Reduce, (57, 0),  3, null, null);
            Table.States (191).Kernel := To_Vector ((((57, 0),  29,  0, (57, 0),  3), ((57, 1),  29,  1, (2147483647,
            0),  0)));
            Table.States (191).Minimal_Complete_Actions := To_Vector (((Reduce, (57, 0),  3), (Shift, (57, 1),  25,
            216)));
            Table.States (192).Action_List.Set_Capacity (8);
            Add_Action (Table.States (192), (15, 21, 22, 23, 24, 31, 36, 38), (56, 0),  3, null, null);
            Table.States (192).Kernel := To_Vector ((0 => ((56, 0),  30,  0, (56, 0),  3)));
            Table.States (192).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 0),  3)));
            Table.States (193).Action_List.Set_Capacity (11);
            Add_Action (Table.States (193), 15, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 21, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 22, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 23, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 24, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 27, (57, 2), 217);
            Add_Action (Table.States (193), 28, (56, 1), 218);
            Add_Action (Table.States (193), 31, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 34, (57, 3), 219);
            Add_Action (Table.States (193), 36, Reduce, (55, 0),  3, null, null);
            Add_Action (Table.States (193), 38, Reduce, (55, 0),  3, null, null);
            Table.States (193).Kernel := To_Vector ((((55, 0),  31,  0, (55, 0),  3), ((56, 1),  31,  1, (2147483647,
            0),  0), ((57, 2),  31,  1, (2147483647, 0),  0), ((57, 3),  31,  1, (2147483647, 0),  0)));
            Table.States (193).Minimal_Complete_Actions := To_Vector (((Reduce, (55, 0),  3), (Shift, (56, 1),  28,
            218), (Shift, (57, 2),  27, 217), (Shift, (57, 3),  34, 219)));
            Table.States (194).Action_List.Set_Capacity (1);
            Add_Action (Table.States (194), 36, (51, 0), 220);
            Table.States (194).Kernel := To_Vector ((0 => ((51, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (194).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  36, 220)));
            Table.States (195).Action_List.Set_Capacity (11);
            Add_Action (Table.States (195), 15, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 21, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 22, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 23, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 24, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 27, (57, 4), 160);
            Add_Action (Table.States (195), 28, (56, 2), 161);
            Add_Action (Table.States (195), 31, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 34, (57, 5), 162);
            Add_Action (Table.States (195), 36, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (195), 38, Reduce, (54, 0),  1, null, null);
            Table.States (195).Kernel := To_Vector ((((54, 0),  36,  0, (54, 0),  1), ((56, 2),  36,  1, (2147483647,
            0),  0), ((57, 4),  36,  1, (2147483647, 0),  0), ((57, 5),  36,  1, (2147483647, 0),  0)));
            Table.States (195).Minimal_Complete_Actions := To_Vector (((Reduce, (54, 0),  1), (Shift, (56, 2),  28,
            161), (Shift, (57, 4),  27, 160), (Shift, (57, 5),  34, 162)));
            Table.States (196).Action_List.Set_Capacity (8);
            Add_Action (Table.States (196), (15, 21, 22, 23, 24, 31, 36, 38), (52, 1),  3, null, null);
            Table.States (196).Kernel := To_Vector ((0 => ((52, 1),  54,  0, (52, 1),  3)));
            Table.States (196).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  3)));
            Table.States (197).Action_List.Set_Capacity (8);
            Add_Action (Table.States (197), 15, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (197), 21, (57, 0), 98);
            Add_Action (Table.States (197), 22, (56, 0), 99);
            Add_Action (Table.States (197), 23, (55, 0), 100);
            Add_Action (Table.States (197), 24, (51, 0), 101);
            Add_Action (Table.States (197), 31, Reduce, (58, 1),  3, null, null);
            Add_Action (Table.States (197), 36, (52, 1), 102);
            Add_Action (Table.States (197), 38, (54, 1), 103);
            Table.States (197).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (197), 51, 104);
            Add_Goto (Table.States (197), 52, 164);
            Add_Goto (Table.States (197), 54, 107);
            Add_Goto (Table.States (197), 55, 108);
            Add_Goto (Table.States (197), 56, 109);
            Add_Goto (Table.States (197), 57, 110);
            Table.States (197).Kernel := To_Vector ((((53, 1),  53,  1, (2147483647, 0),  0), ((58, 1),  53,  0, (58,
            1),  3)));
            Table.States (197).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (198).Action_List.Set_Capacity (11);
            Add_Action (Table.States (198), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (57, 2),  4, null, null);
            Table.States (198).Kernel := To_Vector ((0 => ((57, 2),  27,  0, (57, 2),  4)));
            Table.States (198).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 2),  4)));
            Table.States (199).Action_List.Set_Capacity (11);
            Add_Action (Table.States (199), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (56, 1),  4, null, null);
            Table.States (199).Kernel := To_Vector ((0 => ((56, 1),  28,  0, (56, 1),  4)));
            Table.States (199).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (200).Action_List.Set_Capacity (11);
            Add_Action (Table.States (200), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (57, 3),  4, null, null);
            Table.States (200).Kernel := To_Vector ((0 => ((57, 3),  34,  0, (57, 3),  4)));
            Table.States (200).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 3),  4)));
            Table.States (201).Action_List.Set_Capacity (1);
            Add_Action (Table.States (201), 20, (51, 0), 221);
            Table.States (201).Kernel := To_Vector ((0 => ((51, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (201).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  20, 221)));
            Table.States (202).Action_List.Set_Capacity (5);
            Add_Action (Table.States (202), (15, 26, 32, 36, 39), (49, 6),  4, rhs_list_6'Access, null);
            Table.States (202).Kernel := To_Vector ((0 => ((49, 6),  7,  0, (49, 6),  4)));
            Table.States (202).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 6),  4)));
            Table.States (203).Action_List.Set_Capacity (2);
            Add_Action (Table.States (203), 8, (49, 5), 222);
            Add_Action (Table.States (203), 19, (49, 4), 223);
            Table.States (203).Kernel := To_Vector ((((49, 4),  36,  2, (2147483647, 0),  0), ((49, 5),  36,  2,
            (2147483647, 0),  0)));
            Table.States (204).Action_List.Set_Capacity (2);
            Add_Action (Table.States (204), 8, (49, 3), 224);
            Add_Action (Table.States (204), 19, (49, 2), 225);
            Table.States (204).Kernel := To_Vector ((((49, 2),  36,  2, (2147483647, 0),  0), ((49, 3),  36,  2,
            (2147483647, 0),  0)));
            Table.States (205).Action_List.Set_Capacity (4);
            Add_Action (Table.States (205), (15, 26, 36, 39), (44, 1),  3, null, null);
            Table.States (205).Kernel := To_Vector ((0 => ((44, 1),  36,  0, (44, 1),  3)));
            Table.States (205).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  3)));
            Table.States (206).Action_List.Set_Capacity (8);
            Add_Action (Table.States (206), (15, 21, 22, 23, 24, 29, 36, 38), (57, 1),  4, null, null);
            Table.States (206).Kernel := To_Vector ((0 => ((57, 1),  25,  0, (57, 1),  4)));
            Table.States (206).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  4)));
            Table.States (207).Action_List.Set_Capacity (8);
            Add_Action (Table.States (207), (15, 21, 22, 23, 24, 29, 36, 38), (57, 2),  4, null, null);
            Table.States (207).Kernel := To_Vector ((0 => ((57, 2),  27,  0, (57, 2),  4)));
            Table.States (207).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 2),  4)));
            Table.States (208).Action_List.Set_Capacity (8);
            Add_Action (Table.States (208), (15, 21, 22, 23, 24, 29, 36, 38), (56, 1),  4, null, null);
            Table.States (208).Kernel := To_Vector ((0 => ((56, 1),  28,  0, (56, 1),  4)));
            Table.States (208).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (209).Action_List.Set_Capacity (8);
            Add_Action (Table.States (209), (15, 21, 22, 23, 24, 29, 36, 38), (57, 3),  4, null, null);
            Table.States (209).Kernel := To_Vector ((0 => ((57, 3),  34,  0, (57, 3),  4)));
            Table.States (209).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 3),  4)));
            Table.States (210).Action_List.Set_Capacity (1);
            Add_Action (Table.States (210), 20, (51, 0), 226);
            Table.States (210).Kernel := To_Vector ((0 => ((51, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (210).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  20, 226)));
            Table.States (211).Action_List.Set_Capacity (8);
            Add_Action (Table.States (211), (15, 21, 22, 23, 24, 30, 36, 38), (57, 1),  4, null, null);
            Table.States (211).Kernel := To_Vector ((0 => ((57, 1),  25,  0, (57, 1),  4)));
            Table.States (211).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  4)));
            Table.States (212).Action_List.Set_Capacity (8);
            Add_Action (Table.States (212), (15, 21, 22, 23, 24, 30, 36, 38), (57, 2),  4, null, null);
            Table.States (212).Kernel := To_Vector ((0 => ((57, 2),  27,  0, (57, 2),  4)));
            Table.States (212).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 2),  4)));
            Table.States (213).Action_List.Set_Capacity (8);
            Add_Action (Table.States (213), (15, 21, 22, 23, 24, 30, 36, 38), (56, 1),  4, null, null);
            Table.States (213).Kernel := To_Vector ((0 => ((56, 1),  28,  0, (56, 1),  4)));
            Table.States (213).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (214).Action_List.Set_Capacity (8);
            Add_Action (Table.States (214), (15, 21, 22, 23, 24, 30, 36, 38), (57, 3),  4, null, null);
            Table.States (214).Kernel := To_Vector ((0 => ((57, 3),  34,  0, (57, 3),  4)));
            Table.States (214).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 3),  4)));
            Table.States (215).Action_List.Set_Capacity (1);
            Add_Action (Table.States (215), 20, (51, 0), 227);
            Table.States (215).Kernel := To_Vector ((0 => ((51, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (215).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  20, 227)));
            Table.States (216).Action_List.Set_Capacity (8);
            Add_Action (Table.States (216), (15, 21, 22, 23, 24, 31, 36, 38), (57, 1),  4, null, null);
            Table.States (216).Kernel := To_Vector ((0 => ((57, 1),  25,  0, (57, 1),  4)));
            Table.States (216).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  4)));
            Table.States (217).Action_List.Set_Capacity (8);
            Add_Action (Table.States (217), (15, 21, 22, 23, 24, 31, 36, 38), (57, 2),  4, null, null);
            Table.States (217).Kernel := To_Vector ((0 => ((57, 2),  27,  0, (57, 2),  4)));
            Table.States (217).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 2),  4)));
            Table.States (218).Action_List.Set_Capacity (8);
            Add_Action (Table.States (218), (15, 21, 22, 23, 24, 31, 36, 38), (56, 1),  4, null, null);
            Table.States (218).Kernel := To_Vector ((0 => ((56, 1),  28,  0, (56, 1),  4)));
            Table.States (218).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  4)));
            Table.States (219).Action_List.Set_Capacity (8);
            Add_Action (Table.States (219), (15, 21, 22, 23, 24, 31, 36, 38), (57, 3),  4, null, null);
            Table.States (219).Kernel := To_Vector ((0 => ((57, 3),  34,  0, (57, 3),  4)));
            Table.States (219).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 3),  4)));
            Table.States (220).Action_List.Set_Capacity (1);
            Add_Action (Table.States (220), 20, (51, 0), 228);
            Table.States (220).Kernel := To_Vector ((0 => ((51, 0),  36,  1, (2147483647, 0),  0)));
            Table.States (220).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  20, 228)));
            Table.States (221).Action_List.Set_Capacity (11);
            Add_Action (Table.States (221), (14, 15, 21, 22, 23, 24, 26, 32, 36, 38, 39), (51, 0),  5, null, null);
            Table.States (221).Kernel := To_Vector ((0 => ((51, 0),  20,  0, (51, 0),  5)));
            Table.States (221).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  5)));
            Table.States (222).Action_List.Set_Capacity (1);
            Add_Action (Table.States (222), 36, (44, 0), 229);
            Table.States (222).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (222), 44, 230);
            Table.States (222).Kernel := To_Vector ((0 => ((49, 5),  8,  1, (2147483647, 0),  0)));
            Table.States (222).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 0),  36, 229)));
            Table.States (223).Action_List.Set_Capacity (1);
            Add_Action (Table.States (223), 36, (49, 4), 231);
            Table.States (223).Kernel := To_Vector ((0 => ((49, 4),  19,  1, (2147483647, 0),  0)));
            Table.States (223).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (49, 4),  36, 231)));
            Table.States (224).Action_List.Set_Capacity (1);
            Add_Action (Table.States (224), 36, (44, 0), 229);
            Table.States (224).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (224), 44, 232);
            Table.States (224).Kernel := To_Vector ((0 => ((49, 3),  8,  1, (2147483647, 0),  0)));
            Table.States (224).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 0),  36, 229)));
            Table.States (225).Action_List.Set_Capacity (1);
            Add_Action (Table.States (225), 36, (49, 2), 233);
            Table.States (225).Kernel := To_Vector ((0 => ((49, 2),  19,  1, (2147483647, 0),  0)));
            Table.States (225).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (49, 2),  36, 233)));
            Table.States (226).Action_List.Set_Capacity (8);
            Add_Action (Table.States (226), (15, 21, 22, 23, 24, 29, 36, 38), (51, 0),  5, null, null);
            Table.States (226).Kernel := To_Vector ((0 => ((51, 0),  20,  0, (51, 0),  5)));
            Table.States (226).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  5)));
            Table.States (227).Action_List.Set_Capacity (8);
            Add_Action (Table.States (227), (15, 21, 22, 23, 24, 30, 36, 38), (51, 0),  5, null, null);
            Table.States (227).Kernel := To_Vector ((0 => ((51, 0),  20,  0, (51, 0),  5)));
            Table.States (227).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  5)));
            Table.States (228).Action_List.Set_Capacity (8);
            Add_Action (Table.States (228), (15, 21, 22, 23, 24, 31, 36, 38), (51, 0),  5, null, null);
            Table.States (228).Kernel := To_Vector ((0 => ((51, 0),  20,  0, (51, 0),  5)));
            Table.States (228).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  5)));
         end Subr_5;
         procedure Subr_6
         is begin
            Table.States (229).Action_List.Set_Capacity (5);
            Add_Action (Table.States (229), (15, 26, 32, 36, 39), (44, 0),  1, null, null);
            Table.States (229).Kernel := To_Vector ((0 => ((44, 0),  36,  0, (44, 0),  1)));
            Table.States (229).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (230).Action_List.Set_Capacity (5);
            Add_Action (Table.States (230), 15, (44, 1), 234);
            Add_Conflict (Table.States (230), 15, (49, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (230), 26, Reduce, (49, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (230), 32, Reduce, (49, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (230), 36, Reduce, (49, 5),  6, rhs_list_5'Access, null);
            Add_Action (Table.States (230), 39, Reduce, (49, 5),  6, rhs_list_5'Access, null);
            Table.States (230).Kernel := To_Vector ((((44, 1),  44,  2, (2147483647, 0),  0), ((49, 5),  44,  0, (49,
            5),  6)));
            Table.States (230).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 5),  6)));
            Table.States (231).Action_List.Set_Capacity (5);
            Add_Action (Table.States (231), (15, 26, 32, 36, 39), (49, 4),  6, rhs_list_4'Access, null);
            Table.States (231).Kernel := To_Vector ((0 => ((49, 4),  36,  0, (49, 4),  6)));
            Table.States (231).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 4),  6)));
            Table.States (232).Action_List.Set_Capacity (5);
            Add_Action (Table.States (232), 15, (44, 1), 234);
            Add_Conflict (Table.States (232), 15, (49, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (232), 26, Reduce, (49, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (232), 32, Reduce, (49, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (232), 36, Reduce, (49, 3),  6, rhs_list_3'Access, null);
            Add_Action (Table.States (232), 39, Reduce, (49, 3),  6, rhs_list_3'Access, null);
            Table.States (232).Kernel := To_Vector ((((44, 1),  44,  2, (2147483647, 0),  0), ((49, 3),  44,  0, (49,
            3),  6)));
            Table.States (232).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 3),  6)));
            Table.States (233).Action_List.Set_Capacity (5);
            Add_Action (Table.States (233), (15, 26, 32, 36, 39), (49, 2),  6, rhs_list_2'Access, null);
            Table.States (233).Kernel := To_Vector ((0 => ((49, 2),  36,  0, (49, 2),  6)));
            Table.States (233).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 2),  6)));
            Table.States (234).Action_List.Set_Capacity (1);
            Add_Action (Table.States (234), 36, (44, 1), 235);
            Table.States (234).Kernel := To_Vector ((0 => ((44, 1),  15,  1, (2147483647, 0),  0)));
            Table.States (234).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (44, 1),  36, 235)));
            Table.States (235).Action_List.Set_Capacity (5);
            Add_Action (Table.States (235), (15, 26, 32, 36, 39), (44, 1),  3, null, null);
            Table.States (235).Kernel := To_Vector ((0 => ((44, 1),  36,  0, (44, 1),  3)));
            Table.States (235).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  3)));
         end Subr_6;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Subr_4;
         Subr_5;
         Subr_6;
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
