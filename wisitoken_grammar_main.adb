--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
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

with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_re2c_c;
package body Wisitoken_Grammar_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_re2c_c.New_Lexer,
      wisitoken_grammar_re2c_c.Free_Lexer,
      wisitoken_grammar_re2c_c.Reset_Lexer,
      wisitoken_grammar_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                         :    out WisiToken.Parse.LR.Parser_No_Recover.Parser;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 103,
         First_Terminal    => 3,
         Last_Terminal     => 36,
         First_Nonterminal => 37,
         Last_Nonterminal  => 56);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 23, (38, 0), 1);
            Add_Action (Table.States (0), 33, (43, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 38, 3);
            Add_Goto (Table.States (0), 43, 4);
            Add_Goto (Table.States (0), 55, 5);
            Add_Goto (Table.States (0), 56, 6);
            Table.States (1).Action_List.Set_Capacity (7);
            Add_Action (Table.States (1), 3, (38, 1), 7);
            Add_Action (Table.States (1), 4, (38, 5), 8);
            Add_Action (Table.States (1), 5, (38, 4), 9);
            Add_Action (Table.States (1), 6, (39, 0), 10);
            Add_Action (Table.States (1), 7, (39, 1), 11);
            Add_Action (Table.States (1), 8, (39, 2), 12);
            Add_Action (Table.States (1), 33, (38, 2), 13);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 39, 14);
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 13, (43, 0), 15);
            Add_Action (Table.States (2), 14, (43, 1), 16);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (23, 33, 36), (55, 0),  1, null, null);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (23, 33, 36), (55, 1),  1, null, null);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (23, 33, 36), (56, 0),  1, null, null);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 23, (38, 0), 1);
            Add_Action (Table.States (6), 33, (43, 0), 2);
            Add_Action (Table.States (6), 36, Accept_It, (37, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 38, 3);
            Add_Goto (Table.States (6), 43, 4);
            Add_Goto (Table.States (6), 55, 17);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 33, (40, 0), 18);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 40, 19);
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 5, (38, 5), 20);
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 33, (38, 4), 21);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), (1 =>  33), (39, 0),  1, null, null);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 21, (39, 1), 22);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 21, (39, 2), 23);
            Table.States (13).Action_List.Set_Capacity (14);
            Add_Action (Table.States (13), 8, (42, 11), 24);
            Add_Action (Table.States (13), 10, (42, 6), 25);
            Add_Action (Table.States (13), 12, (42, 0), 26);
            Add_Action (Table.States (13), 15, (42, 1), 27);
            Add_Action (Table.States (13), 16, (42, 3), 28);
            Add_Action (Table.States (13), 20, (42, 4), 29);
            Add_Action (Table.States (13), 23, Reduce, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 28, (42, 7), 30);
            Add_Action (Table.States (13), 30, (42, 8), 31);
            Add_Action (Table.States (13), 32, (42, 5), 32);
            Add_Action (Table.States (13), 33, (42, 2), 33);
            Add_Conflict (Table.States (13), 33, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 34, (42, 9), 34);
            Add_Action (Table.States (13), 35, (42, 10), 35);
            Add_Action (Table.States (13), 36, Reduce, (38, 3),  2, declaration_3'Access, null);
            Table.States (13).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (13), 41, 36);
            Add_Goto (Table.States (13), 42, 37);
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 33, (38, 0), 38);
            Table.States (15).Action_List.Set_Capacity (10);
            Add_Action (Table.States (15), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 18, (53, 0), 39);
            Add_Action (Table.States (15), 19, (52, 0), 40);
            Add_Action (Table.States (15), 20, (51, 0), 41);
            Add_Action (Table.States (15), 21, (47, 0), 42);
            Add_Action (Table.States (15), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 33, (48, 1), 43);
            Add_Conflict (Table.States (15), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 35, (50, 1), 44);
            Add_Action (Table.States (15), 36, Reduce, (46, 0),  0, null, null);
            Table.States (15).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (15), 45, 45);
            Add_Goto (Table.States (15), 46, 46);
            Add_Goto (Table.States (15), 47, 47);
            Add_Goto (Table.States (15), 48, 48);
            Add_Goto (Table.States (15), 49, 49);
            Add_Goto (Table.States (15), 50, 50);
            Add_Goto (Table.States (15), 51, 51);
            Add_Goto (Table.States (15), 52, 52);
            Add_Goto (Table.States (15), 53, 53);
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 18, (53, 0), 39);
            Add_Action (Table.States (16), 19, (52, 0), 40);
            Add_Action (Table.States (16), 20, (51, 0), 41);
            Add_Action (Table.States (16), 21, (47, 0), 42);
            Add_Action (Table.States (16), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 33, (48, 1), 43);
            Add_Conflict (Table.States (16), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 35, (50, 1), 44);
            Add_Action (Table.States (16), 36, Reduce, (46, 0),  0, null, null);
            Table.States (16).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (16), 45, 54);
            Add_Goto (Table.States (16), 46, 46);
            Add_Goto (Table.States (16), 47, 47);
            Add_Goto (Table.States (16), 48, 48);
            Add_Goto (Table.States (16), 49, 49);
            Add_Goto (Table.States (16), 50, 50);
            Add_Goto (Table.States (16), 51, 51);
            Add_Goto (Table.States (16), 52, 52);
            Add_Goto (Table.States (16), 53, 53);
            Table.States (17).Action_List.Set_Capacity (3);
            Add_Action (Table.States (17), (23, 33, 36), (56, 1),  2, null, null);
            Table.States (18).Action_List.Set_Capacity (2);
            Add_Action (Table.States (18), (9, 33), (40, 0),  1, null, null);
            Table.States (19).Action_List.Set_Capacity (2);
            Add_Action (Table.States (19), 9, (38, 1), 55);
            Add_Action (Table.States (19), 33, (40, 1), 56);
            Table.States (20).Action_List.Set_Capacity (3);
            Add_Action (Table.States (20), (23, 33, 36), (38, 5),  3, declaration_5'Access, null);
            Table.States (21).Action_List.Set_Capacity (1);
            Add_Action (Table.States (21), 16, (38, 4), 57);
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 33, (39, 1), 58);
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 33, (39, 2), 59);
            Table.States (24).Action_List.Set_Capacity (14);
            Add_Action (Table.States (24), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 11),  1, null,
            null);
            Table.States (25).Action_List.Set_Capacity (14);
            Add_Action (Table.States (25), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 6),  1, null,
            null);
            Table.States (26).Action_List.Set_Capacity (14);
            Add_Action (Table.States (26), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 0),  1, null,
            null);
            Table.States (27).Action_List.Set_Capacity (14);
            Add_Action (Table.States (27), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 1),  1, null,
            null);
            Table.States (28).Action_List.Set_Capacity (14);
            Add_Action (Table.States (28), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 3),  1, null,
            null);
            Table.States (29).Action_List.Set_Capacity (14);
            Add_Action (Table.States (29), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 4),  1, null,
            null);
            Table.States (30).Action_List.Set_Capacity (14);
            Add_Action (Table.States (30), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 7),  1, null,
            null);
            Table.States (31).Action_List.Set_Capacity (14);
            Add_Action (Table.States (31), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 8),  1, null,
            null);
            Table.States (32).Action_List.Set_Capacity (14);
            Add_Action (Table.States (32), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 5),  1, null,
            null);
            Table.States (33).Action_List.Set_Capacity (14);
            Add_Action (Table.States (33), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 2),  1, null,
            null);
            Table.States (34).Action_List.Set_Capacity (14);
            Add_Action (Table.States (34), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 9),  1, null,
            null);
            Table.States (35).Action_List.Set_Capacity (14);
            Add_Action (Table.States (35), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 10),  1, null,
            null);
            Table.States (36).Action_List.Set_Capacity (14);
            Add_Action (Table.States (36), 8, (42, 11), 24);
            Add_Action (Table.States (36), 10, (42, 6), 25);
            Add_Action (Table.States (36), 12, (42, 0), 26);
            Add_Action (Table.States (36), 15, (42, 1), 27);
            Add_Action (Table.States (36), 16, (42, 3), 28);
            Add_Action (Table.States (36), 20, (42, 4), 29);
            Add_Action (Table.States (36), 23, Reduce, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (36), 28, (42, 7), 30);
            Add_Action (Table.States (36), 30, (42, 8), 31);
            Add_Action (Table.States (36), 32, (42, 5), 32);
            Add_Action (Table.States (36), 33, (42, 2), 33);
            Add_Conflict (Table.States (36), 33, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (36), 34, (42, 9), 34);
            Add_Action (Table.States (36), 35, (42, 10), 35);
            Add_Action (Table.States (36), 36, Reduce, (38, 2),  3, declaration_2'Access, null);
            Table.States (36).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (36), 42, 60);
            Table.States (37).Action_List.Set_Capacity (14);
            Add_Action (Table.States (37), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 0),  1, null,
            null);
            Table.States (38).Action_List.Set_Capacity (12);
            Add_Action (Table.States (38), 8, (42, 11), 24);
            Add_Action (Table.States (38), 10, (42, 6), 25);
            Add_Action (Table.States (38), 12, (42, 0), 26);
            Add_Action (Table.States (38), 15, (42, 1), 27);
            Add_Action (Table.States (38), 16, (42, 3), 28);
            Add_Action (Table.States (38), 20, (42, 4), 29);
            Add_Action (Table.States (38), 28, (42, 7), 30);
            Add_Action (Table.States (38), 30, (42, 8), 31);
            Add_Action (Table.States (38), 32, (42, 5), 32);
            Add_Action (Table.States (38), 33, (42, 2), 33);
            Add_Action (Table.States (38), 34, (42, 9), 34);
            Add_Action (Table.States (38), 35, (42, 10), 35);
            Table.States (38).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (38), 41, 61);
            Add_Goto (Table.States (38), 42, 37);
            Table.States (39).Action_List.Set_Capacity (6);
            Add_Action (Table.States (39), 18, (53, 0), 39);
            Add_Action (Table.States (39), 19, (52, 0), 40);
            Add_Action (Table.States (39), 20, (51, 0), 41);
            Add_Action (Table.States (39), 21, (47, 0), 42);
            Add_Action (Table.States (39), 33, (48, 1), 43);
            Add_Action (Table.States (39), 35, (50, 1), 44);
            Table.States (39).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (39), 47, 47);
            Add_Goto (Table.States (39), 48, 48);
            Add_Goto (Table.States (39), 49, 62);
            Add_Goto (Table.States (39), 50, 50);
            Add_Goto (Table.States (39), 51, 51);
            Add_Goto (Table.States (39), 52, 52);
            Add_Goto (Table.States (39), 53, 53);
            Add_Goto (Table.States (39), 54, 63);
            Table.States (40).Action_List.Set_Capacity (6);
            Add_Action (Table.States (40), 18, (53, 0), 39);
            Add_Action (Table.States (40), 19, (52, 0), 40);
            Add_Action (Table.States (40), 20, (51, 0), 41);
            Add_Action (Table.States (40), 21, (47, 0), 42);
            Add_Action (Table.States (40), 33, (48, 1), 43);
            Add_Action (Table.States (40), 35, (50, 1), 44);
            Table.States (40).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (40), 47, 47);
            Add_Goto (Table.States (40), 48, 48);
            Add_Goto (Table.States (40), 49, 62);
            Add_Goto (Table.States (40), 50, 50);
            Add_Goto (Table.States (40), 51, 51);
            Add_Goto (Table.States (40), 52, 52);
            Add_Goto (Table.States (40), 53, 53);
            Add_Goto (Table.States (40), 54, 64);
            Table.States (41).Action_List.Set_Capacity (6);
            Add_Action (Table.States (41), 18, (53, 0), 39);
            Add_Action (Table.States (41), 19, (52, 0), 40);
            Add_Action (Table.States (41), 20, (51, 0), 41);
            Add_Action (Table.States (41), 21, (47, 0), 42);
            Add_Action (Table.States (41), 33, (48, 1), 43);
            Add_Action (Table.States (41), 35, (50, 1), 44);
            Table.States (41).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (41), 47, 47);
            Add_Goto (Table.States (41), 48, 48);
            Add_Goto (Table.States (41), 49, 62);
            Add_Goto (Table.States (41), 50, 50);
            Add_Goto (Table.States (41), 51, 51);
            Add_Goto (Table.States (41), 52, 52);
            Add_Goto (Table.States (41), 53, 53);
            Add_Goto (Table.States (41), 54, 65);
            Table.States (42).Action_List.Set_Capacity (1);
            Add_Action (Table.States (42), 33, (47, 0), 66);
            Table.States (43).Action_List.Set_Capacity (18);
            Add_Action (Table.States (43), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 16, (48, 1), 67);
            Add_Action (Table.States (43), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 24, (53, 4), 68);
            Add_Action (Table.States (43), 25, (52, 2), 69);
            Add_Action (Table.States (43), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 31, (53, 5), 70);
            Add_Action (Table.States (43), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 36, Reduce, (50, 0),  1, null, null);
            Table.States (44).Action_List.Set_Capacity (15);
            Add_Action (Table.States (44), 11, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 23, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 25, (52, 3), 71);
            Add_Action (Table.States (44), 26, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 27, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 28, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 29, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 36, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (45).Action_List.Set_Capacity (5);
            Add_Action (Table.States (45), 12, (45, 1), 72);
            Add_Action (Table.States (45), 23, (45, 2), 73);
            Add_Conflict (Table.States (45), 23, (44, 1),  0, null, null);
            Add_Action (Table.States (45), 29, (44, 0), 74);
            Add_Action (Table.States (45), 33, Reduce, (44, 1),  0, null, null);
            Add_Action (Table.States (45), 36, Reduce, (44, 1),  0, null, null);
            Table.States (45).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (45), 44, 75);
            Table.States (46).Action_List.Set_Capacity (5);
            Add_Action (Table.States (46), (12, 23, 29, 33, 36), (45, 0),  1, null, null);
            Table.States (47).Action_List.Set_Capacity (14);
            Add_Action (Table.States (47), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 2),  1,
            rhs_item_2'Access, null);
            Table.States (48).Action_List.Set_Capacity (14);
            Add_Action (Table.States (48), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (49, 0),  1, null,
            null);
            Table.States (49).Action_List.Set_Capacity (11);
            Add_Action (Table.States (49), 11, (46, 2), 76);
            Add_Action (Table.States (49), 12, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (49), 18, (53, 0), 39);
            Add_Action (Table.States (49), 19, (52, 0), 40);
            Add_Action (Table.States (49), 20, (51, 0), 41);
            Add_Action (Table.States (49), 21, (47, 0), 42);
            Add_Action (Table.States (49), 23, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (49), 29, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (49), 33, (48, 1), 43);
            Add_Conflict (Table.States (49), 33, (46, 1),  1, null, null);
            Add_Action (Table.States (49), 35, (50, 1), 44);
            Add_Action (Table.States (49), 36, Reduce, (46, 1),  1, null, null);
            Table.States (49).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (49), 47, 47);
            Add_Goto (Table.States (49), 48, 77);
            Add_Goto (Table.States (49), 50, 50);
            Add_Goto (Table.States (49), 51, 51);
            Add_Goto (Table.States (49), 52, 52);
            Add_Goto (Table.States (49), 53, 53);
            Table.States (50).Action_List.Set_Capacity (14);
            Add_Action (Table.States (50), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (48, 0),  1, null,
            null);
            Table.States (51).Action_List.Set_Capacity (14);
            Add_Action (Table.States (51), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 5),  1,
            rhs_item_5'Access, null);
            Table.States (52).Action_List.Set_Capacity (14);
            Add_Action (Table.States (52), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 3),  1,
            rhs_item_3'Access, null);
            Table.States (53).Action_List.Set_Capacity (14);
            Add_Action (Table.States (53), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 4),  1,
            rhs_item_4'Access, null);
            Table.States (54).Action_List.Set_Capacity (5);
            Add_Action (Table.States (54), 12, (45, 1), 72);
            Add_Action (Table.States (54), 23, (45, 2), 73);
            Add_Conflict (Table.States (54), 23, (44, 1),  0, null, null);
            Add_Action (Table.States (54), 29, (44, 0), 74);
            Add_Action (Table.States (54), 33, Reduce, (44, 1),  0, null, null);
            Add_Action (Table.States (54), 36, Reduce, (44, 1),  0, null, null);
            Table.States (54).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (54), 44, 78);
            Table.States (55).Action_List.Set_Capacity (3);
            Add_Action (Table.States (55), (23, 33, 36), (38, 1),  4, declaration_1'Access, null);
            Table.States (56).Action_List.Set_Capacity (2);
            Add_Action (Table.States (56), (9, 33), (40, 1),  2, null, null);
            Table.States (57).Action_List.Set_Capacity (1);
            Add_Action (Table.States (57), 33, (38, 4), 79);
            Table.States (58).Action_List.Set_Capacity (1);
            Add_Action (Table.States (58), 17, (39, 1), 80);
            Table.States (59).Action_List.Set_Capacity (1);
            Add_Action (Table.States (59), 17, (39, 2), 81);
            Table.States (60).Action_List.Set_Capacity (14);
            Add_Action (Table.States (60), (8, 10, 12, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 1),  2, null,
            null);
            Table.States (61).Action_List.Set_Capacity (14);
            Add_Action (Table.States (61), 8, (42, 11), 24);
            Add_Action (Table.States (61), 10, (42, 6), 25);
            Add_Action (Table.States (61), 12, (42, 0), 26);
            Add_Action (Table.States (61), 15, (42, 1), 27);
            Add_Action (Table.States (61), 16, (42, 3), 28);
            Add_Action (Table.States (61), 20, (42, 4), 29);
            Add_Action (Table.States (61), 23, Reduce, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (61), 28, (42, 7), 30);
            Add_Action (Table.States (61), 30, (42, 8), 31);
            Add_Action (Table.States (61), 32, (42, 5), 32);
            Add_Action (Table.States (61), 33, (42, 2), 33);
            Add_Conflict (Table.States (61), 33, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (61), 34, (42, 9), 34);
            Add_Action (Table.States (61), 35, (42, 10), 35);
            Add_Action (Table.States (61), 36, Reduce, (38, 0),  4, declaration_0'Access, null);
            Table.States (61).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (61), 42, 60);
            Table.States (62).Action_List.Set_Capacity (10);
            Add_Action (Table.States (62), 12, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (62), 18, (53, 0), 39);
            Add_Action (Table.States (62), 19, (52, 0), 40);
            Add_Action (Table.States (62), 20, (51, 0), 41);
            Add_Action (Table.States (62), 21, (47, 0), 42);
            Add_Action (Table.States (62), 26, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (62), 27, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (62), 28, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (62), 33, (48, 1), 43);
            Add_Action (Table.States (62), 35, (50, 1), 44);
            Table.States (62).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (62), 47, 47);
            Add_Goto (Table.States (62), 48, 77);
            Add_Goto (Table.States (62), 50, 50);
            Add_Goto (Table.States (62), 51, 51);
            Add_Goto (Table.States (62), 52, 52);
            Add_Goto (Table.States (62), 53, 53);
            Table.States (63).Action_List.Set_Capacity (2);
            Add_Action (Table.States (63), 12, (54, 1), 82);
            Add_Action (Table.States (63), 26, (53, 0), 83);
            Table.States (64).Action_List.Set_Capacity (2);
            Add_Action (Table.States (64), 12, (54, 1), 82);
            Add_Action (Table.States (64), 27, (52, 0), 84);
            Table.States (65).Action_List.Set_Capacity (2);
            Add_Action (Table.States (65), 12, (54, 1), 82);
            Add_Action (Table.States (65), 28, (51, 0), 85);
            Table.States (66).Action_List.Set_Capacity (1);
            Add_Action (Table.States (66), 16, (47, 0), 86);
            Table.States (67).Action_List.Set_Capacity (6);
            Add_Action (Table.States (67), 18, (53, 0), 39);
            Add_Action (Table.States (67), 19, (52, 0), 40);
            Add_Action (Table.States (67), 20, (51, 0), 41);
            Add_Action (Table.States (67), 21, (47, 0), 42);
            Add_Action (Table.States (67), 33, (50, 0), 87);
            Add_Action (Table.States (67), 35, (50, 1), 44);
            Table.States (67).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (67), 47, 47);
            Add_Goto (Table.States (67), 50, 88);
            Add_Goto (Table.States (67), 51, 51);
            Add_Goto (Table.States (67), 52, 52);
            Add_Goto (Table.States (67), 53, 53);
            Table.States (68).Action_List.Set_Capacity (14);
            Add_Action (Table.States (68), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 4),  2, null,
            null);
            Table.States (69).Action_List.Set_Capacity (14);
            Add_Action (Table.States (69), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 2),  2, null,
            null);
            Table.States (70).Action_List.Set_Capacity (14);
            Add_Action (Table.States (70), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 5),  2, null,
            null);
            Table.States (71).Action_List.Set_Capacity (14);
            Add_Action (Table.States (71), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (72).Action_List.Set_Capacity (10);
            Add_Action (Table.States (72), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (72), 18, (53, 0), 39);
            Add_Action (Table.States (72), 19, (52, 0), 40);
            Add_Action (Table.States (72), 20, (51, 0), 41);
            Add_Action (Table.States (72), 21, (47, 0), 42);
            Add_Action (Table.States (72), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (72), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (72), 33, (48, 1), 43);
            Add_Conflict (Table.States (72), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (72), 35, (50, 1), 44);
            Add_Action (Table.States (72), 36, Reduce, (46, 0),  0, null, null);
            Table.States (72).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (72), 46, 89);
            Add_Goto (Table.States (72), 47, 47);
            Add_Goto (Table.States (72), 48, 48);
            Add_Goto (Table.States (72), 49, 49);
            Add_Goto (Table.States (72), 50, 50);
            Add_Goto (Table.States (72), 51, 51);
            Add_Goto (Table.States (72), 52, 52);
            Add_Goto (Table.States (72), 53, 53);
            Table.States (73).Action_List.Set_Capacity (2);
            Add_Action (Table.States (73), 4, (45, 3), 90);
            Add_Action (Table.States (73), 5, (45, 2), 91);
            Table.States (74).Action_List.Set_Capacity (3);
            Add_Action (Table.States (74), (23, 33, 36), (44, 0),  1, null, null);
            Table.States (75).Action_List.Set_Capacity (3);
            Add_Action (Table.States (75), (23, 33, 36), (43, 0),  4, nonterminal_0'Access, null);
            Table.States (76).Action_List.Set_Capacity (6);
            Add_Action (Table.States (76), 11, (46, 3), 92);
            Add_Action (Table.States (76), 12, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (76), 23, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (76), 29, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (76), 33, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (76), 36, Reduce, (46, 2),  2, null, null);
            Table.States (77).Action_List.Set_Capacity (14);
            Add_Action (Table.States (77), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (49, 1),  2, null,
            null);
            Table.States (78).Action_List.Set_Capacity (3);
            Add_Action (Table.States (78), (23, 33, 36), (43, 1),  4, nonterminal_1'Access, null);
            Table.States (79).Action_List.Set_Capacity (3);
            Add_Action (Table.States (79), (23, 33, 36), (38, 4),  5, declaration_4'Access, null);
            Table.States (80).Action_List.Set_Capacity (1);
            Add_Action (Table.States (80), (1 =>  33), (39, 1),  4, null, null);
            Table.States (81).Action_List.Set_Capacity (1);
            Add_Action (Table.States (81), (1 =>  33), (39, 2),  4, null, null);
            Table.States (82).Action_List.Set_Capacity (6);
            Add_Action (Table.States (82), 18, (53, 0), 39);
            Add_Action (Table.States (82), 19, (52, 0), 40);
            Add_Action (Table.States (82), 20, (51, 0), 41);
            Add_Action (Table.States (82), 21, (47, 0), 42);
            Add_Action (Table.States (82), 33, (48, 1), 43);
            Add_Action (Table.States (82), 35, (50, 1), 44);
            Table.States (82).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (82), 47, 47);
            Add_Goto (Table.States (82), 48, 48);
            Add_Goto (Table.States (82), 49, 93);
            Add_Goto (Table.States (82), 50, 50);
            Add_Goto (Table.States (82), 51, 51);
            Add_Goto (Table.States (82), 52, 52);
            Add_Goto (Table.States (82), 53, 53);
            Table.States (83).Action_List.Set_Capacity (15);
            Add_Action (Table.States (83), 11, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 22, (53, 1), 94);
            Add_Action (Table.States (83), 23, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 26, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 27, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 28, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 29, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 35, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (83), 36, Reduce, (53, 0),  3, null, null);
            Table.States (84).Action_List.Set_Capacity (14);
            Add_Action (Table.States (84), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 0),  3, null,
            null);
            Table.States (85).Action_List.Set_Capacity (17);
            Add_Action (Table.States (85), 11, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 23, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 24, (53, 2), 95);
            Add_Action (Table.States (85), 25, (52, 1), 96);
            Add_Action (Table.States (85), 26, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 27, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 28, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 29, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 31, (53, 3), 97);
            Add_Action (Table.States (85), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 35, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (85), 36, Reduce, (51, 0),  3, null, null);
            Table.States (86).Action_List.Set_Capacity (1);
            Add_Action (Table.States (86), 33, (47, 0), 98);
            Table.States (87).Action_List.Set_Capacity (17);
            Add_Action (Table.States (87), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 24, (53, 4), 68);
            Add_Action (Table.States (87), 25, (52, 2), 69);
            Add_Action (Table.States (87), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 31, (53, 5), 70);
            Add_Action (Table.States (87), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (87), 36, Reduce, (50, 0),  1, null, null);
            Table.States (88).Action_List.Set_Capacity (14);
            Add_Action (Table.States (88), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (48, 1),  3, null,
            null);
            Table.States (89).Action_List.Set_Capacity (5);
            Add_Action (Table.States (89), (12, 23, 29, 33, 36), (45, 1),  3, null, null);
            Table.States (90).Action_List.Set_Capacity (1);
            Add_Action (Table.States (90), 5, (45, 3), 99);
            Table.States (91).Action_List.Set_Capacity (1);
            Add_Action (Table.States (91), 33, (45, 2), 100);
            Table.States (92).Action_List.Set_Capacity (5);
            Add_Action (Table.States (92), (12, 23, 29, 33, 36), (46, 3),  3, null, null);
            Table.States (93).Action_List.Set_Capacity (10);
            Add_Action (Table.States (93), 12, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (93), 18, (53, 0), 39);
            Add_Action (Table.States (93), 19, (52, 0), 40);
            Add_Action (Table.States (93), 20, (51, 0), 41);
            Add_Action (Table.States (93), 21, (47, 0), 42);
            Add_Action (Table.States (93), 26, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (93), 27, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (93), 28, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (93), 33, (48, 1), 43);
            Add_Action (Table.States (93), 35, (50, 1), 44);
            Table.States (93).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (93), 47, 47);
            Add_Goto (Table.States (93), 48, 77);
            Add_Goto (Table.States (93), 50, 50);
            Add_Goto (Table.States (93), 51, 51);
            Add_Goto (Table.States (93), 52, 52);
            Add_Goto (Table.States (93), 53, 53);
            Table.States (94).Action_List.Set_Capacity (14);
            Add_Action (Table.States (94), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 1),  4, null,
            null);
            Table.States (95).Action_List.Set_Capacity (14);
            Add_Action (Table.States (95), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 2),  4, null,
            null);
            Table.States (96).Action_List.Set_Capacity (14);
            Add_Action (Table.States (96), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 1),  4, null,
            null);
            Table.States (97).Action_List.Set_Capacity (14);
            Add_Action (Table.States (97), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 3),  4, null,
            null);
            Table.States (98).Action_List.Set_Capacity (1);
            Add_Action (Table.States (98), 17, (47, 0), 101);
            Table.States (99).Action_List.Set_Capacity (5);
            Add_Action (Table.States (99), (12, 23, 29, 33, 36), (45, 3),  4, null, null);
            Table.States (100).Action_List.Set_Capacity (1);
            Add_Action (Table.States (100), 16, (45, 2), 102);
            Table.States (101).Action_List.Set_Capacity (14);
            Add_Action (Table.States (101), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (47, 0),  5,
            null, null);
            Table.States (102).Action_List.Set_Capacity (1);
            Add_Action (Table.States (102), 33, (45, 2), 103);
            Table.States (103).Action_List.Set_Capacity (5);
            Add_Action (Table.States (103), (12, 23, 29, 33, 36), (45, 2),  6, null, null);
         end Subr_1;
      begin
         Subr_1;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      WisiToken.Parse.LR.Parser_No_Recover.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_Main;
