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
         State_Last        => 111,
         First_Terminal    => 3,
         Last_Terminal     => 37,
         First_Nonterminal => 38,
         Last_Nonterminal  => 57);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 24, (39, 0), 1);
            Add_Action (Table.States (0), 34, (44, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 39, 3);
            Add_Goto (Table.States (0), 44, 4);
            Add_Goto (Table.States (0), 56, 5);
            Add_Goto (Table.States (0), 57, 6);
            Table.States (1).Action_List.Set_Capacity (8);
            Add_Action (Table.States (1), 3, (39, 1), 7);
            Add_Action (Table.States (1), 4, (39, 6), 8);
            Add_Action (Table.States (1), 5, (39, 5), 9);
            Add_Action (Table.States (1), 6, (39, 4), 10);
            Add_Action (Table.States (1), 7, (40, 0), 11);
            Add_Action (Table.States (1), 8, (40, 1), 12);
            Add_Action (Table.States (1), 9, (40, 2), 13);
            Add_Action (Table.States (1), 34, (39, 2), 14);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 40, 15);
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 14, (44, 0), 16);
            Add_Action (Table.States (2), 15, (44, 1), 17);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (24, 34, 37), (56, 0),  1, null, null);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (24, 34, 37), (56, 1),  1, null, null);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (24, 34, 37), (57, 0),  1, null, null);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 24, (39, 0), 1);
            Add_Action (Table.States (6), 34, (44, 0), 2);
            Add_Action (Table.States (6), 37, Accept_It, (38, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 39, 3);
            Add_Goto (Table.States (6), 44, 4);
            Add_Goto (Table.States (6), 56, 18);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 34, (41, 0), 19);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 41, 20);
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 6, (39, 6), 21);
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 34, (39, 5), 22);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 34, (39, 4), 23);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), (1 =>  34), (40, 0),  1, null, null);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 22, (40, 1), 24);
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 22, (40, 2), 25);
            Table.States (14).Action_List.Set_Capacity (14);
            Add_Action (Table.States (14), 9, (43, 11), 26);
            Add_Action (Table.States (14), 11, (43, 6), 27);
            Add_Action (Table.States (14), 13, (43, 0), 28);
            Add_Action (Table.States (14), 16, (43, 1), 29);
            Add_Action (Table.States (14), 17, (43, 3), 30);
            Add_Action (Table.States (14), 21, (43, 4), 31);
            Add_Action (Table.States (14), 24, Reduce, (39, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (14), 29, (43, 7), 32);
            Add_Action (Table.States (14), 31, (43, 8), 33);
            Add_Action (Table.States (14), 33, (43, 5), 34);
            Add_Action (Table.States (14), 34, (43, 2), 35);
            Add_Conflict (Table.States (14), 34, (39, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (14), 35, (43, 9), 36);
            Add_Action (Table.States (14), 36, (43, 10), 37);
            Add_Action (Table.States (14), 37, Reduce, (39, 3),  2, declaration_3'Access, null);
            Table.States (14).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (14), 42, 38);
            Add_Goto (Table.States (14), 43, 39);
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 34, (39, 0), 40);
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), 13, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (16), 19, (54, 0), 41);
            Add_Action (Table.States (16), 20, (53, 0), 42);
            Add_Action (Table.States (16), 21, (52, 0), 43);
            Add_Action (Table.States (16), 22, (48, 0), 44);
            Add_Action (Table.States (16), 24, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (16), 30, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (16), 34, (49, 1), 45);
            Add_Conflict (Table.States (16), 34, (47, 0),  0, null, null);
            Add_Action (Table.States (16), 36, (51, 1), 46);
            Add_Action (Table.States (16), 37, Reduce, (47, 0),  0, null, null);
            Table.States (16).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (16), 46, 47);
            Add_Goto (Table.States (16), 47, 48);
            Add_Goto (Table.States (16), 48, 49);
            Add_Goto (Table.States (16), 49, 50);
            Add_Goto (Table.States (16), 50, 51);
            Add_Goto (Table.States (16), 51, 52);
            Add_Goto (Table.States (16), 52, 53);
            Add_Goto (Table.States (16), 53, 54);
            Add_Goto (Table.States (16), 54, 55);
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), 13, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (17), 19, (54, 0), 41);
            Add_Action (Table.States (17), 20, (53, 0), 42);
            Add_Action (Table.States (17), 21, (52, 0), 43);
            Add_Action (Table.States (17), 22, (48, 0), 44);
            Add_Action (Table.States (17), 24, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (17), 30, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (17), 34, (49, 1), 45);
            Add_Conflict (Table.States (17), 34, (47, 0),  0, null, null);
            Add_Action (Table.States (17), 36, (51, 1), 46);
            Add_Action (Table.States (17), 37, Reduce, (47, 0),  0, null, null);
            Table.States (17).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (17), 46, 56);
            Add_Goto (Table.States (17), 47, 48);
            Add_Goto (Table.States (17), 48, 49);
            Add_Goto (Table.States (17), 49, 50);
            Add_Goto (Table.States (17), 50, 51);
            Add_Goto (Table.States (17), 51, 52);
            Add_Goto (Table.States (17), 52, 53);
            Add_Goto (Table.States (17), 53, 54);
            Add_Goto (Table.States (17), 54, 55);
            Table.States (18).Action_List.Set_Capacity (3);
            Add_Action (Table.States (18), (24, 34, 37), (57, 1),  2, null, null);
            Table.States (19).Action_List.Set_Capacity (2);
            Add_Action (Table.States (19), (10, 34), (41, 0),  1, null, null);
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), 10, (39, 1), 57);
            Add_Action (Table.States (20), 34, (41, 1), 58);
            Table.States (21).Action_List.Set_Capacity (3);
            Add_Action (Table.States (21), (24, 34, 37), (39, 6),  3, declaration_6'Access, null);
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 17, (39, 5), 59);
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 17, (39, 4), 60);
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 34, (40, 1), 61);
            Table.States (25).Action_List.Set_Capacity (1);
            Add_Action (Table.States (25), 34, (40, 2), 62);
            Table.States (26).Action_List.Set_Capacity (14);
            Add_Action (Table.States (26), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 11),  1, null,
            null);
            Table.States (27).Action_List.Set_Capacity (14);
            Add_Action (Table.States (27), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 6),  1, null,
            null);
            Table.States (28).Action_List.Set_Capacity (14);
            Add_Action (Table.States (28), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 0),  1, null,
            null);
            Table.States (29).Action_List.Set_Capacity (14);
            Add_Action (Table.States (29), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 1),  1, null,
            null);
            Table.States (30).Action_List.Set_Capacity (14);
            Add_Action (Table.States (30), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 3),  1, null,
            null);
            Table.States (31).Action_List.Set_Capacity (14);
            Add_Action (Table.States (31), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 4),  1, null,
            null);
            Table.States (32).Action_List.Set_Capacity (14);
            Add_Action (Table.States (32), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 7),  1, null,
            null);
            Table.States (33).Action_List.Set_Capacity (14);
            Add_Action (Table.States (33), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 8),  1, null,
            null);
            Table.States (34).Action_List.Set_Capacity (14);
            Add_Action (Table.States (34), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 5),  1, null,
            null);
            Table.States (35).Action_List.Set_Capacity (14);
            Add_Action (Table.States (35), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 2),  1, null,
            null);
            Table.States (36).Action_List.Set_Capacity (14);
            Add_Action (Table.States (36), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 9),  1, null,
            null);
            Table.States (37).Action_List.Set_Capacity (14);
            Add_Action (Table.States (37), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (43, 10),  1, null,
            null);
            Table.States (38).Action_List.Set_Capacity (14);
            Add_Action (Table.States (38), 9, (43, 11), 26);
            Add_Action (Table.States (38), 11, (43, 6), 27);
            Add_Action (Table.States (38), 13, (43, 0), 28);
            Add_Action (Table.States (38), 16, (43, 1), 29);
            Add_Action (Table.States (38), 17, (43, 3), 30);
            Add_Action (Table.States (38), 21, (43, 4), 31);
            Add_Action (Table.States (38), 24, Reduce, (39, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (38), 29, (43, 7), 32);
            Add_Action (Table.States (38), 31, (43, 8), 33);
            Add_Action (Table.States (38), 33, (43, 5), 34);
            Add_Action (Table.States (38), 34, (43, 2), 35);
            Add_Conflict (Table.States (38), 34, (39, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (38), 35, (43, 9), 36);
            Add_Action (Table.States (38), 36, (43, 10), 37);
            Add_Action (Table.States (38), 37, Reduce, (39, 2),  3, declaration_2'Access, null);
            Table.States (38).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (38), 43, 63);
            Table.States (39).Action_List.Set_Capacity (14);
            Add_Action (Table.States (39), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (42, 0),  1, null,
            null);
            Table.States (40).Action_List.Set_Capacity (12);
            Add_Action (Table.States (40), 9, (43, 11), 26);
            Add_Action (Table.States (40), 11, (43, 6), 27);
            Add_Action (Table.States (40), 13, (43, 0), 28);
            Add_Action (Table.States (40), 16, (43, 1), 29);
            Add_Action (Table.States (40), 17, (43, 3), 30);
            Add_Action (Table.States (40), 21, (43, 4), 31);
            Add_Action (Table.States (40), 29, (43, 7), 32);
            Add_Action (Table.States (40), 31, (43, 8), 33);
            Add_Action (Table.States (40), 33, (43, 5), 34);
            Add_Action (Table.States (40), 34, (43, 2), 35);
            Add_Action (Table.States (40), 35, (43, 9), 36);
            Add_Action (Table.States (40), 36, (43, 10), 37);
            Table.States (40).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (40), 42, 64);
            Add_Goto (Table.States (40), 43, 39);
            Table.States (41).Action_List.Set_Capacity (6);
            Add_Action (Table.States (41), 19, (54, 0), 41);
            Add_Action (Table.States (41), 20, (53, 0), 42);
            Add_Action (Table.States (41), 21, (52, 0), 43);
            Add_Action (Table.States (41), 22, (48, 0), 44);
            Add_Action (Table.States (41), 34, (49, 1), 45);
            Add_Action (Table.States (41), 36, (51, 1), 46);
            Table.States (41).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (41), 48, 49);
            Add_Goto (Table.States (41), 49, 50);
            Add_Goto (Table.States (41), 50, 65);
            Add_Goto (Table.States (41), 51, 52);
            Add_Goto (Table.States (41), 52, 53);
            Add_Goto (Table.States (41), 53, 54);
            Add_Goto (Table.States (41), 54, 55);
            Add_Goto (Table.States (41), 55, 66);
            Table.States (42).Action_List.Set_Capacity (6);
            Add_Action (Table.States (42), 19, (54, 0), 41);
            Add_Action (Table.States (42), 20, (53, 0), 42);
            Add_Action (Table.States (42), 21, (52, 0), 43);
            Add_Action (Table.States (42), 22, (48, 0), 44);
            Add_Action (Table.States (42), 34, (49, 1), 45);
            Add_Action (Table.States (42), 36, (51, 1), 46);
            Table.States (42).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (42), 48, 49);
            Add_Goto (Table.States (42), 49, 50);
            Add_Goto (Table.States (42), 50, 65);
            Add_Goto (Table.States (42), 51, 52);
            Add_Goto (Table.States (42), 52, 53);
            Add_Goto (Table.States (42), 53, 54);
            Add_Goto (Table.States (42), 54, 55);
            Add_Goto (Table.States (42), 55, 67);
            Table.States (43).Action_List.Set_Capacity (6);
            Add_Action (Table.States (43), 19, (54, 0), 41);
            Add_Action (Table.States (43), 20, (53, 0), 42);
            Add_Action (Table.States (43), 21, (52, 0), 43);
            Add_Action (Table.States (43), 22, (48, 0), 44);
            Add_Action (Table.States (43), 34, (49, 1), 45);
            Add_Action (Table.States (43), 36, (51, 1), 46);
            Table.States (43).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (43), 48, 49);
            Add_Goto (Table.States (43), 49, 50);
            Add_Goto (Table.States (43), 50, 65);
            Add_Goto (Table.States (43), 51, 52);
            Add_Goto (Table.States (43), 52, 53);
            Add_Goto (Table.States (43), 53, 54);
            Add_Goto (Table.States (43), 54, 55);
            Add_Goto (Table.States (43), 55, 68);
            Table.States (44).Action_List.Set_Capacity (1);
            Add_Action (Table.States (44), 34, (48, 0), 69);
            Table.States (45).Action_List.Set_Capacity (18);
            Add_Action (Table.States (45), 12, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 13, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 17, (49, 1), 70);
            Add_Action (Table.States (45), 19, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 20, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 21, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 22, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 24, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 25, (54, 4), 71);
            Add_Action (Table.States (45), 26, (53, 2), 72);
            Add_Action (Table.States (45), 27, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 28, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 29, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 30, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 32, (54, 5), 73);
            Add_Action (Table.States (45), 34, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 36, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (45), 37, Reduce, (51, 0),  1, null, null);
            Table.States (46).Action_List.Set_Capacity (15);
            Add_Action (Table.States (46), 12, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 13, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 19, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 20, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 21, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 22, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 24, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 26, (53, 3), 74);
            Add_Action (Table.States (46), 27, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 28, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 29, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 30, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 34, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 36, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (46), 37, Reduce, (51, 1),  1, rhs_item_1'Access, null);
            Table.States (47).Action_List.Set_Capacity (5);
            Add_Action (Table.States (47), 13, (46, 1), 75);
            Add_Action (Table.States (47), 24, (46, 2), 76);
            Add_Conflict (Table.States (47), 24, (45, 1),  0, null, null);
            Add_Action (Table.States (47), 30, (45, 0), 77);
            Add_Action (Table.States (47), 34, Reduce, (45, 1),  0, null, null);
            Add_Action (Table.States (47), 37, Reduce, (45, 1),  0, null, null);
            Table.States (47).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (47), 45, 78);
            Table.States (48).Action_List.Set_Capacity (5);
            Add_Action (Table.States (48), (13, 24, 30, 34, 37), (46, 0),  1, null, null);
            Table.States (49).Action_List.Set_Capacity (14);
            Add_Action (Table.States (49), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (51, 2),  1,
            rhs_item_2'Access, null);
            Table.States (50).Action_List.Set_Capacity (14);
            Add_Action (Table.States (50), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (50, 0),  1, null,
            null);
            Table.States (51).Action_List.Set_Capacity (11);
            Add_Action (Table.States (51), 12, (47, 2), 79);
            Add_Action (Table.States (51), 13, Reduce, (47, 1),  1, null, null);
            Add_Action (Table.States (51), 19, (54, 0), 41);
            Add_Action (Table.States (51), 20, (53, 0), 42);
            Add_Action (Table.States (51), 21, (52, 0), 43);
            Add_Action (Table.States (51), 22, (48, 0), 44);
            Add_Action (Table.States (51), 24, Reduce, (47, 1),  1, null, null);
            Add_Action (Table.States (51), 30, Reduce, (47, 1),  1, null, null);
            Add_Action (Table.States (51), 34, (49, 1), 45);
            Add_Conflict (Table.States (51), 34, (47, 1),  1, null, null);
            Add_Action (Table.States (51), 36, (51, 1), 46);
            Add_Action (Table.States (51), 37, Reduce, (47, 1),  1, null, null);
            Table.States (51).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (51), 48, 49);
            Add_Goto (Table.States (51), 49, 80);
            Add_Goto (Table.States (51), 51, 52);
            Add_Goto (Table.States (51), 52, 53);
            Add_Goto (Table.States (51), 53, 54);
            Add_Goto (Table.States (51), 54, 55);
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (52).Action_List.Set_Capacity (14);
            Add_Action (Table.States (52), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (49, 0),  1, null,
            null);
            Table.States (53).Action_List.Set_Capacity (14);
            Add_Action (Table.States (53), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (51, 5),  1,
            rhs_item_5'Access, null);
            Table.States (54).Action_List.Set_Capacity (14);
            Add_Action (Table.States (54), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (51, 3),  1,
            rhs_item_3'Access, null);
            Table.States (55).Action_List.Set_Capacity (14);
            Add_Action (Table.States (55), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (51, 4),  1,
            rhs_item_4'Access, null);
            Table.States (56).Action_List.Set_Capacity (5);
            Add_Action (Table.States (56), 13, (46, 1), 75);
            Add_Action (Table.States (56), 24, (46, 2), 76);
            Add_Conflict (Table.States (56), 24, (45, 1),  0, null, null);
            Add_Action (Table.States (56), 30, (45, 0), 77);
            Add_Action (Table.States (56), 34, Reduce, (45, 1),  0, null, null);
            Add_Action (Table.States (56), 37, Reduce, (45, 1),  0, null, null);
            Table.States (56).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (56), 45, 81);
            Table.States (57).Action_List.Set_Capacity (3);
            Add_Action (Table.States (57), (24, 34, 37), (39, 1),  4, declaration_1'Access, null);
            Table.States (58).Action_List.Set_Capacity (2);
            Add_Action (Table.States (58), (10, 34), (41, 1),  2, null, null);
            Table.States (59).Action_List.Set_Capacity (1);
            Add_Action (Table.States (59), 34, (39, 5), 82);
            Table.States (60).Action_List.Set_Capacity (1);
            Add_Action (Table.States (60), 34, (39, 4), 83);
            Table.States (61).Action_List.Set_Capacity (1);
            Add_Action (Table.States (61), 18, (40, 1), 84);
            Table.States (62).Action_List.Set_Capacity (1);
            Add_Action (Table.States (62), 18, (40, 2), 85);
            Table.States (63).Action_List.Set_Capacity (14);
            Add_Action (Table.States (63), (9, 11, 13, 16, 17, 21, 24, 29, 31, 33, 34, 35, 36, 37), (42, 1),  2, null,
            null);
            Table.States (64).Action_List.Set_Capacity (14);
            Add_Action (Table.States (64), 9, (43, 11), 26);
            Add_Action (Table.States (64), 11, (43, 6), 27);
            Add_Action (Table.States (64), 13, (43, 0), 28);
            Add_Action (Table.States (64), 16, (43, 1), 29);
            Add_Action (Table.States (64), 17, (43, 3), 30);
            Add_Action (Table.States (64), 21, (43, 4), 31);
            Add_Action (Table.States (64), 24, Reduce, (39, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (64), 29, (43, 7), 32);
            Add_Action (Table.States (64), 31, (43, 8), 33);
            Add_Action (Table.States (64), 33, (43, 5), 34);
            Add_Action (Table.States (64), 34, (43, 2), 35);
            Add_Conflict (Table.States (64), 34, (39, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (64), 35, (43, 9), 36);
            Add_Action (Table.States (64), 36, (43, 10), 37);
            Add_Action (Table.States (64), 37, Reduce, (39, 0),  4, declaration_0'Access, null);
            Table.States (64).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (64), 43, 63);
            Table.States (65).Action_List.Set_Capacity (10);
            Add_Action (Table.States (65), 13, Reduce, (55, 0),  1, null, null);
            Add_Action (Table.States (65), 19, (54, 0), 41);
            Add_Action (Table.States (65), 20, (53, 0), 42);
            Add_Action (Table.States (65), 21, (52, 0), 43);
            Add_Action (Table.States (65), 22, (48, 0), 44);
            Add_Action (Table.States (65), 27, Reduce, (55, 0),  1, null, null);
            Add_Action (Table.States (65), 28, Reduce, (55, 0),  1, null, null);
            Add_Action (Table.States (65), 29, Reduce, (55, 0),  1, null, null);
            Add_Action (Table.States (65), 34, (49, 1), 45);
            Add_Action (Table.States (65), 36, (51, 1), 46);
            Table.States (65).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (65), 48, 49);
            Add_Goto (Table.States (65), 49, 80);
            Add_Goto (Table.States (65), 51, 52);
            Add_Goto (Table.States (65), 52, 53);
            Add_Goto (Table.States (65), 53, 54);
            Add_Goto (Table.States (65), 54, 55);
            Table.States (66).Action_List.Set_Capacity (2);
            Add_Action (Table.States (66), 13, (55, 1), 86);
            Add_Action (Table.States (66), 27, (54, 0), 87);
            Table.States (67).Action_List.Set_Capacity (2);
            Add_Action (Table.States (67), 13, (55, 1), 86);
            Add_Action (Table.States (67), 28, (53, 0), 88);
            Table.States (68).Action_List.Set_Capacity (2);
            Add_Action (Table.States (68), 13, (55, 1), 86);
            Add_Action (Table.States (68), 29, (52, 0), 89);
            Table.States (69).Action_List.Set_Capacity (1);
            Add_Action (Table.States (69), 17, (48, 0), 90);
            Table.States (70).Action_List.Set_Capacity (6);
            Add_Action (Table.States (70), 19, (54, 0), 41);
            Add_Action (Table.States (70), 20, (53, 0), 42);
            Add_Action (Table.States (70), 21, (52, 0), 43);
            Add_Action (Table.States (70), 22, (48, 0), 44);
            Add_Action (Table.States (70), 34, (51, 0), 91);
            Add_Action (Table.States (70), 36, (51, 1), 46);
            Table.States (70).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (70), 48, 49);
            Add_Goto (Table.States (70), 51, 92);
            Add_Goto (Table.States (70), 52, 53);
            Add_Goto (Table.States (70), 53, 54);
            Add_Goto (Table.States (70), 54, 55);
            Table.States (71).Action_List.Set_Capacity (14);
            Add_Action (Table.States (71), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (54, 4),  2, null,
            null);
            Table.States (72).Action_List.Set_Capacity (14);
            Add_Action (Table.States (72), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (53, 2),  2, null,
            null);
            Table.States (73).Action_List.Set_Capacity (14);
            Add_Action (Table.States (73), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (54, 5),  2, null,
            null);
            Table.States (74).Action_List.Set_Capacity (14);
            Add_Action (Table.States (74), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (53, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (75).Action_List.Set_Capacity (10);
            Add_Action (Table.States (75), 13, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (75), 19, (54, 0), 41);
            Add_Action (Table.States (75), 20, (53, 0), 42);
            Add_Action (Table.States (75), 21, (52, 0), 43);
            Add_Action (Table.States (75), 22, (48, 0), 44);
            Add_Action (Table.States (75), 24, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (75), 30, Reduce, (47, 0),  0, null, null);
            Add_Action (Table.States (75), 34, (49, 1), 45);
            Add_Conflict (Table.States (75), 34, (47, 0),  0, null, null);
            Add_Action (Table.States (75), 36, (51, 1), 46);
            Add_Action (Table.States (75), 37, Reduce, (47, 0),  0, null, null);
            Table.States (75).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (75), 47, 93);
            Add_Goto (Table.States (75), 48, 49);
            Add_Goto (Table.States (75), 49, 50);
            Add_Goto (Table.States (75), 50, 51);
            Add_Goto (Table.States (75), 51, 52);
            Add_Goto (Table.States (75), 52, 53);
            Add_Goto (Table.States (75), 53, 54);
            Add_Goto (Table.States (75), 54, 55);
            Table.States (76).Action_List.Set_Capacity (3);
            Add_Action (Table.States (76), 4, (46, 4), 94);
            Add_Action (Table.States (76), 5, (46, 3), 95);
            Add_Action (Table.States (76), 6, (46, 2), 96);
            Table.States (77).Action_List.Set_Capacity (3);
            Add_Action (Table.States (77), (24, 34, 37), (45, 0),  1, null, null);
            Table.States (78).Action_List.Set_Capacity (3);
            Add_Action (Table.States (78), (24, 34, 37), (44, 0),  4, nonterminal_0'Access, null);
            Table.States (79).Action_List.Set_Capacity (6);
            Add_Action (Table.States (79), 12, (47, 3), 97);
            Add_Action (Table.States (79), 13, Reduce, (47, 2),  2, null, null);
            Add_Action (Table.States (79), 24, Reduce, (47, 2),  2, null, null);
            Add_Action (Table.States (79), 30, Reduce, (47, 2),  2, null, null);
            Add_Action (Table.States (79), 34, Reduce, (47, 2),  2, null, null);
            Add_Action (Table.States (79), 37, Reduce, (47, 2),  2, null, null);
            Table.States (80).Action_List.Set_Capacity (14);
            Add_Action (Table.States (80), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (50, 1),  2, null,
            null);
            Table.States (81).Action_List.Set_Capacity (3);
            Add_Action (Table.States (81), (24, 34, 37), (44, 1),  4, nonterminal_1'Access, null);
            Table.States (82).Action_List.Set_Capacity (3);
            Add_Action (Table.States (82), (24, 34, 37), (39, 5),  5, declaration_5'Access, null);
            Table.States (83).Action_List.Set_Capacity (3);
            Add_Action (Table.States (83), (24, 34, 37), (39, 4),  5, declaration_4'Access, null);
            Table.States (84).Action_List.Set_Capacity (1);
            Add_Action (Table.States (84), (1 =>  34), (40, 1),  4, null, null);
            Table.States (85).Action_List.Set_Capacity (1);
            Add_Action (Table.States (85), (1 =>  34), (40, 2),  4, null, null);
            Table.States (86).Action_List.Set_Capacity (6);
            Add_Action (Table.States (86), 19, (54, 0), 41);
            Add_Action (Table.States (86), 20, (53, 0), 42);
            Add_Action (Table.States (86), 21, (52, 0), 43);
            Add_Action (Table.States (86), 22, (48, 0), 44);
            Add_Action (Table.States (86), 34, (49, 1), 45);
            Add_Action (Table.States (86), 36, (51, 1), 46);
            Table.States (86).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (86), 48, 49);
            Add_Goto (Table.States (86), 49, 50);
            Add_Goto (Table.States (86), 50, 98);
            Add_Goto (Table.States (86), 51, 52);
            Add_Goto (Table.States (86), 52, 53);
            Add_Goto (Table.States (86), 53, 54);
            Add_Goto (Table.States (86), 54, 55);
            Table.States (87).Action_List.Set_Capacity (15);
            Add_Action (Table.States (87), 12, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 13, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 19, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 20, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 21, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 22, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 23, (54, 1), 99);
            Add_Action (Table.States (87), 24, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 27, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 28, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 29, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 30, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 34, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 36, Reduce, (54, 0),  3, null, null);
            Add_Action (Table.States (87), 37, Reduce, (54, 0),  3, null, null);
            Table.States (88).Action_List.Set_Capacity (14);
            Add_Action (Table.States (88), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (53, 0),  3, null,
            null);
            Table.States (89).Action_List.Set_Capacity (17);
            Add_Action (Table.States (89), 12, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 13, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 19, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 20, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 21, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 22, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 24, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 25, (54, 2), 100);
            Add_Action (Table.States (89), 26, (53, 1), 101);
            Add_Action (Table.States (89), 27, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 28, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 29, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 30, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 32, (54, 3), 102);
            Add_Action (Table.States (89), 34, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 36, Reduce, (52, 0),  3, null, null);
            Add_Action (Table.States (89), 37, Reduce, (52, 0),  3, null, null);
            Table.States (90).Action_List.Set_Capacity (1);
            Add_Action (Table.States (90), 34, (48, 0), 103);
            Table.States (91).Action_List.Set_Capacity (17);
            Add_Action (Table.States (91), 12, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 13, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 19, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 20, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 21, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 22, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 24, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 25, (54, 4), 71);
            Add_Action (Table.States (91), 26, (53, 2), 72);
            Add_Action (Table.States (91), 27, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 28, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 29, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 30, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 32, (54, 5), 73);
            Add_Action (Table.States (91), 34, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 36, Reduce, (51, 0),  1, null, null);
            Add_Action (Table.States (91), 37, Reduce, (51, 0),  1, null, null);
            Table.States (92).Action_List.Set_Capacity (14);
            Add_Action (Table.States (92), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (49, 1),  3, null,
            null);
            Table.States (93).Action_List.Set_Capacity (5);
            Add_Action (Table.States (93), (13, 24, 30, 34, 37), (46, 1),  3, null, null);
            Table.States (94).Action_List.Set_Capacity (1);
            Add_Action (Table.States (94), 6, (46, 4), 104);
            Table.States (95).Action_List.Set_Capacity (1);
            Add_Action (Table.States (95), 34, (46, 3), 105);
            Table.States (96).Action_List.Set_Capacity (1);
            Add_Action (Table.States (96), 34, (46, 2), 106);
            Table.States (97).Action_List.Set_Capacity (5);
            Add_Action (Table.States (97), (13, 24, 30, 34, 37), (47, 3),  3, null, null);
            Table.States (98).Action_List.Set_Capacity (10);
            Add_Action (Table.States (98), 13, Reduce, (55, 1),  3, null, null);
            Add_Action (Table.States (98), 19, (54, 0), 41);
            Add_Action (Table.States (98), 20, (53, 0), 42);
            Add_Action (Table.States (98), 21, (52, 0), 43);
            Add_Action (Table.States (98), 22, (48, 0), 44);
            Add_Action (Table.States (98), 27, Reduce, (55, 1),  3, null, null);
            Add_Action (Table.States (98), 28, Reduce, (55, 1),  3, null, null);
            Add_Action (Table.States (98), 29, Reduce, (55, 1),  3, null, null);
            Add_Action (Table.States (98), 34, (49, 1), 45);
            Add_Action (Table.States (98), 36, (51, 1), 46);
            Table.States (98).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (98), 48, 49);
            Add_Goto (Table.States (98), 49, 80);
            Add_Goto (Table.States (98), 51, 52);
            Add_Goto (Table.States (98), 52, 53);
            Add_Goto (Table.States (98), 53, 54);
            Add_Goto (Table.States (98), 54, 55);
            Table.States (99).Action_List.Set_Capacity (14);
            Add_Action (Table.States (99), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (54, 1),  4, null,
            null);
            Table.States (100).Action_List.Set_Capacity (14);
            Add_Action (Table.States (100), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (54, 2),  4,
            null, null);
            Table.States (101).Action_List.Set_Capacity (14);
            Add_Action (Table.States (101), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (53, 1),  4,
            null, null);
            Table.States (102).Action_List.Set_Capacity (14);
            Add_Action (Table.States (102), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (54, 3),  4,
            null, null);
            Table.States (103).Action_List.Set_Capacity (1);
            Add_Action (Table.States (103), 18, (48, 0), 107);
            Table.States (104).Action_List.Set_Capacity (5);
            Add_Action (Table.States (104), (13, 24, 30, 34, 37), (46, 4),  4, null, null);
            Table.States (105).Action_List.Set_Capacity (1);
            Add_Action (Table.States (105), 17, (46, 3), 108);
            Table.States (106).Action_List.Set_Capacity (1);
            Add_Action (Table.States (106), 17, (46, 2), 109);
            Table.States (107).Action_List.Set_Capacity (14);
            Add_Action (Table.States (107), (12, 13, 19, 20, 21, 22, 24, 27, 28, 29, 30, 34, 36, 37), (48, 0),  5,
            null, null);
            Table.States (108).Action_List.Set_Capacity (1);
            Add_Action (Table.States (108), 34, (46, 3), 110);
            Table.States (109).Action_List.Set_Capacity (1);
            Add_Action (Table.States (109), 34, (46, 2), 111);
            Table.States (110).Action_List.Set_Capacity (5);
            Add_Action (Table.States (110), (13, 24, 30, 34, 37), (46, 3),  6, null, null);
            Table.States (111).Action_List.Set_Capacity (5);
            Add_Action (Table.States (111), (13, 24, 30, 34, 37), (46, 2),  6, null, null);
         end Subr_2;
      begin
         Subr_1;
         Subr_2;
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
