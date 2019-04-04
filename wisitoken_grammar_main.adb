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
     (Parser                       :    out WisiToken.Parse.LR.Parser_No_Recover.Parser;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 94,
         First_Terminal    => 3,
         Last_Terminal     => 35,
         First_Nonterminal => 36,
         Last_Nonterminal  => 55);
   begin
      declare
         procedure Subr_1
         is begin
            Add_Action (Table.States (0), 22, 1);
            Add_Action (Table.States (0), 32, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 37, 3);
            Add_Goto (Table.States (0), 43, 4);
            Add_Goto (Table.States (0), 54, 5);
            Add_Goto (Table.States (0), 55, 6);
            Add_Action (Table.States (1), 3, 7);
            Add_Action (Table.States (1), 4, 8);
            Add_Action (Table.States (1), 5, 9);
            Add_Action (Table.States (1), 6, 10);
            Add_Action (Table.States (1), 7, 11);
            Add_Action (Table.States (1), 8, 12);
            Add_Action (Table.States (1), 32, 13);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 38, 14);
            Add_Action (Table.States (2), 13, 15);
            Add_Error (Table.States (2));
            Add_Action (Table.States (3), (22, 32, 35), (54, 0), 1, null, null);
            Add_Action (Table.States (4), (22, 32, 35), (54, 1), 1, null, null);
            Add_Action (Table.States (5), (22, 32, 35), (55, 0), 1, null, null);
            Add_Action (Table.States (6), 22, 1);
            Add_Action (Table.States (6), 32, 2);
            Add_Action (Table.States (6), 35, Accept_It, (36, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 37, 3);
            Add_Goto (Table.States (6), 43, 4);
            Add_Goto (Table.States (6), 54, 16);
            Add_Action (Table.States (7), 32, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 39, 18);
            Add_Action (Table.States (8), 5, 19);
            Add_Error (Table.States (8));
            Add_Action (Table.States (9), 32, 20);
            Add_Error (Table.States (9));
            Add_Action (Table.States (10), (1 =>  32), (38, 0), 1, null, null);
            Add_Action (Table.States (11), 20, 21);
            Add_Error (Table.States (11));
            Add_Action (Table.States (12), 20, 22);
            Add_Error (Table.States (12));
            Add_Action (Table.States (13), 8, 23);
            Add_Action (Table.States (13), 10, 24);
            Add_Action (Table.States (13), 14, 25);
            Add_Action (Table.States (13), 15, 26);
            Add_Action (Table.States (13), 19, 27);
            Add_Action (Table.States (13), 22, Reduce, (37, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 27, 28);
            Add_Action (Table.States (13), 29, 29);
            Add_Action (Table.States (13), 31, 30);
            Add_Action (Table.States (13), 32, 31, (37, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 33, 32);
            Add_Action (Table.States (13), 34, 33);
            Add_Action (Table.States (13), 35, Reduce, (37, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (13));
            Add_Goto (Table.States (13), 40, 34);
            Add_Goto (Table.States (13), 41, 35);
            Add_Action (Table.States (14), 32, 36);
            Add_Error (Table.States (14));
            Add_Action (Table.States (15), 12, Reduce, (46, 0), 0, null, null);
            Add_Action (Table.States (15), 17, 37);
            Add_Action (Table.States (15), 18, 38);
            Add_Action (Table.States (15), 19, 39);
            Add_Action (Table.States (15), 20, 40);
            Add_Action (Table.States (15), 22, 41, (46, 0), 0, null, null);
            Add_Action (Table.States (15), 28, Reduce, (46, 0), 0, null, null);
            Add_Action (Table.States (15), 32, 42, (46, 0), 0, null, null);
            Add_Action (Table.States (15), 34, 43);
            Add_Action (Table.States (15), 35, Reduce, (46, 0), 0, null, null);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 42, 44);
            Add_Goto (Table.States (15), 45, 45);
            Add_Goto (Table.States (15), 46, 46);
            Add_Goto (Table.States (15), 47, 47);
            Add_Goto (Table.States (15), 48, 48);
            Add_Goto (Table.States (15), 49, 49);
            Add_Goto (Table.States (15), 50, 50);
            Add_Goto (Table.States (15), 51, 51);
            Add_Goto (Table.States (15), 52, 52);
            Add_Action (Table.States (16), (22, 32, 35), (55, 1), 2, null, null);
            Add_Action (Table.States (17), (9, 32), (39, 0), 1, null, null);
            Add_Action (Table.States (18), 9, 53);
            Add_Action (Table.States (18), 32, 54);
            Add_Error (Table.States (18));
            Add_Action (Table.States (19), (22, 32, 35), (37, 5), 3, declaration_5'Access, null);
            Add_Action (Table.States (20), 15, 55);
            Add_Error (Table.States (20));
            Add_Action (Table.States (21), 32, 56);
            Add_Error (Table.States (21));
            Add_Action (Table.States (22), 32, 57);
            Add_Error (Table.States (22));
            Add_Action (Table.States (23), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 10), 1, null,
            null);
            Add_Action (Table.States (24), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 5), 1, null,
            null);
            Add_Action (Table.States (25), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 0), 1, null,
            null);
            Add_Action (Table.States (26), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 2), 1, null,
            null);
            Add_Action (Table.States (27), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 3), 1, null,
            null);
            Add_Action (Table.States (28), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 6), 1, null,
            null);
            Add_Action (Table.States (29), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 7), 1, null,
            null);
            Add_Action (Table.States (30), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 4), 1, null,
            null);
            Add_Action (Table.States (31), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 1), 1, null,
            null);
            Add_Action (Table.States (32), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 8), 1, null,
            null);
            Add_Action (Table.States (33), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (41, 9), 1, null,
            null);
            Add_Action (Table.States (34), 8, 23);
            Add_Action (Table.States (34), 10, 24);
            Add_Action (Table.States (34), 14, 25);
            Add_Action (Table.States (34), 15, 26);
            Add_Action (Table.States (34), 19, 27);
            Add_Action (Table.States (34), 22, Reduce, (37, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (34), 27, 28);
            Add_Action (Table.States (34), 29, 29);
            Add_Action (Table.States (34), 31, 30);
            Add_Action (Table.States (34), 32, 31, (37, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (34), 33, 32);
            Add_Action (Table.States (34), 34, 33);
            Add_Action (Table.States (34), 35, Reduce, (37, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (34));
            Add_Goto (Table.States (34), 41, 58);
            Add_Action (Table.States (35), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (40, 0), 1, null,
            null);
            Add_Action (Table.States (36), 8, 23);
            Add_Action (Table.States (36), 10, 24);
            Add_Action (Table.States (36), 14, 25);
            Add_Action (Table.States (36), 15, 26);
            Add_Action (Table.States (36), 19, 27);
            Add_Action (Table.States (36), 27, 28);
            Add_Action (Table.States (36), 29, 29);
            Add_Action (Table.States (36), 31, 30);
            Add_Action (Table.States (36), 32, 31);
            Add_Action (Table.States (36), 33, 32);
            Add_Action (Table.States (36), 34, 33);
            Add_Error (Table.States (36));
            Add_Goto (Table.States (36), 40, 59);
            Add_Goto (Table.States (36), 41, 35);
            Add_Action (Table.States (37), 17, 37);
            Add_Action (Table.States (37), 18, 38);
            Add_Action (Table.States (37), 19, 39);
            Add_Action (Table.States (37), 20, 40);
            Add_Action (Table.States (37), 32, 42);
            Add_Action (Table.States (37), 34, 43);
            Add_Error (Table.States (37));
            Add_Goto (Table.States (37), 42, 44);
            Add_Goto (Table.States (37), 47, 47);
            Add_Goto (Table.States (37), 48, 60);
            Add_Goto (Table.States (37), 49, 49);
            Add_Goto (Table.States (37), 50, 50);
            Add_Goto (Table.States (37), 51, 51);
            Add_Goto (Table.States (37), 52, 52);
            Add_Goto (Table.States (37), 53, 61);
            Add_Action (Table.States (38), 17, 37);
            Add_Action (Table.States (38), 18, 38);
            Add_Action (Table.States (38), 19, 39);
            Add_Action (Table.States (38), 20, 40);
            Add_Action (Table.States (38), 32, 42);
            Add_Action (Table.States (38), 34, 43);
            Add_Error (Table.States (38));
            Add_Goto (Table.States (38), 42, 44);
            Add_Goto (Table.States (38), 47, 47);
            Add_Goto (Table.States (38), 48, 60);
            Add_Goto (Table.States (38), 49, 49);
            Add_Goto (Table.States (38), 50, 50);
            Add_Goto (Table.States (38), 51, 51);
            Add_Goto (Table.States (38), 52, 52);
            Add_Goto (Table.States (38), 53, 62);
            Add_Action (Table.States (39), 17, 37);
            Add_Action (Table.States (39), 18, 38);
            Add_Action (Table.States (39), 19, 39);
            Add_Action (Table.States (39), 20, 40);
            Add_Action (Table.States (39), 32, 42);
            Add_Action (Table.States (39), 34, 43);
            Add_Error (Table.States (39));
            Add_Goto (Table.States (39), 42, 44);
            Add_Goto (Table.States (39), 47, 47);
            Add_Goto (Table.States (39), 48, 60);
            Add_Goto (Table.States (39), 49, 49);
            Add_Goto (Table.States (39), 50, 50);
            Add_Goto (Table.States (39), 51, 51);
            Add_Goto (Table.States (39), 52, 52);
            Add_Goto (Table.States (39), 53, 63);
            Add_Action (Table.States (40), 32, 64);
            Add_Error (Table.States (40));
            Add_Action (Table.States (41), 4, 65);
            Add_Action (Table.States (41), 5, 66);
            Add_Error (Table.States (41));
            Add_Action (Table.States (42), 11, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 12, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 15, 67);
            Add_Action (Table.States (42), 17, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 18, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 19, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 20, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 22, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 25, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 26, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 27, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 28, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 32, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 34, Reduce, (49, 2), 1, null, null);
            Add_Action (Table.States (42), 35, Reduce, (49, 2), 1, null, null);
            Add_Error (Table.States (42));
            Add_Action (Table.States (43), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 3), 1,
            rhs_item_3'Access, null);
            Add_Action (Table.States (44), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 0), 1,
            rhs_item_0'Access, null);
            Add_Action (Table.States (45), 12, 68);
            Add_Action (Table.States (45), 22, Reduce, (44, 1), 0, null, null);
            Add_Action (Table.States (45), 28, 69);
            Add_Action (Table.States (45), 32, Reduce, (44, 1), 0, null, null);
            Add_Action (Table.States (45), 35, Reduce, (44, 1), 0, null, null);
            Add_Error (Table.States (45));
            Add_Goto (Table.States (45), 44, 70);
            Add_Action (Table.States (46), (12, 22, 28, 32, 35), (45, 0), 1, null, null);
            Add_Action (Table.States (47), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 1), 1,
            rhs_item_1'Access, null);
            Add_Action (Table.States (48), 11, 71);
            Add_Action (Table.States (48), 12, Reduce, (46, 1), 1, null, null);
            Add_Action (Table.States (48), 17, 37);
            Add_Action (Table.States (48), 18, 38);
            Add_Action (Table.States (48), 19, 39);
            Add_Action (Table.States (48), 20, 40);
            Add_Action (Table.States (48), 22, Reduce, (46, 1), 1, null, null);
            Add_Action (Table.States (48), 28, Reduce, (46, 1), 1, null, null);
            Add_Action (Table.States (48), 32, 42, (46, 1), 1, null, null);
            Add_Action (Table.States (48), 34, 43);
            Add_Action (Table.States (48), 35, Reduce, (46, 1), 1, null, null);
            Add_Error (Table.States (48));
            Add_Goto (Table.States (48), 42, 44);
            Add_Goto (Table.States (48), 47, 47);
            Add_Goto (Table.States (48), 49, 72);
            Add_Goto (Table.States (48), 50, 50);
            Add_Goto (Table.States (48), 51, 51);
            Add_Goto (Table.States (48), 52, 52);
            Add_Action (Table.States (49), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (48, 0), 1, null,
            null);
            Add_Action (Table.States (50), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 6), 1,
            rhs_item_6'Access, null);
            Add_Action (Table.States (51), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 4), 1,
            rhs_item_4'Access, null);
            Add_Action (Table.States (52), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (49, 5), 1,
            rhs_item_5'Access, null);
            Add_Action (Table.States (53), (22, 32, 35), (37, 1), 4, declaration_1'Access, null);
            Add_Action (Table.States (54), (9, 32), (39, 1), 2, null, null);
            Add_Action (Table.States (55), 32, 73);
            Add_Error (Table.States (55));
            Add_Action (Table.States (56), 16, 74);
            Add_Error (Table.States (56));
            Add_Action (Table.States (57), 16, 75);
            Add_Error (Table.States (57));
            Add_Action (Table.States (58), (8, 10, 14, 15, 19, 22, 27, 29, 31, 32, 33, 34, 35), (40, 1), 2, null,
            null);
            Add_Action (Table.States (59), 8, 23);
            Add_Action (Table.States (59), 10, 24);
            Add_Action (Table.States (59), 14, 25);
            Add_Action (Table.States (59), 15, 26);
            Add_Action (Table.States (59), 19, 27);
            Add_Action (Table.States (59), 22, Reduce, (37, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (59), 27, 28);
            Add_Action (Table.States (59), 29, 29);
            Add_Action (Table.States (59), 31, 30);
            Add_Action (Table.States (59), 32, 31, (37, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (59), 33, 32);
            Add_Action (Table.States (59), 34, 33);
            Add_Action (Table.States (59), 35, Reduce, (37, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (59));
            Add_Goto (Table.States (59), 41, 58);
            Add_Action (Table.States (60), 12, Reduce, (53, 0), 1, null, null);
            Add_Action (Table.States (60), 17, 37);
            Add_Action (Table.States (60), 18, 38);
            Add_Action (Table.States (60), 19, 39);
            Add_Action (Table.States (60), 20, 40);
            Add_Action (Table.States (60), 25, Reduce, (53, 0), 1, null, null);
            Add_Action (Table.States (60), 26, Reduce, (53, 0), 1, null, null);
            Add_Action (Table.States (60), 27, Reduce, (53, 0), 1, null, null);
            Add_Action (Table.States (60), 32, 42);
            Add_Action (Table.States (60), 34, 43);
            Add_Error (Table.States (60));
            Add_Goto (Table.States (60), 42, 44);
            Add_Goto (Table.States (60), 47, 47);
            Add_Goto (Table.States (60), 49, 72);
            Add_Goto (Table.States (60), 50, 50);
            Add_Goto (Table.States (60), 51, 51);
            Add_Goto (Table.States (60), 52, 52);
            Add_Action (Table.States (61), 12, 76);
            Add_Action (Table.States (61), 25, 77);
            Add_Error (Table.States (61));
            Add_Action (Table.States (62), 12, 76);
            Add_Action (Table.States (62), 26, 78);
            Add_Error (Table.States (62));
            Add_Action (Table.States (63), 12, 76);
            Add_Action (Table.States (63), 27, 79);
            Add_Error (Table.States (63));
            Add_Action (Table.States (64), 15, 80);
            Add_Error (Table.States (64));
            Add_Action (Table.States (65), 5, 81);
            Add_Error (Table.States (65));
            Add_Action (Table.States (66), 32, 82);
            Add_Error (Table.States (66));
            Add_Action (Table.States (67), 32, 83);
            Add_Error (Table.States (67));
            Add_Action (Table.States (68), 12, Reduce, (46, 0), 0, null, null);
            Add_Action (Table.States (68), 17, 37);
            Add_Action (Table.States (68), 18, 38);
            Add_Action (Table.States (68), 19, 39);
            Add_Action (Table.States (68), 20, 40);
            Add_Action (Table.States (68), 22, Reduce, (46, 0), 0, null, null);
            Add_Action (Table.States (68), 28, Reduce, (46, 0), 0, null, null);
            Add_Action (Table.States (68), 32, 42, (46, 0), 0, null, null);
            Add_Action (Table.States (68), 34, 43);
            Add_Action (Table.States (68), 35, Reduce, (46, 0), 0, null, null);
            Add_Error (Table.States (68));
            Add_Goto (Table.States (68), 42, 44);
            Add_Goto (Table.States (68), 46, 84);
            Add_Goto (Table.States (68), 47, 47);
            Add_Goto (Table.States (68), 48, 48);
            Add_Goto (Table.States (68), 49, 49);
            Add_Goto (Table.States (68), 50, 50);
            Add_Goto (Table.States (68), 51, 51);
            Add_Goto (Table.States (68), 52, 52);
            Add_Action (Table.States (69), (22, 32, 35), (44, 0), 1, null, null);
            Add_Action (Table.States (70), (22, 32, 35), (43, 0), 4, nonterminal_0'Access, null);
            Add_Action (Table.States (71), 11, 85);
            Add_Action (Table.States (71), 12, Reduce, (46, 2), 2, null, null);
            Add_Action (Table.States (71), 22, Reduce, (46, 2), 2, null, null);
            Add_Action (Table.States (71), 28, Reduce, (46, 2), 2, null, null);
            Add_Action (Table.States (71), 32, Reduce, (46, 2), 2, null, null);
            Add_Action (Table.States (71), 35, Reduce, (46, 2), 2, null, null);
            Add_Error (Table.States (71));
            Add_Action (Table.States (72), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (48, 1), 2, null,
            null);
            Add_Action (Table.States (73), (22, 32, 35), (37, 4), 5, declaration_4'Access, null);
            Add_Action (Table.States (74), (1 =>  32), (38, 1), 4, null, null);
            Add_Action (Table.States (75), (1 =>  32), (38, 2), 4, null, null);
            Add_Action (Table.States (76), 17, 37);
            Add_Action (Table.States (76), 18, 38);
            Add_Action (Table.States (76), 19, 39);
            Add_Action (Table.States (76), 20, 40);
            Add_Action (Table.States (76), 32, 42);
            Add_Action (Table.States (76), 34, 43);
            Add_Error (Table.States (76));
            Add_Goto (Table.States (76), 42, 44);
            Add_Goto (Table.States (76), 47, 47);
            Add_Goto (Table.States (76), 48, 86);
            Add_Goto (Table.States (76), 49, 49);
            Add_Goto (Table.States (76), 50, 50);
            Add_Goto (Table.States (76), 51, 51);
            Add_Goto (Table.States (76), 52, 52);
            Add_Action (Table.States (77), 11, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 12, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 17, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 18, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 19, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 20, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 21, 87);
            Add_Action (Table.States (77), 22, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 25, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 26, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 27, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 28, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 32, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 34, Reduce, (52, 0), 3, null, null);
            Add_Action (Table.States (77), 35, Reduce, (52, 0), 3, null, null);
            Add_Error (Table.States (77));
            Add_Action (Table.States (78), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (51, 0), 3, null,
            null);
            Add_Action (Table.States (79), 11, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 12, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 17, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 18, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 19, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 20, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 22, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 23, 88);
            Add_Action (Table.States (79), 24, 89);
            Add_Action (Table.States (79), 25, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 26, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 27, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 28, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 30, 90);
            Add_Action (Table.States (79), 32, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 34, Reduce, (50, 0), 3, null, null);
            Add_Action (Table.States (79), 35, Reduce, (50, 0), 3, null, null);
            Add_Error (Table.States (79));
            Add_Action (Table.States (80), 32, 91);
            Add_Error (Table.States (80));
            Add_Action (Table.States (81), (12, 22, 28, 32, 35), (45, 3), 3, null, null);
            Add_Action (Table.States (82), 15, 92);
            Add_Error (Table.States (82));
            Add_Action (Table.States (83), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (42, 0), 3, null,
            null);
            Add_Action (Table.States (84), (12, 22, 28, 32, 35), (45, 1), 3, null, null);
            Add_Action (Table.States (85), (12, 22, 28, 32, 35), (46, 3), 3, null, null);
            Add_Action (Table.States (86), 12, Reduce, (53, 1), 3, null, null);
            Add_Action (Table.States (86), 17, 37);
            Add_Action (Table.States (86), 18, 38);
            Add_Action (Table.States (86), 19, 39);
            Add_Action (Table.States (86), 20, 40);
            Add_Action (Table.States (86), 25, Reduce, (53, 1), 3, null, null);
            Add_Action (Table.States (86), 26, Reduce, (53, 1), 3, null, null);
            Add_Action (Table.States (86), 27, Reduce, (53, 1), 3, null, null);
            Add_Action (Table.States (86), 32, 42);
            Add_Action (Table.States (86), 34, 43);
            Add_Error (Table.States (86));
            Add_Goto (Table.States (86), 42, 44);
            Add_Goto (Table.States (86), 47, 47);
            Add_Goto (Table.States (86), 49, 72);
            Add_Goto (Table.States (86), 50, 50);
            Add_Goto (Table.States (86), 51, 51);
            Add_Goto (Table.States (86), 52, 52);
            Add_Action (Table.States (87), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (52, 1), 4, null,
            null);
            Add_Action (Table.States (88), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (52, 2), 4, null,
            null);
            Add_Action (Table.States (89), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (51, 1), 4, null,
            null);
            Add_Action (Table.States (90), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (52, 3), 4, null,
            null);
            Add_Action (Table.States (91), 16, 93);
            Add_Error (Table.States (91));
            Add_Action (Table.States (92), 32, 94);
            Add_Error (Table.States (92));
            Add_Action (Table.States (93), (11, 12, 17, 18, 19, 20, 22, 25, 26, 27, 28, 32, 34, 35), (47, 0), 5, null,
            null);
            Add_Action (Table.States (94), (12, 22, 28, 32, 35), (45, 2), 5, null, null);
         end Subr_1;
      begin
         Subr_1;
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
