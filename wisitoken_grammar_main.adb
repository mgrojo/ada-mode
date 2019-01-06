--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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
         State_Last        => 89,
         First_Terminal    => 3,
         Last_Terminal     => 34,
         First_Nonterminal => 35,
         Last_Nonterminal  => 52);
   begin
      declare
         procedure Subr_1
         is begin
            Add_Action (Table.States (0), 21, 1);
            Add_Action (Table.States (0), 31, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 36, 3);
            Add_Goto (Table.States (0), 41, 4);
            Add_Goto (Table.States (0), 51, 5);
            Add_Goto (Table.States (0), 52, 6);
            Set_Minimal_Action (Table.States (0).Minimal_Complete_Actions, (1 => (Shift, 31, 2)));
            Add_Action (Table.States (1), 3, 7);
            Add_Action (Table.States (1), 4, 8);
            Add_Action (Table.States (1), 5, 9);
            Add_Action (Table.States (1), 6, 10);
            Add_Action (Table.States (1), 7, 11);
            Add_Action (Table.States (1), 8, 12);
            Add_Action (Table.States (1), 31, 13);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 37, 14);
            Set_Minimal_Action (Table.States (1).Minimal_Complete_Actions, ((Shift, 3, 7), (Shift, 4, 8), (Shift, 5,
            9), (Shift, 6, 10), (Shift, 31, 13)));
            Add_Action (Table.States (2), 13, 15);
            Add_Error (Table.States (2));
            Set_Minimal_Action (Table.States (2).Minimal_Complete_Actions, (1 => (Shift, 13, 15)));
            Add_Action (Table.States (3), (21, 31, 34), (51, 0), 1, null, null);
            Set_Minimal_Action (Table.States (3).Minimal_Complete_Actions, (1 => (Reduce, 51, 1)));
            Add_Action (Table.States (4), (21, 31, 34), (51, 1), 1, null, null);
            Set_Minimal_Action (Table.States (4).Minimal_Complete_Actions, (1 => (Reduce, 51, 1)));
            Add_Action (Table.States (5), (21, 31, 34), (52, 0), 1, null, null);
            Set_Minimal_Action (Table.States (5).Minimal_Complete_Actions, (1 => (Reduce, 52, 1)));
            Add_Action (Table.States (6), 21, 1);
            Add_Action (Table.States (6), 31, 2);
            Add_Action (Table.States (6), 34, Accept_It, (35, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 36, 3);
            Add_Goto (Table.States (6), 41, 4);
            Add_Goto (Table.States (6), 51, 16);
            Add_Action (Table.States (7), 31, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 38, 18);
            Set_Minimal_Action (Table.States (7).Minimal_Complete_Actions, (1 => (Shift, 31, 17)));
            Add_Action (Table.States (8), 5, 19);
            Add_Error (Table.States (8));
            Set_Minimal_Action (Table.States (8).Minimal_Complete_Actions, (1 => (Shift, 5, 19)));
            Add_Action (Table.States (9), 31, 20);
            Add_Error (Table.States (9));
            Set_Minimal_Action (Table.States (9).Minimal_Complete_Actions, (1 => (Shift, 31, 20)));
            Add_Action (Table.States (10), (1 =>  31), (37, 0), 1, null, null);
            Set_Minimal_Action (Table.States (10).Minimal_Complete_Actions, (1 => (Reduce, 37, 1)));
            Add_Action (Table.States (11), 20, 21);
            Add_Error (Table.States (11));
            Set_Minimal_Action (Table.States (11).Minimal_Complete_Actions, (1 => (Shift, 20, 21)));
            Add_Action (Table.States (12), 20, 22);
            Add_Error (Table.States (12));
            Set_Minimal_Action (Table.States (12).Minimal_Complete_Actions, (1 => (Shift, 20, 22)));
            Add_Action (Table.States (13), 8, 23);
            Add_Action (Table.States (13), 10, 24);
            Add_Action (Table.States (13), 14, 25);
            Add_Action (Table.States (13), 15, 26);
            Add_Action (Table.States (13), 21, Reduce, (36, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 28, 27);
            Add_Action (Table.States (13), 30, 28);
            Add_Action (Table.States (13), 31, 29, (36, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 32, 30);
            Add_Action (Table.States (13), 33, 31);
            Add_Action (Table.States (13), 34, Reduce, (36, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (13));
            Add_Goto (Table.States (13), 39, 32);
            Add_Goto (Table.States (13), 40, 33);
            Set_Minimal_Action (Table.States (13).Minimal_Complete_Actions, (1 => (Reduce, 36, 2)));
            Add_Action (Table.States (14), 31, 34);
            Add_Error (Table.States (14));
            Set_Minimal_Action (Table.States (14).Minimal_Complete_Actions, (1 => (Shift, 31, 34)));
            Add_Action (Table.States (15), 12, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 17, 35);
            Add_Action (Table.States (15), 18, 36);
            Add_Action (Table.States (15), 19, 37);
            Add_Action (Table.States (15), 20, 38);
            Add_Action (Table.States (15), 21, 39, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 27, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 31, 40, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 33, 41);
            Add_Action (Table.States (15), 34, Reduce, (44, 0), 0, null, null);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 43, 42);
            Add_Goto (Table.States (15), 44, 43);
            Add_Goto (Table.States (15), 45, 44);
            Add_Goto (Table.States (15), 46, 45);
            Add_Goto (Table.States (15), 47, 46);
            Add_Goto (Table.States (15), 48, 47);
            Add_Goto (Table.States (15), 49, 48);
            Set_Minimal_Action (Table.States (15).Minimal_Complete_Actions, (1 => (Reduce, 43, 0)));
            Add_Action (Table.States (16), (21, 31, 34), (52, 1), 2, null, null);
            Set_Minimal_Action (Table.States (16).Minimal_Complete_Actions, (1 => (Reduce, 52, 2)));
            Add_Action (Table.States (17), (9, 31), (38, 0), 1, null, null);
            Set_Minimal_Action (Table.States (17).Minimal_Complete_Actions, (1 => (Reduce, 38, 1)));
            Add_Action (Table.States (18), 9, 49);
            Add_Action (Table.States (18), 31, 50);
            Add_Error (Table.States (18));
            Set_Minimal_Action (Table.States (18).Minimal_Complete_Actions, (1 => (Shift, 9, 49)));
            Add_Action (Table.States (19), (21, 31, 34), (36, 5), 3, declaration_5'Access, null);
            Set_Minimal_Action (Table.States (19).Minimal_Complete_Actions, (1 => (Reduce, 36, 3)));
            Add_Action (Table.States (20), 15, 51);
            Add_Error (Table.States (20));
            Set_Minimal_Action (Table.States (20).Minimal_Complete_Actions, (1 => (Shift, 15, 51)));
            Add_Action (Table.States (21), 31, 52);
            Add_Error (Table.States (21));
            Set_Minimal_Action (Table.States (21).Minimal_Complete_Actions, (1 => (Shift, 31, 52)));
            Add_Action (Table.States (22), 31, 53);
            Add_Error (Table.States (22));
            Set_Minimal_Action (Table.States (22).Minimal_Complete_Actions, (1 => (Shift, 31, 53)));
            Add_Action (Table.States (23), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 8), 1, null, null);
            Set_Minimal_Action (Table.States (23).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (24), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 4), 1, null, null);
            Set_Minimal_Action (Table.States (24).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (25), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 0), 1, null, null);
            Set_Minimal_Action (Table.States (25).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (26), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 2), 1, null, null);
            Set_Minimal_Action (Table.States (26).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (27), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 5), 1, null, null);
            Set_Minimal_Action (Table.States (27).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (28), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 3), 1, null, null);
            Set_Minimal_Action (Table.States (28).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (29), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 1), 1, null, null);
            Set_Minimal_Action (Table.States (29).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (30), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 6), 1, null, null);
            Set_Minimal_Action (Table.States (30).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (31), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 7), 1, null, null);
            Set_Minimal_Action (Table.States (31).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Add_Action (Table.States (32), 8, 23);
            Add_Action (Table.States (32), 10, 24);
            Add_Action (Table.States (32), 14, 25);
            Add_Action (Table.States (32), 15, 26);
            Add_Action (Table.States (32), 21, Reduce, (36, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), 28, 27);
            Add_Action (Table.States (32), 30, 28);
            Add_Action (Table.States (32), 31, 29, (36, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), 32, 30);
            Add_Action (Table.States (32), 33, 31);
            Add_Action (Table.States (32), 34, Reduce, (36, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (32));
            Add_Goto (Table.States (32), 40, 54);
            Set_Minimal_Action (Table.States (32).Minimal_Complete_Actions, (1 => (Reduce, 36, 3)));
            Add_Action (Table.States (33), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (39, 0), 1, null, null);
            Set_Minimal_Action (Table.States (33).Minimal_Complete_Actions, (1 => (Reduce, 39, 1)));
            Add_Action (Table.States (34), 8, 23);
            Add_Action (Table.States (34), 10, 24);
            Add_Action (Table.States (34), 14, 25);
            Add_Action (Table.States (34), 15, 26);
            Add_Action (Table.States (34), 28, 27);
            Add_Action (Table.States (34), 30, 28);
            Add_Action (Table.States (34), 31, 29);
            Add_Action (Table.States (34), 32, 30);
            Add_Action (Table.States (34), 33, 31);
            Add_Error (Table.States (34));
            Add_Goto (Table.States (34), 39, 55);
            Add_Goto (Table.States (34), 40, 33);
            Set_Minimal_Action (Table.States (34).Minimal_Complete_Actions, (1 => (Shift, 8, 23)));
            Add_Action (Table.States (35), 17, 35);
            Add_Action (Table.States (35), 18, 36);
            Add_Action (Table.States (35), 19, 37);
            Add_Action (Table.States (35), 20, 38);
            Add_Action (Table.States (35), 31, 40);
            Add_Action (Table.States (35), 33, 41);
            Add_Error (Table.States (35));
            Add_Goto (Table.States (35), 45, 56);
            Add_Goto (Table.States (35), 46, 45);
            Add_Goto (Table.States (35), 47, 46);
            Add_Goto (Table.States (35), 48, 47);
            Add_Goto (Table.States (35), 49, 48);
            Add_Goto (Table.States (35), 50, 57);
            Set_Minimal_Action (Table.States (35).Minimal_Complete_Actions, (1 => (Shift, 33, 41)));
            Add_Action (Table.States (36), 17, 35);
            Add_Action (Table.States (36), 18, 36);
            Add_Action (Table.States (36), 19, 37);
            Add_Action (Table.States (36), 20, 38);
            Add_Action (Table.States (36), 31, 40);
            Add_Action (Table.States (36), 33, 41);
            Add_Error (Table.States (36));
            Add_Goto (Table.States (36), 45, 56);
            Add_Goto (Table.States (36), 46, 45);
            Add_Goto (Table.States (36), 47, 46);
            Add_Goto (Table.States (36), 48, 47);
            Add_Goto (Table.States (36), 49, 48);
            Add_Goto (Table.States (36), 50, 58);
            Set_Minimal_Action (Table.States (36).Minimal_Complete_Actions, (1 => (Shift, 33, 41)));
            Add_Action (Table.States (37), 17, 35);
            Add_Action (Table.States (37), 18, 36);
            Add_Action (Table.States (37), 19, 37);
            Add_Action (Table.States (37), 20, 38);
            Add_Action (Table.States (37), 31, 40);
            Add_Action (Table.States (37), 33, 41);
            Add_Error (Table.States (37));
            Add_Goto (Table.States (37), 45, 56);
            Add_Goto (Table.States (37), 46, 45);
            Add_Goto (Table.States (37), 47, 46);
            Add_Goto (Table.States (37), 48, 47);
            Add_Goto (Table.States (37), 49, 48);
            Add_Goto (Table.States (37), 50, 59);
            Set_Minimal_Action (Table.States (37).Minimal_Complete_Actions, (1 => (Shift, 33, 41)));
            Add_Action (Table.States (38), 31, 60);
            Add_Error (Table.States (38));
            Set_Minimal_Action (Table.States (38).Minimal_Complete_Actions, (1 => (Shift, 31, 60)));
            Add_Action (Table.States (39), 4, 61);
            Add_Action (Table.States (39), 5, 62);
            Add_Error (Table.States (39));
            Set_Minimal_Action (Table.States (39).Minimal_Complete_Actions, ((Shift, 4, 61), (Shift, 5, 62)));
            Add_Action (Table.States (40), 11, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 12, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 15, 63);
            Add_Action (Table.States (40), 17, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 18, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 19, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 20, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 21, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 24, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 25, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 26, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 27, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 31, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 33, Reduce, (46, 2), 1, null, null);
            Add_Action (Table.States (40), 34, Reduce, (46, 2), 1, null, null);
            Add_Error (Table.States (40));
            Set_Minimal_Action (Table.States (40).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Add_Action (Table.States (41), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 3), 1, null,
            null);
            Set_Minimal_Action (Table.States (41).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Add_Action (Table.States (42), 12, 64);
            Add_Action (Table.States (42), 21, Reduce, (42, 1), 0, null, null);
            Add_Action (Table.States (42), 27, 65);
            Add_Action (Table.States (42), 31, Reduce, (42, 1), 0, null, null);
            Add_Action (Table.States (42), 34, Reduce, (42, 1), 0, null, null);
            Add_Error (Table.States (42));
            Add_Goto (Table.States (42), 42, 66);
            Set_Minimal_Action (Table.States (42).Minimal_Complete_Actions, (1 => (Reduce, 42, 0)));
            Add_Action (Table.States (43), (12, 21, 27, 31, 34), (43, 0), 1, null, null);
            Set_Minimal_Action (Table.States (43).Minimal_Complete_Actions, (1 => (Reduce, 43, 1)));
            Add_Action (Table.States (44), 11, 67);
            Add_Action (Table.States (44), 12, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (44), 17, 35);
            Add_Action (Table.States (44), 18, 36);
            Add_Action (Table.States (44), 19, 37);
            Add_Action (Table.States (44), 20, 38);
            Add_Action (Table.States (44), 21, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (44), 27, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (44), 31, 40, (44, 1), 1, null, null);
            Add_Action (Table.States (44), 33, 41);
            Add_Action (Table.States (44), 34, Reduce, (44, 1), 1, null, null);
            Add_Error (Table.States (44));
            Add_Goto (Table.States (44), 46, 68);
            Add_Goto (Table.States (44), 47, 46);
            Add_Goto (Table.States (44), 48, 47);
            Add_Goto (Table.States (44), 49, 48);
            Set_Minimal_Action (Table.States (44).Minimal_Complete_Actions, (1 => (Reduce, 44, 1)));
            Add_Action (Table.States (45), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (45, 0), 1, null,
            null);
            Set_Minimal_Action (Table.States (45).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Add_Action (Table.States (46), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 6), 1,
            rhs_item_6'Access, null);
            Set_Minimal_Action (Table.States (46).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Add_Action (Table.States (47), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 4), 1,
            rhs_item_4'Access, null);
            Set_Minimal_Action (Table.States (47).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Add_Action (Table.States (48), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 5), 1,
            rhs_item_5'Access, null);
            Set_Minimal_Action (Table.States (48).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Add_Action (Table.States (49), (21, 31, 34), (36, 1), 4, declaration_1'Access, null);
            Set_Minimal_Action (Table.States (49).Minimal_Complete_Actions, (1 => (Reduce, 36, 4)));
            Add_Action (Table.States (50), (9, 31), (38, 1), 2, null, null);
            Set_Minimal_Action (Table.States (50).Minimal_Complete_Actions, (1 => (Reduce, 38, 2)));
            Add_Action (Table.States (51), 31, 69);
            Add_Error (Table.States (51));
            Set_Minimal_Action (Table.States (51).Minimal_Complete_Actions, (1 => (Shift, 31, 69)));
            Add_Action (Table.States (52), 16, 70);
            Add_Error (Table.States (52));
            Set_Minimal_Action (Table.States (52).Minimal_Complete_Actions, (1 => (Shift, 16, 70)));
            Add_Action (Table.States (53), 16, 71);
            Add_Error (Table.States (53));
            Set_Minimal_Action (Table.States (53).Minimal_Complete_Actions, (1 => (Shift, 16, 71)));
            Add_Action (Table.States (54), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (39, 1), 2, null, null);
            Set_Minimal_Action (Table.States (54).Minimal_Complete_Actions, (1 => (Reduce, 39, 2)));
            Add_Action (Table.States (55), 8, 23);
            Add_Action (Table.States (55), 10, 24);
            Add_Action (Table.States (55), 14, 25);
            Add_Action (Table.States (55), 15, 26);
            Add_Action (Table.States (55), 21, Reduce, (36, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (55), 28, 27);
            Add_Action (Table.States (55), 30, 28);
            Add_Action (Table.States (55), 31, 29, (36, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (55), 32, 30);
            Add_Action (Table.States (55), 33, 31);
            Add_Action (Table.States (55), 34, Reduce, (36, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (55));
            Add_Goto (Table.States (55), 40, 54);
            Set_Minimal_Action (Table.States (55).Minimal_Complete_Actions, (1 => (Reduce, 36, 4)));
            Add_Action (Table.States (56), 12, 72);
            Add_Action (Table.States (56), 17, 35);
            Add_Action (Table.States (56), 18, 36);
            Add_Action (Table.States (56), 19, 37);
            Add_Action (Table.States (56), 20, 38);
            Add_Action (Table.States (56), 24, Reduce, (50, 0), 1, null, null);
            Add_Action (Table.States (56), 25, Reduce, (50, 0), 1, null, null);
            Add_Action (Table.States (56), 26, Reduce, (50, 0), 1, null, null);
            Add_Action (Table.States (56), 31, 40);
            Add_Action (Table.States (56), 33, 41);
            Add_Error (Table.States (56));
            Add_Goto (Table.States (56), 46, 68);
            Add_Goto (Table.States (56), 47, 46);
            Add_Goto (Table.States (56), 48, 47);
            Add_Goto (Table.States (56), 49, 48);
            Set_Minimal_Action (Table.States (56).Minimal_Complete_Actions, (1 => (Reduce, 50, 1)));
            Add_Action (Table.States (57), 24, 73);
            Add_Error (Table.States (57));
            Set_Minimal_Action (Table.States (57).Minimal_Complete_Actions, (1 => (Shift, 24, 73)));
            Add_Action (Table.States (58), 25, 74);
            Add_Error (Table.States (58));
            Set_Minimal_Action (Table.States (58).Minimal_Complete_Actions, (1 => (Shift, 25, 74)));
            Add_Action (Table.States (59), 26, 75);
            Add_Error (Table.States (59));
            Set_Minimal_Action (Table.States (59).Minimal_Complete_Actions, (1 => (Shift, 26, 75)));
            Add_Action (Table.States (60), 15, 76);
            Add_Error (Table.States (60));
            Set_Minimal_Action (Table.States (60).Minimal_Complete_Actions, (1 => (Shift, 15, 76)));
            Add_Action (Table.States (61), 5, 77);
            Add_Error (Table.States (61));
            Set_Minimal_Action (Table.States (61).Minimal_Complete_Actions, (1 => (Shift, 5, 77)));
            Add_Action (Table.States (62), 31, 78);
            Add_Error (Table.States (62));
            Set_Minimal_Action (Table.States (62).Minimal_Complete_Actions, (1 => (Shift, 31, 78)));
            Add_Action (Table.States (63), 31, 79);
            Add_Error (Table.States (63));
            Set_Minimal_Action (Table.States (63).Minimal_Complete_Actions, (1 => (Shift, 31, 79)));
            Add_Action (Table.States (64), 12, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (64), 17, 35);
            Add_Action (Table.States (64), 18, 36);
            Add_Action (Table.States (64), 19, 37);
            Add_Action (Table.States (64), 20, 38);
            Add_Action (Table.States (64), 21, 39, (44, 0), 0, null, null);
            Add_Action (Table.States (64), 27, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (64), 31, 40, (44, 0), 0, null, null);
            Add_Action (Table.States (64), 33, 41);
            Add_Action (Table.States (64), 34, Reduce, (44, 0), 0, null, null);
            Add_Error (Table.States (64));
            Add_Goto (Table.States (64), 44, 80);
            Add_Goto (Table.States (64), 45, 44);
            Add_Goto (Table.States (64), 46, 45);
            Add_Goto (Table.States (64), 47, 46);
            Add_Goto (Table.States (64), 48, 47);
            Add_Goto (Table.States (64), 49, 48);
            Set_Minimal_Action (Table.States (64).Minimal_Complete_Actions, (1 => (Reduce, 44, 0)));
            Add_Action (Table.States (65), (21, 31, 34), (42, 0), 1, null, null);
            Set_Minimal_Action (Table.States (65).Minimal_Complete_Actions, (1 => (Reduce, 42, 1)));
            Add_Action (Table.States (66), (21, 31, 34), (41, 0), 4, nonterminal_0'Access, null);
            Set_Minimal_Action (Table.States (66).Minimal_Complete_Actions, (1 => (Reduce, 41, 4)));
            Add_Action (Table.States (67), 11, 81);
            Add_Action (Table.States (67), 12, Reduce, (44, 2), 2, null, null);
            Add_Action (Table.States (67), 21, Reduce, (44, 2), 2, null, null);
            Add_Action (Table.States (67), 27, Reduce, (44, 2), 2, null, null);
            Add_Action (Table.States (67), 31, Reduce, (44, 2), 2, null, null);
            Add_Action (Table.States (67), 34, Reduce, (44, 2), 2, null, null);
            Add_Error (Table.States (67));
            Set_Minimal_Action (Table.States (67).Minimal_Complete_Actions, (1 => (Reduce, 44, 2)));
            Add_Action (Table.States (68), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (45, 1), 2, null,
            null);
            Set_Minimal_Action (Table.States (68).Minimal_Complete_Actions, (1 => (Reduce, 45, 2)));
            Add_Action (Table.States (69), (21, 31, 34), (36, 4), 5, declaration_4'Access, null);
            Set_Minimal_Action (Table.States (69).Minimal_Complete_Actions, (1 => (Reduce, 36, 5)));
            Add_Action (Table.States (70), (1 =>  31), (37, 1), 4, null, null);
            Set_Minimal_Action (Table.States (70).Minimal_Complete_Actions, (1 => (Reduce, 37, 4)));
            Add_Action (Table.States (71), (1 =>  31), (37, 2), 4, null, null);
            Set_Minimal_Action (Table.States (71).Minimal_Complete_Actions, (1 => (Reduce, 37, 4)));
            Add_Action (Table.States (72), 17, 35);
            Add_Action (Table.States (72), 18, 36);
            Add_Action (Table.States (72), 19, 37);
            Add_Action (Table.States (72), 20, 38);
            Add_Action (Table.States (72), 31, 40);
            Add_Action (Table.States (72), 33, 41);
            Add_Error (Table.States (72));
            Add_Goto (Table.States (72), 45, 82);
            Add_Goto (Table.States (72), 46, 45);
            Add_Goto (Table.States (72), 47, 46);
            Add_Goto (Table.States (72), 48, 47);
            Add_Goto (Table.States (72), 49, 48);
            Set_Minimal_Action (Table.States (72).Minimal_Complete_Actions, (1 => (Shift, 33, 41)));
            Add_Action (Table.States (73), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (49, 0), 3, null,
            null);
            Set_Minimal_Action (Table.States (73).Minimal_Complete_Actions, (1 => (Reduce, 49, 3)));
            Add_Action (Table.States (74), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (48, 0), 3, null,
            null);
            Set_Minimal_Action (Table.States (74).Minimal_Complete_Actions, (1 => (Reduce, 48, 3)));
            Add_Action (Table.States (75), 11, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 12, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 17, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 18, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 19, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 20, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 21, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 22, 83);
            Add_Action (Table.States (75), 23, 84);
            Add_Action (Table.States (75), 24, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 25, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 26, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 27, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 29, 85);
            Add_Action (Table.States (75), 31, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 33, Reduce, (47, 0), 3, null, null);
            Add_Action (Table.States (75), 34, Reduce, (47, 0), 3, null, null);
            Add_Error (Table.States (75));
            Set_Minimal_Action (Table.States (75).Minimal_Complete_Actions, ((Shift, 22, 83), (Shift, 23, 84), (Shift,
            29, 85), (Reduce, 47, 3)));
            Add_Action (Table.States (76), 31, 86);
            Add_Error (Table.States (76));
            Set_Minimal_Action (Table.States (76).Minimal_Complete_Actions, (1 => (Shift, 31, 86)));
            Add_Action (Table.States (77), (12, 21, 27, 31, 34), (44, 5), 3, null, null);
            Set_Minimal_Action (Table.States (77).Minimal_Complete_Actions, (1 => (Reduce, 44, 3)));
            Add_Action (Table.States (78), 15, 87);
            Add_Error (Table.States (78));
            Set_Minimal_Action (Table.States (78).Minimal_Complete_Actions, (1 => (Shift, 15, 87)));
            Add_Action (Table.States (79), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 0), 3, null,
            null);
            Set_Minimal_Action (Table.States (79).Minimal_Complete_Actions, (1 => (Reduce, 46, 3)));
            Add_Action (Table.States (80), (12, 21, 27, 31, 34), (43, 1), 3, null, null);
            Set_Minimal_Action (Table.States (80).Minimal_Complete_Actions, (1 => (Reduce, 43, 3)));
            Add_Action (Table.States (81), (12, 21, 27, 31, 34), (44, 3), 3, null, null);
            Set_Minimal_Action (Table.States (81).Minimal_Complete_Actions, (1 => (Reduce, 44, 3)));
            Add_Action (Table.States (82), 17, 35);
            Add_Action (Table.States (82), 18, 36);
            Add_Action (Table.States (82), 19, 37);
            Add_Action (Table.States (82), 20, 38);
            Add_Action (Table.States (82), 24, Reduce, (50, 1), 3, null, null);
            Add_Action (Table.States (82), 25, Reduce, (50, 1), 3, null, null);
            Add_Action (Table.States (82), 26, Reduce, (50, 1), 3, null, null);
            Add_Action (Table.States (82), 31, 40);
            Add_Action (Table.States (82), 33, 41);
            Add_Error (Table.States (82));
            Add_Goto (Table.States (82), 46, 68);
            Add_Goto (Table.States (82), 47, 46);
            Add_Goto (Table.States (82), 48, 47);
            Add_Goto (Table.States (82), 49, 48);
            Set_Minimal_Action (Table.States (82).Minimal_Complete_Actions, (1 => (Reduce, 50, 3)));
            Add_Action (Table.States (83), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (49, 1), 4, null,
            null);
            Set_Minimal_Action (Table.States (83).Minimal_Complete_Actions, (1 => (Reduce, 49, 4)));
            Add_Action (Table.States (84), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (48, 1), 4, null,
            null);
            Set_Minimal_Action (Table.States (84).Minimal_Complete_Actions, (1 => (Reduce, 48, 4)));
            Add_Action (Table.States (85), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (49, 2), 4, null,
            null);
            Set_Minimal_Action (Table.States (85).Minimal_Complete_Actions, (1 => (Reduce, 49, 4)));
            Add_Action (Table.States (86), 16, 88);
            Add_Error (Table.States (86));
            Set_Minimal_Action (Table.States (86).Minimal_Complete_Actions, (1 => (Shift, 16, 88)));
            Add_Action (Table.States (87), 31, 89);
            Add_Error (Table.States (87));
            Set_Minimal_Action (Table.States (87).Minimal_Complete_Actions, (1 => (Shift, 31, 89)));
            Add_Action (Table.States (88), (11, 12, 17, 18, 19, 20, 21, 24, 25, 26, 27, 31, 33, 34), (46, 1), 5, null,
            null);
            Set_Minimal_Action (Table.States (88).Minimal_Complete_Actions, (1 => (Reduce, 46, 5)));
            Add_Action (Table.States (89), (12, 21, 27, 31, 34), (44, 4), 5, null, null);
            Set_Minimal_Action (Table.States (89).Minimal_Complete_Actions, (1 => (Reduce, 44, 5)));
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
