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
         State_Last        => 63,
         First_Terminal    => 3,
         Last_Terminal     => 27,
         First_Nonterminal => 28,
         Last_Nonterminal  => 39);
   begin
      declare
         procedure Subr_1
         is begin
            Add_Action (Table.States (0), 19, 1);
            Add_Action (Table.States (0), 24, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 29, 3);
            Add_Goto (Table.States (0), 34, 4);
            Add_Goto (Table.States (0), 38, 5);
            Add_Goto (Table.States (0), 39, 6);
            Add_Action (Table.States (1), 3, 7);
            Add_Action (Table.States (1), 4, 8);
            Add_Action (Table.States (1), 5, 9);
            Add_Action (Table.States (1), 6, 10);
            Add_Action (Table.States (1), 7, 11);
            Add_Action (Table.States (1), 8, 12);
            Add_Action (Table.States (1), 24, 13);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 30, 14);
            Add_Action (Table.States (2), 13, 15);
            Add_Error (Table.States (2));
            Add_Action (Table.States (3), (19, 24, 27), (38, 0), 1, null, null);
            Add_Action (Table.States (4), (19, 24, 27), (38, 1), 1, null, null);
            Add_Action (Table.States (5), (19, 24, 27), (39, 0), 1, null, null);
            Add_Action (Table.States (6), 19, 1);
            Add_Action (Table.States (6), 24, 2);
            Add_Action (Table.States (6), 27, Accept_It, (28, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 29, 3);
            Add_Goto (Table.States (6), 34, 4);
            Add_Goto (Table.States (6), 38, 16);
            Add_Action (Table.States (7), 24, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 31, 18);
            Add_Action (Table.States (8), 5, 19);
            Add_Error (Table.States (8));
            Add_Action (Table.States (9), 24, 20);
            Add_Error (Table.States (9));
            Add_Action (Table.States (10), (1 =>  24), (30, 0), 1, null, null);
            Add_Action (Table.States (11), 18, 21);
            Add_Error (Table.States (11));
            Add_Action (Table.States (12), 18, 22);
            Add_Error (Table.States (12));
            Add_Action (Table.States (13), 8, 23);
            Add_Action (Table.States (13), 10, 24);
            Add_Action (Table.States (13), 14, 25);
            Add_Action (Table.States (13), 15, 26);
            Add_Action (Table.States (13), 17, 27);
            Add_Action (Table.States (13), 19, Reduce, (29, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 20, 28);
            Add_Action (Table.States (13), 22, 29);
            Add_Action (Table.States (13), 23, 30);
            Add_Action (Table.States (13), 24, 31, (29, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 25, 32);
            Add_Action (Table.States (13), 26, 33);
            Add_Action (Table.States (13), 27, Reduce, (29, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (13));
            Add_Goto (Table.States (13), 32, 34);
            Add_Goto (Table.States (13), 33, 35);
            Add_Action (Table.States (14), 24, 36);
            Add_Error (Table.States (14));
            Add_Action (Table.States (15), 12, Reduce, (36, 0), 0, null, null);
            Add_Action (Table.States (15), 19, 37);
            Add_Action (Table.States (15), 21, Reduce, (36, 0), 0, null, null);
            Add_Action (Table.States (15), 24, 38);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 35, 39);
            Add_Goto (Table.States (15), 36, 40);
            Add_Goto (Table.States (15), 37, 41);
            Add_Action (Table.States (16), (19, 24, 27), (39, 1), 2, null, null);
            Add_Action (Table.States (17), (9, 24), (31, 0), 1, null, null);
            Add_Action (Table.States (18), 9, 42);
            Add_Action (Table.States (18), 24, 43);
            Add_Error (Table.States (18));
            Add_Action (Table.States (19), (19, 24, 27), (29, 5), 3, declaration_5'Access, null);
            Add_Action (Table.States (20), 15, 44);
            Add_Error (Table.States (20));
            Add_Action (Table.States (21), 24, 45);
            Add_Error (Table.States (21));
            Add_Action (Table.States (22), 24, 46);
            Add_Error (Table.States (22));
            Add_Action (Table.States (23), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 10), 1, null,
            null);
            Add_Action (Table.States (24), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 5), 1, null,
            null);
            Add_Action (Table.States (25), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 0), 1, null,
            null);
            Add_Action (Table.States (26), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 2), 1, null,
            null);
            Add_Action (Table.States (27), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 3), 1, null,
            null);
            Add_Action (Table.States (28), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 6), 1, null,
            null);
            Add_Action (Table.States (29), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 7), 1, null,
            null);
            Add_Action (Table.States (30), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 4), 1, null,
            null);
            Add_Action (Table.States (31), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 1), 1, null,
            null);
            Add_Action (Table.States (32), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 8), 1, null,
            null);
            Add_Action (Table.States (33), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (33, 9), 1, null,
            null);
            Add_Action (Table.States (34), 8, 23);
            Add_Action (Table.States (34), 10, 24);
            Add_Action (Table.States (34), 14, 25);
            Add_Action (Table.States (34), 15, 26);
            Add_Action (Table.States (34), 17, 27);
            Add_Action (Table.States (34), 19, Reduce, (29, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (34), 20, 28);
            Add_Action (Table.States (34), 22, 29);
            Add_Action (Table.States (34), 23, 30);
            Add_Action (Table.States (34), 24, 31, (29, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (34), 25, 32);
            Add_Action (Table.States (34), 26, 33);
            Add_Action (Table.States (34), 27, Reduce, (29, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (34));
            Add_Goto (Table.States (34), 33, 47);
            Add_Action (Table.States (35), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (32, 0), 1, null,
            null);
            Add_Action (Table.States (36), 8, 23);
            Add_Action (Table.States (36), 10, 24);
            Add_Action (Table.States (36), 14, 25);
            Add_Action (Table.States (36), 15, 26);
            Add_Action (Table.States (36), 17, 27);
            Add_Action (Table.States (36), 20, 28);
            Add_Action (Table.States (36), 22, 29);
            Add_Action (Table.States (36), 23, 30);
            Add_Action (Table.States (36), 24, 31);
            Add_Action (Table.States (36), 25, 32);
            Add_Action (Table.States (36), 26, 33);
            Add_Error (Table.States (36));
            Add_Goto (Table.States (36), 32, 48);
            Add_Goto (Table.States (36), 33, 35);
            Add_Action (Table.States (37), 4, 49);
            Add_Action (Table.States (37), 5, 50);
            Add_Error (Table.States (37));
            Add_Action (Table.States (38), (11, 12, 21, 24), (37, 0), 1, null, null);
            Add_Action (Table.States (39), 12, 51);
            Add_Action (Table.States (39), 21, 52);
            Add_Error (Table.States (39));
            Add_Action (Table.States (40), (12, 21), (35, 0), 1, null, null);
            Add_Action (Table.States (41), 11, 53);
            Add_Action (Table.States (41), 12, Reduce, (36, 1), 1, null, null);
            Add_Action (Table.States (41), 21, Reduce, (36, 1), 1, null, null);
            Add_Action (Table.States (41), 24, 54);
            Add_Error (Table.States (41));
            Add_Action (Table.States (42), (19, 24, 27), (29, 1), 4, declaration_1'Access, null);
            Add_Action (Table.States (43), (9, 24), (31, 1), 2, null, null);
            Add_Action (Table.States (44), 24, 55);
            Add_Error (Table.States (44));
            Add_Action (Table.States (45), 16, 56);
            Add_Error (Table.States (45));
            Add_Action (Table.States (46), 16, 57);
            Add_Error (Table.States (46));
            Add_Action (Table.States (47), (8, 10, 14, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27), (32, 1), 2, null,
            null);
            Add_Action (Table.States (48), 8, 23);
            Add_Action (Table.States (48), 10, 24);
            Add_Action (Table.States (48), 14, 25);
            Add_Action (Table.States (48), 15, 26);
            Add_Action (Table.States (48), 17, 27);
            Add_Action (Table.States (48), 19, Reduce, (29, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (48), 20, 28);
            Add_Action (Table.States (48), 22, 29);
            Add_Action (Table.States (48), 23, 30);
            Add_Action (Table.States (48), 24, 31, (29, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (48), 25, 32);
            Add_Action (Table.States (48), 26, 33);
            Add_Action (Table.States (48), 27, Reduce, (29, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (48));
            Add_Goto (Table.States (48), 33, 47);
            Add_Action (Table.States (49), 5, 58);
            Add_Error (Table.States (49));
            Add_Action (Table.States (50), 24, 59);
            Add_Error (Table.States (50));
            Add_Action (Table.States (51), 12, Reduce, (36, 0), 0, null, null);
            Add_Action (Table.States (51), 19, 37);
            Add_Action (Table.States (51), 21, Reduce, (36, 0), 0, null, null);
            Add_Action (Table.States (51), 24, 38);
            Add_Error (Table.States (51));
            Add_Goto (Table.States (51), 36, 60);
            Add_Goto (Table.States (51), 37, 41);
            Add_Action (Table.States (52), (19, 24, 27), (34, 0), 4, nonterminal_0'Access, null);
            Add_Action (Table.States (53), 11, 61);
            Add_Action (Table.States (53), 12, Reduce, (36, 2), 2, null, null);
            Add_Action (Table.States (53), 21, Reduce, (36, 2), 2, null, null);
            Add_Error (Table.States (53));
            Add_Action (Table.States (54), (11, 12, 21, 24), (37, 1), 2, null, null);
            Add_Action (Table.States (55), (19, 24, 27), (29, 4), 5, declaration_4'Access, null);
            Add_Action (Table.States (56), (1 =>  24), (30, 1), 4, null, null);
            Add_Action (Table.States (57), (1 =>  24), (30, 2), 4, null, null);
            Add_Action (Table.States (58), (12, 21), (36, 5), 3, null, null);
            Add_Action (Table.States (59), 15, 62);
            Add_Error (Table.States (59));
            Add_Action (Table.States (60), (12, 21), (35, 1), 3, null, null);
            Add_Action (Table.States (61), (12, 21), (36, 3), 3, null, null);
            Add_Action (Table.States (62), 24, 63);
            Add_Error (Table.States (62));
            Add_Action (Table.States (63), (12, 21), (36, 4), 5, null, null);
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
