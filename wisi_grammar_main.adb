--  generated parser support file.
--  command line: wisi-generate.exe  --generate LALR ADA re2c wisi_grammar.wy
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

with WisiToken.Lexer.re2c;
with wisi_grammar_re2c_c;
with Wisi_Grammar_Actions; use Wisi_Grammar_Actions;
package body Wisi_Grammar_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisi_grammar_re2c_c.New_Lexer,
      wisi_grammar_re2c_c.Free_Lexer,
      wisi_grammar_re2c_c.Reset_Lexer,
      wisi_grammar_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 61,
         First_Terminal    => Trace.Descriptor.First_Terminal,
         Last_Terminal     => Trace.Descriptor.Last_Terminal,
         First_Nonterminal => Trace.Descriptor.First_Nonterminal,
         Last_Nonterminal  => Trace.Descriptor.Last_Nonterminal);
   begin
      declare
         procedure Subr_1
         is begin
            Add_Action (Table.States (0), ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (Table.States (0), (1 => (32, 0)), 22, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 27, 3);
            Add_Goto (Table.States (0), 32, 4);
            Add_Goto (Table.States (0), 36, 5);
            Add_Goto (Table.States (0), 37, 6);
            Add_Action (Table.States (1), (1 => (27, 1)), 3, 7);
            Add_Action (Table.States (1), (1 => (27, 5)), 4, 8);
            Add_Action (Table.States (1), (1 => (27, 4)), 5, 9);
            Add_Action (Table.States (1), (1 => (28, 0)), 6, 10);
            Add_Action (Table.States (1), (1 => (28, 1)), 7, 11);
            Add_Action (Table.States (1), (1 => (28, 2)), 8, 12);
            Add_Action (Table.States (1), ((27, 2), (27, 3)), 22, 13);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 28, 14);
            Add_Action (Table.States (2), (1 => (32, 0)), 13, 15);
            Add_Error (Table.States (2));
            Add_Action (Table.States (3), (18, 22, 25), (36, 0), 1, null, null);
            Add_Action (Table.States (4), (18, 22, 25), (36, 1), 1, null, null);
            Add_Action (Table.States (5), (18, 22, 25), (37, 0), 1, null, null);
            Add_Action (Table.States (6), ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (Table.States (6), (1 => (32, 0)), 22, 2);
            Add_Action (Table.States (6), 25, Accept_It, (26, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 27, 3);
            Add_Goto (Table.States (6), 32, 4);
            Add_Goto (Table.States (6), 36, 16);
            Add_Action (Table.States (7), (1 => (29, 0)), 22, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 29, 18);
            Add_Action (Table.States (8), (1 => (27, 5)), 5, 19);
            Add_Error (Table.States (8));
            Add_Action (Table.States (9), (1 => (27, 4)), 22, 20);
            Add_Error (Table.States (9));
            Add_Action (Table.States (10), (1 =>  22), (28, 0), 1, null, null);
            Add_Action (Table.States (11), (1 => (28, 1)), 17, 21);
            Add_Error (Table.States (11));
            Add_Action (Table.States (12), (1 => (28, 2)), 17, 22);
            Add_Error (Table.States (12));
            Add_Action (Table.States (13), (1 => (31, 8)), 8, 23);
            Add_Action (Table.States (13), (1 => (31, 4)), 10, 24);
            Add_Action (Table.States (13), (1 => (31, 0)), 14, 25);
            Add_Action (Table.States (13), (1 => (31, 2)), 15, 26);
            Add_Action (Table.States (13), 18, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), (1 => (31, 5)), 20, 27);
            Add_Action (Table.States (13), (1 => (31, 3)), 21, 28);
            Add_Action (Table.States (13), (1 => (31, 1)), 22, 29, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), (1 => (31, 6)), 23, 30);
            Add_Action (Table.States (13), (1 => (31, 7)), 24, 31);
            Add_Action (Table.States (13), 25, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (13));
            Add_Goto (Table.States (13), 30, 32);
            Add_Goto (Table.States (13), 31, 33);
            Add_Action (Table.States (14), (1 => (27, 0)), 22, 34);
            Add_Error (Table.States (14));
            Add_Action (Table.States (15), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), (1 => (35, 0)), 22, 35);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 33, 36);
            Add_Goto (Table.States (15), 34, 37);
            Add_Goto (Table.States (15), 35, 38);
            Add_Action (Table.States (16), (18, 22, 25), (37, 1), 2, null, null);
            Add_Action (Table.States (17), (9, 22), (29, 0), 1, null, null);
            Add_Action (Table.States (18), (1 => (27, 1)), 9, 39);
            Add_Action (Table.States (18), (1 => (29, 1)), 22, 40);
            Add_Error (Table.States (18));
            Add_Action (Table.States (19), (18, 22, 25), (27, 5), 3, declaration_5'Access, null);
            Add_Action (Table.States (20), (1 => (27, 4)), 15, 41);
            Add_Error (Table.States (20));
            Add_Action (Table.States (21), (1 => (28, 1)), 22, 42);
            Add_Error (Table.States (21));
            Add_Action (Table.States (22), (1 => (28, 2)), 22, 43);
            Add_Error (Table.States (22));
            Add_Action (Table.States (23), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 8), 1, null, null);
            Add_Action (Table.States (24), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 4), 1, null, null);
            Add_Action (Table.States (25), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 0), 1, null, null);
            Add_Action (Table.States (26), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 2), 1, null, null);
            Add_Action (Table.States (27), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 5), 1, null, null);
            Add_Action (Table.States (28), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 3), 1, null, null);
            Add_Action (Table.States (29), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 1), 1, null, null);
            Add_Action (Table.States (30), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 6), 1, null, null);
            Add_Action (Table.States (31), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 7), 1, null, null);
            Add_Action (Table.States (32), (1 => (31, 8)), 8, 23);
            Add_Action (Table.States (32), (1 => (31, 4)), 10, 24);
            Add_Action (Table.States (32), (1 => (31, 0)), 14, 25);
            Add_Action (Table.States (32), (1 => (31, 2)), 15, 26);
            Add_Action (Table.States (32), 18, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), (1 => (31, 5)), 20, 27);
            Add_Action (Table.States (32), (1 => (31, 3)), 21, 28);
            Add_Action (Table.States (32), (1 => (31, 1)), 22, 29, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), (1 => (31, 6)), 23, 30);
            Add_Action (Table.States (32), (1 => (31, 7)), 24, 31);
            Add_Action (Table.States (32), 25, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (32));
            Add_Goto (Table.States (32), 31, 44);
            Add_Action (Table.States (33), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 0), 1, null, null);
            Add_Action (Table.States (34), (1 => (31, 8)), 8, 23);
            Add_Action (Table.States (34), (1 => (31, 4)), 10, 24);
            Add_Action (Table.States (34), (1 => (31, 0)), 14, 25);
            Add_Action (Table.States (34), (1 => (31, 2)), 15, 26);
            Add_Action (Table.States (34), (1 => (31, 5)), 20, 27);
            Add_Action (Table.States (34), (1 => (31, 3)), 21, 28);
            Add_Action (Table.States (34), (1 => (31, 1)), 22, 29);
            Add_Action (Table.States (34), (1 => (31, 6)), 23, 30);
            Add_Action (Table.States (34), (1 => (31, 7)), 24, 31);
            Add_Error (Table.States (34));
            Add_Goto (Table.States (34), 30, 45);
            Add_Goto (Table.States (34), 31, 33);
            Add_Action (Table.States (35), (11, 12, 18, 19, 22), (35, 0), 1, null, null);
            Add_Action (Table.States (36), (1 => (33, 1)), 12, 46);
            Add_Action (Table.States (36), ((33, 2), (33, 3)), 18, 47);
            Add_Action (Table.States (36), (1 => (32, 0)), 19, 48);
            Add_Error (Table.States (36));
            Add_Action (Table.States (37), (12, 18, 19), (33, 0), 1, null, null);
            Add_Action (Table.States (38), ((34, 2), (34, 3)), 11, 49);
            Add_Action (Table.States (38), 12, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 18, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 19, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), (1 => (35, 1)), 22, 50);
            Add_Error (Table.States (38));
            Add_Action (Table.States (39), (18, 22, 25), (27, 1), 4, declaration_1'Access, null);
            Add_Action (Table.States (40), (9, 22), (29, 1), 2, null, null);
            Add_Action (Table.States (41), (1 => (27, 4)), 22, 51);
            Add_Error (Table.States (41));
            Add_Action (Table.States (42), (1 => (28, 1)), 16, 52);
            Add_Error (Table.States (42));
            Add_Action (Table.States (43), (1 => (28, 2)), 16, 53);
            Add_Error (Table.States (43));
            Add_Action (Table.States (44), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 1), 2, null, null);
            Add_Action (Table.States (45), (1 => (31, 8)), 8, 23);
            Add_Action (Table.States (45), (1 => (31, 4)), 10, 24);
            Add_Action (Table.States (45), (1 => (31, 0)), 14, 25);
            Add_Action (Table.States (45), (1 => (31, 2)), 15, 26);
            Add_Action (Table.States (45), 18, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (45), (1 => (31, 5)), 20, 27);
            Add_Action (Table.States (45), (1 => (31, 3)), 21, 28);
            Add_Action (Table.States (45), (1 => (31, 1)), 22, 29, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (45), (1 => (31, 6)), 23, 30);
            Add_Action (Table.States (45), (1 => (31, 7)), 24, 31);
            Add_Action (Table.States (45), 25, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (45));
            Add_Goto (Table.States (45), 31, 44);
            Add_Action (Table.States (46), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), (1 => (35, 0)), 22, 35);
            Add_Error (Table.States (46));
            Add_Goto (Table.States (46), 34, 54);
            Add_Goto (Table.States (46), 35, 38);
            Add_Action (Table.States (47), (1 => (33, 3)), 4, 55);
            Add_Action (Table.States (47), (1 => (33, 2)), 5, 56);
            Add_Error (Table.States (47));
            Add_Action (Table.States (48), (18, 22, 25), (32, 0), 4, nonterminal_0'Access, null);
            Add_Action (Table.States (49), (1 => (34, 3)), 11, 57);
            Add_Action (Table.States (49), 12, Reduce, (34, 2), 2, null, null);
            Add_Action (Table.States (49), 18, Reduce, (34, 2), 2, null, null);
            Add_Action (Table.States (49), 19, Reduce, (34, 2), 2, null, null);
            Add_Error (Table.States (49));
            Add_Action (Table.States (50), (11, 12, 18, 19, 22), (35, 1), 2, null, null);
            Add_Action (Table.States (51), (18, 22, 25), (27, 4), 5, declaration_4'Access, null);
            Add_Action (Table.States (52), (1 =>  22), (28, 1), 4, null, null);
            Add_Action (Table.States (53), (1 =>  22), (28, 2), 4, null, null);
            Add_Action (Table.States (54), (12, 18, 19), (33, 1), 3, null, null);
            Add_Action (Table.States (55), (1 => (33, 3)), 5, 58);
            Add_Error (Table.States (55));
            Add_Action (Table.States (56), (1 => (33, 2)), 22, 59);
            Add_Error (Table.States (56));
            Add_Action (Table.States (57), (12, 18, 19), (34, 3), 3, null, null);
            Add_Action (Table.States (58), (12, 18, 19), (33, 3), 4, null, null);
            Add_Action (Table.States (59), (1 => (33, 2)), 15, 60);
            Add_Error (Table.States (59));
            Add_Action (Table.States (60), (1 => (33, 2)), 22, 61);
            Add_Error (Table.States (60));
            Add_Action (Table.States (61), (12, 18, 19), (33, 2), 6, null, null);
         end Subr_1;
      begin
         Subr_1;
      end;

      WisiToken.LR.Parser_No_Recover.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace),
         Table,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisi_Grammar_Main;
