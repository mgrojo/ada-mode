--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy
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
     (Parser                       :    out WisiToken.Parse.LR.Parser.Parser;
      Language_Fixes               : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Use_Minimal_Complete_Actions : in
     WisiToken.Parse.LR.Parser.Language_Use_Minimal_Complete_Actions_Access;
      Language_String_ID_Set       : in     WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 37,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Cost_Limit  => 2147483647,
         Check_Limit => 4,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 61,
         First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 37);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Productions := WisiToken.To_Vector ((1 => (26, 0)));
            Add_Action (Table.States (0), 18, 1);
            Add_Action (Table.States (0), 22, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 27, 3);
            Add_Goto (Table.States (0), 32, 4);
            Add_Goto (Table.States (0), 36, 5);
            Add_Goto (Table.States (0), 37, 6);
            Table.States (0).Minimal_Complete_Action := (Shift, 18, 1);
            Table.States (1).Productions := WisiToken.To_Vector (((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27,
            5)));
            Add_Action (Table.States (1), 3, 7);
            Add_Action (Table.States (1), 4, 8);
            Add_Action (Table.States (1), 5, 9);
            Add_Action (Table.States (1), 6, 10);
            Add_Action (Table.States (1), 7, 11);
            Add_Action (Table.States (1), 8, 12);
            Add_Action (Table.States (1), 22, 13);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 28, 14);
            Table.States (1).Minimal_Complete_Action := (Shift, 22, 13);
            Table.States (2).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (2), 13, 15);
            Add_Error (Table.States (2));
            Table.States (2).Minimal_Complete_Action := (Shift, 13, 15);
            Table.States (3).Productions := WisiToken.To_Vector ((1 => (36, 0)));
            Add_Action (Table.States (3), (18, 22, 25), (36, 0), 1, null, null);
            Table.States (3).Minimal_Complete_Action := (Reduce, 36, 1);
            Table.States (4).Productions := WisiToken.To_Vector ((1 => (36, 1)));
            Add_Action (Table.States (4), (18, 22, 25), (36, 1), 1, null, null);
            Table.States (4).Minimal_Complete_Action := (Reduce, 36, 1);
            Table.States (5).Productions := WisiToken.To_Vector ((1 => (37, 0)));
            Add_Action (Table.States (5), (18, 22, 25), (37, 0), 1, compilation_unit_list_0'Access, null);
            Table.States (5).Minimal_Complete_Action := (Reduce, 37, 1);
            Table.States (6).Productions := WisiToken.To_Vector (((26, 0), (37, 1)));
            Add_Action (Table.States (6), 18, 1);
            Add_Action (Table.States (6), 22, 2);
            Add_Action (Table.States (6), 25, Accept_It, (26, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 27, 3);
            Add_Goto (Table.States (6), 32, 4);
            Add_Goto (Table.States (6), 36, 16);
            Table.States (7).Productions := WisiToken.To_Vector ((1 => (27, 1)));
            Add_Action (Table.States (7), 22, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 29, 18);
            Table.States (7).Minimal_Complete_Action := (Shift, 22, 17);
            Table.States (8).Productions := WisiToken.To_Vector ((1 => (27, 5)));
            Add_Action (Table.States (8), 5, 19);
            Add_Error (Table.States (8));
            Table.States (8).Minimal_Complete_Action := (Shift, 5, 19);
            Table.States (9).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (9), 22, 20);
            Add_Error (Table.States (9));
            Table.States (9).Minimal_Complete_Action := (Shift, 22, 20);
            Table.States (10).Productions := WisiToken.To_Vector ((1 => (28, 0)));
            Add_Action (Table.States (10), (1 =>  22), (28, 0), 1, null, null);
            Table.States (10).Minimal_Complete_Action := (Reduce, 28, 1);
            Table.States (11).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (11), 17, 21);
            Add_Error (Table.States (11));
            Table.States (11).Minimal_Complete_Action := (Shift, 17, 21);
            Table.States (12).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (12), 17, 22);
            Add_Error (Table.States (12));
            Table.States (12).Minimal_Complete_Action := (Shift, 17, 22);
            Table.States (13).Productions := WisiToken.To_Vector (((27, 2), (27, 3)));
            Add_Action (Table.States (13), 8, 23);
            Add_Action (Table.States (13), 10, 24);
            Add_Action (Table.States (13), 14, 25);
            Add_Action (Table.States (13), 15, 26);
            Add_Action (Table.States (13), 18, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 20, 27);
            Add_Action (Table.States (13), 21, 28);
            Add_Action (Table.States (13), 22, 29, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (13), 23, 30);
            Add_Action (Table.States (13), 24, 31);
            Add_Action (Table.States (13), 25, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (13));
            Add_Goto (Table.States (13), 30, 32);
            Add_Goto (Table.States (13), 31, 33);
            Table.States (13).Minimal_Complete_Action := (Reduce, 27, 2);
            Table.States (14).Productions := WisiToken.To_Vector ((1 => (27, 0)));
            Add_Action (Table.States (14), 22, 34);
            Add_Error (Table.States (14));
            Table.States (14).Minimal_Complete_Action := (Shift, 22, 34);
            Table.States (15).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (15), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (15), 22, 35);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 33, 36);
            Add_Goto (Table.States (15), 34, 37);
            Add_Goto (Table.States (15), 35, 38);
            Table.States (15).Minimal_Complete_Action := (Reduce, 33, 0);
            Table.States (16).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (16), (18, 22, 25), (37, 1), 2, compilation_unit_list_1'Access, null);
            Table.States (16).Minimal_Complete_Action := (Reduce, 37, 2);
            Table.States (17).Productions := WisiToken.To_Vector ((1 => (29, 0)));
            Add_Action (Table.States (17), (9, 22), (29, 0), 1, null, null);
            Table.States (17).Minimal_Complete_Action := (Reduce, 29, 1);
            Table.States (18).Productions := WisiToken.To_Vector (((27, 1), (29, 1)));
            Add_Action (Table.States (18), 9, 39);
            Add_Action (Table.States (18), 22, 40);
            Add_Error (Table.States (18));
            Table.States (18).Minimal_Complete_Action := (Shift, 9, 39);
            Table.States (19).Productions := WisiToken.To_Vector ((1 => (27, 5)));
            Add_Action (Table.States (19), (18, 22, 25), (27, 5), 3, declaration_5'Access, null);
            Table.States (19).Minimal_Complete_Action := (Reduce, 27, 3);
            Table.States (20).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (20), 15, 41);
            Add_Error (Table.States (20));
            Table.States (20).Minimal_Complete_Action := (Shift, 15, 41);
            Table.States (21).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (21), 22, 42);
            Add_Error (Table.States (21));
            Table.States (21).Minimal_Complete_Action := (Shift, 22, 42);
            Table.States (22).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (22), 22, 43);
            Add_Error (Table.States (22));
            Table.States (22).Minimal_Complete_Action := (Shift, 22, 43);
            Table.States (23).Productions := WisiToken.To_Vector ((1 => (31, 8)));
            Add_Action (Table.States (23), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 8), 1, null, null);
            Table.States (23).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (24).Productions := WisiToken.To_Vector ((1 => (31, 4)));
            Add_Action (Table.States (24), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 4), 1,
            declaration_item_4'Access, null);
            Table.States (24).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (25).Productions := WisiToken.To_Vector ((1 => (31, 0)));
            Add_Action (Table.States (25), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 0), 1, null, null);
            Table.States (25).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (26).Productions := WisiToken.To_Vector ((1 => (31, 2)));
            Add_Action (Table.States (26), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 2), 1, null, null);
            Table.States (26).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (27).Productions := WisiToken.To_Vector ((1 => (31, 5)));
            Add_Action (Table.States (27), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 5), 1, null, null);
            Table.States (27).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (28).Productions := WisiToken.To_Vector ((1 => (31, 3)));
            Add_Action (Table.States (28), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 3), 1, null, null);
            Table.States (28).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (29).Productions := WisiToken.To_Vector ((1 => (31, 1)));
            Add_Action (Table.States (29), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 1), 1, null, null);
            Table.States (29).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (30).Productions := WisiToken.To_Vector ((1 => (31, 6)));
            Add_Action (Table.States (30), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 6), 1, null, null);
            Table.States (30).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (31).Productions := WisiToken.To_Vector ((1 => (31, 7)));
            Add_Action (Table.States (31), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 7), 1, null, null);
            Table.States (31).Minimal_Complete_Action := (Reduce, 31, 1);
            Table.States (32).Productions := WisiToken.To_Vector (((27, 2), (30, 1)));
            Add_Action (Table.States (32), 8, 23);
            Add_Action (Table.States (32), 10, 24);
            Add_Action (Table.States (32), 14, 25);
            Add_Action (Table.States (32), 15, 26);
            Add_Action (Table.States (32), 18, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), 20, 27);
            Add_Action (Table.States (32), 21, 28);
            Add_Action (Table.States (32), 22, 29, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (32), 23, 30);
            Add_Action (Table.States (32), 24, 31);
            Add_Action (Table.States (32), 25, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (32));
            Add_Goto (Table.States (32), 31, 44);
            Table.States (32).Minimal_Complete_Action := (Reduce, 27, 3);
            Table.States (33).Productions := WisiToken.To_Vector ((1 => (30, 0)));
            Add_Action (Table.States (33), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 0), 1, null, null);
            Table.States (33).Minimal_Complete_Action := (Reduce, 30, 1);
            Table.States (34).Productions := WisiToken.To_Vector ((1 => (27, 0)));
            Add_Action (Table.States (34), 8, 23);
            Add_Action (Table.States (34), 10, 24);
            Add_Action (Table.States (34), 14, 25);
            Add_Action (Table.States (34), 15, 26);
            Add_Action (Table.States (34), 20, 27);
            Add_Action (Table.States (34), 21, 28);
            Add_Action (Table.States (34), 22, 29);
            Add_Action (Table.States (34), 23, 30);
            Add_Action (Table.States (34), 24, 31);
            Add_Error (Table.States (34));
            Add_Goto (Table.States (34), 30, 45);
            Add_Goto (Table.States (34), 31, 33);
            Table.States (34).Minimal_Complete_Action := (Shift, 14, 25);
            Table.States (35).Productions := WisiToken.To_Vector ((1 => (35, 0)));
            Add_Action (Table.States (35), (11, 12, 18, 19, 22), (35, 0), 1, null, null);
            Table.States (35).Minimal_Complete_Action := (Reduce, 35, 1);
            Table.States (36).Productions := WisiToken.To_Vector (((32, 0), (33, 1), (33, 2), (33, 3)));
            Add_Action (Table.States (36), 12, 46);
            Add_Action (Table.States (36), 18, 47);
            Add_Action (Table.States (36), 19, 48);
            Add_Error (Table.States (36));
            Table.States (36).Minimal_Complete_Action := (Shift, 19, 48);
            Table.States (37).Productions := WisiToken.To_Vector ((1 => (33, 0)));
            Add_Action (Table.States (37), (12, 18, 19), (33, 0), 1, null, null);
            Table.States (37).Minimal_Complete_Action := (Reduce, 33, 1);
            Table.States (38).Productions := WisiToken.To_Vector (((34, 1), (34, 2), (34, 3), (35, 1)));
            Add_Action (Table.States (38), 11, 49);
            Add_Action (Table.States (38), 12, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 18, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 19, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 22, 50);
            Add_Error (Table.States (38));
            Table.States (38).Minimal_Complete_Action := (Reduce, 34, 1);
            Table.States (39).Productions := WisiToken.To_Vector ((1 => (27, 1)));
            Add_Action (Table.States (39), (18, 22, 25), (27, 1), 4, null, null);
            Table.States (39).Minimal_Complete_Action := (Reduce, 27, 4);
            Table.States (40).Productions := WisiToken.To_Vector ((1 => (29, 1)));
            Add_Action (Table.States (40), (9, 22), (29, 1), 2, null, null);
            Table.States (40).Minimal_Complete_Action := (Reduce, 29, 2);
            Table.States (41).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (41), 22, 51);
            Add_Error (Table.States (41));
            Table.States (41).Minimal_Complete_Action := (Shift, 22, 51);
            Table.States (42).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (42), 16, 52);
            Add_Error (Table.States (42));
            Table.States (42).Minimal_Complete_Action := (Shift, 16, 52);
            Table.States (43).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (43), 16, 53);
            Add_Error (Table.States (43));
            Table.States (43).Minimal_Complete_Action := (Shift, 16, 53);
            Table.States (44).Productions := WisiToken.To_Vector ((1 => (30, 1)));
            Add_Action (Table.States (44), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 1), 2, null, null);
            Table.States (44).Minimal_Complete_Action := (Reduce, 30, 2);
            Table.States (45).Productions := WisiToken.To_Vector (((27, 0), (30, 1)));
            Add_Action (Table.States (45), 8, 23);
            Add_Action (Table.States (45), 10, 24);
            Add_Action (Table.States (45), 14, 25);
            Add_Action (Table.States (45), 15, 26);
            Add_Action (Table.States (45), 18, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (45), 20, 27);
            Add_Action (Table.States (45), 21, 28);
            Add_Action (Table.States (45), 22, 29, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (45), 23, 30);
            Add_Action (Table.States (45), 24, 31);
            Add_Action (Table.States (45), 25, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (45));
            Add_Goto (Table.States (45), 31, 44);
            Table.States (45).Minimal_Complete_Action := (Reduce, 27, 4);
            Table.States (46).Productions := WisiToken.To_Vector ((1 => (33, 1)));
            Add_Action (Table.States (46), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (46), 22, 35);
            Add_Error (Table.States (46));
            Add_Goto (Table.States (46), 34, 54);
            Add_Goto (Table.States (46), 35, 38);
            Table.States (46).Minimal_Complete_Action := (Reduce, 34, 0);
            Table.States (47).Productions := WisiToken.To_Vector (((33, 2), (33, 3)));
            Add_Action (Table.States (47), 4, 55);
            Add_Action (Table.States (47), 5, 56);
            Add_Error (Table.States (47));
            Table.States (47).Minimal_Complete_Action := (Shift, 4, 55);
            Table.States (48).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (48), (18, 22, 25), (32, 0), 4, nonterminal_0'Access, null);
            Table.States (48).Minimal_Complete_Action := (Reduce, 32, 4);
            Table.States (49).Productions := WisiToken.To_Vector (((34, 2), (34, 3)));
            Add_Action (Table.States (49), 11, 57);
            Add_Action (Table.States (49), 12, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (49), 18, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (49), 19, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Error (Table.States (49));
            Table.States (49).Minimal_Complete_Action := (Reduce, 34, 2);
            Table.States (50).Productions := WisiToken.To_Vector ((1 => (35, 1)));
            Add_Action (Table.States (50), (11, 12, 18, 19, 22), (35, 1), 2, null, null);
            Table.States (50).Minimal_Complete_Action := (Reduce, 35, 2);
            Table.States (51).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (51), (18, 22, 25), (27, 4), 5, declaration_4'Access, null);
            Table.States (51).Minimal_Complete_Action := (Reduce, 27, 5);
            Table.States (52).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (52), (1 =>  22), (28, 1), 4, token_keyword_non_grammar_1'Access, null);
            Table.States (52).Minimal_Complete_Action := (Reduce, 28, 4);
            Table.States (53).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (53), (1 =>  22), (28, 2), 4, token_keyword_non_grammar_2'Access, null);
            Table.States (53).Minimal_Complete_Action := (Reduce, 28, 4);
            Table.States (54).Productions := WisiToken.To_Vector ((1 => (33, 1)));
            Add_Action (Table.States (54), (12, 18, 19), (33, 1), 3, null, null);
            Table.States (54).Minimal_Complete_Action := (Reduce, 33, 3);
            Table.States (55).Productions := WisiToken.To_Vector ((1 => (33, 3)));
            Add_Action (Table.States (55), 5, 58);
            Add_Error (Table.States (55));
            Table.States (55).Minimal_Complete_Action := (Shift, 5, 58);
            Table.States (56).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (56), 22, 59);
            Add_Error (Table.States (56));
            Table.States (56).Minimal_Complete_Action := (Shift, 22, 59);
            Table.States (57).Productions := WisiToken.To_Vector ((1 => (34, 3)));
            Add_Action (Table.States (57), (12, 18, 19), (34, 3), 3, rhs_3'Access, null);
            Table.States (57).Minimal_Complete_Action := (Reduce, 34, 3);
            Table.States (58).Productions := WisiToken.To_Vector ((1 => (33, 3)));
            Add_Action (Table.States (58), (12, 18, 19), (33, 3), 4, rhs_list_3'Access, null);
            Table.States (58).Minimal_Complete_Action := (Reduce, 33, 4);
            Table.States (59).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (59), 15, 60);
            Add_Error (Table.States (59));
            Table.States (59).Minimal_Complete_Action := (Shift, 15, 60);
            Table.States (60).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (60), 22, 61);
            Add_Error (Table.States (60));
            Table.States (60).Minimal_Complete_Action := (Shift, 22, 61);
            Table.States (61).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (61), (12, 18, 19), (33, 2), 6, rhs_list_2'Access, null);
            Table.States (61).Minimal_Complete_Action := (Reduce, 33, 6);
         end Subr_1;
      begin
         Subr_1;
      end;

      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         Language_Fixes,
         Language_Use_Minimal_Complete_Actions,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_1_Process_Main;
