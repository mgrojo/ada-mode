--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy
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
         Last_Terminal     => 34,
         First_Nonterminal => 35,
         Last_Nonterminal  => 48,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Cost_Limit  => 2147483647,
         Check_Limit => 4,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 76,
         First_Terminal    => 3,
         Last_Terminal     => 34,
         First_Nonterminal => 35,
         Last_Nonterminal  => 48);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Productions := WisiToken.To_Vector ((1 => (35, 0)));
            Add_Action (Table.States (0), 21, 1);
            Add_Action (Table.States (0), 31, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 36, 3);
            Add_Goto (Table.States (0), 41, 4);
            Add_Goto (Table.States (0), 47, 5);
            Add_Goto (Table.States (0), 48, 6);
            Set_Minimal_Action (Table.States (0).Minimal_Complete_Actions, (1 => (Shift, 31, 2)));
            Table.States (1).Productions := WisiToken.To_Vector (((36, 0), (36, 1), (36, 2), (36, 3), (36, 4), (36,
            5)));
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
            9), (Shift, 8, 12), (Shift, 31, 13)));
            Table.States (2).Productions := WisiToken.To_Vector ((1 => (41, 0)));
            Add_Action (Table.States (2), 13, 15);
            Add_Error (Table.States (2));
            Set_Minimal_Action (Table.States (2).Minimal_Complete_Actions, (1 => (Shift, 13, 15)));
            Table.States (3).Productions := WisiToken.To_Vector ((1 => (47, 0)));
            Add_Action (Table.States (3), (21, 31, 34), (47, 0), 1, null, null);
            Set_Minimal_Action (Table.States (3).Minimal_Complete_Actions, (1 => (Reduce, 47, 1)));
            Table.States (4).Productions := WisiToken.To_Vector ((1 => (47, 1)));
            Add_Action (Table.States (4), (21, 31, 34), (47, 1), 1, null, null);
            Set_Minimal_Action (Table.States (4).Minimal_Complete_Actions, (1 => (Reduce, 47, 1)));
            Table.States (5).Productions := WisiToken.To_Vector ((1 => (48, 0)));
            Add_Action (Table.States (5), (21, 31, 34), (48, 0), 1, null, null);
            Set_Minimal_Action (Table.States (5).Minimal_Complete_Actions, (1 => (Reduce, 48, 1)));
            Table.States (6).Productions := WisiToken.To_Vector (((35, 0), (48, 1)));
            Add_Action (Table.States (6), 21, 1);
            Add_Action (Table.States (6), 31, 2);
            Add_Action (Table.States (6), 34, Accept_It, (35, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 36, 3);
            Add_Goto (Table.States (6), 41, 4);
            Add_Goto (Table.States (6), 47, 16);
            Table.States (7).Productions := WisiToken.To_Vector ((1 => (36, 1)));
            Add_Action (Table.States (7), 31, 17);
            Add_Error (Table.States (7));
            Add_Goto (Table.States (7), 38, 18);
            Set_Minimal_Action (Table.States (7).Minimal_Complete_Actions, (1 => (Shift, 31, 17)));
            Table.States (8).Productions := WisiToken.To_Vector ((1 => (36, 5)));
            Add_Action (Table.States (8), 5, 19);
            Add_Error (Table.States (8));
            Set_Minimal_Action (Table.States (8).Minimal_Complete_Actions, (1 => (Shift, 5, 19)));
            Table.States (9).Productions := WisiToken.To_Vector ((1 => (36, 4)));
            Add_Action (Table.States (9), 31, 20);
            Add_Error (Table.States (9));
            Set_Minimal_Action (Table.States (9).Minimal_Complete_Actions, (1 => (Shift, 31, 20)));
            Table.States (10).Productions := WisiToken.To_Vector ((1 => (37, 0)));
            Add_Action (Table.States (10), (1 =>  31), (37, 0), 1, null, null);
            Set_Minimal_Action (Table.States (10).Minimal_Complete_Actions, (1 => (Reduce, 37, 1)));
            Table.States (11).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (11), 20, 21);
            Add_Error (Table.States (11));
            Set_Minimal_Action (Table.States (11).Minimal_Complete_Actions, (1 => (Shift, 20, 21)));
            Table.States (12).Productions := WisiToken.To_Vector (((37, 2), (37, 3)));
            Add_Action (Table.States (12), 20, 22);
            Add_Action (Table.States (12), 31, Reduce, (37, 3), 1, null, null);
            Add_Error (Table.States (12));
            Set_Minimal_Action (Table.States (12).Minimal_Complete_Actions, (1 => (Reduce, 37, 1)));
            Table.States (13).Productions := WisiToken.To_Vector (((36, 2), (36, 3)));
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
            Table.States (14).Productions := WisiToken.To_Vector ((1 => (36, 0)));
            Add_Action (Table.States (14), 31, 34);
            Add_Error (Table.States (14));
            Set_Minimal_Action (Table.States (14).Minimal_Complete_Actions, (1 => (Shift, 31, 34)));
            Table.States (15).Productions := WisiToken.To_Vector ((1 => (41, 0)));
            Add_Action (Table.States (15), 12, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 15, 35);
            Add_Action (Table.States (15), 16, 36);
            Add_Action (Table.States (15), 17, 37);
            Add_Action (Table.States (15), 18, 38);
            Add_Action (Table.States (15), 19, 39);
            Add_Action (Table.States (15), 20, 40);
            Add_Action (Table.States (15), 21, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 22, 41);
            Add_Action (Table.States (15), 23, 42);
            Add_Action (Table.States (15), 24, 43);
            Add_Action (Table.States (15), 25, 44);
            Add_Action (Table.States (15), 26, 45);
            Add_Action (Table.States (15), 27, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 29, 46);
            Add_Action (Table.States (15), 31, 47, (44, 0), 0, null, null);
            Add_Action (Table.States (15), 33, 48);
            Add_Action (Table.States (15), 34, Reduce, (44, 0), 0, null, null);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 43, 49);
            Add_Goto (Table.States (15), 44, 50);
            Add_Goto (Table.States (15), 45, 51);
            Add_Goto (Table.States (15), 46, 52);
            Set_Minimal_Action (Table.States (15).Minimal_Complete_Actions, (1 => (Reduce, 43, 0)));
            Table.States (16).Productions := WisiToken.To_Vector ((1 => (48, 1)));
            Add_Action (Table.States (16), (21, 31, 34), (48, 1), 2, null, null);
            Set_Minimal_Action (Table.States (16).Minimal_Complete_Actions, (1 => (Reduce, 48, 2)));
            Table.States (17).Productions := WisiToken.To_Vector ((1 => (38, 0)));
            Add_Action (Table.States (17), (9, 31), (38, 0), 1, null, null);
            Set_Minimal_Action (Table.States (17).Minimal_Complete_Actions, (1 => (Reduce, 38, 1)));
            Table.States (18).Productions := WisiToken.To_Vector (((36, 1), (38, 1)));
            Add_Action (Table.States (18), 9, 53);
            Add_Action (Table.States (18), 31, 54);
            Add_Error (Table.States (18));
            Set_Minimal_Action (Table.States (18).Minimal_Complete_Actions, (1 => (Shift, 9, 53)));
            Table.States (19).Productions := WisiToken.To_Vector ((1 => (36, 5)));
            Add_Action (Table.States (19), (21, 31, 34), (36, 5), 3, declaration_5'Access, null);
            Set_Minimal_Action (Table.States (19).Minimal_Complete_Actions, (1 => (Reduce, 36, 3)));
            Table.States (20).Productions := WisiToken.To_Vector ((1 => (36, 4)));
            Add_Action (Table.States (20), 15, 55);
            Add_Error (Table.States (20));
            Set_Minimal_Action (Table.States (20).Minimal_Complete_Actions, (1 => (Shift, 15, 55)));
            Table.States (21).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (21), 31, 56);
            Add_Error (Table.States (21));
            Set_Minimal_Action (Table.States (21).Minimal_Complete_Actions, (1 => (Shift, 31, 56)));
            Table.States (22).Productions := WisiToken.To_Vector ((1 => (37, 2)));
            Add_Action (Table.States (22), 31, 57);
            Add_Error (Table.States (22));
            Set_Minimal_Action (Table.States (22).Minimal_Complete_Actions, (1 => (Shift, 31, 57)));
            Table.States (23).Productions := WisiToken.To_Vector ((1 => (40, 8)));
            Add_Action (Table.States (23), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 8), 1, null, null);
            Set_Minimal_Action (Table.States (23).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (24).Productions := WisiToken.To_Vector ((1 => (40, 4)));
            Add_Action (Table.States (24), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 4), 1,
            declaration_item_4'Access, null);
            Set_Minimal_Action (Table.States (24).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (25).Productions := WisiToken.To_Vector ((1 => (40, 0)));
            Add_Action (Table.States (25), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 0), 1, null, null);
            Set_Minimal_Action (Table.States (25).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (26).Productions := WisiToken.To_Vector ((1 => (40, 2)));
            Add_Action (Table.States (26), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 2), 1, null, null);
            Set_Minimal_Action (Table.States (26).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (27).Productions := WisiToken.To_Vector ((1 => (40, 5)));
            Add_Action (Table.States (27), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 5), 1, null, null);
            Set_Minimal_Action (Table.States (27).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (28).Productions := WisiToken.To_Vector ((1 => (40, 3)));
            Add_Action (Table.States (28), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 3), 1, null, null);
            Set_Minimal_Action (Table.States (28).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (29).Productions := WisiToken.To_Vector ((1 => (40, 1)));
            Add_Action (Table.States (29), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 1), 1, null, null);
            Set_Minimal_Action (Table.States (29).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (30).Productions := WisiToken.To_Vector ((1 => (40, 6)));
            Add_Action (Table.States (30), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 6), 1, null, null);
            Set_Minimal_Action (Table.States (30).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (31).Productions := WisiToken.To_Vector ((1 => (40, 7)));
            Add_Action (Table.States (31), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (40, 7), 1, null, null);
            Set_Minimal_Action (Table.States (31).Minimal_Complete_Actions, (1 => (Reduce, 40, 1)));
            Table.States (32).Productions := WisiToken.To_Vector (((36, 2), (39, 1)));
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
            Add_Goto (Table.States (32), 40, 58);
            Set_Minimal_Action (Table.States (32).Minimal_Complete_Actions, (1 => (Reduce, 36, 3)));
            Table.States (33).Productions := WisiToken.To_Vector ((1 => (39, 0)));
            Add_Action (Table.States (33), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (39, 0), 1, null, null);
            Set_Minimal_Action (Table.States (33).Minimal_Complete_Actions, (1 => (Reduce, 39, 1)));
            Table.States (34).Productions := WisiToken.To_Vector ((1 => (36, 0)));
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
            Add_Goto (Table.States (34), 39, 59);
            Add_Goto (Table.States (34), 40, 33);
            Set_Minimal_Action (Table.States (34).Minimal_Complete_Actions, (1 => (Shift, 8, 23)));
            Table.States (35).Productions := WisiToken.To_Vector ((1 => (45, 0)));
            Add_Action (Table.States (35), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 0), 1, null, null);
            Set_Minimal_Action (Table.States (35).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (36).Productions := WisiToken.To_Vector ((1 => (45, 1)));
            Add_Action (Table.States (36), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 1), 1, null, null);
            Set_Minimal_Action (Table.States (36).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (37).Productions := WisiToken.To_Vector ((1 => (45, 3)));
            Add_Action (Table.States (37), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 3), 1, null, null);
            Set_Minimal_Action (Table.States (37).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (38).Productions := WisiToken.To_Vector ((1 => (45, 4)));
            Add_Action (Table.States (38), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 4), 1, null, null);
            Set_Minimal_Action (Table.States (38).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (39).Productions := WisiToken.To_Vector ((1 => (45, 5)));
            Add_Action (Table.States (39), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 5), 1, null, null);
            Set_Minimal_Action (Table.States (39).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (40).Productions := WisiToken.To_Vector ((1 => (45, 6)));
            Add_Action (Table.States (40), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 6), 1, null, null);
            Set_Minimal_Action (Table.States (40).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (41).Productions := WisiToken.To_Vector ((1 => (45, 7)));
            Add_Action (Table.States (41), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 7), 1, null, null);
            Set_Minimal_Action (Table.States (41).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (42).Productions := WisiToken.To_Vector ((1 => (45, 8)));
            Add_Action (Table.States (42), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 8), 1, null, null);
            Set_Minimal_Action (Table.States (42).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (43).Productions := WisiToken.To_Vector ((1 => (45, 9)));
            Add_Action (Table.States (43), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 9), 1, null, null);
            Set_Minimal_Action (Table.States (43).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (44).Productions := WisiToken.To_Vector ((1 => (45, 10)));
            Add_Action (Table.States (44), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 10), 1, null, null);
            Set_Minimal_Action (Table.States (44).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (45).Productions := WisiToken.To_Vector ((1 => (45, 11)));
            Add_Action (Table.States (45), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 11), 1, null, null);
            Set_Minimal_Action (Table.States (45).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (46).Productions := WisiToken.To_Vector ((1 => (45, 12)));
            Add_Action (Table.States (46), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 12), 1, null, null);
            Set_Minimal_Action (Table.States (46).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (47).Productions := WisiToken.To_Vector ((1 => (45, 2)));
            Add_Action (Table.States (47), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 2), 1, null, null);
            Set_Minimal_Action (Table.States (47).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (48).Productions := WisiToken.To_Vector ((1 => (45, 13)));
            Add_Action (Table.States (48), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (45, 13), 1, null, null);
            Set_Minimal_Action (Table.States (48).Minimal_Complete_Actions, (1 => (Reduce, 45, 1)));
            Table.States (49).Productions := WisiToken.To_Vector (((41, 0), (43, 1), (43, 2), (43, 3)));
            Add_Action (Table.States (49), 12, 60);
            Add_Action (Table.States (49), 21, 61, (42, 1), 0, null, null);
            Add_Action (Table.States (49), 27, 62);
            Add_Action (Table.States (49), 31, Reduce, (42, 1), 0, null, null);
            Add_Action (Table.States (49), 34, Reduce, (42, 1), 0, null, null);
            Add_Error (Table.States (49));
            Add_Goto (Table.States (49), 42, 63);
            Set_Minimal_Action (Table.States (49).Minimal_Complete_Actions, (1 => (Reduce, 42, 0)));
            Table.States (50).Productions := WisiToken.To_Vector ((1 => (43, 0)));
            Add_Action (Table.States (50), (12, 21, 27, 31, 34), (43, 0), 1, null, null);
            Set_Minimal_Action (Table.States (50).Minimal_Complete_Actions, (1 => (Reduce, 43, 1)));
            Table.States (51).Productions := WisiToken.To_Vector ((1 => (46, 0)));
            Add_Action (Table.States (51), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (46, 0), 1, null, null);
            Set_Minimal_Action (Table.States (51).Minimal_Complete_Actions, (1 => (Reduce, 46, 1)));
            Table.States (52).Productions := WisiToken.To_Vector (((44, 1), (44, 2), (44, 3), (46, 1)));
            Add_Action (Table.States (52), 11, 64);
            Add_Action (Table.States (52), 12, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (52), 15, 35);
            Add_Action (Table.States (52), 16, 36);
            Add_Action (Table.States (52), 17, 37);
            Add_Action (Table.States (52), 18, 38);
            Add_Action (Table.States (52), 19, 39);
            Add_Action (Table.States (52), 20, 40);
            Add_Action (Table.States (52), 21, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (52), 22, 41);
            Add_Action (Table.States (52), 23, 42);
            Add_Action (Table.States (52), 24, 43);
            Add_Action (Table.States (52), 25, 44);
            Add_Action (Table.States (52), 26, 45);
            Add_Action (Table.States (52), 27, Reduce, (44, 1), 1, null, null);
            Add_Action (Table.States (52), 29, 46);
            Add_Action (Table.States (52), 31, 47, (44, 1), 1, null, null);
            Add_Action (Table.States (52), 33, 48);
            Add_Action (Table.States (52), 34, Reduce, (44, 1), 1, null, null);
            Add_Error (Table.States (52));
            Add_Goto (Table.States (52), 45, 65);
            Set_Minimal_Action (Table.States (52).Minimal_Complete_Actions, (1 => (Reduce, 44, 1)));
            Table.States (53).Productions := WisiToken.To_Vector ((1 => (36, 1)));
            Add_Action (Table.States (53), (21, 31, 34), (36, 1), 4, null, null);
            Set_Minimal_Action (Table.States (53).Minimal_Complete_Actions, (1 => (Reduce, 36, 4)));
            Table.States (54).Productions := WisiToken.To_Vector ((1 => (38, 1)));
            Add_Action (Table.States (54), (9, 31), (38, 1), 2, null, null);
            Set_Minimal_Action (Table.States (54).Minimal_Complete_Actions, (1 => (Reduce, 38, 2)));
            Table.States (55).Productions := WisiToken.To_Vector ((1 => (36, 4)));
            Add_Action (Table.States (55), 31, 66);
            Add_Error (Table.States (55));
            Set_Minimal_Action (Table.States (55).Minimal_Complete_Actions, (1 => (Shift, 31, 66)));
            Table.States (56).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (56), 16, 67);
            Add_Error (Table.States (56));
            Set_Minimal_Action (Table.States (56).Minimal_Complete_Actions, (1 => (Shift, 16, 67)));
            Table.States (57).Productions := WisiToken.To_Vector ((1 => (37, 2)));
            Add_Action (Table.States (57), 16, 68);
            Add_Error (Table.States (57));
            Set_Minimal_Action (Table.States (57).Minimal_Complete_Actions, (1 => (Shift, 16, 68)));
            Table.States (58).Productions := WisiToken.To_Vector ((1 => (39, 1)));
            Add_Action (Table.States (58), (8, 10, 14, 15, 21, 28, 30, 31, 32, 33, 34), (39, 1), 2, null, null);
            Set_Minimal_Action (Table.States (58).Minimal_Complete_Actions, (1 => (Reduce, 39, 2)));
            Table.States (59).Productions := WisiToken.To_Vector (((36, 0), (39, 1)));
            Add_Action (Table.States (59), 8, 23);
            Add_Action (Table.States (59), 10, 24);
            Add_Action (Table.States (59), 14, 25);
            Add_Action (Table.States (59), 15, 26);
            Add_Action (Table.States (59), 21, Reduce, (36, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (59), 28, 27);
            Add_Action (Table.States (59), 30, 28);
            Add_Action (Table.States (59), 31, 29, (36, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (59), 32, 30);
            Add_Action (Table.States (59), 33, 31);
            Add_Action (Table.States (59), 34, Reduce, (36, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (59));
            Add_Goto (Table.States (59), 40, 58);
            Set_Minimal_Action (Table.States (59).Minimal_Complete_Actions, (1 => (Reduce, 36, 4)));
            Table.States (60).Productions := WisiToken.To_Vector ((1 => (43, 1)));
            Add_Action (Table.States (60), 12, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (60), 15, 35);
            Add_Action (Table.States (60), 16, 36);
            Add_Action (Table.States (60), 17, 37);
            Add_Action (Table.States (60), 18, 38);
            Add_Action (Table.States (60), 19, 39);
            Add_Action (Table.States (60), 20, 40);
            Add_Action (Table.States (60), 21, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (60), 22, 41);
            Add_Action (Table.States (60), 23, 42);
            Add_Action (Table.States (60), 24, 43);
            Add_Action (Table.States (60), 25, 44);
            Add_Action (Table.States (60), 26, 45);
            Add_Action (Table.States (60), 27, Reduce, (44, 0), 0, null, null);
            Add_Action (Table.States (60), 29, 46);
            Add_Action (Table.States (60), 31, 47, (44, 0), 0, null, null);
            Add_Action (Table.States (60), 33, 48);
            Add_Action (Table.States (60), 34, Reduce, (44, 0), 0, null, null);
            Add_Error (Table.States (60));
            Add_Goto (Table.States (60), 44, 69);
            Add_Goto (Table.States (60), 45, 51);
            Add_Goto (Table.States (60), 46, 52);
            Set_Minimal_Action (Table.States (60).Minimal_Complete_Actions, (1 => (Reduce, 44, 0)));
            Table.States (61).Productions := WisiToken.To_Vector (((43, 2), (43, 3)));
            Add_Action (Table.States (61), 4, 70);
            Add_Action (Table.States (61), 5, 71);
            Add_Error (Table.States (61));
            Set_Minimal_Action (Table.States (61).Minimal_Complete_Actions, ((Shift, 4, 70), (Shift, 5, 71)));
            Table.States (62).Productions := WisiToken.To_Vector ((1 => (42, 0)));
            Add_Action (Table.States (62), (21, 31, 34), (42, 0), 1, null, null);
            Set_Minimal_Action (Table.States (62).Minimal_Complete_Actions, (1 => (Reduce, 42, 1)));
            Table.States (63).Productions := WisiToken.To_Vector ((1 => (41, 0)));
            Add_Action (Table.States (63), (21, 31, 34), (41, 0), 4, nonterminal_0'Access, null);
            Set_Minimal_Action (Table.States (63).Minimal_Complete_Actions, (1 => (Reduce, 41, 4)));
            Table.States (64).Productions := WisiToken.To_Vector (((44, 2), (44, 3)));
            Add_Action (Table.States (64), 11, 72);
            Add_Action (Table.States (64), 12, Reduce, (44, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (64), 21, Reduce, (44, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (64), 27, Reduce, (44, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (64), 31, Reduce, (44, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (64), 34, Reduce, (44, 2), 2, rhs_2'Access, null);
            Add_Error (Table.States (64));
            Set_Minimal_Action (Table.States (64).Minimal_Complete_Actions, (1 => (Reduce, 44, 2)));
            Table.States (65).Productions := WisiToken.To_Vector ((1 => (46, 1)));
            Add_Action (Table.States (65), (11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31, 33,
            34), (46, 1), 2, null, null);
            Set_Minimal_Action (Table.States (65).Minimal_Complete_Actions, (1 => (Reduce, 46, 2)));
            Table.States (66).Productions := WisiToken.To_Vector ((1 => (36, 4)));
            Add_Action (Table.States (66), (21, 31, 34), (36, 4), 5, declaration_4'Access, null);
            Set_Minimal_Action (Table.States (66).Minimal_Complete_Actions, (1 => (Reduce, 36, 5)));
            Table.States (67).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (67), (1 =>  31), (37, 1), 4, token_keyword_non_grammar_1'Access, null);
            Set_Minimal_Action (Table.States (67).Minimal_Complete_Actions, (1 => (Reduce, 37, 4)));
            Table.States (68).Productions := WisiToken.To_Vector ((1 => (37, 2)));
            Add_Action (Table.States (68), (1 =>  31), (37, 2), 4, token_keyword_non_grammar_2'Access, null);
            Set_Minimal_Action (Table.States (68).Minimal_Complete_Actions, (1 => (Reduce, 37, 4)));
            Table.States (69).Productions := WisiToken.To_Vector ((1 => (43, 1)));
            Add_Action (Table.States (69), (12, 21, 27, 31, 34), (43, 1), 3, null, null);
            Set_Minimal_Action (Table.States (69).Minimal_Complete_Actions, (1 => (Reduce, 43, 3)));
            Table.States (70).Productions := WisiToken.To_Vector ((1 => (43, 3)));
            Add_Action (Table.States (70), 5, 73);
            Add_Error (Table.States (70));
            Set_Minimal_Action (Table.States (70).Minimal_Complete_Actions, (1 => (Shift, 5, 73)));
            Table.States (71).Productions := WisiToken.To_Vector ((1 => (43, 2)));
            Add_Action (Table.States (71), 31, 74);
            Add_Error (Table.States (71));
            Set_Minimal_Action (Table.States (71).Minimal_Complete_Actions, (1 => (Shift, 31, 74)));
            Table.States (72).Productions := WisiToken.To_Vector ((1 => (44, 3)));
            Add_Action (Table.States (72), (12, 21, 27, 31, 34), (44, 3), 3, rhs_3'Access, null);
            Set_Minimal_Action (Table.States (72).Minimal_Complete_Actions, (1 => (Reduce, 44, 3)));
            Table.States (73).Productions := WisiToken.To_Vector ((1 => (43, 3)));
            Add_Action (Table.States (73), (12, 21, 27, 31, 34), (43, 3), 4, rhs_list_3'Access, null);
            Set_Minimal_Action (Table.States (73).Minimal_Complete_Actions, (1 => (Reduce, 43, 4)));
            Table.States (74).Productions := WisiToken.To_Vector ((1 => (43, 2)));
            Add_Action (Table.States (74), 15, 75);
            Add_Error (Table.States (74));
            Set_Minimal_Action (Table.States (74).Minimal_Complete_Actions, (1 => (Shift, 15, 75)));
            Table.States (75).Productions := WisiToken.To_Vector ((1 => (43, 2)));
            Add_Action (Table.States (75), 31, 76);
            Add_Error (Table.States (75));
            Set_Minimal_Action (Table.States (75).Minimal_Complete_Actions, (1 => (Shift, 31, 76)));
            Table.States (76).Productions := WisiToken.To_Vector ((1 => (43, 2)));
            Add_Action (Table.States (76), (12, 21, 27, 31, 34), (43, 2), 6, rhs_list_2'Access, null);
            Set_Minimal_Action (Table.States (76).Minimal_Complete_Actions, (1 => (Reduce, 43, 6)));
         end Subr_1;
      begin
         Subr_1;
      end;

      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace),
         Table,
         Language_Fixes,
         Language_Use_Minimal_Complete_Actions,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_1_Process_Main;
