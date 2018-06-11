--  generated parser support file.
--  command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process wisi_grammar_1.wy
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

with Wisi_Grammar_1_Process_Actions; use Wisi_Grammar_1_Process_Actions;
with WisiToken.Lexer.re2c;
with wisi_grammar_1_re2c_c;
package body Wisi_Grammar_1_Process_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisi_grammar_1_re2c_c.New_Lexer,
      wisi_grammar_1_re2c_c.Free_Lexer,
      wisi_grammar_1_re2c_c.Reset_Lexer,
      wisi_grammar_1_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                       :    out WisiToken.LR.Parser.Parser;
      Language_Fixes               : in     WisiToken.LR.Parser.Language_Fixes_Access;
      Language_Constrain_Terminals : in     WisiToken.LR.Parser.Language_Constrain_Terminals_Access;
      Language_String_ID_Set       : in     WisiToken.LR.Parser.Language_String_ID_Set_Access;
      Algorithm                    : in     WisiToken.Generator_Algorithm_Type;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.LR;
      use all type WisiToken.Generator_Algorithm_Type;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 61,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);
      pragma Unreferenced (Algorithm);
   begin
      Table.McKenzie_Param :=
        (First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 37,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            1, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4),
         Push_Back =>
           (1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1),
         Task_Count  => 0,
         Cost_Limit  => 5,
         Check_Limit => 4,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 2147483647);


      Table.Productions.Set_Length (38);
      Set_Production (Table.Productions (1), 26, (37, 25));
      Set_Production (Table.Productions (2), 27, (18, 28, 22, 30));
      Set_Production (Table.Productions (3), 27, (18, 3, 29, 9));
      Set_Production (Table.Productions (4), 27, (18, 22, 30));
      Set_Production (Table.Productions (5), 27, (18, 22));
      Set_Production (Table.Productions (6), 27, (18, 5, 22, 15, 22));
      Set_Production (Table.Productions (7), 27, (18, 4, 5));
      Set_Production (Table.Productions (8), 28, (1 => 6));
      Set_Production (Table.Productions (9), 28, (7, 17, 22, 16));
      Set_Production (Table.Productions (10), 28, (8, 17, 22, 16));
      Set_Production (Table.Productions (11), 29, (1 => 22));
      Set_Production (Table.Productions (12), 29, (29, 22));
      Set_Production (Table.Productions (13), 30, (1 => 31));
      Set_Production (Table.Productions (14), 30, (30, 31));
      Set_Production (Table.Productions (15), 31, (1 => 14));
      Set_Production (Table.Productions (16), 31, (1 => 22));
      Set_Production (Table.Productions (17), 31, (1 => 15));
      Set_Production (Table.Productions (18), 31, (1 => 21));
      Set_Production (Table.Productions (19), 31, (1 => 10));
      Set_Production (Table.Productions (20), 31, (1 => 20));
      Set_Production (Table.Productions (21), 31, (1 => 23));
      Set_Production (Table.Productions (22), 31, (1 => 24));
      Set_Production (Table.Productions (23), 31, (1 => 8));
      Set_Production (Table.Productions (24), 32, (22, 13, 33, 19));
      Set_Production (Table.Productions (25), 33, (1 => 34));
      Set_Production (Table.Productions (26), 33, (33, 12, 34));
      Set_Production (Table.Productions (27), 33, (33, 18, 5, 22, 15, 22));
      Set_Production (Table.Productions (28), 33, (33, 18, 4, 5));
      Set_Production (Table.Productions (29), 34, (1 .. 0 => <>));
      Set_Production (Table.Productions (30), 34, (1 => 35));
      Set_Production (Table.Productions (31), 34, (35, 11));
      Set_Production (Table.Productions (32), 34, (35, 11, 11));
      Set_Production (Table.Productions (33), 35, (1 => 22));
      Set_Production (Table.Productions (34), 35, (35, 22));
      Set_Production (Table.Productions (35), 36, (1 => 27));
      Set_Production (Table.Productions (36), 36, (1 => 32));
      Set_Production (Table.Productions (37), 37, (1 => 36));
      Set_Production (Table.Productions (38), 37, (37, 36));

      Table.Minimal_Terminal_Sequences.Set_First (26);
      Table.Minimal_Terminal_Sequences.Set_Last (37);
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (26), (18, 22, 25));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (27), (18, 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (28), (1 => 6));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (29), (1 => 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (30), (1 => 8));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (31), (1 => 8));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (32), (22, 13, 19));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (33), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (34), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (35), (1 => 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (36), (18, 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (37), (18, 22));

      Table.States (0).Productions := WisiToken.To_Vector ((1 => 1));
      Add_Action (Table.States (0), (2, 3, 4, 5, 6, 7), 18, 1);
      Add_Action (Table.States (0), (1 => 24), 22, 2);
      Add_Error (Table.States (0));
      Add_Goto (Table.States (0), 35, 27, 3);
      Add_Goto (Table.States (0), 36, 32, 4);
      Add_Goto (Table.States (0), 37, 36, 5);
      Add_Goto (Table.States (0), 1, 37, 6);
      Table.States (1).Productions := WisiToken.To_Vector ((2, 3, 4, 5, 6, 7));
      Add_Action (Table.States (1), (1 => 3), 3, 9);
      Add_Action (Table.States (1), (1 => 7), 4, 10);
      Add_Action (Table.States (1), (1 => 6), 5, 11);
      Add_Action (Table.States (1), (1 => 8), 6, 12);
      Add_Action (Table.States (1), (1 => 9), 7, 13);
      Add_Action (Table.States (1), (1 => 10), 8, 14);
      Add_Action (Table.States (1), (4, 5), 22, 15);
      Add_Error (Table.States (1));
      Add_Goto (Table.States (1), 2, 28, 16);
      Table.States (2).Productions := WisiToken.To_Vector ((1 => 24));
      Add_Action (Table.States (2), (1 => 24), 13, 8);
      Add_Error (Table.States (2));
      Table.States (3).Productions := WisiToken.To_Vector ((1 => 35));
      Add_Action (Table.States (3), (18, 22, 25), 35, 36, 1, 0, null, null);
      Table.States (4).Productions := WisiToken.To_Vector ((1 => 36));
      Add_Action (Table.States (4), (18, 22, 25), 36, 36, 1, 1, null, null);
      Table.States (5).Productions := WisiToken.To_Vector ((1 => 37));
      Add_Action (Table.States (5), (18, 22, 25), 37, 37, 1, 0, null, null);
      Table.States (6).Productions := WisiToken.To_Vector ((1, 38));
      Add_Action (Table.States (6), (2, 3, 4, 5, 6, 7), 18, 1);
      Add_Action (Table.States (6), (1 => 24), 22, 2);
      Add_Action (Table.States (6), 25, Accept_It, 1, 26, 1, 0, null, null);
      Add_Error (Table.States (6));
      Add_Goto (Table.States (6), 35, 27, 3);
      Add_Goto (Table.States (6), 36, 32, 4);
      Add_Goto (Table.States (6), 38, 36, 7);
      Table.States (7).Productions := WisiToken.To_Vector ((1 => 38));
      Add_Action (Table.States (7), (18, 22, 25), 38, 37, 2, 1, null, null);
      Table.States (8).Productions := WisiToken.To_Vector ((1 => 24));
      Add_Action (Table.States (8), 12, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (8), 18, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (8), 19, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (8), (1 => 33), 22, 35);
      Add_Error (Table.States (8));
      Add_Goto (Table.States (8), 24, 33, 36);
      Add_Goto (Table.States (8), 25, 34, 37);
      Add_Goto (Table.States (8), 30, 35, 38);
      Table.States (9).Productions := WisiToken.To_Vector ((1 => 3));
      Add_Action (Table.States (9), (1 => 11), 22, 33);
      Add_Error (Table.States (9));
      Add_Goto (Table.States (9), 3, 29, 34);
      Table.States (10).Productions := WisiToken.To_Vector ((1 => 7));
      Add_Action (Table.States (10), (1 => 7), 5, 32);
      Add_Error (Table.States (10));
      Table.States (11).Productions := WisiToken.To_Vector ((1 => 6));
      Add_Action (Table.States (11), (1 => 6), 22, 31);
      Add_Error (Table.States (11));
      Table.States (12).Productions := WisiToken.To_Vector ((1 => 8));
      Add_Action (Table.States (12), (1 =>  22), 8, 28, 1, 0, null, null);
      Table.States (13).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (13), (1 => 9), 17, 30);
      Add_Error (Table.States (13));
      Table.States (14).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (14), (1 => 10), 17, 29);
      Add_Error (Table.States (14));
      Table.States (15).Productions := WisiToken.To_Vector ((4, 5));
      Add_Action (Table.States (15), (1 => 23), 8, 18);
      Add_Action (Table.States (15), (1 => 19), 10, 19);
      Add_Action (Table.States (15), (1 => 15), 14, 20);
      Add_Action (Table.States (15), (1 => 17), 15, 21);
      Add_Action (Table.States (15), 18, Reduce, 5, 27, 2, 3, declaration_3'Access, null);
      Add_Action (Table.States (15), (1 => 20), 20, 22);
      Add_Action (Table.States (15), (1 => 18), 21, 23);
      Add_Action (Table.States (15), (1 => 16), 22, 24, 5, 27, 2, 3, declaration_3'Access, null);
      Add_Action (Table.States (15), (1 => 21), 23, 25);
      Add_Action (Table.States (15), (1 => 22), 24, 26);
      Add_Action (Table.States (15), 25, Reduce, 5, 27, 2, 3, declaration_3'Access, null);
      Add_Error (Table.States (15));
      Add_Goto (Table.States (15), 4, 30, 27);
      Add_Goto (Table.States (15), 13, 31, 28);
      Table.States (16).Productions := WisiToken.To_Vector ((1 => 2));
      Add_Action (Table.States (16), (1 => 2), 22, 17);
      Add_Error (Table.States (16));
      Table.States (17).Productions := WisiToken.To_Vector ((1 => 2));
      Add_Action (Table.States (17), (1 => 23), 8, 18);
      Add_Action (Table.States (17), (1 => 19), 10, 19);
      Add_Action (Table.States (17), (1 => 15), 14, 20);
      Add_Action (Table.States (17), (1 => 17), 15, 21);
      Add_Action (Table.States (17), (1 => 20), 20, 22);
      Add_Action (Table.States (17), (1 => 18), 21, 23);
      Add_Action (Table.States (17), (1 => 16), 22, 24);
      Add_Action (Table.States (17), (1 => 21), 23, 25);
      Add_Action (Table.States (17), (1 => 22), 24, 26);
      Add_Error (Table.States (17));
      Add_Goto (Table.States (17), 2, 30, 50);
      Add_Goto (Table.States (17), 13, 31, 28);
      Table.States (18).Productions := WisiToken.To_Vector ((1 => 23));
      Add_Action (Table.States (18), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 23, 31, 1, 8, null, null);
      Table.States (19).Productions := WisiToken.To_Vector ((1 => 19));
      Add_Action (Table.States (19), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 19, 31, 1, 4,
        declaration_item_4'Access, null);
      Table.States (20).Productions := WisiToken.To_Vector ((1 => 15));
      Add_Action (Table.States (20), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 15, 31, 1, 0, null, null);
      Table.States (21).Productions := WisiToken.To_Vector ((1 => 17));
      Add_Action (Table.States (21), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 17, 31, 1, 2, null, null);
      Table.States (22).Productions := WisiToken.To_Vector ((1 => 20));
      Add_Action (Table.States (22), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 20, 31, 1, 5, null, null);
      Table.States (23).Productions := WisiToken.To_Vector ((1 => 18));
      Add_Action (Table.States (23), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 18, 31, 1, 3, null, null);
      Table.States (24).Productions := WisiToken.To_Vector ((1 => 16));
      Add_Action (Table.States (24), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 16, 31, 1, 1, null, null);
      Table.States (25).Productions := WisiToken.To_Vector ((1 => 21));
      Add_Action (Table.States (25), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 21, 31, 1, 6, null, null);
      Table.States (26).Productions := WisiToken.To_Vector ((1 => 22));
      Add_Action (Table.States (26), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 22, 31, 1, 7, null, null);
      Table.States (27).Productions := WisiToken.To_Vector ((4, 14));
      Add_Action (Table.States (27), (1 => 23), 8, 18);
      Add_Action (Table.States (27), (1 => 19), 10, 19);
      Add_Action (Table.States (27), (1 => 15), 14, 20);
      Add_Action (Table.States (27), (1 => 17), 15, 21);
      Add_Action (Table.States (27), 18, Reduce, 4, 27, 3, 2, declaration_2'Access, null);
      Add_Action (Table.States (27), (1 => 20), 20, 22);
      Add_Action (Table.States (27), (1 => 18), 21, 23);
      Add_Action (Table.States (27), (1 => 16), 22, 24, 4, 27, 3, 2, declaration_2'Access, null);
      Add_Action (Table.States (27), (1 => 21), 23, 25);
      Add_Action (Table.States (27), (1 => 22), 24, 26);
      Add_Action (Table.States (27), 25, Reduce, 4, 27, 3, 2, declaration_2'Access, null);
      Add_Error (Table.States (27));
      Add_Goto (Table.States (27), 14, 31, 49);
      Table.States (28).Productions := WisiToken.To_Vector ((1 => 13));
      Add_Action (Table.States (28), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 13, 30, 1, 0, null, null);
      Table.States (29).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (29), (1 => 10), 22, 48);
      Add_Error (Table.States (29));
      Table.States (30).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (30), (1 => 9), 22, 47);
      Add_Error (Table.States (30));
      Table.States (31).Productions := WisiToken.To_Vector ((1 => 6));
      Add_Action (Table.States (31), (1 => 6), 15, 46);
      Add_Error (Table.States (31));
      Table.States (32).Productions := WisiToken.To_Vector ((1 => 7));
      Add_Action (Table.States (32), (18, 22, 25), 7, 27, 3, 5, declaration_5'Access, null);
      Table.States (33).Productions := WisiToken.To_Vector ((1 => 11));
      Add_Action (Table.States (33), (9, 22), 11, 29, 1, 0, null, null);
      Table.States (34).Productions := WisiToken.To_Vector ((3, 12));
      Add_Action (Table.States (34), (1 => 3), 9, 44);
      Add_Action (Table.States (34), (1 => 12), 22, 45);
      Add_Error (Table.States (34));
      Table.States (35).Productions := WisiToken.To_Vector ((1 => 33));
      Add_Action (Table.States (35), (11, 12, 18, 19, 22), 33, 35, 1, 0, null, null);
      Table.States (36).Productions := WisiToken.To_Vector ((24, 26, 27, 28));
      Add_Action (Table.States (36), (1 => 26), 12, 41);
      Add_Action (Table.States (36), (27, 28), 18, 42);
      Add_Action (Table.States (36), (1 => 24), 19, 43);
      Add_Error (Table.States (36));
      Table.States (37).Productions := WisiToken.To_Vector ((1 => 25));
      Add_Action (Table.States (37), (12, 18, 19), 25, 33, 1, 0, null, null);
      Table.States (38).Productions := WisiToken.To_Vector ((30, 31, 32, 34));
      Add_Action (Table.States (38), (31, 32), 11, 39);
      Add_Action (Table.States (38), 12, Reduce, 30, 34, 1, 1, null, null);
      Add_Action (Table.States (38), 18, Reduce, 30, 34, 1, 1, null, null);
      Add_Action (Table.States (38), 19, Reduce, 30, 34, 1, 1, null, null);
      Add_Action (Table.States (38), (1 => 34), 22, 40);
      Add_Error (Table.States (38));
      Table.States (39).Productions := WisiToken.To_Vector ((31, 32));
      Add_Action (Table.States (39), (1 => 32), 11, 57);
      Add_Action (Table.States (39), 12, Reduce, 31, 34, 2, 2, rhs_2'Access, null);
      Add_Action (Table.States (39), 18, Reduce, 31, 34, 2, 2, rhs_2'Access, null);
      Add_Action (Table.States (39), 19, Reduce, 31, 34, 2, 2, rhs_2'Access, null);
      Add_Error (Table.States (39));
      Table.States (40).Productions := WisiToken.To_Vector ((1 => 34));
      Add_Action (Table.States (40), (11, 12, 18, 19, 22), 34, 35, 2, 1, null, null);
      Table.States (41).Productions := WisiToken.To_Vector ((1 => 26));
      Add_Action (Table.States (41), 12, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (41), 18, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (41), 19, Reduce, 29, 34, 0, 0, null, null);
      Add_Action (Table.States (41), (1 => 33), 22, 35);
      Add_Error (Table.States (41));
      Add_Goto (Table.States (41), 26, 34, 56);
      Add_Goto (Table.States (41), 30, 35, 38);
      Table.States (42).Productions := WisiToken.To_Vector ((27, 28));
      Add_Action (Table.States (42), (1 => 28), 4, 54);
      Add_Action (Table.States (42), (1 => 27), 5, 55);
      Add_Error (Table.States (42));
      Table.States (43).Productions := WisiToken.To_Vector ((1 => 24));
      Add_Action (Table.States (43), (18, 22, 25), 24, 32, 4, 0, nonterminal_0'Access, null);
      Table.States (44).Productions := WisiToken.To_Vector ((1 => 3));
      Add_Action (Table.States (44), (18, 22, 25), 3, 27, 4, 1, null, null);
      Table.States (45).Productions := WisiToken.To_Vector ((1 => 12));
      Add_Action (Table.States (45), (9, 22), 12, 29, 2, 1, null, null);
      Table.States (46).Productions := WisiToken.To_Vector ((1 => 6));
      Add_Action (Table.States (46), (1 => 6), 22, 53);
      Add_Error (Table.States (46));
      Table.States (47).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (47), (1 => 9), 16, 52);
      Add_Error (Table.States (47));
      Table.States (48).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (48), (1 => 10), 16, 51);
      Add_Error (Table.States (48));
      Table.States (49).Productions := WisiToken.To_Vector ((1 => 14));
      Add_Action (Table.States (49), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), 14, 30, 2, 1, null, null);
      Table.States (50).Productions := WisiToken.To_Vector ((2, 14));
      Add_Action (Table.States (50), (1 => 23), 8, 18);
      Add_Action (Table.States (50), (1 => 19), 10, 19);
      Add_Action (Table.States (50), (1 => 15), 14, 20);
      Add_Action (Table.States (50), (1 => 17), 15, 21);
      Add_Action (Table.States (50), 18, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (50), (1 => 20), 20, 22);
      Add_Action (Table.States (50), (1 => 18), 21, 23);
      Add_Action (Table.States (50), (1 => 16), 22, 24, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (50), (1 => 21), 23, 25);
      Add_Action (Table.States (50), (1 => 22), 24, 26);
      Add_Action (Table.States (50), 25, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Error (Table.States (50));
      Add_Goto (Table.States (50), 14, 31, 49);
      Table.States (51).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (51), (1 =>  22), 10, 28, 4, 2, token_keyword_non_grammar_2'Access, null);
      Table.States (52).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (52), (1 =>  22), 9, 28, 4, 1, token_keyword_non_grammar_1'Access, null);
      Table.States (53).Productions := WisiToken.To_Vector ((1 => 6));
      Add_Action (Table.States (53), (18, 22, 25), 6, 27, 5, 4, declaration_4'Access, null);
      Table.States (54).Productions := WisiToken.To_Vector ((1 => 28));
      Add_Action (Table.States (54), (1 => 28), 5, 59);
      Add_Error (Table.States (54));
      Table.States (55).Productions := WisiToken.To_Vector ((1 => 27));
      Add_Action (Table.States (55), (1 => 27), 22, 58);
      Add_Error (Table.States (55));
      Table.States (56).Productions := WisiToken.To_Vector ((1 => 26));
      Add_Action (Table.States (56), (12, 18, 19), 26, 33, 3, 1, null, null);
      Table.States (57).Productions := WisiToken.To_Vector ((1 => 32));
      Add_Action (Table.States (57), (12, 18, 19), 32, 34, 3, 3, rhs_3'Access, null);
      Table.States (58).Productions := WisiToken.To_Vector ((1 => 27));
      Add_Action (Table.States (58), (1 => 27), 15, 60);
      Add_Error (Table.States (58));
      Table.States (59).Productions := WisiToken.To_Vector ((1 => 28));
      Add_Action (Table.States (59), (12, 18, 19), 28, 33, 4, 3, rhs_list_3'Access, null);
      Table.States (60).Productions := WisiToken.To_Vector ((1 => 27));
      Add_Action (Table.States (60), (1 => 27), 22, 61);
      Add_Error (Table.States (60));
      Table.States (61).Productions := WisiToken.To_Vector ((1 => 27));
      Add_Action (Table.States (61), (12, 18, 19), 27, 33, 6, 2, rhs_list_2'Access, null);

      WisiToken.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace),
         Table,
         Language_Fixes,
         Language_Constrain_Terminals,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         First_Parser_Label   => 0,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisi_Grammar_1_Process_Main;
