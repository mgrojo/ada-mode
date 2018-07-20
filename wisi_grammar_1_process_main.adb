--  generated parser support file.
--  command line: wisi-generate.exe  --generate LALR ADA_EMACS re2c PROCESS wisi_grammar_1.wy
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
with WisiToken.Productions;
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
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 37,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
         Push_Back =>
           (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
         Task_Count  => 0,
         Cost_Limit  => 2147483647,
         Check_Limit => 4,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      function Productions return WisiToken.Productions.Prod_Arrays.Vector
      is begin
         return Prods : WisiToken.Productions.Prod_Arrays.Vector do
            Prods.Set_First (26);
            Prods.Set_Last (37);
            Set_Production (Prods (26), 26, 0);
            Set_RHS (Prods (26), 0, (37, 25), null, null);
            Set_Production (Prods (27), 27, 5);
            Set_RHS (Prods (27), 0, (18, 28, 22, 30), declaration_0'Access, null);
            Set_RHS (Prods (27), 1, (18, 3, 29, 9), null, null);
            Set_RHS (Prods (27), 2, (18, 22, 30), declaration_2'Access, null);
            Set_RHS (Prods (27), 3, (18, 22), declaration_3'Access, null);
            Set_RHS (Prods (27), 4, (18, 5, 22, 15, 22), declaration_4'Access, null);
            Set_RHS (Prods (27), 5, (18, 4, 5), declaration_5'Access, null);
            Set_Production (Prods (28), 28, 2);
            Set_RHS (Prods (28), 0, (1 => 6), null, null);
            Set_RHS (Prods (28), 1, (7, 17, 22, 16), token_keyword_non_grammar_1'Access, null);
            Set_RHS (Prods (28), 2, (8, 17, 22, 16), token_keyword_non_grammar_2'Access, null);
            Set_Production (Prods (29), 29, 1);
            Set_RHS (Prods (29), 0, (1 => 22), null, null);
            Set_RHS (Prods (29), 1, (29, 22), null, null);
            Set_Production (Prods (30), 30, 1);
            Set_RHS (Prods (30), 0, (1 => 31), null, null);
            Set_RHS (Prods (30), 1, (30, 31), null, null);
            Set_Production (Prods (31), 31, 8);
            Set_RHS (Prods (31), 0, (1 => 14), null, null);
            Set_RHS (Prods (31), 1, (1 => 22), null, null);
            Set_RHS (Prods (31), 2, (1 => 15), null, null);
            Set_RHS (Prods (31), 3, (1 => 21), null, null);
            Set_RHS (Prods (31), 4, (1 => 10), declaration_item_4'Access, null);
            Set_RHS (Prods (31), 5, (1 => 20), null, null);
            Set_RHS (Prods (31), 6, (1 => 23), null, null);
            Set_RHS (Prods (31), 7, (1 => 24), null, null);
            Set_RHS (Prods (31), 8, (1 => 8), null, null);
            Set_Production (Prods (32), 32, 0);
            Set_RHS (Prods (32), 0, (22, 13, 33, 19), nonterminal_0'Access, null);
            Set_Production (Prods (33), 33, 3);
            Set_RHS (Prods (33), 0, (1 => 34), null, null);
            Set_RHS (Prods (33), 1, (33, 12, 34), null, null);
            Set_RHS (Prods (33), 2, (33, 18, 5, 22, 15, 22), rhs_list_2'Access, null);
            Set_RHS (Prods (33), 3, (33, 18, 4, 5), rhs_list_3'Access, null);
            Set_Production (Prods (34), 34, 3);
            Set_RHS (Prods (34), 0, (1 .. 0 => <>), null, null);
            Set_RHS (Prods (34), 1, (1 => 35), null, null);
            Set_RHS (Prods (34), 2, (35, 11), rhs_2'Access, null);
            Set_RHS (Prods (34), 3, (35, 11, 11), rhs_3'Access, null);
            Set_Production (Prods (35), 35, 1);
            Set_RHS (Prods (35), 0, (1 => 22), null, null);
            Set_RHS (Prods (35), 1, (35, 22), null, null);
            Set_Production (Prods (36), 36, 1);
            Set_RHS (Prods (36), 0, (1 => 27), null, null);
            Set_RHS (Prods (36), 1, (1 => 32), null, null);
            Set_Production (Prods (37), 37, 1);
            Set_RHS (Prods (37), 0, (1 => 36), null, null);
            Set_RHS (Prods (37), 1, (37, 36), null, null);
         end return;
      end Productions;

      function Minimal_Terminal_Sequences return WisiToken.Token_Sequence_Arrays.Vector
      is begin
         return Result : WisiToken.Token_Sequence_Arrays.Vector do
            Result.Set_First (26);
            Result.Set_Last (37);
            Set_Token_Sequence (Result (26), (18, 22, 25));
            Set_Token_Sequence (Result (27), (18, 22));
            Set_Token_Sequence (Result (28), (1 => 6));
            Set_Token_Sequence (Result (29), (1 => 22));
            Set_Token_Sequence (Result (30), (1 => 8));
            Set_Token_Sequence (Result (31), (1 => 8));
            Set_Token_Sequence (Result (32), (22, 13, 19));
            Set_Token_Sequence (Result (33), (1 .. 0 => <>));
            Set_Token_Sequence (Result (34), (1 .. 0 => <>));
            Set_Token_Sequence (Result (35), (1 => 22));
            Set_Token_Sequence (Result (36), (18, 22));
            Set_Token_Sequence (Result (37), (18, 22));
         end return;
      end Minimal_Terminal_Sequences;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 61,
         First_Terminal    => Trace.Descriptor.First_Terminal,
         Last_Terminal     => Trace.Descriptor.Last_Terminal,
         First_Nonterminal => Trace.Descriptor.First_Nonterminal,
         Last_Nonterminal  => Trace.Descriptor.Last_Nonterminal);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      Table.Productions := Productions;
      Table.Minimal_Terminal_Sequences := Minimal_Terminal_Sequences;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Productions := WisiToken.To_Vector ((1 => (26, 0)));
            Add_Action (Table.States (0), ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (Table.States (0), (1 => (32, 0)), 22, 2);
            Add_Error (Table.States (0));
            Add_Goto (Table.States (0), 27, 3);
            Add_Goto (Table.States (0), 32, 4);
            Add_Goto (Table.States (0), 36, 5);
            Add_Goto (Table.States (0), 37, 6);
            Table.States (1).Productions := WisiToken.To_Vector (((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27,
            5)));
            Add_Action (Table.States (1), (1 => (27, 1)), 3, 9);
            Add_Action (Table.States (1), (1 => (27, 5)), 4, 10);
            Add_Action (Table.States (1), (1 => (27, 4)), 5, 11);
            Add_Action (Table.States (1), (1 => (28, 0)), 6, 12);
            Add_Action (Table.States (1), (1 => (28, 1)), 7, 13);
            Add_Action (Table.States (1), (1 => (28, 2)), 8, 14);
            Add_Action (Table.States (1), ((27, 2), (27, 3)), 22, 15);
            Add_Error (Table.States (1));
            Add_Goto (Table.States (1), 28, 16);
            Table.States (2).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (2), (1 => (32, 0)), 13, 8);
            Add_Error (Table.States (2));
            Table.States (3).Productions := WisiToken.To_Vector ((1 => (36, 0)));
            Add_Action (Table.States (3), (18, 22, 25), (36, 0), 1, null, null);
            Table.States (4).Productions := WisiToken.To_Vector ((1 => (36, 1)));
            Add_Action (Table.States (4), (18, 22, 25), (36, 1), 1, null, null);
            Table.States (5).Productions := WisiToken.To_Vector ((1 => (37, 0)));
            Add_Action (Table.States (5), (18, 22, 25), (37, 0), 1, null, null);
            Table.States (6).Productions := WisiToken.To_Vector (((26, 0), (37, 1)));
            Add_Action (Table.States (6), ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (Table.States (6), (1 => (32, 0)), 22, 2);
            Add_Action (Table.States (6), 25, Accept_It, (26, 0), 1, null, null);
            Add_Error (Table.States (6));
            Add_Goto (Table.States (6), 27, 3);
            Add_Goto (Table.States (6), 32, 4);
            Add_Goto (Table.States (6), 36, 7);
            Table.States (7).Productions := WisiToken.To_Vector ((1 => (37, 1)));
            Add_Action (Table.States (7), (18, 22, 25), (37, 1), 2, null, null);
            Table.States (8).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (8), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (8), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (8), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (8), (1 => (35, 0)), 22, 35);
            Add_Error (Table.States (8));
            Add_Goto (Table.States (8), 33, 36);
            Add_Goto (Table.States (8), 34, 37);
            Add_Goto (Table.States (8), 35, 38);
            Table.States (9).Productions := WisiToken.To_Vector ((1 => (27, 1)));
            Add_Action (Table.States (9), (1 => (29, 0)), 22, 33);
            Add_Error (Table.States (9));
            Add_Goto (Table.States (9), 29, 34);
            Table.States (10).Productions := WisiToken.To_Vector ((1 => (27, 5)));
            Add_Action (Table.States (10), (1 => (27, 5)), 5, 32);
            Add_Error (Table.States (10));
            Table.States (11).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (11), (1 => (27, 4)), 22, 31);
            Add_Error (Table.States (11));
            Table.States (12).Productions := WisiToken.To_Vector ((1 => (28, 0)));
            Add_Action (Table.States (12), (1 =>  22), (28, 0), 1, null, null);
            Table.States (13).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (13), (1 => (28, 1)), 17, 30);
            Add_Error (Table.States (13));
            Table.States (14).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (14), (1 => (28, 2)), 17, 29);
            Add_Error (Table.States (14));
            Table.States (15).Productions := WisiToken.To_Vector (((27, 2), (27, 3)));
            Add_Action (Table.States (15), (1 => (31, 8)), 8, 18);
            Add_Action (Table.States (15), (1 => (31, 4)), 10, 19);
            Add_Action (Table.States (15), (1 => (31, 0)), 14, 20);
            Add_Action (Table.States (15), (1 => (31, 2)), 15, 21);
            Add_Action (Table.States (15), 18, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (15), (1 => (31, 5)), 20, 22);
            Add_Action (Table.States (15), (1 => (31, 3)), 21, 23);
            Add_Action (Table.States (15), (1 => (31, 1)), 22, 24, (27, 3), 2, declaration_3'Access, null);
            Add_Action (Table.States (15), (1 => (31, 6)), 23, 25);
            Add_Action (Table.States (15), (1 => (31, 7)), 24, 26);
            Add_Action (Table.States (15), 25, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Error (Table.States (15));
            Add_Goto (Table.States (15), 30, 27);
            Add_Goto (Table.States (15), 31, 28);
            Table.States (16).Productions := WisiToken.To_Vector ((1 => (27, 0)));
            Add_Action (Table.States (16), (1 => (27, 0)), 22, 17);
            Add_Error (Table.States (16));
            Table.States (17).Productions := WisiToken.To_Vector ((1 => (27, 0)));
            Add_Action (Table.States (17), (1 => (31, 8)), 8, 18);
            Add_Action (Table.States (17), (1 => (31, 4)), 10, 19);
            Add_Action (Table.States (17), (1 => (31, 0)), 14, 20);
            Add_Action (Table.States (17), (1 => (31, 2)), 15, 21);
            Add_Action (Table.States (17), (1 => (31, 5)), 20, 22);
            Add_Action (Table.States (17), (1 => (31, 3)), 21, 23);
            Add_Action (Table.States (17), (1 => (31, 1)), 22, 24);
            Add_Action (Table.States (17), (1 => (31, 6)), 23, 25);
            Add_Action (Table.States (17), (1 => (31, 7)), 24, 26);
            Add_Error (Table.States (17));
            Add_Goto (Table.States (17), 30, 50);
            Add_Goto (Table.States (17), 31, 28);
            Table.States (18).Productions := WisiToken.To_Vector ((1 => (31, 8)));
            Add_Action (Table.States (18), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 8), 1, null, null);
            Table.States (19).Productions := WisiToken.To_Vector ((1 => (31, 4)));
            Add_Action (Table.States (19), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 4), 1,
            declaration_item_4'Access, null);
            Table.States (20).Productions := WisiToken.To_Vector ((1 => (31, 0)));
            Add_Action (Table.States (20), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 0), 1, null, null);
            Table.States (21).Productions := WisiToken.To_Vector ((1 => (31, 2)));
            Add_Action (Table.States (21), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 2), 1, null, null);
            Table.States (22).Productions := WisiToken.To_Vector ((1 => (31, 5)));
            Add_Action (Table.States (22), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 5), 1, null, null);
            Table.States (23).Productions := WisiToken.To_Vector ((1 => (31, 3)));
            Add_Action (Table.States (23), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 3), 1, null, null);
            Table.States (24).Productions := WisiToken.To_Vector ((1 => (31, 1)));
            Add_Action (Table.States (24), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 1), 1, null, null);
            Table.States (25).Productions := WisiToken.To_Vector ((1 => (31, 6)));
            Add_Action (Table.States (25), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 6), 1, null, null);
            Table.States (26).Productions := WisiToken.To_Vector ((1 => (31, 7)));
            Add_Action (Table.States (26), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 7), 1, null, null);
            Table.States (27).Productions := WisiToken.To_Vector (((27, 2), (30, 1)));
            Add_Action (Table.States (27), (1 => (31, 8)), 8, 18);
            Add_Action (Table.States (27), (1 => (31, 4)), 10, 19);
            Add_Action (Table.States (27), (1 => (31, 0)), 14, 20);
            Add_Action (Table.States (27), (1 => (31, 2)), 15, 21);
            Add_Action (Table.States (27), 18, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (27), (1 => (31, 5)), 20, 22);
            Add_Action (Table.States (27), (1 => (31, 3)), 21, 23);
            Add_Action (Table.States (27), (1 => (31, 1)), 22, 24, (27, 2), 3, declaration_2'Access, null);
            Add_Action (Table.States (27), (1 => (31, 6)), 23, 25);
            Add_Action (Table.States (27), (1 => (31, 7)), 24, 26);
            Add_Action (Table.States (27), 25, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Error (Table.States (27));
            Add_Goto (Table.States (27), 31, 49);
            Table.States (28).Productions := WisiToken.To_Vector ((1 => (30, 0)));
            Add_Action (Table.States (28), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 0), 1, null, null);
            Table.States (29).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (29), (1 => (28, 2)), 22, 48);
            Add_Error (Table.States (29));
            Table.States (30).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (30), (1 => (28, 1)), 22, 47);
            Add_Error (Table.States (30));
            Table.States (31).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (31), (1 => (27, 4)), 15, 46);
            Add_Error (Table.States (31));
            Table.States (32).Productions := WisiToken.To_Vector ((1 => (27, 5)));
            Add_Action (Table.States (32), (18, 22, 25), (27, 5), 3, declaration_5'Access, null);
            Table.States (33).Productions := WisiToken.To_Vector ((1 => (29, 0)));
            Add_Action (Table.States (33), (9, 22), (29, 0), 1, null, null);
            Table.States (34).Productions := WisiToken.To_Vector (((27, 1), (29, 1)));
            Add_Action (Table.States (34), (1 => (27, 1)), 9, 44);
            Add_Action (Table.States (34), (1 => (29, 1)), 22, 45);
            Add_Error (Table.States (34));
            Table.States (35).Productions := WisiToken.To_Vector ((1 => (35, 0)));
            Add_Action (Table.States (35), (11, 12, 18, 19, 22), (35, 0), 1, null, null);
            Table.States (36).Productions := WisiToken.To_Vector (((32, 0), (33, 1), (33, 2), (33, 3)));
            Add_Action (Table.States (36), (1 => (33, 1)), 12, 41);
            Add_Action (Table.States (36), ((33, 2), (33, 3)), 18, 42);
            Add_Action (Table.States (36), (1 => (32, 0)), 19, 43);
            Add_Error (Table.States (36));
            Table.States (37).Productions := WisiToken.To_Vector ((1 => (33, 0)));
            Add_Action (Table.States (37), (12, 18, 19), (33, 0), 1, null, null);
            Table.States (38).Productions := WisiToken.To_Vector (((34, 1), (34, 2), (34, 3), (35, 1)));
            Add_Action (Table.States (38), ((34, 2), (34, 3)), 11, 39);
            Add_Action (Table.States (38), 12, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 18, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), 19, Reduce, (34, 1), 1, null, null);
            Add_Action (Table.States (38), (1 => (35, 1)), 22, 40);
            Add_Error (Table.States (38));
            Table.States (39).Productions := WisiToken.To_Vector (((34, 2), (34, 3)));
            Add_Action (Table.States (39), (1 => (34, 3)), 11, 57);
            Add_Action (Table.States (39), 12, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (39), 18, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Action (Table.States (39), 19, Reduce, (34, 2), 2, rhs_2'Access, null);
            Add_Error (Table.States (39));
            Table.States (40).Productions := WisiToken.To_Vector ((1 => (35, 1)));
            Add_Action (Table.States (40), (11, 12, 18, 19, 22), (35, 1), 2, null, null);
            Table.States (41).Productions := WisiToken.To_Vector ((1 => (33, 1)));
            Add_Action (Table.States (41), 12, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (41), 18, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (41), 19, Reduce, (34, 0), 0, null, null);
            Add_Action (Table.States (41), (1 => (35, 0)), 22, 35);
            Add_Error (Table.States (41));
            Add_Goto (Table.States (41), 34, 56);
            Add_Goto (Table.States (41), 35, 38);
            Table.States (42).Productions := WisiToken.To_Vector (((33, 2), (33, 3)));
            Add_Action (Table.States (42), (1 => (33, 3)), 4, 54);
            Add_Action (Table.States (42), (1 => (33, 2)), 5, 55);
            Add_Error (Table.States (42));
            Table.States (43).Productions := WisiToken.To_Vector ((1 => (32, 0)));
            Add_Action (Table.States (43), (18, 22, 25), (32, 0), 4, nonterminal_0'Access, null);
            Table.States (44).Productions := WisiToken.To_Vector ((1 => (27, 1)));
            Add_Action (Table.States (44), (18, 22, 25), (27, 1), 4, null, null);
            Table.States (45).Productions := WisiToken.To_Vector ((1 => (29, 1)));
            Add_Action (Table.States (45), (9, 22), (29, 1), 2, null, null);
            Table.States (46).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (46), (1 => (27, 4)), 22, 53);
            Add_Error (Table.States (46));
            Table.States (47).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (47), (1 => (28, 1)), 16, 52);
            Add_Error (Table.States (47));
            Table.States (48).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (48), (1 => (28, 2)), 16, 51);
            Add_Error (Table.States (48));
            Table.States (49).Productions := WisiToken.To_Vector ((1 => (30, 1)));
            Add_Action (Table.States (49), (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 1), 2, null, null);
            Table.States (50).Productions := WisiToken.To_Vector (((27, 0), (30, 1)));
            Add_Action (Table.States (50), (1 => (31, 8)), 8, 18);
            Add_Action (Table.States (50), (1 => (31, 4)), 10, 19);
            Add_Action (Table.States (50), (1 => (31, 0)), 14, 20);
            Add_Action (Table.States (50), (1 => (31, 2)), 15, 21);
            Add_Action (Table.States (50), 18, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (50), (1 => (31, 5)), 20, 22);
            Add_Action (Table.States (50), (1 => (31, 3)), 21, 23);
            Add_Action (Table.States (50), (1 => (31, 1)), 22, 24, (27, 0), 4, declaration_0'Access, null);
            Add_Action (Table.States (50), (1 => (31, 6)), 23, 25);
            Add_Action (Table.States (50), (1 => (31, 7)), 24, 26);
            Add_Action (Table.States (50), 25, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Error (Table.States (50));
            Add_Goto (Table.States (50), 31, 49);
            Table.States (51).Productions := WisiToken.To_Vector ((1 => (28, 2)));
            Add_Action (Table.States (51), (1 =>  22), (28, 2), 4, token_keyword_non_grammar_2'Access, null);
            Table.States (52).Productions := WisiToken.To_Vector ((1 => (28, 1)));
            Add_Action (Table.States (52), (1 =>  22), (28, 1), 4, token_keyword_non_grammar_1'Access, null);
            Table.States (53).Productions := WisiToken.To_Vector ((1 => (27, 4)));
            Add_Action (Table.States (53), (18, 22, 25), (27, 4), 5, declaration_4'Access, null);
            Table.States (54).Productions := WisiToken.To_Vector ((1 => (33, 3)));
            Add_Action (Table.States (54), (1 => (33, 3)), 5, 59);
            Add_Error (Table.States (54));
            Table.States (55).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (55), (1 => (33, 2)), 22, 58);
            Add_Error (Table.States (55));
            Table.States (56).Productions := WisiToken.To_Vector ((1 => (33, 1)));
            Add_Action (Table.States (56), (12, 18, 19), (33, 1), 3, null, null);
            Table.States (57).Productions := WisiToken.To_Vector ((1 => (34, 3)));
            Add_Action (Table.States (57), (12, 18, 19), (34, 3), 3, rhs_3'Access, null);
            Table.States (58).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (58), (1 => (33, 2)), 15, 60);
            Add_Error (Table.States (58));
            Table.States (59).Productions := WisiToken.To_Vector ((1 => (33, 3)));
            Add_Action (Table.States (59), (12, 18, 19), (33, 3), 4, rhs_list_3'Access, null);
            Table.States (60).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (60), (1 => (33, 2)), 22, 61);
            Add_Error (Table.States (60));
            Table.States (61).Productions := WisiToken.To_Vector ((1 => (33, 2)));
            Add_Action (Table.States (61), (12, 18, 19), (33, 2), 6, rhs_list_2'Access, null);
         end Subr_1;
      begin
         Subr_1;
      end;

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
         Terminate_Same_State => True);
   end Create_Parser;
end Wisi_Grammar_1_Process_Main;
