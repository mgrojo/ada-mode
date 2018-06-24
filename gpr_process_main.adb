--  generated parser support file.
--  command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process gpr.wy
--

--  Copyright (C) 2013 - 2018 Free Software Foundation, Inc.

--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version.
--
--  This software is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with Gpr_Process_Actions; use Gpr_Process_Actions;
with WisiToken.Lexer.re2c;
with gpr_re2c_c;
package body Gpr_Process_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (gpr_re2c_c.New_Lexer,
      gpr_re2c_c.Free_Lexer,
      gpr_re2c_c.Reset_Lexer,
      gpr_re2c_c.Next_Token);

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
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 144,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);
      pragma Unreferenced (Algorithm);
   begin
      Table.McKenzie_Param :=
        (First_Terminal    => 3,
         Last_Terminal     => 37,
         First_Nonterminal => 38,
         Last_Nonterminal  => 71,
         Insert =>
           (4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4),
         Delete =>
           (4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 8, 8, 8, 8, 8,
            8, 8, 8, 8, 8, 8, 8, 8,
            8, 8, 8, 8, 8, 8, 8, 8,
            8, 8, 8, 8, 8, 8, 8, 8,
            8, 8, 8, 8, 8),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2),
         Task_Count  => 0,
         Cost_Limit  => 20,
         Check_Limit => 3,
         Check_Delta_Limit => 200,
         Enqueue_Limit => 10000);


      Table.Productions.Set_Length (73);
      Set_Production (Table.Productions (1), 38, (46, 37));
      Set_Production (Table.Productions (2), 39, (13, 68, 20));
      Set_Production (Table.Productions (3), 40, (11, 35, 23, 54, 33));
      Set_Production (Table.Productions (4), 40, (11, 35, 13, 52, 20, 23, 54, 33));
      Set_Production (Table.Productions (5), 40, (11, 9, 13, 36, 20, 23, 54, 33));
      Set_Production (Table.Productions (6), 41, (1 => 18));
      Set_Production (Table.Productions (7), 41, (1 => 57));
      Set_Production (Table.Productions (8), 42, (41, 32, 35));
      Set_Production (Table.Productions (9), 42, (41, 32, 35, 13, 36, 20));
      Set_Production (Table.Productions (10), 43, (5, 57, 12, 45, 7, 5, 33));
      Set_Production (Table.Productions (11), 44, (24, 53, 31, 51));
      Set_Production (Table.Productions (12), 45, (1 .. 0 => <>));
      Set_Production (Table.Productions (13), 45, (1 => 44));
      Set_Production (Table.Productions (14), 45, (45, 44));
      Set_Production (Table.Productions (15), 46, (48, 64, 62));
      Set_Production (Table.Productions (16), 47, (1 => 71));
      Set_Production (Table.Productions (17), 47, (47, 71));
      Set_Production (Table.Productions (18), 48, (1 .. 0 => <>));
      Set_Production (Table.Productions (19), 48, (1 => 47));
      Set_Production (Table.Productions (20), 49, (1 => 65));
      Set_Production (Table.Productions (21), 49, (1 => 70));
      Set_Production (Table.Productions (22), 49, (1 => 58));
      Set_Production (Table.Productions (23), 50, (1 => 49));
      Set_Production (Table.Productions (24), 50, (50, 49));
      Set_Production (Table.Productions (25), 51, (1 .. 0 => <>));
      Set_Production (Table.Productions (26), 51, (1 => 50));
      Set_Production (Table.Productions (27), 52, (1 .. 0 => <>));
      Set_Production (Table.Productions (28), 52, (1 => 36));
      Set_Production (Table.Productions (29), 52, (1 => 16));
      Set_Production (Table.Productions (30), 53, (1 => 52));
      Set_Production (Table.Productions (31), 53, (53, 34, 52));
      Set_Production (Table.Productions (32), 54, (1 => 69));
      Set_Production (Table.Productions (33), 54, (54, 26, 69));
      Set_Production (Table.Productions (34), 55, (9, 39));
      Set_Production (Table.Productions (35), 55, (10, 39));
      Set_Production (Table.Productions (36), 56, (1 .. 0 => <>));
      Set_Production (Table.Productions (37), 56, (1 => 35));
      Set_Production (Table.Productions (38), 57, (1 => 56));
      Set_Production (Table.Productions (39), 57, (57, 30, 35));
      Set_Production (Table.Productions (40), 58, (1 => 59));
      Set_Production (Table.Productions (41), 58, (1 => 60));
      Set_Production (Table.Productions (42), 58, (1 => 61));
      Set_Production (Table.Productions (43), 59, (17, 56, 12, 51, 7, 56, 33));
      Set_Production (Table.Productions (44), 60, (17, 56, 8, 57, 12, 51, 7, 56, 33));
      Set_Production (Table.Productions (45), 61, (17, 56, 19, 57, 33));
      Set_Production (Table.Productions (46), 62, (1 .. 0 => <>));
      Set_Production (Table.Productions (47), 62, (1 => 66));
      Set_Production (Table.Productions (48), 62, (1 => 63));
      Set_Production (Table.Productions (49), 63, (18, 56, 8, 36, 12, 51, 7, 56, 33));
      Set_Production (Table.Productions (50), 64, (1 .. 0 => <>));
      Set_Production (Table.Productions (51), 64, (1 => 3));
      Set_Production (Table.Productions (52), 64, (1 => 21));
      Set_Production (Table.Productions (53), 64, (1 => 4));
      Set_Production (Table.Productions (54), 64, (4, 14));
      Set_Production (Table.Productions (55), 64, (1 => 14));
      Set_Production (Table.Productions (56), 64, (1 => 6));
      Set_Production (Table.Productions (57), 65, (35, 28, 54, 33));
      Set_Production (Table.Productions (58), 65, (35, 27, 35, 28, 54, 33));
      Set_Production (Table.Productions (59), 65, (1 => 40));
      Set_Production (Table.Productions (60), 65, (1 => 43));
      Set_Production (Table.Productions (61), 65, (15, 33));
      Set_Production (Table.Productions (62), 66, (18, 56, 12, 51, 7, 56, 33));
      Set_Production (Table.Productions (63), 67, (1 => 36));
      Set_Production (Table.Productions (64), 67, (1 => 57));
      Set_Production (Table.Productions (65), 67, (1 => 55));
      Set_Production (Table.Productions (66), 67, (1 => 42));
      Set_Production (Table.Productions (67), 68, (1 => 54));
      Set_Production (Table.Productions (68), 68, (68, 29, 54));
      Set_Production (Table.Productions (69), 69, (1 => 67));
      Set_Production (Table.Productions (70), 69, (13, 20));
      Set_Production (Table.Productions (71), 69, (1 => 39));
      Set_Production (Table.Productions (72), 70, (22, 35, 12, 39, 33));
      Set_Production (Table.Productions (73), 71, (25, 68, 33));

      Table.Minimal_Terminal_Sequences.Set_First (38);
      Table.Minimal_Terminal_Sequences.Set_Last (71);
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (38), (1 => 37));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (39), (13, 20));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (40), (11, 35, 23, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (41), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (42), (32, 35));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (43), (5, 12, 7, 5, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (44), (24, 31));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (45), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (46), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (47), (25, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (48), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (49), (15, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (50), (15, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (51), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (52), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (53), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (54), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (55), (10, 13, 20));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (56), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (57), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (58), (17, 19, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (59), (17, 12, 7, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (60), (17, 8, 12, 7, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (61), (17, 19, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (62), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (63), (18, 8, 36, 12, 7, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (64), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (65), (15, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (66), (18, 12, 7, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (67), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (68), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (69), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (70), (22, 35, 12, 13, 20, 33));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (71), (25, 33));

      Table.States (0).Productions := WisiToken.To_Vector ((1 => 1));
      Add_Action (Table.States (0), 3, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), 4, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), 6, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), 14, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), 18, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), 21, Reduce, 18, 48, 0, 0, null, null);
      Add_Action (Table.States (0), (1 => 73), 25, 7);
      Add_Action (Table.States (0), 37, Reduce, 18, 48, 0, 0, null, null);
      Add_Error (Table.States (0));
      Add_Goto (Table.States (0), 1, 46, 8);
      Add_Goto (Table.States (0), 17, 47, 9);
      Add_Goto (Table.States (0), 15, 48, 10);
      Add_Goto (Table.States (0), 48, 63, 11);
      Add_Goto (Table.States (0), 47, 66, 12);
      Add_Goto (Table.States (0), 16, 71, 13);
      Table.States (1).Productions := WisiToken.To_Vector ((1 => 51));
      Add_Action (Table.States (1), (18, 37), 51, 64, 1, 1, null, null);
      Table.States (2).Productions := WisiToken.To_Vector ((53, 54));
      Add_Action (Table.States (2), (1 => 54), 14, 33);
      Add_Action (Table.States (2), 18, Reduce, 53, 64, 1, 3, null, null);
      Add_Action (Table.States (2), 37, Reduce, 53, 64, 1, 3, null, null);
      Add_Error (Table.States (2));
      Table.States (3).Productions := WisiToken.To_Vector ((1 => 56));
      Add_Action (Table.States (3), (18, 37), 56, 64, 1, 6, null, null);
      Table.States (4).Productions := WisiToken.To_Vector ((1 => 55));
      Add_Action (Table.States (4), (18, 37), 55, 64, 1, 5, null, null);
      Table.States (5).Productions := WisiToken.To_Vector ((49, 62));
      Add_Action (Table.States (5), 8, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (5), 12, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (5), (1 => 37), 35, 20);
      Add_Error (Table.States (5));
      Add_Goto (Table.States (5), 49, 56, 32);
      Table.States (6).Productions := WisiToken.To_Vector ((1 => 52));
      Add_Action (Table.States (6), (18, 37), 52, 64, 1, 2, null, null);
      Table.States (7).Productions := WisiToken.To_Vector ((1 => 73));
      Add_Action (Table.States (7), (1 => 34), 9, 16);
      Add_Action (Table.States (7), (1 => 35), 10, 17);
      Add_Action (Table.States (7), (2, 70), 13, 18);
      Add_Action (Table.States (7), (1 => 6), 18, 19);
      Add_Action (Table.States (7), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (7), 29, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (7), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (7), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (7), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (7), (1 => 37), 35, 20);
      Add_Action (Table.States (7), (1 => 63), 36, 21);
      Add_Error (Table.States (7));
      Add_Goto (Table.States (7), 71, 39, 22);
      Add_Goto (Table.States (7), 8, 41, 23);
      Add_Goto (Table.States (7), 66, 42, 24);
      Add_Goto (Table.States (7), 33, 54, 25);
      Add_Goto (Table.States (7), 65, 55, 26);
      Add_Goto (Table.States (7), 38, 56, 27);
      Add_Goto (Table.States (7), 7, 57, 28);
      Add_Goto (Table.States (7), 69, 67, 29);
      Add_Goto (Table.States (7), 68, 68, 30);
      Add_Goto (Table.States (7), 32, 69, 31);
      Table.States (8).Productions := WisiToken.To_Vector ((1 => 1));
      Add_Action (Table.States (8), 37, Accept_It, 1, 38, 1, 0, null, null);
      Add_Error (Table.States (8));
      Table.States (9).Productions := WisiToken.To_Vector ((17, 19));
      Add_Action (Table.States (9), 3, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), 4, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), 6, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), 14, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), 18, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), 21, Reduce, 19, 48, 1, 1, null, null);
      Add_Action (Table.States (9), (1 => 73), 25, 7);
      Add_Action (Table.States (9), 37, Reduce, 19, 48, 1, 1, null, null);
      Add_Error (Table.States (9));
      Add_Goto (Table.States (9), 17, 71, 15);
      Table.States (10).Productions := WisiToken.To_Vector ((1 => 15));
      Add_Action (Table.States (10), (1 => 51), 3, 1);
      Add_Action (Table.States (10), (53, 54), 4, 2);
      Add_Action (Table.States (10), (1 => 56), 6, 3);
      Add_Action (Table.States (10), (1 => 55), 14, 4);
      Add_Action (Table.States (10), 18, Reduce, 50, 64, 0, 0, null, null);
      Add_Action (Table.States (10), (1 => 52), 21, 6);
      Add_Action (Table.States (10), 37, Reduce, 50, 64, 0, 0, null, null);
      Add_Error (Table.States (10));
      Add_Goto (Table.States (10), 15, 64, 14);
      Table.States (11).Productions := WisiToken.To_Vector ((1 => 48));
      Add_Action (Table.States (11), (1 =>  37), 48, 62, 1, 2, null, null);
      Table.States (12).Productions := WisiToken.To_Vector ((1 => 47));
      Add_Action (Table.States (12), (1 =>  37), 47, 62, 1, 1, null, null);
      Table.States (13).Productions := WisiToken.To_Vector ((1 => 16));
      Add_Action (Table.States (13), (3, 4, 6, 14, 18, 21, 25, 37), 16, 47, 1, 0, null, null);
      Table.States (14).Productions := WisiToken.To_Vector ((1 => 15));
      Add_Action (Table.States (14), (49, 62), 18, 5);
      Add_Action (Table.States (14), 37, Reduce, 46, 62, 0, 0, null, null);
      Add_Error (Table.States (14));
      Add_Goto (Table.States (14), 15, 62, 46);
      Add_Goto (Table.States (14), 48, 63, 11);
      Add_Goto (Table.States (14), 47, 66, 12);
      Table.States (15).Productions := WisiToken.To_Vector ((1 => 17));
      Add_Action (Table.States (15), (3, 4, 6, 14, 18, 21, 25, 37), 17, 47, 2, 1, null, null);
      Table.States (16).Productions := WisiToken.To_Vector ((1 => 34));
      Add_Action (Table.States (16), (1 => 2), 13, 43);
      Add_Error (Table.States (16));
      Add_Goto (Table.States (16), 34, 39, 45);
      Table.States (17).Productions := WisiToken.To_Vector ((1 => 35));
      Add_Action (Table.States (17), (1 => 2), 13, 43);
      Add_Error (Table.States (17));
      Add_Goto (Table.States (17), 35, 39, 44);
      Table.States (18).Productions := WisiToken.To_Vector ((2, 70));
      Add_Action (Table.States (18), (1 => 34), 9, 16);
      Add_Action (Table.States (18), (1 => 35), 10, 17);
      Add_Action (Table.States (18), (2, 70), 13, 18);
      Add_Action (Table.States (18), (1 => 6), 18, 19);
      Add_Action (Table.States (18), (1 => 70), 20, 41, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (18), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (18), 29, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (18), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (18), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (18), (1 => 37), 35, 20);
      Add_Action (Table.States (18), (1 => 63), 36, 21);
      Add_Error (Table.States (18));
      Add_Goto (Table.States (18), 71, 39, 22);
      Add_Goto (Table.States (18), 8, 41, 23);
      Add_Goto (Table.States (18), 66, 42, 24);
      Add_Goto (Table.States (18), 33, 54, 25);
      Add_Goto (Table.States (18), 65, 55, 26);
      Add_Goto (Table.States (18), 38, 56, 27);
      Add_Goto (Table.States (18), 7, 57, 28);
      Add_Goto (Table.States (18), 69, 67, 29);
      Add_Goto (Table.States (18), 2, 68, 42);
      Add_Goto (Table.States (18), 32, 69, 31);
      Table.States (19).Productions := WisiToken.To_Vector ((1 => 6));
      Add_Action (Table.States (19), (1 =>  32), 6, 41, 1, 0, null, null);
      Table.States (20).Productions := WisiToken.To_Vector ((1 => 37));
      Add_Action (Table.States (20), (8, 12, 19, 20, 26, 29, 30, 32, 33), 37, 56, 1, 1, null,
        identifier_opt_1_check'Access);
      Table.States (21).Productions := WisiToken.To_Vector ((1 => 63));
      Add_Action (Table.States (21), (20, 26, 29, 33), 63, 67, 1, 0, null, null);
      Table.States (22).Productions := WisiToken.To_Vector ((1 => 71));
      Add_Action (Table.States (22), (20, 26, 29, 33), 71, 69, 1, 2, null, null);
      Table.States (23).Productions := WisiToken.To_Vector ((8, 9));
      Add_Action (Table.States (23), (8, 9), 32, 40);
      Add_Error (Table.States (23));
      Table.States (24).Productions := WisiToken.To_Vector ((1 => 66));
      Add_Action (Table.States (24), (20, 26, 29, 33), 66, 67, 1, 3, null, null);
      Table.States (25).Productions := WisiToken.To_Vector ((33, 67));
      Add_Action (Table.States (25), 20, Reduce, 67, 68, 1, 0, null, null);
      Add_Action (Table.States (25), (1 => 33), 26, 39);
      Add_Action (Table.States (25), 29, Reduce, 67, 68, 1, 0, null, null);
      Add_Action (Table.States (25), 33, Reduce, 67, 68, 1, 0, null, null);
      Add_Error (Table.States (25));
      Table.States (26).Productions := WisiToken.To_Vector ((1 => 65));
      Add_Action (Table.States (26), (20, 26, 29, 33), 65, 67, 1, 2, null, null);
      Table.States (27).Productions := WisiToken.To_Vector ((1 => 38));
      Add_Action (Table.States (27), (12, 20, 26, 29, 30, 32, 33), 38, 57, 1, 0, null, null);
      Table.States (28).Productions := WisiToken.To_Vector ((7, 39, 64));
      Add_Action (Table.States (28), 20, Reduce, 64, 67, 1, 1, null, null);
      Add_Action (Table.States (28), 26, Reduce, 64, 67, 1, 1, null, null);
      Add_Action (Table.States (28), 29, Reduce, 64, 67, 1, 1, null, null);
      Add_Action (Table.States (28), (1 => 39), 30, 38);
      Add_Action (Table.States (28), 32, Reduce, 7, 41, 1, 1, null, null);
      Add_Action (Table.States (28), 33, Reduce, 64, 67, 1, 1, null, null);
      Add_Error (Table.States (28));
      Table.States (29).Productions := WisiToken.To_Vector ((1 => 69));
      Add_Action (Table.States (29), (20, 26, 29, 33), 69, 69, 1, 0, null, null);
      Table.States (30).Productions := WisiToken.To_Vector ((68, 73));
      Add_Action (Table.States (30), (1 => 68), 29, 36);
      Add_Action (Table.States (30), (1 => 73), 33, 37);
      Add_Error (Table.States (30));
      Table.States (31).Productions := WisiToken.To_Vector ((1 => 32));
      Add_Action (Table.States (31), (20, 26, 29, 33), 32, 54, 1, 0, null, null);
      Table.States (32).Productions := WisiToken.To_Vector ((49, 62));
      Add_Action (Table.States (32), (1 => 49), 8, 34);
      Add_Action (Table.States (32), (1 => 62), 12, 35);
      Add_Error (Table.States (32));
      Table.States (33).Productions := WisiToken.To_Vector ((1 => 54));
      Add_Action (Table.States (33), (18, 37), 54, 64, 2, 4, null, null);
      Table.States (34).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (34), (1 => 49), 36, 69);
      Add_Error (Table.States (34));
      Table.States (35).Productions := WisiToken.To_Vector ((1 => 62));
      Add_Action (Table.States (35), (1 => 10), 5, 52);
      Add_Action (Table.States (35), 7, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (35), (3, 4, 5), 11, 53);
      Add_Action (Table.States (35), (1 => 61), 15, 54);
      Add_Action (Table.States (35), (43, 44, 45), 17, 55);
      Add_Action (Table.States (35), (1 => 72), 22, 56);
      Add_Action (Table.States (35), (57, 58), 35, 57);
      Add_Error (Table.States (35));
      Add_Goto (Table.States (35), 59, 40, 58);
      Add_Goto (Table.States (35), 60, 43, 59);
      Add_Goto (Table.States (35), 23, 49, 60);
      Add_Goto (Table.States (35), 24, 50, 61);
      Add_Goto (Table.States (35), 62, 51, 62);
      Add_Goto (Table.States (35), 22, 58, 63);
      Add_Goto (Table.States (35), 40, 59, 64);
      Add_Goto (Table.States (35), 41, 60, 65);
      Add_Goto (Table.States (35), 42, 61, 66);
      Add_Goto (Table.States (35), 20, 65, 67);
      Add_Goto (Table.States (35), 21, 70, 68);
      Table.States (36).Productions := WisiToken.To_Vector ((1 => 68));
      Add_Action (Table.States (36), (1 => 34), 9, 16);
      Add_Action (Table.States (36), (1 => 35), 10, 17);
      Add_Action (Table.States (36), (2, 70), 13, 18);
      Add_Action (Table.States (36), (1 => 6), 18, 19);
      Add_Action (Table.States (36), 20, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), 29, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (36), (1 => 37), 35, 20);
      Add_Action (Table.States (36), (1 => 63), 36, 21);
      Add_Error (Table.States (36));
      Add_Goto (Table.States (36), 71, 39, 22);
      Add_Goto (Table.States (36), 8, 41, 23);
      Add_Goto (Table.States (36), 66, 42, 24);
      Add_Goto (Table.States (36), 33, 54, 51);
      Add_Goto (Table.States (36), 65, 55, 26);
      Add_Goto (Table.States (36), 38, 56, 27);
      Add_Goto (Table.States (36), 7, 57, 28);
      Add_Goto (Table.States (36), 69, 67, 29);
      Add_Goto (Table.States (36), 32, 69, 31);
      Table.States (37).Productions := WisiToken.To_Vector ((1 => 73));
      Add_Action (Table.States (37), (3, 4, 6, 14, 18, 21, 25, 37), 73, 71, 3, 0, null, null);
      Table.States (38).Productions := WisiToken.To_Vector ((1 => 39));
      Add_Action (Table.States (38), (1 => 39), 35, 50);
      Add_Error (Table.States (38));
      Table.States (39).Productions := WisiToken.To_Vector ((1 => 33));
      Add_Action (Table.States (39), (1 => 34), 9, 16);
      Add_Action (Table.States (39), (1 => 35), 10, 17);
      Add_Action (Table.States (39), (2, 70), 13, 18);
      Add_Action (Table.States (39), (1 => 6), 18, 19);
      Add_Action (Table.States (39), 20, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), 29, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (39), (1 => 37), 35, 20);
      Add_Action (Table.States (39), (1 => 63), 36, 21);
      Add_Error (Table.States (39));
      Add_Goto (Table.States (39), 71, 39, 22);
      Add_Goto (Table.States (39), 8, 41, 23);
      Add_Goto (Table.States (39), 66, 42, 24);
      Add_Goto (Table.States (39), 65, 55, 26);
      Add_Goto (Table.States (39), 38, 56, 27);
      Add_Goto (Table.States (39), 7, 57, 28);
      Add_Goto (Table.States (39), 69, 67, 29);
      Add_Goto (Table.States (39), 33, 69, 49);
      Table.States (40).Productions := WisiToken.To_Vector ((8, 9));
      Add_Action (Table.States (40), (8, 9), 35, 48);
      Add_Error (Table.States (40));
      Table.States (41).Productions := WisiToken.To_Vector ((1 => 70));
      Add_Action (Table.States (41), (20, 26, 29, 33), 70, 69, 2, 1, null, null);
      Table.States (42).Productions := WisiToken.To_Vector ((2, 68));
      Add_Action (Table.States (42), (1 => 2), 20, 47);
      Add_Action (Table.States (42), (1 => 68), 29, 36);
      Add_Error (Table.States (42));
      Table.States (43).Productions := WisiToken.To_Vector ((1 => 2));
      Add_Action (Table.States (43), (1 => 34), 9, 16);
      Add_Action (Table.States (43), (1 => 35), 10, 17);
      Add_Action (Table.States (43), (2, 70), 13, 18);
      Add_Action (Table.States (43), (1 => 6), 18, 19);
      Add_Action (Table.States (43), 20, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (43), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (43), 29, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (43), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (43), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (43), (1 => 37), 35, 20);
      Add_Action (Table.States (43), (1 => 63), 36, 21);
      Add_Error (Table.States (43));
      Add_Goto (Table.States (43), 71, 39, 22);
      Add_Goto (Table.States (43), 8, 41, 23);
      Add_Goto (Table.States (43), 66, 42, 24);
      Add_Goto (Table.States (43), 33, 54, 25);
      Add_Goto (Table.States (43), 65, 55, 26);
      Add_Goto (Table.States (43), 38, 56, 27);
      Add_Goto (Table.States (43), 7, 57, 28);
      Add_Goto (Table.States (43), 69, 67, 29);
      Add_Goto (Table.States (43), 2, 68, 42);
      Add_Goto (Table.States (43), 32, 69, 31);
      Table.States (44).Productions := WisiToken.To_Vector ((1 => 35));
      Add_Action (Table.States (44), (20, 26, 29, 33), 35, 55, 2, 1, null, null);
      Table.States (45).Productions := WisiToken.To_Vector ((1 => 34));
      Add_Action (Table.States (45), (20, 26, 29, 33), 34, 55, 2, 0, null, null);
      Table.States (46).Productions := WisiToken.To_Vector ((1 => 15));
      Add_Action (Table.States (46), (1 =>  37), 15, 46, 3, 0, null, null);
      Table.States (47).Productions := WisiToken.To_Vector ((1 => 2));
      Add_Action (Table.States (47), (20, 26, 29, 33), 2, 39, 3, 0, aggregate_g_0'Access, null);
      Table.States (48).Productions := WisiToken.To_Vector ((8, 9));
      Add_Action (Table.States (48), (1 => 9), 13, 81);
      Add_Action (Table.States (48), 20, Reduce, 8, 42, 3, 0, null, null);
      Add_Action (Table.States (48), 26, Reduce, 8, 42, 3, 0, null, null);
      Add_Action (Table.States (48), 29, Reduce, 8, 42, 3, 0, null, null);
      Add_Action (Table.States (48), 33, Reduce, 8, 42, 3, 0, null, null);
      Add_Error (Table.States (48));
      Table.States (49).Productions := WisiToken.To_Vector ((1 => 33));
      Add_Action (Table.States (49), (20, 26, 29, 33), 33, 54, 3, 1, null, null);
      Table.States (50).Productions := WisiToken.To_Vector ((1 => 39));
      Add_Action (Table.States (50), (12, 20, 26, 29, 30, 32, 33), 39, 57, 3, 1, null, null);
      Table.States (51).Productions := WisiToken.To_Vector ((33, 68));
      Add_Action (Table.States (51), 20, Reduce, 68, 68, 3, 1, null, null);
      Add_Action (Table.States (51), (1 => 33), 26, 39);
      Add_Action (Table.States (51), 29, Reduce, 68, 68, 3, 1, null, null);
      Add_Action (Table.States (51), 33, Reduce, 68, 68, 3, 1, null, null);
      Add_Error (Table.States (51));
      Table.States (52).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (52), 12, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (52), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (52), (1 => 37), 35, 20);
      Add_Error (Table.States (52));
      Add_Goto (Table.States (52), 38, 56, 27);
      Add_Goto (Table.States (52), 10, 57, 80);
      Table.States (53).Productions := WisiToken.To_Vector ((3, 4, 5));
      Add_Action (Table.States (53), (1 => 5), 9, 78);
      Add_Action (Table.States (53), (3, 4), 35, 79);
      Add_Error (Table.States (53));
      Table.States (54).Productions := WisiToken.To_Vector ((1 => 61));
      Add_Action (Table.States (54), (1 => 61), 33, 77);
      Add_Error (Table.States (54));
      Table.States (55).Productions := WisiToken.To_Vector ((43, 44, 45));
      Add_Action (Table.States (55), 8, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (55), 12, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (55), 19, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (55), (1 => 37), 35, 20);
      Add_Error (Table.States (55));
      Add_Goto (Table.States (55), 43, 56, 76);
      Table.States (56).Productions := WisiToken.To_Vector ((1 => 72));
      Add_Action (Table.States (56), (1 => 72), 35, 75);
      Add_Error (Table.States (56));
      Table.States (57).Productions := WisiToken.To_Vector ((57, 58));
      Add_Action (Table.States (57), (1 => 58), 27, 73);
      Add_Action (Table.States (57), (1 => 57), 28, 74);
      Add_Error (Table.States (57));
      Table.States (58).Productions := WisiToken.To_Vector ((1 => 59));
      Add_Action (Table.States (58), (5, 7, 11, 15, 17, 22, 24, 35), 59, 65, 1, 2, null, null);
      Table.States (59).Productions := WisiToken.To_Vector ((1 => 60));
      Add_Action (Table.States (59), (5, 7, 11, 15, 17, 22, 24, 35), 60, 65, 1, 3, null, null);
      Table.States (60).Productions := WisiToken.To_Vector ((1 => 23));
      Add_Action (Table.States (60), (5, 7, 11, 15, 17, 22, 24, 35), 23, 50, 1, 0, null, null);
      Table.States (61).Productions := WisiToken.To_Vector ((24, 26));
      Add_Action (Table.States (61), (1 => 10), 5, 52);
      Add_Action (Table.States (61), 7, Reduce, 26, 51, 1, 1, null, null);
      Add_Action (Table.States (61), (3, 4, 5), 11, 53);
      Add_Action (Table.States (61), (1 => 61), 15, 54);
      Add_Action (Table.States (61), (43, 44, 45), 17, 55);
      Add_Action (Table.States (61), (1 => 72), 22, 56);
      Add_Action (Table.States (61), 24, Reduce, 26, 51, 1, 1, null, null);
      Add_Action (Table.States (61), (57, 58), 35, 57);
      Add_Error (Table.States (61));
      Add_Goto (Table.States (61), 59, 40, 58);
      Add_Goto (Table.States (61), 60, 43, 59);
      Add_Goto (Table.States (61), 24, 49, 72);
      Add_Goto (Table.States (61), 22, 58, 63);
      Add_Goto (Table.States (61), 40, 59, 64);
      Add_Goto (Table.States (61), 41, 60, 65);
      Add_Goto (Table.States (61), 42, 61, 66);
      Add_Goto (Table.States (61), 20, 65, 67);
      Add_Goto (Table.States (61), 21, 70, 68);
      Table.States (62).Productions := WisiToken.To_Vector ((1 => 62));
      Add_Action (Table.States (62), (1 => 62), 7, 71);
      Add_Error (Table.States (62));
      Table.States (63).Productions := WisiToken.To_Vector ((1 => 22));
      Add_Action (Table.States (63), (5, 7, 11, 15, 17, 22, 24, 35), 22, 49, 1, 2, null, null);
      Table.States (64).Productions := WisiToken.To_Vector ((1 => 40));
      Add_Action (Table.States (64), (5, 7, 11, 15, 17, 22, 24, 35), 40, 58, 1, 0, null, null);
      Table.States (65).Productions := WisiToken.To_Vector ((1 => 41));
      Add_Action (Table.States (65), (5, 7, 11, 15, 17, 22, 24, 35), 41, 58, 1, 1, null, null);
      Table.States (66).Productions := WisiToken.To_Vector ((1 => 42));
      Add_Action (Table.States (66), (5, 7, 11, 15, 17, 22, 24, 35), 42, 58, 1, 2, null, null);
      Table.States (67).Productions := WisiToken.To_Vector ((1 => 20));
      Add_Action (Table.States (67), (5, 7, 11, 15, 17, 22, 24, 35), 20, 49, 1, 0, null, null);
      Table.States (68).Productions := WisiToken.To_Vector ((1 => 21));
      Add_Action (Table.States (68), (5, 7, 11, 15, 17, 22, 24, 35), 21, 49, 1, 1, null, null);
      Table.States (69).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (69), (1 => 49), 12, 70);
      Add_Error (Table.States (69));
      Table.States (70).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (70), (1 => 10), 5, 52);
      Add_Action (Table.States (70), 7, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (70), (3, 4, 5), 11, 53);
      Add_Action (Table.States (70), (1 => 61), 15, 54);
      Add_Action (Table.States (70), (43, 44, 45), 17, 55);
      Add_Action (Table.States (70), (1 => 72), 22, 56);
      Add_Action (Table.States (70), (57, 58), 35, 57);
      Add_Error (Table.States (70));
      Add_Goto (Table.States (70), 59, 40, 58);
      Add_Goto (Table.States (70), 60, 43, 59);
      Add_Goto (Table.States (70), 23, 49, 60);
      Add_Goto (Table.States (70), 24, 50, 61);
      Add_Goto (Table.States (70), 49, 51, 94);
      Add_Goto (Table.States (70), 22, 58, 63);
      Add_Goto (Table.States (70), 40, 59, 64);
      Add_Goto (Table.States (70), 41, 60, 65);
      Add_Goto (Table.States (70), 42, 61, 66);
      Add_Goto (Table.States (70), 20, 65, 67);
      Add_Goto (Table.States (70), 21, 70, 68);
      Table.States (71).Productions := WisiToken.To_Vector ((1 => 62));
      Add_Action (Table.States (71), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (71), (1 => 37), 35, 20);
      Add_Error (Table.States (71));
      Add_Goto (Table.States (71), 62, 56, 93);
      Table.States (72).Productions := WisiToken.To_Vector ((1 => 24));
      Add_Action (Table.States (72), (5, 7, 11, 15, 17, 22, 24, 35), 24, 50, 2, 1, null, null);
      Table.States (73).Productions := WisiToken.To_Vector ((1 => 58));
      Add_Action (Table.States (73), (1 => 58), 35, 92);
      Add_Error (Table.States (73));
      Table.States (74).Productions := WisiToken.To_Vector ((1 => 57));
      Add_Action (Table.States (74), (1 => 34), 9, 16);
      Add_Action (Table.States (74), (1 => 35), 10, 17);
      Add_Action (Table.States (74), (2, 70), 13, 18);
      Add_Action (Table.States (74), (1 => 6), 18, 19);
      Add_Action (Table.States (74), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (74), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (74), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (74), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (74), (1 => 37), 35, 20);
      Add_Action (Table.States (74), (1 => 63), 36, 21);
      Add_Error (Table.States (74));
      Add_Goto (Table.States (74), 71, 39, 22);
      Add_Goto (Table.States (74), 8, 41, 23);
      Add_Goto (Table.States (74), 66, 42, 24);
      Add_Goto (Table.States (74), 33, 54, 91);
      Add_Goto (Table.States (74), 65, 55, 26);
      Add_Goto (Table.States (74), 38, 56, 27);
      Add_Goto (Table.States (74), 7, 57, 28);
      Add_Goto (Table.States (74), 69, 67, 29);
      Add_Goto (Table.States (74), 32, 69, 31);
      Table.States (75).Productions := WisiToken.To_Vector ((1 => 72));
      Add_Action (Table.States (75), (1 => 72), 12, 90);
      Add_Error (Table.States (75));
      Table.States (76).Productions := WisiToken.To_Vector ((43, 44, 45));
      Add_Action (Table.States (76), (1 => 44), 8, 87);
      Add_Action (Table.States (76), (1 => 43), 12, 88);
      Add_Action (Table.States (76), (1 => 45), 19, 89);
      Add_Error (Table.States (76));
      Table.States (77).Productions := WisiToken.To_Vector ((1 => 61));
      Add_Action (Table.States (77), (5, 7, 11, 15, 17, 22, 24, 35), 61, 65, 2, 4, simple_declarative_item_4'Access,
        null);
      Table.States (78).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (78), (1 => 5), 13, 86);
      Add_Error (Table.States (78));
      Table.States (79).Productions := WisiToken.To_Vector ((3, 4));
      Add_Action (Table.States (79), (1 => 4), 13, 84);
      Add_Action (Table.States (79), (1 => 3), 23, 85);
      Add_Error (Table.States (79));
      Table.States (80).Productions := WisiToken.To_Vector ((10, 39));
      Add_Action (Table.States (80), (1 => 10), 12, 83);
      Add_Action (Table.States (80), (1 => 39), 30, 38);
      Add_Error (Table.States (80));
      Table.States (81).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (81), (1 => 9), 36, 82);
      Add_Error (Table.States (81));
      Table.States (82).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (82), (1 => 9), 20, 111);
      Add_Error (Table.States (82));
      Table.States (83).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (83), 7, Reduce, 12, 45, 0, 0, null, null);
      Add_Action (Table.States (83), (1 => 11), 24, 108, 12, 45, 0, 0, null, null);
      Add_Error (Table.States (83));
      Add_Goto (Table.States (83), 13, 44, 109);
      Add_Goto (Table.States (83), 10, 45, 110);
      Table.States (84).Productions := WisiToken.To_Vector ((1 => 4));
      Add_Action (Table.States (84), (1 => 29), 16, 105);
      Add_Action (Table.States (84), 20, Reduce, 27, 52, 0, 0, null, null);
      Add_Action (Table.States (84), (1 => 28), 36, 106);
      Add_Error (Table.States (84));
      Add_Goto (Table.States (84), 4, 52, 107);
      Table.States (85).Productions := WisiToken.To_Vector ((1 => 3));
      Add_Action (Table.States (85), (1 => 34), 9, 16);
      Add_Action (Table.States (85), (1 => 35), 10, 17);
      Add_Action (Table.States (85), (2, 70), 13, 18);
      Add_Action (Table.States (85), (1 => 6), 18, 19);
      Add_Action (Table.States (85), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (85), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (85), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (85), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (85), (1 => 37), 35, 20);
      Add_Action (Table.States (85), (1 => 63), 36, 21);
      Add_Error (Table.States (85));
      Add_Goto (Table.States (85), 71, 39, 22);
      Add_Goto (Table.States (85), 8, 41, 23);
      Add_Goto (Table.States (85), 66, 42, 24);
      Add_Goto (Table.States (85), 3, 54, 104);
      Add_Goto (Table.States (85), 65, 55, 26);
      Add_Goto (Table.States (85), 38, 56, 27);
      Add_Goto (Table.States (85), 7, 57, 28);
      Add_Goto (Table.States (85), 69, 67, 29);
      Add_Goto (Table.States (85), 32, 69, 31);
      Table.States (86).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (86), (1 => 5), 36, 103);
      Add_Error (Table.States (86));
      Table.States (87).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (87), 12, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (87), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (87), (1 => 37), 35, 20);
      Add_Error (Table.States (87));
      Add_Goto (Table.States (87), 38, 56, 27);
      Add_Goto (Table.States (87), 39, 57, 102);
      Table.States (88).Productions := WisiToken.To_Vector ((1 => 43));
      Add_Action (Table.States (88), (1 => 10), 5, 52);
      Add_Action (Table.States (88), 7, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (88), (3, 4, 5), 11, 53);
      Add_Action (Table.States (88), (1 => 61), 15, 54);
      Add_Action (Table.States (88), (43, 44, 45), 17, 55);
      Add_Action (Table.States (88), (1 => 72), 22, 56);
      Add_Action (Table.States (88), (57, 58), 35, 57);
      Add_Error (Table.States (88));
      Add_Goto (Table.States (88), 59, 40, 58);
      Add_Goto (Table.States (88), 60, 43, 59);
      Add_Goto (Table.States (88), 23, 49, 60);
      Add_Goto (Table.States (88), 24, 50, 61);
      Add_Goto (Table.States (88), 43, 51, 101);
      Add_Goto (Table.States (88), 22, 58, 63);
      Add_Goto (Table.States (88), 40, 59, 64);
      Add_Goto (Table.States (88), 41, 60, 65);
      Add_Goto (Table.States (88), 42, 61, 66);
      Add_Goto (Table.States (88), 20, 65, 67);
      Add_Goto (Table.States (88), 21, 70, 68);
      Table.States (89).Productions := WisiToken.To_Vector ((1 => 45));
      Add_Action (Table.States (89), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (89), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (89), (1 => 37), 35, 20);
      Add_Error (Table.States (89));
      Add_Goto (Table.States (89), 38, 56, 27);
      Add_Goto (Table.States (89), 39, 57, 100);
      Table.States (90).Productions := WisiToken.To_Vector ((1 => 72));
      Add_Action (Table.States (90), (1 => 2), 13, 43);
      Add_Error (Table.States (90));
      Add_Goto (Table.States (90), 72, 39, 99);
      Table.States (91).Productions := WisiToken.To_Vector ((33, 57));
      Add_Action (Table.States (91), (1 => 33), 26, 39);
      Add_Action (Table.States (91), (1 => 57), 33, 98);
      Add_Error (Table.States (91));
      Table.States (92).Productions := WisiToken.To_Vector ((1 => 58));
      Add_Action (Table.States (92), (1 => 58), 28, 97);
      Add_Error (Table.States (92));
      Table.States (93).Productions := WisiToken.To_Vector ((1 => 62));
      Add_Action (Table.States (93), (1 => 62), 33, 96);
      Add_Error (Table.States (93));
      Table.States (94).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (94), (1 => 49), 7, 95);
      Add_Error (Table.States (94));
      Table.States (95).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (95), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (95), (1 => 37), 35, 20);
      Add_Error (Table.States (95));
      Add_Goto (Table.States (95), 49, 56, 124);
      Table.States (96).Productions := WisiToken.To_Vector ((1 => 62));
      Add_Action (Table.States (96), (1 =>  37), 62, 66, 7, 0, simple_project_declaration_0'Access,
        simple_project_declaration_0_check'Access);
      Table.States (97).Productions := WisiToken.To_Vector ((1 => 58));
      Add_Action (Table.States (97), (1 => 34), 9, 16);
      Add_Action (Table.States (97), (1 => 35), 10, 17);
      Add_Action (Table.States (97), (2, 70), 13, 18);
      Add_Action (Table.States (97), (1 => 6), 18, 19);
      Add_Action (Table.States (97), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (97), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (97), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (97), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (97), (1 => 37), 35, 20);
      Add_Action (Table.States (97), (1 => 63), 36, 21);
      Add_Error (Table.States (97));
      Add_Goto (Table.States (97), 71, 39, 22);
      Add_Goto (Table.States (97), 8, 41, 23);
      Add_Goto (Table.States (97), 66, 42, 24);
      Add_Goto (Table.States (97), 33, 54, 123);
      Add_Goto (Table.States (97), 65, 55, 26);
      Add_Goto (Table.States (97), 38, 56, 27);
      Add_Goto (Table.States (97), 7, 57, 28);
      Add_Goto (Table.States (97), 69, 67, 29);
      Add_Goto (Table.States (97), 32, 69, 31);
      Table.States (98).Productions := WisiToken.To_Vector ((1 => 57));
      Add_Action (Table.States (98), (5, 7, 11, 15, 17, 22, 24, 35), 57, 65, 4, 0, simple_declarative_item_0'Access,
        null);
      Table.States (99).Productions := WisiToken.To_Vector ((1 => 72));
      Add_Action (Table.States (99), (1 => 72), 33, 122);
      Add_Error (Table.States (99));
      Table.States (100).Productions := WisiToken.To_Vector ((39, 45));
      Add_Action (Table.States (100), (1 => 39), 30, 38);
      Add_Action (Table.States (100), (1 => 45), 33, 121);
      Add_Error (Table.States (100));
      Table.States (101).Productions := WisiToken.To_Vector ((1 => 43));
      Add_Action (Table.States (101), (1 => 43), 7, 120);
      Add_Error (Table.States (101));
      Table.States (102).Productions := WisiToken.To_Vector ((39, 44));
      Add_Action (Table.States (102), (1 => 44), 12, 119);
      Add_Action (Table.States (102), (1 => 39), 30, 38);
      Add_Error (Table.States (102));
      Table.States (103).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (103), (1 => 5), 20, 118);
      Add_Error (Table.States (103));
      Table.States (104).Productions := WisiToken.To_Vector ((3, 33));
      Add_Action (Table.States (104), (1 => 33), 26, 39);
      Add_Action (Table.States (104), (1 => 3), 33, 117);
      Add_Error (Table.States (104));
      Table.States (105).Productions := WisiToken.To_Vector ((1 => 29));
      Add_Action (Table.States (105), (20, 31, 34), 29, 52, 1, 2, null, null);
      Table.States (106).Productions := WisiToken.To_Vector ((1 => 28));
      Add_Action (Table.States (106), (20, 31, 34), 28, 52, 1, 1, null, null);
      Table.States (107).Productions := WisiToken.To_Vector ((1 => 4));
      Add_Action (Table.States (107), (1 => 4), 20, 116);
      Add_Error (Table.States (107));
      Table.States (108).Productions := WisiToken.To_Vector ((1 => 11));
      Add_Action (Table.States (108), (1 => 29), 16, 105);
      Add_Action (Table.States (108), 31, Reduce, 27, 52, 0, 0, null, null);
      Add_Action (Table.States (108), 34, Reduce, 27, 52, 0, 0, null, null);
      Add_Action (Table.States (108), (1 => 28), 36, 106);
      Add_Error (Table.States (108));
      Add_Goto (Table.States (108), 30, 52, 114);
      Add_Goto (Table.States (108), 11, 53, 115);
      Table.States (109).Productions := WisiToken.To_Vector ((1 => 13));
      Add_Action (Table.States (109), (7, 24), 13, 45, 1, 1, null, null);
      Table.States (110).Productions := WisiToken.To_Vector ((10, 14));
      Add_Action (Table.States (110), (1 => 10), 7, 112);
      Add_Action (Table.States (110), (1 => 11), 24, 108);
      Add_Error (Table.States (110));
      Add_Goto (Table.States (110), 14, 44, 113);
      Table.States (111).Productions := WisiToken.To_Vector ((1 => 9));
      Add_Action (Table.States (111), (20, 26, 29, 33), 9, 42, 6, 1, null, null);
      Table.States (112).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (112), (1 => 10), 5, 133);
      Add_Error (Table.States (112));
      Table.States (113).Productions := WisiToken.To_Vector ((1 => 14));
      Add_Action (Table.States (113), (7, 24), 14, 45, 2, 2, null, null);
      Table.States (114).Productions := WisiToken.To_Vector ((1 => 30));
      Add_Action (Table.States (114), (31, 34), 30, 53, 1, 0, null, null);
      Table.States (115).Productions := WisiToken.To_Vector ((11, 31));
      Add_Action (Table.States (115), (1 => 11), 31, 131);
      Add_Action (Table.States (115), (1 => 31), 34, 132);
      Add_Error (Table.States (115));
      Table.States (116).Productions := WisiToken.To_Vector ((1 => 4));
      Add_Action (Table.States (116), (1 => 4), 23, 130);
      Add_Error (Table.States (116));
      Table.States (117).Productions := WisiToken.To_Vector ((1 => 3));
      Add_Action (Table.States (117), (5, 7, 11, 15, 17, 22, 24, 35), 3, 40, 5, 0, attribute_declaration_0'Access,
        null);
      Table.States (118).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (118), (1 => 5), 23, 129);
      Add_Error (Table.States (118));
      Table.States (119).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (119), (1 => 10), 5, 52);
      Add_Action (Table.States (119), 7, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (119), (3, 4, 5), 11, 53);
      Add_Action (Table.States (119), (1 => 61), 15, 54);
      Add_Action (Table.States (119), (43, 44, 45), 17, 55);
      Add_Action (Table.States (119), (1 => 72), 22, 56);
      Add_Action (Table.States (119), (57, 58), 35, 57);
      Add_Error (Table.States (119));
      Add_Goto (Table.States (119), 59, 40, 58);
      Add_Goto (Table.States (119), 60, 43, 59);
      Add_Goto (Table.States (119), 23, 49, 60);
      Add_Goto (Table.States (119), 24, 50, 61);
      Add_Goto (Table.States (119), 44, 51, 128);
      Add_Goto (Table.States (119), 22, 58, 63);
      Add_Goto (Table.States (119), 40, 59, 64);
      Add_Goto (Table.States (119), 41, 60, 65);
      Add_Goto (Table.States (119), 42, 61, 66);
      Add_Goto (Table.States (119), 20, 65, 67);
      Add_Goto (Table.States (119), 21, 70, 68);
      Table.States (120).Productions := WisiToken.To_Vector ((1 => 43));
      Add_Action (Table.States (120), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (120), (1 => 37), 35, 20);
      Add_Error (Table.States (120));
      Add_Goto (Table.States (120), 43, 56, 127);
      Table.States (121).Productions := WisiToken.To_Vector ((1 => 45));
      Add_Action (Table.States (121), (5, 7, 11, 15, 17, 22, 24, 35), 45, 61, 5, 0, package_renaming_0'Access, null);
      Table.States (122).Productions := WisiToken.To_Vector ((1 => 72));
      Add_Action (Table.States (122), (5, 7, 11, 15, 17, 22, 24, 35), 72, 70, 5, 0, typed_string_declaration_0'Access
        , null);
      Table.States (123).Productions := WisiToken.To_Vector ((33, 58));
      Add_Action (Table.States (123), (1 => 33), 26, 39);
      Add_Action (Table.States (123), (1 => 58), 33, 126);
      Add_Error (Table.States (123));
      Table.States (124).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (124), (1 => 49), 33, 125);
      Add_Error (Table.States (124));
      Table.States (125).Productions := WisiToken.To_Vector ((1 => 49));
      Add_Action (Table.States (125), (1 =>  37), 49, 63, 9, 0, project_extension_0'Access,
        project_extension_0_check'Access);
      Table.States (126).Productions := WisiToken.To_Vector ((1 => 58));
      Add_Action (Table.States (126), (5, 7, 11, 15, 17, 22, 24, 35), 58, 65, 6, 1, simple_declarative_item_1'Access
        , null);
      Table.States (127).Productions := WisiToken.To_Vector ((1 => 43));
      Add_Action (Table.States (127), (1 => 43), 33, 140);
      Add_Error (Table.States (127));
      Table.States (128).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (128), (1 => 44), 7, 139);
      Add_Error (Table.States (128));
      Table.States (129).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (129), (1 => 34), 9, 16);
      Add_Action (Table.States (129), (1 => 35), 10, 17);
      Add_Action (Table.States (129), (2, 70), 13, 18);
      Add_Action (Table.States (129), (1 => 6), 18, 19);
      Add_Action (Table.States (129), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (129), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (129), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (129), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (129), (1 => 37), 35, 20);
      Add_Action (Table.States (129), (1 => 63), 36, 21);
      Add_Error (Table.States (129));
      Add_Goto (Table.States (129), 71, 39, 22);
      Add_Goto (Table.States (129), 8, 41, 23);
      Add_Goto (Table.States (129), 66, 42, 24);
      Add_Goto (Table.States (129), 5, 54, 138);
      Add_Goto (Table.States (129), 65, 55, 26);
      Add_Goto (Table.States (129), 38, 56, 27);
      Add_Goto (Table.States (129), 7, 57, 28);
      Add_Goto (Table.States (129), 69, 67, 29);
      Add_Goto (Table.States (129), 32, 69, 31);
      Table.States (130).Productions := WisiToken.To_Vector ((1 => 4));
      Add_Action (Table.States (130), (1 => 34), 9, 16);
      Add_Action (Table.States (130), (1 => 35), 10, 17);
      Add_Action (Table.States (130), (2, 70), 13, 18);
      Add_Action (Table.States (130), (1 => 6), 18, 19);
      Add_Action (Table.States (130), 26, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (130), 30, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (130), 32, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (130), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (130), (1 => 37), 35, 20);
      Add_Action (Table.States (130), (1 => 63), 36, 21);
      Add_Error (Table.States (130));
      Add_Goto (Table.States (130), 71, 39, 22);
      Add_Goto (Table.States (130), 8, 41, 23);
      Add_Goto (Table.States (130), 66, 42, 24);
      Add_Goto (Table.States (130), 4, 54, 137);
      Add_Goto (Table.States (130), 65, 55, 26);
      Add_Goto (Table.States (130), 38, 56, 27);
      Add_Goto (Table.States (130), 7, 57, 28);
      Add_Goto (Table.States (130), 69, 67, 29);
      Add_Goto (Table.States (130), 32, 69, 31);
      Table.States (131).Productions := WisiToken.To_Vector ((1 => 11));
      Add_Action (Table.States (131), (1 => 10), 5, 52);
      Add_Action (Table.States (131), 7, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (131), (3, 4, 5), 11, 53);
      Add_Action (Table.States (131), (1 => 61), 15, 54);
      Add_Action (Table.States (131), (43, 44, 45), 17, 55);
      Add_Action (Table.States (131), (1 => 72), 22, 56);
      Add_Action (Table.States (131), 24, Reduce, 25, 51, 0, 0, null, null);
      Add_Action (Table.States (131), (57, 58), 35, 57);
      Add_Error (Table.States (131));
      Add_Goto (Table.States (131), 59, 40, 58);
      Add_Goto (Table.States (131), 60, 43, 59);
      Add_Goto (Table.States (131), 23, 49, 60);
      Add_Goto (Table.States (131), 24, 50, 61);
      Add_Goto (Table.States (131), 11, 51, 136);
      Add_Goto (Table.States (131), 22, 58, 63);
      Add_Goto (Table.States (131), 40, 59, 64);
      Add_Goto (Table.States (131), 41, 60, 65);
      Add_Goto (Table.States (131), 42, 61, 66);
      Add_Goto (Table.States (131), 20, 65, 67);
      Add_Goto (Table.States (131), 21, 70, 68);
      Table.States (132).Productions := WisiToken.To_Vector ((1 => 31));
      Add_Action (Table.States (132), (1 => 29), 16, 105);
      Add_Action (Table.States (132), 31, Reduce, 27, 52, 0, 0, null, null);
      Add_Action (Table.States (132), 34, Reduce, 27, 52, 0, 0, null, null);
      Add_Action (Table.States (132), (1 => 28), 36, 106);
      Add_Error (Table.States (132));
      Add_Goto (Table.States (132), 31, 52, 135);
      Table.States (133).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (133), (1 => 10), 33, 134);
      Add_Error (Table.States (133));
      Table.States (134).Productions := WisiToken.To_Vector ((1 => 10));
      Add_Action (Table.States (134), (5, 7, 11, 15, 17, 22, 24, 35), 10, 43, 7, 0, case_statement_0'Access, null);
      Table.States (135).Productions := WisiToken.To_Vector ((1 => 31));
      Add_Action (Table.States (135), (31, 34), 31, 53, 3, 1, null, null);
      Table.States (136).Productions := WisiToken.To_Vector ((1 => 11));
      Add_Action (Table.States (136), (7, 24), 11, 44, 4, 0, case_item_0'Access, null);
      Table.States (137).Productions := WisiToken.To_Vector ((4, 33));
      Add_Action (Table.States (137), (1 => 33), 26, 39);
      Add_Action (Table.States (137), (1 => 4), 33, 143);
      Add_Error (Table.States (137));
      Table.States (138).Productions := WisiToken.To_Vector ((5, 33));
      Add_Action (Table.States (138), (1 => 33), 26, 39);
      Add_Action (Table.States (138), (1 => 5), 33, 142);
      Add_Error (Table.States (138));
      Table.States (139).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (139), 33, Reduce, 36, 56, 0, 0, null, null);
      Add_Action (Table.States (139), (1 => 37), 35, 20);
      Add_Error (Table.States (139));
      Add_Goto (Table.States (139), 44, 56, 141);
      Table.States (140).Productions := WisiToken.To_Vector ((1 => 43));
      Add_Action (Table.States (140), (5, 7, 11, 15, 17, 22, 24, 35), 43, 59, 7, 0, package_spec_0'Access,
        package_spec_0_check'Access);
      Table.States (141).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (141), (1 => 44), 33, 144);
      Add_Error (Table.States (141));
      Table.States (142).Productions := WisiToken.To_Vector ((1 => 5));
      Add_Action (Table.States (142), (5, 7, 11, 15, 17, 22, 24, 35), 5, 40, 8, 2, attribute_declaration_2'Access,
        null);
      Table.States (143).Productions := WisiToken.To_Vector ((1 => 4));
      Add_Action (Table.States (143), (5, 7, 11, 15, 17, 22, 24, 35), 4, 40, 8, 1, attribute_declaration_1'Access,
        null);
      Table.States (144).Productions := WisiToken.To_Vector ((1 => 44));
      Add_Action (Table.States (144), (5, 7, 11, 15, 17, 22, 24, 35), 44, 60, 9, 0, package_extension_0'Access,
        package_extension_0_check'Access);

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
end Gpr_Process_Main;
