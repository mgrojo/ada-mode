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
      Table.McKenzie_Param :=
        (First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 37,
         Insert =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0),
         Delete =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0),
         Push_Back =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0),
         Task_Count  => 0,
         Cost_Limit  => 2147483647,
         Check_Limit => 2147483647,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 2147483647);


      declare
         procedure Build_State_0 (State : in out Parse_State)
         is begin
            Add_Action (State, ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (State, (1 => (32, 0)), 22, 2);
            Add_Error (State);
            Add_Goto (State, 27, 3);
            Add_Goto (State, 32, 4);
            Add_Goto (State, 36, 5);
            Add_Goto (State, 37, 6);
         end Build_State_0;
         procedure Build_State_1 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 1)), 3, 9);
            Add_Action (State, (1 => (27, 5)), 4, 10);
            Add_Action (State, (1 => (27, 4)), 5, 11);
            Add_Action (State, (1 => (28, 0)), 6, 12);
            Add_Action (State, (1 => (28, 1)), 7, 13);
            Add_Action (State, (1 => (28, 2)), 8, 14);
            Add_Action (State, ((27, 2), (27, 3)), 22, 15);
            Add_Error (State);
            Add_Goto (State, 28, 16);
         end Build_State_1;
         procedure Build_State_2 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (32, 0)), 13, 8);
            Add_Error (State);
         end Build_State_2;
         procedure Build_State_3 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (36, 0), 1, null, null);
         end Build_State_3;
         procedure Build_State_4 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (36, 1), 1, null, null);
         end Build_State_4;
         procedure Build_State_5 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (37, 0), 1, null, null);
         end Build_State_5;
         procedure Build_State_6 (State : in out Parse_State)
         is begin
            Add_Action (State, ((27, 0), (27, 1), (27, 2), (27, 3), (27, 4), (27, 5)), 18, 1);
            Add_Action (State, (1 => (32, 0)), 22, 2);
            Add_Action (State, 25, Accept_It, (26, 0), 1, null, null);
            Add_Error (State);
            Add_Goto (State, 27, 3);
            Add_Goto (State, 32, 4);
            Add_Goto (State, 36, 7);
         end Build_State_6;
         procedure Build_State_7 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (37, 1), 2, null, null);
         end Build_State_7;
         procedure Build_State_8 (State : in out Parse_State)
         is begin
            Add_Action (State, 12, Reduce, (34, 0), 0, null, null);
            Add_Action (State, 18, Reduce, (34, 0), 0, null, null);
            Add_Action (State, 19, Reduce, (34, 0), 0, null, null);
            Add_Action (State, (1 => (35, 0)), 22, 35);
            Add_Error (State);
            Add_Goto (State, 33, 36);
            Add_Goto (State, 34, 37);
            Add_Goto (State, 35, 38);
         end Build_State_8;
         procedure Build_State_9 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (29, 0)), 22, 33);
            Add_Error (State);
            Add_Goto (State, 29, 34);
         end Build_State_9;
         procedure Build_State_10 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 5)), 5, 32);
            Add_Error (State);
         end Build_State_10;
         procedure Build_State_11 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 4)), 22, 31);
            Add_Error (State);
         end Build_State_11;
         procedure Build_State_12 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 =>  22), (28, 0), 1, null, null);
         end Build_State_12;
         procedure Build_State_13 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 1)), 17, 30);
            Add_Error (State);
         end Build_State_13;
         procedure Build_State_14 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 2)), 17, 29);
            Add_Error (State);
         end Build_State_14;
         procedure Build_State_15 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (31, 8)), 8, 18);
            Add_Action (State, (1 => (31, 4)), 10, 19);
            Add_Action (State, (1 => (31, 0)), 14, 20);
            Add_Action (State, (1 => (31, 2)), 15, 21);
            Add_Action (State, 18, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Action (State, (1 => (31, 5)), 20, 22);
            Add_Action (State, (1 => (31, 3)), 21, 23);
            Add_Action (State, (1 => (31, 1)), 22, 24, (27, 3), 2, declaration_3'Access, null);
            Add_Action (State, (1 => (31, 6)), 23, 25);
            Add_Action (State, (1 => (31, 7)), 24, 26);
            Add_Action (State, 25, Reduce, (27, 3), 2, declaration_3'Access, null);
            Add_Error (State);
            Add_Goto (State, 30, 27);
            Add_Goto (State, 31, 28);
         end Build_State_15;
         procedure Build_State_16 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 0)), 22, 17);
            Add_Error (State);
         end Build_State_16;
         procedure Build_State_17 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (31, 8)), 8, 18);
            Add_Action (State, (1 => (31, 4)), 10, 19);
            Add_Action (State, (1 => (31, 0)), 14, 20);
            Add_Action (State, (1 => (31, 2)), 15, 21);
            Add_Action (State, (1 => (31, 5)), 20, 22);
            Add_Action (State, (1 => (31, 3)), 21, 23);
            Add_Action (State, (1 => (31, 1)), 22, 24);
            Add_Action (State, (1 => (31, 6)), 23, 25);
            Add_Action (State, (1 => (31, 7)), 24, 26);
            Add_Error (State);
            Add_Goto (State, 30, 50);
            Add_Goto (State, 31, 28);
         end Build_State_17;
         procedure Build_State_18 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 8), 1, null, null);
         end Build_State_18;
         procedure Build_State_19 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 4), 1, null, null);
         end Build_State_19;
         procedure Build_State_20 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 0), 1, null, null);
         end Build_State_20;
         procedure Build_State_21 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 2), 1, null, null);
         end Build_State_21;
         procedure Build_State_22 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 5), 1, null, null);
         end Build_State_22;
         procedure Build_State_23 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 3), 1, null, null);
         end Build_State_23;
         procedure Build_State_24 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 1), 1, null, null);
         end Build_State_24;
         procedure Build_State_25 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 6), 1, null, null);
         end Build_State_25;
         procedure Build_State_26 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (31, 7), 1, null, null);
         end Build_State_26;
         procedure Build_State_27 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (31, 8)), 8, 18);
            Add_Action (State, (1 => (31, 4)), 10, 19);
            Add_Action (State, (1 => (31, 0)), 14, 20);
            Add_Action (State, (1 => (31, 2)), 15, 21);
            Add_Action (State, 18, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Action (State, (1 => (31, 5)), 20, 22);
            Add_Action (State, (1 => (31, 3)), 21, 23);
            Add_Action (State, (1 => (31, 1)), 22, 24, (27, 2), 3, declaration_2'Access, null);
            Add_Action (State, (1 => (31, 6)), 23, 25);
            Add_Action (State, (1 => (31, 7)), 24, 26);
            Add_Action (State, 25, Reduce, (27, 2), 3, declaration_2'Access, null);
            Add_Error (State);
            Add_Goto (State, 31, 49);
         end Build_State_27;
         procedure Build_State_28 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 0), 1, null, null);
         end Build_State_28;
         procedure Build_State_29 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 2)), 22, 48);
            Add_Error (State);
         end Build_State_29;
         procedure Build_State_30 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 1)), 22, 47);
            Add_Error (State);
         end Build_State_30;
         procedure Build_State_31 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 4)), 15, 46);
            Add_Error (State);
         end Build_State_31;
         procedure Build_State_32 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (27, 5), 3, declaration_5'Access, null);
         end Build_State_32;
         procedure Build_State_33 (State : in out Parse_State)
         is begin
            Add_Action (State, (9, 22), (29, 0), 1, null, null);
         end Build_State_33;
         procedure Build_State_34 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 1)), 9, 44);
            Add_Action (State, (1 => (29, 1)), 22, 45);
            Add_Error (State);
         end Build_State_34;
         procedure Build_State_35 (State : in out Parse_State)
         is begin
            Add_Action (State, (11, 12, 18, 19, 22), (35, 0), 1, null, null);
         end Build_State_35;
         procedure Build_State_36 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 1)), 12, 41);
            Add_Action (State, ((33, 2), (33, 3)), 18, 42);
            Add_Action (State, (1 => (32, 0)), 19, 43);
            Add_Error (State);
         end Build_State_36;
         procedure Build_State_37 (State : in out Parse_State)
         is begin
            Add_Action (State, (12, 18, 19), (33, 0), 1, null, null);
         end Build_State_37;
         procedure Build_State_38 (State : in out Parse_State)
         is begin
            Add_Action (State, ((34, 2), (34, 3)), 11, 39);
            Add_Action (State, 12, Reduce, (34, 1), 1, null, null);
            Add_Action (State, 18, Reduce, (34, 1), 1, null, null);
            Add_Action (State, 19, Reduce, (34, 1), 1, null, null);
            Add_Action (State, (1 => (35, 1)), 22, 40);
            Add_Error (State);
         end Build_State_38;
         procedure Build_State_39 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (34, 3)), 11, 57);
            Add_Action (State, 12, Reduce, (34, 2), 2, null, null);
            Add_Action (State, 18, Reduce, (34, 2), 2, null, null);
            Add_Action (State, 19, Reduce, (34, 2), 2, null, null);
            Add_Error (State);
         end Build_State_39;
         procedure Build_State_40 (State : in out Parse_State)
         is begin
            Add_Action (State, (11, 12, 18, 19, 22), (35, 1), 2, null, null);
         end Build_State_40;
         procedure Build_State_41 (State : in out Parse_State)
         is begin
            Add_Action (State, 12, Reduce, (34, 0), 0, null, null);
            Add_Action (State, 18, Reduce, (34, 0), 0, null, null);
            Add_Action (State, 19, Reduce, (34, 0), 0, null, null);
            Add_Action (State, (1 => (35, 0)), 22, 35);
            Add_Error (State);
            Add_Goto (State, 34, 56);
            Add_Goto (State, 35, 38);
         end Build_State_41;
         procedure Build_State_42 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 3)), 4, 54);
            Add_Action (State, (1 => (33, 2)), 5, 55);
            Add_Error (State);
         end Build_State_42;
         procedure Build_State_43 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (32, 0), 4, nonterminal_0'Access, null);
         end Build_State_43;
         procedure Build_State_44 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (27, 1), 4, declaration_1'Access, null);
         end Build_State_44;
         procedure Build_State_45 (State : in out Parse_State)
         is begin
            Add_Action (State, (9, 22), (29, 1), 2, null, null);
         end Build_State_45;
         procedure Build_State_46 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (27, 4)), 22, 53);
            Add_Error (State);
         end Build_State_46;
         procedure Build_State_47 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 1)), 16, 52);
            Add_Error (State);
         end Build_State_47;
         procedure Build_State_48 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (28, 2)), 16, 51);
            Add_Error (State);
         end Build_State_48;
         procedure Build_State_49 (State : in out Parse_State)
         is begin
            Add_Action (State, (8, 10, 14, 15, 18, 20, 21, 22, 23, 24, 25), (30, 1), 2, null, null);
         end Build_State_49;
         procedure Build_State_50 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (31, 8)), 8, 18);
            Add_Action (State, (1 => (31, 4)), 10, 19);
            Add_Action (State, (1 => (31, 0)), 14, 20);
            Add_Action (State, (1 => (31, 2)), 15, 21);
            Add_Action (State, 18, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Action (State, (1 => (31, 5)), 20, 22);
            Add_Action (State, (1 => (31, 3)), 21, 23);
            Add_Action (State, (1 => (31, 1)), 22, 24, (27, 0), 4, declaration_0'Access, null);
            Add_Action (State, (1 => (31, 6)), 23, 25);
            Add_Action (State, (1 => (31, 7)), 24, 26);
            Add_Action (State, 25, Reduce, (27, 0), 4, declaration_0'Access, null);
            Add_Error (State);
            Add_Goto (State, 31, 49);
         end Build_State_50;
         procedure Build_State_51 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 =>  22), (28, 2), 4, null, null);
         end Build_State_51;
         procedure Build_State_52 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 =>  22), (28, 1), 4, null, null);
         end Build_State_52;
         procedure Build_State_53 (State : in out Parse_State)
         is begin
            Add_Action (State, (18, 22, 25), (27, 4), 5, declaration_4'Access, null);
         end Build_State_53;
         procedure Build_State_54 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 3)), 5, 59);
            Add_Error (State);
         end Build_State_54;
         procedure Build_State_55 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 2)), 22, 58);
            Add_Error (State);
         end Build_State_55;
         procedure Build_State_56 (State : in out Parse_State)
         is begin
            Add_Action (State, (12, 18, 19), (33, 1), 3, null, null);
         end Build_State_56;
         procedure Build_State_57 (State : in out Parse_State)
         is begin
            Add_Action (State, (12, 18, 19), (34, 3), 3, null, null);
         end Build_State_57;
         procedure Build_State_58 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 2)), 15, 60);
            Add_Error (State);
         end Build_State_58;
         procedure Build_State_59 (State : in out Parse_State)
         is begin
            Add_Action (State, (12, 18, 19), (33, 3), 4, null, null);
         end Build_State_59;
         procedure Build_State_60 (State : in out Parse_State)
         is begin
            Add_Action (State, (1 => (33, 2)), 22, 61);
            Add_Error (State);
         end Build_State_60;
         procedure Build_State_61 (State : in out Parse_State)
         is begin
            Add_Action (State, (12, 18, 19), (33, 2), 6, null, null);
         end Build_State_61;
      begin
         Build_State_0 (Table.States (0));
         Build_State_1 (Table.States (1));
         Build_State_2 (Table.States (2));
         Build_State_3 (Table.States (3));
         Build_State_4 (Table.States (4));
         Build_State_5 (Table.States (5));
         Build_State_6 (Table.States (6));
         Build_State_7 (Table.States (7));
         Build_State_8 (Table.States (8));
         Build_State_9 (Table.States (9));
         Build_State_10 (Table.States (10));
         Build_State_11 (Table.States (11));
         Build_State_12 (Table.States (12));
         Build_State_13 (Table.States (13));
         Build_State_14 (Table.States (14));
         Build_State_15 (Table.States (15));
         Build_State_16 (Table.States (16));
         Build_State_17 (Table.States (17));
         Build_State_18 (Table.States (18));
         Build_State_19 (Table.States (19));
         Build_State_20 (Table.States (20));
         Build_State_21 (Table.States (21));
         Build_State_22 (Table.States (22));
         Build_State_23 (Table.States (23));
         Build_State_24 (Table.States (24));
         Build_State_25 (Table.States (25));
         Build_State_26 (Table.States (26));
         Build_State_27 (Table.States (27));
         Build_State_28 (Table.States (28));
         Build_State_29 (Table.States (29));
         Build_State_30 (Table.States (30));
         Build_State_31 (Table.States (31));
         Build_State_32 (Table.States (32));
         Build_State_33 (Table.States (33));
         Build_State_34 (Table.States (34));
         Build_State_35 (Table.States (35));
         Build_State_36 (Table.States (36));
         Build_State_37 (Table.States (37));
         Build_State_38 (Table.States (38));
         Build_State_39 (Table.States (39));
         Build_State_40 (Table.States (40));
         Build_State_41 (Table.States (41));
         Build_State_42 (Table.States (42));
         Build_State_43 (Table.States (43));
         Build_State_44 (Table.States (44));
         Build_State_45 (Table.States (45));
         Build_State_46 (Table.States (46));
         Build_State_47 (Table.States (47));
         Build_State_48 (Table.States (48));
         Build_State_49 (Table.States (49));
         Build_State_50 (Table.States (50));
         Build_State_51 (Table.States (51));
         Build_State_52 (Table.States (52));
         Build_State_53 (Table.States (53));
         Build_State_54 (Table.States (54));
         Build_State_55 (Table.States (55));
         Build_State_56 (Table.States (56));
         Build_State_57 (Table.States (57));
         Build_State_58 (Table.States (58));
         Build_State_59 (Table.States (59));
         Build_State_60 (Table.States (60));
         Build_State_61 (Table.States (61));
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
