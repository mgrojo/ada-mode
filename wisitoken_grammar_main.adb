--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
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

with SAL;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_re2c_c;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
package body Wisitoken_Grammar_Main is

   function Is_Block_Delimited (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         RAW_CODE_ID |
         REGEXP_ID |
         ACTION_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Is_Block_Delimited;

   function Same_Block_Delimiters (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return False;
      when RAW_CODE_ID => return False;
      when REGEXP_ID => return False;
      when ACTION_ID => return False;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Same_Block_Delimiters;

   function Start_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return 2;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when STRING_LITERAL_1_ID => return 1;
      when STRING_LITERAL_2_ID => return 1;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end Start_Delimiter_Length;

   function End_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return 1;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end End_Delimiter_Length;

   function Find_End_Delimiter
     (Source      : in WisiToken.Lexer.Source;
      ID          : in WisiToken.Token_ID;
      Token_Start : in WisiToken.Buffer_Pos)
     return WisiToken.Buffer_Pos
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => WisiToken.Lexer.Find_New_Line (Source, Token_Start),
         when RAW_CODE_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "%{"),
         when REGEXP_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "%["),
         when ACTION_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "%("),
         when STRING_LITERAL_1_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when STRING_LITERAL_2_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when others => raise SAL.Programmer_Error);
   end Find_End_Delimiter;

   function Find_Scan_End
     (Source   : in WisiToken.Lexer.Source;
      ID       : in WisiToken.Token_ID;
      Region   : in WisiToken.Buffer_Region;
      Inserted : in Boolean;
      Start    : in Boolean)
     return WisiToken.Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID =>
         (if Inserted then (if Start then Lexer.Find_New_Line (Source, Region.Last) else Region.Last)
          elsif Start then Region.Last
          else Lexer.Find_New_Line (Source, Region.Last)),
         when RAW_CODE_ID =>
           (if Start then Region.Last
            else Lexer.Find_String (Source, Region.First, "%{")),
         when REGEXP_ID =>
           (if Start then Region.Last
            else Lexer.Find_String (Source, Region.First, "%[")),
         when ACTION_ID =>
           (if Start then Region.Last
            else Lexer.Find_String (Source, Region.First, "%(")),
         when STRING_LITERAL_1_ID => Lexer.Find_New_Line (Source, Region.Last),
         when STRING_LITERAL_2_ID => Lexer.Find_New_Line (Source, Region.Last),
         when others => raise SAL.Programmer_Error);
   end Find_Scan_End;

   function Contains_End_Delimiter
     (Source : in WisiToken.Lexer.Source;
      ID     : in WisiToken.Token_ID;
      Region : in WisiToken.Buffer_Region)
     return WisiToken.Base_Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => Lexer.Find_New_Line (Source, Region),
         when RAW_CODE_ID => Lexer.Find_String_Or_New_Line (Source, Region, "%{"),
         when REGEXP_ID => Lexer.Find_String_Or_New_Line (Source, Region, "%["),
         when ACTION_ID => Lexer.Find_String_Or_New_Line (Source, Region, "%("),
         when STRING_LITERAL_1_ID => Lexer.Find_String_Or_New_Line (Source, Region, """"),
         when STRING_LITERAL_2_ID => Lexer.Find_String_Or_New_Line (Source, Region, "'"),
         when others => raise SAL.Programmer_Error);
   end Contains_End_Delimiter;

   function Line_Begin_Char_Pos
    (Source : in WisiToken.Lexer.Source;
     Token  : in WisiToken.Lexer.Token;
     Line   : in WisiToken.Line_Number_Type)
   return WisiToken.Buffer_Pos
   is
      use all type WisiToken.Base_Buffer_Pos;
   begin
      case To_Token_Enum (Token.ID) is
      when NEW_LINE_ID => return Token.Char_Region.Last + 1;
      when COMMENT_ID => return Token.Char_Region.Last + 1;
      when RAW_CODE_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when REGEXP_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when ACTION_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when others => raise SAL.Programmer_Error;
      end case;
   end Line_Begin_Char_Pos;

   function Terminated_By_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Terminated_By_New_Line;

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_re2c_c.New_Lexer,
      wisitoken_grammar_re2c_c.Free_Lexer,
      wisitoken_grammar_re2c_c.Reset_Lexer,
      wisitoken_grammar_re2c_c.Set_Verbosity,
      wisitoken_grammar_re2c_c.Set_Position,
      wisitoken_grammar_re2c_c.Next_Token,
      Is_Block_Delimited,
      Same_Block_Delimiters,
      Start_Delimiter_Length,
      End_Delimiter_Length,
      Find_End_Delimiter,
      Contains_End_Delimiter,
      Find_Scan_End,
      Line_Begin_Char_Pos,
      Terminated_By_New_Line);

   function Create_Parse_Table
     return WisiToken.Parse.LR.Parse_Table_Ptr
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 124,
         First_Terminal    => 3,
         Last_Terminal     => 38,
         First_Nonterminal => 39,
         Last_Nonterminal  => 59);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 25, (40, 0), 1);
            Add_Action (Table.States (0), 35, (46, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 40, 3);
            Add_Goto (Table.States (0), 46, 4);
            Add_Goto (Table.States (0), 58, 5);
            Add_Goto (Table.States (0), 59, 6);
            Table.States (1).Action_List.Set_Capacity (8);
            Add_Action (Table.States (1), 3, (40, 2), 7);
            Add_Action (Table.States (1), 4, (40, 9), 8);
            Add_Action (Table.States (1), 5, (40, 7), 9);
            Add_Action (Table.States (1), 6, (40, 5), 10);
            Add_Action (Table.States (1), 8, (41, 0), 11);
            Add_Action (Table.States (1), 9, (40, 1), 12);
            Add_Action (Table.States (1), 10, (41, 2), 13);
            Add_Action (Table.States (1), 35, (40, 3), 14);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 41, 15);
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 15, (46, 0), 16);
            Add_Action (Table.States (2), 16, (46, 1), 17);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (25, 35, 38), (58, 0),  1);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (25, 35, 38), (58, 1),  1);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (25, 35, 38), (59, 0),  1);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 25, (40, 0), 1);
            Add_Action (Table.States (6), 35, (46, 0), 2);
            Add_Action (Table.States (6), 38, Accept_It, (39, 0),  1);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 40, 3);
            Add_Goto (Table.States (6), 46, 4);
            Add_Goto (Table.States (6), 58, 18);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 35, (42, 0), 19);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 42, 20);
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 6, (40, 9), 21);
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 35, (40, 7), 22);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 35, (40, 5), 23);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), (1 =>  35), (41, 0),  1);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 23, (40, 1), 24);
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 23, (41, 2), 25);
            Table.States (14).Action_List.Set_Capacity (15);
            Add_Action (Table.States (14), 7, (45, 3), 26);
            Add_Action (Table.States (14), 10, (45, 12), 27);
            Add_Action (Table.States (14), 12, (45, 7), 28);
            Add_Action (Table.States (14), 14, (45, 0), 29);
            Add_Action (Table.States (14), 17, (45, 1), 30);
            Add_Action (Table.States (14), 18, (45, 4), 31);
            Add_Action (Table.States (14), 22, (45, 5), 32);
            Add_Action (Table.States (14), 25, Reduce, (40, 4),  2);
            Add_Action (Table.States (14), 30, (45, 8), 33);
            Add_Action (Table.States (14), 32, (45, 9), 34);
            Add_Action (Table.States (14), 34, (45, 6), 35);
            Add_Action (Table.States (14), 35, (45, 2), 36);
            Add_Conflict (Table.States (14), 35, (40, 4),  2);
            Add_Action (Table.States (14), 36, (45, 10), 37);
            Add_Action (Table.States (14), 37, (45, 11), 38);
            Add_Action (Table.States (14), 38, Reduce, (40, 4),  2);
            Table.States (14).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (14), 44, 39);
            Add_Goto (Table.States (14), 45, 40);
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 35, (40, 0), 41);
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), 14, Reduce, (49, 0),  0);
            Add_Action (Table.States (16), 20, (56, 0), 42);
            Add_Action (Table.States (16), 21, (55, 0), 43);
            Add_Action (Table.States (16), 22, (54, 0), 44);
            Add_Action (Table.States (16), 23, (50, 0), 45);
            Add_Action (Table.States (16), 25, Reduce, (49, 0),  0);
            Add_Action (Table.States (16), 31, Reduce, (49, 0),  0);
            Add_Action (Table.States (16), 35, (51, 1), 46);
            Add_Conflict (Table.States (16), 35, (49, 0),  0);
            Add_Action (Table.States (16), 37, (53, 1), 47);
            Add_Action (Table.States (16), 38, Reduce, (49, 0),  0);
            Table.States (16).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (16), 48, 48);
            Add_Goto (Table.States (16), 49, 49);
            Add_Goto (Table.States (16), 50, 50);
            Add_Goto (Table.States (16), 51, 51);
            Add_Goto (Table.States (16), 52, 52);
            Add_Goto (Table.States (16), 53, 53);
            Add_Goto (Table.States (16), 54, 54);
            Add_Goto (Table.States (16), 55, 55);
            Add_Goto (Table.States (16), 56, 56);
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), 14, Reduce, (49, 0),  0);
            Add_Action (Table.States (17), 20, (56, 0), 42);
            Add_Action (Table.States (17), 21, (55, 0), 43);
            Add_Action (Table.States (17), 22, (54, 0), 44);
            Add_Action (Table.States (17), 23, (50, 0), 45);
            Add_Action (Table.States (17), 25, Reduce, (49, 0),  0);
            Add_Action (Table.States (17), 31, Reduce, (49, 0),  0);
            Add_Action (Table.States (17), 35, (51, 1), 46);
            Add_Conflict (Table.States (17), 35, (49, 0),  0);
            Add_Action (Table.States (17), 37, (53, 1), 47);
            Add_Action (Table.States (17), 38, Reduce, (49, 0),  0);
            Table.States (17).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (17), 48, 57);
            Add_Goto (Table.States (17), 49, 49);
            Add_Goto (Table.States (17), 50, 50);
            Add_Goto (Table.States (17), 51, 51);
            Add_Goto (Table.States (17), 52, 52);
            Add_Goto (Table.States (17), 53, 53);
            Add_Goto (Table.States (17), 54, 54);
            Add_Goto (Table.States (17), 55, 55);
            Add_Goto (Table.States (17), 56, 56);
            Table.States (18).Action_List.Set_Capacity (3);
            Add_Action (Table.States (18), (25, 35, 38), (59, 1),  2);
            Table.States (19).Action_List.Set_Capacity (2);
            Add_Action (Table.States (19), (11, 35), (42, 0),  1);
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), 11, (40, 2), 58);
            Add_Action (Table.States (20), 35, (42, 1), 59);
            Table.States (21).Action_List.Set_Capacity (3);
            Add_Action (Table.States (21), (25, 35, 38), (40, 9),  3);
            Table.States (22).Action_List.Set_Capacity (2);
            Add_Action (Table.States (22), 7, (40, 8), 60);
            Add_Action (Table.States (22), 18, (40, 7), 61);
            Table.States (23).Action_List.Set_Capacity (2);
            Add_Action (Table.States (23), 7, (40, 6), 62);
            Add_Action (Table.States (23), 18, (40, 5), 63);
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 35, (40, 1), 64);
            Table.States (25).Action_List.Set_Capacity (1);
            Add_Action (Table.States (25), 35, (41, 2), 65);
            Table.States (26).Action_List.Set_Capacity (15);
            Add_Action (Table.States (26), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 3),  1);
            Table.States (27).Action_List.Set_Capacity (15);
            Add_Action (Table.States (27), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 12),  1);
            Table.States (28).Action_List.Set_Capacity (15);
            Add_Action (Table.States (28), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 7),  1);
            Table.States (29).Action_List.Set_Capacity (15);
            Add_Action (Table.States (29), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 0),  1);
            Table.States (30).Action_List.Set_Capacity (15);
            Add_Action (Table.States (30), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 1),  1);
            Table.States (31).Action_List.Set_Capacity (15);
            Add_Action (Table.States (31), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 4),  1);
            Table.States (32).Action_List.Set_Capacity (15);
            Add_Action (Table.States (32), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 5),  1);
            Table.States (33).Action_List.Set_Capacity (15);
            Add_Action (Table.States (33), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 8),  1);
            Table.States (34).Action_List.Set_Capacity (15);
            Add_Action (Table.States (34), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 9),  1);
            Table.States (35).Action_List.Set_Capacity (15);
            Add_Action (Table.States (35), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 6),  1);
            Table.States (36).Action_List.Set_Capacity (15);
            Add_Action (Table.States (36), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 2),  1);
            Table.States (37).Action_List.Set_Capacity (15);
            Add_Action (Table.States (37), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 10),  1);
            Table.States (38).Action_List.Set_Capacity (15);
            Add_Action (Table.States (38), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (45, 11),  1);
            Table.States (39).Action_List.Set_Capacity (15);
            Add_Action (Table.States (39), 7, (45, 3), 26);
            Add_Action (Table.States (39), 10, (45, 12), 27);
            Add_Action (Table.States (39), 12, (45, 7), 28);
            Add_Action (Table.States (39), 14, (45, 0), 29);
            Add_Action (Table.States (39), 17, (45, 1), 30);
            Add_Action (Table.States (39), 18, (45, 4), 31);
            Add_Action (Table.States (39), 22, (45, 5), 32);
            Add_Action (Table.States (39), 25, Reduce, (40, 3),  3);
            Add_Action (Table.States (39), 30, (45, 8), 33);
            Add_Action (Table.States (39), 32, (45, 9), 34);
            Add_Action (Table.States (39), 34, (45, 6), 35);
            Add_Action (Table.States (39), 35, (45, 2), 36);
            Add_Conflict (Table.States (39), 35, (40, 3),  3);
            Add_Action (Table.States (39), 36, (45, 10), 37);
            Add_Action (Table.States (39), 37, (45, 11), 38);
            Add_Action (Table.States (39), 38, Reduce, (40, 3),  3);
            Table.States (39).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (39), 45, 66);
            Table.States (40).Action_List.Set_Capacity (15);
            Add_Action (Table.States (40), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (44, 0),  1);
            Table.States (41).Action_List.Set_Capacity (13);
            Add_Action (Table.States (41), 7, (45, 3), 26);
            Add_Action (Table.States (41), 10, (45, 12), 27);
            Add_Action (Table.States (41), 12, (45, 7), 28);
            Add_Action (Table.States (41), 14, (45, 0), 29);
            Add_Action (Table.States (41), 17, (45, 1), 30);
            Add_Action (Table.States (41), 18, (45, 4), 31);
            Add_Action (Table.States (41), 22, (45, 5), 32);
            Add_Action (Table.States (41), 30, (45, 8), 33);
            Add_Action (Table.States (41), 32, (45, 9), 34);
            Add_Action (Table.States (41), 34, (45, 6), 35);
            Add_Action (Table.States (41), 35, (45, 2), 36);
            Add_Action (Table.States (41), 36, (45, 10), 37);
            Add_Action (Table.States (41), 37, (45, 11), 38);
            Table.States (41).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (41), 44, 67);
            Add_Goto (Table.States (41), 45, 40);
            Table.States (42).Action_List.Set_Capacity (6);
            Add_Action (Table.States (42), 20, (56, 0), 42);
            Add_Action (Table.States (42), 21, (55, 0), 43);
            Add_Action (Table.States (42), 22, (54, 0), 44);
            Add_Action (Table.States (42), 23, (50, 0), 45);
            Add_Action (Table.States (42), 35, (51, 1), 46);
            Add_Action (Table.States (42), 37, (53, 1), 47);
            Table.States (42).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (42), 50, 50);
            Add_Goto (Table.States (42), 51, 51);
            Add_Goto (Table.States (42), 52, 68);
            Add_Goto (Table.States (42), 53, 53);
            Add_Goto (Table.States (42), 54, 54);
            Add_Goto (Table.States (42), 55, 55);
            Add_Goto (Table.States (42), 56, 56);
            Add_Goto (Table.States (42), 57, 69);
            Table.States (43).Action_List.Set_Capacity (6);
            Add_Action (Table.States (43), 20, (56, 0), 42);
            Add_Action (Table.States (43), 21, (55, 0), 43);
            Add_Action (Table.States (43), 22, (54, 0), 44);
            Add_Action (Table.States (43), 23, (50, 0), 45);
            Add_Action (Table.States (43), 35, (51, 1), 46);
            Add_Action (Table.States (43), 37, (53, 1), 47);
            Table.States (43).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (43), 50, 50);
            Add_Goto (Table.States (43), 51, 51);
            Add_Goto (Table.States (43), 52, 68);
            Add_Goto (Table.States (43), 53, 53);
            Add_Goto (Table.States (43), 54, 54);
            Add_Goto (Table.States (43), 55, 55);
            Add_Goto (Table.States (43), 56, 56);
            Add_Goto (Table.States (43), 57, 70);
            Table.States (44).Action_List.Set_Capacity (6);
            Add_Action (Table.States (44), 20, (56, 0), 42);
            Add_Action (Table.States (44), 21, (55, 0), 43);
            Add_Action (Table.States (44), 22, (54, 0), 44);
            Add_Action (Table.States (44), 23, (50, 0), 45);
            Add_Action (Table.States (44), 35, (51, 1), 46);
            Add_Action (Table.States (44), 37, (53, 1), 47);
            Table.States (44).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (44), 50, 50);
            Add_Goto (Table.States (44), 51, 51);
            Add_Goto (Table.States (44), 52, 68);
            Add_Goto (Table.States (44), 53, 53);
            Add_Goto (Table.States (44), 54, 54);
            Add_Goto (Table.States (44), 55, 55);
            Add_Goto (Table.States (44), 56, 56);
            Add_Goto (Table.States (44), 57, 71);
            Table.States (45).Action_List.Set_Capacity (1);
            Add_Action (Table.States (45), 35, (50, 0), 72);
            Table.States (46).Action_List.Set_Capacity (18);
            Add_Action (Table.States (46), 13, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 14, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 18, (51, 1), 73);
            Add_Action (Table.States (46), 20, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 21, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 22, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 23, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 25, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 26, (56, 4), 74);
            Add_Action (Table.States (46), 27, (55, 2), 75);
            Add_Action (Table.States (46), 28, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 29, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 30, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 31, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 33, (56, 5), 76);
            Add_Action (Table.States (46), 35, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 37, Reduce, (53, 0),  1);
            Add_Action (Table.States (46), 38, Reduce, (53, 0),  1);
            Table.States (47).Action_List.Set_Capacity (15);
            Add_Action (Table.States (47), 13, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 14, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 20, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 21, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 22, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 23, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 25, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 27, (55, 3), 77);
            Add_Action (Table.States (47), 28, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 29, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 30, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 31, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 35, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 37, Reduce, (53, 1),  1);
            Add_Action (Table.States (47), 38, Reduce, (53, 1),  1);
            Table.States (48).Action_List.Set_Capacity (5);
            Add_Action (Table.States (48), 14, (48, 1), 78);
            Add_Action (Table.States (48), 25, (48, 2), 79);
            Add_Conflict (Table.States (48), 25, (47, 1),  0);
            Add_Action (Table.States (48), 31, (47, 0), 80);
            Add_Action (Table.States (48), 35, Reduce, (47, 1),  0);
            Add_Action (Table.States (48), 38, Reduce, (47, 1),  0);
            Table.States (48).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (48), 47, 81);
            Table.States (49).Action_List.Set_Capacity (5);
            Add_Action (Table.States (49), (14, 25, 31, 35, 38), (48, 0),  1);
            Table.States (50).Action_List.Set_Capacity (14);
            Add_Action (Table.States (50), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (53, 2),  1);
            Table.States (51).Action_List.Set_Capacity (14);
            Add_Action (Table.States (51), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (52, 0),  1);
            Table.States (52).Action_List.Set_Capacity (11);
            Add_Action (Table.States (52), 13, (49, 2), 82);
            Add_Action (Table.States (52), 14, Reduce, (49, 1),  1);
            Add_Action (Table.States (52), 20, (56, 0), 42);
            Add_Action (Table.States (52), 21, (55, 0), 43);
            Add_Action (Table.States (52), 22, (54, 0), 44);
            Add_Action (Table.States (52), 23, (50, 0), 45);
            Add_Action (Table.States (52), 25, Reduce, (49, 1),  1);
            Add_Action (Table.States (52), 31, Reduce, (49, 1),  1);
            Add_Action (Table.States (52), 35, (51, 1), 46);
            Add_Conflict (Table.States (52), 35, (49, 1),  1);
            Add_Action (Table.States (52), 37, (53, 1), 47);
            Add_Action (Table.States (52), 38, Reduce, (49, 1),  1);
            Table.States (52).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (52), 50, 50);
            Add_Goto (Table.States (52), 51, 83);
            Add_Goto (Table.States (52), 53, 53);
            Add_Goto (Table.States (52), 54, 54);
            Add_Goto (Table.States (52), 55, 55);
            Add_Goto (Table.States (52), 56, 56);
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (53).Action_List.Set_Capacity (14);
            Add_Action (Table.States (53), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (51, 0),  1);
            Table.States (54).Action_List.Set_Capacity (14);
            Add_Action (Table.States (54), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (53, 5),  1);
            Table.States (55).Action_List.Set_Capacity (14);
            Add_Action (Table.States (55), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (53, 3),  1);
            Table.States (56).Action_List.Set_Capacity (14);
            Add_Action (Table.States (56), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (53, 4),  1);
            Table.States (57).Action_List.Set_Capacity (5);
            Add_Action (Table.States (57), 14, (48, 1), 78);
            Add_Action (Table.States (57), 25, (48, 2), 79);
            Add_Conflict (Table.States (57), 25, (47, 1),  0);
            Add_Action (Table.States (57), 31, (47, 0), 80);
            Add_Action (Table.States (57), 35, Reduce, (47, 1),  0);
            Add_Action (Table.States (57), 38, Reduce, (47, 1),  0);
            Table.States (57).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (57), 47, 84);
            Table.States (58).Action_List.Set_Capacity (3);
            Add_Action (Table.States (58), (25, 35, 38), (40, 2),  4);
            Table.States (59).Action_List.Set_Capacity (2);
            Add_Action (Table.States (59), (11, 35), (42, 1),  2);
            Table.States (60).Action_List.Set_Capacity (1);
            Add_Action (Table.States (60), 35, (43, 0), 85);
            Table.States (60).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (60), 43, 86);
            Table.States (61).Action_List.Set_Capacity (1);
            Add_Action (Table.States (61), 35, (40, 7), 87);
            Table.States (62).Action_List.Set_Capacity (1);
            Add_Action (Table.States (62), 35, (43, 0), 85);
            Table.States (62).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (62), 43, 88);
            Table.States (63).Action_List.Set_Capacity (1);
            Add_Action (Table.States (63), 35, (40, 5), 89);
            Table.States (64).Action_List.Set_Capacity (1);
            Add_Action (Table.States (64), 19, (40, 1), 90);
            Table.States (65).Action_List.Set_Capacity (1);
            Add_Action (Table.States (65), 19, (41, 2), 91);
            Table.States (66).Action_List.Set_Capacity (15);
            Add_Action (Table.States (66), (7, 10, 12, 14, 17, 18, 22, 25, 30, 32, 34, 35, 36, 37, 38), (44, 1),  2);
            Table.States (67).Action_List.Set_Capacity (15);
            Add_Action (Table.States (67), 7, (45, 3), 26);
            Add_Action (Table.States (67), 10, (45, 12), 27);
            Add_Action (Table.States (67), 12, (45, 7), 28);
            Add_Action (Table.States (67), 14, (45, 0), 29);
            Add_Action (Table.States (67), 17, (45, 1), 30);
            Add_Action (Table.States (67), 18, (45, 4), 31);
            Add_Action (Table.States (67), 22, (45, 5), 32);
            Add_Action (Table.States (67), 25, Reduce, (40, 0),  4);
            Add_Action (Table.States (67), 30, (45, 8), 33);
            Add_Action (Table.States (67), 32, (45, 9), 34);
            Add_Action (Table.States (67), 34, (45, 6), 35);
            Add_Action (Table.States (67), 35, (45, 2), 36);
            Add_Conflict (Table.States (67), 35, (40, 0),  4);
            Add_Action (Table.States (67), 36, (45, 10), 37);
            Add_Action (Table.States (67), 37, (45, 11), 38);
            Add_Action (Table.States (67), 38, Reduce, (40, 0),  4);
            Table.States (67).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (67), 45, 66);
            Table.States (68).Action_List.Set_Capacity (10);
            Add_Action (Table.States (68), 14, Reduce, (57, 0),  1);
            Add_Action (Table.States (68), 20, (56, 0), 42);
            Add_Action (Table.States (68), 21, (55, 0), 43);
            Add_Action (Table.States (68), 22, (54, 0), 44);
            Add_Action (Table.States (68), 23, (50, 0), 45);
            Add_Action (Table.States (68), 28, Reduce, (57, 0),  1);
            Add_Action (Table.States (68), 29, Reduce, (57, 0),  1);
            Add_Action (Table.States (68), 30, Reduce, (57, 0),  1);
            Add_Action (Table.States (68), 35, (51, 1), 46);
            Add_Action (Table.States (68), 37, (53, 1), 47);
            Table.States (68).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (68), 50, 50);
            Add_Goto (Table.States (68), 51, 83);
            Add_Goto (Table.States (68), 53, 53);
            Add_Goto (Table.States (68), 54, 54);
            Add_Goto (Table.States (68), 55, 55);
            Add_Goto (Table.States (68), 56, 56);
            Table.States (69).Action_List.Set_Capacity (2);
            Add_Action (Table.States (69), 14, (57, 1), 92);
            Add_Action (Table.States (69), 28, (56, 0), 93);
            Table.States (70).Action_List.Set_Capacity (2);
            Add_Action (Table.States (70), 14, (57, 1), 92);
            Add_Action (Table.States (70), 29, (55, 0), 94);
            Table.States (71).Action_List.Set_Capacity (2);
            Add_Action (Table.States (71), 14, (57, 1), 92);
            Add_Action (Table.States (71), 30, (54, 0), 95);
            Table.States (72).Action_List.Set_Capacity (1);
            Add_Action (Table.States (72), 18, (50, 0), 96);
            Table.States (73).Action_List.Set_Capacity (6);
            Add_Action (Table.States (73), 20, (56, 0), 42);
            Add_Action (Table.States (73), 21, (55, 0), 43);
            Add_Action (Table.States (73), 22, (54, 0), 44);
            Add_Action (Table.States (73), 23, (50, 0), 45);
            Add_Action (Table.States (73), 35, (53, 0), 97);
            Add_Action (Table.States (73), 37, (53, 1), 47);
            Table.States (73).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (73), 50, 50);
            Add_Goto (Table.States (73), 53, 98);
            Add_Goto (Table.States (73), 54, 54);
            Add_Goto (Table.States (73), 55, 55);
            Add_Goto (Table.States (73), 56, 56);
            Table.States (74).Action_List.Set_Capacity (14);
            Add_Action (Table.States (74), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (56, 4),  2);
            Table.States (75).Action_List.Set_Capacity (14);
            Add_Action (Table.States (75), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (55, 2),  2);
            Table.States (76).Action_List.Set_Capacity (14);
            Add_Action (Table.States (76), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (56, 5),  2);
            Table.States (77).Action_List.Set_Capacity (14);
            Add_Action (Table.States (77), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (55, 3),  2);
            Table.States (78).Action_List.Set_Capacity (10);
            Add_Action (Table.States (78), 14, Reduce, (49, 0),  0);
            Add_Action (Table.States (78), 20, (56, 0), 42);
            Add_Action (Table.States (78), 21, (55, 0), 43);
            Add_Action (Table.States (78), 22, (54, 0), 44);
            Add_Action (Table.States (78), 23, (50, 0), 45);
            Add_Action (Table.States (78), 25, Reduce, (49, 0),  0);
            Add_Action (Table.States (78), 31, Reduce, (49, 0),  0);
            Add_Action (Table.States (78), 35, (51, 1), 46);
            Add_Conflict (Table.States (78), 35, (49, 0),  0);
            Add_Action (Table.States (78), 37, (53, 1), 47);
            Add_Action (Table.States (78), 38, Reduce, (49, 0),  0);
            Table.States (78).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (78), 49, 99);
            Add_Goto (Table.States (78), 50, 50);
            Add_Goto (Table.States (78), 51, 51);
            Add_Goto (Table.States (78), 52, 52);
            Add_Goto (Table.States (78), 53, 53);
            Add_Goto (Table.States (78), 54, 54);
            Add_Goto (Table.States (78), 55, 55);
            Add_Goto (Table.States (78), 56, 56);
            Table.States (79).Action_List.Set_Capacity (3);
            Add_Action (Table.States (79), 4, (48, 6), 100);
            Add_Action (Table.States (79), 5, (48, 4), 101);
            Add_Action (Table.States (79), 6, (48, 2), 102);
            Table.States (80).Action_List.Set_Capacity (3);
            Add_Action (Table.States (80), (25, 35, 38), (47, 0),  1);
            Table.States (81).Action_List.Set_Capacity (3);
            Add_Action (Table.States (81), (25, 35, 38), (46, 0),  4);
            Table.States (82).Action_List.Set_Capacity (6);
            Add_Action (Table.States (82), 13, (49, 3), 103);
            Add_Action (Table.States (82), 14, Reduce, (49, 2),  2);
            Add_Action (Table.States (82), 25, Reduce, (49, 2),  2);
            Add_Action (Table.States (82), 31, Reduce, (49, 2),  2);
            Add_Action (Table.States (82), 35, Reduce, (49, 2),  2);
            Add_Action (Table.States (82), 38, Reduce, (49, 2),  2);
            Table.States (83).Action_List.Set_Capacity (14);
            Add_Action (Table.States (83), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (52, 1),  2);
            Table.States (84).Action_List.Set_Capacity (3);
            Add_Action (Table.States (84), (25, 35, 38), (46, 1),  4);
            Table.States (85).Action_List.Set_Capacity (5);
            Add_Action (Table.States (85), (14, 25, 31, 35, 38), (43, 0),  1);
            Table.States (86).Action_List.Set_Capacity (4);
            Add_Action (Table.States (86), 14, (43, 1), 104);
            Add_Action (Table.States (86), 25, Reduce, (40, 8),  5);
            Add_Action (Table.States (86), 35, Reduce, (40, 8),  5);
            Add_Action (Table.States (86), 38, Reduce, (40, 8),  5);
            Table.States (87).Action_List.Set_Capacity (3);
            Add_Action (Table.States (87), (25, 35, 38), (40, 7),  5);
            Table.States (88).Action_List.Set_Capacity (4);
            Add_Action (Table.States (88), 14, (43, 1), 104);
            Add_Action (Table.States (88), 25, Reduce, (40, 6),  5);
            Add_Action (Table.States (88), 35, Reduce, (40, 6),  5);
            Add_Action (Table.States (88), 38, Reduce, (40, 6),  5);
            Table.States (89).Action_List.Set_Capacity (3);
            Add_Action (Table.States (89), (25, 35, 38), (40, 5),  5);
            Table.States (90).Action_List.Set_Capacity (1);
            Add_Action (Table.States (90), 35, (40, 1), 105);
            Add_Conflict (Table.States (90), 35, (41, 1),  4);
            Table.States (91).Action_List.Set_Capacity (1);
            Add_Action (Table.States (91), (1 =>  35), (41, 2),  4);
            Table.States (92).Action_List.Set_Capacity (6);
            Add_Action (Table.States (92), 20, (56, 0), 42);
            Add_Action (Table.States (92), 21, (55, 0), 43);
            Add_Action (Table.States (92), 22, (54, 0), 44);
            Add_Action (Table.States (92), 23, (50, 0), 45);
            Add_Action (Table.States (92), 35, (51, 1), 46);
            Add_Action (Table.States (92), 37, (53, 1), 47);
            Table.States (92).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (92), 50, 50);
            Add_Goto (Table.States (92), 51, 51);
            Add_Goto (Table.States (92), 52, 106);
            Add_Goto (Table.States (92), 53, 53);
            Add_Goto (Table.States (92), 54, 54);
            Add_Goto (Table.States (92), 55, 55);
            Add_Goto (Table.States (92), 56, 56);
            Table.States (93).Action_List.Set_Capacity (15);
            Add_Action (Table.States (93), 13, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 14, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 20, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 21, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 22, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 23, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 24, (56, 1), 107);
            Add_Action (Table.States (93), 25, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 28, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 29, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 30, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 31, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 35, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 37, Reduce, (56, 0),  3);
            Add_Action (Table.States (93), 38, Reduce, (56, 0),  3);
            Table.States (94).Action_List.Set_Capacity (14);
            Add_Action (Table.States (94), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (55, 0),  3);
            Table.States (95).Action_List.Set_Capacity (17);
            Add_Action (Table.States (95), 13, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 14, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 20, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 21, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 22, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 23, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 25, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 26, (56, 2), 108);
            Add_Action (Table.States (95), 27, (55, 1), 109);
            Add_Action (Table.States (95), 28, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 29, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 30, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 31, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 33, (56, 3), 110);
            Add_Action (Table.States (95), 35, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 37, Reduce, (54, 0),  3);
            Add_Action (Table.States (95), 38, Reduce, (54, 0),  3);
            Table.States (96).Action_List.Set_Capacity (1);
            Add_Action (Table.States (96), 35, (50, 0), 111);
            Table.States (97).Action_List.Set_Capacity (17);
            Add_Action (Table.States (97), 13, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 14, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 20, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 21, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 22, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 23, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 25, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 26, (56, 4), 74);
            Add_Action (Table.States (97), 27, (55, 2), 75);
            Add_Action (Table.States (97), 28, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 29, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 30, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 31, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 33, (56, 5), 76);
            Add_Action (Table.States (97), 35, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 37, Reduce, (53, 0),  1);
            Add_Action (Table.States (97), 38, Reduce, (53, 0),  1);
            Table.States (98).Action_List.Set_Capacity (14);
            Add_Action (Table.States (98), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (51, 1),  3);
            Table.States (99).Action_List.Set_Capacity (5);
            Add_Action (Table.States (99), (14, 25, 31, 35, 38), (48, 1),  3);
            Table.States (100).Action_List.Set_Capacity (1);
            Add_Action (Table.States (100), 6, (48, 6), 112);
            Table.States (101).Action_List.Set_Capacity (1);
            Add_Action (Table.States (101), 35, (48, 4), 113);
            Table.States (102).Action_List.Set_Capacity (1);
            Add_Action (Table.States (102), 35, (48, 2), 114);
            Table.States (103).Action_List.Set_Capacity (5);
            Add_Action (Table.States (103), (14, 25, 31, 35, 38), (49, 3),  3);
            Table.States (104).Action_List.Set_Capacity (1);
            Add_Action (Table.States (104), 35, (43, 1), 115);
            Table.States (105).Action_List.Set_Capacity (3);
            Add_Action (Table.States (105), (25, 35, 38), (40, 1),  6);
            Table.States (106).Action_List.Set_Capacity (10);
            Add_Action (Table.States (106), 14, Reduce, (57, 1),  3);
            Add_Action (Table.States (106), 20, (56, 0), 42);
            Add_Action (Table.States (106), 21, (55, 0), 43);
            Add_Action (Table.States (106), 22, (54, 0), 44);
            Add_Action (Table.States (106), 23, (50, 0), 45);
            Add_Action (Table.States (106), 28, Reduce, (57, 1),  3);
            Add_Action (Table.States (106), 29, Reduce, (57, 1),  3);
            Add_Action (Table.States (106), 30, Reduce, (57, 1),  3);
            Add_Action (Table.States (106), 35, (51, 1), 46);
            Add_Action (Table.States (106), 37, (53, 1), 47);
            Table.States (106).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (106), 50, 50);
            Add_Goto (Table.States (106), 51, 83);
            Add_Goto (Table.States (106), 53, 53);
            Add_Goto (Table.States (106), 54, 54);
            Add_Goto (Table.States (106), 55, 55);
            Add_Goto (Table.States (106), 56, 56);
            Table.States (107).Action_List.Set_Capacity (14);
            Add_Action (Table.States (107), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (56, 1),  4);
            Table.States (108).Action_List.Set_Capacity (14);
            Add_Action (Table.States (108), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (56, 2),  4);
            Table.States (109).Action_List.Set_Capacity (14);
            Add_Action (Table.States (109), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (55, 1),  4);
            Table.States (110).Action_List.Set_Capacity (14);
            Add_Action (Table.States (110), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (56, 3),  4);
            Table.States (111).Action_List.Set_Capacity (1);
            Add_Action (Table.States (111), 19, (50, 0), 116);
            Table.States (112).Action_List.Set_Capacity (5);
            Add_Action (Table.States (112), (14, 25, 31, 35, 38), (48, 6),  4);
            Table.States (113).Action_List.Set_Capacity (2);
            Add_Action (Table.States (113), 7, (48, 5), 117);
            Add_Action (Table.States (113), 18, (48, 4), 118);
            Table.States (114).Action_List.Set_Capacity (2);
            Add_Action (Table.States (114), 7, (48, 3), 119);
            Add_Action (Table.States (114), 18, (48, 2), 120);
            Table.States (115).Action_List.Set_Capacity (5);
            Add_Action (Table.States (115), (14, 25, 31, 35, 38), (43, 1),  3);
            Table.States (116).Action_List.Set_Capacity (14);
            Add_Action (Table.States (116), (13, 14, 20, 21, 22, 23, 25, 28, 29, 30, 31, 35, 37, 38), (50, 0),  5);
            Table.States (117).Action_List.Set_Capacity (1);
            Add_Action (Table.States (117), 35, (43, 0), 85);
            Table.States (117).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (117), 43, 121);
            Table.States (118).Action_List.Set_Capacity (1);
            Add_Action (Table.States (118), 35, (48, 4), 122);
            Table.States (119).Action_List.Set_Capacity (1);
            Add_Action (Table.States (119), 35, (43, 0), 85);
            Table.States (119).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (119), 43, 123);
            Table.States (120).Action_List.Set_Capacity (1);
            Add_Action (Table.States (120), 35, (48, 2), 124);
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (121).Action_List.Set_Capacity (5);
            Add_Action (Table.States (121), 14, (43, 1), 104);
            Add_Conflict (Table.States (121), 14, (48, 5),  6);
            Add_Action (Table.States (121), 25, Reduce, (48, 5),  6);
            Add_Action (Table.States (121), 31, Reduce, (48, 5),  6);
            Add_Action (Table.States (121), 35, Reduce, (48, 5),  6);
            Add_Action (Table.States (121), 38, Reduce, (48, 5),  6);
            Table.States (122).Action_List.Set_Capacity (5);
            Add_Action (Table.States (122), (14, 25, 31, 35, 38), (48, 4),  6);
            Table.States (123).Action_List.Set_Capacity (5);
            Add_Action (Table.States (123), 14, (43, 1), 104);
            Add_Conflict (Table.States (123), 14, (48, 3),  6);
            Add_Action (Table.States (123), 25, Reduce, (48, 3),  6);
            Add_Action (Table.States (123), 31, Reduce, (48, 3),  6);
            Add_Action (Table.States (123), 35, Reduce, (48, 3),  6);
            Add_Action (Table.States (123), 38, Reduce, (48, 3),  6);
            Table.States (124).Action_List.Set_Capacity (5);
            Add_Action (Table.States (124), (14, 25, 31, 35, 38), (48, 2),  6);
         end Subr_3;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      Table.Max_Parallel := 15;
      return Table;
   end Create_Parse_Table;

   function Create_Lexer (Trace : in WisiToken.Trace_Access) return WisiToken.Lexer.Handle
   is begin
      return Lexer.New_Lexer (Trace, Wisitoken_Grammar_Actions.Descriptor'Access);
   end Create_Lexer;

   function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector
   is begin
      return Result : WisiToken.Syntax_Trees.Production_Info_Trees.Vector do
         Result.Set_First_Last (39, 59);
         Result (40).RHSs.Set_First_Last (0, 9);
         Result (40).RHSs (0).In_Parse_Action := null;
         Result (40).RHSs (0).Post_Parse_Action := declaration_0'Access;
         Result (40).RHSs (1).In_Parse_Action := null;
         Result (40).RHSs (1).Post_Parse_Action := declaration_1'Access;
         Result (40).RHSs (2).In_Parse_Action := null;
         Result (40).RHSs (2).Post_Parse_Action := declaration_2'Access;
         Result (40).RHSs (3).In_Parse_Action := null;
         Result (40).RHSs (3).Post_Parse_Action := declaration_3'Access;
         Result (40).RHSs (4).In_Parse_Action := null;
         Result (40).RHSs (4).Post_Parse_Action := declaration_4'Access;
         Result (40).RHSs (5).In_Parse_Action := null;
         Result (40).RHSs (5).Post_Parse_Action := declaration_5'Access;
         Result (40).RHSs (6).In_Parse_Action := null;
         Result (40).RHSs (6).Post_Parse_Action := declaration_6'Access;
         Result (40).RHSs (7).In_Parse_Action := null;
         Result (40).RHSs (7).Post_Parse_Action := declaration_7'Access;
         Result (40).RHSs (8).In_Parse_Action := null;
         Result (40).RHSs (8).Post_Parse_Action := declaration_8'Access;
         Result (40).RHSs (9).In_Parse_Action := null;
         Result (40).RHSs (9).Post_Parse_Action := declaration_9'Access;
         Result (46).RHSs.Set_First_Last (0, 1);
         Result (46).RHSs (0).In_Parse_Action := null;
         Result (46).RHSs (0).Post_Parse_Action := nonterminal_0'Access;
         Result (46).RHSs (1).In_Parse_Action := null;
         Result (46).RHSs (1).Post_Parse_Action := nonterminal_1'Access;
         Result (53).RHSs.Set_First_Last (0, 5);
         Result (53).RHSs (0).In_Parse_Action := null;
         Result (53).RHSs (0).Post_Parse_Action := null;
         Result (53).RHSs (1).In_Parse_Action := null;
         Result (53).RHSs (1).Post_Parse_Action := rhs_item_1'Access;
         Result (53).RHSs (2).In_Parse_Action := null;
         Result (53).RHSs (2).Post_Parse_Action := rhs_item_2'Access;
         Result (53).RHSs (3).In_Parse_Action := null;
         Result (53).RHSs (3).Post_Parse_Action := rhs_item_3'Access;
         Result (53).RHSs (4).In_Parse_Action := null;
         Result (53).RHSs (4).Post_Parse_Action := rhs_item_4'Access;
         Result (53).RHSs (5).In_Parse_Action := null;
         Result (53).RHSs (5).Post_Parse_Action := rhs_item_5'Access;
         Result (55).RHSs.Set_First_Last (0, 3);
         Result (55).RHSs (0).In_Parse_Action := null;
         Result (55).RHSs (0).Post_Parse_Action := null;
         Result (55).RHSs (1).In_Parse_Action := null;
         Result (55).RHSs (1).Post_Parse_Action := null;
         Result (55).RHSs (2).In_Parse_Action := null;
         Result (55).RHSs (2).Post_Parse_Action := null;
         Result (55).RHSs (3).In_Parse_Action := null;
         Result (55).RHSs (3).Post_Parse_Action := rhs_optional_item_3'Access;
      end return;
   end Create_Productions;

end Wisitoken_Grammar_Main;
