--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2023 Free Software Foundation, Inc.
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
with WisiToken.Parse.LR;
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
         STRING_LITERAL_DOUBLE_ID |
         STRING_LITERAL_SINGLE_ID => return True;
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
      when STRING_LITERAL_DOUBLE_ID => return True;
      when STRING_LITERAL_SINGLE_ID => return True;
      when others => return False;
      end case;
   end Same_Block_Delimiters;

   function Escape_Delimiter_Doubled (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when others => return False;
      end case;
   end Escape_Delimiter_Doubled;

   function Start_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return 2;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when STRING_LITERAL_DOUBLE_ID => return 1;
      when STRING_LITERAL_SINGLE_ID => return 1;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end Start_Delimiter_Length;

   function End_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         STRING_LITERAL_DOUBLE_ID |
         STRING_LITERAL_SINGLE_ID => return 1;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end End_Delimiter_Length;

   function New_Line_Is_End_Delimiter (ID : in WisiToken.Token_ID) return Boolean
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => True,
         when RAW_CODE_ID => False,
         when REGEXP_ID => False,
         when ACTION_ID => False,
         when STRING_LITERAL_DOUBLE_ID => True,
         when STRING_LITERAL_SINGLE_ID => True,
         when others => raise SAL.Programmer_Error);
   end New_Line_Is_End_Delimiter;

   function Find_End_Delimiter
     (Source      : in WisiToken.Lexer.Source;
      ID          : in WisiToken.Token_ID;
      Token_Start : in WisiToken.Buffer_Pos)
     return WisiToken.Buffer_Pos
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => WisiToken.Lexer.Find_New_Line (Source, Token_Start),
         when RAW_CODE_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "}%"),
         when REGEXP_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "]%"),
         when ACTION_ID => WisiToken.Lexer.Find_String (Source, Token_Start, ")%"),
         when STRING_LITERAL_DOUBLE_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when STRING_LITERAL_SINGLE_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
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
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_New_Line (Source, Region.Last)),
         when RAW_CODE_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "}%")),
         when REGEXP_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "]%")),
         when ACTION_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, ")%")),
         when STRING_LITERAL_DOUBLE_ID => Lexer.Find_New_Line (Source, Region.Last),
         when STRING_LITERAL_SINGLE_ID => Lexer.Find_New_Line (Source, Region.Last),
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
         when RAW_CODE_ID => Lexer.Find_String_Or_New_Line (Source, Region, "}%"),
         when REGEXP_ID => Lexer.Find_String_Or_New_Line (Source, Region, "]%"),
         when ACTION_ID => Lexer.Find_String_Or_New_Line (Source, Region, ")%"),
         when STRING_LITERAL_DOUBLE_ID => Lexer.Find_String_Or_New_Line (Source, Region, """"),
         when STRING_LITERAL_SINGLE_ID => Lexer.Find_String_Or_New_Line (Source, Region, "'"),
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

   function Can_Contain_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when RAW_CODE_ID => return True;
      when REGEXP_ID => return True;
      when ACTION_ID => return True;
      when others => return False;
      end case;
   end Can_Contain_New_Line;

   function Terminated_By_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when STRING_LITERAL_DOUBLE_ID => return True;
      when STRING_LITERAL_SINGLE_ID => return True;
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
      Escape_Delimiter_Doubled,
      Start_Delimiter_Length,
      End_Delimiter_Length,
      New_Line_Is_End_Delimiter,
      Find_End_Delimiter,
      Contains_End_Delimiter,
      Find_Scan_End,
      Line_Begin_Char_Pos,
      Can_Contain_New_Line,
      Terminated_By_New_Line);

   function Create_Parse_Table
     return WisiToken.Parse.LR.Parse_Table_Ptr
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 147,
         First_Terminal    => 3,
         Last_Terminal     => 42,
         First_Nonterminal => 43,
         Last_Nonterminal  => 66);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 30, (48, 0), 1);
            Add_Action (Table.States (0), 39, (53, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 48, 3);
            Add_Goto (Table.States (0), 53, 4);
            Add_Goto (Table.States (0), 65, 5);
            Add_Goto (Table.States (0), 66, 6);
            Table.States (1).Action_List.Set_Capacity (10);
            Add_Action (Table.States (1), 4, (48, 6), 7);
            Add_Action (Table.States (1), 5, (48, 7), 8);
            Add_Action (Table.States (1), 6, (48, 9), 9);
            Add_Action (Table.States (1), 7, (48, 16), 10);
            Add_Action (Table.States (1), 8, (48, 14), 11);
            Add_Action (Table.States (1), 9, (48, 12), 12);
            Add_Action (Table.States (1), 11, (48, 5), 13);
            Add_Action (Table.States (1), 12, (48, 2), 14);
            Add_Action (Table.States (1), 16, (48, 0), 15);
            Add_Action (Table.States (1), 39, (48, 10), 16);
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 21, (53, 0), 17);
            Add_Action (Table.States (2), 22, (53, 1), 18);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (30, 39, 42), (65, 0),  1);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (30, 39, 42), (65, 1),  1);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (30, 39, 42), (66, 0),  1);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 30, (48, 0), 1);
            Add_Action (Table.States (6), 39, (53, 0), 2);
            Add_Action (Table.States (6), 42, Accept_It, (43, 0),  1);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 48, 3);
            Add_Goto (Table.States (6), 53, 4);
            Add_Goto (Table.States (6), 65, 19);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 39, (49, 0), 20);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 49, 21);
            Table.States (8).Action_List.Set_Capacity (4);
            Add_Action (Table.States (8), 3, (45, 2), 22);
            Add_Action (Table.States (8), 14, (45, 1), 23);
            Add_Action (Table.States (8), 15, (45, 0), 24);
            Add_Action (Table.States (8), 39, (45, 3), 25);
            Table.States (8).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (8), 45, 26);
            Add_Goto (Table.States (8), 46, 27);
            Add_Goto (Table.States (8), 49, 28);
            Table.States (9).Action_List.Set_Capacity (4);
            Add_Action (Table.States (9), 3, (45, 2), 22);
            Add_Action (Table.States (9), 14, (45, 1), 23);
            Add_Action (Table.States (9), 15, (45, 0), 24);
            Add_Action (Table.States (9), 39, (45, 3), 29);
            Table.States (9).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (9), 45, 26);
            Add_Goto (Table.States (9), 46, 30);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 9, (48, 16), 31);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 39, (48, 14), 32);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 39, (48, 12), 33);
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), 39, (48, 5), 34);
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 28, (48, 2), 35);
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 28, (48, 0), 36);
            Table.States (16).Action_List.Set_Capacity (7);
            Add_Action (Table.States (16), 18, (44, 0), 37);
            Add_Action (Table.States (16), 30, Reduce, (48, 11),  2);
            Add_Action (Table.States (16), 38, (51, 1), 38);
            Add_Action (Table.States (16), 39, (51, 0), 39);
            Add_Conflict (Table.States (16), 39, (48, 11),  2);
            Add_Action (Table.States (16), 40, (44, 1), 40);
            Add_Action (Table.States (16), 41, (44, 2), 41);
            Add_Action (Table.States (16), 42, Reduce, (48, 11),  2);
            Table.States (16).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (16), 44, 42);
            Add_Goto (Table.States (16), 51, 43);
            Add_Goto (Table.States (16), 52, 44);
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 25, (63, 0), 45);
            Add_Action (Table.States (17), 26, (62, 0), 46);
            Add_Action (Table.States (17), 27, (61, 0), 47);
            Add_Action (Table.States (17), 28, (57, 0), 48);
            Add_Action (Table.States (17), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (17), 39, (58, 1), 49);
            Add_Conflict (Table.States (17), 39, (56, 0),  0);
            Add_Action (Table.States (17), 41, (60, 1), 50);
            Add_Action (Table.States (17), 42, Reduce, (56, 0),  0);
            Table.States (17).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (17), 55, 51);
            Add_Goto (Table.States (17), 56, 52);
            Add_Goto (Table.States (17), 57, 53);
            Add_Goto (Table.States (17), 58, 54);
            Add_Goto (Table.States (17), 59, 55);
            Add_Goto (Table.States (17), 60, 56);
            Add_Goto (Table.States (17), 61, 57);
            Add_Goto (Table.States (17), 62, 58);
            Add_Goto (Table.States (17), 63, 59);
            Table.States (18).Action_List.Set_Capacity (10);
            Add_Action (Table.States (18), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 25, (63, 0), 45);
            Add_Action (Table.States (18), 26, (62, 0), 46);
            Add_Action (Table.States (18), 27, (61, 0), 47);
            Add_Action (Table.States (18), 28, (57, 0), 48);
            Add_Action (Table.States (18), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (18), 39, (58, 1), 49);
            Add_Conflict (Table.States (18), 39, (56, 0),  0);
            Add_Action (Table.States (18), 41, (60, 1), 50);
            Add_Action (Table.States (18), 42, Reduce, (56, 0),  0);
            Table.States (18).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (18), 55, 60);
            Add_Goto (Table.States (18), 56, 52);
            Add_Goto (Table.States (18), 57, 53);
            Add_Goto (Table.States (18), 58, 54);
            Add_Goto (Table.States (18), 59, 55);
            Add_Goto (Table.States (18), 60, 56);
            Add_Goto (Table.States (18), 61, 57);
            Add_Goto (Table.States (18), 62, 58);
            Add_Goto (Table.States (18), 63, 59);
            Table.States (19).Action_List.Set_Capacity (3);
            Add_Action (Table.States (19), (30, 39, 42), (66, 1),  2);
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), (17, 39), (49, 0),  1);
            Table.States (21).Action_List.Set_Capacity (2);
            Add_Action (Table.States (21), 17, (48, 6), 61);
            Add_Action (Table.States (21), 39, (49, 1), 62);
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 39, (45, 2), 63);
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 39, (45, 1), 64);
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 39, (45, 0), 65);
            Table.States (25).Action_List.Set_Capacity (5);
            Add_Action (Table.States (25), 13, Reduce, (45, 3),  1);
            Add_Action (Table.States (25), 20, Reduce, (45, 3),  1);
            Add_Action (Table.States (25), 30, Reduce, (49, 0),  1);
            Add_Action (Table.States (25), 39, Reduce, (49, 0),  1);
            Add_Action (Table.States (25), 42, Reduce, (49, 0),  1);
            Table.States (26).Action_List.Set_Capacity (2);
            Add_Action (Table.States (26), (13, 20), (46, 0),  1);
            Table.States (27).Action_List.Set_Capacity (2);
            Add_Action (Table.States (27), 13, (48, 8), 66);
            Add_Action (Table.States (27), 20, (46, 1), 67);
            Table.States (28).Action_List.Set_Capacity (3);
            Add_Action (Table.States (28), 30, Reduce, (48, 7),  3);
            Add_Action (Table.States (28), 39, (49, 1), 62);
            Add_Conflict (Table.States (28), 39, (48, 7),  3);
            Add_Action (Table.States (28), 42, Reduce, (48, 7),  3);
            Table.States (29).Action_List.Set_Capacity (2);
            Add_Action (Table.States (29), (13, 20), (45, 3),  1);
            Table.States (30).Action_List.Set_Capacity (2);
            Add_Action (Table.States (30), 13, (48, 9), 68);
            Add_Action (Table.States (30), 20, (46, 1), 67);
            Table.States (31).Action_List.Set_Capacity (3);
            Add_Action (Table.States (31), (30, 39, 42), (48, 16),  3);
            Table.States (32).Action_List.Set_Capacity (2);
            Add_Action (Table.States (32), 10, (48, 15), 69);
            Add_Action (Table.States (32), 23, (48, 14), 70);
            Table.States (33).Action_List.Set_Capacity (2);
            Add_Action (Table.States (33), 10, (48, 13), 71);
            Add_Action (Table.States (33), 23, (48, 12), 72);
            Table.States (34).Action_List.Set_Capacity (3);
            Add_Action (Table.States (34), 18, (44, 0), 37);
            Add_Action (Table.States (34), 40, (44, 1), 40);
            Add_Action (Table.States (34), 41, (44, 2), 41);
            Table.States (34).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (34), 44, 73);
            Table.States (35).Action_List.Set_Capacity (1);
            Add_Action (Table.States (35), 39, (48, 2), 74);
            Table.States (36).Action_List.Set_Capacity (1);
            Add_Action (Table.States (36), 39, (48, 0), 75);
            Table.States (37).Action_List.Set_Capacity (7);
            Add_Action (Table.States (37), (18, 30, 38, 39, 40, 41, 42), (44, 0),  1);
            Table.States (38).Action_List.Set_Capacity (7);
            Add_Action (Table.States (38), (18, 30, 38, 39, 40, 41, 42), (51, 1),  1);
            Table.States (39).Action_List.Set_Capacity (7);
            Add_Action (Table.States (39), (18, 30, 38, 39, 40, 41, 42), (51, 0),  1);
            Table.States (40).Action_List.Set_Capacity (7);
            Add_Action (Table.States (40), (18, 30, 38, 39, 40, 41, 42), (44, 1),  1);
            Table.States (41).Action_List.Set_Capacity (7);
            Add_Action (Table.States (41), (18, 30, 38, 39, 40, 41, 42), (44, 2),  1);
            Table.States (42).Action_List.Set_Capacity (7);
            Add_Action (Table.States (42), (18, 30, 38, 39, 40, 41, 42), (51, 2),  1);
            Table.States (43).Action_List.Set_Capacity (7);
            Add_Action (Table.States (43), (18, 30, 38, 39, 40, 41, 42), (52, 0),  1);
            Table.States (44).Action_List.Set_Capacity (7);
            Add_Action (Table.States (44), 18, (44, 0), 37);
            Add_Action (Table.States (44), 30, Reduce, (48, 10),  3);
            Add_Action (Table.States (44), 38, (51, 1), 38);
            Add_Action (Table.States (44), 39, (51, 0), 39);
            Add_Conflict (Table.States (44), 39, (48, 10),  3);
            Add_Action (Table.States (44), 40, (44, 1), 40);
            Add_Action (Table.States (44), 41, (44, 2), 41);
            Add_Action (Table.States (44), 42, Reduce, (48, 10),  3);
            Table.States (44).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (44), 44, 42);
            Add_Goto (Table.States (44), 51, 76);
            Table.States (45).Action_List.Set_Capacity (6);
            Add_Action (Table.States (45), 25, (63, 0), 45);
            Add_Action (Table.States (45), 26, (62, 0), 46);
            Add_Action (Table.States (45), 27, (61, 0), 47);
            Add_Action (Table.States (45), 28, (57, 0), 48);
            Add_Action (Table.States (45), 39, (58, 1), 49);
            Add_Action (Table.States (45), 41, (60, 1), 50);
            Table.States (45).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (45), 57, 53);
            Add_Goto (Table.States (45), 58, 54);
            Add_Goto (Table.States (45), 59, 77);
            Add_Goto (Table.States (45), 60, 56);
            Add_Goto (Table.States (45), 61, 57);
            Add_Goto (Table.States (45), 62, 58);
            Add_Goto (Table.States (45), 63, 59);
            Add_Goto (Table.States (45), 64, 78);
            Table.States (46).Action_List.Set_Capacity (6);
            Add_Action (Table.States (46), 25, (63, 0), 45);
            Add_Action (Table.States (46), 26, (62, 0), 46);
            Add_Action (Table.States (46), 27, (61, 0), 47);
            Add_Action (Table.States (46), 28, (57, 0), 48);
            Add_Action (Table.States (46), 39, (58, 1), 49);
            Add_Action (Table.States (46), 41, (60, 1), 50);
            Table.States (46).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (46), 57, 53);
            Add_Goto (Table.States (46), 58, 54);
            Add_Goto (Table.States (46), 59, 77);
            Add_Goto (Table.States (46), 60, 56);
            Add_Goto (Table.States (46), 61, 57);
            Add_Goto (Table.States (46), 62, 58);
            Add_Goto (Table.States (46), 63, 59);
            Add_Goto (Table.States (46), 64, 79);
            Table.States (47).Action_List.Set_Capacity (6);
            Add_Action (Table.States (47), 25, (63, 0), 45);
            Add_Action (Table.States (47), 26, (62, 0), 46);
            Add_Action (Table.States (47), 27, (61, 0), 47);
            Add_Action (Table.States (47), 28, (57, 0), 48);
            Add_Action (Table.States (47), 39, (58, 1), 49);
            Add_Action (Table.States (47), 41, (60, 1), 50);
            Table.States (47).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (47), 57, 53);
            Add_Goto (Table.States (47), 58, 54);
            Add_Goto (Table.States (47), 59, 77);
            Add_Goto (Table.States (47), 60, 56);
            Add_Goto (Table.States (47), 61, 57);
            Add_Goto (Table.States (47), 62, 58);
            Add_Goto (Table.States (47), 63, 59);
            Add_Goto (Table.States (47), 64, 80);
            Table.States (48).Action_List.Set_Capacity (1);
            Add_Action (Table.States (48), 39, (57, 0), 81);
            Table.States (49).Action_List.Set_Capacity (18);
            Add_Action (Table.States (49), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 23, (58, 1), 82);
            Add_Action (Table.States (49), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 31, (63, 4), 83);
            Add_Action (Table.States (49), 32, (62, 2), 84);
            Add_Action (Table.States (49), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 37, (63, 5), 85);
            Add_Action (Table.States (49), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 41, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 42, Reduce, (60, 0),  1);
            Table.States (50).Action_List.Set_Capacity (15);
            Add_Action (Table.States (50), 19, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 30, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 32, (62, 3), 86);
            Add_Action (Table.States (50), 33, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 34, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 35, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 36, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 41, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 42, Reduce, (60, 1),  1);
            Table.States (51).Action_List.Set_Capacity (5);
            Add_Action (Table.States (51), 20, (55, 1), 87);
            Add_Action (Table.States (51), 30, (55, 2), 88);
            Add_Conflict (Table.States (51), 30, (54, 1),  0);
            Add_Action (Table.States (51), 36, (54, 0), 89);
            Add_Action (Table.States (51), 39, Reduce, (54, 1),  0);
            Add_Action (Table.States (51), 42, Reduce, (54, 1),  0);
            Table.States (51).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (51), 54, 90);
            Table.States (52).Action_List.Set_Capacity (5);
            Add_Action (Table.States (52), (20, 30, 36, 39, 42), (55, 0),  1);
            Table.States (53).Action_List.Set_Capacity (14);
            Add_Action (Table.States (53), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 2),  1);
            Table.States (54).Action_List.Set_Capacity (14);
            Add_Action (Table.States (54), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (59, 0),  1);
            Table.States (55).Action_List.Set_Capacity (11);
            Add_Action (Table.States (55), 19, (56, 2), 91);
            Add_Action (Table.States (55), 20, Reduce, (56, 1),  1);
            Add_Action (Table.States (55), 25, (63, 0), 45);
            Add_Action (Table.States (55), 26, (62, 0), 46);
            Add_Action (Table.States (55), 27, (61, 0), 47);
            Add_Action (Table.States (55), 28, (57, 0), 48);
            Add_Action (Table.States (55), 30, Reduce, (56, 1),  1);
            Add_Action (Table.States (55), 36, Reduce, (56, 1),  1);
            Add_Action (Table.States (55), 39, (58, 1), 49);
            Add_Conflict (Table.States (55), 39, (56, 1),  1);
            Add_Action (Table.States (55), 41, (60, 1), 50);
            Add_Action (Table.States (55), 42, Reduce, (56, 1),  1);
            Table.States (55).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (55), 57, 53);
            Add_Goto (Table.States (55), 58, 92);
            Add_Goto (Table.States (55), 60, 56);
            Add_Goto (Table.States (55), 61, 57);
            Add_Goto (Table.States (55), 62, 58);
            Add_Goto (Table.States (55), 63, 59);
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (56).Action_List.Set_Capacity (14);
            Add_Action (Table.States (56), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (58, 0),  1);
            Table.States (57).Action_List.Set_Capacity (14);
            Add_Action (Table.States (57), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 5),  1);
            Table.States (58).Action_List.Set_Capacity (14);
            Add_Action (Table.States (58), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 3),  1);
            Table.States (59).Action_List.Set_Capacity (14);
            Add_Action (Table.States (59), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (60, 4),  1);
            Table.States (60).Action_List.Set_Capacity (5);
            Add_Action (Table.States (60), 20, (55, 1), 87);
            Add_Action (Table.States (60), 30, (55, 2), 88);
            Add_Conflict (Table.States (60), 30, (54, 1),  0);
            Add_Action (Table.States (60), 36, (54, 0), 89);
            Add_Action (Table.States (60), 39, Reduce, (54, 1),  0);
            Add_Action (Table.States (60), 42, Reduce, (54, 1),  0);
            Table.States (60).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (60), 54, 93);
            Table.States (61).Action_List.Set_Capacity (3);
            Add_Action (Table.States (61), (30, 39, 42), (48, 6),  4);
            Table.States (62).Action_List.Set_Capacity (4);
            Add_Action (Table.States (62), (17, 30, 39, 42), (49, 1),  2);
            Table.States (63).Action_List.Set_Capacity (2);
            Add_Action (Table.States (63), (13, 20), (45, 2),  2);
            Table.States (64).Action_List.Set_Capacity (2);
            Add_Action (Table.States (64), (13, 20), (45, 1),  2);
            Table.States (65).Action_List.Set_Capacity (2);
            Add_Action (Table.States (65), (13, 20), (45, 0),  2);
            Table.States (66).Action_List.Set_Capacity (1);
            Add_Action (Table.States (66), 16, (48, 8), 94);
            Table.States (67).Action_List.Set_Capacity (4);
            Add_Action (Table.States (67), 3, (45, 2), 22);
            Add_Action (Table.States (67), 14, (45, 1), 23);
            Add_Action (Table.States (67), 15, (45, 0), 24);
            Add_Action (Table.States (67), 39, (45, 3), 29);
            Table.States (67).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (67), 45, 95);
            Table.States (68).Action_List.Set_Capacity (1);
            Add_Action (Table.States (68), 16, (48, 9), 96);
            Table.States (69).Action_List.Set_Capacity (1);
            Add_Action (Table.States (69), 39, (50, 0), 97);
            Table.States (69).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (69), 50, 98);
            Table.States (70).Action_List.Set_Capacity (1);
            Add_Action (Table.States (70), 39, (48, 14), 99);
            Table.States (71).Action_List.Set_Capacity (1);
            Add_Action (Table.States (71), 39, (50, 0), 97);
            Table.States (71).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (71), 50, 100);
            Table.States (72).Action_List.Set_Capacity (1);
            Add_Action (Table.States (72), 39, (48, 12), 101);
            Table.States (73).Action_List.Set_Capacity (3);
            Add_Action (Table.States (73), (30, 39, 42), (48, 5),  4);
            Table.States (74).Action_List.Set_Capacity (1);
            Add_Action (Table.States (74), 24, (48, 2), 102);
            Table.States (75).Action_List.Set_Capacity (1);
            Add_Action (Table.States (75), 24, (48, 0), 103);
            Table.States (76).Action_List.Set_Capacity (7);
            Add_Action (Table.States (76), (18, 30, 38, 39, 40, 41, 42), (52, 1),  2);
            Table.States (77).Action_List.Set_Capacity (10);
            Add_Action (Table.States (77), 20, Reduce, (64, 0),  1);
            Add_Action (Table.States (77), 25, (63, 0), 45);
            Add_Action (Table.States (77), 26, (62, 0), 46);
            Add_Action (Table.States (77), 27, (61, 0), 47);
            Add_Action (Table.States (77), 28, (57, 0), 48);
            Add_Action (Table.States (77), 33, Reduce, (64, 0),  1);
            Add_Action (Table.States (77), 34, Reduce, (64, 0),  1);
            Add_Action (Table.States (77), 35, Reduce, (64, 0),  1);
            Add_Action (Table.States (77), 39, (58, 1), 49);
            Add_Action (Table.States (77), 41, (60, 1), 50);
            Table.States (77).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (77), 57, 53);
            Add_Goto (Table.States (77), 58, 92);
            Add_Goto (Table.States (77), 60, 56);
            Add_Goto (Table.States (77), 61, 57);
            Add_Goto (Table.States (77), 62, 58);
            Add_Goto (Table.States (77), 63, 59);
            Table.States (78).Action_List.Set_Capacity (2);
            Add_Action (Table.States (78), 20, (64, 1), 104);
            Add_Action (Table.States (78), 33, (63, 0), 105);
            Table.States (79).Action_List.Set_Capacity (2);
            Add_Action (Table.States (79), 20, (64, 1), 104);
            Add_Action (Table.States (79), 34, (62, 0), 106);
            Table.States (80).Action_List.Set_Capacity (2);
            Add_Action (Table.States (80), 20, (64, 1), 104);
            Add_Action (Table.States (80), 35, (61, 0), 107);
            Table.States (81).Action_List.Set_Capacity (1);
            Add_Action (Table.States (81), 23, (57, 0), 108);
            Table.States (82).Action_List.Set_Capacity (6);
            Add_Action (Table.States (82), 25, (63, 0), 45);
            Add_Action (Table.States (82), 26, (62, 0), 46);
            Add_Action (Table.States (82), 27, (61, 0), 47);
            Add_Action (Table.States (82), 28, (57, 0), 48);
            Add_Action (Table.States (82), 39, (60, 0), 109);
            Add_Action (Table.States (82), 41, (60, 1), 50);
            Table.States (82).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (82), 57, 53);
            Add_Goto (Table.States (82), 60, 110);
            Add_Goto (Table.States (82), 61, 57);
            Add_Goto (Table.States (82), 62, 58);
            Add_Goto (Table.States (82), 63, 59);
            Table.States (83).Action_List.Set_Capacity (14);
            Add_Action (Table.States (83), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 4),  2);
            Table.States (84).Action_List.Set_Capacity (14);
            Add_Action (Table.States (84), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 2),  2);
            Table.States (85).Action_List.Set_Capacity (14);
            Add_Action (Table.States (85), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 5),  2);
            Table.States (86).Action_List.Set_Capacity (14);
            Add_Action (Table.States (86), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 3),  2);
            Table.States (87).Action_List.Set_Capacity (10);
            Add_Action (Table.States (87), 20, Reduce, (56, 0),  0);
            Add_Action (Table.States (87), 25, (63, 0), 45);
            Add_Action (Table.States (87), 26, (62, 0), 46);
            Add_Action (Table.States (87), 27, (61, 0), 47);
            Add_Action (Table.States (87), 28, (57, 0), 48);
            Add_Action (Table.States (87), 30, Reduce, (56, 0),  0);
            Add_Action (Table.States (87), 36, Reduce, (56, 0),  0);
            Add_Action (Table.States (87), 39, (58, 1), 49);
            Add_Conflict (Table.States (87), 39, (56, 0),  0);
            Add_Action (Table.States (87), 41, (60, 1), 50);
            Add_Action (Table.States (87), 42, Reduce, (56, 0),  0);
            Table.States (87).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (87), 56, 111);
            Add_Goto (Table.States (87), 57, 53);
            Add_Goto (Table.States (87), 58, 54);
            Add_Goto (Table.States (87), 59, 55);
            Add_Goto (Table.States (87), 60, 56);
            Add_Goto (Table.States (87), 61, 57);
            Add_Goto (Table.States (87), 62, 58);
            Add_Goto (Table.States (87), 63, 59);
            Table.States (88).Action_List.Set_Capacity (3);
            Add_Action (Table.States (88), 7, (55, 6), 112);
            Add_Action (Table.States (88), 8, (55, 4), 113);
            Add_Action (Table.States (88), 9, (55, 2), 114);
            Table.States (89).Action_List.Set_Capacity (3);
            Add_Action (Table.States (89), (30, 39, 42), (54, 0),  1);
            Table.States (90).Action_List.Set_Capacity (3);
            Add_Action (Table.States (90), (30, 39, 42), (53, 0),  4);
            Table.States (91).Action_List.Set_Capacity (6);
            Add_Action (Table.States (91), 19, (56, 3), 115);
            Add_Action (Table.States (91), 20, Reduce, (56, 2),  2);
            Add_Action (Table.States (91), 30, Reduce, (56, 2),  2);
            Add_Action (Table.States (91), 36, Reduce, (56, 2),  2);
            Add_Action (Table.States (91), 39, Reduce, (56, 2),  2);
            Add_Action (Table.States (91), 42, Reduce, (56, 2),  2);
            Table.States (92).Action_List.Set_Capacity (14);
            Add_Action (Table.States (92), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (59, 1),  2);
            Table.States (93).Action_List.Set_Capacity (3);
            Add_Action (Table.States (93), (30, 39, 42), (53, 1),  4);
            Table.States (94).Action_List.Set_Capacity (2);
            Add_Action (Table.States (94), 39, (47, 0), 116);
            Add_Action (Table.States (94), 41, (47, 1), 117);
            Table.States (94).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (94), 47, 118);
            Table.States (95).Action_List.Set_Capacity (2);
            Add_Action (Table.States (95), (13, 20), (46, 1),  3);
            Table.States (96).Action_List.Set_Capacity (2);
            Add_Action (Table.States (96), 39, (47, 0), 116);
            Add_Action (Table.States (96), 41, (47, 1), 117);
            Table.States (96).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (96), 47, 119);
            Table.States (97).Action_List.Set_Capacity (5);
            Add_Action (Table.States (97), (20, 30, 36, 39, 42), (50, 0),  1);
            Table.States (98).Action_List.Set_Capacity (4);
            Add_Action (Table.States (98), 20, (50, 1), 120);
            Add_Action (Table.States (98), 30, Reduce, (48, 15),  5);
            Add_Action (Table.States (98), 39, Reduce, (48, 15),  5);
            Add_Action (Table.States (98), 42, Reduce, (48, 15),  5);
            Table.States (99).Action_List.Set_Capacity (3);
            Add_Action (Table.States (99), (30, 39, 42), (48, 14),  5);
            Table.States (100).Action_List.Set_Capacity (4);
            Add_Action (Table.States (100), 20, (50, 1), 120);
            Add_Action (Table.States (100), 30, Reduce, (48, 13),  5);
            Add_Action (Table.States (100), 39, Reduce, (48, 13),  5);
            Add_Action (Table.States (100), 42, Reduce, (48, 13),  5);
            Table.States (101).Action_List.Set_Capacity (3);
            Add_Action (Table.States (101), (30, 39, 42), (48, 12),  5);
            Table.States (102).Action_List.Set_Capacity (1);
            Add_Action (Table.States (102), 39, (48, 2), 121);
            Table.States (103).Action_List.Set_Capacity (1);
            Add_Action (Table.States (103), 39, (48, 0), 122);
            Table.States (104).Action_List.Set_Capacity (6);
            Add_Action (Table.States (104), 25, (63, 0), 45);
            Add_Action (Table.States (104), 26, (62, 0), 46);
            Add_Action (Table.States (104), 27, (61, 0), 47);
            Add_Action (Table.States (104), 28, (57, 0), 48);
            Add_Action (Table.States (104), 39, (58, 1), 49);
            Add_Action (Table.States (104), 41, (60, 1), 50);
            Table.States (104).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (104), 57, 53);
            Add_Goto (Table.States (104), 58, 54);
            Add_Goto (Table.States (104), 59, 123);
            Add_Goto (Table.States (104), 60, 56);
            Add_Goto (Table.States (104), 61, 57);
            Add_Goto (Table.States (104), 62, 58);
            Add_Goto (Table.States (104), 63, 59);
            Table.States (105).Action_List.Set_Capacity (15);
            Add_Action (Table.States (105), 19, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 29, (63, 1), 124);
            Add_Action (Table.States (105), 30, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 33, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 34, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 35, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 36, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 41, Reduce, (63, 0),  3);
            Add_Action (Table.States (105), 42, Reduce, (63, 0),  3);
            Table.States (106).Action_List.Set_Capacity (14);
            Add_Action (Table.States (106), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 0),  3);
            Table.States (107).Action_List.Set_Capacity (17);
            Add_Action (Table.States (107), 19, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 30, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 31, (63, 2), 125);
            Add_Action (Table.States (107), 32, (62, 1), 126);
            Add_Action (Table.States (107), 33, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 34, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 35, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 36, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 37, (63, 3), 127);
            Add_Action (Table.States (107), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 41, Reduce, (61, 0),  3);
            Add_Action (Table.States (107), 42, Reduce, (61, 0),  3);
            Table.States (108).Action_List.Set_Capacity (1);
            Add_Action (Table.States (108), 39, (57, 0), 128);
            Table.States (109).Action_List.Set_Capacity (17);
            Add_Action (Table.States (109), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 31, (63, 4), 83);
            Add_Action (Table.States (109), 32, (62, 2), 84);
            Add_Action (Table.States (109), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 37, (63, 5), 85);
            Add_Action (Table.States (109), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 41, Reduce, (60, 0),  1);
            Add_Action (Table.States (109), 42, Reduce, (60, 0),  1);
            Table.States (110).Action_List.Set_Capacity (14);
            Add_Action (Table.States (110), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (58, 1),  3);
            Table.States (111).Action_List.Set_Capacity (5);
            Add_Action (Table.States (111), (20, 30, 36, 39, 42), (55, 1),  3);
            Table.States (112).Action_List.Set_Capacity (1);
            Add_Action (Table.States (112), 9, (55, 6), 129);
            Table.States (113).Action_List.Set_Capacity (1);
            Add_Action (Table.States (113), 39, (55, 4), 130);
            Table.States (114).Action_List.Set_Capacity (1);
            Add_Action (Table.States (114), 39, (55, 2), 131);
            Table.States (115).Action_List.Set_Capacity (5);
            Add_Action (Table.States (115), (20, 30, 36, 39, 42), (56, 3),  3);
            Table.States (116).Action_List.Set_Capacity (4);
            Add_Action (Table.States (116), (21, 30, 39, 42), (47, 0),  1);
            Table.States (117).Action_List.Set_Capacity (4);
            Add_Action (Table.States (117), (21, 30, 39, 42), (47, 1),  1);
            Table.States (118).Action_List.Set_Capacity (3);
            Add_Action (Table.States (118), (30, 39, 42), (48, 8),  6);
            Table.States (119).Action_List.Set_Capacity (1);
            Add_Action (Table.States (119), 21, (48, 9), 132);
            Table.States (120).Action_List.Set_Capacity (1);
            Add_Action (Table.States (120), 39, (50, 1), 133);
            Table.States (121).Action_List.Set_Capacity (6);
            Add_Action (Table.States (121), 18, (44, 0), 37);
            Add_Action (Table.States (121), 30, Reduce, (48, 4),  6);
            Add_Action (Table.States (121), 39, Reduce, (48, 4),  6);
            Add_Action (Table.States (121), 40, (44, 1), 40);
            Add_Action (Table.States (121), 41, (44, 2), 41);
            Add_Action (Table.States (121), 42, Reduce, (48, 4),  6);
            Table.States (121).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (121), 44, 134);
            Table.States (122).Action_List.Set_Capacity (3);
            Add_Action (Table.States (122), 18, (44, 0), 37);
            Add_Action (Table.States (122), 40, (44, 1), 40);
            Add_Action (Table.States (122), 41, (44, 2), 41);
            Table.States (122).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (122), 44, 135);
            Table.States (123).Action_List.Set_Capacity (10);
            Add_Action (Table.States (123), 20, Reduce, (64, 1),  3);
            Add_Action (Table.States (123), 25, (63, 0), 45);
            Add_Action (Table.States (123), 26, (62, 0), 46);
            Add_Action (Table.States (123), 27, (61, 0), 47);
            Add_Action (Table.States (123), 28, (57, 0), 48);
            Add_Action (Table.States (123), 33, Reduce, (64, 1),  3);
            Add_Action (Table.States (123), 34, Reduce, (64, 1),  3);
            Add_Action (Table.States (123), 35, Reduce, (64, 1),  3);
            Add_Action (Table.States (123), 39, (58, 1), 49);
            Add_Action (Table.States (123), 41, (60, 1), 50);
            Table.States (123).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (123), 57, 53);
            Add_Goto (Table.States (123), 58, 92);
            Add_Goto (Table.States (123), 60, 56);
            Add_Goto (Table.States (123), 61, 57);
            Add_Goto (Table.States (123), 62, 58);
            Add_Goto (Table.States (123), 63, 59);
            Table.States (124).Action_List.Set_Capacity (14);
            Add_Action (Table.States (124), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 1),  4);
            Table.States (125).Action_List.Set_Capacity (14);
            Add_Action (Table.States (125), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 2),  4);
            Table.States (126).Action_List.Set_Capacity (14);
            Add_Action (Table.States (126), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (62, 1),  4);
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (127).Action_List.Set_Capacity (14);
            Add_Action (Table.States (127), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (63, 3),  4);
            Table.States (128).Action_List.Set_Capacity (1);
            Add_Action (Table.States (128), 24, (57, 0), 136);
            Table.States (129).Action_List.Set_Capacity (5);
            Add_Action (Table.States (129), (20, 30, 36, 39, 42), (55, 6),  4);
            Table.States (130).Action_List.Set_Capacity (2);
            Add_Action (Table.States (130), 10, (55, 5), 137);
            Add_Action (Table.States (130), 23, (55, 4), 138);
            Table.States (131).Action_List.Set_Capacity (2);
            Add_Action (Table.States (131), 10, (55, 3), 139);
            Add_Action (Table.States (131), 23, (55, 2), 140);
            Table.States (132).Action_List.Set_Capacity (1);
            Add_Action (Table.States (132), 39, (48, 9), 141);
            Table.States (133).Action_List.Set_Capacity (5);
            Add_Action (Table.States (133), (20, 30, 36, 39, 42), (50, 1),  3);
            Table.States (134).Action_List.Set_Capacity (6);
            Add_Action (Table.States (134), 18, (44, 0), 37);
            Add_Action (Table.States (134), 30, Reduce, (48, 3),  7);
            Add_Action (Table.States (134), 39, Reduce, (48, 3),  7);
            Add_Action (Table.States (134), 40, (44, 1), 40);
            Add_Action (Table.States (134), 41, (44, 2), 41);
            Add_Action (Table.States (134), 42, Reduce, (48, 3),  7);
            Table.States (134).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (134), 44, 142);
            Table.States (135).Action_List.Set_Capacity (6);
            Add_Action (Table.States (135), 18, (44, 0), 37);
            Add_Action (Table.States (135), 30, Reduce, (48, 1),  7);
            Add_Action (Table.States (135), 39, Reduce, (48, 1),  7);
            Add_Action (Table.States (135), 40, (44, 1), 40);
            Add_Action (Table.States (135), 41, (44, 2), 41);
            Add_Action (Table.States (135), 42, Reduce, (48, 1),  7);
            Table.States (135).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (135), 44, 143);
            Table.States (136).Action_List.Set_Capacity (14);
            Add_Action (Table.States (136), (19, 20, 25, 26, 27, 28, 30, 33, 34, 35, 36, 39, 41, 42), (57, 0),  5);
            Table.States (137).Action_List.Set_Capacity (1);
            Add_Action (Table.States (137), 39, (50, 0), 97);
            Table.States (137).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (137), 50, 144);
            Table.States (138).Action_List.Set_Capacity (1);
            Add_Action (Table.States (138), 39, (55, 4), 145);
            Table.States (139).Action_List.Set_Capacity (1);
            Add_Action (Table.States (139), 39, (50, 0), 97);
            Table.States (139).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (139), 50, 146);
            Table.States (140).Action_List.Set_Capacity (1);
            Add_Action (Table.States (140), 39, (55, 2), 147);
            Table.States (141).Action_List.Set_Capacity (3);
            Add_Action (Table.States (141), (30, 39, 42), (48, 9),  8);
            Table.States (142).Action_List.Set_Capacity (3);
            Add_Action (Table.States (142), (30, 39, 42), (48, 2),  8);
            Table.States (143).Action_List.Set_Capacity (3);
            Add_Action (Table.States (143), (30, 39, 42), (48, 0),  8);
            Table.States (144).Action_List.Set_Capacity (5);
            Add_Action (Table.States (144), 20, (50, 1), 120);
            Add_Conflict (Table.States (144), 20, (55, 5),  6);
            Add_Action (Table.States (144), 30, Reduce, (55, 5),  6);
            Add_Action (Table.States (144), 36, Reduce, (55, 5),  6);
            Add_Action (Table.States (144), 39, Reduce, (55, 5),  6);
            Add_Action (Table.States (144), 42, Reduce, (55, 5),  6);
            Table.States (145).Action_List.Set_Capacity (5);
            Add_Action (Table.States (145), (20, 30, 36, 39, 42), (55, 4),  6);
            Table.States (146).Action_List.Set_Capacity (5);
            Add_Action (Table.States (146), 20, (50, 1), 120);
            Add_Conflict (Table.States (146), 20, (55, 3),  6);
            Add_Action (Table.States (146), 30, Reduce, (55, 3),  6);
            Add_Action (Table.States (146), 36, Reduce, (55, 3),  6);
            Add_Action (Table.States (146), 39, Reduce, (55, 3),  6);
            Add_Action (Table.States (146), 42, Reduce, (55, 3),  6);
            Table.States (147).Action_List.Set_Capacity (5);
            Add_Action (Table.States (147), (20, 30, 36, 39, 42), (55, 2),  6);
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

   function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector
   is begin
      return Result : WisiToken.Syntax_Trees.Production_Info_Trees.Vector do
         Result.Set_First_Last (43, 66);
         Result (48).RHSs.Set_First_Last (0, 16);
         Result (48).RHSs (0).In_Parse_Action := null;
         Result (48).RHSs (0).Post_Parse_Action := declaration_0'Access;
         Result (48).RHSs (1).In_Parse_Action := null;
         Result (48).RHSs (1).Post_Parse_Action := declaration_1'Access;
         Result (48).RHSs (2).In_Parse_Action := null;
         Result (48).RHSs (2).Post_Parse_Action := declaration_2'Access;
         Result (48).RHSs (3).In_Parse_Action := null;
         Result (48).RHSs (3).Post_Parse_Action := declaration_3'Access;
         Result (48).RHSs (4).In_Parse_Action := null;
         Result (48).RHSs (4).Post_Parse_Action := declaration_4'Access;
         Result (48).RHSs (5).In_Parse_Action := null;
         Result (48).RHSs (5).Post_Parse_Action := declaration_5'Access;
         Result (48).RHSs (6).In_Parse_Action := null;
         Result (48).RHSs (6).Post_Parse_Action := declaration_6'Access;
         Result (48).RHSs (7).In_Parse_Action := null;
         Result (48).RHSs (7).Post_Parse_Action := declaration_7'Access;
         Result (48).RHSs (8).In_Parse_Action := null;
         Result (48).RHSs (8).Post_Parse_Action := declaration_8'Access;
         Result (48).RHSs (9).In_Parse_Action := null;
         Result (48).RHSs (9).Post_Parse_Action := declaration_9'Access;
         Result (48).RHSs (10).In_Parse_Action := null;
         Result (48).RHSs (10).Post_Parse_Action := declaration_10'Access;
         Result (48).RHSs (11).In_Parse_Action := null;
         Result (48).RHSs (11).Post_Parse_Action := declaration_11'Access;
         Result (48).RHSs (12).In_Parse_Action := null;
         Result (48).RHSs (12).Post_Parse_Action := declaration_12'Access;
         Result (48).RHSs (13).In_Parse_Action := null;
         Result (48).RHSs (13).Post_Parse_Action := declaration_13'Access;
         Result (48).RHSs (14).In_Parse_Action := null;
         Result (48).RHSs (14).Post_Parse_Action := declaration_14'Access;
         Result (48).RHSs (15).In_Parse_Action := null;
         Result (48).RHSs (15).Post_Parse_Action := declaration_15'Access;
         Result (48).RHSs (16).In_Parse_Action := null;
         Result (48).RHSs (16).Post_Parse_Action := declaration_16'Access;
         Result (53).RHSs.Set_First_Last (0, 1);
         Result (53).RHSs (0).In_Parse_Action := null;
         Result (53).RHSs (0).Post_Parse_Action := nonterminal_0'Access;
         Result (53).RHSs (1).In_Parse_Action := null;
         Result (53).RHSs (1).Post_Parse_Action := nonterminal_1'Access;
         Result (60).RHSs.Set_First_Last (0, 5);
         Result (60).RHSs (0).In_Parse_Action := null;
         Result (60).RHSs (0).Post_Parse_Action := null;
         Result (60).RHSs (1).In_Parse_Action := null;
         Result (60).RHSs (1).Post_Parse_Action := rhs_item_1'Access;
         Result (60).RHSs (2).In_Parse_Action := null;
         Result (60).RHSs (2).Post_Parse_Action := rhs_item_2'Access;
         Result (60).RHSs (3).In_Parse_Action := null;
         Result (60).RHSs (3).Post_Parse_Action := rhs_item_3'Access;
         Result (60).RHSs (4).In_Parse_Action := null;
         Result (60).RHSs (4).Post_Parse_Action := rhs_item_4'Access;
         Result (60).RHSs (5).In_Parse_Action := null;
         Result (60).RHSs (5).Post_Parse_Action := rhs_item_5'Access;
         Result (62).RHSs.Set_First_Last (0, 3);
         Result (62).RHSs (0).In_Parse_Action := null;
         Result (62).RHSs (0).Post_Parse_Action := null;
         Result (62).RHSs (1).In_Parse_Action := null;
         Result (62).RHSs (1).Post_Parse_Action := null;
         Result (62).RHSs (2).In_Parse_Action := null;
         Result (62).RHSs (2).Post_Parse_Action := null;
         Result (62).RHSs (3).In_Parse_Action := null;
         Result (62).RHSs (3).Post_Parse_Action := rhs_optional_item_3'Access;
      end return;
   end Create_Productions;

   function Create_Parser
     (Trace      : in WisiToken.Trace_Access;
      User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.LR.Parser_No_Recover.Parser
   is begin
      return Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser do
         Parser.Tree.Lexer := Lexer.New_Lexer (Trace, Wisitoken_Grammar_Actions.Descriptor'Access);
         Parser.Productions := Create_Productions;
         Parser.User_Data := User_Data;
         Parser.Table := Create_Parse_Table;
      end return;
   end Create_Parser;
end Wisitoken_Grammar_Main;
