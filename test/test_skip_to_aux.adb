--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Text_IO;
with WisiToken.AUnit; use WisiToken.AUnit;
with AUnit.Checks.Containers;
with WisiToken.Lexer;
package body Test_Skip_To_Aux is

   use WisiToken;

   Decl_Count : Integer := 0;
   Comp_Count : Integer := 0;

   procedure Check
     (Label             : in String;
      Lines             : in Line_Region;
      Chars             : in Buffer_Region;
      Bytes             : in Buffer_Region;
      Expected_Lines    : in Line_Region;
      Expected_Chars    : in Buffer_Region;
      Non_Ascii_Offset_First : in Base_Buffer_Pos;
      Non_Ascii_Offset_Last  : in Base_Buffer_Pos)
   is begin
      Check (Label & ".line_region", Lines, Expected_Lines);
      Check (Label & ".char region", Chars, Expected_Chars);

      --  Bytes differs from chars because of non-ascii and DOS line
      --  endings.
      Check (Label & ".byte region", Bytes,
             (Chars.First + (if DOS_Line_Endings then Buffer_Pos (Lines.First) - 1 else 0) + Non_Ascii_Offset_First,
              Chars.Last + (if DOS_Line_Endings then Buffer_Pos (Lines.Last) - 1 else 0) + Non_Ascii_Offset_Last));
   end Check;

   procedure Test_1
     (Label           : in String;
      Nonterm         : in Syntax_Trees.Node_Access;
      Expected_Lines  : in Line_Region;
      Expected_Chars  : in Buffer_Region;
      Non_Ascii_Offset_First : in Base_Buffer_Pos;
      Non_Ascii_Offset_Last  : in Base_Buffer_Pos)
   is begin
      Check
        (Label,
         Lines             => Parser.Tree.Line_Region (Nonterm, Trailing_Non_Grammar => False),
         Chars             => Parser.Tree.Char_Region (Nonterm),
         Bytes             => Parser.Tree.Byte_Region (Nonterm),
         Expected_Lines    => Expected_Lines,
         Expected_Chars    => Expected_Chars,
         Non_Ascii_Offset_First => Non_Ascii_Offset_First,
         Non_Ascii_Offset_Last  => Non_Ascii_Offset_Last);
   end Test_1;

   procedure Test_Comment
     (Label           : in String;
      Token           : in WisiToken.Lexer.Token;
      Expected_Lines  : in Line_Region;
      Expected_Chars  : in Buffer_Region;
      Non_Ascii_Offset_First : in Base_Buffer_Pos;
      Non_Ascii_Offset_Last  : in Base_Buffer_Pos)
   is begin
      Check
        (Label,
         Lines             => Token.Line_Region,
         Chars             => Token.Char_Region,
         Bytes             => Token.Byte_Region,
         Expected_Lines    => Expected_Lines,
         Expected_Chars    => Expected_Chars,
         Non_Ascii_Offset_First => Non_Ascii_Offset_First,
         Non_Ascii_Offset_Last  => Non_Ascii_Offset_Last);
   end Test_Comment;

   procedure Reset
   is begin
      Test_Pass_Count := 0;
      Decl_Count      := 0;
      Comp_Count      := 0;
   end Reset;

   procedure Test_Declaration_0 (Nonterm : in Syntax_Trees.Valid_Node_Access)
   is
      Label : constant String := "declaration_0";
   begin
      if Enable then
            Decl_Count := Decl_Count + 1;
            if Trace_Tests > Detail then
               Ada.Text_IO.Put_Line ("Test_Declaration_0" & Decl_Count'Image);
            end if;

            --  File has either DOS or Unix line endings and non-ASCII chars.
            --
            --  Char_Region from wisi-show-region in .input file (with point _before_ last char)
            --
            --  byte offset from counting non-ascii excess bytes; DOS line endings added in Check.

         case Decl_Count is
         when 1                       =>
            Test_1
              (Label & ".RANGE", Nonterm,
               Expected_Lines         => (7, 7),
               Expected_Chars         => (40, 57),
               Non_Ascii_Offset_First => 0,
               Non_Ascii_Offset_Last  => 0);

         when 2 =>
            Test_1 (Label & ".X1_Non", Nonterm, (8, 8), (59, 76), 0, 2);

         when 3 =>
            Test_1 (Label & ".X2_Non", Nonterm, (9, 9), (109, 126), 2, 4);

         when 4 =>
            Test_1 (Label & ".X3_Non", Nonterm, (10, 10), (154, 171), 4, 5);

         when others =>
            raise Fatal_Error;
         end case;
      end if;
      Test_Pass_Count := @ + 1;
   end Test_Declaration_0;

   procedure Test_Compilation_Unit_0 (Nonterm : in Syntax_Trees.Valid_Node_Access)
   is
   begin
      if Enable then
         Comp_Count := Comp_Count + 1;
         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Test_Compilation_Unit_0" & Comp_Count'Image);
         end if;
         declare
            use AUnit.Checks.Containers;
            Label : constant String := "compilation_unit_0 PREAMBLE" & "." & Comp_Count'Image;
            Non_Grammar : WisiToken.Lexer.Token_Arrays.Vector renames Parser.Tree.Non_Grammar_Const
              (Parser.Tree.Last_Terminal (Nonterm));
         begin
            --  See comment in Test_Declaration_0 for source of expected values.

            case Comp_Count is
            when 1 =>
               --  First delimited text
               Test_1 (Label, Nonterm, (1, 3), (1, 20), 0, 0);

               Check (Label & ".non_grammar.length", Non_Grammar.Length, 5);
               Test_Comment (Label & ".comment 1", Non_Grammar (3), (5, 5), (23, 37), 0, 0);

            when 2 =>
               --  Second delimited text, with non-ascii.
               Test_1 (Label, Nonterm, (12, 16), (196, 264), 5, 6);

               Check (Label & ".non_grammar.length", Non_Grammar.Length, 6);
               Test_Comment (Label & ".comment 1", Non_Grammar (3), (18, 18), (267, 289), 6, 7); -- placeholder

               --  End of file comment is slightly different between the two files.
               if DOS_Line_Endings then
                  Test_Comment (Label & ".comment 2", Non_Grammar (6), (20, 21), (292, 346), 7, 7);
               else
                  Test_Comment (Label & ".comment 2", Non_Grammar (6), (20, 21), (292, 347), 7, 7);
               end if;
            when others =>
               raise Fatal_Error;
            end case;
         end;
      end if;
      Test_Pass_Count := @ + 1;
   end Test_Compilation_Unit_0;

end Test_Skip_To_Aux;
