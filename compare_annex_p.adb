--  Abstract :
--
--  Compare ARM Annex P to ada_annex_p.wy
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
procedure Compare_Annex_P
is
   Arm_Source : constant String := "c:/Projects/org.adaic.arm_form/build/aarm2020.txt/aa-P.TXT";
   --  WY_Source : constant String := "ada_annex_p.wy";

   type String_Array is array (Positive range <>) of Ada.Strings.Unbounded.String_Access;
   type Boolean_Array is array (Positive range <>) of Boolean;

   Keywords : constant String_Array :=
     (new String'("abs"),
      new String'("accept"),
      new String'("abort"),
      new String'("abstract"),
      new String'("access"),
      new String'("aliased"),
      new String'("all"),
      new String'("and"),
      new String'("array"),
      new String'("at"),
      new String'("begin"),
      new String'("case"),
      new String'("constant"),
      new String'("declare"),
      new String'("delay"),
      new String'("delta"),
      new String'("digits"),
      new String'("do"),
      new String'("else"),
      new String'("elsif"),
      new String'("end"),
      new String'("entry"),
      new String'("exception"),
      new String'("exit"),
      new String'("for"),
      new String'("function"),
      new String'("generic"),
      new String'("goto"),
      new String'("if"),
      new String'("in"),
      new String'("interface"),
      new String'("is"),
      new String'("limited"),
      new String'("loop"),
      new String'("mod"),
      new String'("new"),
      new String'("not"),
      new String'("null"),
      new String'("of"),
      new String'("or"),
      new String'("others"),
      new String'("out"),
      new String'("overriding"),
      new String'("package"),
      new String'("parallel"),
      new String'("private"),
      new String'("procedure"),
      new String'("protected"),
      new String'("raise"),
      new String'("record"),
      new String'("rem"),
      new String'("renames"),
      new String'("requeue"),
      new String'("return"),
      new String'("reverse"),
      new String'("separate"),
      new String'("select"),
      new String'("some"),
      new String'("subtype"),
      new String'("synchronized"),
      new String'("tagged"),
      new String'("task"),
      new String'("terminate"),
      new String'("then"),
      new String'("type"),
      new String'("until"),
      new String'("use"),
      new String'("when"),
      new String'("while"),
      new String'("with"),
      new String'("xor"));

   Special_Keywords : constant String_Array :=
     (new String'("body"),
      new String'("pragma"),
      new String'("range"));

   procedure Read_Arm (Arm_Source : in String; Arm_WY_Output : in String)
   --  Read Arm_Source (a .TXT file from org.adaic.arm_form), output the
   --  equivalent WisiToken .wy file.
   is
      Special_Seen : Boolean_Array (Special_Keywords'First .. Special_Keywords'Last) := (others => False);

      function Quote_Tokens (Line : in String) return String
      --  Quote literal tokens
      is
         I      : Integer := Line'First;
         Result : String (Line'First .. Line'First + 2 * Line'Length);
         Last   : Integer := Result'First - 1;

         function Handle_Keyword return Boolean
         is begin
            for Word of Keywords loop
               if ((I - 1 > Line'First and I + Word'Length <= Line'Last) and then
                     Line (I - 1 .. I + Word'Length) = " " & Word.all & " ") or
                 ((I - 1 > Line'First and I + Word'Length - 1 = Line'Last) and then
                    Line (I - 1 .. Line'Last) = " " & Word.all)
               then
                  Last := Last + 1;
                  Result (Last .. Last + Word'Length + 1) := "'" & Word.all & "'";
                  Last := Last + Word'Length + 1;
                  I := I + Word'Length;
                  return True;
               end if;
            end loop;
            return False;
         end Handle_Keyword;

         function Handle_Special_Keyword return Boolean
         is begin
            for J in Special_Keywords'Range loop
               declare
                  Word : String renames Special_Keywords (J).all;
               begin
                  if ((I - 1 > Line'First and I + Word'Length <= Line'Last) and then
                        Line (I - 1 .. I + Word'Length) = " " & Word & " ") or
                    ((I - 1 > Line'First and I + Word'Length - 1 = Line'Last) and then
                       Line (I - 1 .. Line'Last) = " " & Word)
                  then
                     Last := Last + 1;
                     if Special_Seen (J) then
                        Result (Last .. Last + Word'Length + 1) := "'" & Word & "'";
                        Last := Last + Word'Length + 1;
                     else
                        Special_Seen (J) := True;
                        Result (Last .. Last + Word'Length + 1) := Word & "_g";
                        Last := Last + Word'Length + 1;
                     end if;
                     I := I + Word'Length;
                     return True;
                  end if;
               end;
            end loop;
            return False;
         end Handle_Special_Keyword;

      begin
         loop
            exit when I > Line'Last;

            if Handle_Special_Keyword then
               null;

            elsif Handle_Keyword then
               null;

            elsif I + 2 <= Line'Last and then
              (Line (I .. I + 2) = "'['" or
                 Line (I .. I + 2) = "']'" or
                 Line (I .. I + 2) = "::=")
            then
               Last := Last + 1;
               Result (Last .. Last + 2) := Line (I .. I + 2);
               Last := Last + 2;
               I := @ + 3;

            elsif I + 1 <= Line'Last and then
              (Line (I .. I + 1) = "<>" or
                 Line (I .. I + 1) = ":=" or
                 Line (I .. I + 1) = "=>" or
                 Line (I .. I + 1) = ">=" or
                 Line (I .. I + 1) = "<=" or
                 Line (I .. I + 1) = ">>" or
                 Line (I .. I + 1) = "<<" or
                 Line (I .. I + 1) = "/=" or
                 Line (I .. I + 1) = "**" or
                 Line (I .. I + 1) = "--")
            then
               Last := Last + 1;
               Result (Last .. Last + 3) := "'" & Line (I .. I + 1) & "'";
               Last := Last + 3;
               I := @ + 2;

            elsif Line (I) = ''' then
               Last := Last + 1;
               Result (Last .. Last + 5) := " tick ";
               Last := Last + 5;
               I := @ + 1;

            elsif Line (I) in '.' | '+' | '*' | '-' | '/' | '#' | '&' | '(' | ')' | '@' |
              '"' | ',' | ';' | '=' | '>' | '<' |
              '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            then
               Last := Last + 1;
               Result (Last .. Last + 2) := "'" & Line (I) & "'";
               Last := Last + 2;
               I := @ + 1;

            else
               Last := Last + 1;
               Result (Last) := Line (I);
               I := @ + 1;

            end if;
         end loop;
         return Result (Result'First .. Last);
      end Quote_Tokens;

      Arm_File : Ada.Text_IO.File_Type;
      WY_File  : Ada.Text_IO.File_Type;

      In_Production : Boolean := False;
   begin
      if Exists (Arm_WY_Output) then
         Delete_File (Arm_WY_Output);
      end if;

      Open (Arm_File, In_File, Arm_Source);
      Create (WY_File, Out_File, Arm_WY_Output);

      Put_Line (WY_File, ";; generated from " & Arm_Source & " -*- buffer-read-only:t -*-");
      Put_Line (WY_File, "%meta_syntax EBNF");
      Put_Line (WY_File, "%generate None");
      Put_Line (WY_File, "%start compilation");
      Put_Line (WY_File, "%token <left-paren>  LEFT_PAREN  ""(""");
      Put_Line (WY_File, "%token <right-paren> RIGHT_PAREN "")""");
      Put_Line (WY_File, "%token <punctuation> AMPERSAND ""&""");
      Put_Line (WY_File, "%token <punctuation> BAR ""|""");
      Put_Line (WY_File, "%token <punctuation> BOX ""<>""");
      Put_Line (WY_File, "%token <punctuation> COLON "":""");
      Put_Line (WY_File, "%token <punctuation> COLON_EQUAL "":=""");
      Put_Line (WY_File, "%token <punctuation> COMMA "",""");
      Put_Line (WY_File, "%token <punctuation> DOT "".""");
      Put_Line (WY_File, "%token <punctuation> DOT_DOT ""..""");
      Put_Line (WY_File, "%token <punctuation> EQUAL ""=""");
      Put_Line (WY_File, "%token <punctuation> EQUAL_GREATER ""=>""");
      Put_Line (WY_File, "%token <punctuation> GREATER "">""");
      Put_Line (WY_File, "%token <punctuation> GREATER_EQUAL "">=""");
      Put_Line (WY_File, "%token <punctuation> GREATER_GREATER "">>""");
      Put_Line (WY_File, "%token <punctuation> LESS ""<""");
      Put_Line (WY_File, "%token <punctuation> LESS_EQUAL ""<=""");
      Put_Line (WY_File, "%token <punctuation> LESS_LESS ""<<""");
      Put_Line (WY_File, "%token <punctuation> MINUS ""-""");
      Put_Line (WY_File, "%token <punctuation> PLUS ""+""");
      Put_Line (WY_File, "%token <punctuation> SEMICOLON "";""");
      Put_Line (WY_File, "%token <punctuation> SLASH ""/""");
      Put_Line (WY_File, "%token <punctuation> SLASH_EQUAL ""/=""");
      Put_Line (WY_File, "%token <punctuation> STAR ""*""");
      Put_Line (WY_File, "%token <punctuation> STAR_STAR ""**""");

      loop
         declare
            Line       : constant String  := Get_Line (Arm_File);
            Prod_Index : constant Integer := Ada.Strings.Fixed.Index (Line, "::=");
         begin
            if In_Production then
               if Line'Length = 0 then
                  Put_Line (WY_File, ";");
                  New_Line (WY_File);

                  In_Production := False;
               else
                  Put_Line (WY_File, Quote_Tokens (Line));
               end if;
            else
               if Line'Length = 0 then
                  null;

               elsif Line = "        range_constraint ::=  range range" then
                  Put_Line (WY_File, "        range_constraint ::=  'range' range_g ;");
                  New_Line (WY_File);

               elsif Line = "        body ::= proper_body | body_stub" then
                  Put_Line (WY_File, "        body ::= proper_body | body_stub ;");
                  New_Line (WY_File);

               elsif Line = "Syntax Cross Reference" then
                  exit;

               elsif Prod_Index /= 0 then
                  In_Production := True;
                  Put_Line (WY_File, Quote_Tokens (Line));
               end if;
            end if;
         end;
      end loop;
      Put_Line (WY_File, ";; end of file");
      Close (Arm_File);
      Close (WY_File);
   end Read_Arm;


   --  Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
   --       WisiToken.BNF.Generate_Utils.Parse_Grammar_File
   --         (WY_Source, BNF.LALR, BNF.re2c_Lexer, Ignore_Conflicts => True);


   --  WY_Grammar  : WisiToken.Productions.Production_Arrays.Vector renames Generate_Data.Grammar;
begin
   Read_Arm (Arm_Source, "../ada_annex_p_txt.wy");
end Compare_Annex_P;
