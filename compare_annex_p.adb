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

with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat;
with SAL;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees.LR_Utils;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
procedure Compare_Annex_P
is
   --  compare_annex_p <input.txt> <raw.wy> <input.wy>

   Upstream_Txt_Source  : constant String := Ada.Command_Line.Argument (1);
   --  Should be manually copied from the Emacs presentation of the
   --  *.info file, to include <> for italics.

   Upstream_WY_Source   : constant String := Ada.Command_Line.Argument (2);
   Downstream_WY_Source : constant String := Ada.Command_Line.Argument (3);

   Verbosity : constant Integer :=
     (if Ada.Command_Line.Argument_Count > 3
      then Integer'Value (Ada.Command_Line.Argument (4))
      else 0);

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
     (1 => new String'("body"),
      2 => new String'("pragma"),
      3 => new String'("range"),
      4 => new String'("identifier"));

   Keyword_Delimiters : constant String := "][ {}();"; -- _not_ '_'; ']' must be first

   function Build_Keyword_Regexp return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("^[" & Keyword_Delimiters & "](");
      First : Boolean := True;
   begin
      for Keyword of Keywords loop
         Result := Result & (if First then ""else "|") & Keyword.all;
         First := False;
      end loop;
      Result := Result & ")(?:[" & Keyword_Delimiters & "]|$)";
      return To_String (Result);
   end Build_Keyword_Regexp;

   Keyword_Regexp : constant String := Build_Keyword_Regexp;
   Keyword_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Keyword_Regexp);

   function Build_Special_Keyword_Regexp return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("^[" & Keyword_Delimiters & "](?:(");
      First : Boolean := True;
   begin
      for Keyword of Special_Keywords loop
         Result := Result & (if First then "" else ")|(") & Keyword.all;
         First := False;
      end loop;
      Result := Result & "))(?:[" & Keyword_Delimiters & "]|$)";
      return To_String (Result);
   end Build_Special_Keyword_Regexp;

   Special_Keyword_Regexp : constant String := Build_Special_Keyword_Regexp;
   Special_Keyword_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Special_Keyword_Regexp);

   Italics_Regexp : constant String := "<[_a-z]+>";
   Italics_Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Italics_Regexp);

   procedure Arm_Txt_To_Wy (Arm_Source : in String; Arm_WY_Output : in String)
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
         is
            use GNAT.Regpat;
            Matches : Match_Array (0 .. 1);
         begin
            Match (Keyword_Pattern, Line ((if I - 1 > Line'First then I - 1 else I) .. Line'Last), Matches);

            if Matches (0) = No_Match then
               return False;
            else
               declare
                  Word : String renames Line (Matches (1).First .. Matches (1).Last);
               begin
                  Last := Last + 1;
                  Result (Last .. Last + Word'Length + 1) := "'" & Word & "'";
                  Last := Last + Word'Length + 1;
                  I := I + Word'Length;

                  return True;
               end;
            end if;
         end Handle_Keyword;

         function Handle_Special_Keyword return Boolean
         is
            use GNAT.Regpat;
            Matches : Match_Array (0 .. Special_Keywords'Last);
         begin
            Match (Special_Keyword_Pattern, Line ((if I - 1 > Line'First then I - 1 else I) .. Line'Last), Matches);
            if Matches (0) = No_Match then
               return False;
            else
               for J in Special_Keywords'Range loop
                  if Matches (J) /= No_Match then
                     declare
                        Word : String renames Line (Matches (J).First .. Matches (J).Last);
                     begin
                        Last := Last + 1;
                        if J < 4 then
                           if Special_Seen (J) then
                              Result (Last .. Last + Word'Length + 1) := "'" & Word & "'";
                              Last := Last + Word'Length + 1;
                           else
                              Special_Seen (J) := True;
                              Result (Last .. Last + Word'Length + 1) := Word & "_g";
                              Last := Last + Word'Length + 1;
                           end if;
                        else
                           --  upcase 'identifier'
                           Result (Last .. Last + Word'Length - 1) := "IDENTIFIER";
                           Last := Last + Word'Length - 1;
                        end if;

                        I := I + Word'Length;

                        return True;
                     end;
                  end if;
               end loop;
               return False; -- can't get here, but the compiler doesn't know that.
            end if;
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
                 Line (I .. I + 1) = "--" or
                 Line (I .. I + 1) = "..")
            then
               Last := Last + 1;
               Result (Last .. Last + 5) := " '" & Line (I .. I + 1) & "' ";
               Last := Last + 5;
               I := @ + 2;

            elsif Line (I) = ''' then
               Last := Last + 1;
               Result (Last .. Last + 5) := " tick ";
               Last := Last + 5;
               I := @ + 1;

            elsif Line (I) in '.' | '+' | '*' | '-' | '/' | '#' | '&' | '(' | ')' | '@' |
              '"' | ',' | ';' | ':' | '=' | '>' | '<' |
              '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            then
               Last := Last + 1;
               Result (Last .. Last + 4) := " '" & Line (I) & "' ";
               Last := Last + 4;
               I := @ + 1;

            else
               Last := Last + 1;
               Result (Last) := Line (I);
               I := @ + 1;

            end if;
         end loop;
         return Result (Result'First .. Last);
      end Quote_Tokens;

      function Strip_Italics (Line : in String) return String
      is
         use GNAT.Regpat;
         Matches : Match_Array (0 .. 0);
         --  So far there is only one italics per line.
      begin
         Match (Italics_Pattern, Line, Matches);
         if Matches (0) = No_Match then
            return Line;
         else
            return Line (Line'First .. Matches (0).First - 1) & Line (Matches (0).Last + 1 .. Line'Last);
         end if;
      end Strip_Italics;

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
      Put_Line (WY_File, "%token <left-paren>  LEFT_SQUARE_BRACKET  ""[""");
      Put_Line (WY_File, "%token <right-paren> RIGHT_SQUARE_BRACKET ""]""");
      New_Line (WY_File);
      Put_Line (WY_File, "%token <punctuation> AMPERSAND ""&""");
      Put_Line (WY_File, "%token <punctuation> AT '@'");
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

      --  Not in ada.wy, but needed for raw annex_P
      Put_Line (WY_File, "%token <punctuation> HASH ""#""");
      Put_Line (WY_File, "%token <punctuation> DOUBLE_QUOTE '""'");
      Put_Line (WY_File, "%token <punctuation> DASH_DASH '--'");

      --  Not in annex_p, but needed for ada.wy
      Put_Line (WY_File, "%token <punctuation> TICK_1 ""'""");
      Put_Line (WY_File, "%token <punctuation> TICK_2 %[ ""'"" / ""('""([\x20-\U0010FFFF]|GNAT_Char_Coding)""'"" ]%");
      Put_Line (WY_File, "tick : TICK_1 | TICK_2 ;");

      loop
         declare
            Line       : constant String  := Get_Line (Arm_File);
            Prod_Index : constant Integer := Ada.Strings.Fixed.Index (Line, "::=");
         begin
            if In_Production then
               if Line'Length = 0 then
                  New_Line (WY_File);

                  In_Production := False;

               else
                  Put_Line (WY_File, Quote_Tokens (Strip_Italics (Line)));
               end if;
            else
               if Line'Length = 0 then
                  null;

               elsif Line = "     range_constraint ::=  range range" then
                  Put_Line (WY_File, "        range_constraint ::=  'range' range_g ;");
                  New_Line (WY_File);

               elsif Line = "     body ::= proper_body | body_stub" then
                  Put_Line (WY_File, "        body ::= proper_body | body_stub ;");
                  New_Line (WY_File);

               elsif Line = "Syntax Cross Reference" then
                  exit;

               elsif Prod_Index /= 0 then
                  In_Production := True;
                  Put_Line (WY_File, Quote_Tokens (Strip_Italics (Line)));
               end if;
            end if;
         end;
      end loop;
      Put_Line (WY_File, ";; end of file");
      Close (Arm_File);
      Close (WY_File);
   end Arm_Txt_To_Wy;

   Upstream_Parser   : WisiToken.Parse.LR.Parser_No_Recover.Parser;
   Downstream_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

   Virtual_Identifiers : WisiToken.BNF.String_Arrays.Vector;
   --  Empty because we are not translating to BNF; required by
   --  wisitoken_grammar_runtime Get_Text.
begin
   Arm_Txt_To_Wy (Upstream_Txt_Source, Upstream_WY_Source);

   WisiToken.BNF.Generate_Utils.Parse_Grammar_File (Upstream_Parser, Upstream_WY_Source);
   WisiToken.BNF.Generate_Utils.Parse_Grammar_File (Downstream_Parser, Downstream_WY_Source);

   declare
      use WisiToken;
      use WisiToken.Syntax_Trees.LR_Utils;
      use Wisitoken_Grammar_Actions;

      Upstream_Comp_Units : constant Constant_List     := Creators.Create_List
        (Upstream_Parser.Tree_Var_Ref, Upstream_Parser.Tree.Child (Upstream_Parser.Tree.Root, 1),
         +compilation_unit_list_ID, +compilation_unit_ID);
      Upstream_Iter       : constant Constant_Iterator := Upstream_Comp_Units.Iterate_Constant;
      Upstream_Cur        : Cursor                     := Upstream_Iter.First;

      Downstream_Comp_Units : constant Constant_List     := Creators.Create_List
        (Downstream_Parser.Tree_Var_Ref, Downstream_Parser.Tree.Child (Downstream_Parser.Tree.Root, 1),
         +compilation_unit_list_ID, +compilation_unit_ID);
      Downstream_Iter       : constant Constant_Iterator := Downstream_Comp_Units.Iterate_Constant;
      Downstream_Cur        : Cursor                     := Downstream_Iter.First;

      function Upstream_ID return Token_Enum_ID
      is begin
         return -Upstream_Parser.Tree.ID (Upstream_Parser.Tree.Child (Element (Upstream_Cur), 1));
      end Upstream_ID;

      function Upstream_Nonterm return Valid_Node_Index
      is (Upstream_Parser.Tree.Child (Element (Upstream_Cur), 1));

      function Up_Text (Node : in Valid_Node_Index) return String
      is (WisiToken_Grammar_Runtime.Get_Text
            (Upstream_Parser.Terminals, Upstream_Parser.Lexer, Virtual_Identifiers, Upstream_Parser.Tree, Node));

      function Upstream_Error_Message (Msg : in String) return String
      is (Syntax_Trees.Error_Message
            (Upstream_Parser.Tree, Upstream_Parser.Terminals, Element (Upstream_Cur),
             Ada.Directories.Simple_Name (Upstream_WY_Source), Msg));

      function Up_Child (Node : in Valid_Node_Index; Child : in Positive_Index_Type) return Valid_Node_Index
      is (Upstream_Parser.Tree.Child (Node, Child));

      function Downstream_ID return Token_Enum_ID
      is begin
         return -Downstream_Parser.Tree.ID (Downstream_Parser.Tree.Child (Element (Downstream_Cur), 1));
      end Downstream_ID;

      function Downstream_Nonterm return Valid_Node_Index
      is (Downstream_Parser.Tree.Child (Element (Downstream_Cur), 1));

      function Down_Text (Node : in Valid_Node_Index) return String
      is (WisiToken_Grammar_Runtime.Get_Text
            (Downstream_Parser.Terminals, Downstream_Parser.Lexer, Virtual_Identifiers, Downstream_Parser.Tree, Node));

      function Downstream_Error_Message (Msg : in String) return String
      is (Syntax_Trees.Error_Message
            (Downstream_Parser.Tree, Downstream_Parser.Terminals, Element (Downstream_Cur),
             Ada.Directories.Simple_Name (Downstream_WY_Source), Msg));

      function Down_Child (Node : in Valid_Node_Index; Child : in Positive_Index_Type) return Valid_Node_Index
      is (Downstream_Parser.Tree.Child (Node, Child));

      function Build_Inlined_Up_Nonterms return WisiToken.BNF.String_Pair_Lists.List
      is begin
         return Result : WisiToken.BNF.String_Pair_Lists.List do
            Result.Append ((+"defining_identifier", +"IDENTIFIER"));
         end return;
      end Build_Inlined_Up_Nonterms;

      Inlined_Up_Nonterms : constant WisiToken.BNF.String_Pair_Lists.List := Build_Inlined_Up_Nonterms;

      function Is_Inlined (Nonterm : in Valid_Node_Index) return Boolean
      is
         Tree : Syntax_Trees.Tree renames Upstream_Parser.Tree_Var_Ref.Element.all;

         Nonterm_Name : constant String := Up_Text (Tree.Child (Nonterm, 1));
      begin
         return (for some Pair of Inlined_Up_Nonterms => -Pair.Name = Nonterm_Name);
      end Is_Inlined;

      function Up_Text_Inlined (Nonterm : in Valid_Node_Index) return String
      is
         use Ada.Strings.Unbounded;

         Tree : Syntax_Trees.Tree renames Upstream_Parser.Tree_Var_Ref.Element.all;
         Children : constant Valid_Node_Index_Array := Tree.Children (Nonterm);
         RHS_List : constant Constant_List := Creators.Create_List
           (Upstream_Parser.Tree_Var_Ref, Children (3), +rhs_list_ID, +rhs_ID);

         Result : Unbounded_String := +Up_Text (Children (1)) & " " & Up_Text (Children (2)) & " ";

         Need_Bar : Boolean := False;

         function Do_Inlined (Orig : in String) return String
         is begin
            for Pair of Inlined_Up_Nonterms loop
               if -Pair.Name = Orig then
                  return -Pair.Value;
               end if;
            end loop;
            return Orig;
         end Do_Inlined;

         function Get_Inlined (RHS : in Valid_Node_Index) return String
         is
            Result : Unbounded_String;
         begin
            if Tree.RHS_Index (RHS) = 0 then
               return "";
            end if;

            declare
               Item_List : constant Constant_List := Creators.Create_List
                 (Tree, Tree.Child (RHS, 1), +rhs_item_list_ID, +rhs_element_ID);
               Need_Space : Boolean := False;
            begin
               for Node of Item_List loop
                  declare
                     Item : constant Valid_Node_Index := Tree.Find_Descendant (Node, +rhs_item_ID);
                  begin
                     case Tree.RHS_Index (Item) is
                     when 0 =>
                        Result := Result & (if Need_Space then " " else "") & Do_Inlined (Up_Text (Item));
                     when 1 .. 5 =>
                        --  So far no nested substitutions
                        Result := Result & (if Need_Space then " " else "") & Up_Text (Item);
                     when others =>
                        raise SAL.Programmer_Error;
                     end case;
                     Need_Space := True;
                  end;
               end loop;
            end;
            return -Result;
         end Get_Inlined;
      begin
         for RHS of RHS_List loop
            Result := Result & (if Need_Bar then " | " else "") & Get_Inlined (RHS);
            Need_Bar := True;
         end loop;
         declare
            Semicolon : constant String := Up_Text (Children (4));
         begin
            if Semicolon'Length > 0 then
               Result := Result & " " & Semicolon;
            end if;
         end;
         return To_String (Result);
      end Up_Text_Inlined;

      function Down_Text_Redundant (Nonterm : in Valid_Node_Index) return String
      --  Get text of Nonterm tokens from Downstream tree, but include
      --  ;;redundant comments as code, and substitute Inlined_Up_Nonterms.
      is
         use Ada.Strings.Unbounded;

         Tree : Syntax_Trees.Tree renames Downstream_Parser.Tree_Var_Ref.Element.all;
         Children : constant Valid_Node_Index_Array := Tree.Children (Nonterm);
         RHS_List : constant Constant_List := Creators.Create_List
           (Downstream_Parser.Tree_Var_Ref, Children (3), +rhs_list_ID, +rhs_ID);

         Result : Unbounded_String := +Down_Text (Children (1)) & " " & Down_Text (Children (2)) & " ";

         Need_Bar : Boolean := False;

         function Is_New (RHS : in Valid_Node_Index) return Boolean
         is
            use Ada.Strings.Fixed;
            --  A new RHS looks like:
            --      pragma_g ;; new
            New_Pattern : constant String := ";; new";

            Aug : constant Base_Token_Class_Access := Tree.Augmented (Tree.Last_Terminal (RHS));
         begin
            if Aug = null then
               return False;
            end if;
            declare
               Non_Grammar : WisiToken.Base_Token_Arrays.Vector renames WisiToken_Grammar_Runtime.Augmented_Token_Access
                 (Aug).Non_Grammar;
            begin
               for Tok of Non_Grammar loop
                  if Tok.ID = +COMMENT_ID then
                     declare
                        Line : constant String := Downstream_Parser.Lexer.Buffer_Text (Tok.Byte_Region);
                        Red_Index : constant Integer := Index (Line, New_Pattern);
                     begin
                        return Red_Index /= 0;
                     end;
                  end if;
               end loop;
               return False;
            end;
         end Is_New;

         procedure Get_Redundant (RHS : in Valid_Node_Index)
         is
            use Ada.Strings.Fixed;
            --  A redundant RHS looks like:
            --    ;; [ IDENTIFIER '=>' ] name ;; redundant with expression
            --
            Red_Pattern : constant String := ";; redundant";

            Aug : constant Base_Token_Class_Access := Tree.Augmented (Tree.Last_Terminal (RHS));
         begin
            if Aug = null then
               return;
            end if;
            declare
               Non_Grammar : WisiToken.Base_Token_Arrays.Vector renames WisiToken_Grammar_Runtime.Augmented_Token_Access
                 (Aug).Non_Grammar;
            begin
               for Tok of Non_Grammar loop
                  if Tok.ID = +COMMENT_ID then
                     declare
                        Line : constant String := Downstream_Parser.Lexer.Buffer_Text (Tok.Byte_Region);
                        Red_Index : constant Integer := Index (Line, Red_Pattern);
                        Comment_Index : constant Integer := Index (Line, ";;");
                     begin
                        if Red_Index = 0 then
                           null;
                        else
                           Result := Result &
                             (if Need_Bar then " | " else "") &
                             Line (Comment_Index + 3 .. Red_Index - 2);
                           Need_Bar := True;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end Get_Redundant;

      begin
         Get_Redundant (Children (2)); -- original first RHS, after '::='

         for RHS of RHS_List loop
            if Is_New (RHS) then
               null;
            else
               Result := Result & (if Need_Bar then " | " else "") & Down_Text (RHS);
               Need_Bar := True;
               Get_Redundant (RHS);
            end if;
         end loop;
         declare
            Semicolon : constant String := Down_Text (Children (4));
         begin
            if Semicolon'Length > 0 then
               Result := Result & " " & Semicolon;
            end if;
         end;
         return To_String (Result);
      end Down_Text_Redundant;

      Exclude_Up_Nonterms : constant String_Array :=
        --  These are replaced by regexp.
        (new String'("IDENTIFIER"),
         new String'("identifier_start"),
         new String'("identifier_extend"),
         new String'("numeric_literal"),
         new String'("decimal_literal"),
         new String'("numeral"),
         new String'("exponent"),
         new String'("digit"),
         new String'("based_literal"),
         new String'("base"),
         new String'("based_numeral"),
         new String'("extended_digit"),
         new String'("character_literal"),
         new String'("string_literal"),
         new String'("string_element"),
         new String'("comment"));

      function Up_Exclude (Nonterm : in Valid_Node_Index) return Boolean
      is (for some Ptr of Exclude_Up_Nonterms => Ptr.all = Up_Text (Up_Child (Nonterm, 1)));

   begin
      loop
         exit when not Has_Element (Upstream_Cur) or not Has_Element (Downstream_Cur);
         if Upstream_ID = nonterminal_ID and then not Up_Exclude (Upstream_Nonterm) then
            if Downstream_ID = nonterminal_ID then
               declare
                  Up_Nonterm   : constant Valid_Node_Index := Upstream_Nonterm;
                  Down_Nonterm : constant Valid_Node_Index := Downstream_Nonterm;
               begin
                  if Is_Inlined (Up_Nonterm) then
                     if Verbosity > 0 then
                        Put_Line (Up_Text (Up_Child (Up_Nonterm, 1)) & " inlined");
                     end if;
                     Upstream_Cur   := Upstream_Iter.Next (Upstream_Cur);

                  elsif Up_Text_Inlined (Up_Nonterm) = Down_Text_Redundant (Down_Nonterm) then
                     if Verbosity > 0 then
                        Put_Line (Up_Text (Up_Child (Up_Nonterm, 1)) & " = " &
                                    Down_Text (Down_Child (Down_Nonterm, 1)));
                     end if;
                     Upstream_Cur   := Upstream_Iter.Next (Upstream_Cur);
                     Downstream_Cur := Downstream_Iter.Next (Downstream_Cur);
                  else
                     Put_Line (Standard_Error, Upstream_Error_Message ("not equal to"));
                     Put_Line (Standard_Error, Downstream_Error_Message (""));
                     if Verbosity > 0 then
                        Put_Line (Standard_Error, """" & Up_Text (Up_Nonterm) & """");
                        Put_Line (Standard_Error, """" & Down_Text_Redundant (Down_Nonterm) & """");
                     end if;
                     raise User_Error;
                  end if;
               end;
            else
               Downstream_Cur := Downstream_Iter.Next (Downstream_Cur);
            end if;
         else
            Upstream_Cur   := Upstream_Iter.Next (Upstream_Cur);
         end if;
      end loop;
   end;
exception
when WisiToken.User_Error =>
   declare
      use Ada.Command_Line;
   begin
      Set_Exit_Status (Failure);
   end;
end Compare_Annex_P;
