--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The parser actions
--  assume the Emacs Ada mode wisi indentation engine
--
--  If run in a separate process communicating over pipes with the
--  Emacs process, the parser actions output encoded elisp actions;
--  the protocol is documented in Emacs Ada mode wisi-process-parse.el,
--  function wisi-process-parse-execute.
--
--  If run in an Emacs dynamically loaded module, the parser actions
--  call the elisp actions directly.
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Gen_Output_Ada_Common;
with Wisi.Output_Elisp_Common; use Wisi.Output_Elisp_Common;
with Wisi.Utils;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.LR1_Generator;
procedure Wisi.Output_Ada_Emacs
  (Input_File_Name       : in String;
   Output_File_Name_Root : in String;
   Language_Name         : in String;
   Generate_Params                : in Generate_Param_Type;
   Prologues             : in Wisi.Prologues;
   Tokens                : in Wisi.Tokens;
   Conflicts             : in Conflict_Lists.List;
   McKenzie_Recover      : in McKenzie_Recover_Param_Type;
   Elisp_Names           : in Wisi.Elisp_Names;
   Rule_Count            : in Integer;
   Action_Count          : in Integer;
   Check_Count           : in Integer;
   Declare_Enum          : in Boolean)
is
   use all type Standard.Ada.Containers.Count_Type;

   Language_Runtime_Package : constant String := "WisiToken.Wisi_Runtime." & Language_Name;

   package Common is new Wisi.Gen_Output_Ada_Common (Prologues, Tokens, Conflicts, Generate_Params);
   use Common;

   function Split_Sexp
     (Item            : in String;
      Input_File_Name : in String;
      Source_Line     : in WisiToken.Line_Number_Type)
     return String_Lists.List
   is
      --  Return one sexp per element. Remove comments, newlines, and outer '(progn )'.

      Item_I : Integer := Item'First;

      Buffer       : String (Item'First .. Item'Last);
      Buffer_J     : Integer := Buffer'First;
      Buffer_First : Integer := Buffer'First;
      Paren_Count  : Integer := 0;
      In_Comment   : Boolean := False;
      Result       : String_Lists.List;

      Delete_Last_Paren : Boolean := False;
   begin
      --  Loop thru Item, copying chars to Buffer, ignoring comments, newlines.

      if Item (Item'First .. Item'First + 5) = "(progn" then
         Item_I := Item'First + 6;

         Delete_Last_Paren := True;
      end if;

      loop
         exit when Item_I > Item'Last;

         if In_Comment then
            if Item (Item_I) in ASCII.CR | ASCII.LF then
               In_Comment := False;
            end if;
         else
            if Item (Item_I) = '(' then
               if Paren_Count = 0 then
                  Buffer_First := Buffer_J;
               end if;
               Paren_Count := Paren_Count + 1;

               Buffer (Buffer_J) := Item (Item_I);
               Buffer_J := Buffer_J + 1;

            elsif Item (Item_I) = ')' then
               Paren_Count := Paren_Count - 1;
               if Paren_Count = 0 then
                  Buffer (Buffer_J) := Item (Item_I);
                  Result.Append (Buffer (Buffer_First .. Buffer_J));
                  Buffer_First := Buffer'First;
                  Buffer_J     := Buffer'First;

               elsif Paren_Count = -1 then
                  if Delete_Last_Paren then
                     --  all done
                     return Result;
                  else
                     Wisi.Utils.Put_Error (Input_File_Name, Source_Line, "mismatched parens");
                     return String_Lists.Empty_List;
                  end if;
               else
                  Buffer (Buffer_J) := Item (Item_I);
                  Buffer_J := Buffer_J + 1;
               end if;

            elsif Item (Item_I) in ASCII.CR | ASCII.LF then
               null;

            elsif Item (Item_I) = ';' and then Item_I < Item'Last and then Item (Item_I + 1) = ';' then
               In_Comment := True;

            else
               Buffer (Buffer_J) := Item (Item_I);
               Buffer_J := Buffer_J + 1;
            end if;
         end if;
         Item_I := Item_I + 1;
      end loop;
      if Paren_Count /= 0 then
         Wisi.Utils.Put_Error (Input_File_Name, Source_Line, "mismatched parens");
      end if;
      return Result;
   end Split_Sexp;

   procedure Create_Ada_Action
     (Name          : in String;
      RHS           : in RHS_Type;
      Unsplit_Lines : in Standard.Ada.Strings.Unbounded.Unbounded_String;
      Check         : in Boolean)
   is
      --  Create Action (if Check = False; Lines must be RHS.Action) or
      --  Check (if Check = True; Lines must be RHS.Check) subprogram named
      --  Name for RHS.

      use Standard.Ada.Strings;
      use Standard.Ada.Strings.Fixed;
      use Standard.Ada.Strings.Unbounded;
      use Wisi.Utils;

      Sexps : constant String_Lists.List := Split_Sexp (-Unsplit_Lines, Input_File_Name, RHS.Source_Line);

      use all type Standard.Ada.Strings.Maps.Character_Set;

      Blank_Set : constant Standard.Ada.Strings.Maps.Character_Set := Standard.Ada.Strings.Maps.To_Set (" ");

      Space_Paren_Set : constant Standard.Ada.Strings.Maps.Character_Set :=
        Standard.Ada.Strings.Maps.To_Set ("])") or Blank_Set;

      Navigate_Lines     : String_Lists.List;
      Face_Line          : Unbounded_String;
      Indent_Action_Line : Unbounded_String;
      Check_Lines        : String_Lists.List;

      function Statement_Params (Params : in String) return String
      is
         --  Input looks like: [1 function 2 other ...]
         Last       : Integer := Params'First; -- skip [
         First      : Integer;
         Second     : Integer;
         Need_Comma : Boolean := False;
         Result     : Unbounded_String;
         Count      : Integer := 0;
      begin
         loop
            First  := Last + 1;
            Second := Index (Params, Blank_Set, First);
            exit when Second < Params'First;

            Count := Count + 1;
            Last  := Index (Params, Space_Paren_Set, Second + 1);

            Result := Result & (if Need_Comma then ", " else "") &
              "(" & Params (First .. Second - 1) & ", " &
              Elisp_Name_To_Ada (Params (Second + 1 .. Last - 1), Append_ID => False, Trim => 0) & ")";

            Need_Comma := True;
         end loop;
         if Count = 1 then
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (1 => " & (-Result) & "))";
         else
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (" & (-Result) & "))";
         end if;
      end Statement_Params;

      function Containing_Params (Params : in String) return String
      is
         --  Input looks like: 1 2)
         First  : constant Integer := Params'First;
         Second : constant Integer := Index (Params, Blank_Set, First);
      begin
         return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, " &
           Params (First .. Second - 1) & ',' & Params (Second .. Params'Last);
      end Containing_Params;

      function Motion_Params (Params : in String) return String
      is
         --  Input looks like: [1 [2 EXCEPTION WHEN] 3 ...]
         --  Result: (..., Motion_Param_Array'((1, Empty_IDs) & (2, (3 & 8)) & (3, Empty_IDs))
         use Generate_Utils;
         use Standard.Ada.Strings.Maps;
         use WisiToken;

         Delim : constant Character_Set := To_Set ("]") or Blank_Set;

         Last   : Integer          := Params'First; -- skip [
         First  : Integer;
         Vector : Boolean;
         Result : Unbounded_String := +" (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (";

         Index_First  : Integer;
         Index_Last   : Integer;
         IDs          : Unbounded_String;
         IDs_Count    : Integer;
         Need_Comma_1 : Boolean := False;
         Need_Comma_2 : Boolean := False;
      begin
         loop
            Last := Index_Non_Blank (Params, Integer'Min (Params'Last, Last + 1));

            exit when Params (Last) = ']' or Params (Last) = ')';

            Vector := Params (Last) = '[';
            if Vector then
               Index_First  := Last + 1;
               Last         := Index (Params, Delim, Index_First);
               Index_Last   := Last - 1;
               IDs_Count    := 0;
               IDs          := Null_Unbounded_String;
               Need_Comma_2 := False;
               loop
                  exit when Params (Last) = ']';
                  First     := Last + 1;
                  Last      := Index (Params, Delim, First);
                  IDs_Count := IDs_Count + 1;
                  begin
                     IDs := IDs & (if Need_Comma_2 then " & " else "") &
                       Int_Image (Find_Token_ID (Params (First .. Last - 1)));
                     Need_Comma_2 := True;
                  exception
                  when E : Not_Found =>
                     Put_Error (Input_File_Name, RHS.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
                  end;
               end loop;

               Result := Result & (if Need_Comma_1 then " & " else "") & "(" &
                 Params (Index_First .. Index_Last) & ", " &
                 (if IDs_Count = 1 then "+" else "") & IDs & ")";
            else
               First  := Index_Non_Blank (Params, Last);
               Last   := Index (Params, Delim, First);
               Result := Result & (if Need_Comma_1 then " & " else "") &
                 "(" & Params (First .. Last - 1) & ", Empty_IDs)";
            end if;
            Need_Comma_1 := True;
         end loop;
         return -(Result & "))");
      end Motion_Params;

      function Face_Apply_Params (Params : in String) return String
      is
         --  Params is a vector of triples: [1 nil font-lock-keyword-face 3 nil font-lock-function-name-face ...]
         --  Result: ((1, 3, 1), (3, 3, 2), ...)
         use Standard.Ada.Strings.Maps;
         Delim : constant Character_Set := To_Set ("]") or Blank_Set;

         Last       : Integer          := Params'First; -- skip [
         First      : Integer;
         Result     : Unbounded_String;
         Need_Comma : Boolean          := False;
         Count      : Integer          := 0;
      begin
         loop
            Last := Index_Non_Blank (Params, Last + 1);

            exit when Params (Last) = ']' or Params (Last) = ')';

            Count  := Count + 1;
            First  := Last;
            Last   := Index (Params, Delim, First);
            Result := Result & (if Need_Comma then ", (" else "(") & Params (First .. Last - 1);

            if Params (Last) = ']' then
               Put_Error (Input_File_Name, RHS.Source_Line, "invalid wisi-face-apply argument");
               exit;
            end if;

            First  := Index_Non_Blank (Params, Last + 1);
            Last   := Index (Params, Delim, First);
            Result := Result & ',' & Integer'Image (Find_Elisp_ID (Elisp_Names.Faces, Params (First .. Last - 1)));

            if Params (Last) = ']' then
               Put_Error (Input_File_Name, RHS.Source_Line, "invalid wisi-face-apply argument");
               exit;
            end if;

            First  := Index_Non_Blank (Params, Last + 1);
            Last   := Index (Params, Delim, First);
            Result := Result & ',' & Integer'Image (Find_Elisp_ID (Elisp_Names.Faces, Params (First .. Last - 1))) &
              ")";

            Need_Comma := True;
         end loop;
         if Count = 1 then
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (1 => " & (-Result) & "))";
         else
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (" & (-Result) & "))";
         end if;
      exception
      when E : others =>
         Put_Error
           (Input_File_Name, RHS.Source_Line, "invalid syntax: " & Standard.Ada.Exceptions.Exception_Message (E));
         return "";
      end Face_Apply_Params;

      function Face_Mark_Params (Params : in String) return String
      is
         --  Params is a vector of pairs: [1 prefix 3 suffix ...]
         --  Result: ((1, Prefix), (3, Suffix), ...)
         use Standard.Ada.Strings.Maps;
         Delim : constant Character_Set := To_Set ("]") or Blank_Set;

         Last       : Integer          := Params'First; -- skip [
         First      : Integer;
         Result     : Unbounded_String;
         Need_Comma : Boolean          := False;
         Count      : Integer          := 0;
      begin
         loop
            Last := Index_Non_Blank (Params, Last + 1);

            exit when Params (Last) = ']' or Params (Last) = ')';

            Count  := Count + 1;
            First  := Last;
            Last   := Index (Params, Delim, First);
            Result := Result & (if Need_Comma then ", (" else "(") & Params (First .. Last - 1);

            if Params (Last) = ']' then
               Put_Error (Input_File_Name, RHS.Source_Line, "invalid wisi-face-mark argument");
               exit;
            end if;

            First  := Index_Non_Blank (Params, Last + 1);
            Last   := Index (Params, Delim, First);
            Result := Result & ", " & Elisp_Name_To_Ada (Params (First .. Last - 1), False, 0) & ")";

            Need_Comma := True;
         end loop;
         if Count = 1 then
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (1 => " & (-Result) & "))";
         else
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (" & (-Result) & "))";
         end if;
      exception
      when E : others =>
         Put_Error
           (Input_File_Name, RHS.Source_Line, "invalid syntax: " & Standard.Ada.Exceptions.Exception_Message (E));
         return "";
      end Face_Mark_Params;

      function Face_Remove_Params (Params : in String) return String
      is
         --  Params is a vector of token numbers: [1 3 ...]
         --  Result: (1, 3, ...)
         use Standard.Ada.Strings.Maps;
         Delim : constant Character_Set := To_Set ("]") or Blank_Set;

         Last       : Integer          := Params'First; -- skip [
         First      : Integer;
         Result     : Unbounded_String;
         Need_Comma : Boolean          := False;
         Count      : Integer          := 0;
      begin
         loop
            Last := Index_Non_Blank (Params, Last + 1);

            exit when Params (Last) = ']' or Params (Last) = ')';

            Count  := Count + 1;
            First  := Last;
            Last   := Index (Params, Delim, First);
            Result := Result & (if Need_Comma then ", " else "") & Params (First .. Last - 1);

            Need_Comma := True;
         end loop;
         if Count = 1 then
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (1 => " & (-Result) & "))";
         else
            return " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, (" & (-Result) & "))";
         end if;
      exception
      when E : others =>
         Put_Error
           (Input_File_Name, RHS.Source_Line, "invalid syntax: " & Standard.Ada.Exceptions.Exception_Message (E));
         return "";
      end Face_Remove_Params;

      function Indent_Params (Params : in String; N : in String := "") return String
      is
         --  If N is non-empty, it is the first arg in wisi-indent-action*, followed by ','.
         --
         --  Params is a vector, one item for each token in Tokens. Each item is one of:
         --
         --  - an integer; copy to output
         --
         --  - a symbol; convert to Ada name syntax
         --
         --  - a lisp function call with arbitrary args; convert to Indent_Param type
         --
         --  - a vector with two elements [code_indent comment_indent]; convert to Indent_Pair.

         use Standard.Ada.Strings.Maps;
         use Standard.Ada.Containers;

         Delim : constant Character_Set := To_Set ("])") or Blank_Set;

         subtype Digit is Character range '0' .. '9';

         Last          : Integer         := Index_Non_Blank (Params); -- skip [
         Prefix        : constant String := " (Parse_Data, Tree, Tree_Nonterm, Tree_Tokens, " & N & "(";
         Result        : Unbounded_String;
         Need_Comma    : Boolean         := False;
         Param_Count   : Count_Type      := 0;            -- in Params

         function Indent_Label (Elisp_Name : in String) return String
         is begin
            if    Elisp_Name = "wisi-anchored"   then return "Anchored_0";
            elsif Elisp_Name = "wisi-anchored%"  then return "Anchored_1";
            elsif Elisp_Name = "wisi-anchored%-" then return "Anchored_2";
            elsif Elisp_Name = "wisi-anchored*"  then return "Anchored_3";
            elsif Elisp_Name = "wisi-anchored*-" then return "Anchored_4";
            elsif Elisp_Name = "wisi-hanging"    then return "Hanging_0";
            elsif Elisp_Name = "wisi-hanging%"   then return "Hanging_1";
            elsif Elisp_Name = "wisi-hanging%-"  then return "Hanging_2";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "unrecognized wisi indent function: '" & Elisp_Name & "'");
               return "";
            end if;
         end Indent_Label;

         function Ensure_Simple_Indent (Item : in String) return String
         is begin
            --  Return an aggregate for Simple_Indent_Param. Item can be anything
            --  Expression returns except Hanging.

            if Item (Item'First) = '(' then
               --  Anchored or Language
               return Item;

            else
               --  simple integer
               return "(Int, " & Item & ")";
            end if;
         end Ensure_Simple_Indent;

         function Expression (Param_First : in Integer) return String
         is
            --  Return a simple integer expression, or an aggregate for
            --  Simple_Indent_Param or Indent_Param.
            --
            --  Handles this syntax:
            --
            --  integer literal:
            --  2 => 2
            --  -1 => -1
            --
            --  variable name:
            --  ada-indent => Ada_Indent
            --
            --  token_id literal:
            --  'TYPE => 13
            --
            --  simple expression with + - * :
            --  (- ada-indent) => -Ada_Indent
            --  (- ada-indent-when ada-indent) => Ada_Indent_When - Ada_Indent
            --
            --  if expression:
            --  (if c a b) => (if c then a else b)
            --
            --  function call with expression args:
            --  (wisi-hanging (wisi-anchored% 1 ada-indent)
            --                (wisi-anchored% 1 (+ ada-indent ada-indent-broken)))

            use Generate_Utils;

            First : Integer := Index_Non_Blank (Params, Param_First);

            Function_Name : Unbounded_String;
            Args          : Unbounded_String;
            Arg_Count     : Count_Type      := 0;
         begin
            if Params (First) in Digit or Params (First) = '-' then
               Last := Index (Params, Delim, First);
               return Params (First .. Last - 1);

            elsif Params (First) = ''' then
               Last := Index (Params, Delim, First);
               return WisiToken.Int_Image (Find_Token_ID (Params (First + 1 .. Last - 1)));

            elsif Params (First) = '(' then
               First  := First + 1;
               Last   := Index (Params, Delim, First);
               Function_Name := +Params (First .. Last - 1);

               if Length (Function_Name) = 1 then
                  --  - + *
                  Last := Index (Params, Delim, Last + 1);
                  if Params (Last) = ')' then
                     return Result : constant String := -Function_Name & Expression (First + 1)
                     do
                        Last := Last + 1; -- get past ')'
                     end return;
                  else
                     Args := +Expression (First + 1);
                     Args := Args & ' ' & Function_Name & ' ' & Expression (Last + 1);

                     Last := Last + 1; -- get past ')'
                     return -Args;
                  end if;

               elsif -Function_Name = "if" then
                  Args := +Expression (Last + 1);
                  Args := +"(if " & Args & " then " & Expression (Last + 1);
                  Args := Args & " else " & Expression (Last + 1) & ')';

                  Last := Last + 1; -- get past ')'
                  return -Args;

               elsif Is_Present (Elisp_Names.Indents, -Function_Name) then
                  --  Language-specific function call
                  Function_Name := +Value (Elisp_Names.Indents, -Function_Name);
                  Arg_Count     := 0;
                  loop
                     exit when Params (Last) = ')';

                     First := Last + 1;
                     if Arg_Count = 0 then
                        Args := +Expression (First);
                     else
                        Args := Args & " & " & Expression (First);
                     end if;
                     Arg_Count := Arg_Count + 1;
                  end loop;

                  Last := Last + 1; -- get past ')'

                  return "(Language, " & (-Function_Name) & "'Access, " &
                    (if Arg_Count = 0 then "Null_Args"
                     elsif Arg_Count = 1 then '+' & (-Args)
                     else -Args)
                    & ')';

               else
                  --  wisi lisp function call
                  Function_Name := +Indent_Label (-Function_Name);
                  if Length (Function_Name) = 0 then
                     --  not a recognized function
                     Last := 1 + Index (Params, ")", Last);
                     return "";

                  elsif Slice (Function_Name, 1, 4) = "Hang" then
                     --  Arguments are 2 Simple_Indent_Param
                     Args := +Ensure_Simple_Indent (Expression (Last + 1));
                     Args := Args & ", " & Ensure_Simple_Indent (Expression (Last + 1));
                     Last := Last + 1; -- get past ')'
                     return "(" & (-(Function_Name & ", " & Args)) & ")";
                  else
                     --  Arguments are 2 simple integer expressions
                     Args := +Expression (Last + 1);
                     Args := Args & ", " & Expression (Last + 1);
                     Last := Last + 1; -- get past ')'
                     return "(" & (-(Function_Name & ", " & Args)) & ")";
                  end if;
               end if;

            else
               --  Assume it is a language-specific integer indent option, like "ada-indent",
               --  declared in Language_Runtime_Package, which is use-visible.
               Last  := Index (Params, Delim, First);
               return Elisp_Name_To_Ada (Params (First .. Last - 1), False, 0);
            end if;
         exception
         when E : others =>
            Put_Error (Input_File_Name, RHS.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
            return "";
         end Expression;

         function Ensure_Indent_Param (Item : in String) return String
         is begin
            --  Return an aggregate for Indent_Param. Item can be anything
            --  Expression returns.
            if Item'Length = 0 then
               --  Expression could not find an indent function
               return Item;

            elsif Item'Length >= 5 and then Item (Item'First .. Item'First + 4) = "(Hang" then
               return Item;

            elsif Item (Item'First) = '(' then
               --  Anchored or Language
               return "(Simple, " & Item & ")";

            else
               --  simple integer
               return "(Simple, (Int, " & Item & "))";
            end if;
         end Ensure_Indent_Param;

      begin
         loop
            if Params (Last) /= ']' then
               Last := Index_Non_Blank (Params, Last + 1);
            end if;

            exit when Params (Last) = ']';

            if Need_Comma then
               Result := Result & ", ";
            else
               Need_Comma := True;
            end if;

            case Params (Last) is
            when '(' =>
               Result := Result & "(False, " & Ensure_Indent_Param (Expression (Last)) & ')';

            when '[' =>
               --  vector
               Result := Result & "(True, " & Ensure_Indent_Param (Expression (Last + 1));
               Result := Result & ", " & Ensure_Indent_Param (Expression (Last + 1)) & ')';
               if Params (Last) /= ']' then
                  Put_Error (Input_File_Name, RHS.Source_Line, "invalid indent syntax");
               end if;
               Last := Last + 1;

            when others =>
               --  integer or symbol
               Result := Result & "(False, " & Ensure_Indent_Param (Expression (Last)) & ')';

            end case;
            Param_Count := Param_Count + 1;
         end loop;

         if Param_Count /= RHS.Production.Length then
            Put_Error
              (Input_File_Name, RHS.Source_Line, "indent parameters count of" & Count_Type'Image (Param_Count) &
                 " /= production token count of" & Count_Type'Image (RHS.Production.Length));
         end if;

         if Param_Count = 1 then
            Result := Prefix & "1 => " & Result;
         else
            Result := Prefix & Result;
         end if;

         return -(Result & "))");
      end Indent_Params;

      function Merge_Names_Params (Params : in String) return String
      is
         --  Input looks like "1 2)"
         First  : constant Integer := Params'First;
         Second : constant Integer := Index (Params, Blank_Set, First);
      begin
         return " (Nonterm, Tokens, " & Params (First .. Second - 1) & ',' &
           Params (Second .. Params'Last);
      end Merge_Names_Params;

      function Match_Names_Params (Params : in String) return String
      is
         --  Input looks like: 1 2)
         First  : constant Integer := Params'First;
         Second : constant Integer := Index (Params, Blank_Set, First);
      begin
         return " (Lexer, Descriptor, Tokens, " &
           Params (First .. Second - 1) & ',' &
           Params (Second .. Params'Last - 1) & ", " &
           (if Length (Generate_Params.End_Names_Optional_Option) > 0
            then -Generate_Params.End_Names_Optional_Option
            else "False") & ")";
      end Match_Names_Params;

      procedure Translate_Line (Line : in String)
      is
         Last       : constant Integer := Index (Line, Blank_Set);
         Elisp_Name : constant String  := Line (Line'First + 1 .. Last - 1);
      begin
         --  wisi action/check functions, in same order as typically used in
         --  .wy files; Navigate, Face, Indent, Check.
         if Elisp_Name = "wisi-statement-action" then
            Navigate_Lines.Append
              (Elisp_Name_To_Ada (Elisp_Name, False, 5) &
                 Statement_Params (Line (Last + 1 .. Line'Last)) & ";");

         elsif Elisp_Name = "wisi-containing-action" then
            Navigate_Lines.Append
              (Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Containing_Params (Line (Last + 1 .. Line'Last)) & ";");

         elsif Elisp_Name = "wisi-motion-action" then
            Navigate_Lines.Append
              (Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Motion_Params (Line (Last + 1 .. Line'Last)) & ";");

         elsif Elisp_Name = "wisi-face-apply-action" then
            if Length (Face_Line) = 0 then
               Face_Line := +Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Face_Apply_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple face actions");
            end if;

         elsif Elisp_Name = "wisi-face-apply-list-action" then
            if Length (Face_Line) = 0 then
               Face_Line := +Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Face_Apply_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple face actions");
            end if;

         elsif Elisp_Name = "wisi-face-mark-action" then
            if Length (Face_Line) = 0 then
               Face_Line := +Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Face_Mark_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple face actions");
            end if;

         elsif Elisp_Name = "wisi-face-remove-action" then
            if Length (Face_Line) = 0 then
               Face_Line := +Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Face_Remove_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple face actions");
            end if;

         elsif Elisp_Name = "wisi-indent-action" then
            if Length (Indent_Action_Line) = 0 then
               Indent_Action_Line := +"Indent_Action_0" &
                 Indent_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple indent actions");
            end if;

         elsif Elisp_Name = "wisi-indent-action*" then
            if Length (Indent_Action_Line) = 0 then
               declare
                  Temp : constant Integer := Index (Line, Blank_Set, Last + 1);
               begin
                  Indent_Action_Line := +"Indent_Action_1" &
                    Indent_Params (Line (Temp + 1 .. Line'Last), Line (Last + 1 .. Temp - 1) & ", ") & ";";
               end;
            else
               Put_Error (Input_File_Name, RHS.Source_Line, "multiple indent actions");
            end if;

         elsif Elisp_Name = "wisi-propagate-name" then
            if not Check then
               Put_Error (Input_File_Name, RHS.Source_Line, Elisp_Name & " used in action");
               return;
            end if;
            Check_Lines.Append
              ("return " & Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 " (Nonterm, Tokens, " & Line (Last + 1 .. Line'Last) & ";");

         elsif Elisp_Name = "wisi-merge-names" then
            if not Check then
               Put_Error (Input_File_Name, RHS.Source_Line, Elisp_Name & " used in action");
               return;
            end if;
            Check_Lines.Append
              ("return " & Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Merge_Names_Params (Line (Last + 1 .. Line'Last)) & ";");

         elsif Elisp_Name = "wisi-match-names" then
            if not Check then
               Put_Error (Input_File_Name, RHS.Source_Line, Elisp_Name & " used in action");
               return;
            end if;
            Check_Lines.Append
              ("return " & Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Match_Names_Params (Line (Last + 1 .. Line'Last)) & ";");

         else
            Put_Error (Input_File_Name, RHS.Source_Line, "unrecognized elisp action: '" & Elisp_Name & "'");
         end if;
      end Translate_Line;

   begin
      for Sexp of Sexps loop
         begin
            Translate_Line (Sexp);
         exception
         when E : Not_Found =>
            Put_Error (Input_File_Name, RHS.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

      if Check then
         --  in a check
         Indent_Line ("function " & Name);
         Indent_Line (" (Lexer   : in     WisiToken.Lexer.Handle;");
         Indent_Line ("  Nonterm : in out WisiToken.Recover_Token;");
         Indent_Line ("  Tokens  : in     WisiToken.Recover_Token_Array)");
         Indent_Line (" return WisiToken.Semantic_Checks.Check_Status");
         declare
            --  Tokens is always referenced.
            Unref_Lexer   : constant Boolean := (for all Line of Check_Lines => 0 = Index (Line, "Lexer"));
            Unref_Nonterm : constant Boolean := (for all Line of Check_Lines => 0 = Index (Line, "Nonterm"));
         begin
            if Unref_Lexer or Unref_Nonterm then
               Indent_Line ("is");
               if Unref_Lexer then
                  Indent_Line ("   pragma Unreferenced (Lexer);");
               end if;
               if Unref_Nonterm then
                  Indent_Line ("   pragma Unreferenced (Nonterm);");
               end if;
               Indent_Line ("begin");
            else
               Indent_Line ("is begin");
            end if;
         end;
         Indent := Indent + 3;
         for Line of Check_Lines loop
            Indent_Line (Line);
         end loop;
      else
         --  In an action
         Indent_Line ("procedure " & Name);
         Indent_Line (" (User_Data    : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
         Indent_Line ("  Tree         : in out WisiToken.Syntax_Trees.Tree;");
         Indent_Line ("  Tree_Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Index;");
         Indent_Line ("  Tree_Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)");
         Indent_Line ("is");
         Indent_Start ("   Parse_Data : WisiToken.Wisi_Runtime.Parse_Data_Type renames");
         Put_Line (" WisiToken.Wisi_Runtime.Parse_Data_Type (User_Data);");
         Indent_Line ("begin");
         Indent := Indent + 3;

         Indent_Line ("case Parse_Data.Post_Parse_Action is");
         Indent_Line ("when Navigate =>");
         if Navigate_Lines.Length > 0 then
            Indent := Indent + 3;
            for Line of Navigate_Lines loop
               Indent_Line (Line);
            end loop;
            Indent := Indent - 3;
         else
            Indent_Line ("   null;");
         end if;

         Indent_Line ("when Face =>");
         if Length (Face_Line) > 0 then
            Indent := Indent + 3;
            Indent_Line (-Face_Line);
            Indent := Indent - 3;
         else
            Indent_Line ("   null;");
         end if;

         Indent_Line ("when Indent =>");
         if Length (Indent_Action_Line) > 0 then
            Indent := Indent + 3;
            Indent_Line (-Indent_Action_Line);
            Indent := Indent - 3;
         else
            Indent_Line ("   null;");
         end if;
         Indent_Line ("end case;");
      end if;

      Indent := Indent - 3;
      Indent_Line ("end " & Name & ";");
      New_Line;

   end Create_Ada_Action;

   procedure Create_Ada_Body
   is
      use all type WisiToken.Unknown_State_Index;
      use Standard.Ada.Strings.Unbounded;
      use Generate_Utils;
      use Wisi.Utils;

      File_Name : constant String := Output_File_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_process",
         when Module  => "_module") &
        ".adb";

      Package_Name : constant String := -Data.Package_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_Process",
         when Module  => "_Module");

      Body_File : File_Type;

   begin
      if Data.Parser_Algorithm in LALR | LALR_LR1 then
         Parsers (LALR) := WisiToken.LR.LALR_Generator.Generate
           (Data.Grammar,
            LALR_Descriptor,
            WisiToken.State_Index (Generate_Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
               Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
            Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);

         Data.Parser_State_Count := Parsers (LALR).State_Last - Parsers (LALR).State_First + 1;
      end if;

      if Data.Parser_Algorithm in LR1 | LALR_LR1 then
         Parsers (LR1) := WisiToken.LR.LR1_Generator.Generate
           (Data.Grammar,
            LR1_Descriptor,
            WisiToken.State_Index (Generate_Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
               Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Trace                    => WisiToken.Trace_Generate > 1,
            Put_Parse_Table          => WisiToken.Trace_Generate > 0,
            Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
            Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);

         Data.Parser_State_Count := WisiToken.Unknown_State_Index'Max
           (Data.Parser_State_Count,
            Parsers (LR1).State_Last - Parsers (LR1).State_First + 1);
      end if;

      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Prologue
        (Ada_Comment,
         (if Prologues.Body_Context_Clause.Length > 0
          then Prologues.Body_Context_Clause
          else Prologues.Spec_Context_Clause));
      New_Line;

      Put_Line ("with WisiToken.Lexer.re2c;");
      if Check_Count > 0 then
         Put_Line ("with WisiToken.Semantic_Checks; use WisiToken.Semantic_Checks;");
      end if;
      Put_Line ("with WisiToken.Wisi_Runtime; use WisiToken.Wisi_Runtime;");
      Put_Line ("with " & Language_Runtime_Package & "; use " & Language_Runtime_Package & ";");
      Put_Line ("with " & Output_File_Name_Root & "_re2c_c;");

      case Data.Interface_Kind is
      when Process =>
         null;

      when Module =>
         Put_Line ("with Emacs_Module_Aux; use Emacs_Module_Aux;");
         Put_Line ("with Ada.Exceptions;");
         Put_Line ("with Ada.Strings.Unbounded;");
      end case;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("use all type Motion_Param_Array;");
      New_Line;

      Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
      Indent_Line ("  (" & Output_File_Name_Root & "_re2c_c.New_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Free_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Reset_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Next_Token);");
      New_Line;

      --  generate Action and Check subprograms, populate Ada_Action_Names,
      --  Ada_Check_Names.

      for Rule of Tokens.Rules loop
         --  No need for a Token_Cursor here, since we only need the
         --  nonterminals.
         declare
            LHS_ID : constant WisiToken.Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);

            Action_Names     : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Check_Names      : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Prod_Index       : Integer := 0; -- Semantic_Action defines Prod_Index as zero-origin
            Action_All_Empty : Boolean := True;
            Check_All_Empty  : Boolean := True;
         begin
            for RHS of Rule.Right_Hand_Sides loop
               if Length (RHS.Action) > 0 then
                  Action_All_Empty := False;
                  declare
                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Int_Image (Prod_Index);
                  begin
                     Action_Names (Prod_Index) := new String'(Name & "'Access");
                     Create_Ada_Action (Name, RHS, RHS.Action, Check => False);
                  end;
               end if;

               if Length (RHS.Check) > 0 then
                  Check_All_Empty := False;
                  declare
                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Int_Image (Prod_Index) & "_check";
                  begin
                     Check_Names (Prod_Index) := new String'(Name & "'Access");
                     Create_Ada_Action (Name, RHS, RHS.Check, Check => True);
                  end;
               end if;
               Prod_Index := Prod_Index + 1;
            end loop;

            if not Action_All_Empty then
               Ada_Action_Names (LHS_ID) := new Action_Name_List'(Action_Names);
            end if;
            if not Check_All_Empty then
               Ada_Check_Names (LHS_ID) := new Action_Name_List'(Check_Names);
            end if;
         end;
      end loop;

      Create_Create_Parser
        (Data.Parser_Algorithm, Data.Interface_Kind, Generate_Params.First_State_Index,
         Generate_Params.First_Parser_Label);

      case Data.Interface_Kind is
      when Process =>
         null;
      when Module =>
         Indent_Line ("Parser : LR_Parser.Instance;");
         New_Line;

         Indent_Line ("function Parse (Env : Emacs_Env_Access) return emacs_module_h.emacs_value");
         Indent_Line ("is begin");
         Indent := Indent + 3;
         Indent_Line ("WisiToken.Trace_Parse := To_Integer (Env, Symbol_Value (Env, Elisp_Symbols (Wisi_Debug_ID)));");
         Indent_Line ("Wisi_Cache_Max := To_Integer (Env, Symbol_Value (Env, Elisp_Symbols (Wisi_Cache_Max_ID)));");
         Indent_Line ("Parser.Reset;");
         Indent_Line ("Parser.Parse;");
         Indent_Line ("return Env.Qnil;");
         Indent := Indent - 3;
         Indent_Line ("exception");
         Indent_Line ("when E : WisiToken.Parse_Error | WisiToken.Syntax_Error =>");
         Indent_Line ("   return To_Emacs (Env, Ada.Exceptions.Exception_Message (E));");
         Indent_Line ("when E : others =>");
         Indent_Line ("   declare");
         Indent_Line ("      use Ada.Exceptions;");
         Indent_Line ("   begin");
         Indent_Line ("      return To_Emacs (Env, Exception_Name (E) & "": "" & Exception_Message (E));");
         Indent_Line ("   end;");
         Indent_Line ("end Parse;");
         New_Line;

         Indent_Line ("function Init (Env : Emacs_Env_Access) return Interfaces.C.int");
         Indent_Line ("is");
         Indent_Line ("   Lexer_Elisp_Symbols : Lexers.Elisp_Array_Emacs_Value;");
         Indent_Line ("begin");
         Indent_Line ("   " & Package_Name & ".Env := Env;");
         Indent_Line ("   Emacs_Module_Aux.Init (Env);");
         Indent_Line ("   for I in Token_Symbols'Range loop");
         Indent_Line ("      Token_Symbols (I) := Intern_Soft (Env, Token_Images (I).all);");
         Indent_Line ("   end loop;");
         Indent_Line ("   for I in Elisp_Symbols'Range loop");
         Indent_Line ("      Elisp_Symbols (I) := Intern_Soft (Env, Elisp_Names (I).all);");
         Indent_Line ("   end loop;");
         Indent_Line ("   for I in Elisp_Numbers'Range loop");
         Indent_Line ("      Elisp_Numbers (I) := Env.make_fixnum (Env, emacs_module_h.int64_t (I));");
         Indent_Line ("   end loop;");
         Indent_Line ("   for I in Lexer_Elisp_Symbols'Range loop");
         Indent_Line ("      Lexer_Elisp_Symbols (I) := Intern_Soft (Env, Lexers.Elisp_Names (I).all);");
         Indent_Line ("   end loop;");
         Indent_Line ("   Parser := Create_Parser (Env, Lexer_Elisp_Symbols);");
         Indent_Line ("   return 0;");
         Indent_Line ("exception");
         Indent_Line ("when E : others =>");
         Indent_Line
           ("   Signal_Error (Env, " &
              "Ada.Exceptions.Exception_Name (E) & "": "" & Ada.Exceptions.Exception_Message (E), Env.Qnil);");
         Indent_Line ("   return 1;");
         Indent_Line ("end Init;");
         New_Line;
      end case;

      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

      if WisiToken.Trace_Generate > 0 then
         --  Match wisi-output_elisp, wisi-output_ada format
         Put_Line
           (Integer'Image (Rule_Count) & " rules," &
              Integer'Image (Action_Count) & " actions," &
              Integer'Image (Check_Count) & " checks," &
              WisiToken.State_Index'Image (Data.Parser_State_Count) & " states," &
              Integer'Image (Data.Table_Entry_Count) & " table entries");
         Put_Line
           (Integer'Image (Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
              Integer'Image (Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
              Integer'Image (Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");

      end if;
   end Create_Ada_Body;

   procedure Create_Process_Elisp
   is
      use Generate_Utils;
      use Standard.Ada.Strings.Unbounded;
      use Wisi.Utils;

      File : File_Type;

      Paren_1_Done : Boolean := False;
   begin
      Create (File, Out_File, Output_File_Name_Root & "-process.el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");
      Put_Prologue (Elisp_Comment, Prologues.Spec_Context_Clause);
      New_Line;
      Put_Line ("(require 'wisi-process-parse)");
      New_Line;

      Indent_Line  ("(defconst " & Output_File_Name_Root & "-process-token-table");
      Indent_Start ("  [");
      Indent := Indent + 3;
      for Cursor in All_Tokens.Iterate loop
         if Paren_1_Done then
            Indent_Line (Name (Cursor));
         else
            Paren_1_Done := True;
            Put_Line (Name (Cursor));
         end if;

      end loop;
      Indent_Line ("])");
      Indent := Indent - 3;
      New_Line;

      Output_Elisp_Common.Indent_Name_Table (Output_File_Name_Root, "process-face-table", Elisp_Names.Faces);

      Put_Line ("(provide '" & Output_File_Name_Root & "-process)");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Process_Elisp;

   procedure Create_Module_Elisp
   is
      use Standard.Ada.Strings.Unbounded;
      use Generate_Utils;
      use Wisi.Utils;

      Lower_Package_Name_Root : constant String := -Data.Package_Name_Root;

      function To_ID_Image (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
      is
         use WisiToken;
      begin
         --  Ada 'Val is 0 origin; Token_ID is 1 origin
         return Token_ID'Image (-1 + Find_Token_ID (-Name));
      end To_ID_Image;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & "-module.el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");

      --  don't need the prologue here

      Put_Line ("(require 'wisi-parse-common)");
      New_Line;

      --  Lexer tables; also contain terminals for wisi-tokens
      Indent_Keyword_Table (Output_File_Name_Root, "elisp", Tokens.Keywords, To_String'Access);
      Indent_Keyword_Table (Output_File_Name_Root, "module", Tokens.Keywords, To_ID_Image'Access);
      Indent_Token_Table (Output_File_Name_Root, "elisp", Tokens.Tokens, To_String'Access);
      Indent_Token_Table (Output_File_Name_Root, "module", Tokens.Tokens, To_ID_Image'Access);

      --  non-terminals. We only need the ones that actually have
      --  actions, and thus will appear in a call to To_Emacs. But
      --  Token_Symbols must be indexed by Token_ID, so we declare
      --  all of them.
      Indent_Line ("(defconst " & Output_File_Name_Root & "-module-nonterms");
      Indent_Line (" '(");
      Indent := Indent + 3;
      Indent_Line (-WisiToken_Accept_Name);
      for Rule of Tokens.Rules loop
         Indent_Line (-Rule.Left_Hand_Side);
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
      New_Line;

      Indent_Line
        ("(cl-defstruct (" & Lower_Package_Name_Root &
           "-wisi-module-parser (:include wisi-parser)))");
      New_Line;
      Indent_Line ("(defun " & Lower_Package_Name_Root & "-wisi-module-parser-make (dll-name)");
      Indent_Line ("  (module-load dll-name)");
      Indent_Line ("  (make-" & Lower_Package_Name_Root & "-wisi-module-parser))");
      New_Line;

      Indent_Line ("(defvar " & Lower_Package_Name_Root & "-module-lexer nil)");
      Indent_Line
        ("(declare-function " &
           Lower_Package_Name_Root &
           "-wisi-module-parse """ &
           Lower_Package_Name_Root &
           "-wisi-module-parse.c"")");
      New_Line;

      Indent_Line
        ("(cl-defmethod wisi-parse-current ((parser " &
           Lower_Package_Name_Root &
           "-wisi-module-parser))");
      Indent := Indent + 2;
      Indent_Line ("(let* ((wisi-lexer " & Lower_Package_Name_Root & "-module-lexer)");
      Indent_Line ("       (result (" & Lower_Package_Name_Root & "-wisi-module-parse)))");
      --  Result is nil for no errors, a string for some error.
      --  Ada code has already added line:column, but not file name
      Indent_Line ("  (when result");
      Indent_Line ("    (signal 'wisi-parse-error (format ""%s:%s"" (buffer-name) result)))))");
      New_Line;
      Indent := Indent - 2;

      Indent_Line ("(provide '" & Output_File_Name_Root & "-module)");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Module_Elisp;

   procedure Create_Module_Aux
   is
      use Generate_Utils;
      use Wisi.Utils;

      Package_Name_Root       : constant String := -Data.Package_Name_Root;
      Lower_Package_Name_Root : constant String := -Data.Lower_Package_Name_Root;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & "_wisi_module_parse.gpr");
      Set_Output (File);
      Indent := 1;
      Put_Line ("-- generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line ("-- ");
      Indent_Line ("with ""wisi_module_parse_common"";");
      Indent_Line ("library project " & Package_Name_Root & "_Wisi_Module_Parse is");
      New_Line;
      Indent := Indent + 3;
      Indent_Line ("for Languages use (""Ada"");");
      Indent_Line ("for Source_Dirs use (""../.."", ""."");");
      New_Line;
      Indent_Line ("for Source_Files use");
      Indent_Line ("  (");
      Indent := Indent + 3;
      Indent_Line ("""emacs_module_aux.ads"",");
      Indent_Line ("""emacs_module_aux.adb"",");
      Indent_Line ("""emacs_module_h.ads"",");
      Indent_Line ("""fasttoken-lexer-wisi_elisp.adb"",");
      Indent_Line ("""fasttoken-lexer-wisi_elisp.ads"",");
      Indent_Line ("""" & Lower_Package_Name_Root & "_module.adb"",");
      Indent_Line ("""" & Lower_Package_Name_Root & "_module.ads""");
      Indent := Indent - 3;
      Indent_Line ("  );");
      New_Line;
      Indent_Line ("for Object_Dir use ""libobjsjlj"";");
      Indent_Line ("for Library_Name use """ & Lower_Package_Name_Root & "_wisi_module_parse"";");
      Indent_Line ("for Library_Dir use ""libsjlj"";");
      --  This library is linked with *_wisi_module_parse_wrapper.c to
      --  make a dynamic library
      Indent_Line ("for Library_Kind use ""static"";");
      New_Line;
      Indent_Line ("package Compiler is");
      Indent := Indent + 3;
      Indent_Line
        ("for Default_Switches (""Ada"") use Wisi_Module_Parse_Common.Compiler'Default_Switches (""Ada"");");

      --  Grammar files can get very large, so they need some special switches:
      --
      --  'Wisi_Module_Parse_Common.Compiler'Default_Switches' includes 'gnatn', but that hangs
      Indent_Line ("case Wisi_Module_Parse_Common.Build is");
      Indent_Line ("when ""Debug"" =>");
      Indent_Line ("   for Switches (""" & Lower_Package_Name_Root & "_module.adb"") use");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("     (""-O0"");");
      Indent_Line ("when ""Normal"" =>");
      Indent_Line ("   for Switches (""" & Lower_Package_Name_Root & "_module.adb"") use");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("     (""-O2"");");
      Indent_Line ("end case;");

      Indent := Indent - 3;
      Indent_Line ("end Compiler;");
      New_Line;
      Indent_Line ("package Builder is");
      Indent_Line
        ("   for Default_Switches (""Ada"") use Wisi_Module_Parse_Common.Builder'Default_Switches (""Ada"");");
      Indent_Line ("end Builder;");
      Indent := Indent - 3;
      New_Line;
      Indent_Line ("end " & Package_Name_Root & "_Wisi_Module_Parse;");
      Set_Output (Standard_Output);
      Close (File);

      Create (File, Out_File, Output_File_Name_Root & "_wisi_module_parse_agg.gpr");
      Set_Output (File);
      Indent := 1;
      Put_Line ("-- generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line ("-- ");
      Indent_Line ("aggregate project " & Package_Name_Root & "_Wisi_Module_Parse_Agg is");
      Indent_Line ("   for Project_Path use (external (""WISI_FASTTOKEN""));");
      Indent_Line ("   for Project_files use (""" & Lower_Package_Name_Root & "_wisi_module_parse.gpr"");");
      Indent_Line ("end " & Package_Name_Root & "_Wisi_Module_Parse_Agg;");
      Set_Output (Standard_Output);
      Close (File);

      Create (File, Out_File, Output_File_Name_Root & "_wisi_module_parse_wrapper.c");
      Set_Output (File);
      Indent := 1;
      Put_Line ("// generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line ("// ");
      Indent_Line ("//  This file is just a wrapper around the Ada code in");
      Indent_Line ("//  *_wisi_module_parse.adb; it is needed to call adainit.");
      Indent_Line ("#include <emacs_module.h>");
      Indent_Line ("int plugin_is_GPL_compatible;");
      Indent_Line ("extern void adainit(void);");
      Indent_Line ("extern int " & Lower_Package_Name_Root & "_wisi_module_parse_init (emacs_env *env);");
      Indent_Line ("/* Parse current buffer, using parser in current module. */");
      Indent_Line ("extern emacs_value " & Lower_Package_Name_Root & "_wisi_module_parse (emacs_env *env);");
      Indent_Line ("static emacs_value Fparse (emacs_env *env, int nargs, emacs_value args[])");
      Indent_Line ("{");
      Indent_Line ("  return " & Lower_Package_Name_Root & "_wisi_module_parse (env);");
      Indent_Line ("}");
      New_Line;
      Indent_Line ("int emacs_module_init (struct emacs_runtime *ert)");
      Indent_Line ("{");
      Indent_Line ("  emacs_env *env = ert->get_environment (ert);");
      Indent_Line
        ("  env->bind_function (env, """ & Lower_Package_Name_Root &
           "-wisi-module-parse"", env->make_function (env, 1, 1, Fparse));");
      Indent_Line ("  adainit();");
      Indent_Line ("  return " & Lower_Package_Name_Root & "_wisi_module_parse_init (env);");
      Indent_Line ("}");
      Set_Output (Standard_Output);
      Close (File);
   end Create_Module_Aux;

begin
   Common.Initialize (Input_File_Name, Output_File_Name_Root, Check_Interface => True);
   Wisi.Utils.Error := False;

   case Data.Lexer is
   when re2c_Lexer =>
      null;

   when Elisp_Lexer =>
      raise Programmer_Error with "Ada_Emacs requires re2c lexer";
   end case;

   Create_Ada_Spec
     (Input_File_Name,
      Output_File_Name => Output_File_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_process",
         when Module  => "_module") &
        ".ads",
      Package_Name => -Data.Package_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_Process",
         when Module  => "_Module"),
      Output_Language => Ada_Emacs,
      Descriptor      => Generate_Utils.LALR_Descriptor,
      Interface_Kind  => Generate_Params.Interface_Kind,
      Declare_Enum    => Declare_Enum);

   Create_Ada_Body;

   Create_re2c (Input_File_Name, Output_File_Name_Root, Elisp_Names.Regexps);

   case Data.Interface_Kind is
   when Process =>
      Create_Process_Elisp;

   when Module =>
      Create_Module_Elisp;
      Create_Module_Aux;
   end case;

   if Wisi.Utils.Error then
      Wisi.Utils.Put_Error (Input_File_Name, 1, "Errors: aborting");
      raise WisiToken.Syntax_Error;
   end if;
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada_Emacs;
