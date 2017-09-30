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
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
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
with WisiToken.Parser.LR.LALR_Generator;
with WisiToken.Parser.LR.LR1_Generator;
procedure Wisi.Output_Ada_Emacs
  (Input_File_Name       : in String;
   Output_File_Name_Root : in String;
   Params                : in Generate_Param_Type;
   Prologues             : in Wisi.Prologues;
   Tokens                : in Wisi.Tokens;
   Conflicts             : in Conflict_Lists.List;
   McKenzie_Recover      : in McKenzie_Recover_Param_Type;
   Rule_Count            : in Integer;
   Action_Count          : in Integer;
   Profile               : in Boolean)
is
   use all type Standard.Ada.Containers.Count_Type;

   package Common is new Wisi.Gen_Output_Ada_Common (Prologues, Tokens, Conflicts, Params);
   use Common;

   procedure Create_Ada_Action
     (Name        : in String;
      Action      : in String_Lists.List;
      Source_Line : in Standard.Ada.Text_IO.Positive_Count)
   is
      use Standard.Ada.Strings.Fixed;
      use Standard.Ada.Strings.Unbounded;
      use Wisi.Utils;

      Temp : Unbounded_String;

      Paren_State           : Integer := 0;
      Translate_Paren_State : Integer := 0;

      Navigate_Lines     : String_Lists.List;
      Face_Line          : Unbounded_String;
      Indent_Action_Line : Unbounded_String;

      Space_Paren_Set : constant Standard.Ada.Strings.Maps.Character_Set := Standard.Ada.Strings.Maps.To_Set (" ])");

      procedure Count_Parens
      is
         Line : String renames To_String (Temp);
      begin
         for I in Line'First .. Line'Last loop
            case Line (I) is
            when '(' =>
               Paren_State := Paren_State + 1;
            when ')' =>
               Paren_State := Paren_State - 1;
            when others =>
               null;
            end case;
         end loop;
      end Count_Parens;

      function Statement_Params (Params : in String) return String
      is
         --  Input looks like: [1 function 2 other ...]
         Last       : Integer          := Params'First; -- skip [
         First      : Integer;
         Second     : Integer;
         Need_Comma : Boolean          := False;
         Result     : Unbounded_String := +" (Nonterm, Source, (";
      begin
         loop
            First  := Last + 1;
            Second := Index (Params, " ", First);
            exit when Second < Params'First;

            Last := Index (Params, Space_Paren_Set, Second + 1);

            Result := Result & (if Need_Comma then ", " else "") &
              "(" & Params (First .. Second - 1) & "," &
              Integer'Image (Find_Class_ID (Params (Second + 1 .. Last - 1))) & ")";

            Need_Comma := True;
         end loop;
         Result := Result & "))";
         return -Result;
      end Statement_Params;

      function Containing_Params (Params : in String) return String
      is
         --  Input looks like: 1 2)
         First  : constant Integer := Params'First;
         Second : constant Integer := Index (Params, " ", First);
      begin
         return " (Nonterm, Source, " & Params (First .. Second - 1) & ',' & Params (Second .. Params'Last);
      end Containing_Params;

      function Motion_Params (Params : in String) return String
      is
         --  Input looks like: [1 [2 EXCEPTION WHEN] 3 ...]
         --  Result: Motion_Param_Array'((0, 1, Empty_IDs), (2, 2, (3, 8)), (0, 3, Empty_IDs))
         use Generate_Utils;
         use Standard.Ada.Strings.Maps;
         use WisiToken;

         Delim : constant Character_Set := To_Set (" ]");

         Last   : Integer          := Params'First; -- skip [
         First  : Integer;
         Vector : Boolean;
         Result : Unbounded_String := +" (Nonterm, Source, (";

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
                     IDs := IDs & (if Need_Comma_2 then ", " else "") &
                       Int_Image (Find_Token_ID (Params (First .. Last - 1)));
                     Need_Comma_2 := True;
                  exception
                  when E : Not_Found =>
                     Put_Error (Input_File_Name, Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
                  end;
               end loop;

               Result := Result & (if Need_Comma_1 then ", " else "") & "(" & Int_Image (IDs_Count) & ", " &
                 Params (Index_First .. Index_Last) & ", (" &
                 (-IDs) & "))";
            else
               First  := Index_Non_Blank (Params, Last);
               Last   := Index (Params, Delim, First);
               Result := Result & (if Need_Comma_1 then ", " else "") &
                 "(0, " & Params (First .. Last - 1) & ", Empty_IDs)";
            end if;
            Need_Comma_1 := True;
         end loop;
         return -(Result & ')');
      end Motion_Params;

      function Face_Apply_Params (Params : in String) return String
      is
         --  Params is a vector of triples: [1 nil font-lock-keyword-face 3 nil font-lock-function-name-face ...]
         --  Result: ((1, 3, 1), (3, 3, 2), ...)
         use Standard.Ada.Strings.Maps;
         Delim : constant Character_Set := To_Set (" ]");

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
               Put_Error (Input_File_Name, Source_Line, "invalid wisi-face-apply argument");
               exit;
            end if;

            First  := Index_Non_Blank (Params, Last + 1);
            Last   := Index (Params, Delim, First);
            Result := Result & ',' & Integer'Image (Find_Face_ID (Params (First .. Last - 1)));

            if Params (Last) = ']' then
               Put_Error (Input_File_Name, Source_Line, "invalid wisi-face-apply argument");
               exit;
            end if;

            First  := Index_Non_Blank (Params, Last + 1);
            Last   := Index (Params, Delim, First);
            Result := Result & ',' & Integer'Image (Find_Face_ID (Params (First .. Last - 1))) & ")";

            Need_Comma := True;
         end loop;
         if Count = 1 then
            return " (Nonterm, Source, (1 => " & (-Result) & "))";
         else
            return " (Nonterm, Source, (" & (-Result) & "))";
         end if;
      end Face_Apply_Params;

      function Indent_Params (Params : in String) return String
      is
         --  Params is a vector, one item for each token in Source. Each item is one of:
         --
         --  - an integer; copy to output
         --
         --  - a symbol; lookup in elisp_names.indents
         --
         --  - a lisp function call with 2 args (wisi-anchored% 3 ada-indent-broken)
         --    first arg is token id, second is indent (integer or symbol)
         --
         --  - a vector with two elements [code_indent comment_indent].

         use Standard.Ada.Strings.Maps;
         Delim : constant Character_Set := To_Set (" ])");

         subtype Digit is Character range '0' .. '9';

         Last       : Integer          := Params'First; -- skip [
         First      : Integer;
         Result     : Unbounded_String := +" (Nonterm, Source, (";
         Need_Comma : Boolean          := False;

         function Int_Or_Symbol (First : in Integer) return String
         is begin
            Last := Index (Params, Delim, First);
            if Params (First) in Digit then
               return Params (First .. Last - 1);

            else
               return Elisp_Name_To_Ada (Params (First .. Last - 1), False, 0);
            end if;
         exception
         when E : Not_Found =>
            Put_Error (Input_File_Name, Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
            return "";
         end Int_Or_Symbol;

         function Indent_Function (Elisp_Name : in String) return String
         is begin
            if    Elisp_Name = "wisi-anchored"   then return "Anchored_0";
            elsif Elisp_Name = "wisi-anchored%"  then return "Anchored_0";
            elsif Elisp_Name = "wisi-anchored%-" then return "Anchored_1";
            elsif Elisp_Name = "wisi-anchored*"  then return "Anchored_2";
            elsif Elisp_Name = "wisi-anchored*-" then return "Anchored_3";
            else
               Put_Error (Input_File_Name, Source_Line, "unrecognized wisi indent function: '" & Elisp_Name & "'");
               return "";
            end if;
         end Indent_Function;

      begin
         loop
            Last := Index_Non_Blank (Params, Last + 1);

            exit when Params (Last) = ']';

            if Need_Comma then
               Result := Result & ", ";
            else
               Need_Comma := True;
            end if;

            case Params (Last) is
            when '(' =>
               --  lisp function call
               First := Last + 1;
               Last  := Index (Params, Delim, First);
               if Params (Last) /= ' ' then
                  Put_Error (Input_File_Name, Source_Line, "invalid indent function call");
               end if;

               Result := Result & "(False, " & Indent_Function (Params (First .. Last - 1)) & " (";

               First := Last + 1;
               Last  := Index (Params, Delim, First);
               if Params (Last) /= ' ' then
                  Put_Error (Input_File_Name, Source_Line, "invalid indent function call");
               end if;

               Result := Result & Params (First .. Last - 1);

               Result := Result & ", " & Int_Or_Symbol (Last + 1) & "))";

               if Params (Last) /= ')' then
                  Put_Error (Input_File_Name, Source_Line, "invalid indent syntax");
               end if;

            when '[' =>
               --  vector
               Result := Result & "(True, " & Int_Or_Symbol (Last + 1);
               Result := Result & ", " & Int_Or_Symbol (Last + 1) & ')';
               if Params (Last) /= ']' then
                  Put_Error (Input_File_Name, Source_Line, "invalid indent syntax");
               end if;

            when others =>
               --  integer or symbol
               Result := Result & "(False, " & Int_Or_Symbol (Last) & ')';

            end case;
         end loop;
         return -(Result & "))");
      end Indent_Params;

      procedure Translate_Line (Line : in String)
      is
         Last       : constant Integer := Index (Line, " ");
         Elisp_Name : constant String  := Line (Line'First + 1 .. Last - 1);
      begin
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
               Put_Error (Input_File_Name, Source_Line, "multiple face actions");
            end if;

         elsif Elisp_Name = "wisi-indent-action" then
            if Length (Indent_Action_Line) = 0 then
               Indent_Action_Line := +Elisp_Name_To_Ada (Elisp_Name, False, Trim => 5) &
                 Indent_Params (Line (Last + 1 .. Line'Last)) & ";";
            else
               Put_Error (Input_File_Name, Source_Line, "multiple indent actions");
            end if;
         else
            Put_Error (Input_File_Name, Source_Line, "unrecognized elisp action: " & Elisp_Name);
         end if;
      end Translate_Line;

   begin
      Indent_Line ("procedure " & Name);
      Indent_Line (" (Nonterm : in WisiToken.Augmented_Token'Class;");
      Indent_Line ("  Index   : in Natural;");
      Indent_Line ("  Source  : in WisiToken.Augmented_Token_Array)");
      Indent_Line ("is");
      Indent_Line ("   pragma Unreferenced (Index);");
      Indent_Line ("begin");
      Indent := Indent + 3;

      for Line of Action loop
         begin
            if Translate_Paren_State = 0 and Length (Temp) = 0 then
               if Line = "(progn" then
                  Translate_Paren_State := 1;
                  Paren_State := 1;
               elsif String_Lists.Length (Action) = 1 then
                  Translate_Paren_State := 0;
                  Temp := +Line;
                  Count_Parens;
                  if Paren_State = 0 then
                     Translate_Line (-Temp);
                     Temp := +"";
                  end if;
               else
                  Put_Error (Input_File_Name, Source_Line, "invalid action syntax");
                  return;
               end if;
            else
               if Length (Temp) = 0 then
                  Temp := +Line;
               else
                  Temp := Temp & ' ' & Line;
               end if;

               Count_Parens;
               if Paren_State = Translate_Paren_State then
                  Translate_Line (-Temp);
                  Temp := +"";
               elsif Translate_Paren_State = 1 and Paren_State = 0 then
                  --  last line of a 'progn'
                  Translate_Line (Slice (Temp, 1, Length (Temp) - 1));
                  Temp := +"";
               end if;
            end if;
         exception
         when E : Not_Found =>
            Put_Error (Input_File_Name, Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
            Temp := +"";
         end;
      end loop;

      Indent_Line ("case Wisi.Runtime.Parse_Action is");
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

      Indent := Indent - 3;
      Indent_Line ("end " & Name & ";");
      New_Line;

   end Create_Ada_Action;

   procedure Create_Ada_Body
   is
      use all type WisiToken.Parser.LR.Unknown_State_Index;
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
         Parsers (LALR) := WisiToken.Parser.LR.LALR_Generator.Generate
           (Data.Grammar,
            LALR_Descriptor,
            WisiToken.Parser.LR.State_Index (Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count, Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := Parsers (LALR).State_Last - Parsers (LALR).State_First + 1;
      end if;

      if Data.Parser_Algorithm in LR1 | LALR_LR1 then
         Parsers (LR1) := WisiToken.Parser.LR.LR1_Generator.Generate
           (Data.Grammar,
            LR1_Descriptor,
            WisiToken.Parser.LR.State_Index (Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count, Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := WisiToken.Parser.LR.Unknown_State_Index'Max
           (Data.Parser_State_Count,
            Parsers (LR1).State_Last - Parsers (LR1).State_First + 1);
      end if;

      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Comment, Prologues.Body_Context_Clause);

      Put_Line ("with WisiToken.Lexer.re2c;");
      Put_Line ("with WisiToken.Wisi_Runtime;");
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

      Indent_Line ("use WisiToken.Wisi_Runtime;");
      New_Line;

      Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
      Indent_Line ("  (" & Output_File_Name_Root & "_re2c_c.New_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Free_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Reset_Lexer,");
      Indent_Line ("   " & Output_File_Name_Root & "_re2c_c.Next_Token);");
      New_Line;

      --  generate Action subprograms, populate Action_Names, Elisp_Names
      --  (for non-indent actions).

      for Rule of Tokens.Rules loop
         --  No need for a Token_Cursor here, since we only need the nonterminals.
         declare
            LHS_ID : constant Generate_Utils.Nonterminal_ID := Find_Token_ID (-Rule.Left_Hand_Side);

            Prod_Index : Integer := 0; -- Semantic_Action defines Prod_Index as zero-origin
            All_Empty  : Boolean := True;
            Temp_Ada   : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
         begin
            for RHS of Rule.Right_Hand_Sides loop
               if RHS.Action.Length > 0 then
                  All_Empty := False;
                  declare
                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Int_Image (Prod_Index);
                  begin
                     Temp_Ada (Prod_Index) := new String'(Name & "'Access");
                     Create_Ada_Action (Name, RHS.Action, RHS.Source_Line);
                  end;
               end if;
               Prod_Index := Prod_Index + 1;
            end loop;

            if not All_Empty then
               Ada_Action_Names (LHS_ID) := new Action_Name_List'(Temp_Ada);
            end if;
         end;
      end loop;

      Create_Create_Parser
        (Data.Parser_Algorithm, Data.Lexer, Data.Interface_Kind, Params.First_State_Index, Params.First_Parser_Label,
         New_Line_ID => Generate_Utils.Find_Kind ("new-line"));

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

      if Verbosity > 0 then
         --  Match wisi-output_elisp, wisi-output_ada format
         Put_Line
           (Integer'Image (Rule_Count) & " rules," &
              Integer'Image (Action_Count) & " actions," &
              WisiToken.Parser.LR.State_Index'Image (Data.Parser_State_Count) & " states," &
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
      use all type WisiToken.Token_ID;
      use all type RHS_Lists.Cursor;
      use all type Rule_Lists.Cursor;

      File         : File_Type;
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
      Put_Line ("(require 'semantic/lex)");
      Put_Line ("(require 'wisi-process-parse)");
      New_Line;

      Output_Elisp_Common.Indent_Keyword_Table (Output_File_Name_Root, "elisp", Tokens.Keywords, To_String'Access);
      Output_Elisp_Common.Indent_Token_Table (Output_File_Name_Root, "elisp", Tokens.Tokens, To_String'Access);

      --  FIXME: Used for error messages?
      Indent_Line  ("(defconst " & Output_File_Name_Root & "-process-token-table");
      Indent_Line  ("  (wisi-process-compile-tokens");
      Indent_Start ("   [");
      Indent := Indent + 4;
      for Cursor in All_Tokens.Iterate loop
         if Paren_1_Done then
            Indent_Line (Name (Cursor));
         else
            Paren_1_Done := True;
            Put_Line (Name (Cursor));
         end if;

      end loop;
      Indent_Line ("]))");
      Indent := Indent - 4;
      New_Line;

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

      Put_Line ("(require 'semantic/lex)");
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
      raise Programmer_Error;
   end case;

   Create_Ada_Spec
     (Input_File_Name,
      Output_File_Name => Output_File_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_process",
         when Module => "_module") &
        ".ads",
      Package_Name => -Data.Package_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_Process",
         when Module     => "_Module"),
      Output_Language    => Ada_Emacs,
      Descriptor         => Generate_Utils.LALR_Descriptor,
      Interface_Kind     => Params.Interface_Kind,
      Lexer              => Params.Lexer);

   Create_Ada_Body;

   if not Profile then
      case Data.Interface_Kind is
      when Process =>
         Create_Process_Elisp;

      when Module =>
         Create_Module_Elisp;
         Create_Module_Aux;
      end case;
   end if;

   if Wisi.Utils.Error then
      Wisi.Utils.Put_Error (Input_File_Name, 1, "Errors: aborting");
      raise Syntax_Error;
   end if;
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada_Emacs;
--  Local Variables:
--  jit-lock-defer-time: 0.25
--  End:
