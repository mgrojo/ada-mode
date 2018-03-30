--  Abstract :
--
--  Parse the declarations from Input_File, add to various lists.
--
--  Copyright (C) 2012 - 2014, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Declarations
  (Input_File       : in     Standard.Ada.Text_IO.File_Type;
   Generate_Params  : in out Generate_Param_Type;
   Tokens           :    out Wisi.Tokens;
   Elisp_Names      :    out Wisi.Elisp_Names;
   Conflicts        :    out Conflict_Lists.List;
   McKenzie_Recover : in out McKenzie_Recover_Param_Type)
is
   --  McKenzie_Recover is 'in out' to preserve defaults.

   use Standard.Ada.Strings.Fixed;

   Action_Declaration_Str        : constant String := "%action_declaration";
   Case_Insensitive_Str          : constant String := "%case_insensitive";
   Conflict_Str                  : constant String := "%conflict";
   End_If_Str                    : constant String := "%end if";
   Elisp_Face_Str                : constant String := "%elisp_face";
   Elisp_Indent_Str              : constant String := "%elisp_indent";
   Elisp_Regexp_Str              : constant String := "%elisp_regexp";
   First_Parser_Label_Str        : constant String := "%first_parser_label";
   First_State_Index_Str         : constant String := "%first_state_index";
   If_Str                        : constant String := "%if lexer =";
   Interface_Str                 : constant String := "%interface";
   Keyword_Str                   : constant String := "%keyword";
   Lexer_Str                     : constant String := "%lexer";
   End_Names_Optional_Option_Str : constant String := "%end_names_optional_option";
   McKenzie_Check_Limit_Str      : constant String := "%mckenzie_check_limit";
   McKenzie_Cost_Default_Str     : constant String := "%mckenzie_cost_default";
   McKenzie_Cost_Delete_Str      : constant String := "%mckenzie_cost_delete";
   McKenzie_Cost_Insert_Str      : constant String := "%mckenzie_cost_insert";
   McKenzie_Cost_Limit_Str       : constant String := "%mckenzie_cost_limit";
   Non_Grammar_Str               : constant String := "%non_grammar";
   Output_Language_Str           : constant String := "%output_language";
   Parser_Algorithm_Str          : constant String := "%parser_algorithm";
   re2c_Regexp_Str               : constant String := "%re2c_regexp";
   Start_Str                     : constant String := "%start";
   Token_Str                     : constant String := "%token";

   If_Active : Boolean := False;
   --  If true, ignore all declarations except End_If_Str.

   function Index_Blank (Source : in String; From : in Positive) return Natural
   is begin
      return Index (Pattern => " ", Source => Source, From => From);
   end Index_Blank;

begin
   loop
      declare
         Line     : constant String := Skip_Comments (Input_File);
         Key_Last : Integer         := Line'First;

         function Match (ID : in String) return Boolean
         is begin
            Key_Last := ID'Length;
            return (ID'Length < Line'Last and then (ID = Line (1 .. ID'Length) and ' ' = Line (ID'Length + 1))) or else
              (ID'Length = Line'Last and then ID = Line);
         end Match;

         procedure Parse_Kind_Value (List : in out Token_Lists.List)
         is
            use Standard.Ada.Strings.Unbounded;

            --  syntax: <kind> NAME value
            --  strip < > from kind.

            Kind_First : constant Integer := 1 + Index_Non_Blank (Line, Key_Last + 1);

            Kind_Last : constant Integer := -2 +
              Index (Pattern => " ", Source => Line, From => Kind_First);

            Name_First : constant Integer := Index_Non_Blank (Line, Kind_Last + 2);

            Name_Last : constant Integer := -1 + Index (Pattern => " ", Source => Line, From => Name_First);

            Value_First : constant Integer :=
              (if Name_Last = -1 then 0
               else Index_Non_Blank (Line, Name_Last + 1));
         begin
            if Value_First = 0 then
               Add_Token
                 (List,
                  Kind  => Line (Kind_First .. Kind_Last),
                  Name  => Line (Name_First .. Line'Last),
                  Value => "");
            else
               Add_Token
                 (List,
                  Line (Kind_First .. Kind_Last),
                  Line (Name_First .. Name_Last),
                  Line (Value_First .. Line'Last));
            end if;
         end Parse_Kind_Value;
      begin
         exit when Line = "%%";

         if If_Active then
            if Match (End_If_Str) then
               If_Active := False;
            end if;

         elsif Match (Action_Declaration_Str) then
            Generate_Params.Action_Declarations.Append (Line (Key_Last + 2 .. Line'Last));

         elsif Match (Case_Insensitive_Str) then
            Generate_Params.Case_Insensitive := True;

         elsif Match (Conflict_Str) then
            declare
               Action_A_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Action_A_Last  : constant Integer := -1 +
                 Index (Pattern => "/", Source => Line, From => Action_A_First);
               Action_B_First : constant Integer := 2 + Action_A_Last;
               Action_B_Last  : constant Integer := -1 +
                 Index (Pattern => " ", Source => Line, From => Action_B_First);
               Skip_1         : constant Integer := 8 +
                 Index (Pattern => "in state", Source => Line, From => Action_B_Last);
               LHS_A_First    : constant Integer := Index_Non_Blank (Line, Skip_1 + 1);
               LHS_A_Last     : constant Integer := -1 +
                 Index (Pattern => ",", Source => Line, From => LHS_A_First);
               LHS_B_First    : constant Integer := Index_Non_Blank (Line, LHS_A_Last + 2);
               LHS_B_Last     : constant Integer := -1 +
                 Index (Pattern => " ", Source => Line, From => LHS_B_First);
               Skip_2         : constant Integer := 8 +
                 Index (Pattern => "on token", Source => Line, From => LHS_B_Last);
               Token_First    : constant Integer := Index_Non_Blank (Line, Skip_2 + 1);
               Token_Last     : constant Integer := Line'Last;
            begin
               Conflicts.Append
                 ((Standard.Ada.Text_IO.Line (Input_File) - 1,
                   +Line (Action_A_First .. Action_A_Last),
                   +Line (LHS_A_First .. LHS_A_Last),
                   +Line (Action_B_First .. Action_B_Last),
                   +Line (LHS_B_First .. LHS_B_Last),
                   +Line (Token_First .. Token_Last)));
            end;

         elsif Match (End_If_Str) then
            --  matching '%if' specified current lexer.
            null;

         elsif Match (Elisp_Face_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               Elisp_Names.Faces.Append (Line (Value_First .. Line'Last));
            end;

         elsif Match (Elisp_Indent_Str) then
            declare
               Name_First  : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Name_Last   : constant Integer := -1 + Index_Blank (Line, Name_First);
               Value_First : constant Integer := Index_Non_Blank (Line, Name_Last + 1);
            begin
               Elisp_Names.Indents.Append ((+Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (Elisp_Regexp_Str) then
            declare
               Name_First  : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Name_Last   : constant Integer := -1 + Index_Blank (Line, Name_First);
               Value_First : constant Integer := Index_Non_Blank (Line, Name_Last + 1);
            begin
               Elisp_Names.Regexps.Append ((+Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (End_Names_Optional_Option_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               Generate_Params.End_Names_Optional_Option := +Line (Value_First .. Line'Last);
            end;

         elsif Match (First_Parser_Label_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               Generate_Params.First_Parser_Label := Integer'Value (Line (Value_First .. Line'Last));
            end;

         elsif Match (First_State_Index_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               Generate_Params.First_State_Index := Integer'Value (Line (Value_First .. Line'Last));
            end;

         elsif Match (If_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               If_Active := Generate_Params.Lexer /= To_Lexer (Line (Value_First .. Line'Last));
            end;

         elsif Match (Interface_Str) then
            if Generate_Params.Interface_Kind = None then
               declare
                  Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               begin
                  Generate_Params.Interface_Kind := Valid_Interface'Value (Line (Value_First .. Line'Last));
               end;
            end if;

         elsif Match (Keyword_Str) then
            declare
               Name_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);

               Name_Last : constant Integer := -1 + Index_Blank (Line, Name_First);

               Value_First : constant Integer := Index_Non_Blank (Line, Name_Last + 1);
            begin
               Tokens.Keywords.Append ((+Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (Lexer_Str) then
            if Generate_Params.Lexer = None then
               declare
                  Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               begin
                  Generate_Params.Lexer := To_Lexer (Line (Value_First .. Line'Last));
               end;
            end if;

         elsif Match (McKenzie_Check_Limit_Str) then
            McKenzie_Recover.Check_Limit := Integer'Value (Line (Key_Last + 1 .. Line'Last));

         elsif Match (McKenzie_Cost_Default_Str) then
            declare
               Insert_First         : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Insert_Last          : constant Integer := -1 + Index_Blank (Line, Insert_First);
               Delete_Term_First    : constant Integer := Index_Non_Blank (Line, Insert_Last + 1);
               Delete_Term_Last     : constant Integer := -1 + Index_Blank (Line, Delete_Term_First);
               Delete_Nonterm_First : constant Integer := Index_Non_Blank (Line, Delete_Term_Last + 1);
               Delete_Nonterm_Last  : constant Integer := -1 + Index_Blank (Line, Delete_Nonterm_First);
               Delete_Push_Back_First : constant Integer := Index_Non_Blank (Line, Delete_Nonterm_Last + 1);
               Delete_Push_Back_Last  : constant Integer := -1 + Index_Blank (Line, Delete_Push_Back_First);
            begin
               McKenzie_Recover.Default_Insert := Natural'Value (Line (Insert_First .. Insert_Last));
               McKenzie_Recover.Default_Delete_Terminal := Natural'Value (Line (Delete_Term_First .. Delete_Term_Last));
               McKenzie_Recover.Default_Delete_Nonterminal := Natural'Value
                 (Line (Delete_Nonterm_First .. Delete_Nonterm_Last));
               McKenzie_Recover.Default_Push_Back := Natural'Value
                 (Line (Delete_Push_Back_First .. Delete_Push_Back_Last));
               McKenzie_Recover.Default_Push_Back := Natural'Value (Line (Delete_Push_Back_Last + 1 .. Line'Last));
            end;

         elsif Match (McKenzie_Cost_Delete_Str) then
            declare
               Keyword_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Keyword_Last  : constant Integer := -1 + Index_Blank (Line, Keyword_First);
            begin
               McKenzie_Recover.Delete.Append
                 ((Name  => +Line (Keyword_First .. Keyword_Last),
                   Value => +Line (Keyword_Last + 1 .. Line'Last)));
            end;

         elsif Match (McKenzie_Cost_Insert_Str) then
            declare
               Keyword_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Keyword_Last  : constant Integer := -1 + Index_Blank (Line, Keyword_First);
            begin
               McKenzie_Recover.Insert.Append
                 ((Name  => +Line (Keyword_First .. Keyword_Last),
                   Value => +Line (Keyword_Last + 1 .. Line'Last)));
            end;

         elsif Match (McKenzie_Cost_Limit_Str) then
            McKenzie_Recover.Cost_Limit := Integer'Value (Line (Key_Last + 1 .. Line'Last));

         elsif Match (Output_Language_Str) then
            if Generate_Params.Output_Language = None then
               declare
                  Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               begin
                  Generate_Params.Output_Language := Valid_Output_Language'Value (Line (Value_First .. Line'Last));
               end;
            end if;

         elsif Match (Non_Grammar_Str) then
            --  Same syntax as Token_Str
            Parse_Kind_Value (Tokens.Non_Grammar);

         elsif Match (Parser_Algorithm_Str) then
            if Generate_Params.Parser_Algorithm = None then
               declare
                  Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               begin
                  Generate_Params.Parser_Algorithm := Valid_Parser_Algorithm'Value (Line (Value_First .. Line'Last));
               end;
            end if;

         elsif Match (re2c_Regexp_Str) then
            declare
               Name_First  : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
               Name_Last   : constant Integer := -1 + Index_Blank (Line, Name_First);
               Value_First : constant Integer := Index_Non_Blank (Line, Name_Last + 1);
            begin
               Tokens.Regexps.Append ((+Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (Start_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Line, Key_Last + 1);
            begin
               Generate_Params.Start_Token := +Line (Value_First .. Line'Last);
            end;

         elsif Match (Token_Str) then
            Parse_Kind_Value (Tokens.Tokens);

         else
            Put_Error (Input_File, "unexpected syntax");
            raise Syntax_Error;
         end if;
      exception
      when E : others =>
         Utils.Put_Error (Input_File, "syntax error: " & Standard.Ada.Exceptions.Exception_Message (E));
         raise Syntax_Error;
      end;
   end loop;

   if Generate_Params.Output_Language = None then
      Wisi.Utils.Put_Error (Input_File, "output_language not set in grammar file");
      raise User_Error with "missing grammar file directive";
   end if;

   if Generate_Params.Parser_Algorithm = None then
      Wisi.Utils.Put_Error (Input_File, "parser_algorithm not set in grammar file");
      raise User_Error with "missing grammar file directive";
   end if;
end Wisi.Declarations;
