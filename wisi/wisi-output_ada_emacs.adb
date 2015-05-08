--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The parser actions
--  assume the Emacs wisi indentation engine
--
--  If run in a separate process communicating over pipes with the
--  Emacs process, the parser actions output encoded elisp actions.
--
--  If run in an Emacs dynamically loaded module, the parser actions
--  call the elisp wisi actions directly.
--
--  Copyright (C) 2012 - 2015 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with OpenToken;
with Wisi.Gen_Generate_Utils;
with Wisi.Utils;
procedure Wisi.Output_Ada_Emacs
  (Input_File_Name    : in String;
   Output_File_Root   : in String;
   Prologue           : in String_Lists.List;
   Keywords           : in String_Pair_Lists.List;
   Tokens             : in Token_Lists.List;
   Start_Token        : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts          : in Conflict_Lists.List;
   Rules              : in Rule_Lists.List;
   Rule_Count         : in Integer;
   Action_Count       : in Integer;
   Lexer              : in Lexer_Type;
   Interface_Kind     : in Interface_Type;
   First_State_Index  : in Integer;
   First_Parser_Label : in Integer;
   Profile            : in Boolean)
is
   use type Ada.Containers.Count_Type;

   EOI_Name              : constant Ada.Strings.Unbounded.Unbounded_String := +"EOF";
   OpenToken_Accept_Name : constant Ada.Strings.Unbounded.Unbounded_String := +"OPENTOKEN_ACCEPT";

   function To_Token_Ada_Name (Item : in String) return String
   is
      --  Convert Item to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := To_Upper (Item);
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

   function To_Token_Ada_Name (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return To_Token_Ada_Name (-Item);
   end To_Token_Ada_Name;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Name, OpenToken_Accept_Name, First_State_Index, To_Token_Ada_Name);

   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production_Lists.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LALRs.Parse_Table_Ptr := Generate_Utils.LALR_Generators.Generate
     (Grammar,
      Generate_Utils.To_Conflicts (Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
      Trace                    => Verbosity > 1,
      Put_Parse_Table          => Verbosity > 0,
      Ignore_Unused_Tokens     => Verbosity > 1,
      Ignore_Unknown_Conflicts => Verbosity > 1);

   function File_Name_To_Ada (File_Name : in String) return String
   is
      Result : String := File_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '.';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end File_Name_To_Ada;

   function Elisp_Name_To_Ada (Elisp_Name : in String) return String
   is
      Result : String := Elisp_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '_';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result & "_ID"; -- Some elisp names may be Ada reserved words;
   end Elisp_Name_To_Ada;

   Package_Name_Root : constant String := File_Name_To_Ada (Output_File_Root);

   Elisp_Names : String_Lists.List;
   --  Populated by Create_Parser_Body, used by Create_Process_Elisp,
   --  Create_Parser_Body for Module

   procedure Add_Elisp_Name (Item : in String)
   is
      use String_Lists;
   begin
      if Elisp_Names.Find (Item) /= No_Element then
         null;
      else
         Elisp_Names.Append (Item);
      end if;
   end Add_Elisp_Name;

   procedure Get_Elisp_Names (Line : in String)
   is
      --  Line is a wisi action:
      --  (wisi-statement-action [1 'block-start 2 'name-paren 5 'block-middle 7 'block-end 9 'statement-end])
      --  (wisi-containing-action 2 3)
      --  (wisi-motion-action 1 5 [6 block-middle EXCEPTION block-middle WHEN]])
      --  (wisi-face-action [2 'font-lock-keyword-face 4 'font-lock-type-face])
      --  ...
      --
      --  Enter all names in Elisp_Names
      I       : Integer := Line'First;
      First   : Integer := Line'First - 1;
      In_Name : Boolean := False;
   begin
      loop
         exit when not In_Name and I > Line'Last;

         if In_Name then
            if I > Line'Last or else
              (Line (I) = ' ' or Line (I) = ']' or Line (I) = ')')
            then
               In_Name := False;
               if Line (First .. I - 1) = "progn" then
                  null; -- special form, not symbol
               else
                  Add_Elisp_Name (Line (First .. I - 1));
               end if;
            else
               null;
            end if;
         else
            case Line (I) is
            when '(' | ')' | '[' | ']' | '0' .. '9' | ''' | ' ' =>
               null;

            when others =>
               In_Name := True;
               First   := I;
            end case;
         end if;

         I := I + 1;
      end loop;
   end Get_Elisp_Names;

   function Find_Elisp_Name (Name : in String) return Integer
   is
      use String_Lists;

      Code   : Integer             := 0;
      Cursor : String_Lists.Cursor := Elisp_Names.First;
   begin
      loop
         exit when Cursor = No_Element;

         if Constant_Reference (Elisp_Names, Cursor) = Name then
            --  negative numbers for names in action args, to
            --  distguish them from other numbers.
            return -Code;
         end if;
         Code := Code + 1;
         Next (Cursor);
      end loop;
      raise Programmer_Error with "'" & Name & "' not found in Elisp_Names";
   end Find_Elisp_Name;

   function To_Code (Name : in String) return String
   is begin
      return OpenToken.Int_Image (Find_Elisp_Name (Name));
   end To_Code;

   --  Preserve In_Action across lines
   In_Action : Boolean := False;

   function To_Codes (Line : in String) return String
   is
      --  Return Line with names translated to codes
      Result : String (1 .. Line'Length);
      J      : Integer := Result'First - 1;

      I     : Integer := Line'First;
      First : Integer := Line'First - 1;

      In_Name      : Boolean := False;
      In_Func_Name : Boolean := False;

      procedure Add (Name : in String)
      is
         Image : constant String := To_Code (Name);
      begin
         Result (J + 1 .. J + Image'Length) := Image;
         J := J + Image'Length;
      end Add;

   begin
      loop
         exit when not In_Name and I > Line'Last;

         if In_Name then
            if I > Line'Last or else
              (Line (I) = ' ' or Line (I) = ']' or Line (I) = ')')
            then
               In_Name := False;
               if In_Func_Name then
                  In_Func_Name := False;
                  if Line (First .. I - 1) = "progn" then
                     In_Action := False;
                     J := J + 1;
                     Result (J) := '[';
                  else
                     J := J + 1;
                     Result (J) := '(';
                     Add (Line (First .. I - 1));
                     J := J + 1;
                     Result (J) := ' ';
                  end if;
               else
                  Add (Line (First .. I - 1));
                  if I <= Line'Last then
                     J := J + 1;
                     Result (J) := Line (I);
                  end if;
               end if;
            else
               null;
            end if;
         else
            case Line (I) is
            when '(' =>
               In_Func_Name := True;
               In_Action    := True;

            when ')' =>
               J := J + 1;
               if In_Action then
                  In_Action := False;
                  Result (J) := ')';
               else
                  --  replace (progn ...) with [ ... ]
                  Result (J) := ']';
               end if;

            when '[' | ']' | ' ' | ''' | '0' .. '9' =>
               J := J + 1;
               Result (J) := Line (I);

            when others =>
               In_Name := True;
               First   := I;
            end case;
         end if;

         I := I + 1;
      end loop;

      return Result (1 .. J);
   end To_Codes;

   procedure Put_Prologue (Ada_Syntax : in Boolean)
   is
   begin
      --  Prologue has elisp syntax. If Ada_Syntax, keep comments,
      --  ignore everything else.

      for Line of Prologue loop
         if Ada_Syntax and (Line'Length = 2 and then Line = ";;") then
            Put_Line ("--");
         elsif Ada_Syntax and (Line'Length > 2 and then Line (Line'First .. Line'First + 2) = ";; ") then
            Put_Line ("--  " & Line (Line'First + 2 .. Line'Last));
         elsif Ada_Syntax and (Line'Length > 0 and then Line (Line'First) = '(') then
            null;
         else
            Put_Line (Line);
         end if;
      end loop;
   end Put_Prologue;

   procedure Create_Ada_Spec
   is
      use Generate_Utils;

      File_Name : constant String := Output_File_Root &
        (case Interface_Kind is
         when Process => "_process",
         when Module  => "_module") &
        ".ads";

      Package_Name : constant String := Package_Name_Root &
        (case Interface_Kind is
         when Process => "_Process",
         when Module  => "_Module");

      Spec_File : File_Type;
      Cursor    : Token_Cursor;
   begin
      Create (Spec_File, Out_File, File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Syntax => True);

      case Interface_Kind is
      when Process =>
         Put_Line ("with OpenToken.Text_Feeder;");
      when Module =>
         Put_Line ("with Emacs_Module_Aux;");
         Put_Line ("with emacs_module_h;");
         Put_Line ("with Interfaces.C;");
      end case;
      Put_Line ("with OpenToken.Production.List;");
      Put_Line ("with OpenToken.Production.Parser.LALR.Generator;");
      Put_Line ("with OpenToken.Production.Parser.LALR.Parser;");
      Put_Line ("with OpenToken.Production.Parser.LALR.Parser_Lists;");
      Put_Line ("with OpenToken.Token.Nonterminal;");
      Put_Line ("with OpenToken.Wisi_Tokens;");
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;

      New_Line;
      Indent_Line ("type Token_IDs is");
      Indent_Line ("  (");
      Indent := Indent + 3;
      Cursor := First;
      loop
         exit when Cursor.Is_Done;
         Set_Col (Indent);
         Put (To_Token_Ada_Name (Cursor.Token_Name));

         Cursor.Next;

         if Cursor.Is_Done then
            Put_Line (");");
         else
            Put_Line (",");
         end if;
      end loop;
      Indent := Indent - 3;
      New_Line;

      case Lexer is
      when Aflex_Lexer =>
         Indent_Line ("subtype Token is Token_IDs;");
         Indent_Line ("End_Of_Input : Token_IDs renames EOF_ID;");
      when OpenToken_Lexer | Elisp_Lexer =>
         null;
      end case;

      Indent_Line ("First_Terminal : constant Token_IDs := " & To_Token_Ada_Name (Keywords.First_Element.Name) & ";");
      Indent_Line ("Last_Terminal  : constant Token_IDs := EOF_ID;");

      declare
         use Ada.Strings.Unbounded;
         Token_Image_Width : Integer := 0;
      begin
         Indent_Line ("Token_Images   : constant array (Token_IDs) of access constant String :=");
         Indent_Line ("  (");
         Indent := Indent + 3;
         Cursor := First;
         loop
            exit when Cursor.Is_Done;
            Set_Col (Indent);
            Put ("new String'(""" & (-Cursor.Token_Name));
            Token_Image_Width := Integer'Max (Token_Image_Width, Length (Cursor.Token_Name));
            Cursor.Next;
            if Cursor.Is_Done then
               Put_Line ("""));");
            else
               Put_Line ("""),");
            end if;
         end loop;

         Indent := Indent - 3;
         Indent_Line ("Token_Image_Width : constant Integer :=" & Integer'Image (Token_Image_Width) & ";");
         New_Line;
      end;

      Indent_Line ("function Token_Image (ID : in Token_IDs) return String is (Token_Images (ID).all);");

      Indent_Line
        ("package Tokens is new OpenToken.Token (Token_IDs, First_Terminal, Last_Terminal, Token_Image);");
      Indent_Line ("package Nonterminals is new Tokens.Nonterminal;");
      Indent_Line ("package Productions is new OpenToken.Production (Tokens, Nonterminals);");
      Indent_Line ("package Parsers is new Productions.Parser;");
      Indent_Line
        ("First_State_Index : constant Integer := " & OpenToken.Int_Image (First_State_Index) & ";");
      Indent_Line ("package LALRs is new Parsers.LALR (First_State_Index);");
      Indent_Line ("package Production_Lists is new Productions.List;");
      Indent_Line ("package LALR_Generators is new LALRs.Generator (Token_IDs'Width, Production_Lists);");
      Indent_Line
        ("First_Parser_Label : constant Integer := " & OpenToken.Int_Image (First_Parser_Label) & ";");
      Indent_Line ("package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label);");
      Indent_Line ("package LALR_Parsers is new LALRs.Parser (First_Parser_Label, Parser_Lists);");
      New_Line;

      Indent_Line ("package Wisi_Tokens_Pkg is new OpenToken.Wisi_Tokens");
      Indent_Line
        ("  (Token_IDs, First_Terminal, Last_Terminal, Token_Image, Tokens, Nonterminals);");
      New_Line;

      case Interface_Kind is
      when Process =>
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Max_Parallel         : in Integer                               := 15;");
         Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
         Indent_Line ("   Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;");
         Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");
         Indent_Line ("  return LALR_Parsers.Instance;");

      when Module =>
         Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
         Indent_Line ("pragma Export (C, Parse, """ & To_Lower (Package_Name_Root) & "_wisi_module_parse"");");
         Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
         Indent_Line ("pragma Export (C, Init, """ & To_Lower (Package_Name_Root) & "_wisi_module_parse_init"");");

      end case;

      New_Line;
      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);

   end Create_Ada_Spec;

   procedure Put_Module_Action_Line (Line : in String)
   is
      use Generate_Utils;
      use Ada.Strings.Fixed;

      --  Typical lines:
      --
      --  (wisi-test-result 1)

      --  (progn
      --   (wisi-statement-action [1 statement-start 6 statement-end])
      --   (wisi-containing-action 2 3))
      --   (wisi-face-action [2 font-lock-type-face] t))

      --  (wisi-motion-action [1 3 [5 statement-other ELSIF block-middle THEN] 6 8]))
      --  not [... [...] ... [...] ... ]

      --  (wisi-motion-action [1 5 [6 block-middle EXCEPTION block-middle WHEN]])

      function Item_Image (Item : in String) return String
      is
         use Ada.Characters.Handling;
      begin
         if Is_Digit (Item (Item'First)) then
            return "Elisp_Numbers (" & Item & ")";
         else
            return "Elisp_Symbols (" & Elisp_Name_To_Ada (Item) & ")";
         end if;
      end Item_Image;

      First           : Integer := Line'First;
      Last            : Integer;
      Max_Last        : Integer;
      Closing_Bracket : Integer := 0;
      First_Item      : Boolean := True;
   begin
      if Line = "(progn" then
         return;
      elsif Line (First) /= '(' or Line (Line'Last) /= ')' then
         raise Programmer_Error with "To_Module_Action_Line: unsupported line '" & Line & "'";
      end if;

      First := First + 1;

      if Line (Line'Last - 1) = ')' then
         Max_Last := Line'Last - 2; -- "...))"
      else
         Max_Last := Line'Last - 1; -- "...)"
      end if;

      if Line (Max_Last) = ']' then
         Max_Last := Max_Last - 1; -- "...])"
      end if;

      if Line (Max_Last) = ']' then
         Max_Last := Max_Last - 1; -- "...]])"
      end if;

      Indent_Line ("Funcall");
      Indent_Line ("  (Env,");
      Indent := Indent + 3;

      Last := Index (Line, " ", From => First);
      if Last = 0 then
         Indent_Line ("Elisp_Symbols (" & Elisp_Name_To_Ada (Line (First .. Max_Last)) & "));");
         Indent := Indent - 3;
         return;
      end if;

      Indent_Line ("Elisp_Symbols (" & Elisp_Name_To_Ada (Line (First .. Last - 1)) & "),");

      loop
         First := Index_Non_Blank
           (Line,
            From =>
              (if Last = Closing_Bracket
               then Closing_Bracket + 1
               else Last));

         if Line (First) = '[' then
            First := First + 1;

            if Closing_Bracket = 0 then
               Closing_Bracket := Index (Line, "]", From => First);
            end if;
         end if;

         Last := Index (Line, " ", From => First);

         if Last = 0 then
            if First_Item then
               Indent_Line ("(1 => " & Item_Image (Line (First .. Max_Last)) & "));");
               Indent := Indent - 3;
               return;
            else
               Indent_Line (Item_Image (Line (First .. Max_Last)) & "));");
               Indent := Indent - 4;
               return;
            end if;

         elsif Last = Closing_Bracket + 1 then
            Last := Closing_Bracket;
         end if;

         if First_Item then
            Indent_Line ("(" & Item_Image (Line (First .. Last - 1)) & ",");
            Indent := Indent + 1;
         else
            Indent_Line (Item_Image (Line (First .. Last - 1)) & ",");
         end if;

         First_Item := False;
      end loop;
   exception
   when Programmer_Error =>
      raise;
   when others =>
      raise Programmer_Error with "Put_Module_Action_Line: extra space '" & Line & "'";
   end Put_Module_Action_Line;

   procedure Create_Ada_Body
   is
      use Generate_Utils;

      type Action_Name_List is array (Integer range <>) of access constant String;
      type Action_Name_List_Access is access Action_Name_List;

      Empty_Action : constant access constant String := new String'("Self");
      Action_Names : array (Generate_Utils.Token_IDs) of Action_Name_List_Access;
      --  Names of subprograms for each grammar action

      function Action_Name (Item : in Generate_Utils.Token_IDs; Index : in Integer) return String
      is begin
         return Action_Names (Item) (Index).all;
      exception
      when others =>
         Wisi.Utils.Put_Error
           (Input_File_Name,
            1,
            "Name for '" & Generate_Utils.Token_Image (Item) & "'," & Integer'Image (Index) & " not defined.");
         raise Programmer_Error;
      end Action_Name;

      File_Name : constant String := Output_File_Root &
        (case Interface_Kind is
         when Process => "_process",
         when Module  => "_module") &
        ".adb";

      Package_Name : constant String := Package_Name_Root &
        (case Interface_Kind is
         when Process => "_Process",
         when Module  => "_Module");

      Body_File : File_Type;

      Table_Entry_Count : Integer := 0;
   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Syntax => True);

      if not Profile then
         case Interface_Kind is
         when Process =>
            if Action_Count > 0 then
               Indent_Line ("with Ada.Text_IO; use Ada.Text_IO;");
            end if;

         when Module =>
            Indent_Line ("with Emacs_Module_Aux; use Emacs_Module_Aux;");
         end case;
      end if;

      case Lexer is
      when Aflex_Lexer =>
         Put_Line ("with OpenToken.Token.Aflex;");
         Put_Line ("with " & To_Lower (Package_Name_Root) & "_YYLex;");
         Put_Line ("with " & To_Lower (Package_Name_Root) & "_dfa;");
         Put_Line ("with " & To_Lower (Package_Name_Root) & "_io;");

      when OpenToken_Lexer =>
         Put_Line ("with OpenToken.Token.Analyzer;");
         if Is_In (Tokens, """symbol""") then
            Put_Line ("with Ada.Strings.Maps.Constants;");
         end if;

         if Is_In (Tokens, """whitespace""") then
            Put_Line ("with OpenToken.Recognizer.Character_Set;");
         end if;

         Put_Line ("with OpenToken.Recognizer.End_Of_File;");

         if Is_In (Tokens, """symbol""") then
            Put_Line ("with OpenToken.Recognizer.Identifier;");
         end if;

         if Is_In (Tokens, """number""") then
            Put_Line ("with OpenToken.Recognizer.Based_Integer_Real_Ada;");
            --  FIXME: overkill for wisi-number-p; no based
         end if;

         Put_Line ("with OpenToken.Recognizer.Keyword;");

         if Is_In (Tokens, """line_comment""") then
            Put_Line ("with OpenToken.Recognizer.Line_Comment;");
         end if;

         if Is_In (Tokens, """punctuation""") then
            Put_Line ("with OpenToken.Recognizer.Separator;");
         end if;

         if Is_In (Tokens, """string-double""") then
            Put_Line ("with OpenToken.Recognizer.String;");
         end if;

         if Is_In (Tokens, """string-single""") then
            Put_Line ("with OpenToken.Recognizer.Graphic_Character;");
         end if;

      when Elisp_Lexer =>
         Put_Line ("with OpenToken.Token.Wisi_Elisp;");
      end case;

      case Interface_Kind is
      when Process =>
         null;
      when Module =>
         Put_Line ("with Ada.Exceptions;");
      end case;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Lexer is
      when Aflex_Lexer =>
         Indent_Line ("package Lexers is new Tokens.Aflex");
         Indent_Line ("  (" & To_Lower (Package_Name_Root) & "_io.Feeder,");
         Indent := Indent + 3;
         Indent_Line (To_Lower (Package_Name_Root) & "_YYLex,");
         Indent_Line (To_Lower (Package_Name_Root) & "_dfa.YYText,");
         Indent_Line (To_Lower (Package_Name_Root) & "_dfa.YYText_ptr,");
         Indent_Line (To_Lower (Package_Name_Root) & "_dfa.YYLength,");
         Indent_Line (To_Lower (Package_Name_Root) & "_dfa.Set_Buffer_Size,");
         Indent_Line (To_Lower (Package_Name_Root) & "_io.Tok_Begin_Line,");
         Indent_Line (To_Lower (Package_Name_Root) & "_io.Tok_Begin_Col,");
         Indent_Line (To_Lower (Package_Name_Root) & "_dfa.yy_init,");
         Indent_Line (To_Lower (Package_Name_Root) & "_io.yy_eof_has_been_seen,");
         Indent_Line ("Nonterminals,");
         Indent_Line ("Wisi_Tokens_Pkg.Get);");
         Indent := Indent - 3;

      when OpenToken_Lexer =>
         Indent_Line ("package Lexers is new Tokens.Analyzer;");

      when Elisp_Lexer =>
         Indent_Line ("package Lexers is new Tokens.Wisi_Elisp (Wisi_Tokens_Pkg.Get);");
      end case;

      Action_Names (Find_Token_ID (-OpenToken_Accept_Name))     := new Action_Name_List (0 .. 0);
      Action_Names (Find_Token_ID (-OpenToken_Accept_Name)) (0) := Empty_Action;

      case Interface_Kind is
      when Process =>
         --  Add tokens to Elisp_Names
         declare
            Cursor : Token_Cursor := First;
         begin
            loop
               exit when Cursor.Is_Done;
               Add_Elisp_Name (-Cursor.Token_Name);

               Cursor.Next;
            end loop;
         end;

      when Module =>
         Add_Elisp_Name ("set");
         Add_Elisp_Name ("wisi-tokens");
         Add_Elisp_Name ("vector");

      end case;

      if Action_Count = 0 then
         --  Populate Action_Names with Empty_Action.

         Indent_Line ("Self : constant Nonterminals.Synthesize := Wisi_Tokens.Self'Access;");

         for Rule of Rules loop
            declare
               LHS_ID : constant Token_IDs := Find_Token_ID (-Rule.Left_Hand_Side);
            begin
               Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

               for Index in Action_Names (LHS_ID)'Range loop
                  Action_Names (LHS_ID) (Index) := Empty_Action;
               end loop;
            end;
         end loop;

      else
         --  generate Action subprograms, populate Action_Names, more Elisp_Names.

         Indent_Line ("Self : constant Nonterminals.Synthesize := Wisi_Tokens_Pkg.Self'Access;");

         Indent_Line ("use Wisi_Tokens_Pkg;");
         New_Line;

         --  For Module, we need to declare Elisp_Names before the
         --  action subprograms; accumulate them here.
         for Rule of Rules loop
            for RHS of Rule.Right_Hand_Sides loop
               if RHS.Action.Length > 0 then
                  for Line of RHS.Action loop
                     Get_Elisp_Names (Line);
                  end loop;
               end if;
            end loop;
         end loop;

         case Interface_Kind is
         when Process =>
            null;

         when Module =>
            Indent_Line
              ("type Token_Array_Emacs_Value is array (Token_IDs range First_Terminal .. Token_IDs'Last) of");
            Indent_Line ("  emacs_module_h.emacs_value;");

            Indent_Line
              ("type Number_Array_Emacs_Value is array (1 .." &
                 Ada.Containers.Count_Type'Image (Elisp_Names.Length) &
                 ") of emacs_module_h.emacs_value;");
            New_Line;

            Indent_Line ("type Elisp_Index is");
            Indent_Line ("  (");
            Indent := Indent + 3;
            declare
               use String_Lists;
               Cursor : String_Lists.Cursor := Elisp_Names.First;
            begin
               loop
                  exit when Cursor = No_Element;

                  Set_Col (Indent);
                  Put (Elisp_Name_To_Ada (Element (Cursor)));

                  Next (Cursor);

                  if Cursor = No_Element then
                     Put_Line (");");
                  else
                     Put_Line (",");
                  end if;
               end loop;
            end;
            New_Line;

            Indent := Indent - 3;

            Indent_Line ("Elisp_Names : constant array (Elisp_Index) of access constant String :=");
            Indent_Line ("  (");
            Indent := Indent + 3;
            declare
               use String_Lists;
               Cursor : String_Lists.Cursor := Elisp_Names.First;
            begin
               loop
                  exit when Cursor = No_Element;
                  Set_Col (Indent);
                  Put ("new String'(""" & Element (Cursor));
                  Next (Cursor);
                  if Cursor = No_Element then
                     Put_Line ("""));");
                  else
                     Put_Line ("""),");
                  end if;
               end loop;
            end;

            Indent := Indent - 3;
            New_Line;

            Indent_Line ("type Elisp_Array_Emacs_Value is array (Elisp_Index) of emacs_module_h.emacs_value;");
            New_Line;
            Indent_Line ("Token_Symbols  : Token_Array_Emacs_Value;");
            Indent_Line ("Elisp_Numbers  : Number_Array_Emacs_Value;");
            Indent_Line ("Elisp_Symbols  : Elisp_Array_Emacs_Value;");
            Indent_Line ("Env            : Emacs_Env_Access;");
            Indent_Line ("Nil            : emacs_module_h.emacs_value;");
            New_Line;

            Indent_Line ("procedure To_Emacs");
            Indent_Line ("  (Args   : in     Tokens.List.Instance'Class;");
            Indent_Line ("   Tokens : in out Emacs_Value_Array)");
            Indent_Line ("is");
            Indent := Indent + 3;
            Indent_Line ("use " & Package_Name & ".Tokens;");
            Indent_Line ("use " & Package_Name & ".Tokens.List;");
            Indent_Line ("Args_I   : List_Iterator := Initial_Iterator (Args);");
            Indent_Line ("Tokens_I : Integer       := Tokens'First;");
            Indent := Indent - 3;
            Indent_Line ("begin");
            Indent := Indent + 3;
            Indent_Line ("loop");
            Indent := Indent + 3;
            Indent_Line ("exit when Args_I = Null_Iterator;");
            Indent_Line ("Tokens (Tokens_I) := Token_Symbols (ID (Args_I));");
            Indent_Line ("Tokens_I := Tokens_I + 1;");
            Indent_Line ("Next_Token (Args_I);");
            Indent := Indent - 3;
            Indent_Line ("end loop;");
            Indent := Indent - 3;
            Indent_Line ("end To_Emacs;");
            New_Line;

            Indent_Line ("procedure Set_Wisi_Tokens");
            Indent_Line ("  (Nonterm : in Token_IDs;");
            Indent_Line ("   Args    : in Tokens.List.Instance'Class)");
            Indent_Line ("is");
            Indent := Indent + 3;
            Indent_Line ("Tokens : Emacs_Value_Array (1 .. Args.Length + 1);");
            Indent := Indent - 3;
            Indent_Line ("begin");
            Indent := Indent + 3;
            Indent_Line ("Tokens (1) := Token_Symbols (Nonterm);");
            Indent_Line ("To_Emacs (Args, Tokens (2 .. Tokens'Last));");
            Indent_Line ("Funcall");
            Indent_Line (" (Env,");
            Indent_Line ("  Elisp_Symbols (Set_ID),");
            Indent_Line ("  (Elisp_Symbols (Wisi_Tokens_ID),");
            Indent_Line ("   Funcall (Env, Elisp_Symbols (Vector_ID), Tokens)));");
            Indent := Indent - 3;
            Indent_Line ("end Set_Wisi_Tokens;");
            New_Line;
         end case;

         if Profile then
            Indent_Line ("Action_Counts : array (Token_IDs) of Integer := (others => 0);");
         end if;

         for Rule of Rules loop
            declare
               LHS_ID : constant Token_IDs := Find_Token_ID (-Rule.Left_Hand_Side);
               Index  : Integer            := 0; -- Matches Generate_Utils.To_Grammar
            begin
               Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

               for RHS of Rule.Right_Hand_Sides loop
                  if RHS.Action.Length > 0 then
                     declare
                        Name : constant String := -Rule.Left_Hand_Side & '_' & OpenToken.Int_Image (Index);
                     begin
                        Action_Names (LHS_ID) (Index) := new String'(Name & "'Access");

                        Indent_Line ("procedure " & Name);
                        Indent_Line (" (New_Token : out Nonterminals.Class;");
                        Indent_Line ("  Source    : in  Tokens.List.Instance'Class;");
                        Indent_Line ("  To_ID     : in  Token_IDs)");
                        Indent_Line ("is begin");
                        Indent := Indent + 3;
                        Indent_Line ("New_Token := Get (To_ID, Total_Buffer_Range (Source));");

                        if Profile then
                           Indent_Line ("Action_Counts (To_ID) := Action_Counts (To_ID) + 1;");

                        else
                           case Interface_Kind is
                           when Process =>
                              --  Translate symbols into integer codes, for
                              --  faster interpretation on the elisp side.

                              Indent_Line
                                ("Put_Line (""[" & To_Code (-Rule.Left_Hand_Side) & " "" & To_Codes (Source));");

                              for Line of RHS.Action loop
                                 Indent_Line ("Put_Line (""" & To_Codes (Line) & """);");
                              end loop;
                              Indent_Line ("Put_Line (""]"");");

                           when Module =>
                              Indent_Line ("Set_Wisi_Tokens (To_ID, Source);");
                              for Line of RHS.Action loop
                                 Put_Module_Action_Line (Line);
                              end loop;
                           end case;
                        end if;
                        Indent := Indent - 3;
                        Indent_Line ("end " & Name & ";");
                        New_Line;
                     end;
                  else
                     Action_Names (LHS_ID) (Index) := Empty_Action;
                  end if;

                  Index := Index + 1;
               end loop;
            end;
         end loop;
      end if;

      --  This procedure is called for Shift actions
      Indent_Line ("procedure Add_Action");
      Indent_Line ("  (State       : in out LALRs.Parse_State;");
      Indent_Line ("   Symbol      : in     Token_IDs;");
      Indent_Line ("   State_Index : in     LALRs.State_Index)");
      Indent_Line ("is");
      Indent_Line ("   use LALRs;");
      Indent_Line ("   Action : constant Parse_Action_Rec := (Shift, State_Index);");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line
        ("State.Action_List := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), State.Action_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Action;");
      New_Line;

      --  This procedure is called for Reduce or Accept_It actions
      Indent_Line ("procedure Add_Action");
      Indent_Line ("  (State           : in out LALRs.Parse_State;");
      Indent_Line ("   Symbol          : in     Token_IDs;");
      Indent_Line ("   Verb            : in     LALRs.Parse_Action_Verbs;");
      Indent_Line ("   LHS_ID          : in     Token_IDs;");
      Indent_Line ("   RHS_Token_Count : in     Natural;");
      Indent_Line ("   Synthesize      : in     Nonterminals.Synthesize)");
      Indent_Line ("is");
      Indent_Line ("   use LALRs;");
      Indent_Line ("   use Productions;");
      Indent_Line ("   Action : Parse_Action_Rec;");
      Indent_Line
        ("   LHS    : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens_Pkg.Get (LHS_ID));");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("case Verb is");
      Indent_Line ("when Reduce =>");
      Indent_Line ("   Action := (Reduce, LHS, Synthesize, 0, RHS_Token_Count);");
      Indent_Line ("when Accept_It =>");
      Indent_Line ("   Action := (Accept_It, LHS, Synthesize, 0, RHS_Token_Count);");
      Indent_Line ("when others =>");
      Indent_Line ("   null;");
      Indent_Line ("end case;");
      Indent_Line
        ("State.Action_List := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), State.Action_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Action;");
      New_Line;

      if Shift_Reduce_Conflict_Count > 0 then
         --  This procedure is called for Shift/Reduce conflicts
         Indent_Line ("procedure Add_Action");
         Indent_Line ("  (State       : in out LALRs.Parse_State;");
         Indent_Line ("   Symbol      : in     Token_IDs;");
         Indent_Line ("   State_Index : in     LALRs.State_Index;");
         Indent_Line ("   LHS_ID      : in     Token_IDs;");
         Indent_Line ("   RHS_Token_Count : in     Natural;");
         Indent_Line ("   Synthesize  : in     Nonterminals.Synthesize)");
         Indent_Line ("is");
         Indent_Line ("   use LALRs;");
         Indent_Line ("   use Productions;");
         Indent_Line ("   Action_1 : constant Parse_Action_Rec := (Shift, State_Index);");
         Indent_Line
           ("   LHS      : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens_Pkg.Get (LHS_ID));");
         Indent_Line
           ("   Action_2 : constant Parse_Action_Rec := " &
              "(Reduce, LHS, Synthesize, 0, RHS_Token_Count);");
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("State.Action_List := new Action_Node'");
         Indent_Line
           ("  (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);");
         Indent := Indent - 3;
         Indent_Line ("end Add_Action;");
         New_Line;
      end if;

      if Reduce_Reduce_Conflict_Count > 0 then
         --  This procedure is called for Reduce/Reduce conflicts
         Indent_Line ("procedure Add_Action");
         Indent_Line ("  (State             : in out LALRs.Parse_State;");
         Indent_Line ("   Symbol            : in     Token_IDs;");
         Indent_Line ("   LHS_ID_1          : in     Token_IDs;");
         Indent_Line ("   RHS_Token_Count_1 : in     Natural;");
         Indent_Line ("   Synthesize_1      : in     Nonterminals.Synthesize;");
         Indent_Line ("   LHS_ID_2          : in     Token_IDs;");
         Indent_Line ("   RHS_Token_Count_2 : in     Natural;");
         Indent_Line ("   Synthesize_2      : in     Nonterminals.Synthesize)");
         Indent_Line ("is");
         Indent := Indent + 3;
         Indent_Line ("use LALRs;");
         Indent_Line ("use Productions;");
         Indent_Line
           ("LHS_1 : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens_Pkg.Get (LHS_ID_1));");
         Indent_Line
           ("Action_1 : constant Parse_Action_Rec := (Reduce, LHS_1, Synthesize_1, 0, RHS_Token_Count_1);");
         Indent_Line
           ("LHS_2 : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens_Pkg.Get (LHS_ID_2));");
         Indent_Line
           ("Action_2 : constant Parse_Action_Rec := (Reduce, LHS_2, Synthesize_2, 0, RHS_Token_Count_2);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("State.Action_List := new Action_Node'");
         Indent_Line
           ("  (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);");
         Indent := Indent - 3;
         Indent_Line ("end Add_Action;");
         New_Line;
      end if;

      --  This procedure is called for Error actions
      --  Error action must be last in list
      Indent_Line ("procedure Add_Action");
      Indent_Line ("  (State  : in out LALRs.Parse_State;");
      Indent_Line ("   Symbol : in     Token_IDs)");
      Indent_Line ("is");
      Indent_Line ("   use LALRs;");
      Indent_Line ("   Action : constant Parse_Action_Rec := (Verb => Error);");
      Indent_Line ("   Node   : Action_Node_Ptr           := State.Action_List;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("loop");
      Indent_Line ("   exit when Node.Next = null;");
      Indent_Line ("   Node := Node.Next;");
      Indent_Line ("end loop;");
      Indent_Line ("Node.Next := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Action;");
      New_Line;

      Indent_Line ("procedure Add_Goto");
      Indent := Indent + 2;
      Indent_Line ("(State    : in out LALRs.Parse_State;");
      Indent := Indent + 1;
      Indent_Line ("Symbol   : in     Token_IDs;");
      Indent_Line ("To_State : in     LALRs.State_Index)");
      Indent := Indent - 3;
      Indent_Line ("is");
      Indent_Line ("   use LALRs;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("State.Goto_List := new Goto_Node'(Symbol, To_State, State.Goto_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Goto;");
      New_Line;

      case Lexer is
      when Aflex_Lexer | Elisp_Lexer =>
         null;
      when OpenToken_Lexer =>
         Indent_Line ("function Create_Syntax return Analyzers.Syntax");
         Indent_Line ("is");
         Indent_Line ("   use OpenToken.Recognizer;");
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("return");
         Indent_Line ("  (");
         Indent := Indent + 3;

         --  We don't use a Token_Cursor here because the output depends on the Kind
         for Item of Keywords loop
            Indent_Line
              (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get (Keyword.Get (" & (-Item.Value) &
                 "), Wisi_Tokens_Pkg.Get (" & To_Token_Ada_Name (Item.Name) & ")),");
         end loop;
         for Kind of Tokens loop
            if -Kind.Kind = """line_comment""" then
               for Item of Kind.Tokens loop
                  Indent_Line
                    (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get (Line_Comment.Get (" &
                       (-Item.Value) & "), Wisi_Tokens_Pkg.Get (" & To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            elsif -Kind.Kind = """whitespace""" then
               --  Only one whitespace token. Ignoring value.
               if Kind.Tokens.Length > 1 then
                  raise Programmer_Error;
               end if;
               for Item of Kind.Tokens loop
                  Indent_Line (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get");
                  Indent_Line ("   (Character_Set.Get (Character_Set.Standard_Whitespace)),");
               end loop;
            elsif -Kind.Kind = """number""" then
               --  Only one number token. Ignoring value.
               if Kind.Tokens.Length > 1 then
                  raise Programmer_Error;
               end if;
               for Item of Kind.Tokens loop
                  Indent_Line
                    (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get (Based_Integer_Real_Ada.Get, " &
                       "Wisi_Tokens_Pkg.Get (" & To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            elsif -Kind.Kind = """punctuation""" then
               for Item of Kind.Tokens loop
                  Indent_Line
                    (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get (Separator.Get (" &
                       (-Item.Value) & "), Wisi_Tokens_Pkg.Get (" & To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            elsif -Kind.Kind = """symbol""" then
               for Item of Kind.Tokens loop
                  Indent_Line (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get");
                  Indent_Line ("  (Identifier.Get");
                  --  this is compatible with the Emacs Ada mode wisi elisp lexer
                  Indent_Line ("     (Start_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set,");
                  Indent_Line ("      Body_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set),");
                  Indent_Line ("   Wisi_Tokens_Pkg.Get (" & To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            elsif -Kind.Kind = """string-double""" then
               for Item of Kind.Tokens loop
                  Indent_Line
                    (To_Token_Ada_Name (Item.Name) &
                       " => Analyzers.Get (OpenToken.Recognizer.String.Get, Wisi_Tokens_Pkg.Get (" &
                       To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            elsif -Kind.Kind = """string-single""" then
               for Item of Kind.Tokens loop
                  Indent_Line
                    (To_Token_Ada_Name (Item.Name) & " => Analyzers.Get (Graphic_Character.Get, Wisi_Tokens_Pkg.Get (" &
                       To_Token_Ada_Name (Item.Name) & ")),");
               end loop;
            else
               raise OpenToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
            end if;
         end loop;
         Indent_Line ("EOF_ID => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get, Wisi_Tokens_Pkg.Get (EOF_ID)));");
         New_Line;
         Indent := Indent - 6;
         Indent_Line ("end Create_Syntax;");
         New_Line;
      end case;

      Indent_Line ("function Create_Parser");
      case Interface_Kind is
      when Process =>
         Indent_Line ("  (Max_Parallel         : in Integer                               := 15;");
         Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
         Indent_Line ("   Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;");
         Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");
      when Module =>
         Indent_Line ("  (Env                 : in Emacs_Env_Access;");
         Indent_Line ("   Lexer_Elisp_Symbols : in Lexers.Elisp_Array_Emacs_Value;");
         Indent_Line ("   Max_Parallel        : in Integer := 15)");
      end case;

      Indent_Line ("  return LALR_Parsers.Instance");
      Indent_Line ("is");
      Indent := Indent + 3;
      Indent_Line ("use LALRs;");
      Indent_Line ("use Productions;");
      Indent_Line
        ("Table : constant Parse_Table_Ptr := new Parse_Table (" &
           LALRs.State_Image (Parser'First) & " .. " & LALRs.State_Image (Parser'Last) & ");");
      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      for State_Index in Parser'Range loop
         Actions :
         declare
            use Generate_Utils.LALRs;
            Node : Action_Node_Ptr := Parser (State_Index).Action_List;
         begin
            loop
               exit when Node = null;
               Table_Entry_Count := Table_Entry_Count + 1;
               Set_Col (Indent);
               Put ("Add_Action (Table (" & State_Image (State_Index) & "), " & Token_Image (Node.Symbol));
               declare
                  Action_Node : Parse_Action_Node_Ptr := Node.Action;
               begin
                  case Action_Node.Item.Verb is
                  when Shift =>
                     Put (", " & State_Image (Action_Node.Item.State));
                  when Reduce | Accept_It =>
                     if Action_Node.Next = null then
                        if Action_Node.Item.Verb = Reduce then
                           Put (", Reduce");
                        else
                           Put (", Accept_It");
                        end if;
                     else
                        --  conflict; Verb must be reduce
                        null;
                     end if;
                     Put
                       (", " & Token_Image (Tokens_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                          Integer'Image (Action_Node.Item.Token_Count) & ", " &
                          Action_Name (Tokens_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
                  when Error =>
                     null;
                  end case;

                  Action_Node := Action_Node.Next;
                  if Action_Node /= null then
                     --  Conflict; second action is Shift or Reduce
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Put (", " & State_Image (Action_Node.Item.State));
                     when Reduce =>
                        Put
                          (", " & Token_Image (Tokens_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                             Integer'Image (Action_Node.Item.Token_Count) & ", " &
                             Action_Name (Tokens_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
                     when others =>
                        raise Programmer_Error with "second action verb: " &
                          LALRs.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                     end case;
                  end if;
               end;
               Put_Line (");");
               Node := Node.Next;
            end loop;
         end Actions;

         Gotos :
         declare
            use Generate_Utils.LALRs;
            Node : Goto_Node_Ptr := Parser (State_Index).Goto_List;
         begin
            loop
               exit when Node = null;
               Set_Col (Indent);
               Put ("Add_Goto (Table (" & State_Image (State_Index) & "), ");
               Put_Line (Token_Image (Node.Symbol) & ", " & State_Image (Node.State) & ");");
               Node := Node.Next;
            end loop;
         end Gotos;
      end loop;
      New_Line;
      --  FIXME: get Max_Parallel from some command line
      Indent_Line ("return");
      case Lexer is
      when Aflex_Lexer =>
         Indent_Line ("  (Tokens.Source_Handle (Lexers.Initialize (Text_Feeder, Buffer_Size, First_Column => 0)),");
         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State);");

      when OpenToken_Lexer =>
         Indent_Line
           ("  (" &
              "Tokens.Source_Handle (Analyzers.Initialize" &
              " (Create_Syntax, Text_Feeder, Buffer_Size, First_Column => 0)),");

         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State);");

      when Elisp_Lexer =>
         Indent_Line ("  (Tokens.Source_Handle (Lexers.Initialize (Env, Lexer_Elisp_Symbols)),");
         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;

      Indent_Line ("Parser : LALR_Parsers.Instance;");
      New_Line;

      Indent_Line ("function Parse (Env : Emacs_Env_Access) return emacs_module_h.emacs_value");
      Indent_Line ("is begin");
      --  FIXME: set OpenToken.Trace_Parse from elisp var
      Indent_Line ("   Parser.Reset;");
      Indent_Line ("   Parser.Parse;");
      Indent_Line ("   return Nil;");
      Indent_Line ("exception");
      Indent_Line ("when E : others =>");
      --  FIXME: implement emacs_module_h signal_error and use that?
      Indent_Line ("   return To_Emacs (Env, Ada.Exceptions.Exception_Message (E));");
      Indent_Line ("end Parse;");
      New_Line;

      Indent_Line ("function Init (Env : Emacs_Env_Access) return Interfaces.C.int");
      Indent_Line ("is");
      Indent_Line ("   Lexer_Elisp_Symbols : Lexers.Elisp_Array_Emacs_Value;");
      Indent_Line ("begin");
      Indent_Line ("   " & Package_Name & ".Env := Env;");
      Indent_Line ("   Intern_Soft_Symbol := Intern (Env, ""intern-soft"");");
      Indent_Line ("   Nil := Intern_Soft (Env, ""nil"");");
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
      Indent_Line ("when others =>");
      --  FIXME: implement emacs_module_h make_string and return error message
      Indent_Line ("   return 1;");
      Indent_Line ("end Init;");
      New_Line;


      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

      Put_Line
        (Integer'Image (Rule_Count) & " rules," &
           Integer'Image (Action_Count) & " actions," &
           Integer'Image (Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts," &
           LALRs.State_Index'Image (Parser'Last) & " states," &
           Integer'Image (Table_Entry_Count) & " table entries");
   end Create_Ada_Body;

   procedure Create_Aflex
   is
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & ".l");
      Set_Output (File);

      Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Syntax => True);
      New_Line;
      Put_Line ("%%");
      New_Line;

      --  We don't use a Token_Cursor here because the output depends on the Kind
      for Item of Keywords loop
         Put_Line (-Item.Value & " {         return " & To_Token_Ada_Name (Item.Name) & ";}");
      end loop;

      for Kind of Tokens loop
         if -Kind.Kind = """line_comment""" then
            for Item of Kind.Tokens loop
               declare
                  Value : constant String := -Item.Value;
               begin
                  --  drop comment
                  if Value = """--""" then
                     --  matches Aflex comment; need escape
                     Put_Line ("\-\-[^\n]*$ {         null;}");
                  else
                     --  FIXME: strip quotes
                     Put_Line (Value (1 .. Value'Last - 1) & ".*$"" {         null;}");
                  end if;
               end;
            end loop;

         elsif -Kind.Kind = """whitespace""" then
            for Item of Kind.Tokens loop
               declare
                  Value : constant String := -Item.Value;
               begin
                  --  drop whitespace
                  Put_Line (Value (Value'First + 1 .. Value'Last - 1) & " {         null;}");
               end;
            end loop;

         elsif -Kind.Kind = """number""" then
            --  Only one number token.
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               if -Item.Value = "ada-wisi-number-p" then
                  Put_Line
                    ("\([0-9]+#\)?[-+0-9a-fA-F.]+\(#\)? {         return " & To_Token_Ada_Name (Item.Name) & ";}");
               else
                  raise Programmer_Error;
               end if;
            end loop;

         elsif -Kind.Kind = """punctuation""" then
            for Item of Kind.Tokens loop
               Put_Line (-Item.Value & " {         return " & To_Token_Ada_Name (Item.Name) & ";}");
            end loop;

         elsif -Kind.Kind = """symbol""" then
            --  FIXME: need value to determine regexp; assuming Ada
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               Put_Line ("[0-9a-zA-Z_]+ {         return " & To_Token_Ada_Name (Item.Name) & ";}");
            end loop;

         elsif -Kind.Kind = """string-double""" then
            --  FIXME: need value to determine regexp; assuming Ada
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               --  FIXME: this doesn't recognize "" in string
               Put_Line ("\""[^\""]*\"" {         return " & To_Token_Ada_Name (Item.Name) & ";}");
            end loop;

         elsif -Kind.Kind = """string-single""" then
            --  FIXME: need value to determine regexp; assuming Ada
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               Put_Line ("'[^']'|'''' {         return " & To_Token_Ada_Name (Item.Name) & ";}");
            end loop;

         else
            raise OpenToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
         end if;
      end loop;

      --  aflex has built-in EOF

      Put_Line ("%%");
      Put_Line
        ("with " & File_Name_To_Ada (Output_File_Root) & "; use " & File_Name_To_Ada (Output_File_Root) & ";");
      Put_Line ("##");

      Close (File);
   end Create_Aflex;

   procedure Create_Process_Elisp
   is
      use Generate_Utils;

      File_Name : constant String := Output_File_Root & "-process.el";
      File      : File_Type;
   begin
      Create (File, Out_File, File_Name);
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");
      --  don't need the prologue here

      New_Line;
      Indent_Line ("(defconst " & Output_File_Root & "-process-names");
      Indent_Line ("  [");
      Indent := Indent + 3;
      for Name of Elisp_Names loop
         Indent_Line (Name);
      end loop;
      Indent_Line ("])");
      Indent := Indent - 3;
      New_Line;

      Put_Line ("(provide '" & File_Name & ")");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Process_Elisp;

   procedure Create_Module_Elisp
   is
      use Generate_Utils;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & "-module.el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");

      --  don't need the prologue here

      --  FIXME: this duplicates wisi-output_elisp Keyword_Table, Token_Table; factor out and share
      --  or just add all elisp here?
      Put_Line ("(require 'semantic/lex)");
      Put_Line ("(require 'wisi-parse-common)");

      New_Line;
      Indent_Line ("(defconst " & Output_File_Root & "-module-keyword-table");
      Indent_Line ("  (semantic-lex-make-keyword-table");
      Indent_Line ("   '(");
      Indent := Indent + 5;
      for Pair of Keywords loop
         Indent_Line ("(" & (-Pair.Value) & " . " & (-Pair.Name) & ")");
      end loop;
      Indent_Line (")");
      Indent := Indent - 2;
      Indent_Line ("nil))");
      Indent := Indent - 3;
      New_Line;

      Indent_Line ("(defconst " & Output_File_Root & "-module-token-table");
      Indent_Line ("  (semantic-lex-make-type-table");
      Indent_Line ("   '(");
      Indent := Indent + 5;
      for Kind of Tokens loop
         if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
            Indent_Line ("(" & (-Kind.Kind));
            Indent := Indent + 1;
            for Token of Kind.Tokens loop
               declare
                  use Ada.Strings.Unbounded;
                  Img : constant String := Integer'Image
                    (Token_IDs'Pos (Find_Token_ID (-Token.Name)));
               begin
                  if 0 = Length (Token.Value) then
                     Indent_Line ("(" & Img & ")");
                  else
                     if -Kind.Kind = """number""" then
                        --  allow for (<token> <number-p> <require>)
                        Indent_Line ("(" & Img & " " & (-Token.Value) & ")");
                     else
                        Indent_Line ("(" & Img & " . " & (-Token.Value) & ")");
                     end if;
                  end if;
               end;
            end loop;
            Indent_Line (")");
            Indent := Indent - 1;
         end if;
      end loop;
      Indent_Line (")");
      Indent := Indent - 2;
      Indent_Line ("nil))");
      Indent := 1;

      Indent_Line
        ("(cl-defstruct (" &
           To_Lower (Package_Name_Root) &
           "-wisi-module-parser (:include wisi-parser)) dll-name)");
      New_Line;

      Indent_Line
      ("(declare-function " &
           To_Lower (Package_Name_Root) &
           "-wisi-module-parse """ &
           To_Lower (Package_Name_Root) &
           "-wisi-module-parse.c"" (buffer))");
      New_Line;

      Indent_Line
        ("(cl-defmethod wisi-parse-current ((parser " &
           To_Lower (Package_Name_Root) &
           "-wisi-module-parser))");
      Indent := Indent + 2;
      Indent_Line ("(unless (functionp '" & To_Lower (Package_Name_Root) & "-wisi-module-parse)");
      Indent_Line ("  (module-load (" & To_Lower (Package_Name_Root) & "-wisi-module-parser-dll-name parser)))");
      New_Line;

      Indent_Line ("(let ((result (" & To_Lower (Package_Name_Root) & "-wisi-module-parse (current-buffer))))");
      --  result is nil for no errors, a string for some error
      Indent_Line ("  (when result");
      Indent_Line ("    (signal 'wisi-parse-error (wisi-error-msg result)))))");
      New_Line;
      Indent := Indent - 2;

      Indent_Line ("(provide '" & Output_File_Root & "-module)");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Module_Elisp;

   procedure Create_Module_Aux
   is
      use Generate_Utils;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & "_wisi_module_parse.gpr");
      Set_Output (File);
      Indent := 1;
      Put_Line ("-- generated by OpenToken Wisi from " & Input_File_Name);
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
      Indent_Line ("""opentoken-token-wisi_elisp.adb"",");
      Indent_Line ("""opentoken-token-wisi_elisp.ads"",");
      Indent_Line ("""" & To_Lower (Package_Name_Root) & "_module.adb"",");
      Indent_Line ("""" & To_Lower (Package_Name_Root) & "_module.ads""");
      Indent := Indent - 3;
      Indent_Line ("  );");
      New_Line;
      Indent_Line ("for Object_Dir use ""libobjsjlj"";");
      Indent_Line ("for Library_Name use """ & To_Lower (Package_Name_Root) & "_wisi_module_parse"";");
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
      --
      --  gcc gives up without -fno-var-tracking-assignments; it is
      --  only useful when debugging, which is typically not needed in
      --  this file.
      Indent_Line ("case Wisi_Module_Parse_Common.Build is");
      Indent_Line ("when ""Debug"" =>");
      Indent_Line ("   for Switches (""" & To_Lower (Package_Name_Root) & "_module.adb"") use");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("     (""-O0"", ""-fno-var-tracking-assignments"");");
      Indent_Line ("when ""Normal"" =>");
      Indent_Line ("for Switches (""" & To_Lower (Package_Name_Root) & "_module.adb"") use");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("  (""-O2"", ""-fno-var-tracking-assignments"");");
      Indent_Line ("end case;");

      --  Other optimization levels hang here.
      Indent_Line ("for Switches (""" & To_Lower (Package_Name_Root) & "_module.ads"") use");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("  (""-O0""); ");

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

      Create (File, Out_File, Output_File_Root & "_wisi_module_parse_agg.gpr");
      Set_Output (File);
      Indent := 1;
      Put_Line ("-- generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line ("-- ");
      Indent_Line ("aggregate project " & Package_Name_Root & "_Wisi_Module_Parse_Agg is");
      Indent_Line ("   for Project_Path use (external (""WISI_OPENTOKEN""));");
      Indent_Line ("   for Project_files use (""" & To_Lower (Package_Name_Root) & "_wisi_module_parse.gpr"");");
      Indent_Line ("end " & Package_Name_Root & "_Wisi_Module_Parse_Agg;");
      Set_Output (Standard_Output);
      Close (File);

      Create (File, Out_File, Output_File_Root & "_wisi_module_parse_wrapper.c");
      Set_Output (File);
      Indent := 1;
      Put_Line ("// generated by OpenToken Wisi from " & Input_File_Name);
      Put_Command_Line ("// ");
      Indent_Line ("//  This file is just a wrapper around the Ada code in");
      Indent_Line ("//  *_wisi_module_parse.adb; it is needed to call adainit.");
      Indent_Line ("#include <emacs_module.h>");
      Indent_Line ("int plugin_is_GPL_compatible;");
      Indent_Line ("static emacs_value nil;");
      Indent_Line ("extern void adainit(void);");
      Indent_Line ("extern int " & To_Lower (Package_Name_Root) & "_wisi_module_parse_init (emacs_env *env);");
      Indent_Line ("/* Parse current buffer, using parser in current module. */");
      Indent_Line ("extern emacs_value " & To_Lower (Package_Name_Root) & "_wisi_module_parse (emacs_env *env);");
      Indent_Line ("static emacs_value Fparse (emacs_env *env, int nargs, emacs_value args[])");
      Indent_Line ("{");
      Indent_Line ("  return " & To_Lower (Package_Name_Root) & "_wisi_module_parse (env);");
      Indent_Line ("}");
      New_Line;
      Indent_Line ("static void bind_function (emacs_env *env, const char *name, emacs_value Ffun)");
      Indent_Line ("{");
      Indent_Line ("  emacs_value Qfset  = env->intern (env, ""fset"");");
      Indent_Line ("  emacs_value Qsym   = env->intern (env, name);");
      Indent_Line ("  emacs_value args[] = { Qsym, Ffun };");
      Indent_Line ("  env->funcall (env, Qfset, 2, args);");
      Indent_Line ("}");
      New_Line;
      Indent_Line ("int emacs_module_init (struct emacs_runtime *ert)");
      Indent_Line ("{");
      Indent_Line ("  emacs_env *env = ert->get_environment (ert);");
      Indent_Line ("  nil = env->intern (env, ""nil"");");
      Indent_Line
        ("  bind_function (env, """ & To_Lower (Package_Name_Root) &
           "-wisi-module-parse"", env->make_function (env, 1, 1, Fparse));");
      Indent_Line ("  adainit();");
      Indent_Line ("  return " & To_Lower (Package_Name_Root) & "_wisi_module_parse_init (env);");
      Indent_Line ("}");
      Set_Output (Standard_Output);
      Close (File);
   end Create_Module_Aux;

begin
   Create_Ada_Body; -- populates Elisp_Names, used by Create_Ada_Spec
   Create_Ada_Spec;

   case Lexer is
   when Aflex_Lexer =>
      Create_Aflex;
   when OpenToken_Lexer =>
      null;
   when Elisp_Lexer =>
      --  All of the lexers need an elisp file; the form of the elisp
      --  file is determined by Interface_Kind (see below).
      null;
   end case;

   if not Profile then
      case Interface_Kind is
      when Process =>
         Create_Process_Elisp;

      when Module =>
         Create_Module_Elisp;
         Create_Module_Aux;
      end case;
   end if;
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada_Emacs;
