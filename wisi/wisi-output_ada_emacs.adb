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

with Ada.Text_IO; use Ada.Text_IO;
with FastToken;
with Wisi.Gen_Generate_Utils;
with Wisi.Put_Module_Action_Line;
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
   FastToken_Accept_Name : constant Ada.Strings.Unbounded.Unbounded_String := +"OPENTOKEN_ACCEPT";

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
     (Keywords, Tokens, Conflicts, Rules, EOI_Name, FastToken_Accept_Name, First_State_Index, To_Token_Ada_Name);

   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production.List.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LALR.Parse_Table_Ptr := Generate_Utils.LALR_Generator.Generate
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

   Package_Name_Root       : constant String := File_Name_To_Ada (Output_File_Root);
   Lower_Package_Name_Root : constant String := To_Lower (Output_File_Root);

   Elisp_Names : String_Lists.List;
   --  Populated by Create_Ada_Body, used by Create_Process_Elisp, Create_Module_Elisp,
   --  Create_Ada_Body for Module

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
      --  Enter all names in Elisp_Names.
      --
      --  IMPROVEME: for modules, don't need terminal token names in
      --  Elisp_Names; they are already in the keyword and token
      --  tables.
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
      return FastToken.Int_Image (Find_Elisp_Name (Name));
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
      use Wisi.Utils;

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

      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Syntax => True);

      case Interface_Kind is
      when Process =>
         Put_Line ("with FastToken.Text_Feeder;");
      when Module =>
         Put_Line ("with Emacs_Module_Aux;");
         Put_Line ("with emacs_module_h;");
         Put_Line ("with Interfaces.C;");
      end case;
      Put_Line ("with FastToken.Lexer;");
      Put_Line ("with FastToken.Production;");
      Put_Line ("with FastToken.Parser.LALR.Generator;");
      Put_Line ("with FastToken.Parser.LALR.Parser;");
      Put_Line ("with FastToken.Parser.LALR.Parser_Lists;");
      Put_Line ("with FastToken.Token.Nonterminal;");
      Put_Line ("with FastToken.Wisi_Tokens;");
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;

      New_Line;
      Indent_Line ("type Token_ID is");
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
         Indent_Line ("subtype Token is Token_ID;");
         Indent_Line ("End_Of_Input : Token_ID renames EOF_ID;");
      when Elisp_Lexer =>
         null;
      end case;

      Indent_Line ("First_Terminal : constant Token_ID := " & To_Token_Ada_Name (Keywords.First_Element.Name) & ";");
      Indent_Line ("Last_Terminal  : constant Token_ID := EOF_ID;");

      declare
         use Ada.Strings.Unbounded;
         Token_Image_Width : Integer := 0;
      begin
         Indent_Line ("Token_Images   : constant array (Token_ID) of access constant String :=");
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

      Indent_Line ("function Token_Image (ID : in Token_ID) return String is (Token_Images (ID).all);");
      Indent_Line ("procedure Put_Trace (Item : in String);");
      Indent_Line ("procedure Put_Trace_Line (Item : in String);");
      New_Line;
      Indent_Line ("package Token_Pkg is new FastToken.Token");
      Indent_Line ("  (Token_ID, First_Terminal, Last_Terminal, Token_Image, Put_Trace);");
      Indent_Line ("package Nonterminal is new Token_Pkg.Nonterminal;");
      Indent_Line ("package Production is new FastToken.Production (Token_Pkg, Nonterminal);");
      Indent_Line ("package Lexer_Root is new FastToken.Lexer (Token_Pkg);");
      Indent_Line ("package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);");
      Indent_Line
        ("First_State_Index : constant Integer := " & FastToken.Int_Image (First_State_Index) & ";");
      Indent_Line ("package LALR is new Parser_Root.LALR (First_State_Index, Nonterminal);");
      Indent_Line ("package LALR_Generator is new LALR.Generator (Token_ID'Width, Production);");
      Indent_Line
        ("First_Parser_Label : constant Integer := " & FastToken.Int_Image (First_Parser_Label) & ";");
      Indent_Line ("package Parser_Lists is new LALR.Parser_Lists (First_Parser_Label, Put_Trace, Put_Trace_Line);");
      Indent_Line
        ("package LALR_Parser is new LALR.Parser (First_Parser_Label, Put_Trace, Put_Trace_Line, Parser_Lists);");
      New_Line;

      Indent_Line ("package Wisi_Tokens_Pkg is new FastToken.Wisi_Tokens");
      Indent_Line
        ("  (Token_ID, First_Terminal, Last_Terminal, Token_Image, Put_Trace, Token_Pkg, Nonterminal);");
      New_Line;

      case Interface_Kind is
      when Process =>
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Max_Parallel         : in Integer                               := 15;");
         Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
         Indent_Line ("   Text_Feeder          : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;");
         Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");
         Indent_Line ("  return LALR_Parser.Instance;");
         New_Line;
         Indent_Line ("Wisi_Cache_Max : Integer := 0;");

      when Module =>
         Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
         Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name_Root & "_wisi_module_parse"");");
         Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
         Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name_Root & "_wisi_module_parse_init"");");

      end case;

      New_Line;
      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);

   end Create_Ada_Spec;

   procedure Create_Ada_Body
   is
      use Generate_Utils;
      use Wisi.Utils;

      type Action_Name_List is array (Integer range <>) of access constant String;
      type Action_Name_List_Access is access Action_Name_List;

      Empty_Action : constant access constant String := new String'("Self");
      Action_Names : array (Generate_Utils.Token_ID) of Action_Name_List_Access;
      --  Names of subprograms for each grammar action

      function Action_Name (Item : in Generate_Utils.Token_ID; Index : in Integer) return String
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
      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Syntax => True);

      if not Profile then
         case Interface_Kind is
         when Process =>
            Indent_Line ("with Ada.Text_IO; use Ada.Text_IO;");

         when Module =>
            Indent_Line ("with Emacs_Module_Aux; use Emacs_Module_Aux;");
         end case;
      end if;

      case Lexer is
      when Aflex_Lexer =>
         Put_Line ("with FastToken.Lexer.Aflex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_YYLex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_dfa;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_io;");

      when Elisp_Lexer =>
         Put_Line ("with FastToken.Token.Wisi_Elisp;");
      end case;

      case Interface_Kind is
      when Process =>
         null;
      when Module =>
         Put_Line ("with Ada.Exceptions;");
         Put_Line ("with Ada.Strings.Unbounded;");
      end case;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Lexer is
      when Aflex_Lexer =>
         Indent_Line ("package Lexer is new Lexer_Root.Aflex");
         Indent_Line ("  (" & Lower_Package_Name_Root & "_process_io.Feeder,");
         Indent := Indent + 3;
         Indent_Line (Lower_Package_Name_Root & "_process_YYLex,");
         Indent_Line (Lower_Package_Name_Root & "_process_dfa.YYText,");
         Indent_Line (Lower_Package_Name_Root & "_process_dfa.YYText_ptr,");
         Indent_Line (Lower_Package_Name_Root & "_process_dfa.YYLength,");
         Indent_Line (Lower_Package_Name_Root & "_process_dfa.Set_Buffer_Size,");
         Indent_Line (Lower_Package_Name_Root & "_process_io.Tok_Begin_Line,");
         Indent_Line (Lower_Package_Name_Root & "_process_io.Tok_Begin_Col,");
         Indent_Line (Lower_Package_Name_Root & "_process_dfa.yy_init,");
         Indent_Line (Lower_Package_Name_Root & "_process_io.yy_eof_has_been_seen,");
         Indent_Line ("Wisi_Tokens_Pkg.Get);");
         Indent := Indent - 3;

      when Elisp_Lexer =>
         Indent_Line ("package Lexers is new Lexer_Root.Wisi_Elisp (Wisi_Tokens_Pkg.Get);");
      end case;

      Action_Names (Find_Token_ID (-FastToken_Accept_Name))     := new Action_Name_List (0 .. 0);
      Action_Names (Find_Token_ID (-FastToken_Accept_Name)) (0) := Empty_Action;

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

         Indent_Line ("procedure Put_Trace (Item : in String)");
         Indent_Line ("is begin");
         --  FIXME: this matches existing tests, but does not work
         --  with real Emacs; need message in protocol.
         Indent_Line ("   Put (Item);");
         Indent_Line ("end Put_Trace;");
         New_Line;
         Indent_Line ("procedure Put_Trace_Line (Item : in String)");
         Indent_Line ("is begin");
         Indent_Line ("   Put_Line (Item);");
         Indent_Line ("end Put_Trace_Line;");
         New_Line;

      when Module =>
         Add_Elisp_Name ("wisi-debug");
         Add_Elisp_Name ("wisi-nonterm");
         Add_Elisp_Name ("wisi-tokens");
         Add_Elisp_Name ("wisi-cache-max");

      end case;

      Indent_Line ("Self : constant Nonterminal.Synthesize := Wisi_Tokens_Pkg.Self'Access;");

      if Action_Count = 0 then
         --  Populate Action_Names with Empty_Action.

         for Rule of Rules loop
            declare
               LHS_ID : constant Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);
            begin
               Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

               for Index in Action_Names (LHS_ID)'Range loop
                  Action_Names (LHS_ID) (Index) := Empty_Action;
               end loop;
            end;
         end loop;

      else
         --  generate Action subprograms, populate Action_Names, more Elisp_Names.

         Indent_Line ("use Wisi_Tokens_Pkg;");
         New_Line;

         --  For Module, we need to declare Elisp_Names before the
         --  action subprograms; accumulate them here. Elisp_Names has
         --  all the non-token names used in actions; the elisp
         --  functions and their arguments.
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
            --  All symbols used in wisi-tokens; terminals and
            --  non-terminals. Must be indexed by Token_ID for
            --  To_Emacs (token_list).
            Indent_Line
              ("type Token_Array_Emacs_Value is array (Token_ID range First_Terminal .. Token_ID'Last) of");
            Indent_Line ("  emacs_module_h.emacs_value;");

            Indent_Line
              ("type Number_Array_Emacs_Value is array (1 .." &
                 Ada.Containers.Count_Type'Image (Elisp_Names.Length) &
                 ") of emacs_module_h.emacs_value;");
            New_Line;

            --  All other symbols used in actions.
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
            New_Line;

            Indent_Line ("Trace_Buffer : Ada.Strings.Unbounded.Unbounded_String;");
            New_Line;
            Indent_Line ("procedure Put_Trace (Item : in String)");
            Indent_Line ("is");
            Indent_Line ("   use Ada.Strings.Unbounded;");
            Indent_Line ("begin");
            Indent_Line ("   Trace_Buffer := Trace_Buffer & Item;");
            Indent_Line ("end Put_Trace;");
            New_Line;
            Indent_Line ("procedure Put_Trace_Line (Item : in String)");
            Indent_Line ("is");
            Indent_Line ("   use Ada.Strings.Unbounded;");
            Indent_Line ("begin");
            Indent_Line ("   Message (Env, To_String (Trace_Buffer & Item));");
            Indent_Line ("   Set_Unbounded_String (Trace_Buffer, """");");
            Indent_Line ("end Put_Trace_Line;");
            New_Line;

            Indent_Line ("function To_Token_List (Token : in Token_Pkg.Handle) return emacs_module_h.emacs_value");
            Indent_Line ("is");
            Indent_Line ("   use Tokens;");
            Indent_Line ("   Wisi_Token : Wisi_Tokens_Pkg.Instance renames Wisi_Tokens_Pkg.Instance (Token.all);");
            Indent_Line ("   Bounds     : Buffer_Range renames Wisi_Token.Buffer_Range;");
            Indent_Line ("begin");
            Indent_Line ("   if Bounds = Null_Buffer_Range then");
            Indent_Line ("      return Cons (Env, Token_Symbols (ID (Wisi_Token)), Env.Qnil);");
            Indent_Line ("   else");
            Indent_Line ("      return Cons");
            Indent_Line ("        (Env, Token_Symbols (ID (Wisi_Token)),");
            Indent_Line ("         Cons (Env, To_Emacs (Env, Bounds.Begin_Pos), To_Emacs (Env, Bounds.End_Pos)));");
            Indent_Line ("   end if;");
            Indent_Line ("end To_Token_List;");
            New_Line;

            Indent_Line ("procedure Set_Wisi_Tokens");
            Indent_Line ("  (Nonterm : in Token_ID;");
            Indent_Line ("   Args    : in Token_Pkg.List.Instance'Class)");
            Indent_Line ("is");
            Indent := Indent + 3;
            Indent_Line ("use Tokens;");
            Indent_Line ("use Token_Pkg.List;");
            Indent_Line ("Tokens_1 : Emacs_Value_Array (1 .. Args.Length);");
            Indent_Line ("Tokens_I : Integer       := Tokens_1'First;");
            Indent_Line ("Args_I   : List_Iterator := Initial_Iterator (Args);");
            Indent := Indent - 3;
            Indent_Line ("begin");
            Indent := Indent + 3;
            Indent_Line ("Set (Env, Elisp_Symbols (Wisi_Nonterm_ID), Token_Symbols (Nonterm));");
            Indent_Line ("loop");
            Indent := Indent + 3;
            Indent_Line ("exit when Args_I = Null_Iterator;");
            Indent_Line ("Tokens_1 (Tokens_I) := To_Token_List (Token_Pkg.List.Token_Handle (Args_I));");
            Indent_Line ("Tokens_I := Tokens_I + 1;");
            Indent_Line ("Next_Token (Args_I);");
            Indent := Indent - 3;
            Indent_Line ("end loop;");
            Indent_Line ("Set");
            Indent_Line (" (Env,");
            Indent_Line ("  Elisp_Symbols (Wisi_Tokens_ID),");
            Indent_Line ("  Vector (Env, Tokens_1));");
            Indent := Indent - 3;
            Indent_Line ("end Set_Wisi_Tokens;");
            New_Line;
            Indent_Line ("Wisi_Cache_Max : Integer;");
            New_Line;
         end case;

         if Profile then
            Indent_Line ("Action_Counts : array (Token_ID) of Integer := (others => 0);");
         end if;

         for Rule of Rules loop
            declare
               LHS_ID : constant Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);
               Index  : Integer            := 0; -- Matches Generate_Utils.To_Grammar
            begin
               Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

               for RHS of Rule.Right_Hand_Sides loop
                  if RHS.Action.Length > 0 then
                     declare
                        Name : constant String := -Rule.Left_Hand_Side & '_' & FastToken.Int_Image (Index);
                     begin
                        Action_Names (LHS_ID) (Index) := new String'(Name & "'Access");

                        Indent_Line ("procedure " & Name);
                        Indent_Line (" (New_Token : out Nonterminal.Class;");
                        Indent_Line ("  Source    : in  Token_Pkg.List.Instance'Class;");
                        Indent_Line ("  To_ID     : in  Token_ID)");
                        Indent_Line ("is");
                        Indent_Line ("   Bounds : constant Token_Pkg.Buffer_Range := Total_Buffer_Range (Source);");
                        Indent_Line ("begin");
                        Indent := Indent + 3;
                        Indent_Line ("New_Token := Get (To_ID, Bounds);");

                        if Profile then
                           Indent_Line ("Action_Counts (To_ID) := Action_Counts (To_ID) + 1;");

                        else
                           --  We don't execute actions if all tokens
                           --  are before wisi-cache-max, because
                           --  later actions can update existing
                           --  caches, and if the parse fails that
                           --  won't happen. It also saves time.
                           --
                           --  Also skip if no tokens; nothing to do.
                           --  This can happen when all tokens in a
                           --  grammar statement are optional.
                           Indent_Line ("if Bounds.End_Pos > Wisi_Cache_Max and Source.Length > 0 then");
                           Indent := Indent + 3;
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
                                 Wisi.Put_Module_Action_Line (Line);
                              end loop;
                           end case;
                           Indent := Indent - 3;
                           Indent_Line ("end if;");
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
      Indent_Line ("  (State       : in out LALR.Parse_State;");
      Indent_Line ("   Symbol      : in     Token_ID;");
      Indent_Line ("   State_Index : in     LALR.State_Index)");
      Indent_Line ("is");
      Indent_Line ("   use LALR;");
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
      Indent_Line ("  (State           : in out LALR.Parse_State;");
      Indent_Line ("   Symbol          : in     Token_ID;");
      Indent_Line ("   Verb            : in     LALR.Parse_Action_Verbs;");
      Indent_Line ("   LHS_ID          : in     Token_ID;");
      Indent_Line ("   RHS_Token_Count : in     Natural;");
      Indent_Line ("   Synthesize      : in     Nonterminal.Synthesize)");
      Indent_Line ("is");
      Indent_Line ("   use LALR;");
      Indent_Line ("   use Production;");
      Indent_Line ("   Action : Parse_Action_Rec;");
      Indent_Line
        ("   LHS    : constant Nonterminal.Handle := new Nonterminal.Class'(Wisi_Tokens_Pkg.Get (LHS_ID));");
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
         Indent_Line ("  (State       : in out LALR.Parse_State;");
         Indent_Line ("   Symbol      : in     Token_ID;");
         Indent_Line ("   State_Index : in     LALR.State_Index;");
         Indent_Line ("   LHS_ID      : in     Token_ID;");
         Indent_Line ("   RHS_Token_Count : in     Natural;");
         Indent_Line ("   Synthesize  : in     Nonterminal.Synthesize)");
         Indent_Line ("is");
         Indent_Line ("   use LALR;");
         Indent_Line ("   use Production;");
         Indent_Line ("   Action_1 : constant Parse_Action_Rec := (Shift, State_Index);");
         Indent_Line
           ("   LHS      : constant Nonterminal.Handle := new Nonterminal.Class'(Wisi_Tokens_Pkg.Get (LHS_ID));");
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
         Indent_Line ("  (State             : in out LALR.Parse_State;");
         Indent_Line ("   Symbol            : in     Token_ID;");
         Indent_Line ("   LHS_ID_1          : in     Token_ID;");
         Indent_Line ("   RHS_Token_Count_1 : in     Natural;");
         Indent_Line ("   Synthesize_1      : in     Nonterminal.Synthesize;");
         Indent_Line ("   LHS_ID_2          : in     Token_ID;");
         Indent_Line ("   RHS_Token_Count_2 : in     Natural;");
         Indent_Line ("   Synthesize_2      : in     Nonterminal.Synthesize)");
         Indent_Line ("is");
         Indent := Indent + 3;
         Indent_Line ("use LALR;");
         Indent_Line ("use Production;");
         Indent_Line
           ("LHS_1 : constant Nonterminal.Handle := new Nonterminal.Class'(Wisi_Tokens_Pkg.Get (LHS_ID_1));");
         Indent_Line
           ("Action_1 : constant Parse_Action_Rec := (Reduce, LHS_1, Synthesize_1, 0, RHS_Token_Count_1);");
         Indent_Line
           ("LHS_2 : constant Nonterminal.Handle := new Nonterminal.Class'(Wisi_Tokens_Pkg.Get (LHS_ID_2));");
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
      Indent_Line ("  (State  : in out LALR.Parse_State;");
      Indent_Line ("   Symbol : in     Token_ID)");
      Indent_Line ("is");
      Indent_Line ("   use LALR;");
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
      Indent_Line ("(State    : in out LALR.Parse_State;");
      Indent := Indent + 1;
      Indent_Line ("Symbol   : in     Token_ID;");
      Indent_Line ("To_State : in     LALR.State_Index)");
      Indent := Indent - 3;
      Indent_Line ("is");
      Indent_Line ("   use LALR;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("State.Goto_List := new Goto_Node'(Symbol, To_State, State.Goto_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Goto;");
      New_Line;

      Indent_Line ("function Create_Parser");
      case Interface_Kind is
      when Process =>
         Indent_Line ("  (Max_Parallel         : in Integer                               := 15;");
         Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
         Indent_Line ("   Text_Feeder          : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;");
         Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");
      when Module =>
         Indent_Line ("  (Env                 : in Emacs_Env_Access;");
         Indent_Line ("   Lexer_Elisp_Symbols : in Lexers.Elisp_Array_Emacs_Value;");
         Indent_Line ("   Max_Parallel        : in Integer := 15)");
      end case;

      Indent_Line ("  return LALR_Parser.Instance");
      Indent_Line ("is");
      Indent := Indent + 3;
      Indent_Line ("use LALR;");
      Indent_Line ("use Production;");
      Indent_Line
        ("Table : constant Parse_Table_Ptr := new Parse_Table (" &
           LALR.State_Image (Parser'First) & " .. " & LALR.State_Image (Parser'Last) & ");");
      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      for State_Index in Parser'Range loop
         Actions :
         declare
            use Generate_Utils.LALR;
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
                       (", " & Token_Image (Token_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                          Integer'Image (Action_Node.Item.Token_Count) & ", " &
                          Action_Name (Token_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
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
                          (", " & Token_Image (Token_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                             Integer'Image (Action_Node.Item.Token_Count) & ", " &
                             Action_Name (Token_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
                     when others =>
                        raise Programmer_Error with "second action verb: " &
                          LALR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                     end case;
                  end if;
               end;
               Put_Line (");");
               Node := Node.Next;
            end loop;
         end Actions;

         Gotos :
         declare
            use Generate_Utils.LALR;
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
         Indent_Line ("  (Lexer_Root.Handle (Lexer.Initialize (Text_Feeder, Buffer_Size, First_Column => 0)),");
         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State);");

      when Elisp_Lexer =>
         Indent_Line ("  (Lexer_Root.Handle (Lexers.Initialize (Env, Lexer_Elisp_Symbols)),");
         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;

      case Interface_Kind is
      when Process =>
         null;
      when Module =>
         Indent_Line ("Parser : LALR_Parser.Instance;");
         New_Line;

         Indent_Line ("function Parse (Env : Emacs_Env_Access) return emacs_module_h.emacs_value");
         Indent_Line ("is begin");
         Indent := Indent + 3;
         Indent_Line ("FastToken.Trace_Parse := To_Integer (Env, Symbol_Value (Env, Elisp_Symbols (Wisi_Debug_ID)));");
         Indent_Line ("Wisi_Cache_Max := To_Integer (Env, Symbol_Value (Env, Elisp_Symbols (Wisi_Cache_Max_ID)));");
         Indent_Line ("Parser.Reset;");
         Indent_Line ("Parser.Parse;");
         Indent_Line ("return Env.Qnil;");
         Indent := Indent - 3;
         Indent_Line ("exception");
         Indent_Line ("when E : FastToken.Parse_Error | FastToken.Syntax_Error =>");
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

      Put_Line
        (Integer'Image (Rule_Count) & " rules," &
           Integer'Image (Action_Count) & " actions," &
           Integer'Image (Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts," &
           LALR.State_Index'Image (Parser'Last) & " states," &
           Integer'Image (Table_Entry_Count) & " table entries");
   end Create_Ada_Body;

   procedure Create_Aflex
   is
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & "_process.l");
      Set_Output (File);

      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
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
            raise FastToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
         end if;
      end loop;

      --  aflex has built-in EOF

      Put_Line ("%%");
      Put_Line
        ("with " & File_Name_To_Ada (Output_File_Root) & "_Process; use " &
           File_Name_To_Ada (Output_File_Root) & "_Process;");
      Put_Line ("##");

      Close (File);
   end Create_Aflex;

   procedure Create_Process_Elisp
   is
      use Wisi.Utils;
      use Generate_Utils;

      File_Name_Root : constant String := Output_File_Root & "-process";
      File      : File_Type;
   begin
      Create (File, Out_File, File_Name_Root & ".el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");
      Put_Prologue (Ada_Syntax => False);
      New_Line;

      Indent_Names_Elisp (Output_File_Root, "process", Elisp_Names);

      Put_Line ("(provide '" & File_Name_Root & ")");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Process_Elisp;

   procedure Create_Module_Elisp
   is
      use Generate_Utils;
      use Wisi.Utils;

      function To_ID_Image (Name : in Ada.Strings.Unbounded.Unbounded_String) return String
      is begin
         --  Ada 'Val is 0 origin; Generate_Utils Token_ID is 1 origin
         return Integer'Image (-1 + Find_Token_ID (-Name));
      end To_ID_Image;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & "-module.el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");

      --  don't need the prologue here

      --  FIXME: this partly duplicates wisi-output_elisp
      --  Keyword_Table, Token_Table; factor out and share. or just
      --  add all elisp here?
      Put_Line ("(require 'semantic/lex)");
      Put_Line ("(require 'wisi-parse-common)");
      New_Line;

      --  Lexer tables; also contain terminals for wisi-tokens
      Indent_Keyword_Table_Elisp
        (Output_File_Root, "elisp", Keywords, EOI_Name, Ada.Strings.Unbounded.To_String'Access);
      Indent_Keyword_Table_Elisp (Output_File_Root, "module", Keywords, EOI_Name, To_ID_Image'Access);
      Indent_Token_Table_Elisp (Output_File_Root, "elisp", Tokens, Ada.Strings.Unbounded.To_String'Access);
      Indent_Token_Table_Elisp (Output_File_Root, "module", Tokens, To_ID_Image'Access);

      --  non-terminals. We only need the ones that actually have
      --  actions, and thus will appear in a call to To_Emacs. But
      --  Token_Symbols must be indexed by Token_ID, so we declare
      --  all of them.
      Indent_Line ("(defconst " & Output_File_Root & "-module-nonterms");
      Indent_Line (" '(");
      Indent := Indent + 3;
      Indent_Line (-FastToken_Accept_Name);
      for Rule of Rules loop
         Indent_Line (-Rule.Left_Hand_Side);
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
      New_Line;

      --  Remaining symbols used in actions
      Indent_Names_Elisp (Output_File_Root, "module", Elisp_Names);

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
      use Wisi.Utils;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Root & "_wisi_module_parse.gpr");
      Set_Output (File);
      Indent := 1;
      Put_Line ("-- generated by FastToken Wisi from " & Input_File_Name);
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
      --
      --  gcc gives up without -fno-var-tracking-assignments; it is
      --  only useful when debugging, which is typically not needed in
      --  this file.
      Indent_Line ("case Wisi_Module_Parse_Common.Build is");
      Indent_Line ("when ""Debug"" =>");
      Indent_Line ("   for Switches (""" & Lower_Package_Name_Root & "_module.adb"") use");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("     Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("     (""-O0"", ""-fno-var-tracking-assignments"");");
      Indent_Line ("when ""Normal"" =>");
      Indent_Line ("for Switches (""" & Lower_Package_Name_Root & "_module.adb"") use");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Common_Switches &");
      Indent_Line ("  Wisi_Module_Parse_Common.Compiler.Standard_Style &");
      Indent_Line ("  (""-O2"", ""-fno-var-tracking-assignments"");");
      Indent_Line ("end case;");

      --  Other optimization levels hang here.
      Indent_Line ("for Switches (""" & Lower_Package_Name_Root & "_module.ads"") use");
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
      Put_Line ("-- generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line ("-- ");
      Indent_Line ("aggregate project " & Package_Name_Root & "_Wisi_Module_Parse_Agg is");
      Indent_Line ("   for Project_Path use (external (""WISI_OPENTOKEN""));");
      Indent_Line ("   for Project_files use (""" & Lower_Package_Name_Root & "_wisi_module_parse.gpr"");");
      Indent_Line ("end " & Package_Name_Root & "_Wisi_Module_Parse_Agg;");
      Set_Output (Standard_Output);
      Close (File);

      Create (File, Out_File, Output_File_Root & "_wisi_module_parse_wrapper.c");
      Set_Output (File);
      Indent := 1;
      Put_Line ("// generated by FastToken Wisi from " & Input_File_Name);
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
   Create_Ada_Body; -- populates Elisp_Names, used by Create_Ada_Spec
   Create_Ada_Spec;

   case Lexer is
   when Aflex_Lexer =>
      if Interface_Kind /= Process then
         raise Programmer_Error with "Aflex_Lexer assumed Process interface";
      end if;
      Create_Aflex;

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
--  FIXME: if keep, add to `safe-local-variable-values'
--  parsing is slow, so it gets in the way of interactive typeing due to immediate font-lock. Slow that down.
--  Local Variables:
--  jit-lock-defer-time: 0.5
--  End:
