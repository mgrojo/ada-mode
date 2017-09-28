--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;
with WisiToken;
package body Wisi.Gen_Output_Ada_Common is

   --  Public subprograms in alphabetical order

   procedure Create_Ada_Spec
     (Input_File_Name    : in String;
      Output_File_Name   : in String;
      Package_Name       : in String;
      Output_Language    : in Ada_Output_Language;
      Descriptor         : in WisiToken.Descriptor'Class;
      Interface_Kind     : in Valid_Interface;
      Lexer              : in Valid_Lexer)
   is
      use Generate_Utils;
      use Wisi.Utils;

      Lower_Package_Name_Root : constant String := To_Lower (Package_Name);

      Spec_File : File_Type;

      Cursor : Token_Cursor;
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Ada_Prologue_Context_Clause;

      case Output_Language is
      when Ada =>
         Put_Line ("with WisiToken.Text_IO_Trace;");
         Put_Line ("with WisiToken.Token_Region;");

         case Lexer is
         when Regexp_Lexer =>
            Put_Line ("with WisiToken.Text_Feeder;");

         when Elisp_Lexer | re2c_Lexer =>
            null;
         end case;

      when Ada_Emacs =>
         case Interface_Kind is
         when Process =>
            Put_Line ("with WisiToken.Text_IO_Trace;");
            Put_Line ("with WisiToken.Token_Emacs_Process;");

         when Module =>
            Put_Line ("with Emacs_Module_Aux;");
            Put_Line ("with emacs_module_h;");
            Put_Line ("with Interfaces.C;");
            Put_Line ("with WisiToken.Token;");
         end case;
      end case;
      Put_Line ("with WisiToken.Parser.LR.Parser;");
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("type Token_Enum_ID is");
      Indent_Line ("  (");
      Indent := Indent + 3;

      Cursor := First (Non_Reporting => True);
      loop
         exit when Is_Done (Cursor);
         Set_Col (Indent);
         Put (To_Token_Ada_Name (Name (Cursor)));

         Next (Cursor, Other_Tokens => True);

         if Is_Done (Cursor) then
            Put_Line (");");
         else
            Put_Line (",");
         end if;
      end loop;
      Indent := Indent - 3;
      New_Line;

      Indent_Line ("function ""+"" (Item : in Token_Enum_ID) return WisiToken.Token_ID");
      Indent_Line ("  is (WisiToken.""+"" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));");
      New_Line;
      Indent_Line ("function ""-"" (Item : in WisiToken.Token_ID) return Token_Enum_ID");
      Indent_Line ("  is (Token_Enum_ID'Val (WisiToken.""-"" (Item, WisiToken.Token_ID'First)));");
      New_Line;

      Indent_Line ("Descriptor : aliased WisiToken.Descriptor :=");
      Indent_Line ("  (First_Terminal    =>" & WisiToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal     =>" & WisiToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal =>" & WisiToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal  =>" & WisiToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("EOF_ID            =>" & WisiToken.Token_ID'Image (Descriptor.EOF_ID) & ",");
      Indent_Line ("Accept_ID         =>" & WisiToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Image             =>");
      declare
         use Standard.Ada.Strings.Unbounded;
         Paren_Done : Boolean := False;
      begin
         Cursor := First (Non_Reporting => True);
         Indent_Start ("  (");
         Indent := Indent + 3;
         loop
            exit when Is_Done (Cursor);
            if Paren_Done then
               Indent_Start ("new String'(""" & (Name (Cursor)));
            else
               Put ("new String'(""" & (Name (Cursor)));
               Paren_Done := True;
            end if;
            Next (Cursor, Other_Tokens => True);
            if Is_Done (Cursor) then
               Put_Line (""")),");
            else
               Put_Line ("""),");
            end if;
         end loop;

         Indent := Indent - 3;
         Indent_Line ("Terminal_Image_Width =>" & Integer'Image (Descriptor.Terminal_Image_Width) & ",");
         Indent_Line ("Image_Width          =>" & Integer'Image (Descriptor.Image_Width) & ");");
         New_Line;
      end;
      Indent := Indent - 3;
      New_Line;

      case Lexer is
      when Elisp_Lexer | re2c_Lexer =>
         null;
      when Regexp_Lexer =>
         raise Programmer_Error;
      end case;
      New_Line;

      case Output_Language is
      when Ada =>
         Indent_Line ("Trace : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);");
         Indent_Line ("State : aliased WisiToken.Token_Region.State_Type (Trace'Access);");
         New_Line;
         Indent_Line ("procedure Create_Parser");
         Indent_Line ("  (Parser    :    out WisiToken.Parser.LR.Parser.Instance;");
         Indent_Line ("   Algorithm : in     WisiToken.Parser_Algorithm_Type);");
         New_Line;

      when Ada_Emacs =>
         case Interface_Kind is
         when Process =>
            Indent_Line ("Trace : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);");
            Indent_Line ("State : aliased WisiToken.Token_Emacs_Process.State_Type (Trace'Access);");
            New_Line;
            Indent_Line ("procedure Create_Parser");
            Indent_Line ("  (Parser    :    out WisiToken.Parser.LR.Parser.Instance;");
            Indent_Line ("   Algorithm : in     WisiToken.Parser_Algorithm_Type);");
            New_Line;

         when Module =>
            Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
            Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name_Root & "_wisi_module_parse"");");
            Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
            Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name_Root & "_wisi_module_parse_init"");");
            New_Line;

         end case;
      end case;

      Put_Ada_Prologue_Declarations;

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Spec;

   procedure Create_re2c
     (Input_File_Name       : in String;
      Output_File_Name_Root : in String)
   is
      use Standard.Ada.Strings.Fixed;
      use Generate_Utils;
      use Wisi.Utils;
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".re2c");
      Set_Output (File);
      Indent := 1;

      Put_Line ("// -*- C -*- generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line ("//  ");
      Put_Line ("//");
      Put_C_Prologue;
      New_Line;

      Indent_Line ("#include <stddef.h>"); -- size_t
      Indent_Line ("#include <stdio.h>"); -- printf
      Indent_Line ("#include <stdlib.h>"); -- malloc
      New_Line;

      Indent_Line ("typedef struct wisi_lexer");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("unsigned char* buffer;           // input text, in utf-8 encoding");
      Indent_Line ("unsigned char* buffer_last;      // last byte in buffer");
      Indent_Line ("unsigned char* cursor;           // current byte");
      Indent_Line ("unsigned char* byte_token_start; // byte position at start of current token");
      Indent_Line ("size_t         char_pos;         // character position of current character");
      Indent_Line ("size_t         char_token_start; // character position at start of current token");
      Indent_Line ("unsigned char* marker;           // saved cursor");
      Indent_Line ("size_t         marker_pos;       // saved character position");
      Indent_Line ("unsigned char* context;          // saved cursor");
      Indent_Line ("size_t         context_pos;      // saved character position");
      Indent_Line ("int            verbosity;");
      New_Line;
      Indent := Indent - 3;
      Indent_Line ("} wisi_lexer;");
      New_Line;
      Indent_Line ("#define YYCTYPE unsigned char");
      New_Line;

      --  Status values:
      Indent_Line ("#define NO_ERROR 0");
      Indent_Line ("#define ERROR_unrecognized_character 1");

      ----------
      --  new_lexer, free_lexer, reset_lexer

      --  It's normal to increment lexer->cursor one past the end of input,
      --  but not to read that character. To support memory mapped files, we
      --  enforce this strictly; YYPEEK returns EOT (end of text) when
      --  reading past end of buffer; that's how we recognize the end of
      --  text token.

      Indent_Line ("wisi_lexer* " & Output_File_Name_Root & "_new_lexer");
      Indent_Line ("   (unsigned char* input, size_t length, int verbosity)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("wisi_lexer* result = malloc (sizeof (wisi_lexer));");
      Indent_Line ("result->buffer      = input;");
      Indent_Line ("result->buffer_last = input + length - 1;");
      Indent_Line ("result->cursor      = input;");
      Indent_Line ("result->char_pos    = 0;");
      Indent_Line ("result->verbosity   = verbosity;");
      Indent_Line ("return result;");
      Indent_Line ("}");
      Indent := Indent - 3;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_free_lexer(wisi_lexer** lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("free(*lexer);");
      Indent_Line ("*lexer = 0;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_reset_lexer(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->cursor   = lexer->buffer;");
      Indent_Line ("lexer->char_pos = 0;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      ----------
      --  next_token utils

      Indent_Line ("static void debug(wisi_lexer* lexer, int state, unsigned char ch)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->verbosity > 0)");
      Indent_Line ("   printf (""lexer: %d, '%c'\n"", state, ch);");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Line ("#define YYDEBUG(state, ch) debug(lexer, state, ch)");

      --  YYCURSOR is only used in calls of YYDEBUG; we can't define it as
      --  YYPEEK because it is used as '*YYCURSOR'.
      Indent_Line ("#define YYCURSOR lexer->cursor");
      New_Line;

      Indent_Line ("#define YYPEEK() (lexer->cursor <= lexer->buffer_last) ? *lexer->cursor : 4");
      New_Line;

      Indent_Line ("static void skip(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last) ++lexer->cursor;");
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      --  Don't count UTF-8 continuation bytes
      Indent_Line ("   if ((*lexer->cursor & 0xC0) != 0xC0) ++lexer->char_pos;");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Start ("#define YYSKIP() skip(lexer)");
      New_Line;

      Indent_Line ("#define YYBACKUP() lexer->marker = lexer->cursor; lexer->marker_pos = lexer->char_pos");
      Indent_Line ("#define YYRESTORE() lexer->cursor = lexer->marker; lexer->char_pos = lexer->marker_pos");
      Indent_Line ("#define YYBACKUPCTX() lexer->context = lexer->cursor; lexer->context_pos = lexer->char_pos");
      Indent_Line ("#define YYRESTORECTX() lexer->cursor = lexer->context; lexer->char_pos = lexer->context_pos");
      New_Line;

      ----------
      --  next_token
      Indent_Line ("int " & Output_File_Name_Root & "_next_token");
      Indent_Line ("  (wisi_lexer* lexer,");
      Indent_Line ("   int* id,");
      Indent_Line ("   size_t* byte_position,");
      Indent_Line ("   size_t* byte_length,");
      Indent_Line ("   size_t* char_position,");
      Indent_Line ("   size_t* char_length)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Indent_Line ("int status = NO_ERROR;");
      Indent_Line ("*id = 0;");

      Indent_Line ("if (lexer->cursor > lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("*id       = " & WisiToken.Token_ID'Image (LR1_Descriptor.EOF_ID) & ";");
      Indent_Line ("*byte_position = lexer->buffer_last - lexer->buffer;");
      Indent_Line ("*byte_length   = 0;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = 0;");
      Indent_Line ("return status;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("lexer->byte_token_start = lexer->cursor;");
      Indent_Line ("lexer->char_token_start = lexer->char_pos;");
      New_Line;

      Indent_Line ("while (*id == 0 && status == 0)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Put_Line ("/*!re2c");
      Indent_Line ("re2c:yyfill:enable   = 0;");
      New_Line;

      --  definitions
      for I in All_Tokens.Iterate (Non_Reporting => True, Other_Tokens => False) loop

         if Kind (I) = "keyword" and Params.Keywords_Case_Insensitive then
            Indent_Line (Name (I) & " = '" & Strip_Quotes (Value (I)) & "';");

         elsif Kind (I) = "number" and Value (I) = "ada-wisi-number-p" then
            Indent_Line (Name (I) & " = ([0-9]+""#"")?[0-9][0-9a-fA-F._]*(""#"")?;");

         elsif 0 /= Index (Source => Value (I), Pattern => "/") then
            --  trailing context syntax; forbidden in definitions
            null;

         else
            --  Other kinds have values that are regular expressions, in re2c syntax
            Indent_Line (Name (I) & " = " & Value (I) & ";");
         end if;
      end loop;
      New_Line;

      --  rules
      for I in All_Tokens.Iterate (Non_Reporting => True, Other_Tokens => False) loop

         if Kind (I) = "whitespace" or  Kind (I) = "line_comment" then
            Indent_Line (Name (I) & " { lexer->byte_token_start = lexer->cursor;");
            Indent_Line ("    lexer->char_token_start = lexer->char_pos; continue; }");

         elsif 0 /= Index (Source => Value (I), Pattern => "/") then
            Indent_Line (Value (I) & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");

         else
            Indent_Line (Name (I) & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");
         end if;
      end loop;
      New_Line;

      --  end of text
      Indent_Line ("[\x04] {*id = " & WisiToken.Token_ID'Image (LR1_Descriptor.EOF_ID) & "; continue;}");

      --  Default action
      Indent_Line ("* {status = ERROR_unrecognized_character; continue;}");

      Put_Line ("*/");
      Indent_Line ("}");
      Indent := Indent - 3;

      Indent_Line ("*byte_position = lexer->byte_token_start - lexer->buffer;");
      Indent_Line ("*byte_length   = lexer->cursor - lexer->byte_token_start;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = lexer->char_pos - lexer->char_token_start;");
      Indent_Line ("return status;");
      Indent_Line ("}");
      Indent := Indent - 3;
      Set_Output (Standard_Output);
      Close (File);

      declare
         Ada_Name : constant String := Output_File_Name_Root & "_re2c_c";
         --  Output_File_Name_Root is the file name of the grammar file -
         --  assume it is a legal Ada name.
      begin
         Create (File, Out_File, Output_File_Name_Root & "_re2c_c.ads");
         Set_Output (File);
         Indent := 1;
         Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
         Put_Command_Line ("--  ");
         Put_Line ("--");
         Put_Ada_Prologue_Context_Clause;
         New_Line;

         Put_Line ("with Interfaces.C;");
         Put_Line ("with WisiToken;");
         Put_Line ("with System;");
         Put_Line ("package " & Ada_Name & " is");
         Indent := Indent + 3;
         New_Line;

         Indent_Line ("function New_Lexer");
         Indent_Line ("  (Buffer    : in System.Address;");
         Indent_Line ("   Length    : in Interfaces.C.size_t;");
         Indent_Line ("   Verbosity : in Interfaces.C.int)");
         Indent_Line ("  return System.Address");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_new_lexer"";");
         Indent_Line ("--  Create the lexer object, passing it the full text to process.");
         New_Line;
         Indent_Line ("procedure Free_Lexer (Lexer : in out System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_free_lexer"";");
         Indent_Line ("--  Free the lexer object");
         New_Line;

         Indent_Line ("procedure Reset_Lexer (Lexer    : in System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_reset_lexer"";");
         New_Line;

         Indent_Line ("function Next_Token");
         Indent_Line ("  (Lexer         : in     System.Address;");
         Indent_Line ("   ID            :    out WisiToken.Token_ID;");
         Indent_Line ("   Byte_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Byte_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Length   :    out Interfaces.C.size_t)");
         Indent_Line ("  return Interfaces.C.int");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_next_token"";");
         New_Line;

         Indent := Indent - 3;
         Put_Line ("end " & Ada_Name & ";");
         Set_Output (Standard_Output);
         Close (File);
      end;
   end Create_re2c;

   procedure Create_Create_Parser
     (Parser_Algorithm   : in Valid_Parser_Algorithm;
      Lexer              : in Valid_Lexer;
      Interface_Kind     : in Interface_Type;
      First_State_Index  : in Integer;
      First_Parser_Label : in Integer)
   is
      use Generate_Utils;
      use Wisi.Utils;
      use all type WisiToken.Parser.LR.Unknown_State_Index;
   begin
      Indent_Line ("procedure Create_Parser");
      Indent_Line ("  (Parser    : out    WisiToken.Parser.LR.Parser.Instance;");
      case Interface_Kind is
      when None | Process =>
         Indent_Line ("   Algorithm : in     WisiToken.Parser_Algorithm_Type)");
      when Module =>
         Indent_Line ("   Env                 : in Emacs_Env_Access;");
         Indent_Line ("   Lexer_Elisp_Symbols : in Lexers.Elisp_Array_Emacs_Value)");
      end case;

      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use WisiToken.Parser.LR;");
      Indent_Line ("use all type WisiToken.Parser_Algorithm_Type;");
      Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
      Indent_Line  ("  (State_First   =>" & Integer'Image (First_State_Index) & ",");
      Indent := Indent + 3;
      Indent_Start ("State_Last       => ");

      case Parser_Algorithm is
      when LALR =>
         Put_Line (State_Image (Parsers (LALR).State_Last) & ",");

      when LR1 =>
         Put_Line (State_Image (Parsers (LR1).State_Last) & ",");

      when LALR_LR1 =>
         Put_Line
           ("(case Algorithm is when LALR => " & State_Image (Parsers (LALR).State_Last) &
              ", when LR1 => " & State_Image (Parsers (LR1).State_Last) & "),");
      end case;
      Indent_Line ("First_Terminal    => Descriptor.First_Terminal,");
      Indent_Line ("Last_Terminal     => Descriptor.Last_Terminal,");
      Indent_Line ("First_Nonterminal => Descriptor.First_Nonterminal,");
      Indent_Line ("Last_Nonterminal  => Descriptor.Last_Nonterminal);");
      Indent := Indent - 3;

      case Parser_Algorithm is
      when LALR =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LALR));

      when LR1 =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LR1));

      when LALR_LR1 =>
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("case Algorithm is");
         Indent_Line ("when LALR =>");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LALR));
         Indent := Indent - 3;
         Indent_Line ("when LR1 =>");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LR1));
         Indent := Indent - 3;
         Indent_Line ("end case;");
      end case;
      New_Line;

      Indent_Line ("WisiToken.Parser.LR.Parser.New_Parser");
      Indent_Line ("  (Parser,");
      case Interface_Kind is
      when None | Process =>
         case Lexer is
         when re2c_Lexer =>
            Indent_Line ("   Lexer.New_Lexer (Trace'Access),");

         when Elisp_Lexer =>
            Indent_Line ("   WisiToken.Lexer.Elisp_Process.New_Lexer (" & WisiToken.Int_Image (EOF_ID) &
                           ", Trace'Access),");

         when Regexp_Lexer =>
            raise Programmer_Error;
         end case;

         Indent_Line ("   Table,");
         Indent_Line ("   WisiToken.Token.Semantic_State'Class (State)'Access,");
         Indent_Line ("   Max_Parallel            => 15,");
         Indent_Line ("   First_Parser_Label      => " & WisiToken.Int_Image (First_Parser_Label) & ",");
         Indent_Line ("   Terminate_Same_State    => True);");

      when Module =>
         Indent_Line ("   Lexer.New_Lexer (Env, Lexer_Elisp_Symbols),");
         Indent_Line ("   Table, Max_Parallel => 15, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;
   end Create_Create_Parser;

   procedure Create_Parser_Core (Table : in WisiToken.Parser.LR.Parse_Table_Ptr)
   is
      use all type WisiToken.Parser.LR.Patterns.List;
      use all type WisiToken.Token_ID;
      use all type WisiToken.Parser.LR.McKenzie_Param_Type;
      use Generate_Utils;
      use Wisi.Utils;

      Paren_Done      : Boolean  := False;
      Count           : Integer;
      Items_Per_Line : constant := 8;

      function Natural_Image (Item : in Natural) return String
      is
         use Standard.Ada.Strings;
         use Standard.Ada.Strings.Fixed;
      begin
         return Trim (Natural'Image (Item), Both);
      end Natural_Image;

      procedure Put (Label : in String; Item : in WisiToken.Token_Array_Natural)
      is begin
         Indent_Line (Label & " =>");
         Indent_Start ("  (");
         Indent := Indent + 3;
         Count := 0;
         for I in Item'Range loop
            Count := Count + 1;
            Put (Natural_Image (Item (I)));

            if I = Item'Last then
               Put_Line ("),");

            elsif Count = Items_Per_Line then
               Count := 0;
               Put_Line (",");
               Indent_Start ("");

            else
               Put (", ");
            end if;
         end loop;
         Indent := Indent - 3;
      end Put;

   begin
      if Table.McKenzie = WisiToken.Parser.LR.Default_McKenzie_Param then
         Indent_Line ("Table.McKenzie := Default_McKenzie_Param;");
      else
         Indent_Line ("Table.McKenzie :=");
         Indent_Line ("  (First_Terminal    =>" & WisiToken.Token_ID'Image (Table.McKenzie.First_Terminal) & ",");
         Indent := Indent + 3;
         Indent_Line ("Last_Terminal     =>" & WisiToken.Token_ID'Image (Table.McKenzie.Last_Terminal) & ",");
         Indent_Line ("First_Nonterminal =>" & WisiToken.Token_ID'Image (Table.McKenzie.First_Nonterminal) & ",");
         Indent_Line ("Last_Nonterminal  =>" & WisiToken.Token_ID'Image (Table.McKenzie.Last_Nonterminal) & ",");
         Put ("Insert", Table.McKenzie.Insert);
         Put ("Delete", Table.McKenzie.Delete);
         Indent_Line ("Cost_Limit  =>" & Integer'Image (Table.McKenzie.Cost_Limit) & ",");
         Indent_Line ("Check_Limit =>" & Integer'Image (Table.McKenzie.Check_Limit) & ",");
         Indent_Line ("Patterns    => WisiToken.Parser.LR.Patterns.Empty_List);");
         Indent := Indent - 3;
         New_Line;

         --  WORKAROUND: GNAT GPL 2016 compiler hangs on this:
         --  for Pattern of Table.McKenzie.Patterns loop
         --     Indent_Line ("Table.Mckenzie.patterns.Append (" & Pattern.Image & ");");
         --  end loop;
         declare
            use WisiToken.Parser.LR.Patterns;
            I : Cursor := Table.McKenzie.Patterns.First;
         begin
            loop
               exit when I = No_Element;
               Indent_Line ("Table.McKenzie.Patterns.Append (" & Element (I).Image & ");");
               Next (I);
            end loop;
         end;
      end if;

      if not WisiToken.Any (Table.Follow) then
         Indent_Line ("Table.Follow := (others => (others => False));");
      else
         Indent_Line ("Table.Follow :=");
         Indent_Start ("  (");
         Indent := Indent + 3;
         for I in Table.Follow'Range (1) loop
            if WisiToken.Any (Table.Follow, I) then
               Indent_Line (WisiToken.Int_Image (I) & " =>");
               Indent_Start ("  (");
               Indent := Indent + 3;
               Paren_Done := False;
               for J in Table.Follow'Range (2) loop
                  if Table.Follow (I, J) then
                     if Paren_Done then
                        Put_Line (" |");
                        Indent_Start (" " & WisiToken.Int_Image (J));
                     else
                        Paren_Done := True;
                        Put (WisiToken.Int_Image (J));
                     end if;
                  end if;
               end loop;
               if Paren_Done then
                  Put_Line (" => True,");
                  Indent_Line (" others => False),");
               else
                  Put_Line ("others => False),");
               end if;
               Indent := Indent - 3;
            end if;
         end loop;
         Indent_Line ("others => (others => False));");
         Indent := Indent - 3;
      end if;
      New_Line;

      for State_Index in Table.States'Range loop
         Actions :
         declare
            use Standard.Ada.Containers;
            use Standard.Ada.Strings;
            use Standard.Ada.Strings.Unbounded;
            use WisiToken.Parser.LR;
            Base_Indent : constant Standard.Ada.Text_IO.Count := Indent;
            Node        : Action_Node_Ptr := Table.States (State_Index).Action_List;
            Line        : Unbounded_String;

            procedure Append (Item : in String)
            is
               Max_Line_Length : constant := 120;
            begin
               --  -2 for trailing ); or ,
               if Indent + Standard.Ada.Text_IO.Count (Length (Line)) + Item'Length > Max_Line_Length - 2 then
                  Put_Line (-Trim (Line, Right));
                  Indent := Indent + 2;
                  Set_Col (Indent);
                  Line := +Item;
               else
                  Line := Line & Item;
               end if;
            end Append;

         begin
            loop
               exit when Node = null;
               Data.Table_Entry_Count := Data.Table_Entry_Count + 1;
               Set_Col (Indent);
               declare
                  Action_Node : Parse_Action_Node_Ptr := Node.Action;
               begin
                  case Action_Node.Item.Verb is
                  when Shift =>
                     Line := +"Add_Action (Table.States (" & State_Image (State_Index) & "), " &
                       WisiToken.Int_Image (Node.Symbol);
                     Append (", ");
                     Append (State_Image (Action_Node.Item.State));

                  when Reduce | Accept_It =>
                     Line := +"Add_Action (Table.States (" & State_Image (State_Index) & "), " &
                       WisiToken.Int_Image (Node.Symbol);
                     if Action_Node.Item.Verb = Reduce then
                        Append (", Reduce");
                     else
                        Append (", Accept_It");
                     end if;
                     Append (", ");
                     Append (WisiToken.Int_Image (Action_Node.Item.LHS) & ",");
                     Append (Integer'Image (Action_Node.Item.Index) & ", ");
                     Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                     Append
                       ((if Ada_Action_Names (Action_Node.Item.LHS) = null then "null"
                         elsif Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index) = null then "null"
                         else Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index).all));

                  when Error =>
                     Line := +"Add_Error (Table.States (" & State_Image (State_Index) & ")";
                  end case;

                  Action_Node := Action_Node.Next;
                  if Action_Node /= null then
                     --  There is a conflict; must be Shift/{Reduce|Accept} or Reduce/{Reduce|Accept}.
                     --  The added parameters are the same in either case.
                     case Action_Node.Item.Verb is
                     when Reduce | Accept_It =>
                        Append (", ");
                        Append (WisiToken.Int_Image (Action_Node.Item.LHS) & ",");
                        Append (Integer'Image (Action_Node.Item.Index) & ", ");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                        Append
                          ((if Ada_Action_Names (Action_Node.Item.LHS) = null then "null"
                            elsif Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index) = null then "null"
                            else Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index).all));

                     when others =>
                        raise Programmer_Error with "conflict second action verb: " &
                          WisiToken.Parser.LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                     end case;
                  end if;
               end;
               Put_Line (-Line & ");");
               Indent := Base_Indent;
               Node := Node.Next;
            end loop;
         end Actions;

         Gotos :
         declare
            use WisiToken.Parser.LR;
            Node : Goto_Node_Ptr := Table.States (State_Index).Goto_List;
         begin
            loop
               exit when Node = null;
               Set_Col (Indent);
               Put ("Add_Goto (Table.States (" & State_Image (State_Index) & "), ");
               Put_Line (WisiToken.Int_Image (Symbol (Node)) & ", " & State_Image (State (Node)) & ");");
               Node := Next (Node);
            end loop;
         end Gotos;
      end loop;
   end Create_Parser_Core;

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

   procedure Initialize
     (Input_File_Name  : in String;
      Output_File_Root : in String;
      Check_Interface  : in Boolean)
   is begin
      declare
         use Wisi.Utils;
         Quit : Boolean := False;
      begin
         Data.Parser_Algorithm := Params.Parser_Algorithm; -- checked in Wisi.Declarations

         if Params.Lexer in Valid_Lexer then
            Data.Lexer := Valid_Lexer (Params.Lexer);
         else
            Put_Error (Input_File_Name, 1, "Lexer not set in grammar file");
            Quit := True;
         end if;

         if Check_Interface then
            if Params.Interface_Kind in Valid_Interface then
               Data.Interface_Kind := Valid_Interface (Params.Interface_Kind);
            else
               Put_Error (Input_File_Name, 1, "Interface_Kind not set in grammar file");
               Quit := True;
            end if;
         end if;

         if Quit then raise User_Error with "missing grammar file directives"; end if;
      end;

      Data.Grammar := Generate_Utils.To_Grammar (Generate_Utils.LR1_Descriptor, Input_File_Name, -Params.Start_Token);

      Data.Package_Name_Root       := +File_Name_To_Ada (Output_File_Root);
      Data.Lower_Package_Name_Root := +To_Lower (Output_File_Root);
   end Initialize;

   function To_Token_Ada_Name (Item : in String) return String
   is
      --  Convert Item to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := Item;
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

end Wisi.Gen_Output_Ada_Common;
