--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The parser actions
--  assume the Emacs Ada mode wisi indentation engine
--
--  If run in a separate process communicating over pipes with the
--  Emacs process, the parser actions output encoded elisp actions;
--  the protocol is documented in Emacs Ada mode wisi-ext-parse.el,
--  function wisi-ext-parse-execute.
--
--  If run in an Emacs dynamically loaded module, the parser actions
--  call the elisp actions directly.
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
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

with Ada.Text_IO; use Ada.Text_IO;
with FastToken;
with Wisi.Gen_Output_Ada_Common;
with Wisi.Output_Elisp_Common; use Wisi.Output_Elisp_Common;
with Wisi.Utils;
procedure Wisi.Output_Ada_Emacs
  (Input_File_Name         : in String;
   Output_File_Name_Root   : in String;
   Generate_Params         : in Generate_Param_Type;
   Prologue_Context_Clause : in String_Lists.List;
   Prologue_Declarations   : in String_Lists.List;
   Keywords                : in String_Pair_Lists.List;
   Tokens                  : in Token_Lists.List;
   Conflicts               : in Conflict_Lists.List;
   Panic_Recover           : in String_Lists.List;
   Rules                   : in Rule_Lists.List;
   Rule_Count              : in Integer;
   Action_Count            : in Integer;
   Profile                 : in Boolean)
is
   use all type Standard.Ada.Containers.Count_Type;

   procedure Put_Prologue (Ada_Syntax : in Boolean; Prologue : in String_Lists.List)
   is
   begin
      --  Prologue in input file has elisp syntax. If Ada_Syntax, keep
      --  comments, ignore everything else.

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

   procedure Put_Ada_Prologue_Context_Clause
   is begin
      Put_Prologue (True, Prologue_Context_Clause);
   end Put_Ada_Prologue_Context_Clause;

   procedure Put_Ada_Prologue_Declarations
   is begin
      null;
   end Put_Ada_Prologue_Declarations;

   package Common is new Wisi.Gen_Output_Ada_Common
     (Keywords, Tokens, Conflicts, Rules, Generate_Params, Put_Ada_Prologue_Context_Clause,
      Put_Ada_Prologue_Declarations, Put_Ada_Prologue_Context_Clause);
   use Common;

   Elisp_Action_Names : Nonterminal_Array_Action_Names;

   procedure Create_Ada_Body
   is
      use Generate_Utils;
      use Wisi.Utils;

      Elisp_Action : constant access String := new String'("Elisp_Action'Access");
      --  There is only one Ada semantic action subprogram -
      --  Elisp_Action. It encodes and sends wisi-nterm, wisi-tokens,
      --  and the name of the elisp action function.

      Elisp_Action_Index : Natural := 0;
      --  We output a flat elisp array of Elisp_Action_Names;
      --  Elisp_Action_Index is the encoded version of that.

      File_Name : constant String := Output_File_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_process",
         when Module  => "_module") &
        ".adb";

      Package_Name : constant String := -Data.Package_Name_Root &
        (case Data.Interface_Kind is
         when Process => "_Process",
         when Module  => "_Module");

      Lower_Package_Name_Root : constant String := To_Lower (-Data.Package_Name_Root);

      Body_File : File_Type;

   begin
      if Data.Parser_Algorithm in LALR | LALR_LR1 then
         Parsers (LALR) := Generate_Utils.LALR_Generator.Generate
           (Data.Grammar,
            Generate_Utils.To_Conflicts
              (Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count, Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_Nonterminal_ID_Set (Panic_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := Generate_Utils.To_State_Count (Parsers (LALR).State_Last);
      end if;

      if Data.Parser_Algorithm in LR1 | LALR_LR1 then
         Parsers (LR1) := Generate_Utils.LR1_Generator.Generate
           (Data.Grammar,
            Generate_Utils.To_Conflicts
              (Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count, Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_Nonterminal_ID_Set (Panic_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := Generate_Utils.LR.Unknown_State_Index'Max
           (Data.Parser_State_Count, Generate_Utils.To_State_Count (Parsers (LR1).State_Last));
      end if;

      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Data.Table_Entry_Count := 0;
      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Ada_Prologue_Context_Clause;

      case Data.Interface_Kind is
      when Process =>
         Indent_Line ("with Ada.Text_IO; use Ada.Text_IO;");

         case Data.Lexer is
         when Aflex_Lexer =>
            Put_Line ("with FastToken.Lexer.Aflex;");
            Put_Line ("with " & Lower_Package_Name_Root & "_process_YYLex;");
            Put_Line ("with " & Lower_Package_Name_Root & "_process_dfa;");
            Put_Line ("with " & Lower_Package_Name_Root & "_process_io;");

         when Elisp_Lexer =>
            Put_Line ("with FastToken.Lexer.Elisp_Process;");

         when Regexp_Lexer =>
            raise Programmer_Error;
         end case;

      when Module =>
         Indent_Line ("with Emacs_Module_Aux; use Emacs_Module_Aux;");
      end case;

      case Data.Interface_Kind is
      when Process =>
         null;
      when Module =>
         Put_Line ("with Ada.Exceptions;");
         Put_Line ("with Ada.Strings.Unbounded;");
      end case;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Data.Interface_Kind is
      when Process =>
         case Data.Lexer is
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
            Indent_Line (Lower_Package_Name_Root & "_process_io.yy_eof_has_been_seen);");
            Indent := Indent - 3;

         when Elisp_Lexer =>
            Indent_Line ("package Lexer is new Lexer_Root.Elisp_Process;");

         when Regexp_Lexer =>
            raise Programmer_Error;
         end case;
         New_Line;

         --  Anything not starting with [ or ( is ignored by the Elisp side
         Indent_Line ("procedure Put_Trace (Item : in String)");
         Indent_Line ("is begin");
         Indent_Line ("   Put (Item);");
         Indent_Line ("end Put_Trace;");
         New_Line;
         Indent_Line ("procedure Put_Trace_Line (Item : in String)");
         Indent_Line ("is begin");
         Indent_Line ("   Put_Line (Item);");
         Indent_Line ("end Put_Trace_Line;");
         New_Line;

      when Module =>
         null;

         --  FIXME:
         --  Add_Elisp_Name ("wisi-debug");
         --  Add_Elisp_Name ("wisi-nonterm");
         --  Add_Elisp_Name ("wisi-tokens");
         --  Add_Elisp_Name ("wisi-cache-max");

      end case;

      if Profile then
         Indent_Line ("Action_Counts : array (Nonterminal_ID) of Integer := (others => 0);");
      end if;

      if Profile or Action_Count = 0 then
         null;

      else
         --  Populate Action_Names
         Indent_Line ("type Action_Code_List is array (Natural range <>) of Natural;");

         Indent_Line ("Elisp_Action_Codes : constant array (Token_Pkg.Nonterminal_ID) of access Action_Code_List :=");
         Indent_Line ("  (");
         Indent := Indent + 3;

         for Rule of Rules loop
            declare
               LHS_ID    : constant Generate_Utils.Token_Pkg.Nonterminal_ID := Find_Token_ID (-Rule.Left_Hand_Side);

               Index      : Integer := 0; -- Semantic_Action defines Index as zero-origin
               All_Empty  : Boolean := True;
               Temp_Ada   : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
               Temp_Elisp : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            begin
               for RHS of Rule.Right_Hand_Sides loop
                  if RHS.Action.Length > 0 then
                     All_Empty := False;
                     Temp_Elisp (Index) := new String'
                       (-Rule.Left_Hand_Side & ':' & FastToken.Int_Image (Index));
                     Temp_Ada (Index)   := Elisp_Action;
                  end if;

                  Index := Index + 1;
               end loop;

               if not All_Empty then
                  Ada_Action_Names (LHS_ID)   := new Action_Name_List'(Temp_Ada);
                  Elisp_Action_Names (LHS_ID) := new Action_Name_List'(Temp_Elisp);

                  Indent_Start (To_Token_Ada_Name (Rule.Left_Hand_Side) & " => new Action_Code_List'(");
                  if Ada_Action_Names (LHS_ID)'Length = 1 then
                     if Ada_Action_Names (LHS_ID)(0) = null then
                        Put ("0 => 0),");
                     else
                        Put ("0 => " & FastToken.Int_Image (Elisp_Action_Index) & "),");
                        Elisp_Action_Index := Elisp_Action_Index + 1;
                     end if;
                  else
                     for I in Ada_Action_Names (LHS_ID)'Range loop
                        if Ada_Action_Names (LHS_ID)(I) = null then
                           Put ("0");
                        else
                           Put (FastToken.Int_Image (Elisp_Action_Index));
                           Elisp_Action_Index := Elisp_Action_Index + 1;
                        end if;

                        if I = Ada_Action_Names (LHS_ID)'Last then
                           Put_Line ("),");
                        else
                           Put (", ");
                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end loop;
         Indent_Line ("others => null);");
         Indent := Indent - 3;

         New_Line;
         Indent_Line ("procedure Elisp_Action");
         Indent_Line (" (Nonterm : in Token_Pkg.Nonterminal_ID;");
         Indent_Line ("  Index   : in Natural;");
         Indent_Line ("  Source  : in Token_Pkg.List.Instance)");
         Indent_Line ("is");

         if Profile then
            Indent_Line ("   pragma Unreferenced (Index, Source);");
            Indent_Line ("begin");
            Indent_Line ("   Action_Counts (Nonterm) := Action_Counts (Nonterm) + 1;");

         else
            Indent_Line ("   use Tokens_Wisi_Process_Runtime;");
            Indent_Line ("begin");
            Indent := Indent + 3;

            Indent_Line ("if Source.Length > 0 then");
            Indent := Indent + 3;

            case Data.Interface_Kind is
            when Process =>
               Indent_Line ("Put_Line");
               Indent_Line ("  (""["" & To_Code (Nonterm) & To_Codes (Source) & " &
                              "Integer'Image (Elisp_Action_Codes (Nonterm) (Index)) & ""]"");");

            when Module =>
               Indent_Line ("Set_Wisi_Tokens (Nonterm, Source);");
            end case;

            Indent := Indent - 3;
            Indent_Line ("end if;");
         end if;
         Indent := Indent - 3;
         Indent_Line ("end Elisp_Action;");
         New_Line;
      end if;

      Create_Create_Parser (Data.Parser_Algorithm, Data.Lexer, Data.Interface_Kind);

      case Data.Interface_Kind is
      when Process =>
         null;
      when Module =>
         Indent_Line ("Parser : LR_Parser.Instance;");
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
           LR.State_Index'Image (Data.Parser_State_Count) & " states," &
           Integer'Image (Data.Table_Entry_Count) & " table entries");
      Put_Line
        (Integer'Image (Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
   end Create_Ada_Body;

   procedure Create_Process_Elisp
   is
      use Wisi.Utils;
      use Generate_Utils;
      use all type Rule_Lists.Cursor;
      use all type RHS_Lists.Cursor;

      File_Name_Root : constant String := Output_File_Name_Root & "-process";
      File           : File_Type;
      Paren_Done     : Boolean         := False;
      Rule_I         : Rule_Lists.Cursor;
      RHS_I          : RHS_Lists.Cursor;
   begin
      Create (File, Out_File, File_Name_Root & ".el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");
      Put_Prologue (False, Prologue_Context_Clause);
      New_Line;

      --  FIXME: if elisp-lexer, output token, keyword tables

      Indent_Line  ("(defconst " & File_Name_Root & "-token-table");
      Indent_Start ("  [");
      Indent := Indent + 3;
      declare
         Cursor : Token_Cursor := First;
      begin
         loop
            exit when Cursor.Is_Done;
            if Paren_Done then
               Indent_Line (-Cursor.Token_Name);
            else
               Paren_Done := True;
               Put_Line (-Cursor.Token_Name);
            end if;

            Cursor.Next;
         end loop;
      end;
      Indent_Line ("])");
      Indent := Indent - 3;
      New_Line;

      Indent_Line ("(defconst " & File_Name_Root & "-action-table");
      Indent_Line ("  (wisi-ext-compile-actions");
      Indent_Start ("   '(");
      Indent     := Indent + 5;
      Paren_Done := False;
      Rule_I     := Rules.First;
      for I in Elisp_Action_Names'Range loop
         if Elisp_Action_Names (I) /= null then
            RHS_I := Constant_Reference (Rules, Rule_I).Right_Hand_Sides.First;

            for J in Elisp_Action_Names (I).all'Range loop
               if Elisp_Action_Names (I) (J) /= null then
                  if Paren_Done then
                     Indent_Start ("(""" & Elisp_Action_Names (I) (J).all & """");
                  else
                     Paren_Done := True;
                     Put ("(""" & Elisp_Action_Names (I) (J).all & """");
                  end if;
                  for Line of Element (RHS_I).Action loop
                     New_Line;
                     Indent_Start (Line);
                  end loop;
                  Put_Line (")");
               end if;
               Next (RHS_I);
            end loop;
         end if;
         Next (Rule_I);
      end loop;
      Indent_Line (")))");
      Indent := Indent - 5;
      New_Line;

      Put_Line ("(provide '" & File_Name_Root & ")");
      Set_Output (Standard_Output);
      Close (File);

   end Create_Process_Elisp;

   procedure Create_Module_Elisp
   is
      use Generate_Utils;
      use Wisi.Utils;

      Lower_Package_Name_Root : constant String := -Data.Package_Name_Root;

      function To_ID_Image (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
      is begin
         --  Ada 'Val is 0 origin; Generate_Utils Token_ID is 1 origin
         return Integer'Image (-1 + Find_Token_ID (-Name));
      end To_ID_Image;

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & "-module.el");
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
        (Output_File_Name_Root, "elisp", Keywords, EOI_Name, Standard.Ada.Strings.Unbounded.To_String'Access);
      Indent_Keyword_Table_Elisp (Output_File_Name_Root, "module", Keywords, EOI_Name, To_ID_Image'Access);
      Indent_Token_Table_Elisp
        (Output_File_Name_Root, "elisp", Tokens, Standard.Ada.Strings.Unbounded.To_String'Access);
      Indent_Token_Table_Elisp (Output_File_Name_Root, "module", Tokens, To_ID_Image'Access);

      --  non-terminals. We only need the ones that actually have
      --  actions, and thus will appear in a call to To_Emacs. But
      --  Token_Symbols must be indexed by Token_ID, so we declare
      --  all of them.
      Indent_Line ("(defconst " & Output_File_Name_Root & "-module-nonterms");
      Indent_Line (" '(");
      Indent := Indent + 3;
      Indent_Line (-FastToken_Accept_Name);
      for Rule of Rules loop
         Indent_Line (-Rule.Left_Hand_Side);
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
      New_Line;

      --  FIXME: output action names

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
      Put_Line ("-- generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line ("-- ");
      Indent_Line ("with ""wisi_module_parse_common"";");
      Indent_Line ("library project " & Package_Name_Root & "_Wisi_Module_Parse is");
      New_Line;
      Indent := Indent + 3;
      --  FIXME: compile wrapper.c here?
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
      Put_Line ("-- generated by FastToken Wisi from " & Input_File_Name);
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
   if Prologue_Declarations.Length > 0 then
      raise User_Error with Wisi.Utils.Error_String
        (Input_File_Name, 1, "Output language Ada_Emacs does not support prologue declarations");
   end if;

   Common.Initialize (Input_File_Name, Output_File_Name_Root, Check_Interface => True);

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
      Interface_Kind     => Generate_Params.Interface_Kind,
      Lexer              => Generate_Params.Lexer,
      First_State_Index  => Generate_Params.First_State_Index,
      First_Parser_Label => Generate_Params.First_Parser_Label);

   Create_Ada_Body;

   case Data.Lexer is
   when Aflex_Lexer =>
      if Data.Interface_Kind /= Process then
         raise Programmer_Error with "Aflex_Lexer assumed Process interface";
      end if;
      Create_Aflex (Input_File_Name, Output_File_Name_Root & "_process");

   when Elisp_Lexer =>
      --  All of the lexers need an elisp file; the form of the elisp
      --  file is determined by Interface_Kind (see below).
      null;

   when Regexp_Lexer =>
      raise Programmer_Error;
   end case;

   if not Profile then
      case Data.Interface_Kind is
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
--  Local Variables:
--  jit-lock-defer-time: 0.25
--  End:
