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
with Wisi.Put_Module_Action_Line;
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

   procedure Create_Ada_Body
   is
      use Generate_Utils;
      use Wisi.Utils;

      Empty_Action : constant access constant String := new String'("Self");

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

      when Module =>
         Indent_Line ("with Emacs_Module_Aux; use Emacs_Module_Aux;");
      end case;

      case Data.Lexer is
      when Aflex_Lexer =>
         Put_Line ("with FastToken.Lexer.Aflex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_YYLex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_dfa;");
         Put_Line ("with " & Lower_Package_Name_Root & "_process_io;");

      when Elisp_Lexer =>
         Put_Line ("with FastToken.Lexer.Wisi_Elisp;");

      when Regexp_Lexer =>
         raise Programmer_Error;
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
         Indent_Line (Lower_Package_Name_Root & "_process_io.yy_eof_has_been_seen,");
         Indent_Line ("Wisi_Tokens_Pkg.Get);");
         Indent := Indent - 3;

      when Elisp_Lexer =>
         Indent_Line ("package Lexers is new Lexer_Root.Wisi_Elisp (Wisi_Tokens_Pkg.Get);");

      when Regexp_Lexer =>
         raise Programmer_Error;
      end case;

      Action_Names (Find_Token_ID (-FastToken_Accept_Name))     := new Action_Name_List (0 .. 0);
      Action_Names (Find_Token_ID (-FastToken_Accept_Name)) (0) := Empty_Action;

      case Data.Interface_Kind is
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

         case Data.Interface_Kind is
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
                 Standard.Ada.Containers.Count_Type'Image (Elisp_Names.Length) &
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
            Indent_Line ("   use Token_Pkg;");
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
            Indent_Line ("use Token_Pkg;");
            Indent_Line ("use Token_Pkg.List;");
            Indent_Line ("Tokens_1 : Emacs_Value_Array (1 .. Args.Length);");
            Indent_Line ("Tokens_I : Integer       := Tokens_1'First;");
            Indent_Line ("Args_I   : List_Iterator := First (Args);");
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
                           case Data.Interface_Kind is
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

      Create_Create_Parser (Input_File_Name, Data.Parser_Algorithm, Data.Lexer, Data.Interface_Kind);

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

      File_Name_Root : constant String := Output_File_Name_Root & "-process";
      File      : File_Type;
   begin
      Create (File, Out_File, File_Name_Root & ".el");
      Set_Output (File);
      Indent := 1;

      Put_Line (";; generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line (";; ");
      Put_Line (";;");
      Put_Prologue (False, Prologue_Context_Clause);
      New_Line;

      Indent_Names_Elisp (Output_File_Name_Root, "process", Elisp_Names);

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

      --  Remaining symbols used in actions
      Indent_Names_Elisp (Output_File_Name_Root, "module", Elisp_Names);

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

   Create_Ada_Body; -- populates, uses Elisp_Names

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
