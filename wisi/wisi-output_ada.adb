--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The grammar parser
--  actions must be Ada.
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regexp;
with Wisi.Gen_Output_Ada_Common;
with Wisi.Utils;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.LR1_Generator;
with WisiToken.Wisi_Grammar_Runtime;
procedure Wisi.Output_Ada
  (Input_Data            : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
   Output_File_Name_Root : in String)
is
   use all type WisiToken.Unknown_State_Index;

   package Common is new Wisi.Gen_Output_Ada_Common
     (Input_Data.Raw_Code, Input_Data.Tokens, Input_Data.Conflicts, Input_Data.Generate_Params);
   use Common;

   Data       : Common.Data_Type;
   LR_Parsers : LR_Parser_Array;

   function Symbol_Regexp (Item : in String) return String
   is begin
      --  Return a regular expression string that matches Item as a symbol;
      --  it must be preceded and followed by non-symbol characters.
      --
      --  GNAT.Regexp does not have a char for 'end of string', so we hope
      --  that doesn't occur. Sigh.
      return ".*[ (\.]" & Item & "[ );\.,].*";
   end Symbol_Regexp;

   procedure Create_Ada_Actions_Body
     (Ada_Action_Names :    out Nonterminal_Names_Array;
      Ada_Check_Names  :    out Nonterminal_Names_Array;
      Package_Name     : in     String)
   is
      use Generate_Utils;
      use GNAT.Regexp;
      use Wisi.Utils;

      File_Name : constant String := Output_File_Name_Root & "_actions.adb";

      User_Data_Regexp : constant Regexp := Compile (Symbol_Regexp ("User_Data"), Case_Sensitive => False);
      Tree_Regexp      : constant Regexp := Compile (Symbol_Regexp ("Tree"), Case_Sensitive      => False);
      Nonterm_Regexp   : constant Regexp := Compile (Symbol_Regexp ("Nonterm"), Case_Sensitive   => False);
      Tokens_Regexp    : constant Regexp := Compile (Symbol_Regexp ("Tokens"), Case_Sensitive    => False);

      Body_File : File_Type;
   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Data.Table_Entry_Count := 0;
      Put_File_Header (Ada_Comment);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Context));
      New_Line;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      if Input_Data.Check_Count > 0 then
         Indent_Line ("use WisiToken.Semantic_Checks;");
         New_Line;
      end if;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Pre));

      --  generate Action and Check subprograms, populate Ada_Action_Names.
      --  Ada_Check_Names.

      for Rule of Input_Data.Tokens.Rules loop
         --  No need for a Token_Cursor here, since we only need the
         --  nonterminals.
         declare
            use Standard.Ada.Strings.Unbounded;

            LHS_ID : constant WisiToken.Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);

            Action_Names     : Names_Array (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Check_Names      : Names_Array (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            RHS_Index        : Integer := 0;
            Action_All_Empty : Boolean := True;
            Check_All_Empty  : Boolean := True;

            function Is_Elisp (Action : in Unbounded_String) return Boolean
            is begin
               return Length (Action) >= 6 and then
                 (Slice (Action, 1, 6) = "(progn" or
                    Slice (Action, 1, 5) = "wisi-");
            end Is_Elisp;

         begin
            for RHS of Rule.Right_Hand_Sides loop
               if Length (RHS.Action) > 0 and then not Is_Elisp (RHS.Action) then
                  declare
                     Line : constant String := -RHS.Action;
                     --  Actually multiple lines; we assume the formatting is adequate.

                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Trimmed_Image (RHS_Index);

                     Unref_User_Data : Boolean := True;
                     Unref_Tree      : Boolean := True;
                     Unref_Nonterm   : Boolean := True;
                     Unref_Tokens    : Boolean := True;
                     Need_Comma      : Boolean := False;

                     procedure Check_Unref (Line : in String)
                     is begin
                        if Match (Line, User_Data_Regexp) then
                           Unref_User_Data := False;
                        end if;
                        if Match (Line, Tree_Regexp) then
                           Unref_Tree := False;
                        end if;
                        if Match (Line, Nonterm_Regexp) then
                           Unref_Nonterm := False;
                        end if;
                        if Match (Line, Tokens_Regexp) then
                           Unref_Tokens := False;
                        end if;
                     end Check_Unref;

                  begin
                     Check_Unref (Line);

                     Action_All_Empty := False;

                     Action_Names (RHS_Index) := new String'(Name);
                     Indent_Line ("procedure " & Name);
                     Indent_Line (" (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
                     Indent_Line ("  Tree      : in out WisiToken.Syntax_Trees.Tree;");
                     Indent_Line ("  Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;");
                     Indent_Line ("  Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)");
                     Indent_Line ("is");

                     if Unref_User_Data or Unref_Tree or Unref_Nonterm or Unref_Tokens then
                        Indent_Start ("   pragma Unreferenced (");

                        if Unref_User_Data then
                           Put ((if Need_Comma then ", " else "") & "User_Data");
                           Need_Comma := True;
                        end if;
                        if Unref_Tree then
                           Put ((if Need_Comma then ", " else "") & "Tree");
                           Need_Comma := True;
                        end if;
                        if Unref_Nonterm then
                           Put ((if Need_Comma then ", " else "") & "Nonterm");
                           Need_Comma := True;
                        end if;
                        if Unref_Tokens then
                           Put ((if Need_Comma then ", " else "") & "Tokens");
                           Need_Comma := True;
                        end if;
                        Put_Line (");");
                     end if;

                     Indent_Line ("begin");
                     Indent := Indent + 3;

                     Indent_Line (Line);
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & ";");
                     New_Line;
                  end;
               end if;

               if Length (RHS.Check) > 0 and then not Is_Elisp (RHS.Check) then
                  declare
                     use Standard.Ada.Strings.Fixed;
                     Line : constant String := -RHS.Check;
                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Trimmed_Image (RHS_Index);

                     Unref_Lexer   : constant Boolean := 0 = Index (Line, "Lexer");
                     Unref_Nonterm : constant Boolean := 0 = Index (Line, "Nonterm");
                     Unref_Tokens  : constant Boolean := 0 = Index (Line, "Tokens");
                  begin
                     Check_All_Empty := False;

                     Check_Names (RHS_Index) := new String'(Name & "_check");
                     Indent_Line ("function " & Name & "_check");
                     Indent_Line (" (Lexer   : access constant WisiToken.Lexer.Instance'Class;");
                     Indent_Line ("  Nonterm : in out WisiToken.Recover_Token;");
                     Indent_Line ("  Tokens  : in     WisiToken.Recover_Token_Array)");
                     Indent_Line (" return WisiToken.Semantic_Checks.Check_Status");
                     Indent_Line ("is");

                     if Unref_Lexer then
                        Indent_Line ("   pragma Unreferenced (Lexer);");
                     end if;
                     if Unref_Nonterm then
                        Indent_Line ("   pragma Unreferenced (Nonterm);");
                     end if;
                     if Unref_Tokens then
                        Indent_Line ("   pragma Unreferenced (Tokens);");
                     end if;

                     Indent_Line ("begin");
                     Indent := Indent + 3;
                     Indent_Line (Line);
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & "_check;");
                     New_Line;
                  end;
               end if;

               RHS_Index := RHS_Index + 1;
            end loop;

            if not Action_All_Empty then
               Ada_Action_Names (LHS_ID) := new Names_Array'(Action_Names);
            end if;
            if not Check_All_Empty then
               Ada_Check_Names (LHS_ID) := new Names_Array'(Check_Names);
            end if;
         end;
      end loop;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

   end Create_Ada_Actions_Body;

   procedure Create_Ada_Main_Body
     (Ada_Action_Names     : in Nonterminal_Names_Array;
      Ada_Check_Names      : in Nonterminal_Names_Array;
      Actions_Package_Name : in String;
      Main_Package_Name    : in String)
   is
      use Wisi.Utils;

      File_Name            : constant String := Output_File_Name_Root & "_main.adb";
      re2c_Package_Name    : constant String := -Data.Lower_Package_Name_Root & "_re2c_c";

      Body_File : File_Type;
   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Data.Table_Entry_Count := 0;

      Put_File_Header (Ada_Comment);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Put_Line ("with " & Actions_Package_Name & "; use " & Actions_Package_Name & ";");
      Put_Line ("with WisiToken.Lexer.re2c;");
      Put_Line ("with " & re2c_Package_Name & ";");
      Put_Line ("package body " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
      Indent_Line ("  (" & re2c_Package_Name & ".New_Lexer,");
      Indent_Line ("   " & re2c_Package_Name & ".Free_Lexer,");
      Indent_Line ("   " & re2c_Package_Name & ".Reset_Lexer,");
      Indent_Line ("   " & re2c_Package_Name & ".Next_Token);");
      New_Line;

      case Data.Generator_Algorithm is
      when LR_Generator_Algorithm =>
         Create_Create_Parser
           (Data, LR_Parsers, Data.Generator_Algorithm, None, Input_Data.Generate_Params.First_State_Index,
            Input_Data.Generate_Params.First_Parser_Label, Ada_Action_Names, Ada_Check_Names);

      when Packrat =>
         Create_Packrat_Parser (Data.Grammar, Ada_Action_Names, Ada_Check_Names);
      end case;

      Put_Line ("end " & Main_Package_Name & ";");
      Close (Body_File);
      Set_Output (Standard_Output);
   end Create_Ada_Main_Body;

   procedure Generate_LR1
   is begin
      LR_Parsers (LR1) := WisiToken.LR.LR1_Generator.Generate
        (Data.Grammar,
         Generate_Utils.LR1_Descriptor,
         WisiToken.State_Index (Input_Data.Generate_Params.First_State_Index),
         Generate_Utils.To_Conflicts
           (Input_Data.Lexer.File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
            Data.Reduce_Reduce_Conflict_Count),
         Generate_Utils.To_McKenzie_Param (Input_Data.McKenzie_Recover),
         Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
         Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);
   end Generate_LR1;

   procedure Generate_LALR
   is begin
      LR_Parsers (LALR) := WisiToken.LR.LALR_Generator.Generate
        (Data.Grammar,
         Generate_Utils.LALR_Descriptor,
         WisiToken.State_Index (Input_Data.Generate_Params.First_State_Index),
         Generate_Utils.To_Conflicts
           (Input_Data.Lexer.File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
            Data.Reduce_Reduce_Conflict_Count),
         Generate_Utils.To_McKenzie_Param (Input_Data.McKenzie_Recover),
         Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
         Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);
   end Generate_LALR;

   Ada_Action_Names : Nonterminal_Names_Array;
   Ada_Check_Names  : Nonterminal_Names_Array;

begin
   Common.Initialize (Data, Input_Data.Lexer.File_Name, Output_File_Name_Root, Check_Interface => False);

   case Data.Lexer is
   when re2c_Lexer =>
      null;

   when Elisp_Lexer =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_Data.Lexer.File_Name, 1, "Ada output language does not support " & Lexer_Names (Data.Lexer).all &
           " lexer");
   end case;

   case Input_Data.Generate_Params.Interface_Kind is
   when None  =>
      null;

   when Module | Process =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_Data.Lexer.File_Name, 1, "Ada output language does not support setting Interface");
   end case;

   case Data.Generator_Algorithm is
   when LALR_LR1 =>
      Generate_LALR;
      Generate_LR1;

      Data.Parser_State_Count := LR_Parsers (LR1).State_Last - LR_Parsers (LR1).State_First + 1;

   when LALR =>
      Generate_LALR;

      Data.Parser_State_Count := LR_Parsers (LALR).State_Last - LR_Parsers (LALR).State_First + 1;

   when LR1 =>
      Generate_LR1;

      Data.Parser_State_Count := LR_Parsers (LR1).State_Last - LR_Parsers (LR1).State_First + 1;

   when Packrat =>
      --  FIXME: check for left recursive, optimize memoizing, etc.
      Data.Parser_State_Count := 0;
   end case;

   declare
      Main_Package_Name    : constant String := -Data.Package_Name_Root & "_Main";
      Actions_Package_Name : constant String := -Data.Package_Name_Root & "_Actions";
   begin
      if Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0 then
         --  Some WisiToken tests have no actions or checks.
         Create_Ada_Actions_Body (Ada_Action_Names, Ada_Check_Names, Actions_Package_Name);
      end if;

      Create_Ada_Actions_Spec
        (Output_File_Name_Root & "_actions.ads", Actions_Package_Name,
         Generate_Utils.LR1_Descriptor, Input_Data.Generate_Params.Language_Runtime, Ada_Action_Names, Ada_Check_Names,
         Input_Data.Action_Count > 0, Input_Data.Check_Count > 0);

      Create_Ada_Main_Body (Ada_Action_Names, Ada_Check_Names, Actions_Package_Name, Main_Package_Name);

      Create_Ada_Main_Spec
        (Output_File_Name_Root & "_main.ads", Main_Package_Name,
         Data.Generator_Algorithm, Ada, None);
   end;

   Create_re2c (Output_File_Name_Root, Input_Data.Elisp_Names.Regexps);

   Put_Line
     (Integer'Image (Input_Data.Rule_Count) & " rules," &
        Integer'Image (Input_Data.Action_Count) & " actions," &
        Integer'Image (Input_Data.Check_Count) & " checks," &
        WisiToken.State_Index'Image (Data.Parser_State_Count) & " states," &
        Integer'Image (Data.Table_Entry_Count) & " table entries");
   Put_Line
     (Integer'Image (Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
        Integer'Image (Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
        Integer'Image (Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada;
