--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The grammar parser
--  actions must be Ada.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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
with WisiToken.LR.LR1_Generator;
with WisiToken.LR.LALR_Generator;
with Wisi.Gen_Output_Ada_Common;
with Wisi.Utils;
procedure Wisi.Output_Ada
  (Input_File_Name       : in String;
   Output_File_Name_Root : in String;
   Language_Name         : in String;
   Params                : in Generate_Param_Type;
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
   package Common is new Wisi.Gen_Output_Ada_Common (Prologues, Tokens, Conflicts, Params);
   use Common;

   procedure Create_Ada_Body
   is
      use all type WisiToken.LR.Unknown_State_Index;
      use Generate_Utils;
      use Wisi.Utils;

      File_Name               : constant String := Output_File_Name_Root & ".adb";
      Package_Name            : constant String := -Data.Package_Name_Root;
      Lower_Package_Name_Root : constant String := -Data.Lower_Package_Name_Root;

      Body_File    : File_Type;
   begin
      if Data.Parser_Algorithm in LALR | LALR_LR1 then
         Parsers (LALR) := WisiToken.LR.LALR_Generator.Generate
           (Data.Grammar,
            LALR_Descriptor,
            WisiToken.LR.State_Index (Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
               Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := Parsers (LALR).State_Last - Parsers (LALR).State_First + 1;
      end if;

      if Data.Parser_Algorithm in LR1 | LALR_LR1 then
         Parsers (LR1) := WisiToken.LR.LR1_Generator.Generate
           (Data.Grammar,
            LR1_Descriptor,
            WisiToken.LR.State_Index (Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_File_Name, Data.Accept_Reduce_Conflict_Count, Data.Shift_Reduce_Conflict_Count,
               Data.Reduce_Reduce_Conflict_Count),
            Generate_Utils.To_McKenzie_Param (McKenzie_Recover),
            Trace                    => Verbosity > 1,
            Put_Parse_Table          => Verbosity > 0,
            Ignore_Unused_Tokens     => Verbosity > 1,
            Ignore_Unknown_Conflicts => Verbosity > 1);

         Data.Parser_State_Count := WisiToken.LR.Unknown_State_Index'Max
           (Data.Parser_State_Count,
            Parsers (LR1).State_Last - Parsers (LR1).State_First + 1);
      end if;

      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Data.Table_Entry_Count := 0;
      Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Prologue (Ada_Comment, Prologues.Body_Context_Clause);
      New_Line;

      Put_Line ("with WisiToken.Lexer.re2c;");
      Put_Line ("with WisiToken.LR.Parser;");
      if Check_Count > 0 then
         Put_Line ("with WisiToken.LR.Semantic_Checks; use WisiToken.LR.Semantic_Checks;");
      end if;
      Put_Line ("with " & Lower_Package_Name_Root & "_re2c_c;");
      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Put_Prologue (Ada_Comment, Prologues.Body_Declarations);

      Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
      Indent_Line ("  (" & Lower_Package_Name_Root & "_re2c_c.New_Lexer,");
      Indent_Line ("   " & Lower_Package_Name_Root & "_re2c_c.Free_Lexer,");
      Indent_Line ("   " & Lower_Package_Name_Root & "_re2c_c.Reset_Lexer,");
      Indent_Line ("   " & Lower_Package_Name_Root & "_re2c_c.Next_Token);");
      New_Line;

      --  generate Action and Check subprograms, populate Ada_Action_Names.
      --  Ada_Check_Names.

      for Rule of Tokens.Rules loop
         --  No need for a Token_Cursor here, since we only need the
         --  nonterminals.
         declare
            use all type Standard.Ada.Containers.Count_Type;

            LHS_ID : constant WisiToken.Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);

            Action_Names     : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Check_Names      : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Prod_Index       : Integer := 0; -- Semantic_Action defines Prod_Index as zero-origin
            Action_All_Empty : Boolean := True;
            Check_All_Empty  : Boolean := True;

            function Is_Elisp (Action : in String_Lists.List) return Boolean
            is
               Line : constant String := String_Lists.Element (Action.First);
            begin
               return Line'Length >= 5 and then
                 (Line (Line'First .. Line'First + 4) = "progn" or
                    Line (Line'First .. Line'First + 4) = "wisi-");
            end Is_Elisp;

         begin
            for RHS of Rule.Right_Hand_Sides loop
               if RHS.Action.Length > 0 and then not Is_Elisp (RHS.Action) then
                  declare
                     use Standard.Ada.Strings.Fixed;

                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Int_Image (Prod_Index);

                     Unref_Nonterm : Boolean := True;
                     Unref_Tokens  : Boolean := True;
                  begin
                     for Line of RHS.Action loop
                        if 0 < Index (Line, "Nonterm") then
                           Unref_Nonterm := False;
                        end if;
                        if 0 < Index (Line, "Tokens") then
                           Unref_Tokens := False;
                        end if;
                     end loop;

                     Action_All_Empty := False;

                     Action_Names (Prod_Index) := new String'(Name & "'Access");
                     Indent_Line ("procedure " & Name);
                     Indent_Line (" (Nonterm : in WisiToken.Semantic_State.Augmented_Token'Class;");
                     Indent_Line ("  Tokens  : in WisiToken.Semantic_State.Augmented_Token_Array)");
                     Indent_Line ("is");

                     if Unref_Nonterm then
                        Indent_Line ("   pragma Unreferenced (Nonterm);");
                     end if;
                     if Unref_Tokens then
                        Indent_Line ("   pragma Unreferenced (Tokens);");
                     end if;

                     Indent_Line ("begin");
                     Indent := Indent + 3;

                     for Line of RHS.Action loop
                        Indent_Line (Line);
                     end loop;
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & ";");
                     New_Line;
                  end;
               end if;

               if RHS.Check.Length > 0 and then not Is_Elisp (RHS.Check) then
                  declare
                     use Standard.Ada.Strings.Fixed;

                     Name : constant String := -Rule.Left_Hand_Side & '_' & WisiToken.Int_Image (Prod_Index);

                     Unref_Lexer   : Boolean := True;
                     Unref_Nonterm : Boolean := True;
                     Unref_Tokens  : Boolean := True;
                  begin
                     for Line of RHS.Check loop
                        if 0 < Index (Line, "Lexer") then
                           Unref_Lexer := False;
                        end if;
                        if 0 < Index (Line, "Nonterm") then
                           Unref_Nonterm := False;
                        end if;
                        if 0 < Index (Line, "Tokens") then
                           Unref_Tokens := False;
                        end if;
                     end loop;

                     Check_All_Empty := False;

                     Check_Names (Prod_Index) := new String'(Name & "_check'Access");
                     Indent_Line ("function " & Name & "_check");
                     Indent_Line (" (Lexer   : in     WisiToken.Lexer.Handle;");
                     Indent_Line ("  Nonterm : in out WisiToken.Base_Token;");
                     Indent_Line ("  Tokens  : in     WisiToken.Base_Token_Arrays.Vector)");
                     Indent_Line (" return WisiToken.LR.Semantic_Status");
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

                     for Line of RHS.Check loop
                        Indent_Line (Line);
                     end loop;
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & "_check;");
                     New_Line;
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

      Create_Create_Parser (Data.Parser_Algorithm, None, Params.First_State_Index, Params.First_Parser_Label);

      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

      Put_Line
        (Integer'Image (Rule_Count) & " rules," &
           Integer'Image (Action_Count) & " actions," &
           Integer'Image (Check_Count) & " checks," &
           WisiToken.LR.State_Index'Image (Data.Parser_State_Count) & " states," &
           Integer'Image (Data.Table_Entry_Count) & " table entries");
      Put_Line
        (Integer'Image (Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
   end Create_Ada_Body;

begin
   Common.Initialize (Input_File_Name, Output_File_Name_Root, Check_Interface => False);

   case Data.Lexer is
   when re2c_Lexer =>
      null;

   when Elisp_Lexer =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_File_Name, 1, "Ada output language does not support " & Lexer_Names (Data.Lexer).all & " lexer");
   end case;

   case Params.Interface_Kind is
   when None  =>
      null;

   when Module | Process =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_File_Name, 1, "Ada output language does not support setting Interface");
   end case;

   Create_Ada_Spec
     (Input_File_Name, Output_File_Name_Root & ".ads", -Data.Package_Name_Root, Language_Name,
      Ada, Generate_Utils.LR1_Descriptor, None, Declare_Enum);

   Create_Ada_Body;

   Create_re2c (Input_File_Name, Output_File_Name_Root, Elisp_Names.Regexps);

exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada;
--  Local Variables:
--  jit-lock-defer-time: 0.25
--  End:
