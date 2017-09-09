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
with WisiToken.Parser.LR.LR1_Generator;
with WisiToken.Parser.LR.LALR_Generator;
with Wisi.Gen_Output_Ada_Common;
with Wisi.Utils;
procedure Wisi.Output_Ada
  (Input_File_Name         : in String;
   Output_File_Name_Root   : in String;
   Params                  : in Generate_Param_Type;
   Prologue_Context_Clause : in String_Lists.List;
   Prologue_Declarations   : in String_Lists.List;
   Keywords                : in String_Pair_Lists.List;
   Tokens                  : in Token_Lists.List;
   Conflicts               : in Conflict_Lists.List;
   McKenzie_Recover        : in McKenzie_Recover_Param_Type;
   Rules                   : in Rule_Lists.List;
   Rule_Count              : in Integer;
   Action_Count            : in Integer;
   Profile                 : in Boolean)
is
   function Ensure_Ada_Comment (Line : in String) return String
   is begin
      --  Translate Elisp comments to Ada comments.
      return Result : String := Line do
         if Line'Last >= 2 and then Line (Line'First .. Line'First + 1) = ";;" then
            Result (Line'First .. Line'First + 1) := "--";
         end if;
      end return;
   end Ensure_Ada_Comment;

   procedure Put_Ada_Prologue_Context_Clause
   is begin
      for Line of Prologue_Context_Clause loop
         Put_Line (Ensure_Ada_Comment (Line));
      end loop;
   end Put_Ada_Prologue_Context_Clause;

   procedure Put_Ada_Prologue_Declarations
   is begin
      for Line of Prologue_Declarations loop
         Wisi.Utils.Indent_Line (Ensure_Ada_Comment (Line));
      end loop;
   end Put_Ada_Prologue_Declarations;

   procedure Put_Aflex_Prologue
   is begin
      for Line of Prologue_Context_Clause loop
         if Line'Last >= 2 and then Line (1 .. 2) = "--" then
            Put_Line (Line);
         else
            return;
         end if;
      end loop;
   end Put_Aflex_Prologue;

   procedure Put_Quex_Prologue
   is begin
      for Line of Prologue_Context_Clause loop
         if Line'Last >= 2 and then Line (1 .. 2) = "--" then
            Put_Line ("//" & Line (2 .. Line'Last));
         else
            return;
         end if;
      end loop;
   end Put_Quex_Prologue;

   package Common is new Wisi.Gen_Output_Ada_Common
     (Keywords, Tokens, Conflicts, Rules, Params, Put_Ada_Prologue_Context_Clause,
      Put_Ada_Prologue_Declarations, Put_Aflex_Prologue, Put_Quex_Prologue);
   use Common;

   procedure Create_Ada_Body
   is
      use all type WisiToken.Parser.LR.Unknown_State_Index;
      use Generate_Utils;
      use Wisi.Utils;

      File_Name               : constant String := Output_File_Name_Root & ".adb";
      Package_Name            : constant String := -Data.Package_Name_Root;
      Lower_Package_Name_Root : constant String := -Data.Lower_Package_Name_Root;

      Body_File    : File_Type;
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
      Data.Table_Entry_Count := 0;
      Put_Line ("--  generated by WisiToken Wisi from " & Input_File_Name);
      Put_Command_Line  ("--  ");
      Put_Line ("--");
      Put_Ada_Prologue_Context_Clause;
      New_Line;

      case Data.Lexer is
      when Aflex_Lexer =>
         Put_Line ("with WisiToken.Lexer.Aflex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_YYLex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_dfa;");
         Put_Line ("with " & Lower_Package_Name_Root & "_io;");

      when Quex_Lexer =>
         Put_Line ("with WisiToken.Lexer.Quex;");
         Put_Line ("with " & Lower_Package_Name_Root & "_quex_c;");

      when Elisp_Lexer | Regexp_Lexer =>
         --  could support regexp_lexer
         raise Programmer_Error;
      end case;

      Put_Line ("with WisiToken.Token;");

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Data.Lexer is
      when Aflex_Lexer =>
         Indent_Line ("package Lexer is new WisiToken.Lexer.Aflex");
         Indent_Line ("  (" & Lower_Package_Name_Root & "_io.Feeder,");
         Indent := Indent + 3;
         Indent_Line (Lower_Package_Name_Root & "_YYLex,");
         Indent_Line (Lower_Package_Name_Root & "_dfa.YYText,");
         Indent_Line (Lower_Package_Name_Root & "_dfa.yytext_ptr,");
         Indent_Line (Lower_Package_Name_Root & "_dfa.YYLength,");
         Indent_Line (Lower_Package_Name_Root & "_dfa.Set_Buffer_Size,");
         Indent_Line (Lower_Package_Name_Root & "_io.Tok_Begin_Line,");
         Indent_Line (Lower_Package_Name_Root & "_io.Tok_Begin_Col,");
         Indent_Line (Lower_Package_Name_Root & "_dfa.yy_init);");
         Indent := Indent - 3;
         New_Line;

      when Quex_Lexer =>
         Indent_Line ("pragma Linker_Options");
         Indent_Line ("  (""" & Lower_Package_Name_Root & "_quex.o " &
                        Lower_Package_Name_Root & "_lexer.o -liconv"");");

         Indent_Line ("package Lexer is new WisiToken.Lexer.Quex");
         Indent_Line ("  (" & Lower_Package_Name_Root & "_quex_c.New_Lexer_From_Buffer,");
         Indent_Line ("   " & Lower_Package_Name_Root & "_quex_c.Free_Lexer,");
         Indent_Line ("   " & Lower_Package_Name_Root & "_quex_c.Next_Token);");
         New_Line;

      when Elisp_Lexer | Regexp_Lexer =>
         --  could support regexp_lexer
         raise Programmer_Error;
      end case;

      if Action_Count = 0 then
         null;

      else
         --  generate Action subprograms, populate Action_Names.

         if Profile then
            --  FIXME: get token_id_last from descriptor
            Indent_Line ("Action_Counts : array (fasttoken_accept_ID .. Token_ID'Last) of Integer := (others => 0);");
         end if;

         for Rule of Rules loop
            declare
               use all type Standard.Ada.Containers.Count_Type;

               LHS_ID     : constant WisiToken.Token_ID := Find_Token_ID (-Rule.Left_Hand_Side);
               Prod_Index : Integer                     := 0; -- Semantic_Action defines Prod_Index as zero-origin
               Temp       : Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
               All_Empty  : Boolean                     := True;

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
                        Unref_Index   : Boolean := True;
                        Unref_Source  : Boolean := True;
                     begin
                        for Line of RHS.Action loop
                           if 0 < Index (Line, "Nonterm") then
                              Unref_Nonterm := False;
                           end if;
                           if 0 < Index (Line, "Index") then
                              Unref_Index := False;
                           end if;
                           if 0 < Index (Line, "Source") then
                              Unref_Source := False;
                           end if;
                        end loop;

                        All_Empty := False;

                        Temp (Prod_Index) := new String'(Name & "'Access");
                        Indent_Line ("procedure " & Name);
                        Indent_Line (" (Nonterm : in WisiToken.Augmented_Token'Class;");
                        Indent_Line ("  Index   : in Natural;");
                        Indent_Line ("  Source  : in WisiToken.Augmented_Token_Array)");
                        Indent_Line ("is");

                        if Profile or Unref_Nonterm then
                           Indent_Line ("   pragma Unreferenced (Nonterm);");
                        end if;
                        if Profile or Unref_Index then
                           Indent_Line ("   pragma Unreferenced (Index);");
                        end if;
                        if Profile or Unref_Source then
                           Indent_Line ("   pragma Unreferenced (Source);");
                        end if;

                        Indent_Line ("begin");
                        Indent := Indent + 3;

                        if Profile then
                           Indent_Line ("Action_Counts (Nonterm) := Action_Counts (Nonterm) + 1;");

                        else
                           for Line of RHS.Action loop
                              Indent_Line (Line);
                           end loop;
                        end if;
                        Indent := Indent - 3;
                        Indent_Line ("end " & Name & ";");
                        New_Line;
                     end;
                  end if;

                  Prod_Index := Prod_Index + 1;
               end loop;

               if not All_Empty then
                  Ada_Action_Names (LHS_ID) := new Action_Name_List'(Temp);
               end if;
            end;
         end loop;
      end if;

      Create_Create_Parser
        (Data.Parser_Algorithm, Data.Lexer, None, Params.First_State_Index, Params.First_Parser_Label);

      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

      Put_Line
        (Integer'Image (Rule_Count) & " rules," &
           Integer'Image (Action_Count) & " actions," &
           WisiToken.Parser.LR.State_Index'Image (Data.Parser_State_Count) & " states," &
           Integer'Image (Data.Table_Entry_Count) & " table entries");
      Put_Line
        (Integer'Image (Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
   end Create_Ada_Body;

begin
   Common.Initialize (Input_File_Name, Output_File_Name_Root, Check_Interface => False);

   case Data.Lexer is
   when Aflex_Lexer | Regexp_Lexer | Quex_Lexer =>
      null;

   when Elisp_Lexer  =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_File_Name, 1, "Ada output language does not support Elisp lexer");
   end case;

   case Params.Interface_Kind is
   when None  =>
      null;

   when Module | Process =>
      raise User_Error with Wisi.Utils.Error_String
        (Input_File_Name, 1, "Ada output language does not support setting Interface");
   end case;

   Create_Ada_Spec
     (Input_File_Name, Output_File_Name_Root & ".ads", -Data.Package_Name_Root,
      Ada, Generate_Utils.LR1_Descriptor, Process, Params.Lexer);

   Create_Ada_Body;

   case Data.Lexer is
   when Aflex_Lexer =>
      Create_Aflex (Input_File_Name, Output_File_Name_Root);

   when Quex_Lexer =>
      Create_Quex (Input_File_Name, Output_File_Name_Root);

   when Elisp_Lexer | Regexp_Lexer =>
      --  could support regexp_lexer
      raise Programmer_Error;
   end case;

exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada;
--  Local Variables:
--  jit-lock-defer-time: 0.25
--  End:
