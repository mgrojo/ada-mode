--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.LR1_Generator;
with WisiToken.LR.Wisi_Generate_Elisp;
with WisiToken.Productions;
with Wisi.Gen_Generate_Utils;
with Wisi.Output_Elisp_Common;
with WisiToken.Wisi_Grammar_Runtime;
procedure Wisi.Output_Elisp
  (Input_Data    : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
   Elisp_Package : in String)
is
   EOI_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"Wisi_EOI";
   --  See comments in wisi-output_ada_emacs.adb EOI_Name for what
   --  this must match.

   WisiToken_Accept_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"wisitoken_accept";

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Input_Data.Tokens, Input_Data.Conflicts, EOI_Name, WisiToken_Accept_Name);

   Accept_Reduce_Conflict_Count : Integer;
   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant WisiToken.Productions.Prod_Arrays.Vector := Generate_Utils.To_Grammar
     (Generate_Utils.LR1_Descriptor, Input_Data.Lexer.File_Name, -Input_Data.Generate_Params.Start_Token);

   Parser : WisiToken.LR.Parse_Table_Ptr;

   procedure Create_Elisp (Algorithm : in LR_Single_Generator_Algorithm; Both : in Boolean)
   is
      use Standard.Ada.Strings.Unbounded;
      File            : File_Type;
      Elisp_Package_1 : Unbounded_String;
   begin
      if Both then
         case Algorithm is
         when LALR =>
            Elisp_Package_1 := +Elisp_Package & "-lalr";
         when LR1 =>
            Elisp_Package_1 := +Elisp_Package & "-lr1";
         end case;
      else
         Elisp_Package_1 := +Elisp_Package;
      end if;

      WisiToken.LR.Free_Table (Parser);

      case Algorithm is
      when LALR                      =>
         Parser := WisiToken.LR.LALR_Generator.Generate
           (Grammar,
            Generate_Utils.LALR_Descriptor,
            WisiToken.State_Index (Input_Data.Generate_Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_Data.Lexer.File_Name, Accept_Reduce_Conflict_Count, Shift_Reduce_Conflict_Count,
               Reduce_Reduce_Conflict_Count),
            Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
            Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);

      when LR1                       =>
         Parser := WisiToken.LR.LR1_Generator.Generate
           (Grammar,
            Generate_Utils.LR1_Descriptor,
            WisiToken.State_Index (Input_Data.Generate_Params.First_State_Index),
            Generate_Utils.To_Conflicts
              (Input_Data.Lexer.File_Name, Accept_Reduce_Conflict_Count, Shift_Reduce_Conflict_Count,
               Reduce_Reduce_Conflict_Count),
            Ignore_Unused_Tokens     => WisiToken.Trace_Generate > 1,
            Ignore_Unknown_Conflicts => WisiToken.Trace_Generate > 1);
      end case;

      Create (File, Out_File, -Elisp_Package_1 & "-elisp.el");
      Set_Output (File);

      Put_Line (";;; " & (-Elisp_Package_1) & "-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-");
      Put_Command_Line (Elisp_Comment & "  ");
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Copyright_License));
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      New_Line;

      Put_Line ("(require 'wisi)");
      Put_Line ("(require 'wisi-compile)");
      Put_Line ("(require 'wisi-elisp-parse)");
      New_Line;
      Output_Elisp_Common.Indent_Keyword_Table
        (-Elisp_Package_1, "elisp", Input_Data.Tokens.Keywords, To_String'Access);
      New_Line;
      Output_Elisp_Common.Indent_Token_Table (-Elisp_Package_1, "elisp", Input_Data.Tokens.Tokens, To_String'Access);
      New_Line;
      WisiToken.LR.Wisi_Generate_Elisp.Output
        (-Elisp_Package_1, Input_Data.Tokens, Parser, Generate_Utils.LR1_Descriptor);
      New_Line;
      Put_Line ("(provide '" & (-Elisp_Package_1) & "-elisp)");
      Put_Line (";; end of file");
      Close (File);

      Set_Output (Standard_Output);
   end Create_Elisp;

   use all type WisiToken.Unknown_State_Index;
begin
   case LR_Generator_Algorithm (Input_Data.Generate_Params.Generator_Algorithm) is
   when LALR | LR1 =>
      Create_Elisp (Input_Data.Generate_Params.Generator_Algorithm, Both => False);
   when LALR_LR1 =>
      Create_Elisp (LALR, Both => True);
      Create_Elisp (LR1, Both => True);
   end case;

   if WisiToken.Trace_Generate > 0 then
      --  Match wisi-output_ada.adb, wisi-output_ada_emacs.adb format
      Put_Line
        (Integer'Image (Input_Data.Rule_Count) & " rules," &
           Integer'Image (Input_Data.Action_Count) & " actions," &
           WisiToken.State_Index'Image (Parser.State_Last - Parser.State_First + 1) & " states");
      Put_Line
        (Integer'Image (Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
   end if;
end Wisi.Output_Elisp;
