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
with Wisi.Generate_Utils;
with Wisi.Output_Elisp_Common;
with WisiToken.Generate.Packrat;
with WisiToken.Generate;
with WisiToken.LR.Wisi_Generate_Elisp;
with WisiToken.Wisi_Grammar_Runtime;
procedure Wisi.Output_Elisp
  (Input_Data    :         in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
   Elisp_Package :         in String;
   Generate_Data : aliased in Wisi.Generate_Utils.Generate_Data;
   Packrat_Data  :         in WisiToken.Generate.Packrat.Data;
   Tuple         :         in Generate_Tuple)
is
   pragma Unreferenced (Packrat_Data);

   procedure Create_Elisp (Algorithm : in LR_Generate_Algorithm)
   is
      use Standard.Ada.Strings.Unbounded;
      File            : File_Type;
      Elisp_Package_1 : constant String :=
        (case Algorithm is
         when LALR => Elisp_Package & "-lalr",
         when LR1  => Elisp_Package & "-lr1");
   begin
      Create (File, Out_File, Elisp_Package_1 & "-elisp.el");
      Set_Output (File);

      Put_Line (";;; " & Elisp_Package_1 & "-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-");
      Put_Command_Line (Elisp_Comment & "  ", Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Copyright_License));
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      New_Line;

      Put_Line ("(require 'wisi)");
      Put_Line ("(require 'wisi-compile)");
      Put_Line ("(require 'wisi-elisp-parse)");
      New_Line;
      Output_Elisp_Common.Indent_Keyword_Table
        (Elisp_Package_1, "elisp", Input_Data.Tokens.Keywords, To_String'Access);
      New_Line;
      Output_Elisp_Common.Indent_Token_Table (Elisp_Package_1, "elisp", Input_Data.Tokens.Tokens, To_String'Access);
      New_Line;
      WisiToken.LR.Wisi_Generate_Elisp.Output
        (Elisp_Package_1, Input_Data.Tokens, Generate_Data.LR_Parse_Table, Generate_Data.Descriptor.all);
      New_Line;
      Put_Line ("(provide '" & Elisp_Package_1 & "-elisp)");
      Put_Line (";; end of file");
      Close (File);

      Set_Output (Standard_Output);
   end Create_Elisp;

begin
   Create_Elisp (Tuple.Gen_Alg);

   if WisiToken.Trace_Generate > 0 then
      Wisi.Generate_Utils.Put_Stats (Input_Data, Generate_Data);
   end if;
end Wisi.Output_Elisp;
