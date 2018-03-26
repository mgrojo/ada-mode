--  Abstract :
--
--  Generic Emacs background process; parse token stream, return
--  parser actions.
--
--  See gen_run_wisi_parse.ads for a standalone version.
--
--  References :
--
--  [1] On the elisp side, the inter-process protocol is defined in
--  wisi-process-parse.el, functions wisi-process-parse--send-parse
--  and wisi-process-parse--execute.
--
--  [2] On the Ada side, it is defined here, and in
--  wisitoken-token_emacs_process.adb
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Runtime;
generic
   Name : in String; --  for Usage, error messages. "_wisi_parse" will be appended

   Descriptor           : in WisiToken.Descriptor;
   Parse_Data           : in out WisiToken.Wisi_Runtime.Parse_Data_Type'Class;
   Semantic_Check_Fixes : in WisiToken.LR.Semantic_Check_Fixes_Access;

   with procedure Create_Parser
     (Parser               :    out          WisiToken.LR.Parser.Parser;
      Algorithm            : in              WisiToken.Parser_Algorithm_Type;
      Trace                : not null access WisiToken.Trace'Class;
      Semantic_Check_Fixes : in              WisiToken.LR.Semantic_Check_Fixes_Access);

procedure Gen_Emacs_Wisi_Parse;
