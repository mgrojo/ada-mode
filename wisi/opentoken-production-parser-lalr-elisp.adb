--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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
package body OpenToken.Production.Parser.LALR.Elisp is

   procedure Header (Elisp_Package : in String)
   is begin
      Put_Line (";;; " & Elisp_Package & ".el --- Generated parser support file");
      New_Line;
      Put_Line (";; Copyright (C) 2012 Free Software Foundation, Inc.");
      New_Line;
      Put_Line (";; This program is free software; you can redistribute it and/or");
      Put_Line (";; modify it under the terms of the GNU General Public License as");
      Put_Line (";; published by the Free Software Foundation; either version 2, or (at");
      Put_Line (";; your option) any later version.");
      Put_Line (";;");
      Put_Line (";; This software is distributed in the hope that it will be useful,");
      Put_Line (";; but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line (";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU");
      Put_Line (";; General Public License for more details.");
      Put_Line (";;");
      Put_Line (";; You should have received a copy of the GNU General Public License");
      Put_Line (";; along with GNU Emacs; see the file COPYING.  If not, write to the");
      Put_Line (";; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,");
      Put_Line (";; Boston, MA 02110-1301, USA.");
      New_Line;
      Put_Line (";; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically");
      Put_Line (";; generated from the grammar file ada-subset.wy.");
      New_Line;
   end Header;

   procedure Keyword_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List)
   is
      use Wisi; -- "-" unbounded_string
   begin
      Put_Line ("(defconst " & Elisp_Package & "--keyword-table");
      Put_Line ("  (semantic-lex-make-keyword-table");
      Put_Line ("   '(");
      for Pair of Keywords loop
         Put_Line ("    (""" & (-Pair.Value) & """ . " & (-Pair.Name) & ")");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language keywords."")");
   end Keyword_Table;

   procedure Token_Table
     (Elisp_Package : in String;
      Tokens      : in Wisi.String_Pair_Lists.List)
   is
      use Wisi; -- "-" unbounded_string
   begin
      Put_Line ("(defconst " & Elisp_Package & "--token-table");
      Put_Line ("  (semantic-lex-make-token-table");
      Put_Line ("   '(");
      for Pair of Tokens loop
         Put_Line ("    (""" & (-Pair.Value) & """ (" & (-Pair.Name) & "))");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language tokens."")");
   end Token_Table;

   procedure Parse_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Instance)
   is
      pragma Unreferenced (Parser);
      use Wisi; -- "-" unbounded_string
   begin
      Put_Line ("(defconst " & Elisp_Package & "--parse-table");
      Put_Line ("   (wisi-compile-grammar");
      --  terminal tokens
      Put ("   '((");
      for Pair of Keywords loop
         Put (-Pair.Name & " ");
      end loop;
      Put_Line (")");

      --  nonterminal productions
      for Rule of Rules loop
         Put_Line ("(" & (-Rule.Left_Hand_Side));
         Put ("(");
         for RHS of Rule.Right_Hand_Sides loop
            Put ("(");
            for Token of RHS.Production loop
               Put (Token & " ");
            end loop;
            Put_Line (")");
            for Line of RHS.Action loop
               Put_Line (Line);
            end loop;
         end loop;
         Put_Line (")");
      end loop;

--  FIXME:       Action_Table (Parser);
--      Goto_Table (Parser);
      Put_Line (")");

      Put_Line ("  ""Parser table."")");
   end Parse_Table;

   procedure Output
     (Elisp_Package : in String;
      Prologue      : in String;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Tokens        : in Wisi.String_Pair_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Instance)
   is
      File : File_Type;
   begin
      Create (File, Out_File, Elisp_Package & "-wy.el");
      Set_Output (File);
      Header (Elisp_Package);
      Put_Line (Prologue);
      Put_Line ("(require 'semantic-lex)");
      New_Line;
      Keyword_Table (Elisp_Package, Keywords);
      New_Line;
      Token_Table (Elisp_Package, Tokens);
      New_Line;
      Parse_Table (Elisp_Package, Keywords, Rules, Parser);
      New_Line;
      Put_Line ("(provide '" & Elisp_Package & ")");
      New_Line;
      Put_Line (";; end of file");
      Close (File);
   end Output;

end OpenToken.Production.Parser.LALR.Elisp;
