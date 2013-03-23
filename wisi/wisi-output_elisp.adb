--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
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

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR.Elisp;
with OpenToken.Production.Print;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.Nonterminal;
with Wisi.Utils;
procedure Wisi.Output_Elisp
  (Elisp_Package : in String;
   Copyright     : in String;
   Prologue      : in String_Lists.List;
   Keywords      : in String_Pair_Lists.List;
   Tokens        : in Token_Lists.List;
   Start_Token   : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Rules         : in Rule_Lists.List)
is
   subtype Token_IDs is Integer range
     1 .. Count (Tokens) + Integer (Keywords.Length) + 1 + Integer (Rules.Length) + 1;
   --  one extra terminal for $EOI
   --  one extra non-terminal for the OpenToken accept symbol followed by EOI.
   --  don't need a whitespace token; handled by Elisp Wisi lexer

   function Count_Non_Reporting return Integer
   is
      Result : Integer := 0;
   begin
      for Kind of Tokens loop
         if -Kind.Kind = """line_comment""" then
            Result := Result + Integer (Kind.Tokens.Length);
         end if;
      end loop;
      return Result;
   end Count_Non_Reporting;

   Token_Count    : constant Token_IDs := Count (Tokens);
   First_Terminal : constant Token_IDs := Token_IDs'First + Count_Non_Reporting;
   EOI_ID         : constant Token_IDs := Token_Count + Token_IDs (Keywords.Length) + 1; -- last terminal
   Accept_ID      : constant Token_IDs := Token_IDs'Last;                                -- last nonterminal

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_IDs
   is
      use type Standard.Ada.Strings.Unbounded.Unbounded_String;
      Result : Token_IDs := Token_IDs'First;
   begin
      for Kind of Tokens loop
         for Pair of Kind.Tokens loop
            if Pair.Name = Token then
               return Result;
            end if;
            Result := Result + 1;
         end loop;
      end loop;
      for Pair of Keywords loop
         if Pair.Name = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
      Result := Result + 1; -- EOI

      for Rule of Rules loop
         if Rule.Left_Hand_Side = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   Token_Image_Width : Integer := 0;

   type ID_Array_Access_String_Type is array (Token_IDs) of access constant String;

   function Set_Token_Images return ID_Array_Access_String_Type
   is
      ID           : Token_IDs := Token_IDs'First;
      Token_Images : ID_Array_Access_String_Type;
   begin
      for Kind of Tokens loop
         for Pair of Kind.Tokens loop
            Token_Images (ID) := new String'(-Pair.Name);
            ID := ID + 1;
         end loop;
      end loop;

      if ID /= Token_Count + 1 then raise Programmer_Error; end if;

      for Pair of Keywords loop
         Token_Images (ID) := new String'(-Pair.Name);
         ID := ID + 1;
      end loop;

      if ID /= EOI_ID then raise Programmer_Error; end if;

      Token_Images (ID) := new String'("$EOI"); -- match wisent-eoi-term
      ID                := ID + 1;

      for Rule of Rules loop
         Token_Images (ID) := new String'(-Rule.Left_Hand_Side);
         ID := ID + 1;
      end loop;

      if ID /= Accept_ID then raise Programmer_Error; end if;

      Token_Images (ID) := new String'("opentoken_accept");

      for Token of Token_Images loop
         if Token.all'Length > Token_Image_Width then
            Token_Image_Width := Token.all'Length;
         end if;
      end loop;

      return Token_Images;
   end Set_Token_Images;

   Token_Images : constant ID_Array_Access_String_Type := Set_Token_Images;

   function Token_Image (ID : in Token_IDs) return String
   is begin
      return Token_Images (ID).all;
   end Token_Image;

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_Image, Token_Image_Width);
   --  we only need Analyzers to instantiate Parsers, but we might call it for debugging
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => First_Terminal, Last_Terminal => EOI_ID);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALR_Parsers is new Parsers.LALR;

   package Parser_Elisp is new LALR_Parsers.Elisp (Token_Image);

   Grammar : Production_Lists.Instance;
   Parser  : LALR_Parsers.Instance;
   File    : Standard.Ada.Text_IO.File_Type;

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "&" (Tokens : in Token_Lists.Instance; Token : in String) return Token_Lists.Instance
   is begin
      return Tokens & Tokens_Pkg.Get (Find_Token_ID (Token));
   end "&";

   procedure Header (Elisp_Package : in String; Copyright : in String)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line (";;; " & Elisp_Package & "-wy.el --- Generated parser support file");
      New_Line;
      Put_Line (";; Copyright (C) " & Copyright);
      New_Line;
      --  FIXME: allow other license
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
      Put_Line (";; generated from the grammar file " & Elisp_Package & ".wy");
      New_Line;
   end Header;

   procedure Keyword_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--keyword-table");
      Put_Line ("  (semantic-lex-make-keyword-table");
      Put_Line ("   '(");
      for Pair of Keywords loop
         Put_Line ("    (" & (-Pair.Value) & " . " & (-Pair.Name) & ")");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language keywords."")");
   end Keyword_Table;

   procedure Token_Table
     (Elisp_Package : in String;
      Tokens        : in Wisi.Token_Lists.List)
   is
      use Standard.Ada.Strings.Unbounded; -- length
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--token-table");
      Put_Line ("  (semantic-lex-make-type-table");
      Put_Line ("   '(");
      for Kind of Tokens loop
         Put_Line ("     (" & (-Kind.Kind));
         for Token of Kind.Tokens loop
            if 0 = Length (Token.Value) then
               Put_Line ("      (" & (-Token.Name) & ")");
            else
               Put_Line ("      (" & (-Token.Name) & " . " & (-Token.Value) & ")");
            end if;
         end loop;
         Put_Line ("     )");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language tokens."")");
   end Token_Table;

begin
   if Verbosity > 0 then
      declare
         use Standard.Ada.Text_IO;
      begin
         Put_Line ("Tokens:");
         for I in Token_IDs'Range loop
            Put_Line (Token_IDs'Image (I) & " => " & Token_Image (I));
         end loop;
         New_Line;
      end;
   end if;

   begin
      Grammar := Production_Lists.Only
        (Nonterminals.Get (Accept_ID) <= Nonterminals.Get (Find_Token_ID (-Start_Token)) &
           Tokens_Pkg.Get (EOI_ID));
   exception
   when Not_Found =>
      Wisi.Utils.Put_Error
        (Elisp_Package & ".wy", First_Rule_Line, "start token '" & (-Start_Token) & "' not found; need %start?");
      raise Syntax_Error;
   end;

   for Rule of Rules loop
      declare
         Index  : Integer := 0;
      begin
         for Right_Hand_Side of Rule.Right_Hand_Sides loop
            declare
               Tokens : Token_Lists.Instance;
            begin
               for Token of Right_Hand_Side.Production loop
                  Tokens := Tokens & Token;
               end loop;
               Grammar := Grammar and Nonterminals.Get (Find_Token_ID (-Rule.Left_Hand_Side)) <= Tokens + Index;
            exception
            when E : Not_Found =>
               Wisi.Utils.Put_Error
                 (Elisp_Package & ".wy", Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
               raise Syntax_Error;
            end;
            Index := Index + 1;
         end loop;
      end;
   end loop;

   if Verbosity > 0 then
      declare
         use Standard.Ada.Text_IO;
         procedure Print_Action (Item : in Nonterminals.Synthesize) is null;
         package Token_List_Print is new Token_Lists.Print;
         package Print_Production is new Productions.Print (Token_List_Print, Print_Action);
         package Print_Production_Lists is new Production_Lists.Print (Print_Production.Print);
      begin
         Put_Line ("Grammar:");
         Print_Production_Lists.Print (Grammar);
         New_Line;
      end;
   end if;

   begin
      Parser := LALR_Parsers.Generate
        (Grammar,
         Analyzers.Null_Analyzer,
         Trace                => Verbosity > 1,
         Put_Grammar          => Verbosity > 0,
         First_State_Index    => 0); -- match Elisp array indexing
   exception
   when E : OpenToken.Programmer_Error =>
      Wisi.Utils.Put_Error (Elisp_Package & ".wy", First_Rule_Line, Standard.Ada.Exceptions.Exception_Message (E));
      raise Syntax_Error;
   end;

   declare
      use Standard.Ada.Text_IO;
   begin
      Create (File, Out_File, Elisp_Package & "-wy.el");
      Set_Output (File);
      Header (Elisp_Package, Copyright);
      for Line of Prologue loop
         Put_Line (Line);
      end loop;
      Put_Line ("(require 'semantic/lex)"); -- FIXME: emacs 23 wants semantic-lex, 24 semantic/lex
      Put_Line ("(require 'wisi-compile)");
      New_Line;
      Keyword_Table (Elisp_Package, Keywords);
      New_Line;
      Token_Table (Elisp_Package, Tokens);
      New_Line;
      Parser_Elisp.Output (Elisp_Package, Tokens, Keywords, Rules, Parser);
      New_Line;
      Put_Line ("(provide '" & Elisp_Package & "-wy)");
      New_Line;
      Put_Line (";; end of file");
      Close (File);
   end;
end Wisi.Output_Elisp;
