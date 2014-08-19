--  Abstract :
--
--  Utilities for translating input file structures to OpenToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014  All Rights Reserved.
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
with OpenToken.Production.Print;
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.Nonterminal;
generic
   Keywords  : in Wisi.String_Pair_Lists.List;
   Tokens    : in Wisi.Token_Lists.List;
   Conflicts : in Wisi.Conflict_Lists.List;
   Rules     : in Wisi.Rule_Lists.List;
   EOI_Image : in String;
package Wisi.Gen_Generate_Utils is

   subtype Token_IDs is Integer range
     1 .. Count (Tokens) + Integer (Keywords.Length) + 1 + Integer (Rules.Length) + 1;
   --  one extra terminal for EOI
   --  one extra non-terminal for the OpenToken accept symbol followed by EOI.
   --  don't need a whitespace token; handled by Elisp Wisi lexer

   function Count_Non_Reporting return Integer;

   Token_Count    : constant Token_IDs := Count (Tokens);
   First_Terminal : constant Token_IDs := Token_IDs'First + Count_Non_Reporting;
   EOI_ID         : constant Token_IDs := Token_Count + Token_IDs (Keywords.Length) + 1; -- last terminal
   Accept_ID      : constant Token_IDs := Token_IDs'Last;                                -- last nonterminal

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_IDs;

   type ID_Array_Access_String_Type is array (Token_IDs) of access constant String;

   Token_Image_Width : Integer := 0;

   function Set_Token_Images return ID_Array_Access_String_Type;

   Token_Images : constant ID_Array_Access_String_Type := Set_Token_Images;

   function Token_Image (ID : in Token_IDs) return String;

   procedure Put_Tokens;
   --  Put user readable token list to Standard_Output

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_Image, Token_Image_Width);
   --  we only need Analyzers to instantiate Parsers, but we might call it for debugging
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => First_Terminal, Last_Terminal => EOI_ID);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALR_Parsers is new Parsers.LALR (First_State_Index => 0);  -- match Elisp array indexing

   procedure Print_Action (Item : in Nonterminals.Synthesize) is null;
   package Token_List_Print is new Token_Lists.Print;
   package Print_Production is new Productions.Print (Token_List_Print, Print_Action);
   package Print_Production_Lists is new Production_Lists.Print (Print_Production.Print);

   function To_Conflicts return LALR_Parsers.Conflict_Lists.List;

   function To_Grammar (Source_File_Name : in String; Start_Token : in String) return Production_Lists.Instance;
   --  Source_File_Name used in errors

   --  code generation
   Indent : Ada.Text_IO.Positive_Count := 1;

   procedure Indent_Line (Text : in String);
   --  Put Text indented to Indent to Current_Output, with newline.

   function State_Image (Item : in LALR_Parsers.State_Index) return String;
   function Int_Image (Item : in Integer) return String;
   --  no leading space

end Wisi.Gen_Generate_Utils;
