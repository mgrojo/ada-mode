--  Abstract :
--
--  Utilities for translating input file structures to FastToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015  All Rights Reserved.
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

with FastToken.Lexer;
with FastToken.Parser.LALR.Generator;
with FastToken.Production.Put_Trace;
with FastToken.Token.Nonterminal;
generic
   Keywords              : in Wisi.String_Pair_Lists.List;
   Tokens                : in Wisi.Token_Lists.List;
   Conflicts             : in Wisi.Conflict_Lists.List;
   Rules                 : in Wisi.Rule_Lists.List;
   EOI_Name              : in Ada.Strings.Unbounded.Unbounded_String; -- without trailing _ID
   FastToken_Accept_Name : in Ada.Strings.Unbounded.Unbounded_String;
   First_State_Index     : in Integer;

   with function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String;
package Wisi.Gen_Generate_Utils is

   subtype Token_ID is Integer range
     1 .. Count (Tokens) + Integer (Keywords.Length) + 1 + Integer (Rules.Length) + 1;
   --  one extra terminal for EOI
   --  one extra non-terminal for the FastToken accept symbol followed by EOI.

   function Count_Non_Reporting return Integer;

   Token_Count    : constant Token_ID := Count (Tokens);
   First_Terminal : constant Token_ID := Token_ID'First + Count_Non_Reporting;
   EOI_ID         : constant Token_ID := Token_Count + Token_ID (Keywords.Length) + 1; -- last terminal
   Accept_ID      : constant Token_ID := Token_ID'Last;                                -- last nonterminal

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_ID;

   type ID_Array_Access_String_Type is array (Token_ID) of access constant String;

   Token_Image_Width : Integer := 0;

   function Set_Token_Images return ID_Array_Access_String_Type;

   Token_Images : constant ID_Array_Access_String_Type := Set_Token_Images;

   function Token_Image (ID : in Token_ID) return String is (Token_Images (ID).all);

   type Token_Cursor is tagged private;
   --  Iterate thru tokens in a canonical order.

   function First return Token_Cursor;
   procedure Next (Cursor : in out Token_Cursor);
   function Is_Done (Cursor : in out Token_Cursor) return Boolean;
   function Token_Name (Cursor : in out Token_Cursor) return Standard.Ada.Strings.Unbounded.Unbounded_String;
   --  Return the token name from the .wy file:
   --  token: Tokens.name
   --  keyword: Keywords.name
   --  nonterminal: Rules.Left_Hand_Side

   procedure Put_Tokens;
   --  Put user readable token list to Standard_Output

   package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, EOI_ID, Token_Image);
   package Nonterminal_Pkg is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal_Pkg);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LALR is new Parser_Root.LALR (First_State_Index, Nonterminal => Nonterminal_Pkg);
   package LALR_Generator is new LALR.Generator (Token_Image_Width, Production);

   procedure Put_Trace_Action (Item : in Nonterminal_Pkg.Synthesize) is null;
   package Put_Trace_Production is new Production.Put_Trace (Put_Trace_Action);

   function To_Conflicts
     (Shift_Reduce_Conflict_Count  : out Integer;
      Reduce_Reduce_Conflict_Count : out Integer)
     return LALR.Conflict_Lists.List;

   function To_Grammar (Source_File_Name : in String; Start_Token : in String) return Production.List.Instance;
   --  Source_File_Name used in errors

private

   type Token_Cursor_State is
     (Non_Reporting, Terminals_Keywords, Terminals_Others, EOI, FastToken_Accept, Nonterminal, Done);

   type Token_Cursor is tagged record
      State       : Token_Cursor_State;
      Token_Kind  : Wisi.Token_Lists.Cursor;
      Token_Item  : String_Pair_Lists.Cursor;
      Keyword     : String_Pair_Lists.Cursor;
      Nonterminal : Rule_Lists.Cursor;
   end record;

end Wisi.Gen_Generate_Utils;
