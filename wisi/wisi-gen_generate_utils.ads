--  Abstract :
--
--  Utilities for translating input file structures to FastToken
--  structures needed for LALR.Generate.
--
--  Copyright (C) 2014, 2015, 2017  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
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

with FastToken.Lexer;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR1_Items;
with FastToken.Production.Put_Trace;
with FastToken.Token.Nonterminal;
generic
   Keywords              : in Wisi.String_Pair_Lists.List;
   Tokens                : in Wisi.Token_Lists.List;
   Conflicts             : in Wisi.Conflict_Lists.List;
   Rules                 : in Wisi.Rule_Lists.List;
   EOI_Name              : in Standard.Ada.Strings.Unbounded.Unbounded_String; -- without trailing _ID
   FastToken_Accept_Name : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   First_State_Index     : in Integer;

   with function To_Token_Out_Image (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String;
   --  Name of token in output file
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

   type File_Kind is (WY, Output);
   type ID_Array_Access_String_Pair_Type is array (Token_ID, File_Kind) of access constant String;

   Token_WY_Image_Width : Integer := 0; -- set by Set_Token_Images

   function Set_Token_Images return ID_Array_Access_String_Pair_Type;

   Token_Images : constant ID_Array_Access_String_Pair_Type := Set_Token_Images;

   function Token_WY_Image (ID : in Token_ID) return String is (Token_Images (ID, WY).all);
   function Token_Out_Image (ID : in Token_ID) return String is (Token_Images (ID, Output).all);

   type Token_Cursor is tagged private;
   --  Iterate thru Tokens in a canonical order.

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

   package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, EOI_ID, Token_WY_Image);
   package Nonterminal_Pkg is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal_Pkg);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser
     (Token_ID, First_Terminal, EOI_ID, EOI_ID, Accept_ID, Token_WY_Image, Standard.Ada.Text_IO.Put,
      Token_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_WY_Image_Width, Nonterminal_Pkg, Nonterminal_Pkg.Get);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package LALR_Generator is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);
   package LR1_Generator is new LR.LR1_Generator (Production, LR1_Items, Generator_Utils);

   procedure Put_Trace_Action (Item : in Nonterminal_Pkg.Synthesize) is null;
   package Put_Trace_Production is new Production.Put_Trace (Put_Trace_Action);

   function To_Conflicts
     (Accept_Reduce_Conflict_Count : out Integer;
      Shift_Reduce_Conflict_Count  : out Integer;
      Reduce_Reduce_Conflict_Count : out Integer)
     return Generator_Utils.Conflict_Lists.List;

   function To_Grammar (Source_File_Name : in String; Start_Token : in String) return Production.List.Instance;
   --  Source_File_Name used in errors

   function To_Nonterminal_ID_Set (Item : in String_Lists.List) return Token_Pkg.Nonterminal_ID_Set;

   function To_State_Count (State_Last : in LR.State_Index) return LR.State_Index;
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
