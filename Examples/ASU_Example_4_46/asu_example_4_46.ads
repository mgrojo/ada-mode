-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2013, 2014 Stephe Leake
-- Copyright (C) 2000 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  This example is an implementation of Example 4.46 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). The example was meant to demonstrate basic LALR(1) parsing.
--  Here we show to to perform LL(n) parsing with it.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package ASU_Example_4_46 is

   --  The complete list of tokens, with the non-reporting and terminals listed first.
   type Token_IDs is (Whitespace_ID, Asterix_ID, ID_ID, Equals_ID, EOF_ID, S_ID, L_ID, R_ID, S_Prime_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs, Asterix_ID, EOF_ID, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal (Token_List);
   package Production is new OpenToken.Production (Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package Parser is new Production.Parser (Tokenizer);
   package LALRs is new Parser.LALR (First_State_Index => 1);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label);
   package LALR_Parser is new LALRs.Parser (First_Parser_Label, Parser_Lists);
   Token_Image_Width : Integer := Token_IDs'Width;
   package LALR_Generator is new LALRs.Generator (Token_Image_Width, Production_List);

   Syntax : constant Tokenizer.Syntax :=
     (Asterix_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("*")),
      ID_ID         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("id")),
      Equals_ID     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("=")),
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                        (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   --  Define all tokens, for use in declaring the grammar. Note that
   --  these may be constants; the parser stores intermediate results
   --  in the non-terminal tokens, but it allocates new tokens for
   --  that.
   --
   --  Terminal tokens
   Asterix : constant Master_Token.Class := Master_Token.Get (Asterix_ID);
   ID      : constant Master_Token.Class := Master_Token.Get (ID_ID);
   Equals  : constant Master_Token.Class := Master_Token.Get (Equals_ID);
   EOF     : constant Master_Token.Class := Master_Token.Get (EOF_ID);

   --  Non-terminal tokens.
   S       : constant Nonterminal.Class := Nonterminal.Get (S_ID);
   L       : constant Nonterminal.Class := Nonterminal.Get (L_ID);
   R       : constant Nonterminal.Class := Nonterminal.Get (R_ID);
   S_Prime : constant Nonterminal.Class := Nonterminal.Get (S_Prime_ID);

   --  Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   ------------------------------------------------------------------------
   --  Define the Grammar productions.
   --
   --  The text in the example in the book looks something like:
   --
   --  S' -> S
   --  S  -> L = R | R
   --  L  -> * R | id
   --  R  -> L
   --
   Grammar : constant Production_List.Instance :=
     S_Prime <= S & EOF and
     S       <= L & Equals & R and
     S       <= R and
     L       <= Asterix & R + Nonterminal.Synthesize_Self and
     L       <= ID + Nonterminal.Synthesize_Self and
     R       <= L;

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

end ASU_Example_4_46;
