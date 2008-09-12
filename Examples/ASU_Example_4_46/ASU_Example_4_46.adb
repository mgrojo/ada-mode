-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 2, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Maintainer: Ted Dennison (dennison@telepath.com)
--
-- Update History:
-- $Log: ASU_Example_4_46.adb,v $
-- Revision 1.1  2000/01/27 21:01:18  Ted
-- An implementation of example 4.46 from the dragon book
--
--
-------------------------------------------------------------------------------
with Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Token;
with OpenToken.Token.List;
with OpenToken.Token.Analyzer;
with OpenToken.Token.Nonterminal;
with OpenToken.Production;
with OpenToken.Production.List;
with OpenToken.Production.Parser;
with OpenToken.Production.Parser.LALR;
with OpenToken.Production.Parser.LRK_Item;
with OpenToken.Recognizer.Keyword;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Character_Set;

-------------------------------------------------------------------------------
-- This example is an implementation of Example 4.46 from "Compilers
-- Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
-- Dragon Book"). It demonstrates basic LALR(1) parsing
-------------------------------------------------------------------------------
procedure ASU_Example_4_46 is

   -- The complete list of tokens, with the terminals listed first.
   type Token_IDs is (Asterix_ID, ID_ID, Equals_ID, EOF_ID, Whitespace_ID, S_ID, L_ID, R_ID, S_Prime_ID);

   -- Instantiate all the nessecary packages
   package Master_Token is new OpenToken.Token(Token_IDs);
   package Tokenizer is new Master_Token.Analyzer(Whitespace_ID);
   package Token_List is new Master_Token.List;
   package Nonterminal is new Master_Token.Nonterminal(Token_List);
   package Production is new OpenToken.Production(Master_Token, Token_List, Nonterminal);
   package Production_List is new Production.List;
   package Parser is new Production.Parser(Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

   -- Allow infix operators for building productions
   use type Token_List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production_List.Instance;

   -- Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Input_File'Unchecked_Access);

   -- Define all our tokens
   Asterix : aliased Master_Token.Class := Master_Token.Get (Asterix_ID);
   ID      : aliased Master_Token.Class := Master_Token.Get (ID_ID);
   Equals  : aliased Master_Token.Class := Master_Token.Get (Equals_ID);
   EOF     : aliased Master_Token.Class := Master_Token.Get (EOF_ID);
   S       : aliased Nonterminal.Class := Nonterminal.Get (S_ID);
   L       : aliased Nonterminal.Class := Nonterminal.Get (L_ID);
   R       : aliased Nonterminal.Class := Nonterminal.Get (R_ID);
   S_Prime : aliased Nonterminal.Class := Nonterminal.Get (S_Prime_ID);

   -- Define a lexer syntax for the terminals
   Syntax : constant Tokenizer.Syntax :=
     (Asterix_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Keyword.Get ("*"),
                                   New_Token  => Asterix),
      ID_ID      => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Keyword.Get ("id"),
                                   New_Token  => ID),
      Equals_ID     => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Keyword.Get ("="),
                                      New_Token  => Equals
                                      ),
      EOF_ID        => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get,
                                      New_Token  => EOF
                                      ),
      Whitespace_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                      (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
      );

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'access);

   --------------------------------------------------------------------------
   -- Define the Grammar. The text in the example in the book looks something
   -- like:
   --
   -- S' -> S
   -- S  -> L = R | R
   -- L  -> * R | id
   -- R  -> L
   --
   Grammar : constant Production_List.Instance :=
     S_Prime <= S & EOF and
     S       <= L & Equals & R and
     S       <= R and
     L       <= Asterix & R + Nonterminal.Synthesize_Self and
     L       <= ID + Nonterminal.Synthesize_Self and
     R       <= L;


   -- The lalr parser instance.
   Test_Parser : LALR_Parser.Instance :=
     LALR_Parser.Generate (Grammar  => Grammar,
                           Analyzer => Analyzer
                           );

   Test_File_Name : constant String := "Example.txt";
begin

   Ada.Text_IO.Put ("Parsing file " & Test_File_Name & "...");
   Ada.Text_IO.Flush;

   Ada.Text_IO.Open (File => Input_File,
                     Name => Test_File_Name,
                     Mode => Ada.Text_IO.In_File
                     );

   -- Uncomment the following line to get a look at the parser
--   LALR_Parser.Print_Table (Test_Parser);

   LALR_Parser.Parse (Test_Parser);

   Ada.Text_IO.Put_Line ("passed");
end ASU_Example_4_46;



