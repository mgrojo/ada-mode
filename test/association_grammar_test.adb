--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002-2003, 2009-2010, 2013-2015 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with FastToken.Lexer.Regexp;
with FastToken.Parser.LALR.Generator;
with FastToken.Parser.LALR.Parser;
with FastToken.Parser.LALR.Parser_Lists;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token.Nonterminal;
package body Association_Grammar_Test is

   type Token_ID_Type is
     (Whitespace_ID,

      --  terminals
      Comma_ID,
      Equal_Greater_ID,
      Identifier_ID,
      Int_ID,
      Paren_Left_ID,
      Paren_Right_ID,

      --  last terminal
      EOF_ID,

      --  non-terminals
      Aggregate_ID,
      Association_ID,
      Association_List_ID,
      Statement_ID);

   Token_Image_Width : constant Integer := Token_ID_Type'Width;
   package Token_Pkg is new FastToken.Token (Token_ID_Type, Comma_ID, EOF_ID, Token_ID_Type'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);

   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Lexer is new Lexer_Root.Regexp (EOF_ID);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LALR is new Parser_Root.LALR (First_State_Index => 1, Nonterminal => Nonterminal);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LALR.Parser_Lists (First_Parser_Label);
   package LALR_Parser is new LALR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LALR_Generator is new LALR.Generator (Token_Image_Width, Production);

   package Tokens is
      --  For use in right hand sides, syntax.
      Comma         : constant Token_Pkg.Class := Token_Pkg.Get (Comma_ID);
      EOF           : constant Token_Pkg.Class := Token_Pkg.Get (EOF_ID);
      Equal_Greater : constant Token_Pkg.Class := Token_Pkg.Get (Equal_Greater_ID);
      Integer       : constant Token_Pkg.Class := Token_Pkg.Get (Int_ID);
      Identifier    : constant Token_Pkg.Class := Token_Pkg.Get (Identifier_ID);
      Paren_Left    : constant Token_Pkg.Class := Token_Pkg.Get (Paren_Left_ID);
      Paren_Right   : constant Token_Pkg.Class := Token_Pkg.Get (Paren_Right_ID);
   end Tokens;

   Syntax : constant Lexer.Syntax :=
     (
      --  terminals
      Whitespace_ID    => Lexer.Get (" ", Token_Pkg.Get (Whitespace_ID), Report => False),
      Comma_ID         => Lexer.Get (",", Tokens.Comma),
      Equal_Greater_ID => Lexer.Get ("=>", Tokens.Equal_Greater),
      Int_ID           => Lexer.Get ("[0-9]+", Tokens.Integer),
      Identifier_ID    => Lexer.Get ("[0-9a-zA-Z_]+", Tokens.Identifier),
      Paren_Left_ID    => Lexer.Get ("\(", Tokens.Paren_Left),
      Paren_Right_ID   => Lexer.Get ("\)", Tokens.Paren_Right)
     );

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   use type Production.Instance;        --  "<="
   use type Production.List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_Pkg.List.Instance;    --  "&"

   --  For use in right or left hand sides
   Aggregate        : constant Nonterminal.Class := Nonterminal.Get (Aggregate_ID);
   Association      : constant Nonterminal.Class := Nonterminal.Get (Association_ID);
   Association_List : constant Nonterminal.Class := Nonterminal.Get (Association_List_ID);
   Statement        : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
   Self             : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   --  valid syntax:
   --  (identifier)
   --  (identifier, identifier)
   --  (identifier => identifier)
   --  (integer => identifier)
   --  (identifier => identifier, integer => identifier)
   Full_Grammar     : constant Production.List.Instance :=
     Statement        <= Aggregate & Tokens.EOF + Self and
     Aggregate        <= Tokens.Paren_Left & Association_List & Tokens.Paren_Right + Self and
     Association_List <= Association & Tokens.Comma & Association_List + Self and
     Association_List <= Association + Self and
     Association      <= Tokens.Identifier & Tokens.Equal_Greater & Tokens.Identifier + Self and
     Association      <= Tokens.Integer & Tokens.Equal_Greater & Tokens.Identifier + Self and
     Association      <= Tokens.Identifier + Self;

   Parser : LALR_Parser.Instance;

   procedure Parse_Command (Command : in String)
   is begin
      FastToken.Text_Feeder.String.Set (String_Feeder, Command);

      LALR_Parser.Reset (Parser, Buffer_Size => Command'Length);

      LALR_Parser.Parse (Parser);
   exception
   when E : others =>
      AUnit.Assertions.Assert
        (False, Command & ": " & Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
   end Parse_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      Trace_File_Name : constant String := "association_grammar_test.out";
      Trace_File : File_Type;

      Expected_Trace_File_Name : constant String := "../../Test/association_grammar_test.good_out";
   begin
      --  The test is that there are no exceptions, and that the parse
      --  trace matches the known good trace.

      if Exists (Trace_File_Name) then
         Delete_File (Trace_File_Name);
      end if;
      Create (Trace_File, Out_File, Trace_File_Name);
      Set_Output (Trace_File);

      Parser := LALR_Parser.Initialize
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         LALR_Generator.Generate
           (Full_Grammar,
            Put_Parse_Table => Test.Debug,
            Trace           => Test.Debug));

      FastToken.Trace_Parse := 1;

      Parse_Command ("(identifier)");
      Parse_Command ("(identifier, identifier)");
      Parse_Command ("(identifier => identifier)");
      Parse_Command ("(integer => identifier)");
      Parse_Command ("(identifier => identifier, integer => identifier)");

      Close (Trace_File);
      Set_Output (Standard_Output);

      Check_Files ("1", Trace_File_Name, Expected_Trace_File_Name);
   exception
   when others =>
      if Is_Open (Trace_File) then
         Close (Trace_File);
         Set_Output (Standard_Output);
      end if;
      raise;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Association_Grammar_Test");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Association_Grammar_Test;
--  Local Variables:
--  eval: (ada-indent-opentoken-mode)
--  End:
