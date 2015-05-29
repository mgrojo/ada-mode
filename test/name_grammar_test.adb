--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002-2003, 2009, 2013-2015 Stephen Leake.  All Rights Reserved.
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
package body Name_Grammar_Test is

   type Token_ID is
     (Whitespace_ID,

      --  terminals
      Dot_ID,
      Paren_Left_ID,
      Paren_Right_ID,
      Identifier_ID,
      EOF_ID,

      --  non-terminals
      Component_ID,
      Component_List_ID,
      Name_ID,
      Statement_ID,
      Symbol_Name_ID);

   Token_Image_Width : constant Integer := Token_ID'Width;
   package Token_Pkg is new FastToken.Token (Token_ID, Dot_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Lexer is new Lexer_Root.Regexp;
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LALR is new Parser_Root.LALR (First_State_Index => 1, Nonterminal => Nonterminal);
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LALR.Parser_Lists (First_Parser_Label);
   package LALR_Parser is new LALR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);
   package LALR_Generator is new LALR.Generator (Token_Image_Width, Production);

   package Tokens is
      --  For use in right hand sides, syntax.
      Dot         : constant Token_Pkg.Class := Token_Pkg.Get (Dot_ID);
      Paren_Left  : constant Token_Pkg.Class := Token_Pkg.Get (Paren_Left_ID);
      Paren_Right : constant Token_Pkg.Class := Token_Pkg.Get (Paren_Right_ID);
      Identifier  : constant Token_Pkg.Class := Token_Pkg.Get (Identifier_ID);
      EOF         : constant Token_Pkg.Class := Token_Pkg.Get (EOF_ID);

      Statement      : constant Nonterminal.Class := Nonterminal.Get (Statement_ID);
      Name           : constant Nonterminal.Class := Nonterminal.Get (Name_ID);
      Symbol_Name    : constant Nonterminal.Class := Nonterminal.Get (Symbol_Name_ID);
      Component      : constant Nonterminal.Class := Nonterminal.Get (Component_ID);
      Component_List : constant Nonterminal.Class := Nonterminal.Get (Component_List_ID);
   end Tokens;

   Syntax : constant Lexer.Syntax :=
     (
      Whitespace_ID  => Lexer.Get (" ", Token_Pkg.Get (Whitespace_ID), Report => False),
      Dot_ID         => Lexer.Get ("\.", Tokens.Dot),
      Paren_Left_ID  => Lexer.Get ("\(", Tokens.Paren_Left),
      Paren_Right_ID => Lexer.Get ("\)", Tokens.Paren_Right),
      Identifier_ID  => Lexer.Get ("[0-9a-zA-Z_]+", Tokens.Identifier),
      EOF_ID         => Lexer.Get ("" & FastToken.EOF_Character, Tokens.EOF)
     );

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   use type Production.Instance;        --  "<="
   use type Production.List.Instance;   --  "and"
   use type Production.Right_Hand_Side; --  "+"
   use type Token_Pkg.List.Instance;    --  "&"

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   --  valid names:
   --  Module (Index)
   --  Module.Component
   Simple_Grammar : constant Production.List.Instance :=
     Tokens.Statement  <= Tokens.Name & Tokens.EOF + Self and
     Tokens.Name       <= Tokens.Identifier & Tokens.Component + Self and
     Tokens.Component  <= Tokens.Dot & Tokens.Identifier + Self and
     Tokens.Component  <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Self;

   --  valid names:
   --  Module.Symbol (Index)
   --  Module.Symbol.Component
   Medium_Grammar : constant Production.List.Instance :=
     Tokens.Statement   <= Tokens.Name & Tokens.EOF + Self and
     Tokens.Name        <= Tokens.Symbol_Name & Tokens.Component + Self and
     Tokens.Symbol_Name <= Tokens.Identifier & Tokens.Dot & Tokens.Identifier + Self and
     Tokens.Component   <= Tokens.Dot & Tokens.Identifier + Self and
     Tokens.Component   <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Self;

   --  valid names:
   --  Module.Symbol
   --  Module.Symbol (Index)
   --  Module.Symbol.Component
   --  Module.Symbol (Index).Component
   --  Module.Symbol.Component (Index) ...
   Full_Grammar : constant Production.List.Instance :=
     Tokens.Statement      <= Tokens.Name & Tokens.EOF + Self and
     Tokens.Name           <= Tokens.Symbol_Name & Tokens.Component_List + Self and
     Tokens.Name           <= Tokens.Symbol_Name + Self and
     Tokens.Symbol_Name    <= Tokens.Identifier & Tokens.Dot & Tokens.Identifier + Self and
     Tokens.Component_List <= Tokens.Component & Tokens.Component_List + Self and
     Tokens.Component_List <= Tokens.Component + Self and
     Tokens.Component      <= Tokens.Dot & Tokens.Identifier + Self and
     Tokens.Component      <= Tokens.Paren_Left & Tokens.Identifier & Tokens.Paren_Right + Self;

   procedure Parse_Command (Lable : in String; Parser : in out LALR_Parser.Instance; Command : in String)
   is begin
      Ada.Text_IO.Put_Line ("'" & Command & "'");

      FastToken.Text_Feeder.String.Set (String_Feeder, Command);

      Parser.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

      Parser.Parse;

      Ada.Text_IO.Put_Line ("success");
      Ada.Text_IO.New_Line;
   exception
   when E : others =>
      AUnit.Assertions.Assert
        (False, Lable & "'" & Command & "': " &
           Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
   end Parse_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      Trace_File_Name : constant String := "name_grammar_test.out";
      Trace_File : File_Type;

      Expected_Trace_File_Name : constant String := "../../Test/name_grammar_test.good_out";
   begin
      --  The test is that there are no exceptions, and that the parse
      --  trace matches the known good trace.
      FastToken.Trace_Parse := 2;

      if Exists (Trace_File_Name) then
         Delete_File (Trace_File_Name);
      end if;
      Create (Trace_File, Out_File, Trace_File_Name);
      Set_Output (Trace_File);

      Put_Line ("Simple Parser");
      declare
         Parser : LALR_Parser.Instance := LALR_Parser.Initialize
           (Lexer.Initialize (Syntax, String_Feeder'Access),
            LALR_Generator.Generate
              (Simple_Grammar,
               Put_Parse_Table      => Test.Debug,
               Trace                => Test.Debug,
               Ignore_Unused_Tokens => True));
      begin
         Parse_Command ("Simple Parser", Parser, "Module (Index)");
         Parse_Command ("Simple Parser", Parser, "Module.Component");
      end;

      New_Line;
      Put_Line ("Medium Parser");
      declare
         Parser : LALR_Parser.Instance := LALR_Parser.Initialize
           (Lexer.Initialize (Syntax, String_Feeder'Access),
            LALR_Generator.Generate
              (Medium_Grammar,
               Put_Parse_Table      => Test.Debug,
               Trace                => Test.Debug,
               Ignore_Unused_Tokens => True));
      begin
         Parse_Command ("Medium Parser", Parser, "Module.Symbol (Index)");
         Parse_Command ("Medium Parser", Parser, "Module.Symbol.Component");
      end;

      New_Line;
      Put_Line ("Full Parser");
      declare
         Parser : LALR_Parser.Instance := LALR_Parser.Initialize
           (Lexer.Initialize (Syntax, String_Feeder'Access),
            LALR_Generator.Generate
              (Full_Grammar,
               Put_Parse_Table      => Test.Debug,
               Trace                => Test.Debug,
               Ignore_Unused_Tokens => False));
      begin
         Parse_Command ("Full Parser", Parser, "Module.Symbol");
         Parse_Command ("Full Parser", Parser, "Module.Symbol (Index)");
         Parse_Command ("Full Parser", Parser, "Module.Symbol.Component");
         Parse_Command ("Full Parser", Parser, "Module.Symbol (Index).Component");
         Parse_Command ("Full Parser", Parser, "Module.Symbol.Component (Index)");
      end;

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
      return new String'("../../Test/name_grammar_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Name_Grammar_Test;
--  Local Variables:
--  eval: (ada-indent-opentoken-mode)
--  End:
