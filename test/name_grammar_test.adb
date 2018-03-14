--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002-2003, 2009, 2013-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.Gen_Token_Enum;
with WisiToken.LR.LALR_Generator;
with WisiToken.LR.Parser;
with WisiToken.Lexer.Regexp;
with WisiToken.Production;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Name_Grammar_Test is

   type Token_ID is
     (Whitespace_ID,

      --  terminals
      Dot_ID,
      Paren_Left_ID,
      Paren_Right_ID,
      Identifier_ID,
      EOF_ID,

      --  non-terminals; accept is neither first nor last.
      Component_ID,
      Component_List_ID,
      Name_ID,
      Statement_ID,
      Symbol_Name_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Dot_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Component_ID,
      Last_Nonterminal  => Symbol_Name_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Statement_ID);
   use Token_Enum;

   First_State_Index  : constant := 1;
   First_Parser_Label : constant := 1;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Whitespace_ID  => Lexer.Get (" ", Report => False),
       Dot_ID         => Lexer.Get ("\."),
       Paren_Left_ID  => Lexer.Get ("\("),
       Paren_Right_ID => Lexer.Get ("\)"),
       Identifier_ID  => Lexer.Get ("[0-9a-zA-Z_]+"),
       EOF_ID         => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   use all type WisiToken.Production.List.Instance;   --  "and"
   use all type WisiToken.Production.Right_Hand_Side; --  "+"

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   --  valid names:
   --  Module.Symbol
   --  Module.Symbol (Index)
   --  Module.Symbol.Component
   --  Module.Symbol (Index).Component
   --  Module.Symbol.Component (Index) ...
   Full_Grammar : constant WisiToken.Production.List.Instance :=
     Statement_ID      <= Name_ID & EOF_ID + Null_Action and
     Name_ID           <= Symbol_Name_ID & Component_List_ID + Null_Action and
     Name_ID           <= Symbol_Name_ID + Null_Action and
     Symbol_Name_ID    <= Identifier_ID & Dot_ID & Identifier_ID + Null_Action and
     Component_List_ID <= Component_ID & Component_List_ID + Null_Action and
     Component_List_ID <= Component_ID + Null_Action and
     Component_ID      <= Dot_ID & Identifier_ID + Null_Action and
     Component_ID      <= Paren_Left_ID & Identifier_ID & Paren_Right_ID + Null_Action;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);

   procedure Parse_Command
     (Label   : in     String;
      Parser  : in out WisiToken.LR.Parser.Parser;
      Command : in     String)
   is begin
      Ada.Text_IO.Put_Line ("'" & Command & "'");

      Parser.Lexer.Reset_With_String (Command);
      Parser.Semantic_State.Reset;
      Parser.Parse;

      Ada.Text_IO.Put_Line ("success");
      Ada.Text_IO.New_Line;
   exception
   when E : others =>
      AUnit.Assertions.Assert
        (False, Label & "'" & Command & "': " &
           Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
   end Parse_Command;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      Orig_Trace_Parse : constant Integer := WisiToken.Trace_Parse;

      Trace_File_Name : constant String := "name_grammar_test.out";
      Trace_File : File_Type;

      Expected_Trace_File_Name : constant String := "../Test/name_grammar_test.good_out";
   begin
      --  The test is that there are no exceptions, and that the parse
      --  trace matches the known good trace.
      WisiToken.Trace_Parse := WisiToken.Detail + 1;

      if Exists (Trace_File_Name) then
         Delete_File (Trace_File_Name);
      end if;
      Create (Trace_File, Out_File, Trace_File_Name);
      Set_Output (Trace_File);
      Set_Error (Trace_File);

      New_Line;
      Put_Line ("Full Parser");
      declare
         Parser : WisiToken.LR.Parser.Parser;
      begin
         WisiToken.LR.Parser.New_Parser
           (Parser,
            Trace'Access,
            Lexer.New_Lexer (Trace'Access, Syntax),
            WisiToken.LR.LALR_Generator.Generate
              (Full_Grammar,
               LALR_Descriptor,
               First_State_Index,
               Ignore_Unused_Tokens => False),
            First_Parser_Label);
         Parse_Command ("Full Parser", Parser, "Module.Symbol");
         Parse_Command ("Full Parser", Parser, "Module.Symbol (Index)");
         Parse_Command ("Full Parser", Parser, "Module.Symbol.Component");
         Parse_Command ("Full Parser", Parser, "Module.Symbol (Index).Component");
         Parse_Command ("Full Parser", Parser, "Module.Symbol.Component (Index)");
      end;

      Close (Trace_File);
      Set_Output (Standard_Output);
      WisiToken.Trace_Parse := Orig_Trace_Parse;

      Check_Files ("1", Trace_File_Name, Expected_Trace_File_Name);
   exception
   when others =>
      if Is_Open (Trace_File) then
         Close (Trace_File);
         Set_Output (Standard_Output);
      end if;
      WisiToken.Trace_Parse := Orig_Trace_Parse;
      raise;
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("name_grammar_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Name_Grammar_Test;
