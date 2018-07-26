--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002-2003, 2009-2010, 2013-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with WisiToken.LR.LALR_Generate;
with WisiToken.LR.Parser;
with WisiToken.Lexer.Regexp;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada;
package body Association_Grammar_Test is

   type Token_Enum_ID is
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
      Statement_ID, -- must be first nonterm
      Aggregate_ID,
      Association_ID,
      Association_List_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_Enum_ID,
      First_Terminal    => Comma_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Statement_ID,
      Last_Nonterminal  => Association_List_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Statement_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   package Lexer renames WisiToken.Lexer.Regexp;
   Syntax : constant Lexer.Syntax := To_Syntax
     ((Whitespace_ID    => Lexer.Get (" ", Report => False),
       Comma_ID         => Lexer.Get (","),
       Equal_Greater_ID => Lexer.Get ("=>"),
       Int_ID           => Lexer.Get ("[0-9]+"),
       Identifier_ID    => Lexer.Get ("[0-9a-zA-Z_]+"),
       Paren_Left_ID    => Lexer.Get ("\("),
       Paren_Right_ID   => Lexer.Get ("\)"),
       EOF_ID           => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   use WisiToken.Wisi_Ada;   --  "and", "+"

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   --  valid syntax:
   --  (identifier)
   --  (identifier, identifier)
   --  (identifier => identifier)
   --  (integer => identifier)
   --  (identifier => identifier, integer => identifier)
   Full_Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
     Statement_ID        <= Aggregate_ID & EOF_ID + Null_Action and
     Aggregate_ID        <= Paren_Left_ID & Association_List_ID & Paren_Right_ID + Null_Action and
     (Association_List_ID <= Association_ID & Comma_ID & Association_List_ID + Null_Action or
                            Association_ID + Null_Action) and
     (Association_ID      <= Identifier_ID & Equal_Greater_ID & Identifier_ID + Null_Action or
                            Int_ID & Equal_Greater_ID & Identifier_ID + Null_Action or
                            Identifier_ID + Null_Action);

   Parser : WisiToken.LR.Parser.Parser;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

   procedure Parse_Command (Command : in String)
   is begin
      Trace.Put_Line ("'" & Command & "'");

      Parser.Lexer.Reset_With_String (Command);
      Parser.Parse;

      Trace.Put_Line ("success");
      Trace.New_Line;
   exception
   when E : others =>
      AUnit.Assertions.Assert
        (False, Command & ": " & Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
   end Parse_Command;

   ----------
   --  Test procedures

   Trace_File : aliased Ada.Text_IO.File_Type;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      Orig_Trace_Parse : constant Integer := WisiToken.Trace_Parse;

      Trace_File_Name : constant String := "association_grammar_test.out";
      Expected_Trace_File_Name : constant String := "../Test/association_grammar_test.out_good";
   begin
      --  The test is that there are no exceptions, and that the parse
      --  trace matches the known good trace.

      if Exists (Trace_File_Name) then
         Delete_File (Trace_File_Name);
      end if;
      Create (Trace_File, Out_File, Trace_File_Name);
      Trace.Set_File (Trace_File'Access);

      WisiToken.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (Trace'Access, Syntax),
         WisiToken.LR.LALR_Generate.Generate (Full_Grammar, LALR_Descriptor),
         User_Data                    => null,
         Language_Fixes               => null,
         Language_Constrain_Terminals => null,
         Language_String_ID_Set       => null);

      WisiToken.Trace_Parse := WisiToken.Detail + 1;

      Parse_Command ("(identifier)");
      Parse_Command ("(identifier, identifier)");
      Parse_Command ("(identifier => identifier)");
      Parse_Command ("(integer => identifier)");
      Parse_Command ("(identifier => identifier, integer => identifier)");

      Trace.Clear_File;
      Close (Trace_File);
      WisiToken.Trace_Parse := Orig_Trace_Parse;

      Check_Files ("1", Trace_File_Name, Expected_Trace_File_Name);
   exception
   when others =>
      if Is_Open (Trace_File) then
         Trace.Clear_File;
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
      return new String'("association_grammar_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Association_Grammar_Test;
