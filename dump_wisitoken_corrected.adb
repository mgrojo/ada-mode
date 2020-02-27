--  Abstract :
--
--  Parse a file with the WisiToken lalr parser, output the corrected token stream.
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Process_Actions;
with Ada_Process_LALR_Main;
with GNAT.Traceback.Symbolic;
with WisiToken.Parse.LR.McKenzie_Recover.Ada;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
procedure Dump_WisiToken_Corrected
is
   use WisiToken;
   use all type Ada_Process_Actions.Token_Enum_ID;

   procedure Put_Usage
   is begin
      Put_Line ("dump_wisitoken_corrected <file> [verbosity]");
   end Put_Usage;

   Trace  : aliased WisiToken.Text_IO_Trace.Trace (Ada_Process_Actions.Descriptor'Unrestricted_Access);
   Parser : WisiToken.Parse.LR.Parser.Parser;

   function Image (Node : in WisiToken.Syntax_Trees.Valid_Node_Index) return String
   is
      use Ada_Process_Actions;
      use WisiToken.Syntax_Trees;
      Image : constant Token_ID_Array_String (+ABS_ID .. +CHARACTER_LITERAL_ID) :=
        (new String'("abs"),
         new String'("accept"),
         new String'("abort"),
         new String'("abstract"),
         new String'("access"),
         new String'("aliased"),
         new String'("all"),
         new String'("and"),
         new String'("array"),
         new String'("at"),
         new String'("begin"),
         new String'("body"),
         new String'("case"),
         new String'("constant"),
         new String'("declare"),
         new String'("delay"),
         new String'("delta"),
         new String'("digits"),
         new String'("do"),
         new String'("else"),
         new String'("elsif"),
         new String'("end"),
         new String'("entry"),
         new String'("exception"),
         new String'("exit"),
         new String'("for"),
         new String'("function"),
         new String'("generic"),
         new String'("goto"),
         new String'("if"),
         new String'("in"),
         new String'("interface"),
         new String'("is"),
         new String'("limited"),
         new String'("loop"),
         new String'("mod"),
         new String'("new"),
         new String'("not"),
         new String'("null"),
         new String'("of"),
         new String'("or"),
         new String'("others"),
         new String'("out"),
         new String'("overriding"),
         new String'("package"),
         new String'("pragma"),
         new String'("private"),
         new String'("procedure"),
         new String'("protected"),
         new String'("raise"),
         new String'("range"),
         new String'("record"),
         new String'("rem"),
         new String'("renames"),
         new String'("requeue"),
         new String'("return"),
         new String'("reverse"),
         new String'("separate"),
         new String'("select"),
         new String'("some"),
         new String'("subtype"),
         new String'("synchronized"),
         new String'("tagged"),
         new String'("task"),
         new String'("terminate"),
         new String'("then"),
         new String'("type"),
         new String'("until"),
         new String'("use"),
         new String'("when"),
         new String'("while"),
         new String'("with"),
         new String'("xor"),
         new String'("("),
         new String'(")"),
         new String'("&"),
         new String'("@"),
         new String'("|"),
         new String'("<>"),
         new String'(":"),
         new String'(":="),
         new String'(","),
         new String'("."),
         new String'(".."),
         new String'("="),
         new String'("=>"),
         new String'(">"),
         new String'(">="),
         new String'(">>"),
         new String'("<"),
         new String'("<="),
         new String'("<<"),
         new String'("-"),
         new String'("+"),
         new String'(";"),
         new String'("/"),
         new String'("/="),
         new String'("*"),
         new String'("**"),
         new String'("'"),
         new String'("'"),
         new String'("NUMERIC_LITERAL"),
         new String'("IDENTIFIER"),
         new String'("STRING_LITERAL"),
         new String'("CHARACTER_LITERAL"));

      Tree     : WisiToken.Syntax_Trees.Tree renames Parser.Tree;
      ID       : constant Token_ID := Tree.ID (Node);
      ID_Image : constant String   := Image (ID).all;
   begin
      if ID = +IDENTIFIER_ID and Tree.Label (Node) = Shared_Terminal then
         declare
            Token : Base_Token renames Parser.Terminals (Tree.Min_Terminal_Index (Node));
         begin
            return ID_Image & " " & Parser.Lexer.Buffer_Text (Token.Byte_Region);
         end;
      else
         return ID_Image;
      end if;
   end Image;

begin
   Ada_Process_LALR_Main.Create_Parser
     (Parser,
      WisiToken.Parse.LR.McKenzie_Recover.Ada.Language_Fixes'Access,
      WisiToken.Parse.LR.McKenzie_Recover.Ada.Matching_Begin_Tokens'Access,
      WisiToken.Parse.LR.McKenzie_Recover.Ada.String_ID_Set'Access,
      Trace'Unrestricted_Access,
      User_Data => null);

   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      declare
            File_Name : constant String := Argument (1);
      begin
         Parser.Lexer.Reset_With_File (File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (Standard_Error, "'" & File_Name & "' cannot be opened");
         Set_Exit_Status (Failure);
         return;
      end;
   end;

   Parser.Parse;

   declare
      Terminals : constant Syntax_Trees.Valid_Node_Index_Array := Parser.Tree.Get_Terminals (Parser.Tree.Root);
   begin
      for T of Terminals loop
         Put_Line (Image (T));
      end loop;
   end;

exception
when E : others =>
   Put_Line ("exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Dump_WisiToken_Corrected;
