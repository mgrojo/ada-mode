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
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lite_Tree_Sitter_Actions;
with GNAT.Traceback.Symbolic;
with WisiToken.Parse.Tree_Sitter;
procedure Dump_Tree_Sitter_Ada_Lite_Corrected
is
   use WisiToken;

   procedure Put_Usage
   is begin
      Put_Line (Standard_Error, "dump_tree_sitter_ada_lite_corrected <file> [trace_parse]");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;

   Parser : WisiToken.Parse.Tree_Sitter.Parser (Ada_Lite_Tree_Sitter_Actions.Tree_Sitter_Ada_Lite'Access);

   function Image (Node : in WisiToken.Parse.Tree_Sitter.Syntax_Tree_Node) return String
   is
      use Ada_Lite_Tree_Sitter_Actions;

      ID : constant Token_ID := WisiToken.Parse.Tree_Sitter.ID (Node);
   begin
      case To_Token_Enum (ID) is
      when IDENTIFIER_ID =>
         --  workaround a bug in tree-sitter; it sometimes includes the char before the identifier
         declare
            Ident : constant String := Parser.Buffer_Text (WisiToken.Parse.Tree_Sitter.Byte_Region (Node));
         begin
            return "IDENTIFIER " &
              (if Ident (Ident'First) in ' ' | '.'
               then Ident (Ident'First + 1 .. Ident'Last)
               else Ident);
         end;
      when NUMERIC_LITERAL_ID =>
         return "NUMERIC_LITERAL";
      when STRING_LITERAL_ID =>
         return "STRING_LITERAL";
      when others =>
         return Ada_Lite_Tree_Sitter_Actions.Descriptor.Image (ID).all;
      end case;
   end Image;

begin
   declare
      use Ada.Command_Line;
   begin
      Set_Exit_Status (Success);
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      File_Name := +Argument (1);

      if Argument_Count >= 2 then
         WisiToken.Trace_Parse := Integer'Value (Argument (2));
      end if;
   end;

   WisiToken.Parse.Tree_Sitter.Parse (Parser, -File_Name);

   declare
      use WisiToken.Parse.Tree_Sitter;
      Tree : Syntax_Tree renames Syntax_Tree (Parser.Tree);
      Terminals : constant Node_Array := Get_Terminals (Tree.Root, Ada_Lite_Tree_Sitter_Actions.Descriptor);
   begin
      for T of Terminals loop
         Put_Line (Image (T));
      end loop;
   end;

exception
when E : others =>
   Put_Line (Standard_Error, -File_Name & ":1:1");
   Put_Line (Standard_Error,
             "exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Dump_Tree_Sitter_Ada_Lite_Corrected;
--  Local Variables:
--  ada-case-strict: nil
--  End:
