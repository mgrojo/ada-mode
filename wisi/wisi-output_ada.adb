--  Abstract :
--
--  Output Ada code implementing the grammar defined by input parameters.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with OpenToken;
procedure Wisi.Output_Ada
  (Input_File_Name  : in String;
   Output_File_Root : in String;
   Copyright        : in String;
   Prologue         : in String_Lists.List;
   Keywords         : in String_Pair_Lists.List;
   Tokens           : in Token_Lists.List;
   Start_Token      : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Rules            : in Rule_Lists.List)
is
   pragma Unreferenced (Prologue);
   Grammar_File : File_Type;
   Exe_File     : File_Type;

   function To_Ada (File_Name : in String) return String
   is
      --  Convert File_Name to Ada name
      use Standard.Ada.Characters.Handling;
      Result : String := File_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '.';
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end To_Ada;

   Grammar_Package_Name : constant String := To_Ada (Output_File_Root);
   Exe_Name             : constant String := To_Ada (Output_File_Root) & ".Parse";

   Indent : Positive_Count := 1;

   procedure Indent_Line (Text : in String)
   is begin
      Set_Col (Indent);
      Put_Line (Text);
   end Indent_Line;

   procedure Put (List : in String_Lists.List)
   is
      --  Put OpenToken syntax for a production
      use Standard.Ada.Strings;
      use Standard.Ada.Strings.Unbounded;
      use String_Lists;
      use type Standard.Ada.Containers.Count_Type;
      Item : Cursor := List.First;
   begin
      for Element of List loop
         Put ("(+" & Element & "_ID)");
         Next (Item);
         exit when Item = No_Element;
         if Col > 100 then
            Put_Line (" &");
            Set_Col (Indent + 2);
         else
            Put (" & ");
         end if;
      end loop;
      if List.Length = 0 then
         Put (" +Self");
      else
         Put (" + Self");
      end if;
   end Put;

begin
   Create (Grammar_File, Out_File, Output_File_Root & ".ads");
   Set_Output (Grammar_File);
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Line ("--  Copyright (C) " & Copyright);
   Put_Line ("--");
   Put_Line ("with Ada.Strings.Maps.Constants;");
   Put_Line ("with OpenToken.Production.List;");
   Put_Line ("with OpenToken.Production.Parser.LALR;");
   Put_Line ("with OpenToken.Recognizer.Integer;");
   Put_Line ("with OpenToken.Recognizer.Character_Set;");
   Put_Line ("with OpenToken.Recognizer.End_Of_File;");
   Put_Line ("with OpenToken.Recognizer.Identifier;");
   Put_Line ("with OpenToken.Recognizer.Keyword;");
   Put_Line ("with OpenToken.Recognizer.Line_Comment;");
   Put_Line ("with OpenToken.Recognizer.Separator;");
   Put_Line ("with OpenToken.Recognizer.String;");
   Put_Line ("with OpenToken.Token.Enumerated;");
   Put_Line ("with OpenToken.Token.Enumerated.Analyzer;");
   Put_Line ("with OpenToken.Token.Enumerated.List;");
   Put_Line ("with OpenToken.Token.Enumerated.Nonterminal;");
   Put_Line ("package " & Grammar_Package_Name & " is");
   Indent := Indent + 3;

   New_Line;
   Indent_Line ("type Token_IDs is");
   Indent_Line ("  (");
   Indent := Indent + 3;
   Indent_Line ("--  non-reporting");
   Indent_Line ("Whitespace_ID,");
   for Kind of Tokens loop
      if -Kind.Kind = """line_comment""" then
         for Item of Kind.Tokens loop
            Indent_Line (-Item.Name & "_ID,");
         end loop;
      end if;
   end loop;
   New_Line;
   Indent_Line ("--  terminals");
   --  Keywords first, so they have precedence over identifiers
   for Item of Keywords loop
      Indent_Line (-Item.Name & "_ID,");
   end loop;
   for Kind of Tokens loop
      if -Kind.Kind /= """line_comment""" then
         for Item of Kind.Tokens loop
            Indent_Line (-Item.Name & "_ID,"); -- avoid collision with Ada reserved words
         end loop;
      end if;
   end loop;
   Indent_Line ("EOF_ID,");
   New_Line;
   Indent_Line ("--  non-terminals");
   declare
      use Rule_Lists;
      I : Cursor := Rules.First;
   begin
      --  Add an extra nonterminal as the OpenToken accept symbol
      --  followed by EOF.
      Indent_Line ("opentoken_accept_ID,");

      loop
         Set_Col (Indent);
         Put (-Element (I).Left_Hand_Side);
         Next (I);
         if I = No_Element then
            Put_Line ("_ID);");
            exit;
         else
            Put_Line ("_ID,");
         end if;
      end loop;
   end;
   Indent := Indent - 3;

   Indent_Line
     ("package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);");
   Indent_Line ("package Analyzers is new Tokens_Pkg.Analyzer");
   Indent_Line ("  (First_Terminal => " & (-Keywords.First_Element.Name) & "_ID,");
   Indent_Line ("   Last_Terminal  => EOF_ID);");
   Indent_Line ("package Token_Lists is new Tokens_Pkg.List;");
   Indent_Line ("package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);");
   Indent_Line ("package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);");
   Indent_Line ("package Production_Lists is new Productions.List;");
   Indent_Line ("package Parsers is new Productions.Parser (Production_Lists, Analyzers);");
   Indent_Line ("package LALR_Parsers is new Parsers.LALR;");
   New_Line;
   Indent_Line ("Syntax : constant Analyzers.Syntax :=");
   Indent_Line ("  (Whitespace_ID  => Analyzers.Get");
   Indent_Line ("     (OpenToken.Recognizer.Character_Set.Get");
   Indent_Line ("        (OpenToken.Recognizer.Character_Set.Standard_Whitespace)),");
   Indent := Indent + 3;
   for Item of Keywords loop
      Indent_Line
        (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.Keyword.Get (" & (-Item.Value) & ")),");
   end loop;
   for Kind of Tokens loop
      if -Kind.Kind = """line_comment""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.Line_Comment.Get (" & (-Item.Value) & ")),");
         end loop;
      elsif -Kind.Kind = """number""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.Integer.Get),");
         end loop;
      elsif -Kind.Kind = """punctuation""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.Separator.Get (" & (-Item.Value) & ")),");
         end loop;
      elsif -Kind.Kind = """symbol""" then
         for Item of Kind.Tokens loop
            Indent_Line (-Item.Name & "_ID => Analyzers.Get");
            Indent_Line ("  (OpenToken.Recognizer.Identifier.Get");
            Indent_Line ("     (Start_Chars => Ada.Strings.Maps.Constants.Letter_Set,");
            Indent_Line ("      Body_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set)),");
         end loop;
      elsif -Kind.Kind = """string""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.String.Get),");
         end loop;
      else
         raise OpenToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
      end if;
   end loop;
   Indent_Line ("EOF_ID => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get));");
   New_Line;

   Indent := Indent - 3;

   Indent_Line ("function ""+""");
   Indent_Line ("  (Item : in Token_IDs)");
   Indent_Line ("  return Tokens_Pkg.Instance'Class");
   Indent_Line ("renames Tokens_Pkg.""+"";");
   New_Line;

   Indent_Line ("--  Allow infix operators for building productions");
   Indent_Line ("use type Token_Lists.Instance;");
   Indent_Line ("use type Productions.Right_Hand_Side;");
   Indent_Line ("use type Productions.Instance;");
   Indent_Line ("use type Production_Lists.Instance;");
   New_Line;

   Indent_Line ("Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;");
   New_Line;

   Indent_Line ("Grammar : constant Production_Lists.Instance :=");
   Indent := Indent + 2;
   declare
      Rule_Cursor : Rule_Lists.Cursor := Rules.First;
      use type Rule_Lists.Cursor;
   begin
      --  Add the required OpenToken accept production:
      Indent_Line
        ("Nonterminals.Get (opentoken_accept_ID) <= Nonterminals.Get (" & (-Start_Token) &
           "_ID) & (+EOF_ID) and");
      loop
         declare
            use type RHS_Lists.Cursor;
            Rule       : constant Rule_Lists.Constant_Reference_Type := Rules.Constant_Reference (Rule_Cursor);
            RHS_Cursor : Wisi.RHS_Lists.Cursor                       := Rule.Right_Hand_Sides.First;
         begin
            if RHS_Cursor = Wisi.RHS_Lists.No_Element then
               Put_Line
                 (Standard_Error, Input_File_Name & ":0:0: no productions for rule '" & (-Rule.Left_Hand_Side) & "'");
            else
               loop
                  Set_Col (Indent);
                  Put ("Nonterminals.Get (" & (-Rule.Left_Hand_Side) & "_ID) <= ");
                  declare
                     RHS : constant RHS_Lists.Constant_Reference_Type :=
                       Rule.Right_Hand_Sides.Constant_Reference (RHS_Cursor);
                  begin
                     Put (RHS.Production);
                     Wisi.RHS_Lists.Next (RHS_Cursor);
                     if RHS_Cursor = Wisi.RHS_Lists.No_Element then
                        exit;
                     else
                        Indent_Line ("and");
                     end if;
                  end;
               end loop;
            end if;
         end;
         Rule_Lists.Next (Rule_Cursor);
         if Rule_Cursor = Rule_Lists.No_Element then
            Indent_Line (";");
            exit;
         else
            Indent_Line ("and");
         end if;
      end loop;
   end;

   Put_Line ("end " & Grammar_Package_Name & ";");
   Close (Grammar_File);

   Create (Exe_File, Out_File, Output_File_Root & "-parse.adb");
   Set_Output (Exe_File);
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Line ("--  Copyright (C) " & Copyright);
   Put_Line ("--");
   Put_Line ("with Ada.Command_Line;");
   Put_Line ("with Ada.Directories;");
   Put_Line ("with Ada.Exceptions;");
   Put_Line ("with Ada.Text_IO; use Ada.Text_IO;");
   Put_Line ("with OpenToken.Text_Feeder.Text_IO;");
   Put_Line ("procedure " & Exe_Name & " is");
   New_Line;
   Put_Line ("   procedure Put_Usage");
   Put_Line ("   is begin");
   Put_Line ("      Put_Line (""" & Output_File_Root & "-parse [-v verbosity] filename"");");
   Put_Line ("      Put_Line (""  verbosity 0 (default); only grammar action output"");");
   Put_Line ("      Put_Line (""  verbosity 1 : also output parse table, and trace of states while parsing"");");
   Put_Line ("      Put_Line (""  verbosity 2 : also output grammar building debug info"");");
   Put_Line ("   end Put_Usage;");
   New_Line;
   Put_Line ("   Input_File  : aliased File_Type;");
   Put_Line ("   Verbosity   : Integer := 0;");
   Put_Line ("   Parser      : LALR_Parsers.Instance;");
   New_Line;
   Put_Line ("   procedure Use_File (File_Name : in String)");
   Put_Line ("   is begin");
   Put_Line ("      Open (Input_File, In_File, File_Name);");
   Put_Line ("   exception");
   Put_Line ("   when Name_Error =>");
   Put_Line ("      Put_Line (File_Name & "" cannot be opened"");");
   Put_Line ("      raise OpenToken.User_Error;");
   Put_Line ("   end Use_File;");
   New_Line;
   Put_Line ("begin");
   Put_Line ("   declare");
   Put_Line ("      use Ada.Command_Line;");
   Put_Line ("   begin");
   Put_Line ("      case Argument_Count is");
   Put_Line ("      when 1 =>");
   Put_Line ("         Use_File (Argument (1));");
   New_Line;
   Put_Line ("      when 3 =>");
   Put_Line ("         if Argument (1) = ""-v"" then");
   Put_Line ("            Verbosity := Integer'Value (Argument (2));");
   Put_Line ("            OpenToken.Trace_Parse := Verbosity > 0;");
   New_Line;
   Put_Line ("         else");
   Put_Line ("            Set_Exit_Status (Failure);");
   Put_Line ("            Put_Usage;");
   Put_Line ("            return;");
   Put_Line ("         end if;");
   New_Line;
   Put_Line ("         Use_File (Argument (3));");
   New_Line;
   Put_Line ("      when others =>");
   Put_Line ("         Set_Exit_Status (Failure);");
   Put_Line ("         Put_Usage;");
   Put_Line ("         return;");
   Put_Line ("      end case;");
   Put_Line ("   exception");
   Put_Line ("   when others =>");
   Put_Line ("      Set_Exit_Status (Failure);");
   Put_Line ("      Put_Usage;");
   Put_Line ("      return;");
   Put_Line ("   end;");
   New_Line;
   Put_Line ("   Parser := LALR_Parsers.Generate");
   Put_Line ("     (Grammar, Analyzers.Initialize");
   Put_Line ("       (Syntax, OpenToken.Text_Feeder.Text_IO.Create (Input_File'Unchecked_Access)),");
   Put_Line ("      Trace       => Verbosity > 1,");
   Put_Line ("      Put_Grammar => Verbosity > 0);");
   New_Line;
   Put_Line ("   Parser.Parse;");
   Put_Line ("exception");
   Put_Line ("when E : OpenToken.Grammar_Error =>");
   Put_Line ("   Put_Line (Standard_Error, Ada.Exceptions.Exception_Message (E));");
   Put_Line ("   Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);");
   Put_Line ("when E : OpenToken.Syntax_Error =>");
   Put_Line ("   Put_Line");
   Put_Line ("     (Standard_Error, Ada.Directories.Simple_Name (Name (Input_File)) & ':' &");
   Put_Line ("      Ada.Exceptions.Exception_Message (E));");
   Put_Line ("   Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);");
   Put_Line ("when End_Error =>");
   Put_Line ("   null;");
   Put_Line ("end " & Exe_Name & ";");
   Close (Exe_File);
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada;
