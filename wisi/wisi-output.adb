--  Abstract :
--
--  Output Ada code implementing the grammar defined by Declarations,
--  Rules.
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
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
procedure Wisi.Output
  (Input_File_Name  : in String;
   Output_File_Root : in String;
   Prologue         : in String_Lists.List;
   Keywords         : in String_Pair_Lists.List;
   Tokens           : in String_Pair_Lists.List;
   Rules            : in Rule_Lists.List)
is
   Output_File : File_Type;

   function To_Ada (Item : in String) return String
   is
      use Ada.Characters.Handling;
      Result : String := Item;
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

   procedure Create_Parents (Name : in String)
   is
      use Ada.Strings.Fixed;
      Last  : Integer := Index (Pattern => "-", Source => Name);
      File  : File_Type;
   begin
      --  The parent package is 'wisi'; we need the real wisi.ads, so don't generate it
      Last := -1 + Index (Pattern => "-", Source => Name, From => Last + 1);
      loop
         Create (File, Out_File, Name (Name'First .. Last) & ".ads");
         Put_Line (File, "package " & To_Ada (Name (Name'First .. Last)) & " is");
         Put_Line (File, "end " & To_Ada (Name (Name'First .. Last)) & ";");
         Close (File);
         exit when Last = Name'Last;
         Last := Index (Pattern => "-", Source => Name, From => Last + 2);
         if Last = 0 then
            Last := Name'Last;
         else
            Last := Last - 1;
         end if;
      end loop;
   end Create_Parents;

   Package_Name : constant String := "Wisi." & To_Ada (Output_File_Root) & ".Generate";

   Indent : Positive_Count := 1;

   procedure Indent_Line (Text : in String)
   is begin
      Set_Col (Indent);
      Put_Line (Text);
   end Indent_Line;

   procedure Put (List : in String_Lists.List)
   is
      use Ada.Strings.Unbounded;
      use String_Lists;
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
   end Put;

   procedure Put (Item : in String_Pair_Type)
   is begin
      Put ("(+""" & (-Item.Name) & """, +" & (-Item.Value) & ")");
   end Put;

begin
   Create_Parents ("wisi-" & Output_File_Root);
   Create (Output_File, Out_File, "wisi-" & Output_File_Root & "-generate.adb");
   Set_Output (Output_File);
   Put_Line ("with Ada.Command_Line;");
   Put_Line ("with Ada.Text_IO;");
   Put_Line ("with OpenToken.Production.List;");
   Put_Line ("with OpenToken.Production.Parser.LALR.Elisp;");
   Put_Line ("with OpenToken.Token.Enumerated;");
   Put_Line ("with OpenToken.Token.Enumerated.Analyzer;");
   Put_Line ("with OpenToken.Token.Enumerated.List;");
   Put_Line ("with OpenToken.Token.Enumerated.Nonterminal;");
   Put_Line ("procedure " & Package_Name);
   Put_Line ("is");
   Indent := Indent + 3;

   Indent_Line ("procedure Put_Usage");
   Indent_Line ("is begin");
   Indent_Line ("   Standard.Ada.Text_IO.Put_Line (""usage: wisi-ada-subset-generate [-v]"");");
   Indent_Line ("end Put_Usage;");

   Indent_Line ("Elisp_Package : constant String := """ & Output_File_Root & """;");

   Indent_Line ("Prologue : constant String :=");

   declare
      use String_Lists;
      Line : Cursor := Prologue.First;
   begin
      loop
         Set_Col (Indent);
         Put ('"' & Element (Line) & '"');
         Next (Line);

         if Line = No_Element then
            Put_Line (";");
            exit;
         else
            Put_Line ("&");
         end if;
      end loop;
   end;

   New_Line;
   Indent_Line ("Keywords : String_Pair_Lists.List;");
   Indent_Line ("Tokens   : String_Pair_Lists.List;");

   New_Line;
   Indent_Line ("type Token_IDs is");
   Indent_Line ("  (");
   Indent := Indent + 3;
   for Item of Keywords loop
      Indent_Line (-Item.Name & "_ID,"); -- avoid collision with Ada reserved words
   end loop;
   for Item of Tokens loop
      Indent_Line (-Item.Name & "_ID,"); -- avoid collision with Ada reserved words
   end loop;
   Indent_Line ("--  non-terminals");
   declare
      use Rule_Lists;
      I : Cursor := Rules.First;
   begin
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

   Indent_Line ("package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs);");
   Indent_Line ("--  we only need Analyzers to instantiate Parsers; we will never call it");
   Indent_Line ("package Analyzers is new Tokens_Pkg.Analyzer (Token_IDs'First);");
   Indent_Line ("package Token_Lists is new Tokens_Pkg.List;");
   Indent_Line ("package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);");
   Indent_Line ("package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);");
   Indent_Line ("package Production_Lists is new Productions.List;");
   Indent_Line ("package Parsers is new Productions.Parser (Production_Lists, Analyzers);");
   Indent_Line ("package LALR_Parsers is new Parsers.LALR;");
   New_Line;
   Indent_Line ("--  Allow infix operators for building productions");
   Indent_Line ("use type Token_Lists.Instance;");
   Indent_Line ("use type Productions.Right_Hand_Side;");
   Indent_Line ("use type Productions.Instance;");
   Indent_Line ("use type Production_Lists.Instance;");
   New_Line;

   Indent_Line ("function ""+"" (Item : in Token_IDs) return Tokens_Pkg.Instance'Class");
   Indent_Line ("is begin");
   Indent := Indent + 3;
   Indent_Line ("return Tokens_Pkg.Get (Item);");
   Indent := Indent - 3;
   Indent_Line ("end ""+"";");
   New_Line;

   Indent_Line ("Grammar : constant Production_Lists.Instance :=");
   Indent := Indent + 2;
   declare
      use Rule_Lists;
      Rule_Cursor : Cursor := Rules.First;
   begin
      loop
         declare
            Rule       : constant Constant_Reference_Type := Rules.Constant_Reference (Rule_Cursor);
            Production : Wisi.Production_Lists.Cursor     := Rule.Right_Hand_Side.First;
            use type Wisi.Production_Lists.Cursor;
         begin
            if Production = Wisi.Production_Lists.No_Element then
               Put_Line
                 (Standard_Error, Input_File_Name & ":0:0: no productions for rule '" & (-Rule.Left_Hand_Side) & "'");
            else
               loop
                  Set_Col (Indent);
                  Put ("Nonterminals.Get (" & (-Rule.Left_Hand_Side) & "_ID) <= ");
                  Put (Wisi.Production_Lists.Element (Production));
                  Wisi.Production_Lists.Next (Production);
                  if Production = Wisi.Production_Lists.No_Element then
                     exit;
                  else
                     Indent_Line ("and");
                  end if;
               end loop;
            end if;
         end;
         Next (Rule_Cursor);
         if Rule_Cursor = No_Element then
            Indent_Line (";");
            exit;
         else
            Indent_Line ("and");
         end if;
      end loop;
   end;
   Indent := Indent - 2;

   Indent_Line ("Parser : LALR_Parsers.Instance;");

   New_Line;
   Indent_Line ("package Elisp is new LALR_Parsers.Elisp;");
   New_Line;
   Indent_Line ("Verbose : Boolean := False;");

   Put_Line ("begin");
   Indent_Line ("declare");
   Indent_Line ("   use Standard.Ada.Command_Line;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("case Argument_Count is");
   Indent_Line ("when 0 =>");
   Indent_Line ("   null;");
   New_Line;
   Indent_Line ("when 1 =>");
   Indent := Indent + 3;
   Indent_Line ("if Argument (1) = ""-v"" then");
   Indent_Line ("   Verbose := True;");
   Indent_Line ("else");
   Indent := Indent + 3;
   Indent_Line ("Set_Exit_Status (Failure);");
   Indent_Line ("Put_Usage;");
   Indent_Line ("return;");
   Indent := Indent - 3;
   Indent_Line ("end if;");
   Indent := Indent - 3;
   New_Line;
   Indent_Line ("when others =>");
   Indent := Indent + 3;
   Indent_Line ("Set_Exit_Status (Failure);");
   Indent_Line ("Put_Usage;");
   Indent_Line ("return;");
   Indent := Indent - 3;
   Indent_Line ("end case;");
   Indent := Indent - 3;
   Indent_Line ("end;");

   Indent_Line ("Parser := LALR_Parsers.Generate (Grammar, Analyzers.Null_Analyzer, Verbose);");

   for Item of Keywords loop
      Set_Col (Indent);
      Put ("Keywords.Append (");
      Put (Item);
      Put_Line (");");
   end loop;
   for Item of Tokens loop
      Set_Col (Indent);
      Put ("Tokens.Append (");
      Put (Item);
      Put_Line (");");
   end loop;
   Indent_Line ("Elisp.Output (Elisp_Package, Prologue, Keywords, Tokens, Parser);");
   Put_Line ("end " & Package_Name & ";");
   Close (Output_File);
end Wisi.Output;
