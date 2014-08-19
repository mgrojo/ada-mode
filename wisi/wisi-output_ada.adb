--  Abstract :
--
--  Output Ada code implementing the grammar defined by input parameters.
--
--  Copyright (C) 2012 - 2014 Stephen Leake.  All Rights Reserved.
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
with OpenToken.Production.Put_Ada;
with OpenToken;
with Wisi.Gen_Generate_Utils;
with Wisi.Utils;
procedure Wisi.Output_Ada
  (Input_File_Name  : in String;
   Output_File_Root : in String;
   Prologue         : in String_Lists.List;
   Keywords         : in String_Pair_Lists.List;
   Tokens           : in Token_Lists.List;
   Start_Token      : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts        : in Conflict_Lists.List;
   Rules            : in Rule_Lists.List)
is
   EOI_Image : constant String := "EOF";

   package Generate_Utils is new Wisi.Gen_Generate_Utils (Keywords, Tokens, Conflicts, Rules, EOI_Image);

   type Action_Name_List is array (Integer range <>) of access constant String;
   type Action_Name_List_Access is access Action_Name_List;

   Empty_Action : constant access constant String := new String'("Self");
   Action_Names : array (Generate_Utils.Token_IDs) of Action_Name_List_Access;

   function Token_Name (ID : in Generate_Utils.Token_IDs) return String
   is begin
      return Generate_Utils.Token_Image (ID) & "_ID";
   end Token_Name;

   function Action_Name (Item : in Generate_Utils.Token_IDs; Index : in Integer) return String
   is begin
      return Action_Names (Item) (Index).all;
   exception
   when others =>
      Wisi.Utils.Put_Error
        (Input_File_Name,
         1,
         "Name for '" & Generate_Utils.Token_Image (Item) & "'," & Integer'Image (Index) & " not defined.");
      raise Programmer_Error;
   end Action_Name;

   package Production_Put_Ada is new Generate_Utils.Productions.Put_Ada (Token_Name, Action_Name);

   Grammar : constant Generate_Utils.Production_Lists.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);
   Parser  : constant Generate_Utils.LALR_Parsers.Instance := Generate_Utils.LALR_Parsers.Generate
     (Grammar,
      Generate_Utils.Analyzers.Null_Analyzer,
      Generate_Utils.To_Conflicts,
      Trace                    => Verbosity > 1,
      Put_Grammar              => Verbosity > 0,
      Ignore_Unused_Tokens     => Verbosity > 1,
      Ignore_Unknown_Conflicts => Verbosity > 1);

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
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end To_Ada;

   Package_Name : constant String := To_Ada (Output_File_Root);

   procedure Put_Prologue
   is
   begin
      for Line of Prologue loop
         if Line'Length = 2 and then Line = ";;" then
            Put_Line ("--");
         elsif Line'Length > 2 and then Line (Line'First .. Line'First + 2) = ";; " then
            Put_Line ("--  " & Line (Line'First + 2 .. Line'Last));
         elsif Line'Length > 0 and then Line (Line'First) = '(' then
            null;
         else
            Put_Line (Line);
         end if;
      end loop;
   end Put_Prologue;

   procedure Put (Action : in Generate_Utils.LALR_Parsers.Parse_Action_Rec)
   is
      use Generate_Utils.LALR_Parsers;
   begin
      case Action.Verb is
      when Shift =>
         Put (" (Shift");
         Put ("," & State_Index'Image (Action.State));
      when Reduce | Accept_It =>
         if Action.Verb = Reduce then
            Put (" (Reduce");
         else
            Put (" (Accept_It");
         end if;
         Put (", ");
         Production_Put_Ada.Put (Action.Production);
         Put ("," & Integer'Image (Action.Length));
      when Error =>
         Put (" (Verb => Error");
         null;
      end case;
      Put (")");
   end Put;

   use Generate_Utils;
   Spec_File : File_Type;
   Body_File : File_Type;

begin
   if Verbosity > 0 then
      Generate_Utils.Put_Tokens;
   end if;

   Create (Spec_File, Out_File, Output_File_Root & ".ads");
   Set_Output (Spec_File);
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Prologue;

   if Is_In (Tokens, """symbol""") then
      Put_Line ("with Ada.Strings.Maps.Constants;");
   end if;

   Put_Line ("with OpenToken.Production.List;");
   Put_Line ("with OpenToken.Production.Parser.LALR;");
   Put_Line ("with OpenToken.Recognizer.Character_Set;");
   Put_Line ("with OpenToken.Recognizer.End_Of_File;");

   if Is_In (Tokens, """symbol""") then
      Put_Line ("with OpenToken.Recognizer.Identifier;");
   end if;

   if Is_In (Tokens, """number""") then
      Put_Line ("with OpenToken.Recognizer.Integer;");
   end if;

   Put_Line ("with OpenToken.Recognizer.Keyword;");

   if Is_In (Tokens, """line_comment""") then
      Put_Line ("with OpenToken.Recognizer.Line_Comment;");
   end if;

   if Is_In (Tokens, """punctuation""") then
      Put_Line ("with OpenToken.Recognizer.Separator;");
   end if;

   if Is_In (Tokens, """string-double""") then
      Put_Line ("with OpenToken.Recognizer.String;");
   end if;

   if Is_In (Tokens, """string-single""") then
      Put_Line ("with OpenToken.Recognizer.Graphic_Character;");
   end if;

   Put_Line ("with OpenToken.Token.Enumerated;");
   Put_Line ("with OpenToken.Token.Enumerated.Analyzer;");
   Put_Line ("with OpenToken.Token.Enumerated.List;");
   Put_Line ("with OpenToken.Token.Enumerated.Nonterminal;");
   Put_Line ("package " & Package_Name & " is");
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
   Indent_Line (EOI_Image & "_ID,");
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
   Indent_Line
     ("package LALR_Parsers is new Parsers.LALR (First_State_Index => " &
        State_Image (LALR_Parsers.State_Index'First) & ");");
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
      elsif -Kind.Kind = """string-double""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.String.Get),");
         end loop;
      elsif -Kind.Kind = """string-single""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (-Item.Name & "_ID => Analyzers.Get (OpenToken.Recognizer.Graphic_Character.Get),");
         end loop;
      else
         raise OpenToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
      end if;
   end loop;
   Indent_Line ("EOF_ID => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get));");
   New_Line;
   Indent := Indent - 3;

   Put_Line ("function Create_Parser return LALR_Parsers.Instance;");
   New_Line;
   Put_Line ("Parser : LALR_Parsers.Instance := Create_Parser;");
   New_Line;

   Put_Line ("end " & Package_Name & ";");
   Close (Spec_File);

   Create (Body_File, Out_File, Output_File_Root & ".adb");
   Set_Output (Body_File);
   Indent := 1;
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Prologue;
   Put_Line ("with Ada.Containers.Indefinite_Doubly_Linked_Lists;");
   Put_Line ("with Ada.Text_IO; use Ada.Text_IO;");
   Put_Line ("package body " & Package_Name & " is");
   Indent := Indent + 3;
   New_Line;

   Indent_Line ("Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;");
   New_Line;

   Action_Names (Find_Token_ID ("opentoken_accept")) := new Action_Name_List (0 .. 0);
   Action_Names (Find_Token_ID ("opentoken_accept")) (0) := Empty_Action;

   Indent_Line ("package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);");
   New_Line;
   Indent_Line ("Actions : String_Lists.List;");
   New_Line;

   for Rule of Rules loop
      declare
         use type Ada.Containers.Count_Type;
         LHS_ID : constant Token_IDs := Find_Token_ID (-Rule.Left_Hand_Side);
         Index  : Integer            := 0; -- Matches Generate_Utils.To_Grammar
      begin
         if Action_Names (LHS_ID) = null then
            Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
         end if;

         for RHS of Rule.Right_Hand_Sides loop
            if RHS.Action.Length > 0 then
               declare
                  Name : constant String := -Rule.Left_Hand_Side & '_' & Int_Image (Index);
               begin
                  Action_Names (LHS_ID) (Index) := new String'(Name & "'Access");

                  Indent_Line ("procedure " & Name);
                  Indent := Indent + 2;
                  Indent_Line ("(New_Token : out Nonterminals.Class;");
                  Indent := Indent + 1;
                  Indent_Line ("Source    : in  Token_Lists.Instance'Class;");
                  Indent_Line ("To_ID     : in  Token_IDs)");
                  Indent := Indent - 3;
                  Indent_Line ("is");
                  Indent_Line ("   pragma Unreferenced (Source);");
                  Indent_Line ("begin");
                  Indent := Indent + 3;
                  Indent_Line ("New_Token := Nonterminals.Get (To_ID);");
                  --  Assuming simple elisp syntax for actions, as used by wisi elisp parser
                  --  Just accumulate elisp sexp to pass back to Emacs
                  for Line of RHS.Action loop
                     Indent_Line ("Actions.Append (""" & Line & """);");
                  end loop;
                  Indent := Indent - 3;
                  Indent_Line ("end " & Name & ";");
                  New_Line;
               end;
            else
               Action_Names (LHS_ID) (Index) := Empty_Action;
            end if;

            Index := Index + 1;
         end loop;
      end;
   end loop;

   Indent_Line ("procedure Add_Action");
   Indent := Indent + 2;
   Indent_Line ("(State  : in out LALR_Parsers.Parse_State;");
   Indent := Indent + 1;
   Indent_Line ("Symbol : in     Token_IDs;");
   Indent_Line ("Action : in     LALR_Parsers.Parse_Action_Rec)");
   Indent := Indent - 3;
   Indent_Line ("is");
   Indent_Line ("   use LALR_Parsers;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line
     ("State.Action_List := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), State.Action_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Action;");
   New_Line;

   Indent_Line ("procedure Add_Action");
   Indent := Indent + 2;
   Indent_Line ("(State    : in out LALR_Parsers.Parse_State;");
   Indent := Indent + 1;
   Indent_Line ("Symbol   : in     Token_IDs;");
   Indent_Line ("Action_1 : in     LALR_Parsers.Parse_Action_Rec;");
   Indent_Line ("Action_2 : in     LALR_Parsers.Parse_Action_Rec)");
   Indent := Indent - 3;
   Indent_Line ("is");
   Indent_Line ("   use LALR_Parsers;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("State.Action_List := new Action_Node'");
   Indent_Line
     ("  (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Action;");
   New_Line;

   Indent_Line ("procedure Add_Reduction");
   Indent := Indent + 2;
   Indent_Line ("(State    : in out LALR_Parsers.Parse_State;");
   Indent := Indent + 1;
   Indent_Line ("Symbol   : in     Token_IDs;");
   Indent_Line ("To_State : in     LALR_Parsers.State_Index)");
   Indent := Indent - 3;
   Indent_Line ("is");
   Indent_Line ("   use LALR_Parsers;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("State.Reduction_List := new Reduction_Node'(Symbol, To_State, State.Reduction_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Reduction;");
   New_Line;

   Indent_Line ("function Create_Parser return LALR_Parsers.Instance");
   Indent_Line ("is");
   Indent := Indent + 3;
   Indent_Line ("use LALR_Parsers;");
   Indent_Line ("use Productions;");
   Indent_Line
     ("Table : constant Parse_Table_Ptr := new Parse_Table (" &
        State_Image (Parser.Table'First) & " .. " & State_Image (Parser.Table'Last) & ");");
   Indent := Indent - 3;
   Indent_Line ("begin");
   Indent := Indent + 3;
   for State_Index in Parser.Table.all'Range loop
      Actions :
      declare
         use Generate_Utils.LALR_Parsers;
         Node : Action_Node_Ptr := Parser.Table (State_Index).Action_List;
      begin
         loop
            exit when Node = null;
            Set_Col (Indent);
            Put ("Add_Action (Table (" & State_Image (State_Index) & "), ");
            Put (Token_Name (Node.Symbol) & ",");
            declare
               Action_Node : Parse_Action_Node_Ptr := Node.Action;
            begin
               loop
                  exit when Action_Node = null;
                  Put (Action_Node.Item);
                  Action_Node := Action_Node.Next;
                  if Action_Node /= null then
                     Put (",");
                  end if;
               end loop;
            end;
            Put_Line (");");
            Node := Node.Next;
         end loop;
      end Actions;

      Reductions :
      declare
         use Generate_Utils.LALR_Parsers;
         Node : Reduction_Node_Ptr := Parser.Table (State_Index).Reduction_List;
      begin
         loop
            exit when Node = null;
            Set_Col (Indent);
            Put ("Add_Reduction (Table (" & State_Image (State_Index) & "), ");
            Put_Line (Token_Name (Node.Symbol) & ", " & State_Image (Node.State) & ");");
            Node := Node.Next;
         end loop;
      end Reductions;
   end loop;
   New_Line;
   Indent_Line ("return (Analyzers.Initialize (Syntax, null), Table);");
   Indent := Indent - 3;
   Indent_Line ("end Create_Parser;");
   Put_Line ("end " & Package_Name & ";");
   Close (Body_File);
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada;
