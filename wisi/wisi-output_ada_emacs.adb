--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, with actions suitable for the Emacs Ada mode
--  indentation engine.
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
with OpenToken;
with Wisi.Gen_Generate_Utils;
with Wisi.Utils;
procedure Wisi.Output_Ada_Emacs
  (Input_File_Name  : in String;
   Output_File_Root : in String;
   Prologue         : in String_Lists.List;
   Keywords         : in String_Pair_Lists.List;
   Tokens           : in Token_Lists.List;
   Start_Token      : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts        : in Conflict_Lists.List;
   Rules            : in Rule_Lists.List;
   Rule_Count       : in Integer;
   Action_Count     : in Integer)
is
   use type Ada.Containers.Count_Type;

   EOI_Image              : constant String := "EOF_ID";
   OpenToken_Accept_Image : constant String := "OPENTOKEN_ACCEPT_ID";

   First_State_Index : constant := 0;
   --  Match wisi-output_elisp.adb, to allow comparing parse traces.

   function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      --  Add "_ID" to avoid collision with Ada reserved words
      return Ada.Characters.Handling.To_Upper (-Item) & "_ID";
   end To_Token_Image;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Image, OpenToken_Accept_Image, First_State_Index,
      To_Token_Image    => To_Token_Image);

   type Action_Name_List is array (Integer range <>) of access constant String;
   type Action_Name_List_Access is access Action_Name_List;

   Empty_Action : constant access constant String := new String'("Self");
   Action_Names : array (Generate_Utils.Token_IDs) of Action_Name_List_Access;

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

   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production_Lists.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LALRs.Parse_Table_Ptr := Generate_Utils.LALR_Generators.Generate
     (Grammar,
      Generate_Utils.To_Conflicts (Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
      Trace                    => Verbosity > 1,
      Put_Parse_Table          => Verbosity > 0,
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

   use Generate_Utils;

   Spec_File         : File_Type;
   Body_File         : File_Type;
   Table_Entry_Count : Integer := 0;
begin
   Create (Spec_File, Out_File, Output_File_Root & ".ads");
   Set_Output (Spec_File);
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Prologue;

   Put_Line ("with OpenToken.Production.List;");
   Put_Line ("with OpenToken.Production.Parser.LALR.Generator;");
   Put_Line ("with OpenToken.Production.Parser.LALR.Parser;");
   Put_Line ("with OpenToken.Text_Feeder;");

   Put_Line ("with OpenToken.Token.Enumerated.Analyzer;");
   Put_Line ("with OpenToken.Token.Enumerated.List;");
   Put_Line ("with OpenToken.Token.Enumerated.Nonterminal;");
   Put_Line ("with OpenToken.Wisi_Tokens;");
   Put_Line ("package " & Package_Name & " is");
   Indent := Indent + 3;

   New_Line;
   Indent_Line ("type Token_IDs is");
   Indent_Line ("  (");
   Indent := Indent + 3;
   Indent_Line ("--  non-reporting");
   for Kind of Tokens loop
      if -Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""" then
         for Item of Kind.Tokens loop
            Indent_Line (To_Token_Image (Item.Name) & ",");
         end loop;
      end if;
   end loop;
   New_Line;
   Indent_Line ("--  terminals");
   --  Keywords first, so they have precedence over identifiers
   for Item of Keywords loop
      Indent_Line (To_Token_Image (Item.Name) & ",");
   end loop;
   for Kind of Tokens loop
      if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
         for Item of Kind.Tokens loop
            Indent_Line (To_Token_Image (Item.Name) & ",");
         end loop;
      end if;
   end loop;
   Indent_Line (EOI_Image & ",");
   New_Line;
   Indent_Line ("--  non-terminals");
   declare
      use Rule_Lists;
      I : Cursor := Rules.First;
   begin
      Indent_Line (OpenToken_Accept_Image & ",");
      loop
         Set_Col (Indent);
         Put (To_Token_Image (Element (I).Left_Hand_Side));
         Next (I);
         if I = No_Element then
            Put_Line (");");
            exit;
         else
            Put_Line (",");
         end if;
      end loop;
   end;
   Indent := Indent - 3;
   New_Line;

   Indent_Line ("First_Terminal : constant Token_IDs := " & To_Token_Image (Keywords.First_Element.Name) & ";");
   Indent_Line ("Last_Terminal : constant Token_IDs := EOF_ID;");

   declare
      use Ada.Strings.Unbounded;
      Token_Image_Width : Integer := 0;
   begin
      Indent_Line ("Token_Images : constant array (Token_IDs) of access constant String :=");
      Indent_Line ("  (");
      Indent := Indent + 3;
      Indent_Line ("--  non-reporting");
      for Kind of Tokens loop
         if -Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""" then
            for Item of Kind.Tokens loop
               Indent_Line ("new String'(""" & To_String (Item.Name) & """),");
               Token_Image_Width := Integer'Max (Token_Image_Width, Length (Item.Name));
            end loop;
         end if;
      end loop;
      New_Line;
      Indent_Line ("--  terminals");
      --  Keywords first, so they have precedence over identifiers
      for Item of Keywords loop
         Indent_Line ("new String'(""" & To_String (Item.Name) & """),");
         Token_Image_Width := Integer'Max (Token_Image_Width, Length (Item.Name));
      end loop;
      for Kind of Tokens loop
         if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
            for Item of Kind.Tokens loop
               Indent_Line ("new String'(""" & To_String (Item.Name) & """),");
               Token_Image_Width := Integer'Max (Token_Image_Width, Length (Item.Name));
            end loop;
         end if;
      end loop;
      Indent_Line ("new String'(""" & EOI_Image & """),");
      Token_Image_Width := Integer'Max (Token_Image_Width, EOI_Image'Length);
      New_Line;
      Indent_Line ("--  non-terminals");
      declare
         use Rule_Lists;
         I : Cursor := Rules.First;
      begin
         Indent_Line ("new String'(""" & OpenToken_Accept_Image & """),");
         Token_Image_Width := Integer'Max (Token_Image_Width, OpenToken_Accept_Image'Length);
         loop
            Set_Col (Indent);
            Put ("new String'(""" & To_String (Element (I).Left_Hand_Side));
            Token_Image_Width := Integer'Max (Token_Image_Width, Length (Element (I).Left_Hand_Side));
            Next (I);
            if I = No_Element then
               Put_Line ("""));");
               exit;
            else
               Put_Line ("""),");
            end if;
         end loop;
      end;
      Indent := Indent - 3;
      Indent_Line ("Token_Image_Width : constant Integer :=" & Integer'Image (Token_Image_Width) & ";");
      New_Line;
   end;

   Indent_Line ("function Token_Image (ID : in Token_IDs) return String is (Token_Images (ID).all);");

   Indent_Line
     ("package Tokens is new OpenToken.Token.Enumerated (Token_IDs, First_Terminal, Last_Terminal, Token_Image);");
   Indent_Line ("package Analyzers is new Tokens.Analyzer;");
   Indent_Line ("package Token_Lists is new Tokens.List;");
   Indent_Line ("package Nonterminals is new Tokens.Nonterminal (Token_Lists);");
   Indent_Line ("package Productions is new OpenToken.Production (Tokens, Token_Lists, Nonterminals);");
   Indent_Line ("package Parsers is new Productions.Parser (Analyzers);");
   Indent_Line
     ("package LALRs is new Parsers.LALR (First_State_Index => " &
        OpenToken.Int_Image (First_State_Index) & ");");
   Indent_Line ("package Production_Lists is new Productions.List;");
   Indent_Line ("package LALR_Generators is new LALRs.Generator (Token_IDs'Width, Production_Lists);");
   Indent_Line ("package LALR_Parsers is new LALRs.Parser;");
   New_Line;
   Indent_Line
     ("package Wisi_Tokens is new OpenToken.Wisi_Tokens");
   Indent_Line
     ("  (Token_IDs, First_Terminal, Last_Terminal, Token_Image_Width, Token_Image, Tokens, Token_Lists,");
   Indent_Line ("     Nonterminals);");
   New_Line;

   Indent_Line ("function Create_Parser");
   Indent_Line ("  (Max_Parallel         : in Integer := 15;");
   Indent_Line ("   Terminate_Same_State : in Boolean := True;");
   Indent_Line ("   Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null)");
   Indent_Line ("  return LALR_Parsers.Instance;");
   New_Line;
   Put_Line ("end " & Package_Name & ";");
   Close (Spec_File);

   Create (Body_File, Out_File, Output_File_Root & ".adb");
   Set_Output (Body_File);
   Indent := 1;
   Put_Line ("--  generated by OpenToken Wisi from " & Input_File_Name);
   Put_Line ("--");
   Put_Prologue;

   if Is_In (Tokens, """symbol""") then
      Put_Line ("with Ada.Strings.Maps.Constants;");
   end if;

   if Action_Count > 0 then
      Indent_Line ("with Ada.Text_IO; use Ada.Text_IO;");
   end if;

   if Is_In (Tokens, """whitespace""") then
      Put_Line ("with OpenToken.Recognizer.Character_Set;");
   end if;

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

   Put_Line ("package body " & Package_Name & " is");
   Indent := Indent + 3;
   New_Line;

   Action_Names (Find_Token_ID (OpenToken_Accept_Image))     := new Action_Name_List (0 .. 0);
   Action_Names (Find_Token_ID (OpenToken_Accept_Image)) (0) := Empty_Action;

   if Action_Count = 0 then
      --  Populate Action_Names with Empty_Action.

      Indent_Line ("Self : constant Nonterminals.Synthesize := Wisi_Tokens.Self'Access;");

      for Rule of Rules loop
         declare
            LHS_ID : constant Token_IDs := Find_Token_ID (-Rule.Left_Hand_Side);
         begin
            Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

            for Index in Action_Names (LHS_ID)'Range loop
               Action_Names (LHS_ID) (Index) := Empty_Action;
            end loop;
         end;
      end loop;

   else
      --  generate Action subprograms, populate Action_Names.

      Indent_Line ("Self : constant Nonterminals.Synthesize := Wisi_Tokens.Self'Access;");

      Indent_Line ("use Wisi_Tokens;");
      New_Line;

      for Rule of Rules loop
         declare
            LHS_ID : constant Token_IDs := Find_Token_ID (-Rule.Left_Hand_Side);
            Index  : Integer            := 0; -- Matches Generate_Utils.To_Grammar
         begin
            Action_Names (LHS_ID) := new Action_Name_List (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);

            for RHS of Rule.Right_Hand_Sides loop
               if RHS.Action.Length > 0 then
                  declare
                     Name : constant String := -Rule.Left_Hand_Side & '_' & OpenToken.Int_Image (Index);
                  begin
                     Action_Names (LHS_ID) (Index) := new String'(Name & "'Access");

                     Indent_Line ("procedure " & Name);
                     Indent := Indent + 2;
                     Indent_Line ("(New_Token : out Nonterminals.Class;");
                     Indent := Indent + 1;
                     Indent_Line ("Source    : in  Token_Lists.Instance'Class;");
                     Indent_Line ("To_ID     : in  Token_IDs)");
                     Indent := Indent - 3;
                     Indent_Line ("is begin");
                     Indent := Indent + 3;
                     Indent_Line ("New_Token := Get (To_ID, Total_Buffer_Range (Source));");
                     --  Assuming simple elisp syntax for actions, as used by wisi elisp parser
                     --  Just accumulate elisp sexp to pass back to Emacs
                     Indent_Line ("Put_Line (Image (Source));");
                     for Line of RHS.Action loop
                        Indent_Line ("Put_Line (""" & Line & """);");
                     end loop;
                     Indent_Line ("Put_Line ("")"");");
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
   end if;

   --  This procedure is called for Shift actions
   Indent_Line ("procedure Add_Action");
   Indent_Line ("  (State       : in out LALRs.Parse_State;");
   Indent_Line ("   Symbol      : in     Token_IDs;");
   Indent_Line ("   State_Index : in     LALRs.State_Index)");
   Indent_Line ("is");
   Indent_Line ("   use LALRs;");
   Indent_Line ("   Action : constant Parse_Action_Rec := (Shift, State_Index);");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line
     ("State.Action_List := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), State.Action_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Action;");
   New_Line;

   --  This procedure is called for Reduce or Accept_It actions
   Indent_Line ("procedure Add_Action");
   Indent_Line ("  (State           : in out LALRs.Parse_State;");
   Indent_Line ("   Symbol          : in     Token_IDs;");
   Indent_Line ("   Verb            : in     LALRs.Parse_Action_Verbs;");
   Indent_Line ("   LHS_ID          : in     Token_IDs;");
   Indent_Line ("   RHS_Token_Count : in     Natural;");
   Indent_Line ("   Synthesize      : in     Nonterminals.Synthesize)");
   Indent_Line ("is");
   Indent_Line ("   use LALRs;");
   Indent_Line ("   use Productions;");
   Indent_Line ("   Action : Parse_Action_Rec;");
   Indent_Line ("   LHS    : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens.Get (LHS_ID));");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("case Verb is");
   Indent_Line ("when Reduce =>");
   Indent_Line ("   Action := (Reduce, LHS, Synthesize, 0, RHS_Token_Count);");
   Indent_Line ("when Accept_It =>");
   Indent_Line ("   Action := (Accept_It, LHS, Synthesize, 0, RHS_Token_Count);");
   Indent_Line ("when others =>");
   Indent_Line ("   null;");
   Indent_Line ("end case;");
   Indent_Line
     ("State.Action_List := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), State.Action_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Action;");
   New_Line;

   if Shift_Reduce_Conflict_Count > 0 then
      --  This procedure is called for Shift/Reduce conflicts
      Indent_Line ("procedure Add_Action");
      Indent_Line ("  (State       : in out LALRs.Parse_State;");
      Indent_Line ("   Symbol      : in     Token_IDs;");
      Indent_Line ("   State_Index : in     LALRs.State_Index;");
      Indent_Line ("   LHS_ID      : in     Token_IDs;");
      Indent_Line ("   RHS_Token_Count : in     Natural;");
      Indent_Line ("   Synthesize  : in     Nonterminals.Synthesize)");
      Indent_Line ("is");
      Indent_Line ("   use LALRs;");
      Indent_Line ("   use Productions;");
      Indent_Line ("   Action_1 : constant Parse_Action_Rec := (Shift, State_Index);");
      Indent_Line ("   LHS      : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens.Get (LHS_ID));");
      Indent_Line
        ("   Action_2 : constant Parse_Action_Rec := " &
           "(Reduce, LHS, Synthesize, 0, RHS_Token_Count);");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("State.Action_List := new Action_Node'");
      Indent_Line
        ("  (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Action;");
      New_Line;
   end if;

   if Reduce_Reduce_Conflict_Count > 0 then
      --  This procedure is called for Reduce/Reduce conflicts
      Indent_Line ("procedure Add_Action");
      Indent_Line ("  (State             : in out LALRs.Parse_State;");
      Indent_Line ("   Symbol            : in     Token_IDs;");
      Indent_Line ("   LHS_ID_1          : in     Token_IDs;");
      Indent_Line ("   RHS_Token_Count_1 : in     Natural;");
      Indent_Line ("   Synthesize_1      : in     Nonterminals.Synthesize;");
      Indent_Line ("   LHS_ID_2          : in     Token_IDs;");
      Indent_Line ("   RHS_Token_Count_2 : in     Natural;");
      Indent_Line ("   Synthesize_2      : in     Nonterminals.Synthesize)");
      Indent_Line ("is");
      Indent := Indent + 3;
      Indent_Line ("use LALRs;");
      Indent_Line ("use Productions;");
      Indent_Line ("LHS_1 : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens.Get (LHS_ID_1));");
      Indent_Line
        ("Action_1 : constant Parse_Action_Rec := (Reduce, LHS_1, Synthesize_1, 0, RHS_Token_Count_1);");
      Indent_Line ("LHS_2 : constant Nonterminals.Handle := new Nonterminals.Class'(Wisi_Tokens.Get (LHS_ID_2));");
      Indent_Line
        ("Action_2 : constant Parse_Action_Rec := (Reduce, LHS_2, Synthesize_2, 0, RHS_Token_Count_2);");
      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("State.Action_List := new Action_Node'");
      Indent_Line
        ("  (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);");
      Indent := Indent - 3;
      Indent_Line ("end Add_Action;");
      New_Line;
   end if;

   --  This procedure is called for Error actions
   --  Error action must be last in list
   Indent_Line ("procedure Add_Action");
   Indent_Line ("  (State  : in out LALRs.Parse_State;");
   Indent_Line ("   Symbol : in     Token_IDs)");
   Indent_Line ("is");
   Indent_Line ("   use LALRs;");
   Indent_Line ("   Action : constant Parse_Action_Rec := (Verb => Error);");
   Indent_Line ("   Node   : Action_Node_Ptr           := State.Action_List;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("loop");
   Indent_Line ("   exit when Node.Next = null;");
   Indent_Line ("   Node := Node.Next;");
   Indent_Line ("end loop;");
   Indent_Line ("Node.Next := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Action;");
   New_Line;

   Indent_Line ("procedure Add_Goto");
   Indent := Indent + 2;
   Indent_Line ("(State    : in out LALRs.Parse_State;");
   Indent := Indent + 1;
   Indent_Line ("Symbol   : in     Token_IDs;");
   Indent_Line ("To_State : in     LALRs.State_Index)");
   Indent := Indent - 3;
   Indent_Line ("is");
   Indent_Line ("   use LALRs;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("State.Goto_List := new Goto_Node'(Symbol, To_State, State.Goto_List);");
   Indent := Indent - 3;
   Indent_Line ("end Add_Goto;");
   New_Line;

   Indent_Line ("function Create_Syntax return Analyzers.Syntax");
   Indent_Line ("is");
   Indent_Line ("   use OpenToken.Recognizer;");
   Indent_Line ("begin");
   Indent := Indent + 3;
   Indent_Line ("return");
   Indent_Line ("  (");
   Indent := Indent + 3;
   for Item of Keywords loop
      Indent_Line
        (To_Token_Image (Item.Name) & " => Analyzers.Get (Keyword.Get (" & (-Item.Value) &
        "), Wisi_Tokens.Get (" & To_Token_Image (Item.Name) & ")),");
   end loop;
   for Kind of Tokens loop
      if -Kind.Kind = """line_comment""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (To_Token_Image (Item.Name) & " => Analyzers.Get (Line_Comment.Get (" &
                 (-Item.Value) & "), Wisi_Tokens.Get (" & To_Token_Image (Item.Name) & ")),");
         end loop;
      elsif -Kind.Kind = """whitespace""" then
         --  Only one whitespace token. Ignoring value.
         if Kind.Tokens.Length > 1 then
            raise Programmer_Error;
         end if;
         for Item of Kind.Tokens loop
            Indent_Line (To_Token_Image (Item.Name) & " => Analyzers.Get");
            Indent_Line ("   (Character_Set.Get (Character_Set.Standard_Whitespace)),");
         end loop;
      elsif -Kind.Kind = """number""" then
         raise Programmer_Error;
      elsif -Kind.Kind = """punctuation""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (To_Token_Image (Item.Name) & " => Analyzers.Get (Separator.Get (" &
                 (-Item.Value) & "), Wisi_Tokens.Get (" & To_Token_Image (Item.Name) & ")),");
         end loop;
      elsif -Kind.Kind = """symbol""" then
         for Item of Kind.Tokens loop
            Indent_Line (To_Token_Image (Item.Name) & " => Analyzers.Get");
            Indent_Line ("  (Identifier.Get");
            --  this is compatible with the Emacs Ada mode wisi elisp lexer
            Indent_Line ("     (Start_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set,");
            Indent_Line ("      Body_Chars => Ada.Strings.Maps.Constants.Alphanumeric_Set),");
            Indent_Line ("   Wisi_Tokens.Get (" & To_Token_Image (Item.Name) & ")),");
         end loop;
      elsif -Kind.Kind = """string-double""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (To_Token_Image (Item.Name) & " => Analyzers.Get (OpenToken.Recognizer.String.Get, Wisi_Tokens.Get (" &
                 To_Token_Image (Item.Name) & ")),");
         end loop;
      elsif -Kind.Kind = """string-single""" then
         for Item of Kind.Tokens loop
            Indent_Line
              (To_Token_Image (Item.Name) & " => Analyzers.Get (Graphic_Character.Get, Wisi_Tokens.Get (" &
                 To_Token_Image (Item.Name) & ")),");
         end loop;
      else
         raise OpenToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
      end if;
   end loop;
   Indent_Line ("EOF_ID => Analyzers.Get (OpenToken.Recognizer.End_Of_File.Get, Wisi_Tokens.Get (EOF_ID)));");
   New_Line;
   Indent := Indent - 6;
   Indent_Line ("end Create_Syntax;");
   New_Line;

   Indent_Line ("function Create_Parser");
   Indent_Line ("  (Max_Parallel         : in Integer := 15;");
   Indent_Line ("   Terminate_Same_State : in Boolean := True;");
   Indent_Line ("   Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null)");
   Indent_Line ("  return LALR_Parsers.Instance");
   Indent_Line ("is");
   Indent := Indent + 3;
   Indent_Line ("use LALRs;");
   Indent_Line ("use Productions;");
   Indent_Line
     ("Table : constant Parse_Table_Ptr := new Parse_Table (" &
        LALRs.State_Image (Parser'First) & " .. " & LALRs.State_Image (Parser'Last) & ");");
   Indent := Indent - 3;
   Indent_Line ("begin");
   Indent := Indent + 3;

   for State_Index in Parser'Range loop
      Actions :
      declare
         use Generate_Utils.LALRs;
         Node : Action_Node_Ptr := Parser (State_Index).Action_List;
      begin
         loop
            exit when Node = null;
            Table_Entry_Count := Table_Entry_Count + 1;
            Set_Col (Indent);
            Put ("Add_Action (Table (" & State_Image (State_Index) & "), " & Token_Image (Node.Symbol));
            declare
               Action_Node : Parse_Action_Node_Ptr := Node.Action;
            begin
               case Action_Node.Item.Verb is
               when Shift =>
                  Put (", " & State_Image (Action_Node.Item.State));
               when Reduce | Accept_It =>
                  if Action_Node.Next = null then
                     if Action_Node.Item.Verb = Reduce then
                        Put (", Reduce");
                     else
                        Put (", Accept_It");
                     end if;
                  else
                     --  conflict; Verb must be reduce
                     null;
                  end if;
                  Put
                    (", " & Token_Image (Tokens_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                       Integer'Image (Action_Node.Item.Token_Count) & ", " &
                       Action_Name (Tokens_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
               when Error =>
                  null;
               end case;

               Action_Node := Action_Node.Next;
               if Action_Node /= null then
                  --  Conflict; second action is Shift or Reduce
                  case Action_Node.Item.Verb is
                  when Shift =>
                     Put (", " & State_Image (Action_Node.Item.State));
                  when Reduce =>
                     Put
                       (", " & Token_Image (Tokens_Pkg.ID (Action_Node.Item.LHS.all)) & "," &
                          Integer'Image (Action_Node.Item.Token_Count) & ", " &
                          Action_Name (Tokens_Pkg.ID (Action_Node.Item.LHS.all), Action_Node.Item.Index));
                  when others =>
                     raise Programmer_Error with "second action verb: " &
                       LALRs.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                  end case;
               end if;
            end;
            Put_Line (");");
            Node := Node.Next;
         end loop;
      end Actions;

      Gotos :
      declare
         use Generate_Utils.LALRs;
         Node : Goto_Node_Ptr := Parser (State_Index).Goto_List;
      begin
         loop
            exit when Node = null;
            Set_Col (Indent);
            Put ("Add_Goto (Table (" & State_Image (State_Index) & "), ");
            Put_Line (Token_Image (Node.Symbol) & ", " & State_Image (Node.State) & ");");
            Node := Node.Next;
         end loop;
      end Gotos;
   end loop;
   New_Line;
   --  FIXME: get Max_Parallel from some command line
   Indent_Line ("return");
   Indent_Line ("  (Analyzers.Initialize (Create_Syntax, Text_Feeder), Table, Max_Parallel, Terminate_Same_State);");
   Indent := Indent - 3;
   Indent_Line ("end Create_Parser;");
   Put_Line ("end " & Package_Name & ";");
   Close (Body_File);

   Set_Output (Standard_Output);

   Put_Line
     (Integer'Image (Rule_Count) & " rules," &
        Integer'Image (Action_Count) & " actions," &
        Integer'Image (Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
        Integer'Image (Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts," &
        LALRs.State_Index'Image (Parser'Last) & " states," &
        Integer'Image (Table_Entry_Count) & " table entries");
exception
when others =>
   Set_Output (Standard_Output);
   raise;
end Wisi.Output_Ada_Emacs;
