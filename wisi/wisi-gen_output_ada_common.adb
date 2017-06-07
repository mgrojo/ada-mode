--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with FastToken;
with Wisi.Utils;
package body Wisi.Gen_Output_Ada_Common is

   --  Public subprograms in alphabetical order

   procedure Create_Ada_Spec
     (Input_File_Name    : in String;
      Output_File_Name   : in String;
      Package_Name       : in String;
      Output_Language    : in Ada_Output_Language;
      Descriptor         : in FastToken.Descriptor'Class;
      Interface_Kind     : in Valid_Interface;
      Lexer              : in Valid_Lexer)
   is
      use Generate_Utils;
      use Wisi.Utils;

      Lower_Package_Name_Root : constant String := To_Lower (Package_Name);

      Spec_File : File_Type;

      Cursor : Token_Cursor;
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Ada_Prologue_Context_Clause;

      case Output_Language is
      when Ada =>
         Put_Line ("with FastToken.Text_Feeder;");
         Put_Line ("with FastToken.Token_Region;");

      when Ada_Emacs =>
         case Interface_Kind is
         when Process =>
            Put_Line ("with FastToken.Lexer.Elisp_Process;");
            Put_Line ("with FastToken.Token;");
            Put_Line ("with FastToken.Token_Wisi_Process;");

         when Module =>
            Put_Line ("with Emacs_Module_Aux;");
            Put_Line ("with emacs_module_h;");
            Put_Line ("with Interfaces.C;");
            Put_Line ("with FastToken.Token;");
         end case;
      end case;
      Put_Line ("with FastToken.Parser.LR.Parser;");
      Put_Line ("with FastToken.Text_IO_Trace;");
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("type Token_Enum_ID is");
      Indent_Line ("  (");
      Indent := Indent + 3;
      Cursor := First;
      loop
         exit when Cursor.Is_Done;
         Set_Col (Indent);
         Put (To_Token_Ada_Name (Cursor.Token_Name));

         Cursor.Next;

         if Cursor.Is_Done then
            Put_Line (");");
         else
            Put_Line (",");
         end if;
      end loop;
      Indent := Indent - 3;
      New_Line;

      Indent_Line ("function ""+"" (Item : in Token_Enum_ID) return FastToken.Token_ID");
      Indent_Line ("  is (FastToken.""+"" (FastToken.Token_ID'First, Token_Enum_ID'Pos (Item)));");
      New_Line;
      Indent_Line ("function ""-"" (Item : in FastToken.Token_ID) return Token_Enum_ID");
      Indent_Line ("  is (Token_Enum_ID'Val (FastToken.""-"" (Item, FastToken.Token_ID'First)));");
      New_Line;

      Indent_Line ("Descriptor : aliased FastToken.Descriptor :=");
      Indent_Line ("  (First_Terminal    =>" & FastToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal     =>" & FastToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal =>" & FastToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal  =>" & FastToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("EOF_ID            =>" & FastToken.Token_ID'Image (Descriptor.EOF_ID) & ",");
      Indent_Line ("Accept_ID         =>" & FastToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Image             =>");
      declare
         use Standard.Ada.Strings.Unbounded;
         Token_Image_Width : Integer      := 0;
         Paren_Done        : Boolean      := False;
      begin
         Cursor := First;
         Indent_Start ("  (");
         Indent := Indent + 3;
         loop
            exit when Cursor.Is_Done;
            if Paren_Done then
               Indent_Start ("new String'(""" & (-Cursor.Token_Name));
            else
               Put ("new String'(""" & (-Cursor.Token_Name));
               Paren_Done := True;
            end if;
            Token_Image_Width := Integer'Max (Token_Image_Width, Length (Cursor.Token_Name));
            Cursor.Next;
            if Cursor.Is_Done then
               Put_Line (""")),");
            else
               Put_Line ("""),");
            end if;
         end loop;

         Indent := Indent - 3;
         Indent_Line ("Image_Width =>" & Integer'Image (Token_Image_Width) & ");");
         New_Line;
      end;
      Indent := Indent - 3;
      New_Line;

      case Lexer is
      when Aflex_Lexer =>
         --  Aflex yylex skeleton (which we are not (yet) overriding)
         --  has type "Token" as the return type, and expects that to
         --  be defined in this package. It also assumes
         --  "End_Of_Buffer" is defined here.
         Indent_Line ("--  For Aflex");
         Indent_Line ("subtype Token is FastToken.Token_ID;");
         Indent_Line ("End_Of_Input : Token renames Descriptor.EOF_ID;");
      when Elisp_Lexer =>
         null;
      when Regexp_Lexer =>
         raise Programmer_Error;
      end case;
      New_Line;

      case Output_Language is
      when Ada =>
         Indent_Line ("Trace : aliased FastToken.Text_IO_Trace.Trace (Descriptor'Access);");
         Indent_Line ("State : aliased FastToken.Token_Region.State_Type (Trace'Access);");
         New_Line;
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Algorithm            : in FastToken.Parser_Algorithm_Type;");
         Indent_Line ("   Max_Parallel         : in Integer                               := 15;");
         Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
         Indent_Line ("   Text_Feeder          : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;");
         Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");
         Indent_Line ("  return FastToken.Parser.LR.Parser.Instance;");
         New_Line;

      when Ada_Emacs =>
         case Interface_Kind is
         when Process =>
            Indent_Line ("State_Aug : aliased Token_Aug.State_Type;");
            New_Line;
            Indent_Line ("package Lexer is new Lexer_Root.Elisp_Process (" & (-EOI_Name) & "_ID, Token_Pkg);");
            New_Line;
            Indent_Line ("function Create_Parser");
            Indent_Line ("  (Algorithm    : in FastToken.Parser_Algorithm_Type;");
            Indent_Line ("   Max_Parallel : in Integer := 15)");
            Indent_Line ("  return LR_Parser.Instance;");
            New_Line;

         when Module =>
            Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
            Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name_Root & "_wisi_module_parse"");");
            Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
            Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name_Root & "_wisi_module_parse_init"");");
            New_Line;

         end case;
      end case;

      Put_Ada_Prologue_Declarations;

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Spec;

   procedure Create_Aflex
     (Input_File_Name       : in String;
      Output_File_Name_Root : in String)
   is
      use FastToken;
      use Generate_Utils;
      use Wisi.Utils;
      use all type Standard.Ada.Containers.Count_Type;

      Package_Name : constant String := File_Name_To_Ada (Output_File_Name_Root);

      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".l");
      Set_Output (File);

      Put_Line ("--  generated by FastToken Wisi from " & Input_File_Name);
      Put_Command_Line ("--  ");
      Put_Line ("--");
      Put_Aflex_Prologue;
      New_Line;
      Put_Line ("%%");
      New_Line;

      --  We don't use a Token_Cursor here because the output depends on the Kind
      for Item of Keywords loop
         Put_Line (-Item.Value & " {         return" & Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
      end loop;

      for Kind of Tokens loop
         if -Kind.Kind = """line_comment""" then
            for Item of Kind.Tokens loop
               Put_Line (Strip_Quotes (-Item.Value) & " {         null;}");
            end loop;

         elsif -Kind.Kind = """whitespace""" then
            for Item of Kind.Tokens loop
               declare
                  Value : constant String := -Item.Value;
               begin
                  if Value'Length = 0 or else
                    Value = (1 .. Value'Length => ' ') or else
                    Value = """"""
                  then
                     --  Copied from elisp lexer
                     raise Programmer_Error with "whitespace needs a regexp; try ""[ \t\n]""";
                  else
                     Put_Line (Strip_Quotes (Value) & " {         null;}");
                  end if;
               end;
            end loop;

         elsif -Kind.Kind = """number""" then
            --  Only one number token.
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               if -Item.Value = "ada-wisi-number-p" then
                  Put_Line
                    ("([0-9]+#)?[0-9a-fA-F._]+(#)? {         return " &
                       Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
               else
                  Put_Line (Strip_Quotes (-Item.Value) & " {         return " &
                              Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
               end if;
            end loop;

         elsif -Kind.Kind = """punctuation""" then
            for Item of Kind.Tokens loop
               declare
                  Value : constant String := -Item.Value;
               begin
                  --  Sometimes Aflex wants quotes around the regexp, sometimes it doesn’t
                  Put_Line
                    ((if Value (1) = '"' and Value'Length <= 4
                      then Value
                      else Strip_Quotes (Value)) &
                       " {         return " & Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
               end;
            end loop;

         elsif -Kind.Kind = """symbol""" then
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error;
            end if;
            for Item of Kind.Tokens loop
               Put_Line (Strip_Quotes (-Item.Value) & " {         return " &
                           Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
            end loop;

         elsif -Kind.Kind = """string-double""" then
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error with "only one string-double value supported";
            end if;
            for Item of Kind.Tokens loop
               Put_Line (Strip_Quotes (-Item.Value) & " {         return " &
                           Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
            end loop;

         elsif -Kind.Kind = """string-single""" then
            if Kind.Tokens.Length > 1 then
               raise Programmer_Error with "only one string-single value supported";
            end if;
            for Item of Kind.Tokens loop
               Put_Line (Strip_Quotes (-Item.Value) & " {         return " &
                           Token_ID'Image (Find_Token_ID (-Item.Name)) & ";}");
            end loop;

         else
            raise FastToken.Grammar_Error with "unsupported token type '" & (-Kind.Kind) & "'";
         end if;
      end loop;

      --  aflex has built-in EOF

      Put_Line ("%%");
      Put_Line ("with " & Package_Name & "; use " & Package_Name & ";");
      Put_Line ("##");

      Close (File);
      Set_Output (Standard_Output);
   end Create_Aflex;

   procedure Create_Create_Parser
     (Parser_Algorithm   : in Valid_Parser_Algorithm;
      Lexer              : in Valid_Lexer;
      Interface_Kind     : in Interface_Type;
      First_State_Index  : in Integer;
      First_Parser_Label : in Integer)
   is
      use Generate_Utils;
      use Wisi.Utils;
      use all type FastToken.Parser.LR.Unknown_State_Index;
   begin
      Indent_Line ("function Create_Parser");
      case Interface_Kind is
      when None | Process =>
         case Data.Lexer is
         when Aflex_Lexer =>
            Indent_Line ("  (Algorithm            : in FastToken.Parser_Algorithm_Type;");
            Indent_Line ("   Max_Parallel         : in Integer                               := 15;");
            Indent_Line ("   Terminate_Same_State : in Boolean                               := True;");
            Indent_Line ("   Text_Feeder          : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;");
            Indent_Line ("   Buffer_Size          : in Integer                               := 1024)");

         when Elisp_Lexer =>
            Indent_Line ("  (Algorithm    : in FastToken.Parser_Algorithm_Type;");
            Indent_Line ("   Max_Parallel : in Integer := 15)");

         when Regexp_Lexer =>
            raise Programmer_Error;
         end case;
      when Module =>
         Indent_Line ("  (Env                 : in Emacs_Env_Access;");
         Indent_Line ("   Lexer_Elisp_Symbols : in Lexers.Elisp_Array_Emacs_Value;");
         Indent_Line ("   Max_Parallel        : in Integer := 15)");
      end case;

      Indent_Line ("  return FastToken.Parser.LR.Parser.Instance");
      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use FastToken.Parser.LR;");
      Indent_Line ("use all type FastToken.Parser_Algorithm_Type;");
      Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
      Indent_Line  ("  (State_First   =>" & Integer'Image (First_State_Index) & ",");
      Indent := Indent + 3;
      Indent_Start ("State_Last       => ");

      case Parser_Algorithm is
      when LALR =>
         Put_Line (State_Image (Parsers (LALR).State_Last) & ",");

      when LR1 =>
         Put_Line (State_Image (Parsers (LR1).State_Last) & ",");

      when LALR_LR1 =>
         Put_Line
           ("(case Algorithm is when LALR => " & State_Image (Parsers (LALR).State_Last) &
              ", when LR1 => " & State_Image (Parsers (LR1).State_Last) & "),");
      end case;
      Indent_Line ("First_Terminal    => Descriptor.First_Terminal,");
      Indent_Line ("Last_Terminal     => Descriptor.Last_Terminal,");
      Indent_Line ("First_Nonterminal => Descriptor.First_Nonterminal,");
      Indent_Line ("Last_Nonterminal  =>  Descriptor.Last_Nonterminal);");
      Indent := Indent - 3;

      case Parser_Algorithm is
      when LALR =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LALR));

      when LR1 =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LR1));

      when LALR_LR1 =>
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("case Algorithm is");
         Indent_Line ("when LALR =>");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LALR));
         Indent := Indent - 3;
         Indent_Line ("when LR1 =>");
         Indent := Indent + 3;
         Create_Parser_Core (Parsers (LR1));
         Indent := Indent - 3;
         Indent_Line ("end case;");
      end case;
      New_Line;

      --  IMPROVEME: get Max_Parallel from some command line
      Indent_Line ("return");
      case Interface_Kind is
      when None | Process =>
         case Lexer is
         when Aflex_Lexer =>
            Indent_Line ("  (Lexer.New_Lexer (Text_Feeder, Buffer_Size, First_Column => 0),");
            Indent_Line ("   Table, FastToken.Token.Semantic_State'Class (State)'Access, " &
                           "FastToken.Token.List.Null_List,");
            Indent_Line ("   Max_Parallel, " & FastToken.Int_Image (First_Parser_Label)
                           & ", Terminate_Same_State);");

         when Elisp_Lexer =>
            Indent_Line ("  (Lexer.New_Lexer, Table, State_Aug'Access, Token_Pkg.List.Null_List,");
            Indent_Line ("   Max_Parallel, Terminate_Same_State => True);");

         when Regexp_Lexer =>
            raise Programmer_Error;
         end case;

      when Module =>
         Indent_Line ("  (Lexer.New_Lexer (Env, Lexer_Elisp_Symbols),");
         Indent_Line ("   Table, Max_Parallel, Terminate_Same_State);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;
   end Create_Create_Parser;

   procedure Create_Parser_Core (Parser : in FastToken.Parser.LR.Parse_Table_Ptr)
   is
      use Generate_Utils;
      use Wisi.Utils;

      Paren_Done : Boolean := False;
   begin
      Indent_Line ("Table.Panic_Recover :=");
      Indent_Start ("  (");
      Indent := Indent + 3;
      for I in Parser.Panic_Recover'Range loop
         if Parser.Panic_Recover (I) then
            if Paren_Done then
               Put_Line (" |");
               Indent_Start (FastToken.Int_Image (I));
            else
               Paren_Done := True;
               Put (FastToken.Int_Image (I));
            end if;
         end if;
      end loop;
      if Paren_Done then
         Put_Line (" => True,");
         Indent_Line ("others => False);");
      else
         Put_Line ("others => False);");
      end if;
      Indent := Indent - 3;
      New_Line;

      if not FastToken.Any (Parser.Follow) then
         Indent_Line ("Table.Follow := (others => (others => False));");
      else
         Indent_Line ("Table.Follow :=");
         Indent_Start ("  (");
         Indent := Indent + 3;
         for I in Parser.Follow'Range (1) loop
            if FastToken.Any (Parser.Follow, I) then
               Indent_Line (FastToken.Int_Image (I) & " =>");
               Indent_Start ("  (");
               Indent := Indent + 3;
               Paren_Done := False;
               for J in Parser.Follow'Range (2) loop
                  if Parser.Follow (I, J) then
                     if Paren_Done then
                        Put_Line (" |");
                        Indent_Start (" " & FastToken.Int_Image (J));
                     else
                        Paren_Done := True;
                        Put (FastToken.Int_Image (J));
                     end if;
                  end if;
               end loop;
               if Paren_Done then
                  Put_Line (" => True,");
                  Indent_Line (" others => False),");
               else
                  Put_Line ("others => False),");
               end if;
               Indent := Indent - 3;
            end if;
         end loop;
         Indent_Line ("others => (others => False));");
         Indent := Indent - 3;
      end if;
      New_Line;

      for State_Index in Parser.States'Range loop
         Actions :
         declare
            use Standard.Ada.Containers;
            use Standard.Ada.Strings;
            use Standard.Ada.Strings.Unbounded;
            use FastToken.Parser.LR;
            Base_Indent : constant Standard.Ada.Text_IO.Count := Indent;
            Node        : Action_Node_Ptr := Parser.States (State_Index).Action_List;
            Line        : Unbounded_String;

            procedure Append (Item : in String)
            is
               Max_Line_Length : constant := 120;
            begin
               --  -2 for trailing ); or ,
               if Indent + Standard.Ada.Text_IO.Count (Length (Line)) + Item'Length > Max_Line_Length - 2 then
                  Put_Line (-Trim (Line, Right));
                  Indent := Indent + 2;
                  Set_Col (Indent);
                  Line := +Item;
               else
                  Line := Line & Item;
               end if;
            end Append;

         begin
            loop
               exit when Node = null;
               Data.Table_Entry_Count := Data.Table_Entry_Count + 1;
               Set_Col (Indent);
               declare
                  Action_Node : Parse_Action_Node_Ptr := Node.Action;
               begin
                  case Action_Node.Item.Verb is
                  when Shift =>
                     Line := +"Add_Action (Table.States (" & State_Image (State_Index) & "), " &
                       FastToken.Int_Image (Node.Symbol);
                     Append (", ");
                     Append (State_Image (Action_Node.Item.State));
                  when Reduce | Accept_It =>
                     Line := +"Add_Action (Table.States (" & State_Image (State_Index) & "), " &
                       FastToken.Int_Image (Node.Symbol);
                     if Action_Node.Item.Verb = Reduce then
                        Append (", Reduce");
                     else
                        Append (", Accept_It");
                     end if;
                     Append (", ");
                     Append (FastToken.Int_Image (Action_Node.Item.LHS) & ",");
                     Append (Integer'Image (Action_Node.Item.Index) & ", ");
                     Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                     Append
                       ((if Ada_Action_Names (Action_Node.Item.LHS) = null then "null"
                         elsif Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index) = null then "null"
                         else Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index).all));

                  when Error =>
                     Line := +"Add_Error (Table.States (" & State_Image (State_Index) & ")";
                  end case;

                  Action_Node := Action_Node.Next;
                  if Action_Node /= null then
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Append (", ");
                        Append (State_Image (Action_Node.Item.State));
                     when Reduce | Accept_It =>
                        Append (", ");
                        Append (FastToken.Int_Image (Action_Node.Item.LHS) & ",");
                        Append (Integer'Image (Action_Node.Item.Index) & ", ");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                        Append
                          ((if Ada_Action_Names (Action_Node.Item.LHS) = null then "null"
                            elsif Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index) = null then "null"
                            else Ada_Action_Names (Action_Node.Item.LHS)(Action_Node.Item.Index).all));

                     when others =>
                        raise Programmer_Error with "conflict second action verb: " &
                          FastToken.Parser.LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                     end case;
                  end if;
               end;
               Put_Line (-Line & ");");
               Indent := Base_Indent;
               Node := Node.Next;
            end loop;
         end Actions;

         Gotos :
         declare
            use FastToken.Parser.LR;
            Node : Goto_Node_Ptr := Parser.States (State_Index).Goto_List;
         begin
            loop
               exit when Node = null;
               Set_Col (Indent);
               Put ("Add_Goto (Table.States (" & State_Image (State_Index) & "), ");
               Put_Line (FastToken.Int_Image (Symbol (Node)) & ", " & State_Image (State (Node)) & ");");
               Node := Next (Node);
            end loop;
         end Gotos;
      end loop;
   end Create_Parser_Core;

   function File_Name_To_Ada (File_Name : in String) return String
   is
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
   end File_Name_To_Ada;

   procedure Initialize
     (Input_File_Name  : in String;
      Output_File_Root : in String;
      Check_Interface  : in Boolean)
   is begin
      declare
         use Wisi.Utils;
         Quit : Boolean := False;
      begin
         Data.Parser_Algorithm := Params.Parser_Algorithm; -- checked in Wisi.Declarations

         if Params.Lexer in Valid_Lexer then
            Data.Lexer := Valid_Lexer (Params.Lexer);
         else
            Put_Error (Input_File_Name, 1, "Lexer not set in grammar file");
            Quit := True;
         end if;

         if Check_Interface then
            if Params.Interface_Kind in Valid_Interface then
               Data.Interface_Kind := Valid_Interface (Params.Interface_Kind);
            else
               Put_Error (Input_File_Name, 1, "Interface_Kind not set in grammar file");
               Quit := True;
            end if;
         end if;

         if Quit then raise User_Error with "missing grammar file directives"; end if;
      end;

      Data.Grammar := Generate_Utils.To_Grammar (Generate_Utils.LR1_Descriptor, Input_File_Name, -Params.Start_Token);

      Data.Package_Name_Root       := +File_Name_To_Ada (Output_File_Root);
      Data.Lower_Package_Name_Root := +To_Lower (Output_File_Root);
   end Initialize;

   function To_Token_Ada_Name (Item : in String) return String
   is
      --  Convert Item to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := Item;
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

   function To_Token_Ada_Name (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return To_Token_Ada_Name (-Item);
   end To_Token_Ada_Name;

end Wisi.Gen_Output_Ada_Common;
