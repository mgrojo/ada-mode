--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with System.Multiprocessors;
with Wisi.Generate_Packrat_Parser;
with Wisi.Utils;
package body Wisi.Gen_Output_Ada_Common is

   --  Public subprograms in alphabetical order

   procedure Create_Ada_Actions_Spec
     (Output_File_Name : in String;
      Package_Name     : in String;
      Descriptor       : in WisiToken.Descriptor'Class;
      Declare_Enum     : in Boolean;
      Ada_Action_Names : in Nonterminal_Names_Array;
      Ada_Check_Names  : in Nonterminal_Names_Array;
      Actions_Present  : in Boolean;
      Checks_Present   : in Boolean)
   is
      use Generate_Utils;
      use Wisi.Utils;

      Spec_File  : File_Type;
      Paren_Done : Boolean      := False;
      Cursor     : Token_Cursor := First (Non_Grammar => True);
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header (Ada_Comment);
      Put_Raw_Code (Ada_Comment, Raw_Code (Copyright_License));
      New_Line;

      if not (Actions_Present or Checks_Present) then
         Put_Line ("with WisiToken;");
      end if;
      if Actions_Present then
         Put_Line ("with WisiToken.Syntax_Trees;");
      end if;
      if Checks_Present then
         Put_Line ("with WisiToken.Lexer;");
         Put_Line ("with WisiToken.Semantic_Checks;");
      end if;
      Put_Raw_Code (Ada_Comment, Raw_Code (Actions_Spec_Context));
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Put_Raw_Code (Ada_Comment, Raw_Code (Actions_Spec_Pre));

      Indent_Line ("Descriptor : aliased WisiToken.Descriptor :=");
      Indent_Line ("  (First_Terminal                =>" & WisiToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal                 =>" & WisiToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal             =>" & WisiToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal              =>" & WisiToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("EOF_ID                        =>" & WisiToken.Token_ID'Image (Descriptor.EOF_ID) & ",");
      Indent_Line ("Accept_ID                     =>" & WisiToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Case_Insensitive              => " & Image (Params.Case_Insensitive) & ",");
      Indent_Line ("New_Line_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.New_Line_ID) & ",");
      Indent_Line ("Comment_ID                    =>" & WisiToken.Token_ID'Image (Descriptor.Comment_ID) & ",");
      Indent_Line ("Left_Paren_ID                 =>" & WisiToken.Token_ID'Image (Descriptor.Left_Paren_ID) & ",");
      Indent_Line ("Right_Paren_ID                =>" & WisiToken.Token_ID'Image (Descriptor.Right_Paren_ID) & ",");
      Indent_Line ("String_1_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.String_1_ID) & ",");
      Indent_Line ("String_2_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.String_2_ID) & ",");
      Indent_Line ("Embedded_Quote_Escape_Doubled => " & Image (Params.Embedded_Quote_Escape_Doubled) & ",");
      Indent_Line ("Image                         =>");
      Indent_Start ("  (");
      Indent := Indent + 3;
      loop
         exit when Is_Done (Cursor);
         if Paren_Done then
            Indent_Start ("new String'(""" & (Name (Cursor)));
         else
            Put ("new String'(""" & (Name (Cursor)));
            Paren_Done := True;
         end if;
         Next (Cursor, Other_Tokens => True);
         if Is_Done (Cursor) then
            Put_Line (""")),");
         else
            Put_Line ("""),");
         end if;
      end loop;

      Indent := Indent - 3;
      Indent_Line ("Terminal_Image_Width =>" & Integer'Image (Descriptor.Terminal_Image_Width) & ",");
      Indent_Line ("Image_Width          =>" & Integer'Image (Descriptor.Image_Width) & ");");
      Indent := Indent - 3;
      New_Line;

      if Declare_Enum then
         Paren_Done := False;

         Cursor := First (Non_Grammar => True);
         Indent_Line ("type Token_Enum_ID is");
         Indent_Start ("  (");
         Indent := Indent + 3;
         loop
            exit when Is_Done (Cursor);
            if Paren_Done then
               Indent_Start (To_Token_Ada_Name (Name (Cursor)));
            else
               Put (To_Token_Ada_Name (Name (Cursor)));
               Paren_Done := True;
            end if;
            Next (Cursor, Other_Tokens => True);
            if Is_Done (Cursor) then
               Put_Line (");");
            else
               Put_Line (",");
            end if;
         end loop;

         Indent := Indent - 3;
         New_Line;

         Indent_Line ("type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;");
         Indent_Line ("use all type WisiToken.Token_ID;");
         Indent_Line ("function ""+"" (Item : in Token_Enum_ID) return WisiToken.Token_ID");
         Indent_Line ("  is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));");

         Indent_Line ("function ""-"" (Item : in WisiToken.Token_ID) return Token_Enum_ID");
         Indent_Line ("  is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));");
         New_Line;

      end if;

      for Name_List of Ada_Action_Names loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("procedure " & Name.all);
                  Indent_Line (" (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
                  Indent_Line ("  Tree      : in out WisiToken.Syntax_Trees.Tree;");
                  Indent_Line ("  Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;");
                  Indent_Line ("  Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);");
               end if;
            end loop;
         end if;
      end loop;

      for Name_List of Ada_Check_Names loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("function " & Name.all);
                  Indent_Line (" (Lexer   : access constant WisiToken.Lexer.Instance'Class;");
                  Indent_Line ("  Nonterm : in out WisiToken.Recover_Token;");
                  Indent_Line ("  Tokens  : in     WisiToken.Recover_Token_Array)");
                  Indent_Line (" return WisiToken.Semantic_Checks.Check_Status;");
               end if;
            end loop;
         end if;
      end loop;

      Put_Raw_Code (Ada_Comment, Raw_Code (Actions_Spec_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Actions_Spec;

   procedure Create_Ada_Main_Spec
     (Output_File_Name    : in String;
      Main_Package_Name   : in String;
      Generator_Algorithm : in Valid_Generator_Algorithm;
      Output_Language     : in Ada_Output_Language;
      Interface_Kind      : in Interface_Type)
   is
      use Wisi.Utils;

      Lower_Package_Name : constant String := To_Lower (Main_Package_Name);

      Spec_File : File_Type;

      procedure LR_Process
      is begin
         Indent_Line ("procedure Create_Parser");
         if Params.Error_Recover then
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes               : in     WisiToken.LR.Parser.Language_Fixes_Access;");
            Indent_Line
              ("   Language_Constrain_Terminals : in     WisiToken.LR.Parser.Language_Constrain_Terminals_Access;");
            Indent_Line
              ("   Language_String_ID_Set       : in     WisiToken.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;");
         end if;
         Indent_Line ("   Algorithm                    : in     WisiToken.Generator_Algorithm_Type;");
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Line ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access);");
         New_Line;
      end LR_Process;

      procedure Packrat_Process
      is begin
         Indent_Line
           ("type Derivs_Type is array (WisiToken.Token_ID range" &
              WisiToken.Token_ID'Image (Generate_Utils.Nonterminal_ID'First) & " .." &
              WisiToken.Token_ID'Image (Generate_Utils.Nonterminal_ID'Last) &
              ") of WisiToken.Packrat.Memos.Vector;");
         New_Line;

         Indent_Line ("type Parser_Type is record");
         Indent := Indent + 3;
         Indent_Line ("Trace            : access WisiToken.Trace'Class;");
         Indent_Line ("Lexer            : WisiToken.Lexer.Handle;");
         Indent_Line ("User_Data        : WisiToken.Syntax_Trees.User_Data_Access;");
         Indent_Line ("Derivs           : Derivs_Type;");
         Indent_Line ("Terminals        : WisiToken.Base_Token_Arrays.Vector;");
         Indent_Line ("Line_Begin_Token : WisiToken.Line_Begin_Token_Vectors.Vector;");
         Indent_Line ("Base_Tree        : aliased WisiToken.Syntax_Trees.Base_Tree;");
         --  FIXME: only need Base_Tree, unless for error handling?
         Indent_Line ("Tree             : WisiToken.Syntax_Trees.Tree;");
         Indent := Indent - 3;
         Indent_Line ("end record;");
         New_Line;

         Indent_Line ("procedure Create_Parser");
         Indent_Line ("  (Parser    :    out Parser_Type;");
         Indent_Line ("   Trace     : not null access WisiToken.Trace'Class;");
         Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access);");
         New_Line;

         Indent_Line ("function Parse");
         Indent_Line ("  (Parser : aliased in out Parser_Type)");
         Indent_Line ("  return WisiToken.Packrat.Result_Type;");
         New_Line;
      end Packrat_Process;

   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header (Ada_Comment);
      Put_Raw_Code (Ada_Comment, Raw_Code (Copyright_License));
      New_Line;

      case Output_Language is
      when Ada =>
         Put_Line ("with WisiToken.Syntax_Trees;");

      when Ada_Emacs =>
         case Interface_Kind is
         when None =>
            raise Programmer_Error;

         when Process =>
            Put_Line ("with WisiToken.Syntax_Trees;");

         when Module =>
            Put_Line ("with Emacs_Module_Aux;");
            Put_Line ("with emacs_module_h;");
            Put_Line ("with Interfaces.C;");
            Put_Line ("with WisiToken.Semantic_State;");
         end case;
      end case;

      case Generator_Algorithm is
      when LR_Generator_Algorithm =>
         if Params.Error_Recover then
            Put_Line ("with WisiToken.LR.Parser;");
         else
            Put_Line ("with WisiToken.LR.Parser_No_Recover;");
         end if;

      when Packrat =>
         Put_Line ("with WisiToken.Lexer;");
         Put_Line ("with WisiToken.Packrat;");
      end case;

      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Output_Language is
      when Ada =>
         case Generator_Algorithm is
         when LR_Generator_Algorithm =>
            LR_Process;
         when Packrat =>
            Packrat_Process;
         end case;

      when Ada_Emacs =>
         case Interface_Kind is
         when None =>
            raise Programmer_Error;

         when Process =>
            --  FIXME: packrat
            LR_Process;

         when Module =>
            Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
            Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name & "_wisi_module_parse"");");
            Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
            Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name & "_wisi_module_parse_init"");");
            New_Line;

         end case;
      end case;

      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Main_Spec;

   procedure Create_Parser_Core
     (Data             : in out Data_Type;
      Table            : in     WisiToken.LR.Parse_Table_Ptr;
      Ada_Action_Names : in     Nonterminal_Names_Array;
      Ada_Check_Names  : in     Nonterminal_Names_Array)
   is
      use all type Standard.Ada.Containers.Count_Type;
      use all type WisiToken.Token_ID;
      use all type WisiToken.LR.McKenzie_Param_Type;
      use Wisi.Utils;

      Count          : Integer;
      Items_Per_Line : constant := 8;

      function Natural_Image (Item : in Natural) return String
      is
         use Standard.Ada.Strings;
         use Standard.Ada.Strings.Fixed;
      begin
         return Trim (Natural'Image (Item), Both);
      end Natural_Image;

      procedure Put (Label : in String; Item : in WisiToken.Token_ID_Array_Natural)
      is begin
         Indent_Line (Label & " =>");
         Indent_Start ("  (");
         Indent := Indent + 3;
         Count := 0;
         for I in Item'Range loop
            Count := Count + 1;
            Put (Natural_Image (Item (I)));

            if I = Item'Last then
               Put_Line ("),");

            elsif Count = Items_Per_Line then
               Count := 0;
               Put_Line (",");
               Indent_Start ("");

            else
               Put (", ");
            end if;
         end loop;
         Indent := Indent - 3;
      end Put;

   begin
      if Table.McKenzie_Param /= WisiToken.LR.Default_McKenzie_Param then
         --  FIXME: duplicated for LALR_LR1
         Indent_Line ("Table.McKenzie_Param :=");
         Indent_Line ("  (First_Terminal    =>" & WisiToken.Token_ID'Image (Table.McKenzie_Param.First_Terminal) & ",");
         Indent := Indent + 3;
         Indent_Line ("Last_Terminal     =>" & WisiToken.Token_ID'Image (Table.McKenzie_Param.Last_Terminal) & ",");
         Indent_Line ("First_Nonterminal =>" & WisiToken.Token_ID'Image (Table.McKenzie_Param.First_Nonterminal) & ",");
         Indent_Line ("Last_Nonterminal  =>" & WisiToken.Token_ID'Image (Table.McKenzie_Param.Last_Nonterminal) & ",");
         Put ("Insert", Table.McKenzie_Param.Insert);
         Put ("Delete", Table.McKenzie_Param.Delete);
         Put ("Push_Back", Table.McKenzie_Param.Push_Back);
         Indent_Line ("Task_Count  =>" & System.Multiprocessors.CPU_Range'Image
                        (Table.McKenzie_Param.Task_Count) & ",");
         Indent_Line ("Cost_Limit  =>" & Integer'Image (Table.McKenzie_Param.Cost_Limit) & ",");
         Indent_Line ("Check_Limit =>" & WisiToken.Token_Index'Image (Table.McKenzie_Param.Check_Limit) & ",");
         Indent_Line ("Check_Delta_Limit =>" & Integer'Image (Table.McKenzie_Param.Check_Delta_Limit) & ",");
         Indent_Line ("Enqueue_Limit =>" & Integer'Image (Table.McKenzie_Param.Enqueue_Limit) & ");");
         Indent := Indent - 3;
         New_Line;

      end if;
      New_Line;

      if Params.Error_Recover then
         Indent_Line ("Table.Productions.Set_First (" & WisiToken.Trimmed_Image (Data.Grammar.First_Index) & ");");
         Indent_Line ("Table.Productions.Set_Last (" & WisiToken.Trimmed_Image (Data.Grammar.Last_Index) & ");");

         for I in Generate_Utils.Nonterminal_ID loop
            declare
               P : WisiToken.Productions.Instance renames Data.Grammar (I);
            begin
               Indent_Start
                 ("Set_Production (Table.Productions (" & WisiToken.Trimmed_Image (P.LHS) & "), " &
                    WisiToken.Trimmed_Image (P.LHS) & "," & Integer'Image (P.RHSs.Last_Index) & ");");

               for J in P.RHSs.First_Index .. P.RHSs.Last_Index loop
                  Indent_Start
                    ("Set_RHS (Table.Productions (" & WisiToken.Trimmed_Image (P.LHS) & ")," & Natural'Image (J) &
                       ", (");
                  declare
                     RHS : WisiToken.Productions.Right_Hand_Side renames P.RHSs (J);
                  begin
                     if RHS.Tokens.Length = 0 then
                        Put ("1 .. 0 => <>");
                     elsif RHS.Tokens.Length = 1 then
                        Put ("1 => " & Trimmed_Image (RHS.Tokens (1)));
                     else
                        for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                           Put (Trimmed_Image (RHS.Tokens (I)));
                           if I < RHS.Tokens.Last_Index then
                              Put (", ");
                           end if;
                        end loop;
                     end if;
                  end;
                  Put_Line ("));");
               end loop;
            end;
         end loop;
         New_Line;

         Indent_Line
           ("Table.Minimal_Terminal_Sequences.Set_First (" & WisiToken.Trimmed_Image
              (Table.Minimal_Terminal_Sequences.First_Index) & ");");

         Indent_Line
           ("Table.Minimal_Terminal_Sequences.Set_Last (" & WisiToken.Trimmed_Image
              (Table.Minimal_Terminal_Sequences.Last_Index) & ");");

         for I in Table.Minimal_Terminal_Sequences.First_Index .. Table.Minimal_Terminal_Sequences.Last_Index loop
            Indent_Start
              ("Set_Token_Sequence (Table.Minimal_Terminal_Sequences (" & WisiToken.Trimmed_Image (I) & "), (");

            declare
               S : WisiToken.Token_ID_Arrays.Vector renames Table.Minimal_Terminal_Sequences (I);
            begin
               if S.Length = 0 then
                  Put ("1 .. 0 => <>");
               elsif S.Length = 1 then
                  Put ("1 =>" & WisiToken.Token_ID'Image (S (S.First_Index)));
               else
                  for J in S.First_Index .. S.Last_Index loop
                     Put (Trimmed_Image (S (J)));
                     if J /= S.Last_Index then
                        Put (", ");
                     end if;
                  end loop;
               end if;
            end;
            Put_Line ("));");
         end loop;
         New_Line;
      end if;

      Data.Table_Entry_Count := 0;

      for State_Index in Table.States'Range loop

         if Params.Error_Recover then
            Indent_Line
              ("Table.States (" & WisiToken.Image (State_Index) & ").Productions := WisiToken.To_Vector (" &
                 WisiToken.Image (Table.States (State_Index).Productions, Strict => True) & ");");
         end if;

         Actions :
         declare
            use Standard.Ada.Containers;
            use Standard.Ada.Strings;
            use Standard.Ada.Strings.Unbounded;
            use WisiToken.LR;
            Base_Indent : constant Standard.Ada.Text_IO.Count := Indent;
            Node        : Action_Node_Ptr := Table.States (State_Index).Action_List;
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
            if Duplicate_Reduce (Table.States (State_Index)) then
               declare
                  Action : constant Reduce_Action_Rec := Node.Action.Item;
               begin
                  Data.Table_Entry_Count := Data.Table_Entry_Count + Actions_Length (Table.States (State_Index)) + 1;
                  --  +1 for Error.

                  Set_Col (Indent);
                  Line := +"Add_Action (Table.States (" & WisiToken.Image (State_Index) & "), " &
                    Symbols_Image (Table.States (State_Index)) & ", " &
                    WisiToken.Image (Action.Production) & "," &
                    Count_Type'Image (Action.Token_Count) & ", ";

                  Append
                    ((if Ada_Action_Names (Action.Production.Nonterm) = null then "null"
                      elsif Ada_Action_Names (Action.Production.Nonterm)(Action.Production.RHS) = null then "null"
                      else Ada_Action_Names (Action.Production.Nonterm)(Action.Production.RHS).all & "'Access"));
                  Append (", ");
                  Append
                    ((if Ada_Check_Names (Action.Production.Nonterm) = null then "null"
                      elsif Ada_Check_Names (Action.Production.Nonterm)(Action.Production.RHS) = null then "null"
                      else Ada_Check_Names (Action.Production.Nonterm)(Action.Production.RHS).all & "'Access"));

                  Put_Line (-Line & ");");
                  Indent := Base_Indent;
               end;

            else
               loop
                  exit when Node = null;
                  Data.Table_Entry_Count := Data.Table_Entry_Count + 1;
                  Set_Col (Indent);
                  declare
                     Action_Node : Parse_Action_Node_Ptr := Node.Action;
                  begin
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Line := +"Add_Action (Table.States (" & WisiToken.Image (State_Index) & "), " &
                          WisiToken.Image (Action_Node.Item.Productions, Strict => True) & ", " &
                          WisiToken.Trimmed_Image (Node.Symbol);
                        Append (", ");
                        Append (WisiToken.Image (Action_Node.Item.State));

                     when Reduce | Accept_It =>
                        Line := +"Add_Action (Table.States (" & WisiToken.Image (State_Index) & "), " &
                          WisiToken.Trimmed_Image (Node.Symbol);
                        if Action_Node.Item.Verb = Reduce then
                           Append (", Reduce");
                        else
                           Append (", Accept_It");
                        end if;
                        Append (", ");
                        Append (WisiToken.Image (Action_Node.Item.Production) & ",");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                        Append
                          ((if Ada_Action_Names (Action_Node.Item.Production.Nonterm) = null then "null"
                            elsif Ada_Action_Names
                              (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Ada_Action_Names
                              (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS).all &
                               "'Access"));
                        Append (", ");
                        Append
                          ((if Ada_Check_Names (Action_Node.Item.Production.Nonterm) = null then "null"
                            elsif Ada_Check_Names
                              (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Ada_Check_Names
                              (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS).all &
                               "'Access"));

                     when WisiToken.LR.Error =>
                        Line := +"Add_Error (Table.States (" & WisiToken.Image (State_Index) & ")";
                     end case;

                     Action_Node := Action_Node.Next;
                     if Action_Node /= null then
                        --  There is a conflict; must be Shift/{Reduce|Accept} or Reduce/{Reduce|Accept}.
                        --  The added parameters are the same in either case.
                        case Action_Node.Item.Verb is
                        when Reduce | Accept_It =>
                           Append (", ");
                           Append (WisiToken.Image (Action_Node.Item.Production) & ",");
                           Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                           Append
                             ((if Ada_Action_Names (Action_Node.Item.Production.Nonterm) = null then "null"
                               elsif Ada_Action_Names
                                 (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Ada_Action_Names
                                 (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));
                           Append (", ");
                           Append
                             ((if Ada_Check_Names (Action_Node.Item.Production.Nonterm) = null then "null"
                               elsif Ada_Check_Names
                                 (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Ada_Check_Names
                                 (Action_Node.Item.Production.Nonterm)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));

                        when others =>
                           raise Programmer_Error with "conflict second action verb: " &
                             WisiToken.LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                        end case;
                     end if;
                  end;
                  Put_Line (-Line & ");");
                  Indent := Base_Indent;
                  Node   := Node.Next;
               end loop;
            end if;
         end Actions;

         Gotos :
         declare
            use WisiToken.LR;
            Node : Goto_Node_Ptr := Table.States (State_Index).Goto_List;
         begin
            loop
               exit when Node = null;
               Set_Col (Indent);
               Put ("Add_Goto (Table.States (" & WisiToken.Image (State_Index) & "), ");
               Put (WisiToken.Image (WisiToken.LR.Prod_ID (Node)) & ", ");
               Put_Line (WisiToken.Trimmed_Image (Symbol (Node)) & ", " & WisiToken.Image (State (Node)) & ");");
               Node := Next (Node);
            end loop;
         end Gotos;
      end loop;
   end Create_Parser_Core;

   procedure Create_Create_Parser
     (Data                : in out Data_Type;
      Parsers             : in     LR_Parser_Array;
      Generator_Algorithm : in     LR_Generator_Algorithm;
      Interface_Kind      : in     Interface_Type;
      First_State_Index   : in     Integer;
      First_Parser_Label  : in     Integer;
      Action_Names        : in     Nonterminal_Names_Array;
      Check_Names         : in     Nonterminal_Names_Array)
   is
      use Wisi.Utils;
   begin
      Indent_Line ("procedure Create_Parser");
      case Interface_Kind is
      when None | Process =>
         if Params.Error_Recover then
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes               : in     WisiToken.LR.Parser.Language_Fixes_Access;");
            Indent_Line
              ("   Language_Constrain_Terminals : in     WisiToken.LR.Parser.Language_Constrain_Terminals_Access;");
               Indent_Line
                 ("   Language_String_ID_Set       : in     WisiToken.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;");
         end if;
         Indent_Line ("   Algorithm                    : in     WisiToken.Generator_Algorithm_Type;");
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Line ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)");

      when Module =>
         Indent_Line ("  (Parser              :    out WisiToken.LR.Parser.Parser;");
         Indent_Line ("   Env                 : in     Emacs_Env_Access;");
         Indent_Line ("   Lexer_Elisp_Symbols : in     Lexers.Elisp_Array_Emacs_Value)");
      end case;

      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use WisiToken.LR;");

      if Generator_Algorithm = LALR_LR1 then
         Indent_Line ("use all type WisiToken.Generator_Algorithm_Type;");
      end if;

      Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
      Indent_Line ("  (State_First       =>" & Integer'Image (First_State_Index) & ",");
      Indent := Indent + 3;
      Indent_Start ("State_Last        => ");

      case Generator_Algorithm is
      when LALR =>
         Put_Line (WisiToken.Image (Parsers (LALR).State_Last) & ",");

      when LR1 =>
         Put_Line (WisiToken.Image (Parsers (LR1).State_Last) & ",");

      when LALR_LR1 =>
         Put_Line
           ("(case Algorithm is when LALR => " & WisiToken.Image (Parsers (LALR).State_Last) &
              ", when LR1 => " & WisiToken.Image (Parsers (LR1).State_Last) & "),");
      end case;
      Indent_Line ("First_Terminal    => Descriptor.First_Terminal,");
      Indent_Line ("Last_Terminal     => Descriptor.Last_Terminal,");
      Indent_Line ("First_Nonterminal => Descriptor.First_Nonterminal,");
      Indent_Line ("Last_Nonterminal  => Descriptor.Last_Nonterminal);");
      Indent := Indent - 3;

      case Generator_Algorithm is
      when LALR =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Data, Parsers (LALR), Action_Names, Check_Names);

      when LR1 =>
         Indent_Line ("pragma Unreferenced (Algorithm);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Create_Parser_Core (Data, Parsers (LR1), Action_Names, Check_Names);

      when LALR_LR1 =>
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         Indent_Line ("case Algorithm is");
         Indent_Line ("when LALR =>");
         Indent := Indent + 3;
         Create_Parser_Core (Data, Parsers (LALR), Action_Names, Check_Names);
         Indent := Indent - 3;
         Indent_Line ("when LR1 =>");
         Indent := Indent + 3;
         Create_Parser_Core (Data, Parsers (LR1), Action_Names, Check_Names);
         Indent := Indent - 3;
         Indent_Line ("end case;");
      end case;
      New_Line;

      if Params.Error_Recover then
         Indent_Line ("WisiToken.LR.Parser.New_Parser");
      else
         Indent_Line ("WisiToken.LR.Parser_No_Recover.New_Parser");
      end if;
      Indent_Line ("  (Parser,");
      case Interface_Kind is
      when None | Process =>
         Indent_Line ("   Trace,");
         Indent_Line ("   Lexer.New_Lexer (Trace),");
         Indent_Line ("   Table,");
         if Params.Error_Recover then
            Indent_Line ("   Language_Fixes,");
            Indent_Line ("   Language_Constrain_Terminals,");
            Indent_Line ("   Language_String_ID_Set,");
         end if;
         Indent_Line ("   User_Data,");
         Indent_Line ("   Max_Parallel         => 15,");
         Indent_Line ("   First_Parser_Label   => " & WisiToken.Trimmed_Image (First_Parser_Label) & ",");
         Indent_Line ("   Terminate_Same_State => True);");

      when Module =>
         Indent_Line ("   Lexer.New_Lexer (Env, Lexer_Elisp_Symbols),");
         Indent_Line ("   Table, Max_Parallel => 15, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
   end Create_Create_Parser;

   procedure Create_Packrat_Parser
     (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
      Action_Names : in Nonterminal_Names_Array;
      Check_Names  : in Nonterminal_Names_Array;
      Descriptor   : in WisiToken.Descriptor)
   is
      use Wisi.Utils;
   begin
      Wisi.Generate_Packrat_Parser (Grammar, Action_Names, Check_Names, Descriptor);

      Indent_Line ("procedure Create_Parser");
      Indent_Line ("  (Parser    :    out Parser_Type;");
      Indent_Line ("   Trace     : not null access WisiToken.Trace'Class;");
      Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)");
      Indent_Line ("is begin");
      Indent := Indent + 3;

      Indent_Line ("Parser.Trace := Trace;");
      Indent_Line ("Parser.Lexer := Lexer.New_Lexer (Trace);");
      Indent_Line ("Parser.User_Data := User_Data;");

      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;

      Indent_Line ("function Parse");
      Indent_Line ("  (Parser : aliased in out Parser_Type)");
      Indent_Line ("  return WisiToken.Packrat.Result_Type");
      Indent_Line ("is");
      Indent := Indent + 3;
      Indent_Line ("Junk : WisiToken.Syntax_Trees.Valid_Node_Index;");
      Indent_Line ("pragma Unreferenced (Junk);");
      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("Parser.Tree.Initialize (Parser.Base_Tree'Access, Flush => True);");
      Indent_Line ("WisiToken.Parse.Lex_All");
      Indent_Line ("  (Parser.Lexer, Parser.Terminals, Parser.Line_Begin_Token, Parser.User_Data, Parser.Trace);");
      New_Line;

      Indent_Line
        ("for Nonterm in Parser.Trace.Descriptor.First_Nonterminal .. Parser.Trace.Descriptor.Last_Nonterminal loop");
      Indent := Indent + 3;
      Indent_Line ("Parser.Derivs (Nonterm).Clear;");
      Indent_Line ("Parser.Derivs (Nonterm).Set_First (Parser.Terminals.First_Index);");
      Indent_Line ("Parser.Derivs (Nonterm).Set_Last (Parser.Terminals.Last_Index);");
      Indent := Indent - 3;
      Indent_Line ("end loop;");
      New_Line;

      Indent_Line ("for Token_Index in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop");
      Indent := Indent + 3;
      Indent_Line ("Junk := Parser.Tree.Add_Terminal (Token_Index, Parser.Terminals);");
      --  FIXME: move this into Lex_All, delete Terminals, just use Syntax_Tree
      Indent := Indent - 3;
      Indent_Line ("end loop;");

      Indent_Line ("return Parse_wisitoken_accept (Parser, Parser.Terminals.First_Index);");
      Indent := Indent - 3;
      Indent_Line ("end Parse;");
      New_Line;
   end Create_Packrat_Parser;

   procedure Create_re2c
     (Output_File_Name_Root : in String;
      Elisp_Regexps         : in Wisi.String_Pair_Lists.List)
   is
      use Standard.Ada.Strings.Fixed;
      use Generate_Utils;
      use Wisi.Utils;
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".re2c");
      Set_Output (File);
      Indent := 1;

      Put_File_Header (C_Comment, " -*- mode: C -*-");
      Put_Raw_Code (C_Comment, Raw_Code (Copyright_License));
      New_Line;

      Indent_Line ("#include <stddef.h>"); -- size_t
      Indent_Line ("#include <stdio.h>"); -- printf
      Indent_Line ("#include <stdlib.h>"); -- malloc
      New_Line;

      Indent_Line ("typedef struct wisi_lexer");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("unsigned char* buffer;           // input text, in utf-8 encoding");
      Indent_Line ("unsigned char* buffer_last;      // last byte in buffer");
      Indent_Line ("unsigned char* cursor;           // current byte");
      Indent_Line ("unsigned char* byte_token_start; // byte position at start of current token");
      Indent_Line ("size_t         char_pos;         // character position of current character");
      Indent_Line ("size_t         char_token_start; // character position at start of current token");
      Indent_Line ("int            line;             // 1 indexed");
      Indent_Line ("int            line_token_start; // line at start of current token");
      Indent_Line ("unsigned char* marker;           // saved cursor");
      Indent_Line ("size_t         marker_pos;       // saved character position");
      Indent_Line ("size_t         marker_line;      // saved line ");
      Indent_Line ("unsigned char* context;          // saved cursor");
      Indent_Line ("size_t         context_pos;      // saved character position");
      Indent_Line ("int            context_line;     // saved line");
      Indent_Line ("int            verbosity;");
      New_Line;
      Indent := Indent - 3;
      Indent_Line ("} wisi_lexer;");
      New_Line;
      Indent_Line ("#define YYCTYPE unsigned char");
      New_Line;

      --  Status values:
      Indent_Line ("#define NO_ERROR 0");
      Indent_Line ("#define ERROR_unrecognized_character 1");

      ----------
      --  new_lexer, free_lexer, reset_lexer

      --  It's normal to increment lexer->cursor one past the end of input,
      --  but not to read that character. To support memory mapped files, we
      --  enforce this strictly; YYPEEK returns EOT (end of text) when
      --  reading past end of buffer; that's how we recognize the end of
      --  text token.

      Indent_Line ("wisi_lexer* " & Output_File_Name_Root & "_new_lexer");
      Indent_Line ("   (unsigned char* input, size_t length, int verbosity)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("wisi_lexer* result  = malloc (sizeof (wisi_lexer));");
      Indent_Line ("result->buffer      = input;");
      Indent_Line ("result->buffer_last = input + length - 1;");
      Indent_Line ("result->cursor      = input;");
      Indent_Line ("result->char_pos    = 1;");
      Indent_Line ("result->line        = (*result->cursor == 0x0A) ? 2 : 1;");
      Indent_Line ("result->verbosity   = verbosity;");
      Indent_Line ("return result;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_free_lexer(wisi_lexer** lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("free(*lexer);");
      Indent_Line ("*lexer = 0;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_reset_lexer(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->cursor   = lexer->buffer;");
      Indent_Line ("lexer->char_pos = 1;");
      Indent_Line ("lexer->line     = (*lexer->cursor == 0x0A) ? 2 : 1;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      ----------
      --  next_token utils

      Indent_Line ("static void debug(wisi_lexer* lexer, int state, unsigned char ch)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->verbosity > 0)");
      Indent_Line ("   {");
      Indent_Line ("   if (ch < ' ')");
      Indent_Line ("      printf (""lexer: %d, 0x%x\n"", state, ch);");
      Indent_Line ("   else");
      Indent_Line ("      printf (""lexer: %d, '%c' 0x%x\n"", state, ch, ch);");
      Indent_Line ("   }");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Line ("#define YYDEBUG(state, ch) debug(lexer, state, ch)");

      --  YYCURSOR is only used in calls of YYDEBUG; we can't define it as
      --  YYPEEK because it is used as '*YYCURSOR'.
      Indent_Line ("#define YYCURSOR lexer->cursor");
      New_Line;

      Indent_Line ("#define YYPEEK() (lexer->cursor <= lexer->buffer_last) ? *lexer->cursor : 4");
      New_Line;

      --  Don't count UTF-8 continuation bytes, or first byte of DOS newline
      Indent_Line ("#define DO_COUNT ((*lexer->cursor & 0xC0) != 0xC0) && (*lexer->cursor != 0x0D)");
      New_Line;

      Indent_Line ("static void skip(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last) ++lexer->cursor;");
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      Indent_Line ("   if (DO_COUNT) ++lexer->char_pos;");
      Indent_Line ("if (*lexer->cursor == 0x0A) ++lexer->line;");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Start ("#define YYSKIP() skip(lexer)");
      New_Line;

      Indent_Line ("#define YYBACKUP() lexer->marker = lexer->cursor; lexer->marker_pos = lexer->char_pos;" &
                     "lexer->marker_line = lexer->line");
      Indent_Line ("#define YYRESTORE() lexer->cursor = lexer->marker; lexer->char_pos = lexer->marker_pos;" &
                     "lexer->line = lexer->marker_line");
      Indent_Line ("#define YYBACKUPCTX() lexer->context = lexer->cursor; lexer->context_pos = lexer->char_pos;" &
                     "lexer->context_line = lexer->line");
      Indent_Line ("#define YYRESTORECTX() lexer->cursor = lexer->context; lexer->char_pos = lexer->context_pos;" &
                     "lexer->line = lexer->context_line");
      New_Line;

      if Is_In (Tokens.Tokens, "delimited-text") then
         Indent_Line ("static void skip_to(wisi_lexer* lexer, char* target)");
         Indent_Line ("{");
         Indent_Line ("  int i;");
         New_Line;
         Indent_Line ("  while (lexer->cursor <= lexer->buffer_last)");
         Indent_Line ("    {");
         Indent_Line ("      if (*lexer->cursor == target[0])");
         Indent_Line ("      {");
         Indent_Line ("        i = 0;");
         Indent_Line ("        do");
         Indent_Line ("          i++;");
         Indent_Line ("        while (0 != target[i] &&");
         Indent_Line ("               lexer->cursor + i <= lexer->buffer_last &&");
         Indent_Line ("               *(lexer->cursor + i) == target[i]);");
         New_Line;
         Indent_Line ("        if (0 == target[i])");
         Indent_Line ("          {");
         Indent_Line ("            for (i = 0; 0 != target[i]; i++)");
         Indent_Line ("               skip(lexer);");
         Indent_Line ("            break;");
         Indent_Line ("          }");
         Indent_Line ("      }");
         Indent_Line ("      skip(lexer);");
         Indent_Line ("    };");
         Indent_Line ("}");
         New_Line;
      end if;

      ----------
      --  next_token
      Indent_Line ("int " & Output_File_Name_Root & "_next_token");
      Indent_Line ("  (wisi_lexer* lexer,");
      Indent_Line ("   int* id,");
      Indent_Line ("   size_t* byte_position,");
      Indent_Line ("   size_t* byte_length,");
      Indent_Line ("   size_t* char_position,");
      Indent_Line ("   size_t* char_length,");
      Indent_Line ("   int*    line_start)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Indent_Line ("int status = NO_ERROR;");
      Indent_Line ("*id = 0;");

      Indent_Line ("if (lexer->cursor > lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("*id            =" & WisiToken.Token_ID'Image (LR1_Descriptor.EOF_ID) & ";");
      Indent_Line ("*byte_position = lexer->buffer_last - lexer->buffer + 1;");
      Indent_Line ("*byte_length   = 0;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = 0;");
      Indent_Line ("*line_start    = lexer->line;");
      Indent_Line ("return status;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("lexer->byte_token_start = lexer->cursor;");
      Indent_Line ("if (DO_COUNT)");
      Indent_Line ("   lexer->char_token_start = lexer->char_pos;");
      Indent_Line ("else");
      Indent_Line ("   lexer->char_token_start = lexer->char_pos + 1;");
      Indent_Line ("if (*lexer->cursor == 0x0A)");
      Indent_Line ("   lexer->line_token_start = lexer->line-1;");
      Indent_Line ("else");
      Indent_Line ("   lexer->line_token_start = lexer->line;");
      New_Line;

      Indent_Line ("while (*id == 0 && status == 0)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Put_Line ("/*!re2c");
      Indent_Line ("re2c:yyfill:enable   = 0;");
      New_Line;

      --  Regexps used in definitions
      for Pair of Tokens.Regexps loop
         Indent_Line (-Pair.Name & " = " & (-Pair.Value) & ";");
      end loop;
      New_Line;

      --  definitions
      for I in All_Tokens.Iterate (Non_Grammar => True, Other_Tokens => False) loop

         declare
            Val : constant String :=
              (if Is_Present (Elisp_Regexps, Value (I))
               then Value (Elisp_Regexps, Value (I))
               else Value (I));
         begin
            if 0 /= Index (Source => Val, Pattern => "/") then
               --  trailing context syntax; forbidden in definitions
               null;

            elsif Kind (I) = "delimited-text" then
               --  not declared in definitions
               null;

            elsif Kind (I) = "keyword" and Params.Case_Insensitive then
               Indent_Line (Name (I) & " = '" & Strip_Quotes (Val) & "';");

            else
               --  Other kinds have values that are regular expressions, in re2c syntax
               Indent_Line (Name (I) & " = " & Val & ";");
            end if;
         end;
      end loop;
      New_Line;

      --  rules
      for I in All_Tokens.Iterate (Non_Grammar => True, Other_Tokens => False) loop
         declare
            Val : constant String :=
              (if Is_Present (Elisp_Regexps, Value (I))
               then Value (Elisp_Regexps, Value (I))
               else Value (I));
         begin

            if Kind (I) = "non-reporting" then
               Indent_Line (Name (I) & " { lexer->byte_token_start = lexer->cursor;");
               Indent_Line ("    lexer->char_token_start = lexer->char_pos;");
               Indent_Line ("    if (*lexer->cursor == 0x0A)");
               Indent_Line ("       lexer->line_token_start = lexer->line-1;");
               Indent_Line ("    else");
               Indent_Line ("       lexer->line_token_start = lexer->line;");
               Indent_Line ("    continue; }");

            elsif Kind (I) = "delimited-text" then
               --  Val contains the start and end strings, separated by space
               declare
                  Start_Last : constant Integer := Index (Val, " ");
               begin
                  Indent_Line
                    (Val (1 .. Start_Last - 1) & " {*id = " & WisiToken.Token_ID'Image (ID (I)) &
                     "; skip_to(lexer, " & Val (Start_Last + 1 .. Val'Last) & "); continue;}");
               end;

            elsif 0 /= Index (Source => Val, Pattern => "/") then
               Indent_Line (Val & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");

            else
               Indent_Line (Name (I) & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");
            end if;
         end;
      end loop;
      New_Line;

      --  end of text
      Indent_Line ("[\x04] {*id = " & WisiToken.Token_ID'Image (LR1_Descriptor.EOF_ID) & "; continue;}");

      --  Default action
      Indent_Line ("* {status = ERROR_unrecognized_character; continue;}");

      Put_Line ("*/");
      Indent_Line ("}");
      Indent := Indent - 3;

      Indent_Line ("*byte_position = lexer->byte_token_start - lexer->buffer + 1;");
      Indent_Line ("*byte_length   = lexer->cursor - lexer->byte_token_start;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("if (DO_COUNT)");
      Indent_Line ("   *char_length = lexer->char_pos - lexer->char_token_start;");
      Indent_Line ("else");
      Indent_Line ("   *char_length = lexer->char_pos - lexer->char_token_start + 1;");
      Indent_Line ("*line_start     = lexer->line_token_start;");
      Indent_Line ("return status;");
      Indent_Line ("}");
      Indent := Indent - 3;
      Set_Output (Standard_Output);
      Close (File);

      declare
         Ada_Name : constant String := Output_File_Name_Root & "_re2c_c";
         --  Output_File_Name_Root is the file name of the grammar file -
         --  assume it is a legal Ada name.
      begin
         Create (File, Out_File, Output_File_Name_Root & "_re2c_c.ads");
         Set_Output (File);
         Indent := 1;
         Put_File_Header (Ada_Comment);
         Put_Raw_Code (Ada_Comment, Raw_Code (Copyright_License));
         New_Line;

         Put_Line ("with Interfaces.C;");
         Put_Line ("with WisiToken;");
         Put_Line ("with System;");
         Put_Line ("package " & Ada_Name & " is");
         Indent := Indent + 3;
         New_Line;

         Indent_Line ("function New_Lexer");
         Indent_Line ("  (Buffer    : in System.Address;");
         Indent_Line ("   Length    : in Interfaces.C.size_t;");
         Indent_Line ("   Verbosity : in Interfaces.C.int)");
         Indent_Line ("  return System.Address");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_new_lexer"";");
         Indent_Line ("--  Create the lexer object, passing it the full text to process.");
         New_Line;
         Indent_Line ("procedure Free_Lexer (Lexer : in out System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_free_lexer"";");
         Indent_Line ("--  Free the lexer object");
         New_Line;

         Indent_Line ("procedure Reset_Lexer (Lexer : in System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_reset_lexer"";");
         New_Line;

         Indent_Line ("function Next_Token");
         Indent_Line ("  (Lexer         : in     System.Address;");
         Indent_Line ("   ID            :    out WisiToken.Token_ID;");
         Indent_Line ("   Byte_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Byte_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Line_Start    :    out Interfaces.C.int)");
         Indent_Line ("  return Interfaces.C.int");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_next_token"";");
         New_Line;

         Indent := Indent - 3;
         Put_Line ("end " & Ada_Name & ";");
         Set_Output (Standard_Output);
         Close (File);
      end;
   end Create_re2c;

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
     (Data             : in out Data_Type;
      Input_File_Name  : in     String;
      Output_File_Root : in     String;
      Check_Interface  : in     Boolean)
   is begin
      declare
         use Wisi.Utils;
         Quit : Boolean := False;
      begin
         Data.Generator_Algorithm := Params.Generator_Algorithm;

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

   function To_Token_Ada_Name (WY_Name : in String) return String
   is
      --  Convert WY_Name to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := WY_Name;
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

end Wisi.Gen_Output_Ada_Common;
