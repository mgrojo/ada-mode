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
with WisiToken.BNF.Generate_Grammar;
with WisiToken.BNF.Utils;
with WisiToken.Generate; use WisiToken.Generate;
with WisiToken.LR;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
package body WisiToken.BNF.Output_Ada_Common is

   --  Public subprograms in alphabetical order

   procedure Create_Ada_Actions_Spec
     (Output_File_Name :         in String;
      Package_Name     :         in String;
      Input_Data       :         in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data      :         in Output_Ada_Common.Common_Data;
      Generate_Data    : aliased in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Generate_Utils;

      Descriptor  : WisiToken.Descriptor renames Generate_Data.Descriptor.all;
      Spec_File : File_Type;
      Paren_Done  : Boolean      := False;
      Cursor      : Token_Cursor := First (Generate_Data, Non_Grammar => True, Nonterminals => True);
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      if not (Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0) then
         Put_Line ("with WisiToken;");
      end if;
      if Input_Data.Action_Count > 0 then
         Put_Line ("with WisiToken.Syntax_Trees;");
      end if;
      if Input_Data.Check_Count > 0 then
         Put_Line ("with WisiToken.Lexer;");
         Put_Line ("with WisiToken.Semantic_Checks;");
      end if;
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Pre));

      Indent_Line ("Descriptor : aliased WisiToken.Descriptor :=");
      Indent_Line ("  (First_Terminal                =>" & WisiToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal                 =>" & WisiToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal             =>" & WisiToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal              =>" & WisiToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("EOF_ID                        =>" & WisiToken.Token_ID'Image (Descriptor.EOF_ID) & ",");
      Indent_Line ("Accept_ID                     =>" & WisiToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Case_Insensitive              => " & Image (Input_Data.Language_Params.Case_Insensitive) & ",");
      Indent_Line ("New_Line_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.New_Line_ID) & ",");
      Indent_Line ("Comment_ID                    =>" & WisiToken.Token_ID'Image (Descriptor.Comment_ID) & ",");
      Indent_Line ("Left_Paren_ID                 =>" & WisiToken.Token_ID'Image (Descriptor.Left_Paren_ID) & ",");
      Indent_Line ("Right_Paren_ID                =>" & WisiToken.Token_ID'Image (Descriptor.Right_Paren_ID) & ",");
      Indent_Line ("String_1_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.String_1_ID) & ",");
      Indent_Line ("String_2_ID                   =>" & WisiToken.Token_ID'Image (Descriptor.String_2_ID) & ",");
      Indent_Line ("Embedded_Quote_Escape_Doubled => " & Image
                     (Input_Data.Language_Params.Embedded_Quote_Escape_Doubled) & ",");
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
         Next (Cursor, Nonterminals => True);
         if Is_Done (Cursor) then
            Put_Line (""")),");
         else
            Put_Line ("""),");
         end if;
      end loop;

      Indent := Indent - 3;
      Indent_Line ("Terminal_Image_Width =>" & Integer'Image (Descriptor.Terminal_Image_Width) & ",");
      Indent_Line ("Image_Width          =>" & Integer'Image (Descriptor.Image_Width) & ",");
      Indent_Line ("Last_Lookahead       =>" & WisiToken.Token_ID'Image (Descriptor.Last_Lookahead) & ");");
      Indent := Indent - 3;
      New_Line;

      if Input_Data.Language_Params.Declare_Enums then
         Paren_Done := False;

         Cursor := First (Generate_Data, Non_Grammar => True, Nonterminals => True);
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
            Next (Cursor, Nonterminals => True);
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

      for Name_List of Generate_Data.Action_Names.all loop
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

      for Name_List of Generate_Data.Check_Names.all loop
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

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Actions_Spec;

   procedure Create_Ada_Main_Spec
     (Output_File_Name  : in String;
      Main_Package_Name : in String;
      Input_Data        : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data       : in Output_Ada_Common.Common_Data)
   is
      Lower_Package_Name : constant String := To_Lower (Main_Package_Name);

      Spec_File : File_Type;

      procedure LR_Process
      is begin
         Indent_Line ("procedure Create_Parser");
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes               : in     WisiToken.LR.Parser.Language_Fixes_Access;");
            Indent_Line
              ("   Language_Use_Minimal_Complete_Actions : in    " &
                 "WisiToken.LR.Parser.Language_Use_Minimal_Complete_Actions_Access;");
            Indent_Line
              ("   Language_String_ID_Set       : in     WisiToken.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;");
         end if;
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Start ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access");

         if Common_Data.Text_Rep then
            Put_Line (";");
            Indent_Line ("   Text_Rep_File_Name : in String);");
         else
            Put_Line (");");
         end if;
         New_Line;
      end LR_Process;

      procedure Packrat_Process
      is begin
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Trace     : not null access WisiToken.Trace'Class;");
         Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)");
         Indent_Line ("  return WisiToken.Parse.Base_Parser'Class;");
         New_Line;
      end Packrat_Process;

   begin
      if Common_Data.Generate_Algorithm = External then
         raise SAL.Programmer_Error;
      end if;

      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      case Common_Data.Output_Language is
      when Ada =>
         Put_Line ("with WisiToken.Syntax_Trees;");

      when Ada_Emacs =>
         case Common_Data.Interface_Kind is
         when Process =>
            Put_Line ("with WisiToken.Syntax_Trees;");

         when Module =>
            Put_Line ("with Emacs_Module_Aux;");
            Put_Line ("with emacs_module_h;");
            Put_Line ("with Interfaces.C;");
            Put_Line ("with WisiToken.Semantic_State;");
         end case;
      end case;

      case Common_Data.Generate_Algorithm is
      when LR_Generate_Algorithm =>
         if Input_Data.Language_Params.Error_Recover then
            Put_Line ("with WisiToken.LR.Parser;");
         else
            Put_Line ("with WisiToken.LR.Parser_No_Recover;");
         end if;

      when Packrat_Generate_Algorithm =>
         Put_Line ("with WisiToken.Parse;");

      when External =>
         null;
      end case;

      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Common_Data.Output_Language is
      when Ada =>
         case Common_Data.Generate_Algorithm is
         when LR_Generate_Algorithm =>
            LR_Process;
         when Packrat_Generate_Algorithm =>
            Packrat_Process;
         when External =>
            null;
         end case;

      when Ada_Emacs =>
         case Common_Data.Interface_Kind is
         when Process =>
            case Common_Data.Generate_Algorithm is
            when LR_Generate_Algorithm =>
               LR_Process;
            when Packrat_Generate_Algorithm =>
               Packrat_Process;
            when External =>
               null;
            end case;

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

   procedure Create_External_Main_Spec
     (Actions_Package_Name : in String;
      Main_Package_Name    : in String;
      Tuple                : in Generate_Tuple;
      Input_Data           : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data        : in Generate_Utils.Generate_Data)
   is
      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

      File_Name : constant String := To_Lower (Main_Package_Name) & ".ads";
      Spec_File : File_Type;
   begin
      Create (Spec_File, Out_File, File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      if Input_Data.Action_Count > 0 then
         Put_Line ("with " & Actions_Package_Name & "; use " & Actions_Package_Name & ";");
      end if;

      Put_Line ("with Ada.Containers;");
      Put_Line ("with WisiToken.Syntax_Trees;");
      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("type Action_Item is record");
      Indent_Line ("   Token_Count : Ada.Containers.Count_Type;");
      Indent_Line ("   Action      : WisiToken.Syntax_Trees.Semantic_Action;");
      Indent_Line ("end record;");
      Indent_Line ("type RHS_Array_Action is array (Natural range <>) of Action_Item;");

      Indent_Wrap
        ("Actions : constant array (Token_Enum_ID range " & Image (Descriptor.First_Nonterminal, Descriptor) &
           "_ID .. " & Image (Descriptor.Last_Nonterminal, Descriptor) & "_ID) of access RHS_Array_Action :=");

      Indent_Line ("  (");
      Indent := Indent + 3;
      for LHS in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
         if Generate_Data.Action_Names (LHS) = null then
            Indent_Line (Image (LHS, Descriptor) & "_ID => null,");

         else
            declare
               use all type Standard.Ada.Containers.Count_Type;
               use Standard.Ada.Strings.Unbounded;
               Grammar_RHSs : Productions.RHS_Arrays.Vector renames Generate_Data.Grammar (LHS).RHSs;
               --  Grammar_RHSs.Action is not set.
               Name_RHSs    : Names_Array renames Generate_Data.Action_Names (LHS).all;
               Line         : Unbounded_String := +Image (LHS, Descriptor) & "_ID => new RHS_Array_Action'((";
               Need_Comma   : Boolean          := False;
            begin
               if Grammar_RHSs.Length = 1 then
                  Line := Line &
                    (Trimmed_Image (Grammar_RHSs.First_Index) & " => (" &
                       Trimmed_Image (Grammar_RHSs (Grammar_RHSs.First_Index).Tokens.Length) & ", " &
                       Name_RHSs (Name_RHSs'First).all & "'Access)");
               else
                  for RHS_Index in Grammar_RHSs.First_Index .. Grammar_RHSs.Last_Index loop
                     if Need_Comma then
                        Line := Line & ", ";
                     else
                        Need_Comma := True;
                     end if;
                     if Name_RHSs (RHS_Index) = null then
                        Line := Line & "(0, null)";
                     else
                        Line := Line & "(" & Trimmed_Image (Grammar_RHSs (RHS_Index).Tokens.Length) & ", " &
                          Name_RHSs (RHS_Index).all & "'Access)";
                     end if;
                  end loop;
               end if;
               if LHS = Generate_Data.Grammar.Last_Index then
                  Line := Line & "))";
               else
                  Line := Line & ")),";
               end if;
               Indent_Wrap (-Line);
            end;
         end if;
      end loop;
      Indent_Line (");");

      Indent := Indent - 3;
      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);
   end Create_External_Main_Spec;

   procedure Create_LR_Parser_Core_1 (Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Standard.Ada.Strings.Unbounded;
      use all type Standard.Ada.Containers.Count_Type;

      subtype Nonterminal_ID is Token_ID range
        Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index;

      Table : WisiToken.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Line  : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Line := Line & Item;
      end Append;

      procedure Put (Label : in String; Item : in Token_ID_Array_Natural)
      is begin
         Indent_Line (Label & " =>");
         Indent_Start ("  (");
         Indent := Indent + 3;
         Line := +"";
         for I in Item'Range loop
            Append (Trimmed_Image (Item (I)));

            if I = Item'Last then
               Append ("),");

            else
               Append (", ");
            end if;
         end loop;
         Indent_Wrap (-Line);
         Indent := Indent - 3;
      end Put;

   begin
      Indent_Line ("McKenzie_Param : constant McKenzie_Param_Type :=");
      Indent_Line ("  (First_Terminal    =>" & Token_ID'Image (Table.McKenzie_Param.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal     =>" & Token_ID'Image (Table.McKenzie_Param.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal =>" & Token_ID'Image (Table.McKenzie_Param.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal  =>" & Token_ID'Image (Table.McKenzie_Param.Last_Nonterminal) & ",");
      Put ("Insert", Table.McKenzie_Param.Insert);
      Put ("Delete", Table.McKenzie_Param.Delete);
      Put ("Push_Back", Table.McKenzie_Param.Push_Back);
      Indent_Line ("Ignore_Check_Fail  =>" & Integer'Image (Table.McKenzie_Param.Ignore_Check_Fail) & ",");
      Indent_Line ("Task_Count  =>" & System.Multiprocessors.CPU_Range'Image
                     (Table.McKenzie_Param.Task_Count) & ",");
      Indent_Line ("Cost_Limit  =>" & Integer'Image (Table.McKenzie_Param.Cost_Limit) & ",");
      Indent_Line ("Check_Limit =>" & Token_Index'Image (Table.McKenzie_Param.Check_Limit) & ",");
      Indent_Line ("Check_Delta_Limit =>" & Integer'Image (Table.McKenzie_Param.Check_Delta_Limit) & ",");
      Indent_Line ("Enqueue_Limit =>" & Integer'Image (Table.McKenzie_Param.Enqueue_Limit) & ");");
      Indent := Indent - 3;
      New_Line;

      Indent_Line ("function Productions return WisiToken.Productions.Prod_Arrays.Vector");
      Indent_Line ("is begin");
      Indent := Indent + 3;
      Indent_Line ("return Prods : WisiToken.Productions.Prod_Arrays.Vector do");
      Indent := Indent + 3;
      Indent_Line
        ("Prods.Set_First (" & Trimmed_Image (Generate_Data.Grammar.First_Index) & ");");
      Indent_Line
        ("Prods.Set_Last (" & Trimmed_Image (Generate_Data.Grammar.Last_Index) & ");");

      for I in Nonterminal_ID loop
         declare
            P : Productions.Instance renames Generate_Data.Grammar (I);
         begin
            Indent_Line
              ("Set_Production (Prods (" & Trimmed_Image (P.LHS) & "), " &
                 Trimmed_Image (P.LHS) & "," & Integer'Image (P.RHSs.Last_Index) & ");");

            for J in P.RHSs.First_Index .. P.RHSs.Last_Index loop
               Line := +"Set_RHS (Prods (" & Trimmed_Image (P.LHS) & ")," & Natural'Image (J) & ", (";
               declare
                  RHS : Productions.Right_Hand_Side renames P.RHSs (J);
               begin
                  if RHS.Tokens.Length = 0 then
                     Append ("1 .. 0 => <>");
                  elsif RHS.Tokens.Length = 1 then
                     Append ("1 => " & Trimmed_Image (RHS.Tokens (1)));
                  else
                     for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                        Append (Trimmed_Image (RHS.Tokens (I)));
                        if I < RHS.Tokens.Last_Index then
                           Append (", ");
                        end if;
                     end loop;
                  end if;

                  Append ("), ");
                  Append
                    ((if Generate_Data.Action_Names (P.LHS) = null then "null"
                      elsif Generate_Data.Action_Names (P.LHS)(J) = null then "null"
                      else Generate_Data.Action_Names (P.LHS)(J).all & "'Access"));
                  Append (", ");
                  Append
                    ((if Generate_Data.Check_Names (P.LHS) = null then "null"
                      elsif Generate_Data.Check_Names (P.LHS)(J) = null then "null"
                      else Generate_Data.Check_Names (P.LHS)(J).all & "'Access"));
               end;
               Append (");");
               Indent_Wrap (-Line);
            end loop;
         end;
      end loop;
      Indent := Indent - 3;
      Indent_Line ("end return;");
      Indent := Indent - 3;
      Indent_Line ("end Productions;");
      New_Line;
   end Create_LR_Parser_Core_1;

   procedure Create_LR_Parser_Table
     (Input_Data    : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use all type Standard.Ada.Containers.Count_Type;
      use Standard.Ada.Strings.Unbounded;

      Table            : WisiToken.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Lines_Per_Subr   : constant := 1000;
      Subr_Count       : Integer  := 1;
      Last_Subr_Closed : Boolean  := False;
      Line             : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Line := Line & Item;
      end Append;
   begin
      --  Optimize source structure for GNAT compile time; one subroutine
      --  with thousands of "Table.States (*) := ..." takes forever to
      --  compile (apparently depending on available memory). But hundreds
      --  of subroutines, containing the same lines in chunks of 1000,
      --  compiles in acceptable time.

      Indent_Line ("declare");
      Indent := Indent + 3;

      Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
      Indent_Line ("is begin");
      Indent     := Indent + 3;
      Line_Count := 0;

      Declare_Subroutines :
      for State_Index in Table.States'Range loop

         if Input_Data.Language_Params.Error_Recover then
            Indent_Wrap
              ("Table.States (" & Trimmed_Image (State_Index) & ").Productions := WisiToken.To_Vector (" &
                 Image (Table.States (State_Index).Productions, Strict => True) & ");");
         end if;

         Actions :
         declare
            use Standard.Ada.Containers;
            use WisiToken.LR;
            Base_Indent : constant Standard.Ada.Text_IO.Count := Indent;
            Node        : Action_Node_Ptr := Table.States (State_Index).Action_List;
         begin
            if Duplicate_Reduce (Table.States (State_Index)) then
               declare
                  Action : constant Reduce_Action_Rec := Node.Action.Item;
               begin
                  Set_Col (Indent);
                  Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                    Symbols_Image (Table.States (State_Index)) & ", " &
                    Image (Action.Production) & "," &
                    Count_Type'Image (Action.Token_Count) & ", ";

                  Append
                    ((if Generate_Data.Action_Names (Action.Production.LHS) = null then "null"
                      elsif Generate_Data.Action_Names
                        (Action.Production.LHS)(Action.Production.RHS) = null then "null"
                      else Generate_Data.Action_Names
                        (Action.Production.LHS)(Action.Production.RHS).all & "'Access"));
                  Append (", ");
                  Append
                    ((if Generate_Data.Check_Names (Action.Production.LHS) = null then "null"
                      elsif Generate_Data.Check_Names
                        (Action.Production.LHS)(Action.Production.RHS) = null then "null"
                      else Generate_Data.Check_Names
                        (Action.Production.LHS)(Action.Production.RHS).all & "'Access"));

                  Indent_Wrap (-Line & ");");
                  Line_Count := Line_Count + 1;
                  Indent     := Base_Indent;
               end;

            else
               loop
                  exit when Node = null;
                  Set_Col (Indent);
                  declare
                     Action_Node : Parse_Action_Node_Ptr := Node.Action;
                  begin
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Image (Action_Node.Item.Productions, Strict => True) & ", " &
                          Trimmed_Image (Node.Symbol);
                        Append (", ");
                        Append (Trimmed_Image (Action_Node.Item.State));

                     when Reduce | Accept_It =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Trimmed_Image (Node.Symbol);
                        if Action_Node.Item.Verb = Reduce then
                           Append (", Reduce");
                        else
                           Append (", Accept_It");
                        end if;
                        Append (", ");
                        Append (Image (Action_Node.Item.Production) & ",");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                        Append
                          ((if Generate_Data.Action_Names (Action_Node.Item.Production.LHS) = null then "null"
                            elsif Generate_Data.Action_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Generate_Data.Action_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                               "'Access"));
                        Append (", ");
                        Append
                          ((if Generate_Data.Check_Names (Action_Node.Item.Production.LHS) = null then "null"
                            elsif Generate_Data.Check_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Generate_Data.Check_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                               "'Access"));

                     when LR.Error =>
                        Line := +"Add_Error (Table.States (" & Trimmed_Image (State_Index) & ")";
                     end case;

                     Action_Node := Action_Node.Next;
                     if Action_Node /= null then
                        --  There is a conflict; must be Shift/{Reduce|Accept} or Reduce/{Reduce|Accept}.
                        --  The added parameters are the same in either case.
                        case Action_Node.Item.Verb is
                        when Reduce | Accept_It =>
                           Append (", ");
                           Append (Image (Action_Node.Item.Production) & ",");
                           Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                           Append
                             ((if Generate_Data.Action_Names (Action_Node.Item.Production.LHS) = null then "null"
                               elsif Generate_Data.Action_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Generate_Data.Action_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));
                           Append (", ");
                           Append
                             ((if Generate_Data.Check_Names (Action_Node.Item.Production.LHS) = null then "null"
                               elsif Generate_Data.Check_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Generate_Data.Check_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));

                        when others =>
                           raise SAL.Programmer_Error with "conflict second action verb: " &
                             LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                        end case;
                     end if;
                  end;
                  Indent_Wrap (-Line & ");");
                  Line_Count := Line_Count + 1;
                  Indent     := Base_Indent;
                  Node       := Node.Next;
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
               Put ("Add_Goto (Table.States (" & Trimmed_Image (State_Index) & "), ");
               Put_Line (Trimmed_Image (Symbol (Node)) & ", " & Trimmed_Image (State (Node)) & ");");
               Line_Count := Line_Count + 1;
               Node := Next (Node);
            end loop;
         end Gotos;

         if Table.States (State_Index).Minimal_Complete_Actions.Length > 0 then
            Indent_Wrap
              ("Set_Minimal_Action (Table.States (" & Trimmed_Image (State_Index) & ").Minimal_Complete_Actions, " &
                 WisiToken.LR.Image (Table.States (State_Index).Minimal_Complete_Actions, Strict => True) & ");");
         end if;

         if Line_Count > Lines_Per_Subr then
            Line_Count := 0;
            Indent := Indent - 3;
            Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");

            if State_Index < Table.States'Last then
               Subr_Count := Subr_Count + 1;
               Last_Subr_Closed := False;
               Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
               Indent_Line ("is begin");
               Indent := Indent + 3;
            else
               Last_Subr_Closed := True;
            end if;
         end if;

      end loop Declare_Subroutines;

      if not Last_Subr_Closed then
         Indent := Indent - 3;
         Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");
      end if;

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      for Subr in 1 .. Subr_Count loop
         Indent_Line ("Subr_" & Trimmed_Image (Subr) & ";");
      end loop;
      Indent := Indent - 3;
      Indent_Line ("end;");
   end Create_LR_Parser_Table;

   procedure LR_Create_Create_Parser
     (Input_Data    :         in     WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data)
   is begin
      Indent_Line ("procedure Create_Parser");
      case Common_Data.Interface_Kind is
      when Process =>
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes               : in     WisiToken.LR.Parser.Language_Fixes_Access;");
            Indent_Line
              ("   Language_Use_Minimal_Complete_Actions : in    " &
                 "WisiToken.LR.Parser.Language_Use_Minimal_Complete_Actions_Access;");
            Indent_Line
              ("   Language_String_ID_Set       : in     WisiToken.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;");
         end if;
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Start ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access");

      when Module =>
         Indent_Line ("  (Parser              :    out WisiToken.LR.Parser.Parser;");
         Indent_Line ("   Env                 : in     Emacs_Env_Access;");
         Indent_Start ("   Lexer_Elisp_Symbols : in     Lexers.Elisp_Array_Emacs_Value");
      end case;

      if Common_Data.Text_Rep then
         Put_Line (";");
         Indent_Line ("   Text_Rep_File_Name : in String)");
      else
         Put_Line (")");
      end if;

      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use WisiToken.LR;");

      if Common_Data.Text_Rep then
         Create_LR_Parser_Core_1 (Generate_Data);
         Indent_Line ("Table : constant Parse_Table_Ptr := Get_Text_Rep");
         Indent_Line ("  (Text_Rep_File_Name, McKenzie_Param, Productions);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;

      else
         if Input_Data.Language_Params.Error_Recover then
            Create_LR_Parser_Core_1 (Generate_Data);
         end if;

         Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
         Indent_Line ("  (State_First       => 0,");
         Indent := Indent + 3;
         Indent_Start ("State_Last        => ");
         Put_Line
           (WisiToken.Trimmed_Image (Generate_Data.LR_Parse_Table.State_Last) & ",");

         Indent_Line ("First_Terminal    => Trace.Descriptor.First_Terminal,");
         Indent_Line ("Last_Terminal     => Trace.Descriptor.Last_Terminal,");
         Indent_Line ("First_Nonterminal => Trace.Descriptor.First_Nonterminal,");
         Indent_Line ("Last_Nonterminal  => Trace.Descriptor.Last_Nonterminal);");
         Indent := Indent - 3;

         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("Table.McKenzie_Param := McKenzie_Param;");
            Indent_Line ("Table.Productions := Productions;");
         end if;
         Create_LR_Parser_Table (Input_Data, Generate_Data);
         New_Line;
      end if;

      if Input_Data.Language_Params.Error_Recover then
         Indent_Line ("WisiToken.LR.Parser.New_Parser");
      else
         Indent_Line ("WisiToken.LR.Parser_No_Recover.New_Parser");
      end if;
      Indent_Line ("  (Parser,");
      case Common_Data.Interface_Kind is
      when Process =>
         Indent_Line ("   Trace,");
         Indent_Line ("   Lexer.New_Lexer (Trace),");
         Indent_Line ("   Table,");
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("   Language_Fixes,");
            Indent_Line ("   Language_Use_Minimal_Complete_Actions,");
            Indent_Line ("   Language_String_ID_Set,");
         end if;
         Indent_Line ("   User_Data,");
         Indent_Line ("   Max_Parallel         => 15,");
         Indent_Line ("   Terminate_Same_State => True);");

      when Module =>
         Indent_Line ("   Lexer.New_Lexer (Env, Lexer_Elisp_Symbols),");
         Indent_Line ("   Table, Max_Parallel => 15, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
   end LR_Create_Create_Parser;

   procedure Packrat_Create_Create_Parser
     (Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data;
      Packrat_Data  :         in     WisiToken.Generate.Packrat.Data)
   is
      use Standard.Ada.Strings.Unbounded;

      Text     : Unbounded_String;
      Need_Bar : Boolean := True;
   begin
      Indent_Line ("function Create_Parser");
      Indent_Line ("  (Trace     : not null access WisiToken.Trace'Class;");
      Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)");
      Indent_Line ("  return WisiToken.Parse.Base_Parser'Class");

      case Packrat_Generate_Algorithm'(Common_Data.Generate_Algorithm) is
      when Packrat_Gen =>
         Indent_Line ("is begin");
         Indent := Indent + 3;
         Indent_Line ("return Parser : WisiToken.Parse.Packrat.Generated.Parser do");
         Indent := Indent + 3;
         Indent_Line ("Parser.Trace := Trace;");
         Indent_Line ("Parser.Lexer := Lexer.New_Lexer (Trace);");
         Indent_Line ("Parser.User_Data := User_Data;");
         Indent_Line ("Parser.Parse_WisiToken_Accept := Parse_wisitoken_accept_1'Access;");
         Indent := Indent - 3;
         Indent_Line ("end return;");

      when Packrat_Proc =>
         Indent_Line ("is");
         Indent := Indent + 3;
         Indent_Line ("use WisiToken;");
         Indent_Line ("use WisiToken.Productions;");
         Indent_Line ("Grammar               : Prod_Arrays.Vector;");
         Indent_Line
           ("Direct_Left_Recursive : constant WisiToken.Token_ID_Set (" &
              Trimmed_Image (Generate_Data.Grammar.First_Index) & " .. " &
              Trimmed_Image (Generate_Data.Grammar.Last_Index) & ") :=");

         Need_Bar := False;
         if Any (Packrat_Data.Direct_Left_Recursive) then
            for I in Packrat_Data.Direct_Left_Recursive'Range loop
               if Packrat_Data.Direct_Left_Recursive (I) then
                  if Need_Bar then
                     Text := Text & " | ";
                  else
                     Need_Bar := True;
                  end if;
                  Text := Text & Trimmed_Image (I);
               end if;
            end loop;
            Indent_Start ("  (");
            Indent := Indent + 3;
            Indent_Wrap (-Text & " => True,");
            Indent_Line ("others => False);");
            Indent := Indent - 3;
         else
            Indent_Line ("  (others => False);");
         end if;
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         WisiToken.BNF.Generate_Grammar (Generate_Data.Grammar, Generate_Data.Action_Names.all);

         Indent_Line ("return WisiToken.Parse.Packrat.Procedural.Create");
         Indent_Line
           ("  (Grammar, Direct_Left_Recursive, " & Trimmed_Image (Generate_Data.Descriptor.Accept_ID) &
              ", Trace, Lexer.New_Lexer (Trace), User_Data);");
      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;
   end Packrat_Create_Create_Parser;

   procedure Create_re2c
     (Input_Data            :         in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Tuple                 :         in Generate_Tuple;
      Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String;
      Elisp_Regexps         :         in WisiToken.BNF.String_Pair_Lists.List)
   is
      use Standard.Ada.Strings.Fixed;
      use Generate_Utils;
      use WisiToken.BNF.Utils;
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".re2c");
      Set_Output (File);
      Indent := 1;

      Put_File_Header (C_Comment, " -*- mode: C -*-", Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (C_Comment, Input_Data.Raw_Code (Copyright_License));
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

      if Is_In (Input_Data.Tokens.Tokens, "delimited-text") then
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
      Indent_Line ("*id = -1;"); --  Token_ID'First = 0; see dragon_4_43.wy

      Indent_Line ("if (lexer->cursor > lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("*id            =" & WisiToken.Token_ID'Image (Generate_Data.Descriptor.EOF_ID) & ";");
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

      Indent_Line ("while (*id == -1 && status == 0)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Put_Line ("/*!re2c");
      Indent_Line ("re2c:yyfill:enable   = 0;");
      New_Line;

      --  Regexps used in definitions
      for Pair of Input_Data.Tokens.Regexps loop
         Indent_Line (-Pair.Name & " = " & (-Pair.Value) & ";");
      end loop;
      New_Line;

      --  definitions
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop

         declare
            Val : constant String :=
              (if Is_Present (Elisp_Regexps, Value (I))
               then Value (Elisp_Regexps, Value (I))
               else Value (I));
         begin
            if 0 /= Index (Source => Val, Pattern => "/") then
               --  trailing context syntax; forbidden in definitions
               null;

            elsif Kind (I) = "EOI" then
               Indent_Line (Name (I) & " = [\x04];");

            elsif Kind (I) = "delimited-text" then
               --  not declared in definitions
               null;

            elsif Kind (I) = "keyword" and Input_Data.Language_Params.Case_Insensitive then
               Indent_Line (Name (I) & " = '" & Strip_Quotes (Val) & "';");

            else
               --  Other kinds have values that are regular expressions, in re2c syntax
               Indent_Line (Name (I) & " = " & Val & ";");
            end if;
         end;
      end loop;
      New_Line;

      --  lexer rules
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop
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
         Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
         Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
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

   function Initialize
     (Input_Data        : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Tuple             : in Generate_Tuple;
      Output_File_Root  : in String;
      Check_Interface   : in Boolean)
     return Common_Data
   is begin
      return Data : Common_Data do
         Data.Generate_Algorithm := Tuple.Gen_Alg;

         Data.Output_Language := Ada_Output_Language (Tuple.Out_Lang);

         if Tuple.Gen_Alg = External or else Input_Data.User_Lexer in Valid_Lexer then
            Data.Lexer := Input_Data.User_Lexer;
         else
            raise SAL.Programmer_Error;
         end if;

         if Check_Interface then
            if Tuple.Interface_Kind in Valid_Interface then
               Data.Interface_Kind := Valid_Interface (Tuple.Interface_Kind);
            else
               Put_Error
                 (Error_Message
                    (Input_Data.Grammar_Lexer.File_Name, 1, "Interface_Kind not set"));
            end if;
         else
            Data.Interface_Kind := Process;
         end if;

         Data.Text_Rep := Tuple.Text_Rep;

         Data.Lower_File_Name_Root := +To_Lower (Output_File_Root);
      end return;
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

end WisiToken.BNF.Output_Ada_Common;
