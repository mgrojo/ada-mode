--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
--
--  Copyright (C) 2012 - 2015, 2017 - 2019 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.BNF.Generate_Utils;
with WisiToken.BNF.Output_Elisp_Common;
with WisiToken.Generate.Packrat;
with WisiToken.Parse.LR;
with WisiToken_Grammar_Runtime;
procedure WisiToken.BNF.Output_Elisp
  (Input_Data    :         in WisiToken_Grammar_Runtime.User_Data_Type;
   Elisp_Package :         in String;
   Generate_Data : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
   Packrat_Data  :         in WisiToken.Generate.Packrat.Data;
   Tuple         :         in Generate_Tuple)
is
   pragma Unreferenced (Packrat_Data);

   procedure Action_Table (Table : in WisiToken.Parse.LR.Parse_Table; Descriptor : in WisiToken.Descriptor)
   is
      use all type SAL.Base_Peek_Type;
      use WisiToken.Parse.LR;
   begin
      Put ("     [");
      for State in Table.States'Range loop
         if State = Table.States'First then
            Put ("(");
         else
            Put ("      (");
         end if;

         Put ("(default . error)");

         for I in Table.States (State).Action_List.First_Index .. Table.States (State).Action_List.Last_Index loop
            declare
               Action : Action_Node renames Table.States (State).Action_List (I);
               Parse_Action_Node : Parse_Action_Node_Ptr := Action.Actions;
               Conflict          : constant Boolean      := Parse_Action_Node.Next /= null;
            begin
               Put (" (" & Image (Action.Symbol, Descriptor) & " . ");

               if Conflict then
                  Put ("(");
               end if;

               loop
                  declare
                     Parse_Action : Parse_Action_Rec renames Parse_Action_Node.Item;
                  begin
                     case Parse_Action.Verb is
                     when Accept_It =>
                        Put ("accept");

                     when Error =>
                        raise SAL.Programmer_Error;

                     when Reduce =>
                        Put
                          ("(" & Image (Parse_Action.Production.LHS, Descriptor) & " ." &
                             Integer'Image (Parse_Action.Production.RHS) & ")");

                     when Shift =>
                        Put (State_Index'Image (Parse_Action.State));

                     end case;

                     if Parse_Action_Node.Next = null then
                        if Conflict then
                           Put (")");
                        end if;
                        Put (")");
                        exit;
                     else
                        Put (" ");
                        Parse_Action_Node := Parse_Action_Node.Next;
                     end if;
                  end;
               end loop;
            end;
            if I = Table.States (State).Action_List.Last_Index then
               if State = Table.States'Last then
                  Put (")");
               else
                  Put_Line (")");
               end if;
            end if;
         end loop;
      end loop;
      Put_Line ("]");
   end Action_Table;

   procedure Goto_Table (Table : in WisiToken.Parse.LR.Parse_Table; Descriptor : in WisiToken.Descriptor)
   is
      use WisiToken.Parse.LR;

      subtype Nonterminals is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;

      function Count_Nonterminals (List : in Goto_Arrays.Vector) return Integer
      is
         Count : Integer := 0;
      begin
         for Item of List loop
            if Item.Symbol in Nonterminals then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_Nonterminals;

   begin
      Put ("     [");
      for State in Table.States'Range loop
         declare
            Nonterminal_Count : constant Integer := Count_Nonterminals (Table.States (State).Goto_List);
         begin
            if Nonterminal_Count = 0 then
               if State = Table.States'First then
                  Put_Line ("nil");
               else
                  if State = Table.States'Last then
                     Put ("      nil");
                  else
                     Put_Line ("      nil");
                  end if;
               end if;
            else
               if State = Table.States'First then
                  Put ("(");
               else
                  Put ("      (");
               end if;
               for Item of Table.States (State).Goto_List loop
                  if Item.Symbol in Nonterminals then
                     Put ("(" & Image (Item.Symbol, Descriptor) & " ." & Item.State'Image & ")");
                  end if;
               end loop;
               if State = Table.States'Last then
                  Put (")");
               else
                  Put_Line (")");
               end if;
            end if;
         end;
      end loop;
      Put ("]");
   end Goto_Table;

   procedure Output
     (Elisp_Package : in String;
      Tokens        : in WisiToken.BNF.Tokens;
      Parser        : in WisiToken.Parse.LR.Parse_Table_Ptr;
      Descriptor    : in WisiToken.Descriptor)
   is
      use Ada.Strings.Unbounded;
      use Ada.Containers; -- count_type

      Rule_Length : constant Count_Type := Tokens.Rules.Length;
      Rule_Count  : Count_Type := 1;

      RHS_Length : Count_Type;
      RHS_Count  : Count_Type;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-elisp-parse-table");
      Put_Line ("   (wisi-compile-grammar");

      --  nonterminal productions
      Put ("   '((");
      for Rule of Tokens.Rules loop
         if Rule_Count = 1 then
            Put ("(");
         else
            Put ("      (");
         end if;
         Put_Line (-Rule.Left_Hand_Side);

         RHS_Length := Rule.Right_Hand_Sides.Length;
         RHS_Count  := 1;
         for RHS of Rule.Right_Hand_Sides loop
            Put ("       ((");
            for Token of RHS.Tokens loop
               Put (-Token.Identifier & " ");
            end loop;
            if Length (RHS.Action) = 0 then
               Put (")");
            else
               Put_Line (")");
               Put ("        " & (-RHS.Action));
            end if;

            if RHS_Count = RHS_Length then
               Put (")");
            else
               Put_Line (")");
            end if;
            RHS_Count := RHS_Count + 1;
         end loop;
         if Rule_Count = Rule_Length then
            Put (")");
         else
            Put_Line (")");
         end if;
         Rule_Count := Rule_Count + 1;
      end loop;
      Put_Line (")");

      Action_Table (Parser.all, Descriptor);
      Goto_Table (Parser.all, Descriptor);
      Put_Line ("))");

      Put_Line ("  ""Parser table."")");
   end Output;

   procedure Create_Elisp (Algorithm : in LR_Generate_Algorithm)
   is
      use Ada.Strings.Unbounded;
      File            : File_Type;
      Elisp_Package_1 : constant String :=
        (case Algorithm is
         when LALR => Elisp_Package & "-lalr",
         when LR1  => Elisp_Package & "-lr1");
   begin
      Create (File, Out_File, Elisp_Package_1 & "-elisp.el");
      Set_Output (File);

      Put_Line (";;; " & Elisp_Package_1 & "-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-");
      Put_Command_Line (Elisp_Comment & "  ", Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Copyright_License));
      Put_Raw_Code (Elisp_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      New_Line;

      Put_Line ("(require 'wisi)");
      Put_Line ("(require 'wisi-compile)");
      Put_Line ("(require 'wisi-elisp-parse)");
      New_Line;
      Output_Elisp_Common.Indent_Keyword_Table
        (Elisp_Package_1, "elisp", Input_Data.Tokens.Keywords, To_String'Access);
      New_Line;
      Output_Elisp_Common.Indent_Token_Table (Elisp_Package_1, "elisp", Input_Data.Tokens.Tokens, To_String'Access);
      New_Line;
      Output (Elisp_Package_1, Input_Data.Tokens, Generate_Data.LR_Parse_Table, Generate_Data.Descriptor.all);
      New_Line;
      Put_Line ("(provide '" & Elisp_Package_1 & "-elisp)");
      Put_Line (";; end of file");
      Close (File);

      Set_Output (Standard_Output);
   end Create_Elisp;

begin
   Create_Elisp (Tuple.Gen_Alg);

   if WisiToken.Trace_Generate > 0 then
      WisiToken.BNF.Generate_Utils.Put_Stats (Input_Data, Generate_Data);
   end if;
end WisiToken.BNF.Output_Elisp;
