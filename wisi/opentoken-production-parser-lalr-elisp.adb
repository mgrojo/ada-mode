--  Abstract :
--
--  See spec.
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
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
package body OpenToken.Production.Parser.LALR.Elisp is

   procedure Header (Elisp_Package : in String; Copyright : in String)
   is begin
      Put_Line (";;; " & Elisp_Package & "-wy.el --- Generated parser support file");
      New_Line;
      Put_Line (";; Copyright (C) " & Copyright);
      New_Line;
      --  FIXME: allow other license
      Put_Line (";; This program is free software; you can redistribute it and/or");
      Put_Line (";; modify it under the terms of the GNU General Public License as");
      Put_Line (";; published by the Free Software Foundation; either version 2, or (at");
      Put_Line (";; your option) any later version.");
      Put_Line (";;");
      Put_Line (";; This software is distributed in the hope that it will be useful,");
      Put_Line (";; but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line (";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU");
      Put_Line (";; General Public License for more details.");
      Put_Line (";;");
      Put_Line (";; You should have received a copy of the GNU General Public License");
      Put_Line (";; along with GNU Emacs; see the file COPYING.  If not, write to the");
      Put_Line (";; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,");
      Put_Line (";; Boston, MA 02110-1301, USA.");
      New_Line;
      Put_Line (";; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically");
      Put_Line (";; generated from the grammar file " & Elisp_Package & ".wy");
      New_Line;
   end Header;

   procedure Keyword_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List)
   is
      use Wisi; -- "-" unbounded_string
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--keyword-table");
      Put_Line ("  (semantic-lex-make-keyword-table");
      Put_Line ("   '(");
      for Pair of Keywords loop
         Put_Line ("    (""" & (-Pair.Value) & """ . " & (-Pair.Name) & ")");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language keywords."")");
   end Keyword_Table;

   procedure Token_Table
     (Elisp_Package : in String;
      Tokens        : in Wisi.String_Triplet_Lists.List)
   is
      use Wisi; -- "-" unbounded_string
      use Ada.Strings.Unbounded; -- length
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--token-table");
      Put_Line ("  (semantic-lex-make-type-table");
      Put_Line ("   '(");
      for Triplet of Tokens loop
         if 0 = Length (Triplet.Value) then
            Put_Line ("    (""" & (-Triplet.Kind) & """ (" & (-Triplet.Name) & "))");
         else
            Put_Line ("    (""" & (-Triplet.Kind) & """ (" & (-Triplet.Name) & " . """ & (-Triplet.Value) & """))");
         end if;
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language tokens."")");
   end Token_Table;

   function Token_Image (Item : in Token.Parent_Token_ID) return String
   is
      Full : constant String := Token.Parent_Token_ID'Image (Item);
   begin
      --  Strip trailing _ID. Convert to lowercase if nonterminal
      if Item in Tokenizer.Terminal_ID then
         return Full (1 .. Full'Length - 3);
      else
         return Ada.Characters.Handling.To_Lower (Full (1 .. Full'Length - 3));
      end if;
   end Token_Image;

   procedure Action_Table (Parser : in Instance)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;
   begin
      Put ("     [");
      for State in Parser.Table'Range loop
         if State = Parser.Table'First then
            Put ("(");
         else
            Put ("      (");
         end if;

         Put ("(default . error)");

         declare
            Action : Action_Node_Ptr := Parser.Table (State).Action_List;
         begin
            loop
               declare
                  Parse_Action_Node : Parse_Action_Node_Ptr := Action.Action;
                  Conflict          : constant Boolean      := Parse_Action_Node.Next /= null;
               begin
                  Put (" (" & Token_Image (Action.Symbol) & " . ");

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
                           Put ("error");

                        when Reduce =>
                           Put
                             ("""" & To_Lower (Token_Image (LHS_ID (Parse_Action.Production))) & ":" &
                                Trim (Integer'Image (Index (Parse_Action.Production)), Both) & """");

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

               Action := Action.Next;

               if Action.Next = null then
                  if Action.Action.Item.Verb /= Error then
                     raise Programmer_Error with "state" & State_Index'Image (State) & ": default action is not error";
                  end if;
                  --  let default handle it
                  Action := null;
               end if;

               if Action = null then
                  if State = Parser.Table'Last then
                     Put (")");
                  else
                     Put_Line (")");
                  end if;
                  exit;
               end if;
            end loop;
         end;
      end loop;
      Put_Line ("]");
   end Action_Table;

   procedure Goto_Table (Parser : in Instance)
   is
      function Filter_Terminals (List : in Reduction_Node_Ptr) return Reduction_Node_Ptr
      is
         Result : Reduction_Node_Ptr := List;
         Prev   : Reduction_Node_Ptr := List;
         Item   : Reduction_Node_Ptr := List;
      begin
         while Item /= null loop
            if Item.Symbol in Tokenizer.Terminal_ID then
               --  delete from Result list
               if Item = Result then
                  Result := Item.Next;
               else
                  Prev.Next := Item.Next;
                  Item      := Item.Next;
               end if;
            else
               Prev := Item;
               Item := Item.Next;
            end if;
         end loop;
         return Result;
      end Filter_Terminals;

   begin
      Put ("     [");
      for State in Parser.Table'Range loop
         declare
            Gotos : Reduction_Node_Ptr := Filter_Terminals (Parser.Table (State).Reduction_List);
         begin
            if Gotos = null then
               if State = Parser.Table'First then
                  Put_Line ("nil");
               else
                  if State = Parser.Table'Last then
                     Put ("      nil");
                  else
                     Put_Line ("      nil");
                  end if;
               end if;
            else
               if State = Parser.Table'First then
                  Put ("(");
               else
                  Put ("      (");
               end if;
               loop
                  Put ("(" & Token_Image (Gotos.Symbol) & " ." & State_Index'Image (Gotos.State) & ")");
                  Gotos := Gotos.Next;
                  exit when Gotos = null;
               end loop;
               if State = Parser.Table'Last then
                  Put (")");
               else
                  Put_Line (")");
               end if;
            end if;
         end;
      end loop;
      Put ("]");
   end Goto_Table;

   procedure Parse_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Tokens        : in Wisi.String_Triplet_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Instance)
   is
      use Ada.Containers; -- count_type
      use Wisi; -- "-" unbounded_string

      Rule_Length : constant Count_Type := Rules.Length;
      Rule_Count  : Count_Type := 1;

      RHS_Length : Count_Type;
      RHS_Count  : Count_Type;

      Action_Length : Count_Type;
      Action_Count  : Count_Type;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--parse-table");
      Put_Line ("   (wisi-compile-grammar");
      --  terminal tokens
      Put ("   '((");
      for Pair of Keywords loop
         Put (-Pair.Name & " ");
      end loop;
      for Triplet of Tokens loop
         Put (-Triplet.Name & " ");
      end loop;
      Put_Line (")");

      --  nonterminal productions
      Put ("     (");
      for Rule of Rules loop
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
            for Token of RHS.Production loop
               Put (Token & " ");
            end loop;
            Action_Length := RHS.Action.Length;
            Action_Count  := 1;
            if Action_Length = 0 then
               Put (")");
            else
               Put_Line (")");
            end if;
            for Line of RHS.Action loop
               if Action_Count = Action_Length then
                  Put ("        " & Line);
               else
                  Put_Line ("        " & Line);
               end if;
               Action_Count := Action_Count + 1;
            end loop;
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

      Action_Table (Parser);
      Goto_Table (Parser);
      Put_Line ("))");

      Put_Line ("  ""Parser table."")");
   end Parse_Table;

   procedure Output
     (Elisp_Package : in String;
      Copyright     : in String;
      Prologue      : in String;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Tokens        : in Wisi.String_Triplet_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Instance)
   is
      File : File_Type;
   begin
      Create (File, Out_File, Elisp_Package & "-wy.el");
      Set_Output (File);
      Header (Elisp_Package, Copyright);
      Put_Line (Prologue);
      Put_Line ("(require 'semantic/lex)"); -- FIXME: emacs 23 wants semantic-lex, 24 semantic/lex
      Put_Line ("(require 'wisi-compile)");
      New_Line;
      Keyword_Table (Elisp_Package, Keywords);
      New_Line;
      Token_Table (Elisp_Package, Tokens);
      New_Line;
      Parse_Table (Elisp_Package, Keywords, Tokens, Rules, Parser);
      New_Line;
      Put_Line ("(provide '" & Elisp_Package & "-wy)");
      New_Line;
      Put_Line (";; end of file");
      Close (File);
   end Output;

end OpenToken.Production.Parser.LALR.Elisp;
