--  Abstract :
--
--  See spec.
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

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
package body OpenToken.Production.Parser.LALR.Elisp is

   procedure Action_Table (Parser : in Instance)
   is begin
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
                             ("(" & Token_Image (LHS_ID (Parse_Action.Production)) & " ." &
                                Integer'Image (Index (Parse_Action.Production)) & ")");

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

   procedure Output
     (Elisp_Package : in String;
      Tokens        : in Wisi.Token_Lists.List;
      Keywords      : in Wisi.String_Pair_Lists.List;
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
      for Kind of Tokens loop
         if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
            for Pair of Kind.Tokens loop
               Put (-Pair.Name & " ");
            end loop;
         end if;
      end loop;
      for Pair of Keywords loop
         Put (-Pair.Name & " ");
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
   end Output;

end OpenToken.Production.Parser.LALR.Elisp;
