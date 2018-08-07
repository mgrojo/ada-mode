--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2012-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
package body WisiToken.LR.Wisi_Generate_Elisp is

   procedure Action_Table (Table : in Parse_Table; Descriptor : in WisiToken.Descriptor)
   is begin
      Put ("     [");
      for State in Table.States'Range loop
         if State = Table.States'First then
            Put ("(");
         else
            Put ("      (");
         end if;

         Put ("(default . error)");

         declare
            Action : Action_Node_Ptr := Table.States (State).Action_List;
         begin
            loop
               declare
                  Parse_Action_Node : Parse_Action_Node_Ptr := Action.Action;
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
                           Put ("error");

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

               Action := Action.Next;

               if Action.Next = null then
                  if Action.Action.Item.Verb /= Error then
                     raise Programmer_Error with "state" & State_Index'Image (State) & ": default action is not error";
                  end if;
                  --  let default handle it
                  Action := null;
               end if;

               if Action = null then
                  if State = Table.States'Last then
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

   procedure Goto_Table (Table : in Parse_Table; Descriptor : in WisiToken.Descriptor)
   is
      function Filter_Terminals (List : in Goto_Node_Ptr) return Goto_Node_Ptr
      is
         Result : Goto_Node_Ptr := List;
         Prev   : Goto_Node_Ptr := List;
         Item   : Goto_Node_Ptr := List;
      begin
         while Item /= null loop
            if Item.Symbol in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
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
      for State in Table.States'Range loop
         declare
            Gotos : Goto_Node_Ptr := Filter_Terminals (Table.States (State).Goto_List);
         begin
            if Gotos = null then
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
               loop
                  Put ("(" & Image (Gotos.Symbol, Descriptor) & " ." & State_Index'Image (Gotos.State) & ")");
                  Gotos := Gotos.Next;
                  exit when Gotos = null;
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
      Parser        : in Parse_Table_Ptr;
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
               Put (Token & " ");
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

end WisiToken.LR.Wisi_Generate_Elisp;
