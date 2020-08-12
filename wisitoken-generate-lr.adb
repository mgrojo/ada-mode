--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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
with Ada.Text_IO;
with System.Multiprocessors;
with WisiToken.Generate;
with WisiToken.Syntax_Trees;
with SAL.Unix_Text_IO;
package body WisiToken.Generate.LR is

   package RHS_Set is new SAL.Gen_Unbounded_Definite_Vectors (Natural, Boolean, Default_Element => False);

   type LHS_RHS_Set is array (Token_ID range <>) of RHS_Set.Vector;

   ----------
   --  Body subprograms, alphabetical

   function Count (Actions : Parse_Action_Node_Ptr) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Node : Parse_Action_Node_Ptr := Actions;
   begin
      return Result : Count_Type := 0 do
         loop
            exit when Node = null;
            Result  := @ + 1;
            Node    := Node.Next;
         end loop;
      end return;
   end Count;

   function Min
     (Item    : in RHS_Sequence_Arrays.Vector;
      RHS_Set : in LR.RHS_Set.Vector)
     return Integer
   is
      use all type Ada.Containers.Count_Type;
      Min_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
      Min_RHS    : Natural                   := Natural'Last;
   begin
      for RHS in Item.First_Index .. Item.Last_Index loop
         if RHS_Set (RHS) and then Min_Length > Item (RHS).Length then
               Min_Length := Item (RHS).Length;
               Min_RHS    := RHS;
         end if;
      end loop;
      if Min_RHS = Natural'Last then
         raise SAL.Programmer_Error with "nonterm has no minimum terminal sequence";
      else
         return Min_RHS;
      end if;
   end Min;

   function Image
     (Nonterm    : in Token_ID;
      Sequences  : in Minimal_Sequence_Array;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      return Trimmed_Image (Nonterm) & " " & Image (Nonterm, Descriptor) & " ==> (" &
        Sequences (Nonterm).Min_RHS'Image & ", " & Image (Sequences (Nonterm).Sequence, Descriptor) & ")";
   end Image;

   procedure Terminal_Sequence
     (Grammar       : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor    : in     WisiToken.Descriptor;
      All_Sequences : in out Minimal_Sequence_Array;
      All_Seq_Set   : in out Token_ID_Set;
      RHS_Seq_Set   : in out LHS_RHS_Set;
      Recursing     : in out Token_ID_Set;
      Nonterm       : in     Token_ID)
   is
      use Ada.Containers;
      use Token_ID_Arrays;

      subtype Terminals is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      Prod : Productions.Instance renames Grammar (Nonterm);

      Skipped_Recursive : Boolean := False;

      procedure Init_All_Sequences (LHS : in Token_ID)
      is
         Prod : Productions.Instance renames Grammar (LHS);
      begin
         if All_Sequences (LHS).Sequence.Length = 0 then
            All_Sequences (LHS).Sequence.Set_First_Last (Prod.RHSs.First_Index, Prod.RHSs.Last_Index);
         end if;
         if RHS_Seq_Set (LHS).Length = 0 then
            RHS_Seq_Set (LHS).Set_First_Last (Prod.RHSs.First_Index, Prod.RHSs.Last_Index);
         end if;
      end Init_All_Sequences;

   begin
      --  We get here because All_Sequences (Nonterm) has not been fully
      --  computed yet (All_Seq_Set (Nonterm) is False). Attempt to
      --  compute All_Sequences (Nonterm); it may not succeed due to
      --  recursion. If successful, set All_Seq_Set (Nonterm).
      --
      --  In a useful grammar, all direct and indirect recursive nonterms
      --  have a non-recursive minimal terminal sequence; finding it will
      --  break the recursion, allowing this algorithm to complete. This is
      --  checked in Compute_Minimal_Terminal_Sequences.

      Init_All_Sequences (Nonterm);

      for RHS in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         if not RHS_Seq_Set (Nonterm)(RHS) then
            if Trace_Generate_Minimal_Complete > Extra then
               Ada.Text_IO.Put_Line (Trimmed_Image ((Nonterm, RHS)) & " " & Image (Nonterm, Descriptor) & " compute");
            end if;
            if Prod.RHSs (RHS).Tokens.Length = 0 then
               RHS_Seq_Set (Nonterm)(RHS) := True;
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line (Trimmed_Image (Production_ID'(Nonterm, RHS)) & " => () empty");
               end if;

            else
               for I in Prod.RHSs (RHS).Tokens.First_Index .. Prod.RHSs (RHS).Tokens.Last_Index loop
                  declare
                     ID : Token_ID renames Prod.RHSs (RHS).Tokens (I);
                  begin
                     if ID in Terminals then
                        All_Sequences (Nonterm).Sequence (RHS).Append (ID);

                     else
                        if (for some RHS of RHS_Seq_Set (ID) => RHS) then
                           --  There is a minimal sequence for ID; use it
                           null;
                        else
                           if ID = Nonterm or Recursing (ID) then
                              --  Clear partial minimal sequence; we are starting over.
                              All_Sequences (Nonterm).Sequence (RHS).Clear;
                              goto Skip;

                           else
                              Recursing (ID) := True;
                              Terminal_Sequence
                                (Grammar, Descriptor, All_Sequences, All_Seq_Set, RHS_Seq_Set, Recursing, ID);
                              Recursing (ID) := False;

                              if All_Seq_Set (ID) or else (for some RHS of RHS_Seq_Set (ID) => RHS) then
                                 --  Found a minimal sequence for ID; use it
                                 null;
                              else
                                 All_Sequences (Nonterm).Sequence (RHS).Clear;
                                 goto Skip;
                              end if;
                           end if;
                        end if;
                        declare
                           Min_RHS : constant Integer := Min (All_Sequences (ID).Sequence, RHS_Seq_Set (ID));
                        begin
                           All_Sequences (ID).Min_RHS := Min_RHS;

                           All_Sequences (Nonterm).Sequence (RHS).Append (All_Sequences (ID).Sequence (Min_RHS));
                        end;
                     end if;
                  end;
               end loop;
               RHS_Seq_Set (Nonterm)(RHS) := True;
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line
                    (Trimmed_Image (Production_ID'(Nonterm, RHS)) & " => " &
                       Image (All_Sequences (Nonterm).Sequence (RHS), Descriptor));
               end if;
            end if;
         end if;
         <<Skip>>
         Skipped_Recursive := True;
      end loop;

      if Skipped_Recursive then
         if (for some RHS of RHS_Seq_Set (Nonterm) => not RHS) then
            --  Some RHSs are have unresolved recursion; we will
            --  eventually try again when the recursion is resolved.
            if Trace_Generate_Minimal_Complete > Extra then
               Ada.Text_IO.Put_Line
                 (Trimmed_Image (Nonterm) & " " & Image (Nonterm, Descriptor) & " skipped some recursive");
            end if;
            return;
         end if;
      end if;

      All_Seq_Set (Nonterm) := True;

      if Trace_Generate_Minimal_Complete > Extra then
         Ada.Text_IO.Put_Line (Image (Nonterm, All_Sequences, Descriptor));
      end if;
   end Terminal_Sequence;

   function To_Conflict
     (Action_Node : in Parse.LR.Action_Node;
      State       : in State_Index)
     return Conflict
   is
      Node : Parse_Action_Node_Ptr := Action_Node.Actions;
   begin
      return Result : Conflict do
         Result.On := Action_Node.Symbol;
         Result.States.Append (State);
         loop
            Result.Items.Insert ((Node.Item.Verb, Node.Item.Production.LHS));
            Node := Node.Next;
            exit when Node = null;
         end loop;
      end return;
   end To_Conflict;

   ----------
   --  Public subprograms, declaration order

   function Image (Conflict : in LR.Conflict; Descriptor : in WisiToken.Descriptor) return String
   is
      use Ada.Strings.Unbounded;
      use all type Ada.Containers.Count_Type;

      --  Must match wisitoken_grammar_runtime.adb Add_Declaration
      --  "conflict"; see there for comment with format.

      Result   : Unbounded_String := +"%conflict ";
      Need_Bar : Boolean          := False;

      function Image (Item : in Conflict_Parse_Actions) return String
      --  WORKAROUND: subtype_object'Image GNAT Community 2020 with -gnat2020
      --  returns integer, not name.
      is (case Item is
          when Shift => "SHIFT",
          when Reduce => "REDUCE",
          when Accept_It => "ACCEPT_IT");
   begin
      for Item of Conflict.Items loop
         if Need_Bar then
            Result := Result & " | ";
         else
            Need_Bar := True;
         end if;

         Result := Result & Image (Item.Action) & " " & Image (Item.LHS, Descriptor);
      end loop;

      Result := Result & " on token " & Image (Conflict.On, Descriptor);

      Need_Bar := False;

      if Conflict.States.Length > 0 then
         Result := Result & " (";
         for State of Conflict.States loop
            if Need_Bar then
               Result := Result & "," & State'Image;
            else
               Need_Bar := True;
               Result := Result & Trimmed_Image (State);
            end if;
         end loop;

         Result := Result & ")";
      end if;
      return -Result;
   end Image;

   function Conflict_Compare (Left, Right : in Conflict) return SAL.Compare_Result
   is
      use all type SAL.Compare_Result;
      use all type SAL.Base_Peek_Type;
   begin
      if Left.On > Right.On then
         return Greater;
      elsif Left.On < Right.On then
         return Less;
      else
         declare
            I : SAL.Peek_Type := Left.Items.First_Index;
            J : SAL.Peek_Type := Right.Items.First_Index;
         begin
            loop
               case Conflict_Item_Compare (Left.Items (I), Right.Items (J)) is
               when Greater =>
                  return Greater;
               when Less =>
                  return Less;
               when Equal =>
                  I := I + 1;
                  J := J + 1;

                  if I > Left.Items.Last_Index and J > Right.Items.Last_Index then
                     return Equal;
                  elsif I > Left.Items.Last_Index then
                     return Greater;
                  elsif J > Right.Items.Last_Index then
                     return Less;
                  else
                     null;
                  end if;
               end case;
            end loop;
         end;
      end if;
   end Conflict_Compare;

   procedure Put
     (Item       : in Conflict_Lists.Tree;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor)
   is begin
      for Conflict of Item loop
         Ada.Text_IO.Put_Line (File, Image (Conflict, Descriptor));
      end loop;
   end Put;

   procedure Check_Conflicts
     (Label            : in     String;
      Found_Conflicts  : in out Conflict_Lists.Tree;
      Known_Conflicts  : in out Conflict_Lists.Tree;
      File_Name        : in     String;
      Descriptor       : in     WisiToken.Descriptor;
      Ignore_Conflicts : in     Boolean)
   is
      use Conflict_Lists;
      use all type SAL.Compare_Result;
      use all type Ada.Containers.Count_Type;

      Known_Iter : constant Iterator := Known_Conflicts.Iterate;
      Known      : Cursor   := Known_Iter.First;

      Found_Iter : constant Iterator := Found_Conflicts.Iterate;
      Found      : Cursor   := Found_Iter.First;

   begin
      loop
         exit when Known = No_Element or Found = No_Element;

         case Conflict_Compare
           (Known_Conflicts.Constant_Ref (Known),
            Found_Conflicts.Constant_Ref (Found))
         is
         when Greater =>
            Found := Found_Iter.Next (Found);

         when Less =>
            Known := Known_Iter.Next (Known);

         when Equal =>
            declare
               Temp : Cursor := Known;
            begin
               Known := Known_Iter.Next (Known);
               Known_Conflicts.Delete (Temp);

               Temp  := Found;
               Found := Found_Iter.Next (Found);
               Found_Conflicts.Delete (Temp);
            end;
         end case;
      end loop;

      if Found_Conflicts.Length > 0 then
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Error_Message (File_Name, 1, Label & " unknown conflicts:"));
         Put (Found_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Conflicts;
      end if;

      if Known_Conflicts.Length > 0 then
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error, Error_Message (File_Name, 1, Label & " excess known conflicts:"));
         Put (Known_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Conflicts;
      end if;

   end Check_Conflicts;

   procedure Collect_Conflicts
     (Table           : in Parse_Table;
      Conflicts       : in out Conflict_Lists.Tree;
      Conflict_Counts : in out Conflict_Count_Lists.Vector)
   is
      use all type Ada.Containers.Count_Type;
   begin
      Conflict_Counts.Set_First_Last (Table.States'First, Table.States'Last);

      for State in Table.States'Range loop
         declare
            Counts : Conflict_Count renames Conflict_Counts (State);
         begin

            for Action_Node of Table.States (State).Action_List loop
               if Count (Action_Node.Actions) > 1 then
                  declare
                     use Conflict_Lists;

                     New_Conflict : constant Conflict := To_Conflict (Action_Node, State);

                     Existing_Conflict : constant Cursor := Conflicts.Iterate.Find (New_Conflict);
                  begin
                     if Existing_Conflict = No_Element then
                        Conflicts.Insert (New_Conflict);
                     else
                        Conflicts.Variable_Ref (Existing_Conflict).States.Append (State);
                     end if;

                     if Action_Node.Actions.Item.Verb = Shift then
                        Counts.Shift_Reduce := @ + 1;
                     elsif New_Conflict.Items (New_Conflict.Items.Last_Index).Action = Accept_It then
                        Counts.Accept_Reduce := @ + 1;
                     else
                        Counts.Reduce_Reduce := @ + 1;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Collect_Conflicts;

   ----------
   --  Build parse table

   procedure Add_Action
     (Symbol      : in     Token_ID;
      Action      : in     Parse_Action_Rec;
      Action_List : in out Action_Arrays.Vector;
      Descriptor  : in     WisiToken.Descriptor)
   is
      Matching_Action_Node : constant Action_Arrays.Find_Reference_Type := Action_List.Find (Symbol);
   begin
      if Trace_Generate_Table > Detail then
         Ada.Text_IO.Put (Image (Symbol, Descriptor) & " => ");
         Put (Descriptor, Action);
         Ada.Text_IO.New_Line;
      end if;

      if Matching_Action_Node.Element /= null then
         if Is_In (Action, Matching_Action_Node.Actions) then
            if Trace_Generate_Table > Detail then
               Ada.Text_IO.Put_Line (" - already present");
            end if;
            return;
         else
            --  New conflict. Sort to match Conflict_Item_Compare order
            if Action.Verb = Shift then
               Matching_Action_Node.Actions := new Parse_Action_Node'(Action, Matching_Action_Node.Actions);
            else
               declare
                  Node : Parse_Action_Node_Ptr := Matching_Action_Node.Actions;
                  Prev : Parse_Action_Node_Ptr := null;
               begin
                  if Node.Item.Verb = Shift then
                     Prev := Node;
                     Node := Node.Next;
                  end if;
                  loop
                     exit when Node = null or else Node.Item.Production.LHS > Action.Production.LHS;
                     Prev := Node;
                     Node := Node.Next;
                  end loop;

                  if Prev = null then
                     Matching_Action_Node.Actions := new Parse_Action_Node'(Action, Matching_Action_Node.Actions);
                  else
                     Prev.Next := new Parse_Action_Node'(Action, Node);
                  end if;
               end;
            end if;
         end if;
      else
         WisiToken.Parse.LR.Add (Action_List, Symbol, Action);
      end if;
   end Add_Action;

   procedure Add_Actions
     (Closure    : in     LR1_Items.Item_Set;
      Table      : in out Parse_Table;
      Grammar    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
   is
      use Token_ID_Arrays;

      State : constant State_Index := Closure.Tree_Node.State;
   begin
      if Trace_Generate_Table > Detail then
         Ada.Text_IO.Put_Line ("setting table actions for state" & State_Index'Image (State));
      end if;

      for Item of Closure.Set loop
         declare
            Item_Tokens : Token_ID_Arrays.Vector renames Productions.Constant_Ref_RHS
              (Grammar, Item.Prod).Tokens;
         begin
            if Item.Dot not in Item_Tokens.First_Index .. Item_Tokens.Last_Index then
               Add_Lookahead_Actions (Item, Table.States (State).Action_List, Grammar, Descriptor);

            elsif Item_Tokens (Item.Dot) in Descriptor.First_Terminal .. Descriptor.Last_Terminal
            then
               --  Dot is before a terminal token.
               declare
                  use all type Ada.Containers.Count_Type;

                  P_ID : constant Production_ID := Item.Prod;

                  Dot_ID : constant Token_ID := Item_Tokens (Item.Dot);
                  --  ID of token after Item.Dot

                  Goto_State : constant Unknown_State_Index := LR1_Items.Goto_State (Closure, Dot_ID);
               begin
                  if Dot_ID = Descriptor.EOI_ID then
                     --  This is the start symbol production with dot before EOF.
                     declare
                        RHS  : Productions.Right_Hand_Side renames Grammar (P_ID.LHS).RHSs (P_ID.RHS);
                     begin
                        Add_Action
                          (Dot_ID,
                           (Accept_It, P_ID, RHS.Action, RHS.Check, RHS.Tokens.Length - 1),
                           --  EOF is not pushed on stack in parser, because the action for EOF
                           --  is Accept, not Shift.
                           Table.States (State).Action_List, Descriptor);
                     end;
                  else
                     if Goto_State /= Unknown_State then
                        Add_Action
                          (Dot_ID,
                           (Shift, P_ID, Goto_State),
                           Table.States (State).Action_List,
                           Descriptor);
                     end if;
                  end if;
               end;
            else
               --  Dot is before a non-terminal token; no action.
               if Trace_Generate_Table > Detail then
                  Ada.Text_IO.Put_Line (Image (Item_Tokens (Item.Dot), Descriptor) & " => no action");
               end if;
            end if;
         end;
      end loop;

      --  We don't place a default error action at the end of every state;
      --  Parse.LR.Action_For returns Table.Error_Action when Symbol is not found.
      Table.Error_Action := new Parse_Action_Node'((Verb => WisiToken.Parse.LR.Error, others => <>), null);

      for Item of Closure.Goto_List loop
         if Item.Symbol in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal then
            --  Goto_List also has terminals, used above in Goto_State. We can't just
            --  use Goto_List to create actions for terminals; they don't contain
            --  enough information.
            Add_Goto (Table.States (State), Item.Symbol, Item.State);
         end if;
      end loop;
   end Add_Actions;

   procedure Add_Lookahead_Actions
     (Item        : in     LR1_Items.Item;
      Action_List : in out Action_Arrays.Vector;
      Grammar     : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor  : in     WisiToken.Descriptor)
   is
      Prod   : Productions.Instance renames Grammar (Item.Prod.LHS);
      RHS    : Productions.Right_Hand_Side renames Prod.RHSs (Item.Prod.RHS);
      Action : constant Parse_Action_Rec := (Reduce, Item.Prod, RHS.Action, RHS.Check, RHS.Tokens.Length);
   begin
      if Trace_Generate_Table > Detail then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      --  We ignore propagate lookaheads here.
      for Lookahead in Item.Lookaheads'Range loop
         if Item.Lookaheads (Lookahead) then
            if Lookahead = Descriptor.First_Nonterminal then
               null;
            else
               Add_Action (Lookahead, Action, Action_List, Descriptor);
            end if;
         end if;
      end loop;
   end Add_Lookahead_Actions;

   ----------
   --  Minimal terminal sequences.

   function Min_Length (Item : in RHS_Sequence_Arrays.Vector) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Min : Count_Type := Count_Type'Last;
   begin
      for RHS of Item loop
         if RHS.Length < Min then
            Min := RHS.Length;
         end if;
      end loop;
      return Min;
   end Min_Length;

   function Min (Item : in RHS_Sequence_Arrays.Vector) return Token_ID_Arrays.Vector
   is
      use all type Ada.Containers.Count_Type;
      Min_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
      Min_RHS    : Natural                   := Natural'Last;
   begin
      --  This version assumes all RHS are computed.
      for RHS in Item.First_Index .. Item.Last_Index loop
         if Min_Length > Item (RHS).Length then
            Min_Length := Item (RHS).Length;
            Min_RHS    := RHS;
         end if;
      end loop;
      if Min_RHS = Natural'Last then
         raise Grammar_Error with "nonterm has no minimum terminal sequence";
      else
         return Item (Min_RHS);
      end if;
   end Min;

   function Compute_Minimal_Terminal_Sequences
     (Descriptor        : in WisiToken.Descriptor;
      Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      Grammar_File_Name : in String)
     return Minimal_Sequence_Array
   is
      --  Result (ID).Sequence.Length = 0 is a valid result (ie the
      --  nonterminal can be empty), so we use an auxilliary array to track
      --  whether Result (ID) has been computed.

      All_Seq_Set : Token_ID_Set := (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => False);
      Recursing   : Token_ID_Set := (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => False);

      RHS_Seq_Set : LHS_RHS_Set :=
        (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => RHS_Set.Empty_Vector);

      Last_Seq_Count : Integer := 0;
      This_Count     : Integer;
      Pass_Count     : Integer := 0;
   begin
      return Result : Minimal_Sequence_Array (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal) do
         loop
            exit when (for all B of All_Seq_Set => B);
            Pass_Count := Pass_Count + 1;
            if Trace_Generate_Minimal_Complete > Detail then
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.New_Line;
               end if;
               Ada.Text_IO.Put_Line ("Compute_Minimal_Terminal_Sequences pass" & Integer'Image (Pass_Count));
            end if;
            for P of Grammar loop
               Terminal_Sequence (Grammar, Descriptor, Result, All_Seq_Set, RHS_Seq_Set, Recursing, P.LHS);
            end loop;
            This_Count := Count (All_Seq_Set);

            if This_Count = Last_Seq_Count then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Current_Error,
                  Error_Message
                    (File_Name => Grammar_File_Name,
                     File_Line => Line_Number_Type'First,
                     Message   => "terminal sequences not resolved:"));

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Current_Error,
                  Error_Message
                    (File_Name => Grammar_File_Name,
                     File_Line => Line_Number_Type'First,
                     Message   => Image (All_Seq_Set, Descriptor, Inverted => True)));
               raise Parse_Error;
            end if;
            Last_Seq_Count := This_Count;
         end loop;

         if Trace_Generate_Minimal_Complete > Detail then
            Ada.Text_IO.Put_Line ("Minimal_Terminal_Sequences:");
            for LHS in Result'Range loop
               Ada.Text_IO.Put_Line (Image (LHS, Result, Descriptor));
            end loop;
         end if;
      end return;
   end Compute_Minimal_Terminal_Sequences;

   function Compute_Minimal_Terminal_First
     (Descriptor                 : in WisiToken.Descriptor;
      Minimal_Terminal_Sequences : in Minimal_Sequence_Array)
     return Token_Array_Token_ID
   is
      use Token_ID_Arrays;
   begin
      return Result : Token_Array_Token_ID (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal) do
         for ID in Result'Range loop
            declare
               use all type Ada.Containers.Count_Type;
               Min_Seq : Token_ID_Arrays.Vector renames Min (Minimal_Terminal_Sequences (ID).Sequence);
            begin
               if Min_Seq.Length = 0 then
                  Result (ID) := Invalid_Token_ID;
               else
                  Result (ID) := Element (Min_Seq.First);
               end if;
            end;
         end loop;
      end return;
   end Compute_Minimal_Terminal_First;

   procedure Set_Minimal_Complete_Actions
     (State                      : in out Parse_State;
      Kernel                     : in     LR1_Items.Item_Set;
      Descriptor                 : in     WisiToken.Descriptor;
      Grammar                    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Nullable                   : in     Token_Array_Production_ID;
      Minimal_Terminal_Sequences : in     Minimal_Sequence_Array;
      Minimal_Terminal_First     : in     Token_Array_Token_ID)
   is
      use all type Ada.Containers.Count_Type;
      use LR1_Items.Item_Lists;
      use Token_ID_Arrays;

      subtype Terminals is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      function Find_Action (List : in Action_Arrays.Vector; ID : in Token_ID) return Minimal_Action
      is begin
         --  ID is a terminal after Dot in an item in a kernel that has List as
         --  the actions; return the appropriate action.
         for Node of List loop
            if Node.Symbol = ID then
               case Node.Actions.Item.Verb is
               when Shift =>
                  return (Shift, Node.Actions.Item.Production, ID, Node.Actions.Item.State);
               when Reduce =>
                  --  Item.Dot is a nonterm that starts with a nullable nonterm; reduce
                  --  to that first. After any more such reductions, the action will be
                  --  Shift ID.
                  return (Reduce, Node.Actions.Item.Production, 0);
               when Accept_It | WisiToken.Parse.LR.Error =>
                  raise SAL.Programmer_Error;
               end case;
            end if;
         end loop;
         raise SAL.Programmer_Error with
           "Set_Minimal_Complete_Actions: action for " & Image (ID, Descriptor) & " not found in state" &
           Kernel.Tree_Node.State'Image;
      end Find_Action;

      function Compute_Action (ID : in Token_ID) return Minimal_Action
      is begin
         if ID in Terminals then
            return Find_Action (State.Action_List, ID);

         else
            if Minimal_Terminal_First (ID) = Invalid_Token_ID then
               --  Item.Dot is a nullable nonterm; include a reduce to the null
               --  nonterm, rather than a shift of the following terminal; recover
               --  must do the reduce first.
               return (Reduce, (ID, Minimal_Terminal_Sequences (ID).Min_RHS), Token_Count => 0);

            else
               return Find_Action (State.Action_List, Minimal_Terminal_First (ID));
            end if;
         end if;
      end Compute_Action;

      function Length_After_Dot (Item : in LR1_Items.Item) return Ada.Containers.Count_Type
      is
         use Ada.Containers;
         Prod   : constant Production_ID := Item.Prod;
         Result : Count_Type             := 0;
         Tokens : Vector renames Grammar (Prod.LHS).RHSs (Prod.RHS).Tokens;
         I      : Token_ID_Arrays.Cursor := Tokens.To_Cursor (Item.Dot);
      begin
         if not Has_Element (I) then
            --  Can only compute this at runtime.
            return 0;
         end if;

         loop
            exit when not Has_Element (I);

            if Element (I) in Terminals then
               Result := Result + 1;
            else
               Result := Result + Min_Length (Minimal_Terminal_Sequences (Tokens (I)).Sequence);
            end if;
            Next (I);
         end loop;
         return Result;
      end Length_After_Dot;

   begin
      if Kernel.Tree_Node.State = 0 then
         --  State 0 has dot before all tokens, which is never needed in the
         --  Minimal_Complete_Action algorithm.
         return;

      elsif (for some Item of Kernel.Set =>
               Item.Prod.LHS = Descriptor.Accept_ID and
               (Item.Dot /= No_Index and then Productions.Constant_Ref_RHS
                  (Grammar, Item.Prod).Tokens (Item.Dot) = Descriptor.EOI_ID))
      then
         --  No actions
         return;
      end if;

      --  Set State.Kernel, and delete Items from Working_Set that are known
      --  to be non-minimal.
      declare
         use Ada.Containers;

         function Before_Dot (Item : in LR1_Items.Item) return Token_ID
         is
            Tokens : Token_ID_Arrays.Vector renames Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS).Tokens;
         begin
            if Item.Dot = Token_ID_Arrays.No_Index then
               return Tokens (Tokens.Last_Index);
            else
               return Tokens (Item.Dot - 1);
            end if;
         end Before_Dot;

         type State_Label is (Unknown, Keep_Always, Keep_If_Minimal, Drop);
         type Item_State (Label : State_Label := Unknown)
         is record
            case Label is
            when Keep_Always | Keep_If_Minimal =>
               Minimal_Action : WisiToken.Parse.LR.Minimal_Action;
               --  Minimal_Action.Production = Invalid_Production_ID (the default) if it is unknown.
            when Unknown | Drop =>
               null;
            end case;
         end record;

         subtype Kernel_Index is Count_Type range 1 .. Kernel.Set.Length;
         Item_States : array (Kernel_Index) of Item_State;
         I           : Kernel_Index := Kernel_Index'First;
         Min_Length  : Count_Type := Count_Type'Last;
      begin
         State.Kernel.Set_First_Last (Kernel_Index'First, Kernel_Index'Last);
         for Item of Kernel.Set loop
            declare
               RHS    : WisiToken.Productions.Right_Hand_Side renames Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS);
               Dot_ID : constant Token_ID :=
                 (if Item.Dot = No_Index
                  then Invalid_Token_ID
                  else RHS.Tokens (Item.Dot));

               --  Kernel components
               Length_After_Dot  : constant Count_Type := Set_Minimal_Complete_Actions.Length_After_Dot (Item);
               Reduce_Production : constant Production_ID :=
                 (if Length_After_Dot = 0
                  then (if Dot_ID in Nullable'Range then Nullable (Dot_ID) else Item.Prod)
                  else Invalid_Production_ID);
               Reduce_Count : constant Count_Type :=
                 (if Reduce_Production = Invalid_Production_ID
                  then 0
                  else Grammar (Reduce_Production.LHS).RHSs (Reduce_Production.RHS).Tokens.Length);
            begin
               --  Here we must compute Item_State (I).Label and .Minimal_Action,
               --  considering recursion.
               --
               --  Insert_Minimal_Complete_Actions does not need any recursion
               --  information at runtim, because we elminate all cases where it
               --  might here.
               --
               --  The strategy in Insert_Minimal_Complete_Actions when
               --  Item.Length_After_Dot = 0 is to compute Length_After_Dot by doing
               --  Reduce until a Shift is encountered, and using Length_After_Dot
               --  for that item.
               --
               --  Consider these kernel items with possible recursion (from
               --  ada_lite_lalr_re2c_t1.parse_table - not listed in state order here, to
               --  group related productions). The recursion of each production is
               --  shown after ';', if not all None.
               --
               --  State 2:
               --       86.0:exit_statement <= EXIT ^ identifier_opt WHEN expression_opt SEMICOLON
               --       86.1:exit_statement <= EXIT ^ identifier_opt SEMICOLON
               --
               --  State 42:
               --     103.2:name <= IDENTIFIER ^
               --
               --  State 30:
               --     103.3:name <= selected_component ^ ; ( 1 => Other_Left)
               --
               --  State 47:
               --      103.0:name <= name ^ LEFT_PAREN range_list RIGHT_PAREN ; ( 1 => Direct_Left,  3 => Other)
               --      103.1:name <= name ^ actual_parameter_part ; ( 1 => Direct_Left,  2 => Other_Right)
               --      113.2:primary <= name ^  ; ( 1 => Other_Left)
               --      124.0:selected_component <= name ^ DOT IDENTIFIER ; ( 1 => Other_Left)
               --
               --  State 68:
               --       95.1:generic_instantiation <= PROCEDURE name ^ IS NEW name SEMICOLON
               --      103.0:name <= name ^ LEFT_PAREN range_list RIGHT_PAREN ; ( 1 => Direct_Left,  3 => Other)
               --      103.1:name <= name ^ actual_parameter_part ; ( 1 => Direct_Left,  2 => Other_Right)
               --      115.0:procedure_specification <= PROCEDURE name ^ parameter_profile_opt
               --      124.0:selected_component <= name ^ DOT IDENTIFIER ; ( 1 => Other_Left)
               --
               --  State 50:
               --       87.1:expression <= relation_and_list ^ ; ( 1 => Other_Left)
               --      119.0:relation_and_list <= relation_and_list ^ AND relation;(1 => Direct_Left, 3 => Other_Right)
               --
               --  State 77:
               --       57.0:actual_parameter_part <= LEFT_PAREN ^ association_list RIGHT_PAREN ; ( 2 => Other)
               --      103.0:name <= name LEFT_PAREN ^ range_list RIGHT_PAREN ; ( 1 => Direct_Left,  3 => Other)
               --
               --  State 154:
               --      103.0:name <= name LEFT_PAREN range_list ^ RIGHT_PAREN ; ( 1 => Direct_Left,  3 => Other)
               --      118.0:range_list <= range_list ^ COMMA range_g ; (1 => Direct_Left, 3 => Other_Right)
               --
               --  State 251:
               --      110.0:parameter_specification <= IDENTIFIER COLON IDENTIFIER ^ COLON_EQUAL expression_opt
               --      110.1:parameter_specification <= IDENTIFIER COLON IDENTIFIER ^
               --
               --  From java_enum_ch19_lr1_t1.parse_table:
               --
               --  State 8:
               --       9.1:EnumConstantList <= EnumConstantList COMMA ^ EnumConstant ; (1 => Direct_Left)
               --      11.0:EnumBody <= LEFT_CURLY_BRACKET EnumConstantList COMMA ^ RIGHT_CURLY_BRACKET
               --
               --  From empty_production_2_lalr.parse_table:
               --
               --  State 5:
               --        8.0:declarations <= declarations ^ declaration ; (1 => Direct_Left, 2 => Other_Right)
               --        9.0:body <= IS declarations ^ BEGIN SEMICOLON ; (2 => Other)

               --  case 0: In states 42 and 30, there is only one possible action.
               --  Recursion is ignored; Minimal_Action is computed by
               --  Compute_Action, Label is Keep_Always.
               --
               --  In the following, we only consider kernels where there is more
               --  than one item.
               --
               --  case 1: In state 47 production 113.2, Dot is after all tokens, so
               --  the true Length_After_Dot must be computed at runtime. Recursion
               --  is not considered, because any other McKensie operation would also
               --  need to do a reduce to the LHS here. Label is Keep_Always,
               --  Minimal_Action is Reduce_Production.
               --
               --  In state 68 production 115.0, Length_After_Dot is 0 because
               --  parameter_profile_opt is nullable. We don't ignore recursion in
               --  this case; the nullable token may be in the recursion cycle. So if
               --  the production is recursive, the item is dropped.
               --
               --  case 2: In state 47, if LEFT_PAREN or First
               --  (actual_parameter_part) is inserted, a recursion cycle is followed
               --  via 103.0 or 103.1; these have Direct_Left recursion, can never be
               --  minimal, and we set Label to Drop. 113.2 breaks the recursion; it
               --  has Length_After_Dot = 0 and is covered by case 1. 124.0 has
               --  Other_Left; since Length_After_Dot is > 0, it follows the
               --  recursion cycle and is never minimal, so it is the same as
               --  Direct_Left. Similarly, in java_enum_ch19_lr1.parse_table state 8
               --  production 9.1, inserting EnumConstant continues the recursion
               --  cycle; left recursion applies even when it is not just before the
               --  parse point. On the other hand, in ada_lite state 154, both
               --  productions are left recursive; 103.0 could be preserved. In the
               --  current algorithm, both are dropped; this avoids needing cycle
               --  detection at runtime.
               --
               --  It is tempting to allow a minimal complete action for tokens in an
               --  RHS that are not in a recursion cycle. However, with partial
               --  recursion this is not possible because we don't have accurate
               --  recursion information, and in simpler languages that allow
               --  computing full recursion it is not very helpful. So we treat
               --  productions with left recursion independent of dot.
               --
               --  It is possible for both case 1 and case 2 to apply; see
               --  empty_production_2_lalar.parse_table State 5 above and
               --  ada_lite_ebnf_lalr.parse_table state 46. case 2 has precedence.
               --
               --  case 3: In state 251, there is no recursion, and Length_After_Dot
               --  is correct; Label is set to Keep_If_Minimal, Minimal_Action to
               --  Compute_Action. In State 77, Dot_ID is association_list which has
               --  Other recursion; we say "there is recursion at the parse point".
               --  However, Length_After_Dot is correct; it assumes the
               --  recursion-breaking case for the expansion of association_list. So
               --  this is the same as no recursion at the parse point
               --
               --  It is possible for both case 2 and 3 to be true; see
               --  empty_production_2_lalr.parse_table state 5. Case 2 has
               --  precedence (left recursion is worse).

               if Item_States'Length = 1 then
                  --  case 0
                  Item_States (I) :=
                    (Keep_Always,
                     (if Length_After_Dot = 0
                      then (Reduce, Reduce_Production, Reduce_Count)
                      else Compute_Action (Dot_ID)));

               elsif Length_After_Dot = 0 then
                  if Item.Dot /= No_Index and RHS.Recursion (1) in Direct_Left | Other_Left then
                     --  case 2
                     Item_States (I) := (Label => Drop);
                  else
                     --  case 1
                     Item_States (I) :=
                       (Label          => Keep_Always,
                        Minimal_Action => (Reduce, Reduce_Production, Reduce_Count));
                  end if;

               elsif RHS.Recursion (1) in Direct_Left | Other_Left then
                  --  case 2
                  Item_States (I) := (Label => Drop);

               else
                  --  case 3
                  Item_States (I) := (Keep_If_Minimal, Compute_Action (Dot_ID));
               end if;

               State.Kernel (I) :=
                 (Production        => Item.Prod,
                  Before_Dot        => Before_Dot (Item),
                  Length_After_Dot  => Length_After_Dot,
                  Reduce_Production => Reduce_Production,
                  Reduce_Count      => Reduce_Count);

               if Item_States (I).Label = Keep_If_Minimal then
                  if Length_After_Dot < Min_Length then
                     Min_Length := Length_After_Dot;
                  end if;
               end if;

               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line
                    ("kernel" & I'Image & " " & Strict_Image (State.Kernel (I)) &
                       " ; " & Item_States (I).Label'Image &
                       " " & State.Kernel (I).Length_After_Dot'Image);
               end if;

               if I < Kernel_Index'Last then
                  I := I + 1;
               end if;
            end;
         end loop;

         --  It is tempting to Assert that if all items are dropped, there is a
         --  grammar recursion cycle with no exit. But that is not true; see
         --  java_expressions_ch19_lr1.parse_table, state 8. However, that
         --  state should never be encountered during Insert_Minimal_Complete,
         --  because it is never minimal. So we set Minimal_Actions to empty.

         --  Update State_Items based on Min_Length
         for I in Item_States'Range loop

            case Item_States (I).Label is
            when Unknown =>
               null;

            when Keep_Always =>
               pragma Assert (Item_States (I).Minimal_Action.Production /= Invalid_Production_ID);

            when Keep_If_Minimal =>
               if State.Kernel (I).Length_After_Dot = Min_Length then
                  null;
               else
                  Item_States (I) := (Label => Drop);
               end if;

            when Drop =>
               null;
            end case;
         end loop;

         --  Set State.Minimal_Actions
         for Item_State of Item_States loop
            case Item_State.Label is
            when Unknown | Drop =>
               null;

            when Keep_Always | Keep_If_Minimal =>
               if (for some A of State.Minimal_Complete_Actions => A = Item_State.Minimal_Action) then
                  --  Duplicate action; see three_action_conflict_lalr.parse_table state
                  --  3 or lalr_generator_bug_01_lalr.parse_table state 28
                  null;
               else
                  pragma Assert (Item_State.Minimal_Action.Production /= Invalid_Production_ID);
                  State.Minimal_Complete_Actions.Append (Item_State.Minimal_Action);
               end if;
            end case;
         end loop;

         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.Put_Line (Image (State.Minimal_Complete_Actions, Descriptor));
         end if;
      end;
   end Set_Minimal_Complete_Actions;

   ----------
   --  Parse table output

   procedure Put_Text_Rep
     (Table        : in Parse_Table;
      File_Name    : in String;
      Action_Names : in Names_Array_Array;
      Check_Names  : in Names_Array_Array)
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Containers;
      use SAL.Unix_Text_IO;
      File : File_Type;
   begin
      --  Only space, semicolon, newline delimit object values. Bounds of
      --  arrays output before each array, unless known from discriminants.
      --  End of lists indicated by semicolon. Action, Check subprograms are
      --  represented by True if present, False if not.
      --
      --  We use Unix_Text_IO to enforce Unix line endings; a later dos2unix
      --  step is very slow on very large files.

      Create (File, Out_File, File_Name);

      --  First the discriminants
      Put (File,
           Trimmed_Image (Table.State_First) & State_Index'Image (Table.State_Last) &
             Token_ID'Image (Table.First_Terminal) & Token_ID'Image (Table.Last_Terminal) &
             Token_ID'Image (Table.First_Nonterminal) & Token_ID'Image (Table.Last_Nonterminal));
      New_Line (File);

      for State of Table.States loop
         Put (File, Trimmed_Image (State.Action_List.Length) & ' ');
         for I in State.Action_List.First_Index .. State.Action_List.Last_Index loop
            --  Action first, for historical reasons
            declare
               Node_I : Action_Node renames State.Action_List (I);
               Node_J : Parse_Action_Node_Ptr := Node_I.Actions;
            begin
               loop
                  --  WORKAROUND: subtype'Image in GNAT Community 2020 produces integer.
                  Put (File, All_Parse_Action_Verbs'Image (All_Parse_Action_Verbs (Node_J.Item.Verb)));
                  Put (File, Node_J.Item.Production.LHS'Image & Node_J.Item.Production.RHS'Image);

                  case Node_J.Item.Verb is
                  when Shift =>
                     Put (File, State_Index'Image (Node_J.Item.State));

                  when Reduce | Accept_It =>
                     if Action_Names (Node_J.Item.Production.LHS) /= null and then
                       Action_Names (Node_J.Item.Production.LHS)(Node_J.Item.Production.RHS) /= null
                     then
                        Put (File, " true");
                     else
                        Put (File, " false");
                     end if;
                     if Check_Names (Node_J.Item.Production.LHS) /= null and then
                       Check_Names (Node_J.Item.Production.LHS)(Node_J.Item.Production.RHS) /= null
                     then
                        Put (File, " true");
                     else
                        Put (File, " false");
                     end if;

                     Put (File, Ada.Containers.Count_Type'Image (Node_J.Item.Token_Count));

                  when Parse.LR.Error =>
                     raise SAL.Programmer_Error;
                  end case;

                  Node_J := Node_J.Next;
                  exit when Node_J = null;
                  Put (File, ' ');
               end loop;
               Put (File, ';');
               Put (File, Token_ID'Image (Node_I.Symbol));
            end;
            if I = State.Action_List.Last_Index then
               Put_Line (File, ";");
            else
               New_Line (File);
            end if;
         end loop;

         if State.Goto_List.Length > 0 then
            Put (File, Trimmed_Image (State.Goto_List.Length));
            for Node of State.Goto_List loop
               Put (File, Node.Symbol'Image & Node.State'Image);
            end loop;
         end if;
         Put (File, ';');
         New_Line (File);

         if State.Kernel.Length = 0 then
            --  Kernel not set for state 0
            Put_Line (File, "0 -1");

         else
            Put (File, Count_Type'Image (State.Kernel.First_Index));
            Put (File, Count_Type'Image (State.Kernel.Last_Index));
            for Item of State.Kernel loop
               Put (File, Token_ID'Image (Item.Production.LHS) & Item.Production.RHS'Image);
               Put (File, Item.Before_Dot'Image);
               Put (File, Count_Type'Image (Item.Length_After_Dot));
               Put (File, Token_ID'Image (Item.Reduce_Production.LHS) & Item.Reduce_Production.RHS'Image);
               Put (File, Item.Reduce_Count'Image);
            end loop;
            New_Line (File);
         end if;

         if State.Minimal_Complete_Actions.Length = 0 then
            null;
         else
            Put (File, Count_Type'Image (State.Minimal_Complete_Actions.First_Index));
            Put (File, Count_Type'Image (State.Minimal_Complete_Actions.Last_Index));
            for Action of State.Minimal_Complete_Actions loop
               Put (File, " ");
               --  WORKAROUND: subtype'Image in GNAT Community 2020 produces integer.
               Put (File, All_Parse_Action_Verbs'Image (All_Parse_Action_Verbs (Action.Verb)));
               Put (File, Action.Production.LHS'Image & Action.Production.RHS'Image);
               case Action.Verb is
               when Shift =>
                  Put (File, Token_ID'Image (Action.ID) & State_Index'Image (Action.State));
               when Reduce =>
                  Put (File, Action.Token_Count'Image);
               end case;
            end loop;
         end if;
         Put_Line (File, ";");
      end loop;
      Close (File);
   end Put_Text_Rep;

   procedure Put (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor)
   is
      use Ada.Containers;
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.Production.LHS, Descriptor));
      when Accept_It =>
         Put ("accept it");
      when Parse.LR.Error =>
         Put ("ERROR");
      end case;
   end Put;

   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(Insert =>");
      for I in Item.Insert'Range loop
         Put (" " & Padded_Image (I, Descriptor) & " =>" & Natural'Image (Item.Insert (I)));
         if I = Item.Insert'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("(Delete =>");
      for I in Item.Delete'Range loop
         Put (" " & Padded_Image (I, Descriptor) & " =>" & Natural'Image (Item.Delete (I)));
         if I = Item.Delete'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("(Push_Back =>");
      for I in Item.Push_Back'Range loop
         Put (" " & Padded_Image (I, Descriptor) & " =>" & Natural'Image (Item.Push_Back (I)));
         if I = Item.Push_Back'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("(Undo_Reduce =>");
      for I in Item.Undo_Reduce'Range loop
         Put (" " & Padded_Image (I, Descriptor) & " =>" & Natural'Image (Item.Undo_Reduce (I)));
         if I = Item.Undo_Reduce'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("Minimal_Complete_Cost_Delta => " & Integer'Image (Item.Minimal_Complete_Cost_Delta));
      Put_Line ("Fast_Forward      => " & Integer'Image (Item.Fast_Forward));
      Put_Line ("Matching_Begin    => " & Integer'Image (Item.Matching_Begin));
      Put_Line ("Ignore_Check_Fail =>" & Integer'Image (Item.Ignore_Check_Fail));
      Put_Line ("Task_Count        =>" & System.Multiprocessors.CPU_Range'Image (Item.Task_Count));
      Put_Line ("Check_Limit       =>" & Item.Check_Limit'Image);
      Put_Line ("Check_Delta_Limit =>" & Integer'Image (Item.Check_Delta_Limit));
      Put_Line ("Enqueue_Limit     =>" & Integer'Image (Item.Enqueue_Limit));
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Parse_Action_Rec)
   is
      use Ada.Containers;
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));
         Put (" " & Trimmed_Image (Item.Production));

      when Reduce =>
         Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.Production.LHS, Descriptor));
         Put (" " & Trimmed_Image (Item.Production));

      when Accept_It =>
         Put ("accept it");
         Put (" " & Trimmed_Image (Item.Production));

      when Parse.LR.Error =>
         Put ("ERROR");
      end case;
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor; Action : in Parse_Action_Node_Ptr)
   is
      use Ada.Text_IO;
      Ptr    : Parse_Action_Node_Ptr   := Action;
      Column : constant Positive_Count := Col;
   begin
      loop
         Put (Descriptor, Ptr.Item);
         Ptr := Ptr.Next;
         exit when Ptr = null;
         Put_Line (",");
         Set_Col (Column);
      end loop;
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor; State : in Parse_State)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      for Action of State.Action_List loop
         Put ("   " & Image (Action.Symbol, Descriptor) &
                (Descriptor.Image_Width - Image (Action.Symbol, Descriptor)'Length) * ' '
                & " => ");
         Put (Descriptor, Action.Actions);
         New_Line;
      end loop;

      --  The error line is redundant, but we keep it to match existing good parse tables.
      Put_Line ("   default" & (Descriptor.Image_Width - 7) * ' ' & " => ERROR");

      if State.Goto_List.Length > 0 then
         New_Line;
      end if;

      for Item of State.Goto_List loop
         Put_Line
           ("   " & Image (Item.Symbol, Descriptor) &
              (Descriptor.Image_Width - Image (Item.Symbol, Descriptor)'Length) * ' ' &
              " goto state" & Item.State'Image);
      end loop;

      New_Line;
      Put ("   Minimal_Complete_Action => "); --  No trailing 's' for compatibility with previous good parse tables.
      case State.Minimal_Complete_Actions.Length is
      when 0 =>
         null;
      when 1 =>
         --  No () here for compatibity with previous known good parse tables.
         declare
            Action : Minimal_Action renames State.Minimal_Complete_Actions (State.Minimal_Complete_Actions.First_Index);
         begin
            case Action.Verb is
            when Shift =>
               Put (Image (Action.ID, Descriptor));
            when Reduce =>
               Put (Image (Action.Production.LHS, Descriptor));
            end case;
            Put (" " & Trimmed_Image (Action.Production));
         end;
      when others =>
         Put ("(");
         for I in State.Minimal_Complete_Actions.First_Index .. State.Minimal_Complete_Actions.Last_Index loop
            declare
               Action : Minimal_Action renames State.Minimal_Complete_Actions (I);
            begin
               case Action.Verb is
               when Shift =>
                  Put (Image (Action.ID, Descriptor));
               when Reduce =>
                  Put (Image (Action.Production.LHS, Descriptor));
               end case;
               Put (" " & Trimmed_Image (Action.Production));
            end;
            if I < State.Minimal_Complete_Actions.Last_Index then
               Put (", ");
            end if;
         end loop;
         Put (")");
      end case;
      New_Line;
   end Put;

   procedure Put_Parse_Table
     (Table                 : in Parse_Table_Ptr;
      Parse_Table_File_Name : in String;
      Title                 : in String;
      Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Recursions            : in Generate.Recursions;
      Kernels               : in LR1_Items.Item_Set_List;
      Conflicts             : in Conflict_Count_Lists.Vector;
      Descriptor            : in WisiToken.Descriptor;
      Include_Extra         : in Boolean := False)
   is
      use all type WisiToken.Syntax_Trees.Element_Index;
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;

      Parse_Table_File : File_Type;

      Minimal_Complete_Action_States : Integer := 0;
      Minimal_Complete_Actions       : Ada.Containers.Count_Type := 0;
   begin
      Create (Parse_Table_File, Out_File, Parse_Table_File_Name);
      Set_Output (Parse_Table_File);
      Put_Line ("Tokens:");
      WisiToken.Put_Tokens (Descriptor);

      New_Line;
      Put_Line ("Productions:");
      WisiToken.Productions.Put (Grammar, Descriptor);

      if Include_Extra then
         New_Line;
         Put_Line ((if Recursions.Full then "Recursions:" else "Partial recursions:"));
         for I in Recursions.Recursions.First_Index .. Recursions.Recursions.Last_Index loop
            Put_Line (Trimmed_Image (I) & " => " & Grammar_Graphs.Image (Recursions.Recursions (I)));
         end loop;
      end if;

      if Table.McKenzie_Param.Check_Limit /= Default_McKenzie_Param.Check_Limit or
        Table.McKenzie_Param.Check_Delta_Limit /= Default_McKenzie_Param.Check_Delta_Limit or
        Table.McKenzie_Param.Enqueue_Limit /= Default_McKenzie_Param.Enqueue_Limit
      then
         New_Line;
         Put_Line ("McKenzie:");
         Put (Table.McKenzie_Param, Descriptor);
      end if;

      New_Line;
      Put_Line (Title & " Parse Table:");

      for State_Index in Table.States'Range loop
         Put_Line ("State" & Unknown_State_Index'Image (State_Index) & ":");

         declare
            use WisiToken.Generate.LR1_Items;
         begin
            for Item of Kernels (State_Index).Set loop
               if In_Kernel (Grammar, Descriptor, Item) then
                  Put ("  " & Image (Grammar, Descriptor, Item, Show_Lookaheads => False));
                  New_Line;
               end if;
            end loop;
         end;
         New_Line;
         Put (Descriptor, Table.States (State_Index));

         if State_Index /= Table.States'Last then
            New_Line;
         end if;

         Minimal_Complete_Actions := @ + Table.States (State_Index).Minimal_Complete_Actions.Length;
         if Table.States (State_Index).Minimal_Complete_Actions.Length > 0 then
            Minimal_Complete_Action_States := @ + 1;
         end if;
      end loop;

      New_Line;
      Put_Line
        (Trimmed_Image (Minimal_Complete_Action_States) & " states with minimal_complete_actions;" &
           Minimal_Complete_Actions'Image & " total minimal_complete_actions.");

      declare
         use Ada.Strings.Unbounded;
         Line          : Unbounded_String;
         State_Count   : Integer          := 0;
         Accept_Reduce : Integer          := 0;
         Shift_Reduce  : Integer          := 0;
         Reduce_Reduce : Integer          := 0;
      begin
         for State in Conflicts.Iterate loop
            if Conflicts (State).Accept_Reduce > 0 or
              Conflicts (State).Shift_Reduce > 0 or
              Conflicts (State).Reduce_Reduce > 0
            then
               State_Count   := State_Count + 1;
               if Include_Extra then
                  Line := Line & Conflict_Count_Lists.To_Index (State)'Image;
               end if;
               Accept_Reduce := @ + Conflicts (State).Accept_Reduce;
               Shift_Reduce  := @ + Conflicts (State).Shift_Reduce;
               Reduce_Reduce := @ + Conflicts (State).Reduce_Reduce;
            end if;
         end loop;

         if State_Count > 0 then
            New_Line;

            if Include_Extra then
               Line := Trimmed_Image (State_Count) & " states with conflicts:" & Line;
               Indent_Wrap (-Line);
               New_Line;
            else
               Put_Line (Trimmed_Image (State_Count) & " states with conflicts");
            end if;

            Put_Line
              (Accept_Reduce'Image & " accept/reduce conflicts," &
                 Shift_Reduce'Image & " shift/reduce conflicts," &
                 Reduce_Reduce'Image & " reduce/reduce conflicts");
         else
            New_Line;
            Put_Line (" 0 accept/reduce conflicts, 0 shift/reduce conflicts, 0 reduce/reduce conflicts");
         end if;
      end;
      Set_Output (Standard_Output);
      Close (Parse_Table_File);
   end Put_Parse_Table;

end WisiToken.Generate.LR;
