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
package body WisiToken.Generate.LR is

   package RHS_Set is new SAL.Gen_Unbounded_Definite_Vectors (Natural, Boolean, Default_Element => False);

   type LHS_RHS_Set is array (Token_ID range <>) of RHS_Set.Vector;

   ----------
   --  Body subprograms, alphabetical

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

   ----------
   --  Public subprograms, declaration order

   procedure Put
     (Item       : in Conflict_Lists.List;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor)
   is begin
      for Conflict of Item loop
         Ada.Text_IO.Put_Line (File, Image (Conflict, Descriptor));
      end loop;
   end Put;

   procedure Add_Action
     (Symbol               : in     Token_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Arrays.Vector;
      Closure              : in     LR1_Items.Item_Set;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor)
   is
      Matching_Action : constant Action_Arrays.Find_Reference_Type := Action_List.Find (Symbol);
   begin
      if Trace_Generate_Table > Detail then
         Ada.Text_IO.Put (Image (Symbol, Descriptor) & " => ");
         Put (Descriptor, Action);
         Ada.Text_IO.New_Line;
      end if;

      if Matching_Action.Element /= null then
         if Is_In (Action, Matching_Action.Actions) then
            --  Action is already in the list.
            if Trace_Generate_Table > Detail then
               Ada.Text_IO.Put_Line (" - already present");
            end if;
            return;
         else
            --  There is a conflict. Report it and add it, so the
            --  generalized parser can follow all paths
            declare
               --  Enforce canonical Shift/Reduce or Accept/Reduce order, to simplify
               --  searching and code generation. There can be only one Shift in the
               --  list of conflicting actions, so we keep it the first item in the
               --  list; no order in the rest of the list.
               Action_A : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Action else Matching_Action.Actions.Item);

               Action_B : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Matching_Action.Actions.Item else Action);

               New_Conflict : constant Conflict :=
                 (Action_A    => Action_A.Verb,
                  Action_B    => Action_B.Verb,
                  LHS_A       => Find
                    (Closure, Action_A, Symbol, Grammar, Has_Empty_Production, First_Nonterm_Set, Descriptor),
                  LHS_B       => Find
                    (Closure, Action_B, Symbol, Grammar, Has_Empty_Production, First_Nonterm_Set, Descriptor),
                  State_Index => Closure.State,
                  On          => Symbol);

               Counts : Conflict_Count_Lists.Cursor;
            begin
               for Cur in Conflict_Counts.Iterate loop
                  if Conflict_Counts (Cur).State = Closure.State then
                     Counts := Cur;
                     exit;
                  end if;
               end loop;

               if not Conflict_Count_Lists.Has_Element (Counts) then
                  Conflict_Counts.Append ((Closure.State, others => 0));
                  Counts := Conflict_Counts.Last;
               end if;

               declare
                  use Conflict_Count_Lists;
                  Counts_Ref : constant Reference_Type := Reference (Conflict_Counts, Counts);
               begin
                  case Action_A.Verb is
                  when Shift =>
                     case Action_B.Verb is
                     when Shift | Accept_It | WisiToken.Parse.LR.Error =>
                        raise SAL.Programmer_Error;
                     when Reduce =>
                        Counts_Ref.Shift_Reduce := Counts_Ref.Shift_Reduce + 1;
                     end case;
                  when Reduce =>
                     case Action_B.Verb is
                     when Shift | Accept_It | WisiToken.Parse.LR.Error =>
                        raise SAL.Programmer_Error;
                     when Reduce =>
                        Counts_Ref.Reduce_Reduce := Counts_Ref.Reduce_Reduce + 1;
                     end case;
                  when Accept_It =>
                     case Action_B.Verb is
                     when Shift | Accept_It | WisiToken.Parse.LR.Error =>
                        raise SAL.Programmer_Error;
                     when Reduce =>
                        Counts_Ref.Accept_Reduce := Counts_Ref.Accept_Reduce + 1;
                     end case;
                  when WisiToken.Parse.LR.Error =>
                     raise SAL.Programmer_Error;
                  end case;
               end;

               if not Is_Present (New_Conflict, Conflicts) then
                  --  The same conflict may occur in a different
                  --  item set. Only add it to conflicts once.
                  Conflicts.Append (New_Conflict);

                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line (" - conflict added: " & Image (New_Conflict, Descriptor));
                  end if;
               else
                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line (" - conflict duplicate: " & Image (New_Conflict, Descriptor));
                  end if;
               end if;

               if Action.Verb = Shift then
                  Matching_Action.Actions := new Parse_Action_Node'(Action, Matching_Action.Actions);
               else
                  Matching_Action.Actions.Next := new Parse_Action_Node'(Action, Matching_Action.Actions.Next);
               end if;
            end;
         end if;
      else
         WisiToken.Parse.LR.Add (Action_List, Symbol, Action);
      end if;
   end Add_Action;

   procedure Add_Actions
     (Closure              : in     LR1_Items.Item_Set;
      Table                : in out Parse_Table;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor)
   is
      use WisiToken.Token_ID_Arrays;

      State : constant State_Index := Closure.State;
   begin
      if Trace_Generate_Table > Detail then
         Ada.Text_IO.Put_Line ("adding actions for state" & State_Index'Image (State));
      end if;

      for Item of Closure.Set loop
         if Item.Dot = No_Element then
            --  Pointer is at the end of the production; add a reduce action.

            Add_Lookahead_Actions
              (Item, Table.States (State).Action_List, Grammar, Has_Empty_Production, First_Nonterm_Set,
               Conflict_Counts, Conflicts, Closure, Descriptor);

         elsif Element (Item.Dot) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
            --  Dot is before a terminal token.
            declare
               use all type Ada.Containers.Count_Type;

               P_ID : constant Production_ID := Item.Prod;

               Dot_ID : constant Token_ID := Element (Item.Dot);
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
                        Table.States (State).Action_List, Closure,
                        Grammar, Has_Empty_Production, First_Nonterm_Set, Conflict_Counts, Conflicts, Descriptor);
                  end;
               else
                  if Goto_State /= Unknown_State then
                     Add_Action
                       (Dot_ID,
                        (Shift, P_ID, Goto_State),
                        Table.States (State).Action_List,
                        Closure, Grammar, Has_Empty_Production, First_Nonterm_Set,
                        Conflict_Counts, Conflicts, Descriptor);
                  end if;
               end if;
            end;
         else
            --  Dot is before a non-terminal token; no action.
            if Trace_Generate_Table > Detail then
               Ada.Text_IO.Put_Line (Image (Element (Item.Dot), Descriptor) & " => no action");
            end if;
         end if;
      end loop;

      --  We don't place a default error action at the end of every state;
      --  Parse.LR.Action_For returns Table.Error_Action when Symbol is not found.
      Table.Error_Action := new Parse_Action_Node'((Verb => WisiToken.Parse.LR.Error, others => <>), null);

      for Item of Closure.Goto_List loop
         if Item.Symbol in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal then
            Add_Goto (Table.States (State), Item.Symbol, Item.State); -- note list is already sorted.
         end if;
      end loop;
   end Add_Actions;

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item;
      Action_List          : in out Action_Arrays.Vector;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Descriptor           : in     WisiToken.Descriptor)
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
               Add_Action
                 (Lookahead, Action, Action_List, Closure, Grammar,
                  Has_Empty_Production, First_Nonterm_Set, Conflict_Counts, Conflicts, Descriptor);
            end if;
         end if;
      end loop;
   end Add_Lookahead_Actions;

   procedure Delete_Known
     (Conflicts       : in out Conflict_Lists.List;
      Known_Conflicts : in out Conflict_Lists.List)
   is
      --  Delete all elements in Conflicts that match an element in
      --  Known_Conflicts. There can be more than one Conflict that
      --  match one Known_Conflict.
      use Conflict_Lists;
      Known      : Cursor  := Known_Conflicts.First;
      Next_Known : Cursor;
   begin
      loop
         exit when Known = No_Element;
         Next_Known := Next (Known);
         declare
            I      : Cursor  := Conflicts.First;
            Next_I : Cursor;
            Used   : Boolean := False;
         begin
            loop
               exit when I = No_Element;
               Next_I := Next (I);
               if Match (Element (Known), Conflicts.Constant_Reference (I)) then
                  Delete (Conflicts, I);
                  Used := True;
               end if;
               I := Next_I;
            end loop;

            if Used then
               Delete (Known_Conflicts, Known);
            end if;
         end;
         Known := Next_Known;
      end loop;
   end Delete_Known;

   function Find
     (Closure              : in LR1_Items.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token_ID;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Descriptor           : in WisiToken.Descriptor)
     return Token_ID
   is
      use WisiToken.Token_ID_Arrays;

      ID_I : Cursor;
   begin
      case Action.Verb is
      when Reduce | Accept_It =>
         --  If the nonterm produced by the reduce is the LHS of the state
         --  production, use it.
         for Item of Closure.Set loop
            if LR1_Items.In_Kernel (Grammar, Descriptor, Item) and
              Action.Production.LHS = Item.Prod.LHS
            then
               return Item.Prod.LHS;
            end if;
         end loop;

         --  The reduce nonterm is after Dot in a state production; find which
         --  one, use that.
         for Item of Closure.Set loop
            if LR1_Items.In_Kernel (Grammar, Descriptor, Item) then
               ID_I := Item.Dot;
               loop
                  if ID_I = No_Element then
                     if Item.Lookaheads (Lookahead) then
                        return Item.Prod.LHS;
                     end if;
                  else
                     declare
                        Dot_ID : Token_ID renames Element (ID_I);
                     begin
                        if Dot_ID = Lookahead or
                          (Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                             First (Dot_ID, Lookahead))
                        then
                           return Item.Prod.LHS;
                        end if;
                        exit when Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                          not Has_Empty_Production (Dot_ID);
                     end;
                  end if;

                  exit when ID_I = No_Element;
                  Next (ID_I);
               end loop;
            end if;
         end loop;

      when Shift =>

         for Item of Closure.Set loop
            --  Lookahead (the token shifted) is starting a nonterm in a state
            --  production; it is in First of that nonterm.
            if LR1_Items.In_Kernel (Grammar, Descriptor, Item) then
               ID_I := Item.Dot;
               loop
                  exit when ID_I = No_Element;
                  declare
                     Dot_ID : Token_ID renames Element (ID_I);
                  begin
                     if Dot_ID = Lookahead or
                       (Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                          First (Dot_ID, Lookahead))
                     then
                        return Item.Prod.LHS;
                     end if;

                     exit when Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                       not Has_Empty_Production (Dot_ID);
                  end;

                  Next (ID_I);
               end loop;
            end if;
         end loop;

      when WisiToken.Parse.LR.Error =>
         raise SAL.Programmer_Error;
      end case;

      Ada.Text_IO.Put_Line
        ("item for " & Image (Action, Descriptor) & " on " & Image (Lookahead, Descriptor) & " not found in");
      LR1_Items.Put (Grammar, Descriptor, Closure, Kernel_Only => True);
      raise SAL.Programmer_Error;
   end Find;

   function Image (Item : in Conflict; Descriptor : in WisiToken.Descriptor) return String
   is begin
      return
        ("%conflict " &
           Conflict_Parse_Actions'Image (Item.Action_A) & "/" &
           Conflict_Parse_Actions'Image (Item.Action_B) & " in state " &
           Image (Item.LHS_A, Descriptor) & ", " &
           Image (Item.LHS_B, Descriptor) &
           " on token " & Image (Item.On, Descriptor) &
           " (" & State_Index'Image (Item.State_Index) & ")"); -- state number last for easier delete
   end Image;

   function Is_Present (Item : in Conflict; Conflicts : in Conflict_Lists.List) return Boolean
   is
      use Conflict_Lists;
      I : Cursor := Conflicts.First;
   begin
      loop
         exit when I = No_Element;
         if Match (Item, Conflicts.Constant_Reference (I)) then
            return True;
         end if;
         I := Next (I);
      end loop;
      return False;
   end Is_Present;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean
   is begin
      --  Ignore State_Index. Actions are in canonical order; enforced
      --  in Add_Action above. For reduce/reduce, LHS_A, LHS_B are not
      --  in canonical order.
      return
        Known.Action_A = Item.Action_A and
        Known.Action_B = Item.Action_B and
        ((Known.LHS_A = Item.LHS_A and Known.LHS_B = Item.LHS_B) or
           (Known.LHS_B = Item.LHS_A and Known.LHS_A = Item.LHS_B)) and
        Known.On = Item.On;
   end Match;

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
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
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
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Image (All_Seq_Set, Descriptor, Inverted => True));
               raise Grammar_Error with "sequences not resolved";
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

      Working_Set : LR1_Items.Item_Lists.List := Kernel.Set;

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
         raise SAL.Programmer_Error;
      end Find_Action;

      function Length_After_Dot (Item : in LR1_Items.Item) return Ada.Containers.Count_Type
      is
         use Ada.Containers;
         Prod   : constant Production_ID := Item.Prod;
         I      : Token_ID_Arrays.Cursor := Item.Dot;
         Result : Count_Type             := 0;
         Tokens : Vector renames Grammar (Prod.LHS).RHSs (Prod.RHS).Tokens;
      begin
         if I = Token_ID_Arrays.No_Element then
            --  Can only compute this at runtime.
            return 0;
         end if;

         loop
            exit when I = Token_ID_Arrays.No_Element;

            if Tokens (I) in Terminals then
               Result := Result + 1;
            else
               Result := Result + Min_Length (Minimal_Terminal_Sequences (Tokens (I)).Sequence);
            end if;
            Next (I);
         end loop;
         return Result;
      end Length_After_Dot;

      procedure Delete_Non_Minimal
      is
         use Ada.Containers;

         I            : LR1_Items.Item_Lists.Cursor := Working_Set.First;
         Kernel_Index : Count_Type := State.Kernel.First_Index;

         procedure Delete
         is
            Del : LR1_Items.Item_Lists.Cursor := I;
         begin
            Next (I);
            Kernel_Index := Kernel_Index + 1;
            Working_Set.Delete (Del);
         end Delete;

      begin

      end Delete_Non_Minimal;

   begin
      if Kernel.State = 0 then
         --  State 0 has dot before all tokens, which is never needed in the
         --  Minimal_Complete_Action algorithm.
         return;

      elsif (for some Item of Working_Set =>
               Item.Prod.LHS = Descriptor.Accept_ID and
               (Has_Element (Item.Dot) and then Element (Item.Dot) = Descriptor.EOI_ID))
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
            if Item.Dot = Token_ID_Arrays.No_Element then
               return Tokens (Tokens.Last_Index);
            else
               return Tokens (Prev (Item.Dot));
            end if;
         end Before_Dot;

         type State_Label is (Unknown, Always_Keep, Keep_If_Min_Length, Drop);
         type Item_State (Label : State_Label := Unknown)
         is record
            Item : Item_Lists.Cursor;
            case Label is
            when Always_Keep | Keep_If_Min_Length =>
               Minimal_Action : WisiToken.Parse.LR.Minimal_Action;
               --  Minimal_Action.Production = Invalid_Production_ID (the default) if it is unknown.
            when Unknown | Drop =>
               null;
            end case;
         end record;

         Item_States : array (1 .. Kernel.Set.Length) of Item_State;
         I           : Count_Type := 1; --  Indexes State.Kernel, Item_States
         Min_Length  : Count_Type := Count_Type'Last;

      begin
         State.Kernel.Set_First_Last (1, Kernel.Set.Length);
         for Item of Kernel.Set loop
            declare
               Dot_Index : constant Positive := To_Index (Item.Dot); -- Indexes RHS.Tokens, Recursion.
               RHS       : WisiToken.Productions.Right_Hand_Side renames
                 Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS);

               --  Kernel components
               Length_After_Dot  : constant Count_Type := Set_Minimal_Complete_Actions.Length_After_Dot (Item);
               Reduce_Production : constant Production_ID :=
                 (if Length_After_Dot = 0
                  then Nullable (Element (Item.Dot))
                  else Invalid_Production_ID);
               Reduce_Count      : constant Count_Type :=
                 (if Length_After_Dot = 0
                  then 0
                  else Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS).Tokens.Length);
               Recursion         : Recursion_Class;
            begin
               --  State.Kernel (I).Recursion is used at runtime by
               --  Insert_Minimal_Complete to help determine what token to insert.
               --  Consider these kernel items with possible recursion (from
               --  ada_lite_lalr.parse_table - not listed in state order here, to
               --  group related productions):
               --
               --  State 43:
               --     103.2:name <= IDENTIFIER ^
               --
               --  State 30:
               --     103.3:name <= selected_component ^
               --
               --  State 47:
               --      103.0:name <= name ^ LEFT_PAREN range_list RIGHT_PAREN
               --      103.1:name <= name ^ actual_parameter_part
               --      113.2:primary <= name ^
               --      124.0:selected_component <= name ^ DOT IDENTIFIER
               --
               --  State 77:
               --       57.0:actual_parameter_part <= LEFT_PAREN ^ association_list RIGHT_PAREN
               --      103.0:name <= name LEFT_PAREN ^ range_list RIGHT_PAREN
               --
               --  State 37:
               --      112.0:paren_expression <= LEFT_PAREN ^ expression_opt RIGHT_PAREN
               --
               --  State 85:
               --      112.0:paren_expression <= LEFT_PAREN expression_opt ^ RIGHT_PAREN
               --
               --  State 48:
               --      113.3:primary <= paren_expression ^
               --
               --  103.0, .1 are direct left recursive; 124.0 is indirect left
               --  recursive. 112 is other recursive.
               --
               --  case 1: In states 43 and 30, Item.Length_After_Dot = 0, which
               --  means Insert_Minimal_Complete must compute the actual
               --  after_dot_length. Thus Insert_Minimal_Complete is already handling
               --  this, and recursion needs as much Recursion information as we can
               --  give it, which is Other if any token has Recursion other than
               --  None, otherwise None.
               --
               --  case 2: In state 47, if DOT, LEFT_PAREN, or First
               --  (actual_parameter_part) is inserted, a recursion cycle is followed
               --  via 103.0, .1 or 124.0; these have Direct_Left recursion, can
               --  never be minimal, and are dropped here. A token in First (name)
               --  uses 113.2, breaking the recursion; that is the minimal action.
               --  Other_Left recursion must be handled at runtime in
               --  Insert_Minimal_Complete.
               --
               --  case 3: In State 37, first assume expression_opt is not nullable
               --  (token after dot will be in similar cases). If LEFT_PAREN is
               --  inserted, the recursion cycle is followed. If any other token in
               --  First (expression_opt) is inserted, the recursion cycle is not
               --  followed. We cannot compute in general which tokens will follow
               --  the recursion, so we let Insert_Minimal_Compute handle this;
               --  recursion is important if 'Recursion (To_Index (Dot)) = Other'
               --  (which implies 'To_Index (Dot) > 2').
               --
               --  It is possible for both case 2 and 3 to be true; case 2 has
               --  precedence (left recursion is worse).
               --
               --  case 4: In State 37, if expression_opt is nullable, then reducing
               --  0 tokens to expression_opt breaks the recursion; we want that to
               --  be the minimal action for this Item. Note that it makes no sense
               --  for the recursion token to be nullable in case 2.

               if Item.Dot = No_Element then
                  --  case 1
                  pragma Assert (Length_After_Dot = 0);
                  Recursion := (if (for all R of RHS.Recursion => R = None) then None else Other);

                  Item_States (I) :=
                    (Label          => Always_Keep,
                     Item           => Item,
                     Minimal_Action =>
                       (Reduce, Item.Prod,
                        Token_Count => Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS).Tokens.Length));

               elsif Element (Item.Dot) in Nullable'Range and then
                 Nullable (Element (Item.Dot)) /= Invalid_Production_ID
               then
                  --  case 4

                  pragma Assert (Dot_Index /= 2 or else RHS.Recursion (1) = None);
                  --  case 2 Direct_Left cannot be Nullable

                  Recursion := None;

                  Item_States (I) :=
                    (Label          => Keep_If_Minimal,
                     Item           => Item,
                     Minimal_Action => (Reduce, Nullable (Element (Item.Dot)), Token_Count => 0));

               elsif Dot_Index = 2 then

                  case RHS.Recursion (1) is
                  when None =>
                     --  case 3
                     Recursion := RHS.Recursion (2);

                     Item_States (I) :=
                       (Label          => Always_Keep,
                        Item           => Item,
                        Minimal_Action => (others => <>));

                  when Direct_Left =>
                     --  case 2a
                     Recursion       := Direct_Left;
                     Item_States (I) := (Label => Drop);

                  when Other_Left      =>
                     --  case 2b
                     Recursion       := Other_Left;
                     Item_States (I) :=
                       (Label          => Always_Keep,
                        Item           => Item,
                        Minimal_Action => (others => <>));

                  when Other =>
                     pragma Assert (RHS.Recursion (1) /= Other);
                     null;
                  end case;

               else
                  --  case 3
                  Recursion       := RHS.Recursion (Dot_Index);
                  Item_States (I) :=
                    (Label          => Keep_If_Minimal,
                     Item           => Item,
                     Minimal_Action => (others => <>));
               end if;

               State.Kernel (I) :=
                 (Production        => Item.Prod,
                  Before_Dot        => Before_Dot (Item),
                  Length_After_Dot  => Length_After_Dot,
                  Reduce_Production => Reduce_Production,
                  Reduce_Count      => Reduce_Count,
                  Recursion         => Recursion);

               if Item_States (I) = Keep_If_Minimal then
                  if Item.Length_After_Dot < Min_Length then
                     Min_Length := Item.Length_After_Dot;
                  end if;
               end if;

               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line ("kernel" & I'Image & " " & Strict_Image (State.Kernel (I)));
               end if;

               I := I + 1;
            end;
         end loop;

         --  Now we have the minimum length; check remaining items against that
         for Item_State of Item_States loop
            case Item_State.Label is
            when Unknown =>
               raise Programmer_Error with "Item_State.Label = Unknown";

            when Always_Keep =>
               if
            when Keep_If_Minimal =>
               if Min_Length < Length then
                  if Trace_Generate_Minimal_Complete > Extra then
                     Ada.Text_IO.Put_Line ("delete " & Length'Image & " >" & Min_Length'Image);
                  end if;
                  Delete;
               else
                  if Trace_Generate_Minimal_Complete > Extra then
                     Ada.Text_IO.Put_Line
                       ("keep " & Image (Constant_Ref (I).Prod) & Length'Image & " =" & Min_Length'Image);
                  end if;
                  Next (I);
                  Kernel_Index := Kernel_Index + 1;
               end if;

            when Direct_Left =>
               --  Direct_Left recursion is never minimal.
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line ("delete " & Image (Constant_Ref (I).Prod) & " direct left recursive");
               end if;
               Delete;

            when Other_Left | Other =>
               --   Keep all Other recursion, to be sorted out at runtime.
               if Trace_Generate_Minimal_Complete > Extra then
                  Ada.Text_IO.Put_Line ("keep other recursive");
               end if;
               Next (I);
               Kernel_Index := Kernel_Index + 1;
            end case;
         end loop;
      end;

      Delete_Non_Minimal;

      if Working_Set.Length > 0 then

         declare
         begin
            for Item of Working_Set loop

               if not Has_Element (Item.Dot) then
                  --  Item has no next terminal. Include a reduce action; the
                  --  Minimal_Terminal_First for the resulting state will be used.
                  Actions (I) :=
                    (Reduce, Item.Prod,
                     Token_Count => Grammar (Item.Prod.LHS).RHSs (Item.Prod.RHS).Tokens.Length);
               else
                  declare
                     ID : constant Token_ID := Element (Item.Dot);
                  begin
                     if ID in Terminals then
                        Actions (I) := Find_Action (State.Action_List, ID);

                     else
                        if Minimal_Terminal_First (ID) = Invalid_Token_ID then
                           --  Item.Dot is a nullable nonterm; include a reduce to the null
                           --  nonterm, rather than a shift of the following terminal; recover
                           --  must do the reduce first.
                           Actions (I) := (Reduce, (ID, Minimal_Terminal_Sequences (ID).Min_RHS), Token_Count => 0);

                        else
                           Actions (I) := Find_Action (State.Action_List, Minimal_Terminal_First (ID));
                        end if;
                     end if;
                  end;
               end if;
               I := I + 1;
            end loop;

            if Actions'Length = 1 then
               State.Minimal_Complete_Actions := Minimal_Action_Arrays.To_Vector (Actions (Actions'First));
            else
               --  Check for duplicates; see three_action_conflict_lalr.parse_table
               --  state 3 or lalr_generator_bug_01_lalr.parse_table state 28
               for I in Actions'Range loop
                  Skip := False;
                  for J in Actions'First .. I - 1 loop
                     if Actions (I) = Actions (J) then
                        Skip := True;
                        exit;
                     end if;
                  end loop;
                  if not Skip then
                     State.Minimal_Complete_Actions.Append (Actions (I));
                  end if;
               end loop;
            end if;

            if Trace_Generate_Minimal_Complete > Extra then
               Ada.Text_IO.Put_Line (Image (State.Minimal_Complete_Actions, Descriptor));
            end if;
         end;
      end if;
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
      use Ada.Text_IO;
      File : File_Type;
   begin
      --  Only space, semicolon, newline delimit object values. Bounds of
      --  arrays output before each array, unless known from discriminants.
      --  End of lists indicated by semicolon. Action, Check subprograms are
      --  represented by True if present, False if not.

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
                  Put (File, Node_J.Item.Verb'Image);
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
               Put (File, Token_ID'Image (Item.Production.LHS) & Item.Production.RHS'Image &
                      Count_Type'Image (Item.Length_After_Dot));
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
               Put (File, Action.Verb'Image);
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
      Put_Line ("Check_Limit       =>" & Token_Index'Image (Item.Check_Limit));
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
               Put (Image (Action.Production.LHS, Descriptor) & " " & Trimmed_Image (Action.Production));
            end case;
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
                  Put (Image (Action.Production.LHS, Descriptor) & " " & Trimmed_Image (Action.Production));
               end case;
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
      Conflicts             : in Conflict_Count_Lists.List;
      Descriptor            : in WisiToken.Descriptor;
      Include_Extra         : in Boolean := False)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
      Parse_Table_File : File_Type;
   begin
      Create (Parse_Table_File, Out_File, Parse_Table_File_Name);
      Set_Output (Parse_Table_File);
      Put_Line ("Tokens:");
      WisiToken.Put_Tokens (Descriptor);

      New_Line;
      Put_Line ("Productions:");
      for LHS in Grammar.First_Index .. Grammar.Last_Index loop
         declare
            Prod : WisiToken.Productions.Instance renames Grammar (LHS);
         begin
            for RHS in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
               Put (WisiToken.Productions.Image (Prod.LHS, RHS, Prod.RHSs (RHS).Tokens, Descriptor));
               if (for all Item of Grammar (LHS).RHSs (RHS).Recursion => Item = None) then
                  New_Line;
               else
                  Put_Line (" ; " & WisiToken.Productions.Image (Grammar (LHS).RHSs (RHS).Recursion));
               end if;
            end loop;
         end;
      end loop;

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
            Item_Index : Ada.Containers.Count_Type := 1;
         begin
            for Item of Kernels (State_Index).Set loop
               if In_Kernel (Grammar, Descriptor, Item) then
                  Put ("  " & Image (Grammar, Descriptor, Item, Show_Lookaheads => False));
                  if State_Index > 0 and then Table.States (State_Index).Kernel (Item_Index).Recursion /= None then
                     --  State.Kernel not set for state 0 (see Set_Minimal_Complete_Actions)
                     Put (" ; recursive");
                  end if;
                  Item_Index := Item_Index + 1;
                  New_Line;
               end if;
            end loop;
         end;
         New_Line;
         Put (Descriptor, Table.States (State_Index));

         if State_Index /= Table.States'Last then
            New_Line;
         end if;
      end loop;

      if Conflicts.Length > 0 then
         declare
            use Ada.Strings.Unbounded;
            Line          : Unbounded_String := +"States with conflicts:";
            Accept_Reduce : Integer          := 0;
            Shift_Reduce  : Integer          := 0;
            Reduce_Reduce : Integer          := 0;
         begin
            for Count of Conflicts loop
               Line          := Line & State_Index'Image (Count.State);
               Accept_Reduce := Accept_Reduce + Count.Accept_Reduce;
               Shift_Reduce  := Shift_Reduce + Count.Shift_Reduce;
               Reduce_Reduce := Reduce_Reduce + Count.Reduce_Reduce;
            end loop;

            New_Line;
            Indent_Wrap (-Line);

            New_Line;
            Put_Line
              (Integer'Image (Accept_Reduce) & " accept/reduce conflicts," &
                 Integer'Image (Shift_Reduce) & " shift/reduce conflicts," &
                 Integer'Image (Reduce_Reduce) & " reduce/reduce conflicts");
         end;
      else
         New_Line;
         Put_Line (" 0 accept/reduce conflicts, 0 shift/reduce conflicts, 0 reduce/reduce conflicts");
      end if;
      Set_Output (Standard_Output);
      Close (Parse_Table_File);
   end Put_Parse_Table;

end WisiToken.Generate.LR;
