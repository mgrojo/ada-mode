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

with Ada.Text_IO;
package body WisiToken.LR.Generator_Utils is

   procedure Add_Action
     (Symbol               : in     Token_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LR1_Items.Item_Set;
      Grammar              : in     WisiToken.Productions.Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean;
      Descriptor           : in     WisiToken.Descriptor'Class)
   is
      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Trace then
         Ada.Text_IO.Put (Image (Symbol, Descriptor) & " => ");
         Put (Descriptor, Action);
         Ada.Text_IO.New_Line;
      end if;

      if Matching_Action /= null then
         if Equal (Matching_Action.Action.Item, Action) then
            --  Matching_Action is identical to Action, so there is no
            --  conflict; just don't add it again.
            Matching_Action.Action.Item.Productions.Append (Action.Productions);

            if Trace then
               Ada.Text_IO.Put_Line (" - already present");
            end if;
            return;
         else
            --  There is a conflict. Report it and add it, so the
            --  generalized parser can follow both paths
            declare
               --  Enforce canonical Shift/Reduce or Accept/Reduce
               --  order, to simplify searching and code generation.
               Action_A : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Action else Matching_Action.Action.Item);

               Action_B : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Matching_Action.Action.Item else Action);

               New_Conflict : constant Conflict :=
                 (Action_A    => Action_A.Verb,
                  Action_B    => Action_B.Verb,
                  LHS_A       => Find (Closure, Action_A, Symbol, Grammar, Has_Empty_Production, First, Descriptor),
                  LHS_B       => Find (Closure, Action_B, Symbol, Grammar, Has_Empty_Production, First, Descriptor),
                  State_Index => Closure.State,
                  On          => Symbol);
            begin
               if not Is_Present (New_Conflict, Conflicts) then
                  --  The same conflict may occur in a different
                  --  item set. Only add it to conflicts once.
                  Conflicts.Append (New_Conflict);

                  if Trace then
                     Ada.Text_IO.Put_Line (" - conflict added");
                  end if;
               else
                  if Trace then
                     Ada.Text_IO.Put_Line (" - conflict duplicate");
                  end if;
               end if;

               --  More than two actions can occur; see triple_conflict.wy. We make
               --  that an error, since the grammar will be better off without them.
               --  But keep going; the full parse table output will be needed to fix
               --  the excess conflict.
               if Matching_Action.Action.Next /= null then
                  if Matching_Action.Action.Item = Action or Matching_Action.Action.Next.Item = Action then
                     if Trace then
                        Ada.Text_IO.Put_Line (" - conflict duplicate");
                     end if;
                  else
                     Put_Error
                       ("More than two actions on " & Image (Symbol, Descriptor) &
                          " in state" & State_Index'Image (Closure.State));
                     Error := True;
                  end if;
               end if;

               if Action.Verb = Shift then
                  Matching_Action.Action := new Parse_Action_Node'(Action, Matching_Action.Action);
               else
                  Matching_Action.Action.Next := new Parse_Action_Node'(Action, Matching_Action.Action.Next);
               end if;
            end;
         end if;
      else
         Add (Action_List, Symbol, Action);
      end if;
   end Add_Action;

   procedure Add_Actions
     (Closure              : in     LR1_Items.Item_Set;
      Table                : in out Parse_Table;
      Grammar              : in     WisiToken.Productions.Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean;
      Descriptor           : in     WisiToken.Descriptor'Class)
   is
      use WisiToken.Productions.Token_ID_Lists;
      use all type LR1_Items.Item_Ptr;

      State : constant State_Index := Closure.State;
      Item  : LR1_Items.Item_Ptr   := Closure.Set;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("adding actions for state" & State_Index'Image (State));
         LR1_Items.Put (Descriptor, Closure.Goto_List);
      end if;

      while Item /= null loop
         if Dot (Item) = No_Element then
            --  Pointer is at the end of the production; add a reduce action.

            Add_Lookahead_Actions
              (Item, Table.States (State).Action_List,
               Grammar, Has_Empty_Production, First, Conflicts, Closure, Trace, Descriptor);

         elsif Element (Dot (Item)) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
            --  Dot is before a terminal token.
            declare
               use Production_ID_Arrays;
               use all type Ada.Containers.Count_Type;
               use all type LR1_Items.Item_Set_Ptr;

               Dot_ID : constant Token_ID := Element (Dot (Item));
               --  ID of token after Item.Dot

               Goto_Set : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (Closure, Dot_ID);
            begin
               if Dot_ID = Descriptor.EOF_ID then
                  --  This is the start symbol production with dot before EOF. Note that
                  --  semantic_check is null; we only support semantic checks in Wisi
                  --  source files, not grammars defined in Ada code.
                  declare
                     Prod : Productions.Instance renames Grammar (Prod_ID (Item));
                  begin
                     Add_Action
                       (Dot_ID,
                        (Accept_It, +Prod_ID (Item),
                         Prod.LHS, Prod.RHS.Action, null,
                         Prod.RHS.Tokens.Length - 1, -- EOF is not pushed on stack
                         Prod.RHS.Name_Index),
                        Table.States (State).Action_List,
                        Closure, Grammar, Has_Empty_Production, First, Conflicts, Trace, Descriptor);
                  end;
               else
                  if Goto_Set /= null then
                     Add_Action
                       (Dot_ID,
                        (Verb        => Shift,
                         Productions => +Prod_ID (Item),
                         State       => Goto_Set.State),
                        Table.States (State).Action_List,
                        Closure, Grammar, Has_Empty_Production, First, Conflicts, Trace, Descriptor);
                  end if;
               end if;
            end;
         else
            --  Dot is before a non-terminal token; no action.
            if Trace then
               Ada.Text_IO.Put_Line (Image (Element (Dot (Item)), Descriptor) & " => no action");
            end if;
         end if;

         Item := Next (Item);
      end loop;

      --  Place a default error action at the end of every state.
      --  (it should always have at least one action already).
      declare
         --  The default action, when nothing else matches an input
         Default_Action : constant Action_Node :=
           --  The symbol here is actually irrelevant; it is the
           --  position as the last on a state's action list that makes
           --  it the default. The various Put routines replace
           --  this with 'default'.
           (Symbol => Invalid_Token_ID,
            Action => new Parse_Action_Node'
              (Parse_Action_Rec'(Verb => LR.Error, Productions => Production_ID_Arrays.Empty_Vector), null),
            Next   => null);

         Last_Action : Action_Node_Ptr := Table.States (State).Action_List;
      begin
         if Last_Action = null then
            --  This happens if the first production in the grammar is
            --  not the start symbol production.
            --
            --  It also happens when the start symbol production does
            --  not have an explicit EOF, or when there is more than
            --  one production that has the start symbol on the left
            --  hand side.
            --
            --  It also happens when the grammar is bad, for example:
            --
            --  declarations <= declarations & declaration
            --
            --  without 'declarations <= declaration'.
            --
            --  We continue generating the grammar, in order to help the user
            --  debug this issue.
            Error := True;

            Ada.Text_IO.Put_Line
              ("Error: Generating parser: state" & State_Index'Image (State) &
                 " has no actions; bad grammar, or " &
                 "first production in grammar must be the only start symbol production, " &
                 "and it must must have an explicit EOF.");
            Ada.Text_IO.New_Line;
         else
            while Last_Action.Next /= null loop
               Last_Action := Last_Action.Next;
            end loop;
            Last_Action.Next := new Action_Node'(Default_Action);
         end if;
      end;

      --  Fill in this state's Goto transitions
      declare
         use all type LR1_Items.Goto_Item_Ptr;
         Goto_Ptr : LR1_Items.Goto_Item_Ptr := Closure.Goto_List;
      begin
         while Goto_Ptr /= null loop
            if Symbol (Goto_Ptr) in Descriptor.Last_Terminal + 1 .. Descriptor.Last_Nonterminal then
               Add_Goto (Table.States (State), Prod_ID (Goto_Ptr), Symbol (Goto_Ptr), LR1_Items.State (Goto_Ptr));
            end if;
            Goto_Ptr := Next (Goto_Ptr);
         end loop;
      end;
   end Add_Actions;

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item_Ptr;
      Action_List          : in out Action_Node_Ptr;
      Grammar              : in     WisiToken.Productions.Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Trace                : in     Boolean;
      Descriptor           : in     WisiToken.Descriptor'Class)
   is
      use Production_ID_Arrays;
      use all type LR1_Items.Item_Ptr;

      Prod   : Productions.Instance renames Grammar (Prod_ID (Item));
      Action : constant Parse_Action_Rec :=
        (Reduce, +Prod_ID (Item), Prod.LHS, Prod.RHS.Action, Prod.RHS.Check, Prod.RHS.Tokens.Length,
         Prod.RHS.Name_Index);
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      --  We ignore propagate lookaheads here.
      for Lookahead in Lookaheads (Item)'Range loop
         if Lookaheads (Item) (Lookahead) then
            if Descriptor in LALR_Descriptor and then
              Lookahead = LALR_Descriptor (Descriptor).Propagate_ID
            then
               null;
            else
               Add_Action
                 (Lookahead, Action, Action_List, Closure, Grammar,
                  Has_Empty_Production, First, Conflicts, Trace, Descriptor);
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
     (Symbol      : in Token_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr
   is
      Action_Node : Action_Node_Ptr := Action_List;
   begin
      while Action_Node /= null loop
         if Action_Node.Symbol = Symbol then
            return Action_Node;
         end if;
         Action_Node := Action_Node.Next;
      end loop;

      return null;
   end Find;

   function Find
     (Closure              : in LR1_Items.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token_ID;
      Grammar              : in WisiToken.Productions.Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Descriptor           : in WisiToken.Descriptor'Class)
     return Token_ID
   is
      use WisiToken.Productions.Token_ID_Lists;
      use all type LR1_Items.Item_Ptr;
      use all type LR1_Items.Item_Set_Ptr;

      Current : LR1_Items.Item_Set := Closure;
      Item    : LR1_Items.Item_Ptr;
      ID_I    : Cursor;
   begin
      case Action.Verb is
      when Reduce | Accept_It =>
         --  If the nonterm produced by the reduce is the LHS of the state
         --  production, use it.
         loop
            Item := Current.Set;
            loop
               exit when Item = null;
               if In_Kernel (Grammar, Descriptor, Item) and
                 Action.LHS = Grammar (Prod_ID (Item)).LHS
               then
                  return Action.LHS;
               end if;
               Item := Next (Item);
            end loop;
            exit when Current.Next = null;
            Current := Current.Next.all;
         end loop;

         --  The reduce nonterm is after Dot in a state production; find which
         --  one, use that.
         Current := Closure;
         loop
            Item := Current.Set;
            loop
               exit when Item = null;
               if In_Kernel (Grammar, Descriptor, Item) then
                  ID_I := Dot (Item);
                  loop
                     if ID_I = No_Element then
                        if Lookaheads (Item) (Lookahead) then
                           return Grammar (Prod_ID (Item)).LHS;
                        end if;
                     else
                        declare
                           Dot_ID : Token_ID renames Element (ID_I);
                        begin
                           if Dot_ID = Lookahead or
                             (Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                                First (Dot_ID, Lookahead))
                           then
                              return Grammar (Prod_ID (Item)).LHS;
                           end if;
                           exit when Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                             not Has_Empty_Production (Dot_ID);
                        end;
                     end if;

                     exit when ID_I = No_Element;
                     Next (ID_I);
                  end loop;
               end if;
               Item := Next (Item);
            end loop;
            exit when Current.Next = null;
            Current := Current.Next.all;
         end loop;

      when Shift =>
         loop
            Item := Current.Set;
            loop
               exit when Item = null;
               if In_Kernel (Grammar, Descriptor, Item) and then
                 Prod_ID (Item) = Action.Productions (1)
               then
                  --  The action production is one of the state productions, use that
                  --  LHS.
                  return Grammar (Prod_ID (Item)).LHS;
               end if;
               Item := Next (Item);
            end loop;
            exit when Current.Next = null;
            Current := Current.Next.all;
         end loop;

         Current := Closure;
         loop
            Item := Current.Set;
            loop
               exit when Item = null;
               --  Lookahead (the token shifted) is starting a nonterm in a state
               --  production; it is in First of that nonterm.
               if In_Kernel (Grammar, Descriptor, Item) then
                  ID_I := Dot (Item);
                  loop
                     exit when ID_I = No_Element;
                     declare
                        Dot_ID : Token_ID renames Element (ID_I);
                     begin
                        if Dot_ID = Lookahead or
                          (Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                             First (Dot_ID, Lookahead))
                        then
                           return Grammar (Prod_ID (Item)).LHS;
                        end if;

                        exit when Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
                          not Has_Empty_Production (Dot_ID);
                     end;

                     Next (ID_I);
                  end loop;
               end if;
               Item := Next (Item);
            end loop;
            exit when Current.Next = null;
            Current := Current.Next.all;
         end loop;

      when LR.Error =>
         raise Programmer_Error;
      end case;

      Ada.Text_IO.Put_Line
        ("item for " & Image (Action, Descriptor) & " on " & Image (Lookahead, Descriptor) & " not found in");
      LR1_Items.Put (Grammar, Descriptor, Closure, Kernel_Only => True);
      raise Programmer_Error;
   end Find;

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Conflict) return String
   is begin
      return
        ("%conflict " &
           Conflict_Parse_Actions'Image (Item.Action_A) & "/" &
           Conflict_Parse_Actions'Image (Item.Action_B) & " in state " &
           Image (Item.LHS_A, Descriptor) & ", " &
           Image (Item.LHS_B, Descriptor) &
           "  on token " & Image (Item.On, Descriptor) &
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

   procedure Put
     (Item       : in Conflict_Lists.List;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor'Class)
   is begin
      for Conflict of Item loop
         Ada.Text_IO.Put_Line (File, Image (Descriptor, Conflict));
      end loop;
   end Put;

   procedure Terminal_Sequence
     (Grammar       : in     WisiToken.Productions.Arrays.Vector;
      Descriptor    : in     WisiToken.Descriptor'Class;
      All_Sequences : in out Token_Sequence_Arrays.Vector;
      All_Set       : in out Token_ID_Set;
      Recursing     : in out Token_ID_Set;
      Nonterm       : in     Token_ID)
   is
      use WisiToken.Productions.Arrays;
      use Ada.Containers;
      Prods : constant Productions.Production_ID_Range := WisiToken.Productions.Find (Grammar, Nonterm);

      Temp              : Token_Sequence_Arrays.Vector;
      Min_Length        : Count_Type := Count_Type'Last;
      Skipped_Recursive : Boolean    := False;
   begin
      --  We get here because All_Sequences (Nonterm) has not been comptued
      --  yet. Attempt to compute All_Sequences (Nonterm); if succesful, set
      --  All_Set (Nonterm) True.

      --  First fill Temp with terminals from each production for Nonterm.
      for L in Prods.First .. Prods.Last loop

         if Grammar (L).RHS.Tokens.Length = 0 then
            All_Set (Nonterm) := True;

            if Trace_Generate > Detail then
               Ada.Text_IO.Put_Line (Image (Nonterm, Descriptor) & " => ()");
            end if;

            return;
         end if;

         if WisiToken.Productions.Token_ID_Lists.Element (Grammar (L).RHS.Tokens.First) = Nonterm then
            --  The first RHS token = LHS; a recursive list. This will never be
            --  the shortest production, so just skip it.
            null;

         else
            declare
               Sequence : Token_ID_Arrays.Vector;
            begin
               for ID of Grammar (L).RHS.Tokens loop
                  if ID in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                     Sequence.Append (ID);

                  else
                     if not All_Set (ID) then
                        if Recursing (ID) then
                           --  This nonterm is mutually recursive with some other. This
                           --  production will never be the shortest unless it's the only one,
                           --  so skip it.
                           if Trace_Generate > Detail then
                              Ada.Text_IO.Put_Line (Image (ID, Descriptor) & " mutual recurse skipped");
                           end if;
                           Skipped_Recursive := True;
                           goto Skip;
                        else
                           Recursing (ID) := True;
                           if Trace_Generate > Detail then
                              Ada.Text_IO.Put_Line (Image (ID, Descriptor) & " recurse");
                           end if;
                           Terminal_Sequence (Grammar, Descriptor, All_Sequences, All_Set, Recursing, ID);
                           Recursing (ID) := False;

                           if not All_Set (ID) then
                              --  abandoned because of recursion
                              Skipped_Recursive := True;
                              goto Skip;
                           end if;
                        end if;
                     end if;
                     Sequence.Append (All_Sequences (ID));
                  end if;
               end loop;

               if Trace_Generate > Detail then
                  Ada.Text_IO.Put_Line (Image (Nonterm, Descriptor) & " -> " & Image (Sequence, Descriptor));
               end if;
               Temp.Append (Sequence);
            end;
         end if;

         <<Skip>>
         null;
      end loop;

      --  Now find the minimum length.
      if Temp.Length = 0 and Skipped_Recursive then
         --  better luck next time.
         return;
      end if;

      for S of Temp loop
         if S.Length <= Min_Length then
            Min_Length := S.Length;

            All_Sequences (Nonterm) := S;
         end if;
      end loop;

      if Trace_Generate > Detail then
         Ada.Text_IO.Put_Line (Image (Nonterm, Descriptor) & " ==> " & Image (All_Sequences (Nonterm), Descriptor));
      end if;

      All_Set (Nonterm) := True;
   end Terminal_Sequence;

   procedure Compute_Minimal_Terminal_Sequences
     (Grammar    : in     WisiToken.Productions.Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor'Class;
      Result     : in out Token_Sequence_Arrays.Vector)
   is
      --  Result (ID).Length = 0 is a valid sequence (ie the nonterminal can
      --  be empty), so we use an auxilliary array to track whether Result
      --  (ID) has been computed.
      --
      --  We also need to detect mutual recursion, and incomplete grammars.

      All_Set   : Token_ID_Set := (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => False);
      Recursing : Token_ID_Set := (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => False);

      Last_Count : Integer := 0;
      This_Count : Integer;
   begin
      Result.Set_First (Descriptor.First_Nonterminal);
      Result.Set_Last (Descriptor.Last_Nonterminal);

      loop
         exit when (for all B of All_Set => B);
         for P of Grammar loop
            if not All_Set (P.LHS) then
               Terminal_Sequence (Grammar, Descriptor, Result, All_Set, Recursing, P.LHS);
            end if;
         end loop;
         This_Count := Count (All_Set);
         if This_Count = Last_Count then
            raise Grammar_Error with "nonterminals have no minimum terminal sequence: " &
              Image (All_Set, Descriptor, Inverted => True);
         end if;
         Last_Count := This_Count;
      end loop;
   end Compute_Minimal_Terminal_Sequences;

end WisiToken.LR.Generator_Utils;
