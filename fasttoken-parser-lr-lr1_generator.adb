--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
package body FastToken.Parser.LR.LR1_Generator is

   function Find
     (Symbol      : in Token.Terminal_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr
   is
      use type Token.Terminal_ID;
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
      Lookahead            : in Token.Token_ID;
      Has_Empty_Production : in LR1_Items.Nonterminal_ID_Set)
     return Token.Token_ID
   is
      --  Return LHS of production that matches Action, Lookahead
      use Token.List;
      use type LR1_Items.Item_Set;
      use type Token.Token_ID;
      use type LR1_Items.Item_Ptr;
      use type LR1_Items.Item_Set_Ptr;

      Current : LR1_Items.Item_Set := Closure;
      Item    : LR1_Items.Item_Ptr;
   begin
      loop
         Item := Current.Set;
         loop
            exit when Item = null;
            case Action.Verb is
            when Shift =>
               if Item.Dot /= Null_Iterator and then
                 ID (Item.Dot) = Lookahead
               then
                  return Nonterminal.ID (Item.Prod.LHS);
               end if;
            when Reduce =>
               if Nonterminal.ID (Item.Prod.LHS) = Nonterminal.ID (Action.LHS) and
                 (Item.Dot = Null_Iterator or else
                    (Next_Token (Item.Dot) = Null_Iterator and
                       (ID (Item.Dot) in Nonterminal_ID and then
                          Has_Empty_Production (ID (Item.Dot)))))
               then
                  return Nonterminal.ID (Action.LHS);
               end if;
            when others =>
               raise Programmer_Error;
            end case;
            Item := Item.Next;
         end loop;
         exit when Current.Next = null;
         Current := Current.Next.all;
      end loop;

      Ada.Text_IO.Put_Line
        ("item for " & Parse_Action_Verbs'Image (Action.Verb) &
           (case Action.Verb is
            when Shift => State_Index'Image (Action.State),
            when Reduce => " " & Token.Token_Image (Nonterminal.ID (Action.LHS)),
            when others => "") & ", " &
           Token.Token_Image (Lookahead) & " not found in");
      LR1_Items.Put (Closure);
      raise Programmer_Error;
   end Find;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean
   is
      use type Token.Token_ID;
   begin
      --  ignore State_Index
      return
        Known.Action_A = Item.Action_A and
        Known.LHS_A = Item.LHS_A and
        Known.Action_B = Item.Action_B and
        Known.LHS_B = Item.LHS_B and
        Known.On = Item.On;
   end Match;

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

   procedure Add_Action
     (Symbol               : in     Token.Terminal_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LR1_Items.Item_Set;
      State_Index          : in     Unknown_State_Index;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean)
   is
      --  Add (Symbol, Action) to Action_List; check for conflicts
      --
      --  Closure .. Conflicts are for conflict reporting

      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Trace then
         Ada.Text_IO.Put (Token.Token_Image (Symbol) & " => ");
         Put (Action);
         Ada.Text_IO.New_Line;
      end if;

      if Matching_Action /= null then
         if Matching_Action.Action.Item = Action then
            --  Matching_Action is identical to Action, so there is no
            --  conflict; just don't add it again.
            if Trace then
               Ada.Text_IO.Put_Line (" - already present");
            end if;
            return;
         else
            --  There is a conflict. Report it and add it, so the
            --  generalized parser can follow both paths
            declare
               --  Enforce canonical Shift/Reduce order, to simplify
               --  searching and code generation.
               Action_A : constant Parse_Action_Rec :=
                 (if Action.Verb = Shift then Action else Matching_Action.Action.Item);

               Action_B : constant Parse_Action_Rec :=
                 (if Action.Verb = Shift then Matching_Action.Action.Item else Action);

               New_Conflict : constant Conflict :=
                 (Action_A    => Action_A.Verb,
                  Action_B    => Action_B.Verb,
                  LHS_A       => Find (Closure, Action_A, Symbol, Has_Empty_Production),
                  LHS_B       => Find (Closure, Action_B, Symbol, Has_Empty_Production),
                  State_Index => State_Index,
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

               if Action.Verb = Shift then
                  Matching_Action.Action := new Parse_Action_Node'(Action, Matching_Action.Action);
               else
                  Matching_Action.Action.Next := new Parse_Action_Node'(Action, null);
               end if;
            end;
         end if;
      else
         Action_List := new Action_Node'
           (Symbol => Symbol,
            Action => new Parse_Action_Node'(Action, null),
            Next   => Action_List);
      end if;
   end Add_Action;

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item_Ptr;
      Item_Set             : in     LR1_Items.Item_Set_Ptr;
      Action_List          : in out Action_Node_Ptr;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean)
   is
      --  Add actions for Item.Lookaheads to Action_List
      --  Item must be from Item_Set.
      --  Has_Empty_Production used for conflict reporting.

      Action : constant Parse_Action_Rec :=
        (Reduce, Item.Prod.LHS, Item.Prod.RHS.Action, Item.Prod.RHS.Index, Item.Prod.RHS.Tokens.Length);
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      --  We ignore propagate lookaheads here.
      for Lookahead in Item.Lookaheads.Tokens'Range loop
         if Item.Lookaheads.Tokens (Lookahead) then
            Add_Action
              (Symbol               => Lookahead,
               Action               => Action,
               Action_List          => Action_List,
               Closure              => Item_Set.all,
               State_Index          => Item_Set.State,
               Has_Empty_Production => Has_Empty_Production,
               Conflicts            => Conflicts,
               Trace                => Trace);
         end if;
      end loop;
   end Add_Lookahead_Actions;

   procedure Add_Actions
     (Item_Set             : in     LR1_Items.Item_Set_Ptr;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      --  Add actions for Item_Set to Table
      use type LR1_Items.Item_Ptr;
      use type Token.List.List_Iterator;

      State : constant State_Index := Item_Set.State;
      Item  : LR1_Items.Item_Ptr   := Item_Set.Set;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("adding actions for state" & State_Index'Image (Item_Set.State));
         LR1_Items.Put (Item_Set.Goto_List);
      end if;

      while Item /= null loop
         if Item.Dot = Token.List.Null_Iterator then
            --  Pointer is at the end of the production; add a reduce action.
            Add_Lookahead_Actions
              (Item, Item_Set, Table (State).Action_List, Has_Empty_Production, Conflicts, Trace);

         elsif Token.List.ID (Item.Dot) in Token.Terminal_ID then
            --  Dot is before a terminal token.
            declare
               use type Token.Token_ID;
               use type LR1_Items.Item_Set_Ptr;

               Dot_ID : constant Token.Terminal_ID := Token.List.ID (Item.Dot);
               --  ID of token after Item.Dot

               Goto_Set : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (Item_Set.all, Dot_ID);
            begin
               if Dot_ID = EOF_Token then
                  --  This is the start symbol production with dot before EOF.
                  Add_Action
                    (Symbol               => Dot_ID,
                     Action               =>
                       (Accept_It, Item.Prod.LHS, Item.Prod.RHS.Action, Item.Prod.RHS.Index,
                        Item.Prod.RHS.Tokens.Length - 1), -- EOF is not pushed on stack
                     Action_List          => Table (State).Action_List,
                     State_Index          => Item_Set.State,
                     Closure              => Item_Set.all,
                     Has_Empty_Production => Has_Empty_Production,
                     Conflicts            => Conflicts,
                     Trace                => Trace);
               else
                  if Goto_Set /= null then
                     Add_Action
                       (Symbol               => Dot_ID,
                        Action               =>
                          (Verb              => Shift,
                           State             => Goto_Set.State),
                        Action_List          => Table (State).Action_List,
                        State_Index          => Item_Set.State,
                        Closure              => Item_Set.all,
                        Has_Empty_Production => Has_Empty_Production,
                        Conflicts            => Conflicts,
                        Trace                => Trace);
                  end if;
               end if;
            end;
         else
            --  Dot is before a non-terminal token; no action.
            if Trace then
               Ada.Text_IO.Put_Line (Token.Token_Image (Token.List.ID (Item.Dot)) & " => no action");
            end if;
         end if;

         Item := Item.Next;
      end loop;

      --  Place a default error action at the end of every state.
      --  (it should always have at least one action already).
      declare
         --  The default action, when nothing else matches an input
         Default_Action : constant Action_Node :=
           --  The symbol here is actually irrelevant; it is the
           --  position as the last on a state's action list that makes
           --  it the default. It's too bad we can't extend an
           --  enumeration type to make this 'default', for viewing this
           --  list in a debugger. The various Put routines do replace
           --  this with 'default'.
           (Symbol => Token.Terminal_ID'Last,
            Action => new Parse_Action_Node'(Parse_Action_Rec'(Verb => Error), null),
            Next   => null);

         Last_Action : Action_Node_Ptr := Table (State).Action_List;
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
            raise Programmer_Error with
              "Generating parser: state" & State_Index'Image (Item_Set.State) &
              " has no actions; bad grammar, or " &
              "first production in grammar must be the only start symbol production, " &
              "and it must must have an explicit EOF.";
         else
            while Last_Action.Next /= null loop
               Last_Action := Last_Action.Next;
            end loop;
            Last_Action.Next := new Action_Node'(Default_Action);
         end if;
      end;

      --  Fill in this state's Goto transitions
      declare
         use type LR1_Items.Goto_Item_Ptr;
         Goto_Ptr : LR1_Items.Goto_Item_Ptr := Item_Set.Goto_List;
      begin
         while Goto_Ptr /= null loop
            if Goto_Ptr.Symbol in Nonterminal_ID then
               Table (State).Goto_List := new Goto_Node'
                 (Symbol => Goto_Ptr.Symbol,
                  State  => Goto_Ptr.Set.State,
                  Next   => Table (State).Goto_List);
            end if;
            Goto_Ptr := Goto_Ptr.Next;
         end loop;
      end;
   end Add_Actions;

   procedure Add_Actions
     (Item_Sets            : in     LR1_Items.Item_Set_List;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      --  Add actions for all Item_Sets to Table.

      Item_Set : LR1_Items.Item_Set_Ptr := Item_Sets.Head;
      use type LR1_Items.Item_Set_Ptr;
   begin
      while Item_Set /= null loop
         Add_Actions (Item_Set, Has_Empty_Production, Conflicts, Table, Trace);
         Item_Set := Item_Set.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   procedure Put_Parse_Table
     (Table     : in Parse_Table_Ptr;
      Item_Sets : in LR1_Items.Item_Set_List)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("LR1 Parse Table:");
      for State in Table'Range loop
         LR1_Items.Put (LR1_Items.Find (State, Item_Sets).all, Kernel_Only => True, Show_Lookaheads => True);
         New_Line;
         Put (Table (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   function Check_Unused_Tokens (Grammar : in Production.List.Instance) return Boolean
   is
      use Production.List;

      Used_Tokens : Token.Token_Array_Boolean := (others => False);

      Unused_Tokens : Boolean := False;

      I : List_Iterator := First (Grammar);
   begin
      loop
         exit when Is_Done (I);
         declare
            use Production;
            use Token.List;
            Prod : constant Production.Instance := Current (I);
            J    : Token.List.List_Iterator     := First (Prod.RHS.Tokens);
         begin
            Used_Tokens (Nonterminal.ID (Prod.LHS)) := True;

            loop
               exit when Is_Done (J);
               Used_Tokens (ID (J)) := True;
               Next (J);
            end loop;
         end;

         Next (I);
      end loop;

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Token.Token_Image (I));
         end if;
      end loop;

      return Unused_Tokens;
   end Check_Unused_Tokens;

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
      --  WORKAROUND: GNAT GPL 2012 doesn't like an explicit exit in an 'of' loop
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

   function Generate
     (Grammar                  : in Production.List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
      use type Ada.Containers.Count_Type;

      Unused_Tokens : constant Boolean := Check_Unused_Tokens (Grammar);

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set := LR1_Items.Has_Empty_Production (Grammar);
      First                : constant LR1_Items.Derivation_Matrix  := LR1_Items.First
        (Grammar, Has_Empty_Production, Trace);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Items.LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, EOF_Token, Unknown_State_Index (First_State_Index), Trace);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;
   begin
      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Item_Sets:");
         LR1_Items.Put (Item_Sets);
      end if;

      Table := new Parse_Table (State_Index'First .. Item_Sets.Size - 1 + State_Index'First);

      Add_Actions (Item_Sets, Has_Empty_Production, Unknown_Conflicts, Table.all, Trace);

      if Put_Parse_Table then
         LR1_Generator.Put_Parse_Table (Table, Item_Sets);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line ("unknown conflicts:");
         Put (Unknown_Conflicts);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "unknown conflicts; aborting";
         end if;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line ("excess known conflicts:");
         Put (Known_Conflicts_Edit);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "excess known conflicts; aborting";
         end if;
      end if;

      if Unused_Tokens and not (Trace or Ignore_Unused_Tokens) then
         raise Grammar_Error with "unused tokens; aborting";
      end if;

      return Table;
   end Generate;

end FastToken.Parser.LR.LR1_Generator;
