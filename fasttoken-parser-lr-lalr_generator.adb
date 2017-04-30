--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body FastToken.Parser.LR.LALR_Generator is

   --  The following types are used for computing lookahead
   --  propagations.

   type Item_List;
   type Item_List_Ptr is access Item_List;
   type Item_List is record
      Item : LR1_Items.Item_Ptr;
      Next : Item_List_Ptr;
   end record;

   type Item_Item_List_Mapping;
   type Item_Item_List_Mapping_Ptr is access Item_Item_List_Mapping;

   type Item_Item_List_Mapping is record
      From : LR1_Items.Item_Ptr;
      To   : Item_List_Ptr;
      Next : Item_Item_List_Mapping_Ptr;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);

   procedure Free (List : in out Item_List_Ptr)
   is
      Old_Item : Item_List_Ptr := List;
   begin
      while Old_Item /= null loop
         List := Old_Item.Next;
         Dispose (Old_Item);
         Old_Item := List;
      end loop;
   end Free;

   procedure Free (List : in out Item_Item_List_Mapping_Ptr)
   is
      Old_Mapping : Item_Item_List_Mapping_Ptr := List;
   begin
      while Old_Mapping /= null loop
         List := Old_Mapping.Next;
         Free (Old_Mapping.To);
         Dispose (Old_Mapping);
         Old_Mapping := List;
      end loop;
   end Free;

   ----------
   --  Debug output

   procedure Print_Propagations (Propagations : Item_Item_List_Mapping_Ptr) is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LR1_Items.Put (Next_Prop.From.all, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("To   ");
            LR1_Items.Put (Next_To.Item.all, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Print_Propagations;

   procedure Put_Parse_Table
     (Table   : in Parse_Table_Ptr;
      Kernels : in LR1_Items.Item_Set_List)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("LALR Parse Table:");
      for State in Table'Range loop
         LR1_Items.Put (LR1_Items.Find (State, Kernels).all, Show_Lookaheads => True);
         New_Line;
         Put (Table (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   ----------
   --  Generator utils

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

   --  Add propagation entries (if they don't already exist) from From
   --  to all kernel items that match To.
   procedure Add_Propagations
     (From         : in     LR1_Items.Item_Ptr;
      From_Set     : in     LR1_Items.Item_Set;
      To           : in     LR1_Items.Item_Node;
      For_Token    : in     Token.Token_ID;
      Propagations : in out Item_Item_List_Mapping_Ptr)
   is
      use type Production.Instance;
      use type Token.List.List_Iterator;
      use type LR1_Items.Item_Set_Ptr;
      use type LR1_Items.Item_Ptr;

      Goto_Set  : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (From_Set, For_Token);
      To_Kernel : constant LR1_Items.Item_Ptr     :=
        (if Goto_Set = null then null
         else LR1_Items.Find (To, Goto_Set.all, Match_Lookaheads => False));

      Prop_Match    : Item_Item_List_Mapping_Ptr := Propagations;
      Prop_To_Match : Item_List_Ptr;
      Found_From    : Boolean                    := False;
      Found_To      : Boolean                    := False;
   begin
      if To_Kernel = null then
         return;
      end if;

      Find_Matching_Prop :
      while Prop_Match /= null loop
         if Prop_Match.From = From then

            Found_From    := True;
            Prop_To_Match := Prop_Match.To;
            while Prop_To_Match /= null loop

               --  ignore lookaheads in this match
               if Prop_To_Match.Item.Prod = To_Kernel.Prod and
                 Prop_To_Match.Item.Dot = To_Kernel.Dot
               then
                  Found_To := True;
                  exit Find_Matching_Prop;
               end if;
               Prop_To_Match := Prop_To_Match.Next;
            end loop;
            exit Find_Matching_Prop;
         end if;

         Prop_Match := Prop_Match.Next;
      end loop Find_Matching_Prop;

      if not Found_From then
         --  propagation for a new from_kernel
         Propagations := new Item_Item_List_Mapping'
           (From, new Item_List'(To_Kernel, Next => null), Next => Propagations);

      elsif not Found_To then
         --  add to propagations for an existing from_kernel
         Prop_Match.To := new Item_List'(To_Kernel, Next => Prop_Match.To);

      else
         raise Programmer_Error with "Add_Propagations: unexpected case";
      end if;
   end Add_Propagations;

   --  Calculate the lookaheads from Closure_Item for Source_Item.
   --  Source_Item must be one of the kernel items in Source_Set.
   --  Closure_Item must be an item in the lookahead closure of Source_Item for #.
   --
   --  Spontaneous lookaheads are put in Source_Item.Lookahead,
   --  propagated lookaheads in Propagations.
   --
   --  The start symbol (with Source_Set.State = Accept_State) is treated specially.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LR1_Items.Item_Ptr;
      Source_Set   : in     LR1_Items.Item_Set;
      Closure_Item : in     LR1_Items.Item_Node;
      Accept_State : in     State_Index;
      Propagations : in out Item_Item_List_Mapping_Ptr;
      Used_Tokens  : in out Token.Token_Array_Boolean;
      Trace        : in     Boolean)
   is
      Spontaneous_Count : Integer := 0;

      use type LR1_Items.Lookahead_Ptr;
      use type LR1_Items.Item_Ptr;
      use type LR1_Items.Item_Set_Ptr;
      use type Token.List.List_Iterator;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LR1_Items.Put (Closure_Item, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;
      end if;

      --  If this is the start symbol production, it gets a lookahead
      --  for each terminal, so it will reduce on anything. FIXME: doesn't work, not needed?
      if Source_Set.State = Accept_State then
         for Token_ID in Token.Terminal_ID loop
            --  These tokens are not actually used in the grammar, so
            --  we don't set Used_Tokens here.
            declare
               Lookahead : constant Token_Pkg.Token_ID := Token_ID;
            begin
               if Trace then
                  Ada.Text_IO.Put ("  default:");
                  LR1_Items.Put (Source_Item.all, Show_Lookaheads => False);
                  Ada.Text_IO.Put_Line ("; " & Token.Token_Image (Lookahead));
               end if;

               LR1_Items.Include (Source_Item.Lookaheads, Lookahead);
            end;
         end loop;
      end if;

      if Closure_Item.Dot = Token.List.Null_Iterator then
         return;
      end if;

      declare
         Token_ID : constant Token.Token_ID           := Token.List.ID (Closure_Item.Dot);
         Next_Dot : constant Token.List.List_Iterator := Token.List.Next_Token (Closure_Item.Dot);

         Next_Item : constant LR1_Items.Item_Node :=
           (Prod       => Closure_Item.Prod,
            Dot        => Next_Dot,
            State      => Unknown_State,
            Lookaheads => null,
            Next       => null);

         Goto_Set : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (Source_Set, Token_ID);

         Next_Kernel : constant LR1_Items.Item_Ptr :=
           (if Goto_Set = null then null
            else LR1_Items.Find (Next_Item, Goto_Set.all, Match_Lookaheads => False));

         Lookahead : LR1_Items.Lookahead_Ptr := Closure_Item.Lookaheads;
      begin
         begin
            Used_Tokens (Token_ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Token.Token_Image (Token_ID) & " used in grammar";
         end;

         while Lookahead /= null loop
            if Lookahead.Propagate then
               Add_Propagations
                 (From         => Source_Item,
                  From_Set     => Source_Set,
                  To           => Next_Item,
                  For_Token    => Token_ID,
                  Propagations => Propagations);
            else
               if Next_Kernel /= null then
                  if Trace then
                     Spontaneous_Count := Spontaneous_Count + 1;
                     Ada.Text_IO.Put_Line ("  spontaneous: " & LR1_Items.Print (Lookahead.all));
                  end if;

                  LR1_Items.Include (Next_Kernel.Lookaheads, Lookahead);
               end if;

            end if;
            Lookahead := Lookahead.Next;
         end loop;

         if Spontaneous_Count > 0 then
            Ada.Text_IO.Put ("  Next_Kernel (" & Token.Token_Image (Token_ID) & "): ");
            LR1_Items.Put (Next_Kernel.all, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List  : in Item_Item_List_Mapping_Ptr;
      Trace : in Boolean)
   is
      use type LR1_Items.Lookahead_Ptr;

      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LR1_Items.Lookahead_Ptr;
      Added_One     : Boolean;
      Added_Some    : Boolean := False;
   begin
      while More_To_Check loop

         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookaheads;

            while Lookahead /= null loop
               if not Lookahead.Propagate then
                  To := Mapping.To;
                  while To /= null loop
                     LR1_Items.Include (To.Item.Lookaheads, Lookahead.Lookahead, Added_One);

                     if Trace and Added_One then
                        Added_Some := True;
                        Ada.Text_IO.Put ("  to:");
                        LR1_Items.Put (To.Item.all, Show_Lookaheads => True);
                        Ada.Text_IO.New_Line;
                     end if;

                     More_To_Check := More_To_Check or Added_One;
                     To := To.Next;
                  end loop;
               end if;
               Lookahead := Lookahead.Next;
            end loop;

            if Trace and Added_Some then
               Added_Some := False;
               Ada.Text_IO.Put ("from: ");
               LR1_Items.Put (Mapping.From.all, Show_Lookaheads => True);
               Ada.Text_IO.New_Line;
            end if;

            Mapping := Mapping.Next;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LALR(1) lookaheads for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LALR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      First                : in     LR1_Items.Derivation_Matrix;
      Kernels              : in out LR1_Items.Item_Set_List;
      Accept_State         : in     State_Index;
      Used_Tokens          : in out Token.Token_Array_Boolean;
      Trace                : in     Boolean)
   is
      Kernel       : LR1_Items.Item_Set_Ptr := Kernels.Head;
      Kernel_Item  : LR1_Items.Item_Ptr;
      Closure_Item : LR1_Items.Item_Ptr;

      Kernel_Item_Set : LR1_Items.Item_Set :=
        (Set       => new LR1_Items.Item_Node,
         Goto_List => null,
         State     => Unknown_State,
         Next      => null);

      Closure : LR1_Items.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LR1_Items.Item_Set_Ptr;
      use type LR1_Items.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookaheads := new LR1_Items.Lookahead'(Propagate => True, Next => null);

      while Kernel /= null loop
         if Trace then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LR1_Items.Put (Kernel.all, Show_Lookaheads => True);
         end if;

         Kernel_Item := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Dot  := Kernel_Item.Dot;

            Closure := LR1_Items.Closure
              (Kernel_Item_Set, Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel.all, Closure_Item.all, Accept_State, Propagation_List, Used_Tokens, Trace);

               Closure_Item := Closure_Item.Next;
            end loop;

            LR1_Items.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Propagations:");
         Print_Propagations (Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LR1_Items.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

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
            when Accept_It =>
               if Nonterminal.ID (Item.Prod.LHS) = Nonterminal.ID (Action.LHS) and
                 (Item.Dot /= Null_Iterator and then
                    ID (Item.Dot) = EOF_Token)
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
            when Reduce | Accept_It => " " & Token.Token_Image (Nonterminal.ID (Action.LHS)),
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

   --  Add (Symbol, Action) to Action_List
   --  Closure .. Conflicts are for conflict reporting
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
      use type Ada.Strings.Unbounded.Unbounded_String;
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
            --  There is a conflict. Report it, but add it anyway, so
            --  an enhanced parser can follow both paths
            declare
               --  Enforce canonical Shift/Reduce or Accept/Reduce
               --  order, to simplify searching and code generation.
               Action_A : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Action else Matching_Action.Action.Item);

               Action_B : constant Parse_Action_Rec :=
                 (if Action.Verb in Shift | Accept_It then Matching_Action.Action.Item else Action);

               Action_A_Ptr : Parse_Action_Node_Ptr;
               Action_B_Ptr : Parse_Action_Node_Ptr;

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
                  --  kernel. Only add it to conflicts once, but still
                  --  need second action on current kernel.
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
                  Action_A_Ptr := new Parse_Action_Node'(Action, Matching_Action.Action);

               else
                  Action_B_Ptr      := new Parse_Action_Node'(Action, null);
                  Action_A_Ptr      := Matching_Action.Action;
                  Action_A_Ptr.Next := Action_B_Ptr;
               end if;

               Matching_Action.Action := Action_A_Ptr;

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
      Kernel               : in     LR1_Items.Item_Set_Ptr;
      Action_List          : in out Action_Node_Ptr;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Trace                : in     Boolean)
   is
      --  Add actions for Item.Lookaheads to Action_List
      --  Item must be from Kernel.
      --  Closure must be from Lookahead_Closure (Kernel).
      --  Has_Empty_Production .. Closure used for conflict reporting.
      use type LR1_Items.Lookahead_Ptr;

      Action : constant Parse_Action_Rec :=
        (Reduce, Item.Prod.LHS, Item.Prod.RHS.Action, Item.Prod.RHS.Index, Item.Prod.RHS.Tokens.Length);

      Lookahead : LR1_Items.Lookahead_Ptr := Item.Lookaheads;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      while Lookahead /= null loop
         --  Lookahead.Propagate should never be True here.
         Add_Action
           (Symbol               => Lookahead.Lookahead,
            Action               => Action,
            Action_List          => Action_List,
            State_Index          => Kernel.State,
            Closure              => Closure,
            Has_Empty_Production => Has_Empty_Production,
            Conflicts            => Conflicts,
            Trace                => Trace);
         Lookahead := Lookahead.Next;
      end loop;
   end Add_Lookahead_Actions;

   --  Add actions for Kernel to Table
   procedure Add_Actions
     (Kernel               : in     LR1_Items.Item_Set_Ptr;
      Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      First                : in     LR1_Items.Derivation_Matrix;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      State : constant State_Index := Kernel.State;

      Closure : LR1_Items.Item_Set := LR1_Items.Closure
        (Kernel.all, Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);

      Item : LR1_Items.Item_Ptr := Closure.Set;

      use type LR1_Items.Item_Ptr;
      use type LR1_Items.Goto_Item_Ptr;
      use type Token.List.List_Iterator;
      use type Token.Handle;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("adding actions for kernel" & State_Index'Image (Kernel.State));
         Ada.Text_IO.Put ("closure: ");
         LR1_Items.Put (Closure);
         LR1_Items.Put (Kernel.Goto_List);
      end if;

      while Item /= null loop
         if Item.Dot = Token.List.Null_Iterator then
            --  Pointer is at the end of the production; add a reduce action.

            Add_Lookahead_Actions
              (Item, Kernel, Table (State).Action_List, Has_Empty_Production, Conflicts, Closure, Trace);

         elsif Token.List.ID (Item.Dot) in Token.Terminal_ID then
            --  Dot is before a terminal token.
            declare
               use type Token.Token_ID;
               use type LR1_Items.Item_Set_Ptr;

               Dot_ID : constant Token.Terminal_ID := Token.List.ID (Item.Dot);
               --  ID of token after Item.Dot

               Goto_Set : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (Kernel.all, Dot_ID);
            begin
               if Dot_ID = EOF_Token then
                  --  This is the start symbol production with dot before EOF.
                  Add_Action
                    (Symbol               => Dot_ID,
                     Action               =>
                       (Accept_It, Item.Prod.LHS, Item.Prod.RHS.Action, Item.Prod.RHS.Index,
                        Item.Prod.RHS.Tokens.Length - 1), -- EOF is not pushed on stack
                     Action_List          => Table (State).Action_List,
                     State_Index          => Kernel.State,
                     Closure              => Closure,
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
                        State_Index          => Kernel.State,
                        Closure              => Closure,
                        Has_Empty_Production => Has_Empty_Production,
                        Conflicts            => Conflicts,
                        Trace                => Trace);
                  end if;
               end if;
            end;
         else
            --  Dot is before a non-terminal token; no action. An
            --  empty production for the non-terminal will appear in
            --  the closure, and be handled above.
            if Trace then
               Ada.Text_IO.Put_Line (Token.Token_Image (Token.List.ID (Item.Dot)) & " => no action");
            end if;
         end if;

         Item := Item.Next;
      end loop;

      --  Place a default error action at the end of every state.
      --  (it should always have at least one action already).
      --
      --  IMPROVEME: instead, optimize use of default action; compress
      --  accept, at least.
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
            --  not the start symbol production; that violates the
            --  assumptions Generate_Lookahead_Info makes when
            --  computing lookaheads, and Add_Actions makes
            --  when assigning accept/reduce actions.
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
              "Generating parser: state" & State_Index'Image (Kernel.State) &
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

      LR1_Items.Free (Closure);

      --  Fill in this state's Goto transitions
      declare
         Goto_Ptr : LR1_Items.Goto_Item_Ptr := Kernel.Goto_List;
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

   --  Add actions for all Kernels to Table.
   procedure Add_Actions
     (Kernels              : in     LR1_Items.Item_Set_List;
      Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      First                : in     LR1_Items.Derivation_Matrix;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      Kernel : LR1_Items.Item_Set_Ptr := Kernels.Head;
      use type LR1_Items.Item_Set_Ptr;
   begin
      while Kernel /= null loop
         Add_Actions (Kernel, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace);
         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

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
      use type Production.Instance;
      use type LR1_Items.Item_Set_Ptr;

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set := LR1_Items.Has_Empty_Production (Grammar);
      First                : constant LR1_Items.Derivation_Matrix  := LR1_Items.First
        (Grammar, Has_Empty_Production, Trace);
      Used_Tokens          : Token.Token_Array_Boolean             := (others => False);

      Kernels : LR1_Items.Item_Set_List := LR1_Items.LALR_Kernels
        (Grammar, First, EOF_Token, Trace, Unknown_State_Index (First_State_Index));

      I             : LR1_Items.Item_Set_Ptr := Kernels.Head;
      Accept_State  : Unknown_State_Index    := Unknown_State;
      Unused_Tokens : Boolean                := False;

      First_Production : Production.Instance renames Production.List.Current (Grammar.First);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

   begin
      --  Accept_State identifies the kernel that is the start symbol
      --  production with dot before EOF. The start symbol production
      --  must be the first production in Grammar, but that does not
      --  guarrantee its position in Kernels, so we search for it.
      loop
         exit when I = null;
         if I.Set.Prod = First_Production then
            Accept_State := I.State;
            exit;
         end if;
         I := I.Next;
      end loop;

      if Accept_State = Unknown_State then
         raise Programmer_Error with "Accept_State = 0; something wrong with Grammar?";
      end if;

      if Trace then
         Ada.Text_IO.Put_Line ("Accept_State:" & State_Index'Image (Accept_State));
      end if;

      Used_Tokens (Nonterminal.ID (First_Production.LHS)) := True;

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Accept_State, Used_Tokens, Trace);

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Token.Token_Image (I));
         end if;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LR1_Items.Put (Kernels);
      end if;

      Table := new Parse_Table (State_Index'First .. Kernels.Size - 1 + State_Index'First);

      Add_Actions
        (Kernels, Grammar, Has_Empty_Production, First, Unknown_Conflicts, Table.all, Trace);

      if Put_Parse_Table then
         LALR_Generator.Put_Parse_Table (Table, Kernels);
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

end FastToken.Parser.LR.LALR_Generator;
--  Local Variables:
--  jit-lock-defer-time: 0.25
--  End:
