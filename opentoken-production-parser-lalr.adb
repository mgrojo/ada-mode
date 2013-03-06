--  Copyright (C) 2002 - 2005, 2008 - 2013 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with OpenToken.Production.Parser.LRk_Item;
package body OpenToken.Production.Parser.LALR is

   Line_End : constant String := "" & Ada.Characters.Latin_1.LF;

   package LRk is new OpenToken.Production.Parser.LRk_Item (1);

   --  Following are the types used in the parse "table". The parse
   --  table is an array indexed by parse state that where each state
   --  contains a list of parse actions and a list of reduction
   --  actions.
   --
   --  Parse actions are indexed by the terminal they match and are either
   --    o Shift and change to a designated state.
   --    o Reduce by the given production
   --
   --  Reduction actions are indexed by the nonterminal they match and
   --  designate the state the parser need to change to.

   --  The following types are used for the Parser's stack. The stack
   --  designates the tokens that have been read or derived, and the
   --  parser states in which that occurred.

   type State_Node;
   type State_Node_Ptr is access State_Node;

   type State_Node is record
      State      : State_Index := 0;
      Seen_Token : Token.Handle;
      Next       : State_Node_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (State_Node, State_Node_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Token.Class, Token.Handle);

   --  The following types are used for computing lookahead
   --  propagations

   type Item_List;
   type Item_List_Ptr is access Item_List;
   type Item_List is record
      Item : LRk.Item_Ptr;
      Next : Item_List_Ptr;
   end record;

   type Item_Item_List_Mapping;
   type Item_Item_List_Mapping_Ptr is access Item_Item_List_Mapping;

   type Item_Item_List_Mapping is record
      From : LRk.Item_Ptr;
      To   : Item_List_Ptr;
      Next : Item_Item_List_Mapping_Ptr;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);

   procedure Free (List : in out Item_List_Ptr) is
      Old_Item : Item_List_Ptr := List;
   begin
      while Old_Item /= null loop
         List := Old_Item.Next;
         Dispose (Old_Item);
         Old_Item := List;
      end loop;
   end Free;

   procedure Free (List : in out Item_Item_List_Mapping_Ptr) is
      Old_Mapping : Item_Item_List_Mapping_Ptr := List;
   begin
      while Old_Mapping /= null loop
         List := Old_Mapping.Next;
         Free (Old_Mapping.To);
         Dispose (Old_Mapping);
         Old_Mapping := List;
      end loop;
   end Free;

   --  Return the action for the given state index and terminal ID.
   --  The final action in the action list for a state is returned if no
   --  other node matches ID.
   function Action_For
     (Table : in Parse_Table_Ptr;
      State : in State_Index;
      ID    : in Tokenizer.Terminal_ID)
     return Parse_Action_Rec
   is
      use type Tokenizer.Terminal_ID;
      Action_Node : Action_Node_Ptr := Table.all (State).Action_List;
   begin
      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      if Action_Node.Action.Next = null then
         return Action_Node.Action.Item;
      else
         raise Parse_Error with "conflicting actions in state" & State_Index'Image (State);
      end if;
   end Action_For;

   function Goto_For
     (Table : in Parse_Table_Ptr;
      State : in State_Index;
      ID    : in Token.Token_ID)
     return State_Index
   is
      use type Tokenizer.Terminal_ID;
      Reduction_Node : Reduction_Node_Ptr := Table.all (State).Reduction_List;
   begin
      while Reduction_Node.Next /= null and Reduction_Node.Symbol /= ID loop
         Reduction_Node := Reduction_Node.Next;
      end loop;

      return Reduction_Node.State;
   end Goto_For;

   function Find
     (Symbol      : in Tokenizer.Terminal_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr
   is
      use type Tokenizer.Terminal_ID;
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

   procedure Print_Propagations (Propagations : Item_Item_List_Mapping_Ptr) is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Propagations:");

      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LRk.Put_Item (Next_Prop.From.all);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("          To ");
            LRk.Put_Item (Next_To.Item.all);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Print_Propagations;

   --  Add propagation entries (if they don't already exist) from From
   --  to all kernel items that match To.
   procedure Add_Propagations
     (From         : in     LRk.Item_Ptr;
      From_Set     : in     LRk.Item_Set;
      To           : in     LRk.Item_Node;
      For_Token    : in     Token.Token_ID;
      Propagations : in out Item_Item_List_Mapping_Ptr)
   is
      use type Token_List.List_Iterator;
      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;

      To_Kernel : constant LRk.Item_Ptr := LRk.Find (To, LRk.Goto_Set (From_Set, For_Token).all);

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

               if Prop_To_Match.Item = To_Kernel then
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

   --  Calculate the lookaheads for Source_Set, Source_Set, Closure_Item.
   --  Spontanious lookaheads are put in Source_Item.Lookahead, propagated lookaheads in Propagations.
   --
   --  The start symbol (with Source_Set.Index = Accept_Index) is treated specially.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  : in     LRk.Item_Ptr;
      Source_Set   : in     LRk.Item_Set;
      Closure_Item : in     LRk.Item_Node;
      Accept_Index : in     Integer;
      Propagations : in out Item_Item_List_Mapping_Ptr;
      Used_Tokens  : in out Tokenizer.Token_Array_Boolean;
      Trace        : in     Boolean)
   is
      Next_Item   : LRk.Item_Node;
      Next_Token  : Token_List.List_Iterator;
      Token_ID    : Token.Token_ID;
      Next_Kernel : LRk.Item_Ptr;
      Lookahead   : LRk.Item_Lookahead_Ptr := Closure_Item.Lookahead_Set;

      use type Token.Handle;
      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
      use type LRk.Item_Lookahead_Ptr;
   begin
      --  If this is the start symbol production, it gets a lookahead
      --  for each terminal, so it will reduce on anything.
      if Source_Set.Index = Accept_Index then
         for Token_ID in Tokenizer.Terminal_ID loop
            --  These tokens are not actually used in the grammar, so
            --  we don't set Used_Tokens here.
            declare
               Lookahead : constant LRk.Item_Lookahead :=
                 (Last       => 1,
                  Lookaheads => (1 => Token_ID),
                  Next       => null);
            begin
               if Trace then
                  Ada.Text_IO.Put_Line ("Adding default lookahead:");
                  LRk.Put_Item (Source_Item.all);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead));
               end if;

               LRk.Include
                 (Set   => Source_Item.Lookahead_Set,
                  Value => Lookahead);
            end;
         end loop;
      end if;

      --  If the closure item doesn't have a token after Dot,
      --  there's nothing else to do.
      if Token_List.Token_Handle (Closure_Item.Dot) = null then
         return;
      end if;

      Next_Token := Closure_Item.Dot;
      Token_List.Next_Token (Next_Token); --  Second token after Dot
      Next_Item  :=
        (Prod          => Closure_Item.Prod,
         Dot           => Next_Token,
         Lookahead_Set => null,
         Next          => null);

      Token_ID := Token.ID (Token_List.Token_Handle (Closure_Item.Dot).all);

      begin
         Used_Tokens (Token_ID) := True;
      exception
      when Constraint_Error =>
         raise Grammar_Error with "non-reporting " & Token.Token_Image (Token_ID) & " used in grammar";
      end;

      --  Check all of the closure item's lookaheads
      while Lookahead /= null loop
         if Lookahead.Last = 0 then
            --  Lookaheads propagate
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To           => Next_Item,
               For_Token    => Token_ID,
               Propagations => Propagations);

         else
            --  Lookaheads are generated spontaneously for all items
            --  in the source item's goto for the current symbol that
            --  match the next_item.

            Next_Kernel := LRk.Find (Next_Item, LRk.Goto_Set (Source_Set, Token_ID).all);

            if Next_Kernel /= null then
               if Trace then
                  Ada.Text_IO.Put_Line ("Adding spontaneous lookahead:");
                  LRk.Put_Item (Next_Kernel.all);
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead.all));
               end if;

               LRk.Include
                 (Set   => Next_Kernel.Lookahead_Set,
                  Value => Lookahead.all);
            end if;

         end if;

         Lookahead := Lookahead.Next;
      end loop;
   end Generate_Lookahead_Info;

   --  Propagate lookaheads as directed by List, until no more
   --  lookaheads are propagated.
   procedure Propagate_Lookaheads
     (List  : in Item_Item_List_Mapping_Ptr;
      Trace : in Boolean)
   is
      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LRk.Item_Lookahead_Ptr;
      Added_One     : Boolean;

      use type LRk.Item_Lookahead_Ptr;
   begin
      while More_To_Check loop

         --  Check every valid lookahead against every mapped item in every mapping
         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookahead_Set;
            while Lookahead /= null loop

               if Lookahead.Last > 0 then
                  To := Mapping.To;
                  while To /= null loop
                     LRk.Include
                       (Set   => To.Item.Lookahead_Set,
                        Value => Lookahead.all,
                        Added => Added_One);

                     if Trace and Added_One then
                        Ada.Text_IO.Put_Line ("Adding propagated lookahead:");
                        LRk.Put_Item (To.Item.all);
                        Ada.Text_IO.New_Line;
                        Ada.Text_IO.Put_Line ("   " & LRk.Print (Lookahead));
                     end if;

                     More_To_Check := More_To_Check or Added_One;
                     To := To.Next;
                  end loop;
               end if;

               Lookahead := Lookahead.Next;
            end loop;

            Mapping := Mapping.Next;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LR(1) propogations for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar      : in     Production_List.Instance;
      First        : in     LRk.Derivation_Matrix;
      Kernels      : in out LRk.Item_Set_List;
      Accept_Index : in     Integer;
      Used_Tokens  : in out Tokenizer.Token_Array_Boolean;
      Trace        : in     Boolean)
   is

      Kernel       : LRk.Item_Set_Ptr := Kernels.Head;
      Kernel_Item  : LRk.Item_Ptr;
      Closure_Item : LRk.Item_Ptr;

      Kernel_Item_Set : LRk.Item_Set :=
        (Set       => new LRk.Item_Node,
         Goto_List => null,
         Index     => 0,
         Next      => null);

      Propagate_Lookahead : constant LRk.Item_Lookahead_Ptr := new LRk.Item_Lookahead'
        (Last       => 0,
         Lookaheads => (others => Tokenizer.Terminal_ID'First),
         Next       => null);

      Closure : LRk.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookahead_Set := Propagate_Lookahead;

      while Kernel /= null loop
         if Trace then
            Ada.Text_IO.Put_Line ("Adding lookaheads for state" & Integer'Image (Kernel.Index));
         end if;

         Kernel_Item := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Dot  := Kernel_Item.Dot;

            Closure := LRk.Lookahead_Closure (Kernel_Item_Set, First, Grammar);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel.all, Closure_Item.all, Accept_Index, Propagation_List,
                  Used_Tokens, Trace);

               Closure_Item := Closure_Item.Next;
            end loop;

            LRk.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Print_Propagations (Propagation_List);
      end if;

      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LRk.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   ----------------------------------------------------------------------------
   --  A trimmed Image.
   ----------------------------------------------------------------------------
   function Integer_Image (Subject : in Integer) return String is
      State_Image : String (1 .. 5);
   begin
      Ada.Integer_Text_IO.Put
        (To   => State_Image,
         Item => Subject);

      return Ada.Strings.Fixed.Trim (Source => State_Image, Side => Ada.Strings.Both);
   end Integer_Image;

   procedure Put_Parse_Action (Action : in Parse_Action_Node_Ptr)
   is
      use Ada.Text_IO;
      Ptr    : Parse_Action_Node_Ptr   := Action;
      Column : constant Positive_Count := Col;
   begin
      loop
         declare
            Parse_Action : Parse_Action_Rec renames Ptr.Item;
         begin
            case Parse_Action.Verb is
            when Shift =>
               Put ("shift and goto state" & State_Index'Image (Parse_Action.State));

            when Reduce =>
               Put
                 ("reduce" & Integer'Image (Parse_Action.Length) &
                    " tokens to " & Token.Token_Image (LHS_ID (Parse_Action.Production)));
            when Accept_It =>
               Put ("accept it");
            when Error =>
               Put ("ERROR");
            end case;
         end;
         Ptr := Ptr.Next;
         exit when Ptr = null;
         Put_Line (",");
         Set_Col (Column);
      end loop;
   end Put_Parse_Action;

   procedure Put_Parse_State
     (State      : in Parse_State;
      Kernel_Set : in LRk.Item_Set_Ptr)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use type LRk.Item_Ptr;
      Action    : Action_Node_Ptr    := State.Action_List;
      Reduction : Reduction_Node_Ptr := State.Reduction_List;
      Kernel    : LRk.Item_Ptr       := Kernel_Set.Set;
   begin
      New_Line;
      while Kernel /= null loop
         LRk.Put_Item (Kernel.all, "   ");
         New_Line;
         Kernel := Kernel.Next;
      end loop;

      New_Line;
      if Action = null then
         raise Programmer_Error with "LALR: Action contains no default entry";
      elsif Action.Next = null then
         Put_Line ("   (no actions)");
      end if;

      while Action /= null loop
         if Action.Next = null then
            Put ("   default" & (Token.Token_Image_Width - 7) * ' ' & " => ");
            Put_Parse_Action (Action.Action);
            New_Line;
         else
            Put ("   " & Token.Token_Image (Action.Symbol) &
                   (Token.Token_Image_Width - Token.Token_Image (Action.Symbol)'Length) * ' '
                   & " => ");
            Put_Parse_Action (Action.Action);
            New_Line;
         end if;
         Action := Action.Next;
      end loop;

      New_Line;
      --  FIXME: these are "gotos"; the state to goto after reducing to Reduction.symbol. or something like that.
      --  Reduction actions are shown above. Change the name!
      while Reduction /= null loop
         if Reduction.Symbol not in Tokenizer.Terminal_ID then
            --  Terminal_IDs are shown in Actions, above
            Put_Line
              ("   " & Token.Token_Image (Reduction.Symbol) &
                 (Token.Token_Image_Width - Token.Token_Image (Reduction.Symbol)'Length) * ' ' &
                 " goto state" & State_Index'Image (Reduction.State));
         end if;
         Reduction := Reduction.Next;
      end loop;
   end Put_Parse_State;

   procedure Put_Parse_Table
     (Table       : in Parse_Table;
      Kernel_Sets : in LRk.Item_Set_List)
   is
      use Ada.Text_IO;
   begin
      for State in Table'Range loop
         Put_Line ("State" & State_Index'Image (State) & ":");
         Put_Parse_State (Table (State), LRk.Find (Integer (State), Kernel_Sets));
         New_Line;
      end loop;
   end Put_Parse_Table;

   --  Add (Symbol, Action) to Action_List
   procedure Add_Action
     (Symbol      : in     Tokenizer.Terminal_ID;
      Action      : in     Parse_Action_Rec;
      Action_List : in out Action_Node_Ptr;
      Source      : in     LRk.Item_Set;
      Conflicts   : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      --  Source .. Conflicts are for conflict reporting
      use type Ada.Strings.Unbounded.Unbounded_String;
      Matching_Action : constant Action_Node_Ptr := Find (Symbol, Action_List);
   begin
      if Matching_Action /= null then
         if Matching_Action.Action.Item = Action then
            --  Matching_Action is identical to Action, so there is no
            --  conflict; just don't add it again.
            return;
         else
            --  There is a conflict. Report it, but add it anyway, so
            --  an enhanced parser can follow both paths
            Conflicts := Conflicts & Parse_Action_Verbs'Image (Matching_Action.Action.Item.Verb) &
              "/" & Parse_Action_Verbs'Image (Action.Verb) & " in state:" & Natural'Image (Source.Index) &
              " on token " & Tokenizer.Terminal_ID'Image (Symbol) & Line_End;

            Matching_Action.Action := new Parse_Action_Node'
              (Item => Action,
               Next => Matching_Action.Action);
         end if;
      else
         Action_List := new Action_Node'
           (Symbol => Symbol,
            Action => new Parse_Action_Node'(Action, null),
            Next   => Action_List);
      end if;
   end Add_Action;

   ----------------------------------------------------------------------------
   --  Fill in the parse table using the given LR(k) kernel sets.
   ----------------------------------------------------------------------------
   procedure Fill_In_Parse_Table
     (LRk_Kernels  : in     LRk.Item_Set_List;
      Accept_Index : in     Integer;
      Grammar      : in     Production_List.Instance;
      First        : in     LRk.Derivation_Matrix;
      Table        : in out Parse_Table;
      Trace        : in     Boolean)
   is
      use Ada.Strings.Unbounded;

      --  The default action, when nothing else matches an input
      Default_Action : constant Action_Node :=
         --  The symbol here is actually irrelevant; it is the
         --  position as the last on a state's action list that makes
         --  it the default. It's too bad we can't extend an
         --  enumeration type to make this 'default', for viewing this
         --  list in a debugger. The various Put routines do replace
         --  this with 'default'.
        (Symbol => Tokenizer.Terminal_ID'Last,
         Action => new Parse_Action_Node'(Parse_Action_Rec'(Verb => Error), null),
         Next   => null);

      Last_Action : Action_Node_Ptr;

      Kernel    : LRk.Item_Set_Ptr := LRk_Kernels.Head;
      Closure   : LRk.Item_Set;
      Item      : LRk.Item_Ptr;
      Lookahead : LRk.Item_Lookahead_Ptr;

      Production_Length : Natural;
      RHS_Iterator      : Token_List.List_Iterator;

      Goto_Node : LRk.Set_Reference_Ptr;

      Conflicts : Unbounded_String := Null_Unbounded_String;

      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;
      use type LRk.Set_Reference_Ptr;
      use type LRk.Item_Lookahead_Ptr;
      use type Token_List.List_Iterator;
      use type Token.Handle;
   begin
      while Kernel /= null loop

         if Trace then
            Ada.Text_IO.Put_Line ("adding actions for kernel" & Integer'Image (Kernel.Index));
         end if;

         Closure := LRk.Lookahead_Closure
           (Set     => Kernel.all,
            First   => First,
            Grammar => Grammar);

         Item := Closure.Set;
         while Item /= null loop
            if Item.Dot = Token_List.Null_Iterator then
               --  Pointer is at the end of the production; add a reduce or accept action.

               --  Find the length of the producion to save time during reductions
               Production_Length := 0;
               RHS_Iterator := Token_List.Initial_Iterator (Item.Prod.RHS.Tokens);
               while Token_List.Token_Handle (RHS_Iterator) /= null loop
                  Production_Length := Production_Length + 1;
                  Token_List.Next_Token (RHS_Iterator);
               end loop;

               if Trace then
                  Ada.Text_IO.Put_Line ("processing lookaheads");
               end if;

               Lookahead := Item.Lookahead_Set;
               while Lookahead /= null loop
                  --  Add reduction/accept action

                  --  Only the start symbol kernel gets accept; the
                  --  rest get reduce. See [dragon] algorithm 4.11
                  --  page 238, 4.10 page 234, except that here the
                  --  augmenting production is implicit.
                  if Kernel.Index = Accept_Index then
                     if Trace then
                        Ada.Text_IO.Put_Line ("adding Accept_It");
                     end if;

                     Add_Action
                       (Symbol        => Lookahead.Lookaheads (1),
                        Action        =>
                          (Verb       => Accept_It,
                           Production => Item.Prod,
                           Length     => Production_Length),
                        Action_List   => Table (State_Index (Kernel.Index)).Action_List,
                        Source        => Kernel.all,
                        Conflicts     => Conflicts);
                  else
                     if Trace then
                        Ada.Text_IO.Put_Line ("adding Reduce");
                     end if;

                     Add_Action
                       (Symbol        => Lookahead.Lookaheads (1),
                        Action        =>
                          (Verb       => Reduce,
                           Production => Item.Prod,
                           Length     => Production_Length),
                        Action_List   => Table (State_Index (Kernel.Index)).Action_List,
                        Source        => Kernel.all,
                        Conflicts     => Conflicts);
                  end if;

                  Lookahead := Lookahead.Next;
               end loop;

            elsif Token.ID (Token_List.Token_Handle (Item.Dot).all) in Tokenizer.Terminal_ID then
               --  Dot is before a terminal token.
               declare
                  Item_Dot_ID : constant Token.Token_ID := Token.ID (Token_List.Token_Handle (Item.Dot).all);
                  --  ID of token after Item.Dot
               begin
                  if Trace then
                     Ada.Text_IO.Put_Line (Token.Token_Image (Item_Dot_ID) & " => Shift");
                  end if;

                  Add_Action
                    (Symbol      => Item_Dot_ID,
                     Action      =>
                       (Verb     => Shift,
                        State    => State_Index (LRk.Goto_Set (Kernel.all, Item_Dot_ID).Index)),
                     Action_List => Table (State_Index (Kernel.Index)).Action_List,
                     Source      => Kernel.all,
                     Conflicts   => Conflicts);
               end;
            else
               --  Pointer is before a non-terminal token; action is
               --  determined by the lookahead, handled above.

               if Trace then
                  Ada.Text_IO.Put_Line
                    (Token.Token_ID'Image (Token.ID (Token_List.Token_Handle (Item.Dot).all)) &
                       " => no action");
               end if;
            end if;

            Item := Item.Next;
         end loop;

         --  Place a default error action at the end of every state.
         --  (it should always have at least one action already).
         --
         --  FIXME: instead, optimize use of default action; compress
         --  accept, at least.
         Last_Action := Table (State_Index (Kernel.Index)).Action_List;

         if Last_Action = null then
            --  This happens if the first production in the grammar is
            --  not the start symbol production; that violates the
            --  assumptions Generate_Lookahead_Info makes when
            --  computing lookaheads, and Fill_In_Parse_Table makes
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
              "Generating parser: state" & Integer'Image (Kernel.Index) &
              " has no actions; bad grammar, or " &
              "first production in grammar must be the only start symbol production, " &
              "and it must must have an explicit EOF.";
         else
            while Last_Action.Next /= null loop
               Last_Action := Last_Action.Next;
            end loop;
            Last_Action.Next := new Action_Node'(Default_Action);
         end if;

         LRk.Free (Closure);

         --  Fill in this state's Goto transitions
         Goto_Node := Kernel.Goto_List;
         while Goto_Node /= null loop
            Table (State_Index (Kernel.Index)).Reduction_List :=
              new Reduction_Node'
              (Symbol => Goto_Node.Symbol,
               State  => State_Index (Goto_Node.Set.Index),
               Next   => Table (State_Index (Kernel.Index)).Reduction_List);

            Goto_Node := Goto_Node.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Length (Conflicts) /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Conflicts: ");
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, To_String (Conflicts));
      end if;

   end Fill_In_Parse_Table;

   procedure Reduce_Stack
     (Stack            : in out State_Node_Ptr;
      Number_Of_Tokens : in     Natural;
      Production       : in     OpenToken.Production.Instance)
   is
      Arguments    : Token_List.Instance;
      Popped_State : State_Node_Ptr;
      Args_Added   : Natural := 0;

      use type Nonterminal.Synthesize;
   begin
      --  Pop the indicated number of token states from the stack, and
      --  call the production action routine to create a new
      --  nonterminal token.

      --  Build the argument list, while popping all but the last
      --  argument's state off of the stack.
      if Number_Of_Tokens > 0 then
         loop
            Token_List.Enqueue (Arguments, Stack.Seen_Token);

            Args_Added := Args_Added + 1;
            exit when Args_Added = Number_Of_Tokens;

            Popped_State := Stack;
            Stack        := Stack.Next;
            Free (Popped_State);
         end loop;
      end if;

      Production.RHS.Action (Production.LHS.all, Arguments, Token.ID (Production.LHS.all));
      Token_List.Clean (Arguments);

      --  Replace stack token with LHS of production
      Stack.Seen_Token := new Nonterminal.Class'(Production.LHS.all);
   end Reduce_Stack;

   overriding function Generate
     (Grammar           : in Production_List.Instance;
      Analyzer          : in Tokenizer.Instance;
      Trace             : in Boolean := False;
      Put_Grammar       : in Boolean := False;
      First_State_Index : in Integer := 1)
     return Instance
   is
      New_Parser  : Instance;

      First_Tokens : constant LRk.Derivation_Matrix := LRk.First_Derivations (Grammar, Trace);
      Used_Tokens  : Tokenizer.Token_Array_Boolean  := (others => False);

      Kernels       : LRk.Item_Set_List := LRk.LR0_Kernels (Grammar, First_Tokens, Trace, First_State_Index);
      I             : LRk.Item_Set_Ptr  := Kernels.Head;
      Accept_Index  : Integer           := 0;
      Unused_Tokens : Boolean           := False;

      First_Production : OpenToken.Production.Instance renames
        Production_List.Get_Production (Production_List.Initial_Iterator (Grammar));

      use type LRk.Item_Set_Ptr;
   begin
      New_Parser.Analyzer := Analyzer;

      Used_Tokens (LHS_ID (First_Production)) := True;

      --  Accept_Index identifies the kernel that is the start symbol
      --  production, which must be the first production in Grammar.
      --  That does not guarrantee its position in Kernels, so we
      --  search for it.
      loop
         exit when I = null;
         if I.Set.Prod = First_Production then
            Accept_Index := I.Index;
            exit;
         end if;
         I := I.Next;
      end loop;

      if Accept_Index = 0 then
         raise Programmer_Error with
           "Accept_Index = 0; something wrong with Grammar?";
      end if;

      if Trace then
         Ada.Text_IO.Put_Line ("Accept_Index:" & Integer'Image (Accept_Index));
      end if;

      Fill_In_Lookaheads (Grammar, First_Tokens, Kernels, Accept_Index, Used_Tokens, Trace);

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
         LRk.Print_Item_Set_List (Kernels);
      end if;

      New_Parser.Table := new Parse_Table
        (State_Index (First_State_Index) .. State_Index (Kernels.Size - 1 + First_State_Index));

      --  Add actions
      Fill_In_Parse_Table (Kernels, Accept_Index, Grammar, First_Tokens, New_Parser.Table.all, Trace);

      if Put_Grammar then
         Put_Parse_Table (New_Parser.Table.all, Kernels);
      end if;

      LRk.Free (Kernels);
      if Unused_Tokens and not Trace then
         raise Grammar_Error with "unused tokens; aborting";
      end if;
      return New_Parser;
   end Generate;

   type Token_Array is array (Integer range <>) of Token.Token_ID;

   function Expecting (Table : in Parse_Table_Ptr; State : in State_Index) return Token_Array
   is
      Action : Action_Node_Ptr := Table (State).Action_List;
      Count  : Integer         := 0;
   begin
      loop
         exit when Action = null;

         Count  := Count + 1;
         Action := Action.Next;
      end loop;

      --  Last action is error; don't include it.
      declare
         Result : Token_Array (1 .. Count - 1);
      begin
         Action := Table (State).Action_List;
         for I in Result'Range loop
            Result (I) := Action.Symbol;
            Action     := Action.Next;
         end loop;
         return Result;
      end;
   end Expecting;

   function Names (Analyzer : in Tokenizer.Instance; Tokens : in Token_Array) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in Tokens'Range loop
         Result := Result & "'" & Tokenizer.Name (Analyzer, Tokens (I));
         if I = Tokens'Last then
            Result := Result & "'";
         else
            Result := Result & "' or ";
         end if;

      end loop;
      return To_String (Result);
   end Names;

   overriding procedure Parse (Parser : in out Instance)
   is
      Stack         : State_Node_Ptr;
      Current_State : State_Node;
      Action        : Parse_Action_Rec;
      Popped_State  : State_Node_Ptr;

      use type Token_List.Instance;
   begin

      --  Get the first token from the analyzer
      begin
         Tokenizer.Find_Next (Parser.Analyzer);
      exception
      when E : Syntax_Error =>
         raise Syntax_Error with
           Integer_Image (Line (Parser)) &
           ":" &
           Integer_Image (Column (Parser) - 1) &
           " " &
           Ada.Exceptions.Exception_Message (E);
      end;

      Current_State.Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

      Current_State.State := 1;
      loop

         --  Find the action for this token's ID
         Action := Action_For
           (Table => Parser.Table,
            State => Current_State.State,
            ID    => Token.ID (Current_State.Seen_Token.all));

         if Trace_Parse then
            Ada.Text_IO.Put
              (State_Index'Image (Current_State.State) &
                 " : " & Token.Token_ID'Image (Token.ID (Current_State.Seen_Token.all)) &
                 " : " & Parse_Action_Verbs'Image (Action.Verb));
         end if;

         case Action.Verb is
         when Shift =>
            --  Push this token state on the stack
            Current_State.Next := Stack;
            Stack := new State_Node'(Current_State);

            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Get the next token
            begin
               Tokenizer.Find_Next (Parser.Analyzer);
            exception
            when E : Syntax_Error =>
               raise Syntax_Error with
                 Integer_Image (Line (Parser)) &
                 ":" &
                 Integer_Image (Column (Parser) - 1) &
                 " " &
                 Ada.Exceptions.Exception_Message (E);
            end;

            Current_State.Seen_Token := new Token.Class'
              (Token.Class (Tokenizer.Get (Parser.Analyzer)));

            Current_State.State := Action.State;

         when Reduce =>

            --  Reduce by the indicated production
            Reduce_Stack
              (Stack            => Stack,
               Number_Of_Tokens => Action.Length,
               Production       => Action.Production);

            --  The next state is the one that the reduced state's goto for the
            --  LHS token takes us to.
            Current_State.State := Goto_For
              (Table => Parser.Table,
               State => Stack.State,
               ID    => Token.ID (Action.Production.LHS.all));

            if Trace_Parse then
               Ada.Text_IO.Put_Line
                 (" to state" &
                    State_Index'Image (Current_State.State) &
                    " : " & Token.Token_ID'Image (Token.ID (Action.Production.LHS.all)));
            end if;

         when Accept_It =>
            --  Reduce by the indicated production
            Reduce_Stack
              (Stack            => Stack,
               Number_Of_Tokens => Action.Length,
               Production       => Action.Production);

            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Clean up
            Free (Current_State.Seen_Token);
            while Stack /= null loop
               Popped_State := Stack;
               Stack := Stack.Next;
               Free (Popped_State.Seen_Token);
               Free (Popped_State);
            end loop;

            return;

         when Error =>
            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Clean up
            declare
               ID     : constant String := Token.Name (Current_State.Seen_Token.all);
               Lexeme : constant String := Tokenizer.Lexeme (Parser.Analyzer);

               Expecting_Tokens : constant Token_Array := Expecting (Parser.Table, Current_State.State);
            begin

               Free (Current_State.Seen_Token);
               while Stack /= null loop
                  Popped_State := Stack;
                  Stack := Stack.Next;
                  Free (Popped_State.Seen_Token);
                  Free (Popped_State);
               end loop;

               raise Syntax_Error with
                 Integer_Image (Line (Parser)) &
                 ":" &
                 Integer_Image (Column (Parser) - 1) &
                 ": Syntax error; expecting " &
                 Names (Parser.Analyzer, Expecting_Tokens) &
                 "; found " &
                 ID &
                 " '" &
                 Lexeme &
                 "'";
            end;
         end case;

      end loop;
   end Parse;

   procedure Put_Table (Parser : in Instance) is
   begin
      Ada.Text_IO.Put_Line ("Parse Table:");
      Put_Parse_Table (Parser.Table.all, (null, 0));
   end Put_Table;

end OpenToken.Production.Parser.LALR;
