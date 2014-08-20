--  Copyright (C) 2002 - 2005, 2008 - 2014 Stephe Leake
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

pragma License (Modified_GPL);

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
package body OpenToken.Production.Parser.LALR is

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
      State      : State_Index;
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
         LRk.Put (Next_Prop.From.all, Show_Lookaheads => False);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("To   ");
            LRk.Put (Next_To.Item.all, Show_Lookaheads => False);
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
   --  Spontaneous lookaheads are put in Source_Item.Lookahead, propagated lookaheads in Propagations.
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
      Spontaneous_Count : Integer := 0;

      use type LRk.Item_Lookahead_Ptr;
      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;
      use type Token_List.List_Iterator;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LRk.Put (Closure_Item, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;
      end if;

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
                  Ada.Text_IO.Put ("  default:");
                  LRk.Put (Source_Item.all, Show_Lookaheads => False);
                  Ada.Text_IO.Put_Line ("; " & LRk.Print (Lookahead));
               end if;

               LRk.Include (Source_Item.Lookaheads, Lookahead);
            end;
         end loop;
      end if;

      --  If the closure item doesn't have a token after Dot,
      --  there's nothing else to do.
      if Closure_Item.Dot = Token_List.Null_Iterator then
         return;
      end if;

      declare
         Token_ID   : constant Token.Token_ID           := Token_List.ID (Closure_Item.Dot);
         Next_Token : constant Token_List.List_Iterator := Token_List.Next_Token (Closure_Item.Dot);

         Next_Item : constant LRk.Item_Node :=
           (Prod       => Closure_Item.Prod,
            Dot        => Next_Token,
            Index      => -1,
            Lookaheads => null,
            Next       => null);

         Next_Kernel : constant LRk.Item_Ptr  := LRk.Find (Next_Item, LRk.Goto_Set (Source_Set, Token_ID).all);
         Lookahead   : LRk.Item_Lookahead_Ptr := Closure_Item.Lookaheads;
      begin
         begin
            Used_Tokens (Token_ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Token.Token_Image (Token_ID) & " used in grammar";
         end;

         while Lookahead /= null loop
            if Lookahead.Last = 0 then
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
                     Ada.Text_IO.Put_Line ("  spontaneous: " & LRk.Print (Lookahead.all));
                  end if;

                  LRk.Include (Next_Kernel.Lookaheads, Lookahead.all);
               end if;

            end if;

            Lookahead := Lookahead.Next;
         end loop;

         if Spontaneous_Count > 0 then
            Ada.Text_IO.Put ("  Next_Kernel (" & Token.Token_Image (Token_ID) & "): ");
            LRk.Put (Next_Kernel.all, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List  : in Item_Item_List_Mapping_Ptr;
      Trace : in Boolean)
   is
      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Lookahead     : LRk.Item_Lookahead_Ptr;
      Added_One     : Boolean;
      Added_Some    : Boolean := False;

      use type LRk.Item_Lookahead_Ptr;
   begin
      while More_To_Check loop

         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            Lookahead := Mapping.From.Lookaheads;

            while Lookahead /= null loop

               if Lookahead.Last > 0 then
                  To := Mapping.To;
                  while To /= null loop
                     LRk.Include (To.Item.Lookaheads, Lookahead.all, Added_One);

                     if Trace and Added_One then
                        Added_Some := True;
                        Ada.Text_IO.Put ("  to:");
                        LRk.Put (To.Item.all, Show_Lookaheads => True);
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
               LRk.Put (Mapping.From.all, Show_Lookaheads => True);
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
     (Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Kernels              : in out LRk.Item_Set_List;
      Accept_Index         : in     Integer;
      Used_Tokens          : in out Tokenizer.Token_Array_Boolean;
      Trace                : in     Boolean)
   is
      Kernel       : LRk.Item_Set_Ptr := Kernels.Head;
      Kernel_Item  : LRk.Item_Ptr;
      Closure_Item : LRk.Item_Ptr;

      Kernel_Item_Set : LRk.Item_Set :=
        (Set       => new LRk.Item_Node,
         Goto_List => null,
         Index     => -1,
         Next      => null);

      --  '#' lookahead from [dragon]
      Propagate_Lookahead : constant LRk.Item_Lookahead_Ptr := new LRk.Item_Lookahead'
        (Last       => 0,
         Lookaheads => (others => Tokenizer.Terminal_ID'First),
         Next       => null);

      Closure : LRk.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

      use type LRk.Item_Set_Ptr;
      use type LRk.Item_Ptr;
   begin

      Kernel_Item_Set.Set.Lookaheads := Propagate_Lookahead;

      while Kernel /= null loop
         if Trace then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LRk.Put (Kernel.all);
         end if;

         Kernel_Item := Kernel.Set;
         while Kernel_Item /= null loop
            Kernel_Item_Set.Set.Prod := Kernel_Item.Prod;
            Kernel_Item_Set.Set.Dot  := Kernel_Item.Dot;

            Closure := LRk.Lookahead_Closure (Kernel_Item_Set, Has_Empty_Production, First, Grammar, Trace => False);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel.all, Closure_Item.all, Accept_Index, Propagation_List, Used_Tokens, Trace);

               Closure_Item := Closure_Item.Next;
            end loop;

            LRk.Free (Closure);
            Kernel_Item := Kernel_Item.Next;
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Print_Propagations (Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LRk.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   --  A trimmed Image.
   function Integer_Image (Subject : in Integer) return String is
      State_Image : String (1 .. 5);
   begin
      Ada.Integer_Text_IO.Put
        (To   => State_Image,
         Item => Subject);

      return Ada.Strings.Fixed.Trim (Source => State_Image, Side => Ada.Strings.Both);
   end Integer_Image;

   procedure Put (Item : in Parse_Action_Rec)
   is
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Put
           ("reduce" & Integer'Image (Item.Length) &
              " tokens to " & Token.Token_Image (LHS_ID (Item.Production)));
      when Accept_It =>
         Put ("accept it");
      when Error =>
         Put ("ERROR");
      end case;
   end Put;

   procedure Put_Parse_Action (Action : in Parse_Action_Node_Ptr)
   is
      use Ada.Text_IO;
      Ptr    : Parse_Action_Node_Ptr   := Action;
      Column : constant Positive_Count := Col;
   begin
      loop
         Put (Ptr.Item);
         Ptr := Ptr.Next;
         exit when Ptr = null;
         Put_Line (",");
         Set_Col (Column);
      end loop;
   end Put_Parse_Action;

   procedure Put (State : in Parse_State)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use LRk;
      Action    : Action_Node_Ptr    := State.Action_List;
      Reduction : Reduction_Node_Ptr := State.Reduction_List;
   begin
      if Action = null then
         raise Programmer_Error with "LALR: Action contains no default entry";
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
   end Put;

   procedure Put_Parse_Table
     (Table       : in Parse_Table;
      Kernel_Sets : in LRk.Item_Set_List)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Parse Table:");
      for State in Table'Range loop
         LRk.Put (LRk.Find (Integer (State), Kernel_Sets).all);
         New_Line;
         Put (Table (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   function Image (Item : in Parse_Action_Rec) return String
   is begin
      case Item.Verb is
      when Shift =>
         return Parse_Action_Verbs'Image (Item.Verb) & State_Index'Image (Item.State);
      when Reduce | Accept_It =>
         return Parse_Action_Verbs'Image (Item.Verb) & " " & Token.Token_Image (LHS_ID (Item.Production));
      when Error =>
         return Parse_Action_Verbs'Image (Item.Verb);
      end case;
   end Image;

   function Find
     (Closure              : in LRk.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token.Token_ID;
      Has_Empty_Production : in LRk.Nonterminal_ID_Set)
     return Token.Token_ID
   is
      --  Return LHS of production that matches Action, Lookahead
      use Token_List;
      use type LRk.Item_Set;
      use type Token.Token_ID;
      use type LRk.Item_Ptr;
      use type LRk.Item_Set_Ptr;

      Current : LRk.Item_Set := Closure;
      Item    : LRk.Item_Ptr;
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
                  return LHS_ID (Item.Prod);
               end if;
            when Reduce =>
               if LHS_ID (Item.Prod) = LHS_ID (Action.Production) and
                 (Item.Dot = Null_Iterator or else
                    (Next_Token (Item.Dot) = Null_Iterator and
                       (ID (Item.Dot) in Nonterminal_ID and then
                          Has_Empty_Production (ID (Item.Dot)))))
               then
                  return LHS_ID (Action.Production);
               end if;
            when others =>
               raise Programmer_Error;
            end case;
            Item := Item.Next;
         end loop;
         exit when Current.Next = null;
         Current := Current.Next.all;
      end loop;

      Ada.Text_IO.Put_Line ("item for " & Image (Action) & ", " & Token.Token_Image (Lookahead) & " not found in");
      LRk.Put (Closure);
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
     (Symbol               : in     Tokenizer.Terminal_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LRk.Item_Set;
      State_Index          : in     Integer;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
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
                  Conflicts.Append (New_Conflict);

                  if Matching_Action = Action_List then
                     Action_List.Action := new Parse_Action_Node'
                       (Item => Action,
                        Next => Action_List.Action);
                  else
                     Matching_Action.Action := new Parse_Action_Node'
                       (Item => Action,
                        Next => Matching_Action.Action);
                  end if;
               end if;
            end;
            if Trace then
               Ada.Text_IO.Put_Line (" - conflict");
            end if;
         end if;
      else
         Action_List := new Action_Node'
           (Symbol => Symbol,
            Action => new Parse_Action_Node'(Action, null),
            Next   => Action_List);
      end if;
   end Add_Action;

   procedure Add_Lookahead_Actions
     (Item                 : in     LRk.Item_Ptr;
      Kernel               : in     LRk.Item_Set_Ptr;
      Accept_Index         : in     Integer;
      Action_List          : in out Action_Node_Ptr;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LRk.Item_Set;
      Trace                : in     Boolean)
   is
      --  Add actions for Item.Lookaheads to Action_List
      --  Item must be from Kernel.
      --  Closure must be from Lookahead_Closure (Kernel).
      --  Has_Empty_Production .. Closure used for conflict reporting.
      use type LRk.Item_Lookahead_Ptr;

      RHS_Length : constant Integer := Token_List.Length (Item.Prod.RHS.Tokens);

      --  Only the start symbol kernel gets accept; the rest get
      --  reduce. See [dragon] algorithm 4.11 page 238, 4.10 page 234,
      --  except that here the augmenting production is implicit.
      Action : constant Parse_Action_Rec :=
        (if Kernel.Index = Accept_Index then
           (Accept_It, Item.Prod, RHS_Length)
         else
            (Reduce, Item.Prod, RHS_Length));

      Lookahead : LRk.Item_Lookahead_Ptr := Item.Lookaheads;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("processing lookaheads");
      end if;

      while Lookahead /= null loop
         Add_Action
           (Symbol               => Lookahead.Lookaheads (1),
            Action               => Action,
            Action_List          => Action_List,
            State_Index          => Kernel.Index,
            Closure              => Closure,
            Has_Empty_Production => Has_Empty_Production,
            Conflicts            => Conflicts,
            Trace                => Trace);
         Lookahead := Lookahead.Next;
      end loop;
   end Add_Lookahead_Actions;

   --  Add actions for Kernel to Table
   procedure Add_Actions
     (Kernel               : in     LRk.Item_Set_Ptr;
      Accept_Index         : in     Integer;
      Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      State : constant State_Index := State_Index (Kernel.Index);

      Closure : LRk.Item_Set := LRk.Lookahead_Closure
        (Kernel.all, Has_Empty_Production, First, Grammar, Trace => False);

      Item : LRk.Item_Ptr := Closure.Set;

      use type LRk.Item_Ptr;
      use type LRk.Set_Reference_Ptr;
      use type Token_List.List_Iterator;
      use type Token.Handle;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("adding actions for kernel" & Integer'Image (Kernel.Index));
         Ada.Text_IO.Put ("closure: ");
         LRk.Put (Closure);
         LRk.Put (Kernel.Goto_List);
      end if;

      while Item /= null loop
         if Item.Dot = Token_List.Null_Iterator then
            --  Pointer is at the end of the production; add a reduce
            --  or accept action.

            Add_Lookahead_Actions
              (Item, Kernel, Accept_Index, Table (State).Action_List, Has_Empty_Production, Conflicts, Closure, Trace);

         elsif Token_List.ID (Item.Dot) in Tokenizer.Terminal_ID then
            --  Dot is before a terminal token.
            declare
               Dot_ID : constant Tokenizer.Terminal_ID := Token_List.ID (Item.Dot);
               --  ID of token after Item.Dot
            begin
               Add_Action
                 (Symbol               => Dot_ID,
                  Action               =>
                    (Verb              => Shift,
                     State             => State_Index (LRk.Goto_Set (Kernel.all, Dot_ID).Index)),
                  Action_List          => Table (State).Action_List,
                  State_Index          => Kernel.Index,
                  Closure              => Closure,
                  Has_Empty_Production => Has_Empty_Production,
                  Conflicts            => Conflicts,
                  Trace                => Trace);
            end;
         else
            --  Dot is before a non-terminal token; no action. An
            --  empty production for the non-terminal will appear in
            --  the closure, and be handled above.
            if Trace then
               Ada.Text_IO.Put_Line (Token.Token_Image (Token_List.ID (Item.Dot)) & " => no action");
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
           (Symbol => Tokenizer.Terminal_ID'Last,
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
      end;

      LRk.Free (Closure);

      --  Fill in this state's Goto transitions
      declare
         Goto_Node : LRk.Set_Reference_Ptr := Kernel.Goto_List;
      begin
         while Goto_Node /= null loop
            if Goto_Node.Symbol in Nonterminal_ID then
               Table (State).Reduction_List :=
                 new Reduction_Node'
                 (Symbol => Goto_Node.Symbol,
                  State  => State_Index (Goto_Node.Set.Index),
                  Next   => Table (State).Reduction_List);
            end if;
            Goto_Node := Goto_Node.Next;
         end loop;
      end;
   end Add_Actions;

   --  Add actions for all LRk_Kernels to Table.
   procedure Add_Actions
     (LRk_Kernels          : in     LRk.Item_Set_List;
      Accept_Index         : in     Integer;
      Grammar              : in     Production_List.Instance;
      Has_Empty_Production : in     LRk.Nonterminal_ID_Set;
      First                : in     LRk.Derivation_Matrix;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean)
   is
      Kernel : LRk.Item_Set_Ptr := LRk_Kernels.Head;
      use type LRk.Item_Set_Ptr;
   begin
      while Kernel /= null loop
         Add_Actions (Kernel, Accept_Index, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace);
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

   function Image (Item : in Conflict) return String
   is begin
      return
        (Conflict_Parse_Actions'Image (Item.Action_A) & "/" &
           Conflict_Parse_Actions'Image (Item.Action_B) & " in state " &
           Token.Token_Image (Item.LHS_A) & ", " &
           Token.Token_Image (Item.LHS_B) &
           " (" & Integer'Image (Item.State_Index) & ") on token " &
           Token.Token_Image (Item.On));
   end Image;

   procedure Put (Item : in Conflict_Lists.List)
   is
      use Ada.Text_IO;
   begin
      --  WORKAROUND: GNAT GPL 2012 doesn't like 'of' loop
      declare
         use Conflict_Lists;
         Conflict : Cursor := Item.First;
      begin
         loop
            exit when Conflict = No_Element;
            Put_Line (Image (Element (Conflict)));
            Next (Conflict);
         end loop;
      end;
   end Put;

   function Generate
     (Grammar                  : in Production_List.Instance;
      Analyzer                 : in Tokenizer.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Grammar              : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Instance
   is
      use type Ada.Containers.Count_Type;

      New_Parser  : Instance;

      Has_Empty_Production : constant LRk.Nonterminal_ID_Set := LRk.Has_Empty_Production (Grammar);
      First                : constant LRk.Derivation_Matrix  := LRk.First_Derivations
        (Grammar, Has_Empty_Production, Trace);
      Used_Tokens          : Tokenizer.Token_Array_Boolean   := (others => False);

      Kernels : LRk.Item_Set_List := LRk.LR0_Kernels (Grammar, First, Trace, First_State_Index);

      I             : LRk.Item_Set_Ptr  := Kernels.Head;
      Accept_Index  : Integer           := 0;
      Unused_Tokens : Boolean           := False;

      First_Production : OpenToken.Production.Instance renames
        Production_List.Get_Production (Production_List.Initial_Iterator (Grammar));

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

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

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Accept_Index, Used_Tokens, Trace);

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
         LRk.Put (Kernels);
      end if;

      New_Parser.Table := new Parse_Table
        (State_Index (First_State_Index) .. State_Index (Kernels.Size - 1 + First_State_Index));

      Add_Actions
        (Kernels, Accept_Index, Grammar, Has_Empty_Production, First, Unknown_Conflicts, New_Parser.Table.all, Trace);

      if Put_Grammar then
         Put_Parse_Table (New_Parser.Table.all, Kernels);
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

      LRk.Free (Kernels);
      if Unused_Tokens and not (Trace or Ignore_Unused_Tokens) then
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

   procedure Reduce_Stack
     (Stack            : in out State_Node_Ptr;
      Number_Of_Tokens : in     Natural;
      Production       : in     OpenToken.Production.Instance)
   is
      use type Nonterminal.Synthesize;

      Arguments    : Token_List.Instance;
      Popped_State : State_Node_Ptr;
      Args_Added   : Natural := 0;

      New_Token : constant Nonterminal.Handle := new Nonterminal.Class'(Production.LHS.all);
   begin
      --  Pop the indicated number of token states from the stack, and
      --  call the production action routine to create a new
      --  nonterminal token.
      --
      --  Leave Stack.State containing post-reduce state and produced
      --  token (after action call).

      --  Build the argument list, while popping all but the last
      --  argument's state off of the stack.
      if Number_Of_Tokens > 0 then
         loop
            Token_List.Enqueue (Arguments, Stack.Seen_Token);

            Args_Added := Args_Added + 1;
            exit when Args_Added = Number_Of_Tokens;

            --  Leave the state containing the first token in
            --  Production on Stack; overwrite it with the produced
            --  token
            Popped_State := Stack;
            Stack        := Stack.Next;
            Free (Popped_State);
         end loop;
         Stack.State      := Stack.Next.State;
         Stack.Seen_Token := Token.Handle (New_Token);
      else
         --  Empty production; push a new item on the stack.
         Stack := new State_Node'
           (State      => Stack.State,
            Seen_Token => Token.Handle (New_Token),
            Next       => Stack);
      end if;

      Production.RHS.Action (New_Token.all, Arguments, Token.ID (Production.LHS.all));
      Token_List.Clean (Arguments);

   end Reduce_Stack;

   overriding procedure Parse (Parser : in out Instance)
   is
      Stack        : State_Node_Ptr := new State_Node; -- Stack is current state, stack.next is prev state, etc.
      Seen_Token   : Token.Handle;
      Action       : Parse_Action_Rec;
      Popped_State : State_Node_Ptr;

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

      Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

      Stack.State := State_Index'First;
      loop
         Action := Action_For
           (Table => Parser.Table,
            State => Stack.State,
            ID    => Token.ID (Seen_Token.all));

         if Trace_Parse then
            declare
               use type Token.Handle;
               Stack_I : State_Node_Ptr := Stack;
            begin
               for I in 1 .. 10 loop
                  exit when Stack_I = null;
                  Ada.Text_IO.Put_Line
                    (State_Index'Image (Stack_I.State) &
                       " : " &
                       (if Stack_I.Seen_Token = null then ""
                         else Token.Token_Image (Token.ID (Stack_I.Seen_Token.all))));
                  Stack_I := Stack_I.Next;
               end loop;
            end;
            Ada.Text_IO.Put
              (Token.Token_Image (Token.ID (Seen_Token.all)) & " : " & Parse_Action_Verbs'Image (Action.Verb));
         end if;

         case Action.Verb is
         when Shift =>
            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            Stack := new State_Node'
              (State      => Action.State,
               Seen_Token => Seen_Token,
               Next       => Stack);

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

            Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

         when Reduce =>

            Reduce_Stack
              (Stack            => Stack,
               Number_Of_Tokens => Action.Length,
               Production       => Action.Production);

            Stack.State := Goto_For
              (Table => Parser.Table,
               State => Stack.State,
               ID    => Token.ID (Action.Production.LHS.all));

            if Trace_Parse then
               Ada.Text_IO.Put_Line
                 (" to " & Token.Token_Image (Token.ID (Action.Production.LHS.all)) &
                    ", goto state" & State_Index'Image (Stack.State));
            end if;

         when Accept_It         =>

            Reduce_Stack
              (Stack            => Stack,
               Number_Of_Tokens => Action.Length,
               Production       => Action.Production);

            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Clean up
            Free (Stack.Seen_Token);
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
               ID     : constant String := Token.Name (Seen_Token.all);
               Lexeme : constant String := Tokenizer.Lexeme (Parser.Analyzer);

               Expecting_Tokens : constant Token_Array := Expecting (Parser.Table, Stack.State);
            begin

               Free (Stack.Seen_Token);
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
      Put_Parse_Table (Parser.Table.all, (null, 0));
   end Put_Table;

end OpenToken.Production.Parser.LALR;
