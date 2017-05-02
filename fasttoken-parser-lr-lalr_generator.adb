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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with FastToken.Parser.LR.Generator_Utils;
package body FastToken.Parser.LR.LALR_Generator is

   package Utils is new FastToken.Parser.LR.Generator_Utils (Production, LR1_Items);
   use Utils;

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

   procedure Put (Propagations : Item_Item_List_Mapping_Ptr) is
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

   end Put;

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

   function LALR_Goto_Transitions
     (Kernel  : in LR1_Items.Item_Set;
      Symbol  : in Token.Token_ID;
      First   : in LR1_Items.Derivation_Matrix;
      Grammar : in Production.List.Instance;
      Trace   : in Boolean)
     return LR1_Items.Item_Set
   is
      use Token.List;
      use LR1_Items;
      use type Token.Handle;
      use type Token.Token_ID;

      Goto_Set : Item_Set;

      Item   : Item_Ptr := Kernel.Set;
      Dot_ID : Token.Token_ID;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop

         if Item.Dot /= Null_Iterator then

            Dot_ID := ID (Item.Dot);
            --  ID of token after Dot

            --  If Symbol = EOF_Token, this is the start symbol accept
            --  production; don't need a kernel with dot after EOF.
            if Dot_ID = Symbol and
              Symbol /= EOF_Token
            then
               Goto_Set.Set := new Item_Node'
                 (Prod       => Item.Prod,
                  Dot        => Next_Token (Item.Dot),
                  State      => Unknown_State, -- replaced in Kernels
                  Lookaheads => Item.Lookaheads,
                  Next       => Goto_Set.Set);

               if Trace then
                  Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Token.Token_Image (Symbol));
                  Put (Goto_Set, Show_Lookaheads => True, Include_Goto_List => True);
               end if;
            end if;

            if Dot_ID in Nonterminal_ID and then First (Dot_ID)(Symbol) then
               --  Find the production(s) that create Dot_ID
               --  with first token Symbol and put them in.
               --
               --  FIXME: this is _not_ [dragon] fix 4.38 closure; where did it come from?
               declare
                  Prod_I : Production.List.List_Iterator := Production.List.First (Grammar);
                  Prod   : Production.Instance;
                  RHS_I  : Token.List.List_Iterator;
               begin
                  while not Production.List.Is_Done (Prod_I) loop
                     Prod  := Production.List.Current (Prod_I);
                     RHS_I := Prod.RHS.Tokens.First;

                     if (Dot_ID = Nonterminal.ID (Prod.LHS) or First (Dot_ID)(Nonterminal.ID (Prod.LHS))) and
                       (RHS_I /= Null_Iterator and then ID (RHS_I) = Symbol)
                     then
                        declare
                           New_Item : constant Item_Node :=
                             (Prod       => Prod,
                              Dot        => Next_Token (RHS_I),
                              State      => Unknown_State, -- replaced in Kernels
                              Lookaheads => Null_Lookaheads,
                              Next       => Goto_Set.Set);
                        begin
                           if null = Find (New_Item, Goto_Set, Match_Lookaheads => False) then
                              Goto_Set.Set := new Item_Node'(New_Item);
                              --  else already in goto set

                              if Trace then
                                 Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Token.Token_Image (Symbol));
                                 Put (Goto_Set, Show_Lookaheads => True, Include_Goto_List => True);
                              end if;
                           end if;
                        end;
                     end if;

                     Production.List.Next (Prod_I);
                  end loop;
               end;
            end if;
         end if; -- item.dot /= null

         Item := Item.Next;
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LALR_Kernels
     (Grammar           : in Production.List.Instance;
      First             : in LR1_Items.Derivation_Matrix;
      Trace             : in Boolean;
      First_State_Index : in Unknown_State_Index)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;
      use type Token.List.List_Iterator;
      use type Token.Token_ID;

      Kernel_List : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => new Item_Node'
              (Item_Node_Of
                 (Production.List.Current (Production.List.First (Grammar)), First_State_Index)),
            Goto_List => null,
            State     => First_State_Index,
            Next      => null),
         Size         => 1);

      New_Items_To_Check   : Boolean      := True;
      Checking_Set         : Item_Set_Ptr;
      New_Items            : Item_Set;
      New_Items_Set        : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= null loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (Checking_Set.all);
            end if;

            for Symbol in Token.Token_ID loop

               New_Items := LALR_Goto_Transitions (Checking_Set.all, Symbol, First, Grammar, Trace);

               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, Kernel_List, Match_Lookaheads => False);

                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next  := Kernel_List.Head;
                     New_Items.State := Kernel_List.Size + First_State_Index;

                     declare
                        I : Item_Ptr := New_Items.Set;
                     begin
                        while I /= null loop
                           I.State := New_Items.State;
                           I       := I.Next;
                        end loop;
                     end;

                     if Trace then
                        Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (New_Items.State));
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Checking_Set.Goto_List := new Goto_Item'
                       (Set    => Kernel_List.Head,
                        Symbol => Symbol,
                        Next   => Checking_Set.Goto_List);

                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Symbol    => Symbol,
                        Set       => New_Items_Set,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put_Line
                             ("  adding goto on " & Token.Token_Image (Symbol) & " to state" &
                                Unknown_State_Index'Image (New_Items_Set.State));

                        end if;

                        Checking_Set.Goto_List := new Goto_Item'
                          (Set    => New_Items_Set,
                           Symbol => Symbol,
                           Next   => Checking_Set.Goto_List);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            Checking_Set := Checking_Set.Next;
         end loop;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Kernel_List;
   end LALR_Kernels;


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
            Lookaheads => LR1_Items.Null_Lookaheads,
            Next       => null);

         Goto_Set : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (Source_Set, Token_ID);

         Next_Kernel : constant LR1_Items.Item_Ptr :=
           (if Goto_Set = null then null
            else LR1_Items.Find (Next_Item, Goto_Set.all, Match_Lookaheads => False));
      begin
         begin
            Used_Tokens (Token_ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Token.Token_Image (Token_ID) & " used in grammar";
         end;

         if Closure_Item.Lookaheads.Propagate then
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To           => Next_Item,
               For_Token    => Token_ID,
               Propagations => Propagations);
         end if;

         if Next_Kernel /= null then
            if Trace then
               Spontaneous_Count := Spontaneous_Count + 1;
               Ada.Text_IO.Put_Line ("  spontaneous: " & LR1_Items.Image (Closure_Item.Lookaheads));
            end if;

            LR1_Items.Include (Next_Kernel.Lookaheads, Closure_Item.Lookaheads, Exclude_Propagate => True);
         end if;

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
      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Added_One     : Boolean;
      Added_Some    : Boolean := False;
   begin
      while More_To_Check loop

         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            To := Mapping.To;
            while To /= null loop
               LR1_Items.Include (To.Item.Lookaheads, Mapping.From.Lookaheads, Added_One, Exclude_Propagate => True);

               if Trace and Added_One then
                  Added_Some := True;
                  Ada.Text_IO.Put ("  to:");
                  LR1_Items.Put (To.Item.all, Show_Lookaheads => True);
                  Ada.Text_IO.New_Line;
               end if;

               More_To_Check := More_To_Check or Added_One;
               To := To.Next;
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

      Kernel_Item_Set.Set.Lookaheads := (Propagate => True, Tokens => (others => False));

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
         Put (Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace);

      Free (Propagation_List);
      LR1_Items.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

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
      use type LR1_Items.Item_Set_Ptr;

      Kernel : LR1_Items.Item_Set_Ptr := Kernels.Head;
      Closure : LR1_Items.Item_Set;
   begin
      while Kernel /= null loop
         Closure := LR1_Items.Closure
           (Kernel.all, Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);

         Add_Actions (Closure, Table, Has_Empty_Production, Conflicts, Trace);
         Kernel := Kernel.Next;

         LR1_Items.Free (Closure);
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

      Kernels : LR1_Items.Item_Set_List := LALR_Kernels
        (Grammar, First, Trace, Unknown_State_Index (First_State_Index));

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
         LR1_Items.Put (Kernels, Show_Lookaheads => True);
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
