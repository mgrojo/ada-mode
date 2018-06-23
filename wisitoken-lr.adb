--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017, 2018 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO;
package body WisiToken.LR is

   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor'Class)
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
      Put_Line ("Cost_Limit =>" & Integer'Image (Item.Cost_Limit));
      New_Line;
   end Put;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID
   is begin
      return List.Symbol;
   end Symbol;

   function Prod_ID (List : in Goto_Node_Ptr) return Production_ID
   is begin
      return List.Production;
   end Prod_ID;

   function State (List : in Goto_Node_Ptr) return State_Index
   is begin
      return List.State;
   end State;

   function Next (List : in Goto_Node_Ptr) return Goto_Node_Ptr
   is begin
      return List.Next;
   end Next;

   function First (State : in Parse_State) return Action_List_Iterator
   is begin
      return Iter : Action_List_Iterator := (Node => State.Action_List, Item => null) do
         loop
            exit when Iter.Node = null;
            Iter.Item := Iter.Node.Action;
            exit when Iter.Item /= null;
            Iter.Node := Iter.Node.Next;
         end loop;
      end return;
   end First;

   function Is_Done (Iter : in Action_List_Iterator) return Boolean
   is begin
      return Iter.Node = null;
   end Is_Done;

   procedure Next (Iter : in out Action_List_Iterator)
   is begin
      if Iter.Node = null then
         return;
      end if;

      if Iter.Item.Next = null then
         loop
            Iter.Node := Iter.Node.Next;
            exit when Iter.Node = null;
            Iter.Item := Iter.Node.Action;
            exit when Iter.Item /= null;
         end loop;
      else
         Iter.Item := Iter.Item.Next; -- a conflict
      end if;
   end Next;

   function Symbol (Iter : in Action_List_Iterator) return Token_ID
   is begin
      return Iter.Node.Symbol;
   end Symbol;

   function Action (Iter : in Action_List_Iterator) return Parse_Action_Rec
   is begin
      return Iter.Item.Item;
   end Action;

   function First (State : in Parse_State) return Goto_List_Iterator
   is begin
      return (Node => State.Goto_List);
   end First;

   function Is_Done (Iter : in Goto_List_Iterator) return Boolean
   is begin
      return Iter.Node = null;
   end Is_Done;

   procedure Next (Iter : in out Goto_List_Iterator)
   is begin
      if Iter.Node /= null then
         Iter.Node := Iter.Node.Next;
      end if;
   end Next;

   function Symbol (Iter : in Goto_List_Iterator) return Token_ID
   is begin
      return Iter.Node.Symbol;
   end Symbol;

   function State (Iter : in Goto_List_Iterator) return State_Index
   is begin
      return Iter.Node.State;
   end State;

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor'Class) return String
   is
      use Ada.Containers;
   begin
      case Item.Verb is
      when Shift =>
         return "(Shift," & State_Index'Image (Item.State) & "," & Image (Item.Productions) & ")";

      when Reduce =>
         return "(Reduce," & Count_Type'Image (Item.Token_Count) & ", " &
           Image (Item.Production.Nonterm, Descriptor) & "," & Trimmed_Image (Item.Production.RHS) & ")";
      when Accept_It =>
         return "(Accept It)";
      when Error =>
         return "(Error)";
      end case;
   end Image;

   function Equal (Left, Right : in Parse_Action_Rec) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Left.Verb = Right.Verb then
         case Left.Verb is
         when Shift =>
            return Left.State = Right.State;

         when Reduce | Accept_It =>
            return Left.Production.Nonterm = Right.Production.Nonterm and Left.Token_Count = Right.Token_Count;

         when Error =>
            return True;
         end case;
      else
         return False;
      end if;
   end Equal;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec)
   is
      use Ada.Containers;
   begin
      case Item.Verb is
      when Shift =>
         Trace.Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Trace.Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.Production.Nonterm, Trace.Descriptor.all));
      when Accept_It =>
         Trace.Put ("accept it");
      when Error =>
         Trace.Put ("ERROR");
      end case;
   end Put;

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec)
   is
      New_Item : constant Action_Node_Ptr := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      I        : Action_Node_Ptr          := List;
   begin
      if I = null then
         List := New_Item;
      else
         if List.Symbol > Symbol then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or else I.Next.Symbol > Symbol;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add;

   procedure Add_Action
     (State       : in out LR.Parse_State;
      Productions : in     Production_ID_Array;
      Symbol      : in     Token_ID;
      State_Index : in     WisiToken.State_Index)
   is
      Action   : constant Parse_Action_Rec := (Shift, +Productions, State_Index);
      New_Node : constant Action_Node_Ptr  := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      Node     : Action_Node_Ptr;
   begin
      if State.Action_List = null then
         State.Action_List := New_Node;
      else
         Node := State.Action_List;
         loop
            exit when Node.Next = null;
            Node := Node.Next;
         end loop;
         Node.Next := New_Node;
      end if;
   end Add_Action;

   procedure Add_Action
     (State           : in out LR.Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     LR.Parse_Action_Verbs;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     Semantic_Checks.Semantic_Check)
   is
      use Production_ID_Arrays;
      Action   : Parse_Action_Rec;
      New_Node : Action_Node_Ptr;
      Node     : Action_Node_Ptr;
   begin
      case Verb is
      when Reduce =>
         Action := (Reduce, Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
      when Accept_It =>
         Action := (Accept_It, Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
      when others =>
         null;
      end case;
      New_Node := new Action_Node'(Symbol, new Parse_Action_Node'(Action, null), null);
      if State.Action_List = null then
         State.Action_List := New_Node;
      else
         Node := State.Action_List;
         loop
            exit when Node.Next = null;
            Node := Node.Next;
         end loop;
         Node.Next := New_Node;
      end if;
   end Add_Action;

   function Duplicate_Reduce (State : in Parse_State) return Boolean
   is
      Node        : Action_Node_Ptr       := State.Action_List;
      Action_Node : Parse_Action_Node_Ptr := Node.Action;
      First       : Boolean               := True;
      Action      : Reduce_Action_Rec;
   begin
      loop
         Action_Node := Node.Action;
         if Action_Node.Next /= null then
            --  conflict
            return False;
         elsif Action_Node.Item.Verb /= Reduce then
            return False;
         end if;

         if First then
            Action := Action_Node.Item;
            First  := False;
         else
            if not Equal (Action, Action_Node.Item) then
               return False;
            end if;
         end if;
         Node := Node.Next;
         exit when Node.Next = null; --  Last entry is Error.
      end loop;
      return True;
   end Duplicate_Reduce;

   function Actions_Length (State : in Parse_State) return Integer
   is
      Node : Action_Node_Ptr := State.Action_List;
   begin
      return Result : Integer := 0
      do
         loop
            exit when Node = null;
            Result := Result + 1;
            Node := Node.Next;
            exit when Node.Next = null; -- don't count Error
         end loop;
      end return;
   end Actions_Length;

   function Symbols_Image (State : in Parse_State) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean          := False;
      Node       : Action_Node_Ptr  := State.Action_List;
   begin
      if Actions_Length (State) = 1 then
         return "(1 => " & Token_ID'Image (Node.Symbol) & ")";
      else
         Result := +"(";
         loop
            Result := Result &
              (if Need_Comma then ", " else "") &
              Trimmed_Image (Node.Symbol);
            Need_Comma := True;
            Node := Node.Next;
            exit when Node.Next = null; -- last is Error
         end loop;
         Result := Result & ")";
         return -Result;
      end if;
   end Symbols_Image;

   procedure Add_Action
     (State           : in out Parse_State;
      Symbols         : in     Token_ID_Array;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check)
   is begin
      --  We assume Duplicate_Reduce is True for this state; no
      --  conflicts, all the same action.
      for Symbol of Symbols loop
         Add_Action
           (State, Symbol, Reduce, Production, RHS_Token_Count,
            Semantic_Action, Semantic_Check);
      end loop;
      Add_Error (State);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Shift_Productions : in     Production_ID_Array;
      Symbol            : in     Token_ID;
      State_Index       : in     WisiToken.State_Index;
      Reduce_Production : in     Production_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type;
      Semantic_Action   : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check    : in     Semantic_Checks.Semantic_Check)
   is
      use Production_ID_Arrays;
      Action_1 : constant Parse_Action_Rec := (Shift, +Shift_Productions, State_Index);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, Reduce_Production, Semantic_Action, Semantic_Check, RHS_Token_Count);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     LR.Parse_Action_Verbs;
      Production_1      : in     Production_ID;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_1  : in     Semantic_Checks.Semantic_Check;
      Production_2      : in     Production_ID;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_2  : in     Semantic_Checks.Semantic_Check)
   is
      use Production_ID_Arrays;
      Action_1 : constant Parse_Action_Rec :=
        (case Verb is
         when Reduce    =>
           (Reduce, Production_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1),
         when Accept_It =>
           (Accept_It, Production_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1),
         when others => raise WisiToken.Programmer_Error);

      Action_2 : constant Parse_Action_Rec :=
        (Reduce, Production_2, Semantic_Action_2, Semantic_Check_2, RHS_Token_Count_2);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Error (State  : in out LR.Parse_State)
   is
      Action : constant Parse_Action_Rec := (Verb => Error);
      Node   : Action_Node_Ptr           := State.Action_List;
   begin
      if Node = null then
         raise Programmer_Error with "adding an error action to a parse table state before other actions.";
      end if;
      loop
         exit when Node.Next = null;
         Node := Node.Next;
      end loop;
      Node.Next := new Action_Node'(Invalid_Token_ID, new Parse_Action_Node'(Action, null), null);
   end Add_Error;

   procedure Add_Goto
     (State      : in out LR.Parse_State;
      Production : in     Production_ID;
      Symbol     : in     Token_ID;
      To_State   : in     State_Index)
   is
      List     : Goto_Node_Ptr renames State.Goto_List;
      New_Item : constant Goto_Node_Ptr := new Goto_Node'(Production, Symbol, To_State, null);
      I        : Goto_Node_Ptr := List;
   begin
      if I = null then
         List := New_Item;
      else
         if List.Symbol > Symbol then
            New_Item.Next := List;
            List          := New_Item;
         else
            if List.Next = null then
               List.Next := New_Item;
            else
               I := List;
               loop
                  exit when I.Next = null or List.Symbol > Symbol;
                  I := I.Next;
               end loop;
               New_Item.Next := I.Next;
               I.Next        := New_Item;
            end if;
         end if;
      end if;
   end Add_Goto;

   procedure Set_Token_Sequence
     (Vector : in out Token_ID_Arrays.Vector;
      Tokens : in     Token_ID_Array)
   is begin
      Vector.Set_Length (Tokens'Length);
      for I in Tokens'Range loop
         Vector (I) := Tokens (I);
      end loop;
   end Set_Token_Sequence;

   procedure Set_Production
     (Prod     : in out Productions.Instance;
      LHS      : in     Token_ID;
      RHS_Last : in     Natural)
   is begin
      Prod.LHS := LHS;
      Prod.RHSs.Set_First (0);
      Prod.RHSs.Set_Last (RHS_Last);
   end Set_Production;

   procedure Set_RHS
     (Prod      : in out Productions.Instance;
      RHS_Index : in     Natural;
      RHS       : in     Token_ID_Array)
   is begin
      if RHS'Length > 0 then
         Prod.RHSs (RHS_Index).Tokens.Set_First (1);
         Prod.RHSs (RHS_Index).Tokens.Set_Last (RHS'Length);
         for I in RHS'Range loop
            Prod.RHSs (RHS_Index).Tokens (I) := RHS (I);
         end loop;
      end if;
   end Set_RHS;

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr
   is
      Action_Node : Action_Node_Ptr := Table.States (State).Action_List;
   begin
      if Action_Node = null then
         raise Programmer_Error with "no actions for state" & Unknown_State_Index'Image (State);
      end if;

      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      return Action_Node.Action;
   end Action_For;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index
   is
      Goto_Node : constant Goto_Node_Ptr := Goto_For (Table, State, ID);
   begin
      if Goto_Node = null then
         --  We can only get here during error recovery.
         return Unknown_State;
      else
         return Goto_Node.State;
      end if;
   end Goto_For;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Goto_Node_Ptr
   is
      Goto_Node : Goto_Node_Ptr := Table.States (State).Goto_List;
   begin
      while Goto_Node /= null and then Goto_Node.Symbol /= ID loop
         Goto_Node := Goto_Node.Next;
      end loop;

      return Goto_Node;
   end Goto_For;

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set
   is
      Result : Token_ID_Set    := (Table.First_Terminal .. Table.Last_Terminal => False);
      Action : Action_Node_Ptr := Table.States (State).Action_List;
   begin
      loop
         --  Last action is error; don't include it.
         exit when Action.Next = null;

         Result (Action.Symbol) := True;
         Action := Action.Next;
      end loop;
      return Result;
   end Expecting;

   function Reductions
     (Table       : in     Parse_Table;
      State       : in     State_Index;
      Shift_Count :    out Natural)
     return Reduce_Action_Array
   is
      Iter         : Action_List_Iterator := First (Table.States (State));
      Reduce_Count : Integer              := 0;
      Action       : Reduce_Action_Rec;
      --  In the absence of conflicts and reduce to empty nonterm, there is
      --  only one reduce action in each state.

      --  FIXME: if this is useful, cache shift, reduce counts in table.
      --  FIXME: also compress reduce?
   begin
      Shift_Count := 0;
      loop
         exit when Is_Done (Iter);

         if Iter.Item.Item.Verb = Reduce then
            if Action.Production.Nonterm = Invalid_Token_ID then
               Action       := Iter.Item.Item;
               Reduce_Count := 1;
            else
               if not Equal (Iter.Item.Item, Action) then
                  Reduce_Count := Reduce_Count + 1;
               end if;
            end if;
         elsif Iter.Item.Item.Verb = Shift then
            Shift_Count := Shift_Count + 1;
         end if;
         Next (Iter);
      end loop;

      if Reduce_Count = 0 then
         return (1 .. 0 => <>);
      elsif Reduce_Count = 1 then
         return (1 => Action);
      else
         declare
            Result : Reduce_Action_Array (1 .. Reduce_Count);
            Last   : Integer := 0;
            Found  : Boolean;
         begin
            Iter := First (Table.States (State));
            loop
               exit when Is_Done (Iter);

               if Iter.Item.Item.Verb = Reduce then
                  if Last = 0 then
                     Last := 1;
                     Result (Last) := Iter.Item.Item;
                  else
                     Found := False;
                     for I in 1 .. Last loop
                        if Equal (Result (I), Iter.Item.Item) then
                           Found := True;
                           exit;
                        end if;
                     end loop;
                     if not Found then
                        Last := Last + 1;
                        Result (Last) := Iter.Item.Item;
                     end if;
                  end if;
               end if;
               Next (Iter);
            end loop;
            return Result (1 .. Last);
         end;
      end if;
   end Reductions;

   procedure Free_Table (Table : in out Parse_Table_Ptr)
   is

      procedure Free is new Ada.Unchecked_Deallocation (Parse_Table, Parse_Table_Ptr);
      Action            : Action_Node_Ptr;
      Temp_Action       : Action_Node_Ptr;
      Parse_Action      : Parse_Action_Node_Ptr;
      Temp_Parse_Action : Parse_Action_Node_Ptr;
      Got               : Goto_Node_Ptr;
      Temp_Got          : Goto_Node_Ptr;
   begin
      if Table = null then
         return;
      end if;

      for State of Table.States loop
         Action := State.Action_List;
         loop
            exit when Action = null;
            Parse_Action := Action.Action;
            loop
               exit when Parse_Action = null;
               Temp_Parse_Action := Parse_Action;
               Parse_Action := Parse_Action.Next;
               Free (Temp_Parse_Action);
            end loop;

            Temp_Action := Action;
            Action := Action.Next;
            Free (Temp_Action);
         end loop;

         Got := State.Goto_List;
         loop
            exit when Got = null;
            Temp_Got := Got;
            Got := Got.Next;
            Free (Temp_Got);
         end loop;
      end loop;

      Free (Table);
   end Free_Table;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec)
   is
      use Ada.Containers;
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));
         Put (" " & Trimmed_Image (Item.Productions, Strict => False));
      when Reduce =>
         Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.Production.Nonterm, Descriptor));
         Put (" " & Trimmed_Image (Item.Production));
      when Accept_It =>
         Put ("accept it");
         Put (" " & Trimmed_Image (Item.Production));
      when Error =>
         Put ("ERROR");
      end case;
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Action : in Parse_Action_Node_Ptr)
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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; State : in Parse_State)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      Action_Ptr : Action_Node_Ptr := State.Action_List;
      Goto_Ptr   : Goto_Node_Ptr   := State.Goto_List;
   begin
      while Action_Ptr /= null loop
         Put ("   ");
         if Action_Ptr.Next = null then
            Put ("default" & (Descriptor.Image_Width - 7) * ' ' & " => ");

         elsif Action_Ptr.Action.Item.Verb /= Error then
            Put (Image (Action_Ptr.Symbol, Descriptor) &
                   (Descriptor.Image_Width - Image (Action_Ptr.Symbol, Descriptor)'Length) * ' '
                   & " => ");
         end if;
         Put (Descriptor, Action_Ptr.Action);
         New_Line;
         Action_Ptr := Action_Ptr.Next;
      end loop;

      New_Line;

      while Goto_Ptr /= null loop
         Put_Line
           ("  " & Padded_Image (Goto_Ptr.Production, Width => Prod_ID_Image_Width) & ": " &
              Image (Goto_Ptr.Symbol, Descriptor) &
              (Descriptor.Image_Width - Image (Goto_Ptr.Symbol, Descriptor)'Length) * ' ' &
              " goto state" & State_Index'Image (Goto_Ptr.State));
         Goto_Ptr := Goto_Ptr.Next;
      end loop;
   end Put;

   function None_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean
   is begin
      for O of reverse Ops loop
         exit when O.Op = Fast_Forward;
         if O.Op = Op then
            return False;
         end if;
      end loop;
      return True;
   end None_Since_FF;

   function Match_Since_FF (Ops : in Config_Op_Arrays.Vector; Op : in Config_Op) return Boolean
   is begin
      for O of reverse Ops loop
         exit when O.Op = Fast_Forward;
         if O = Op then
            return True;
         end if;
      end loop;
      return False;
   end Match_Since_FF;

   function Valid_Tree_Indices (Stack : in Recover_Stacks.Stack; Depth : in SAL.Base_Peek_Type) return Boolean
   is
      use all type Syntax_Trees.Node_Index;
   begin
      for I in 1 .. Depth loop
         if Stack (I).Tree_Index = Syntax_Trees.Invalid_Node_Index then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Tree_Indices;

   procedure Set_Key (Item : in out Configuration; Key : in Integer)
   is begin
      Item.Cost := Key;
   end Set_Key;

   function Image
     (Item       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      case Item.Label is
      when Action =>
         return "Action, expecting: " & Image (Item.Expecting, Descriptor) &
           ", found" & Tree.Image (Item.Error_Token, Descriptor);

      when Check =>
         return "Check, " & Semantic_Checks.Image (Item.Check_Status, Descriptor);
      end case;
   end Image;

end WisiToken.LR;
