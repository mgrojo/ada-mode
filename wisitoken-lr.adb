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

   function To_Vector (Item : in Production_ID_Array) return Production_ID_Arrays.Vector
   is begin
      return Result : Production_ID_Arrays.Vector do
         for I of Item loop
            Result.Append (I);
         end loop;
      end return;
   end To_Vector;

   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor'Class)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
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

   function Prod_ID (List : in Goto_Node_Ptr) return Positive
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
           Image (Item.LHS, Descriptor) & "," & Image (Item.Productions) & ")";
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
            return Left.LHS = Right.LHS and Left.Token_Count = Right.Token_Count;

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
              Image (Item.LHS, Trace.Descriptor.all));
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
      use all type Token_ID;
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
      Production      : in     Positive;
      LHS_ID          : in     Token_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Name_Index      : in     Natural;
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
         Action := (Reduce, +Production, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Name_Index);
      when Accept_It =>
         Action := (Accept_It, +Production, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Name_Index);
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

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Shift_Productions : in     Production_ID_Array;
      Symbol            : in     Token_ID;
      State_Index       : in     WisiToken.State_Index;
      Reduce_Production : in     Positive;
      LHS_ID            : in     Token_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type;
      Name_Index        : in     Natural;
      Semantic_Action   : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check    : in     Semantic_Checks.Semantic_Check)
   is
      use Production_ID_Arrays;
      Action_1 : constant Parse_Action_Rec := (Shift, +Shift_Productions, State_Index);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, +Reduce_Production, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Name_Index);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     LR.Parse_Action_Verbs;
      Production_1      : in     Positive;
      LHS_ID_1          : in     Token_ID;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Name_Index_1      : in     Natural;
      Semantic_Action_1 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_1  : in     Semantic_Checks.Semantic_Check;
      Production_2      : in     Positive;
      LHS_ID_2          : in     Token_ID;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Name_Index_2      : in     Natural;
      Semantic_Action_2 : in     Syntax_Trees.Semantic_Action;
      Semantic_Check_2  : in     Semantic_Checks.Semantic_Check)
   is
      use Production_ID_Arrays;
      Action_1 : constant Parse_Action_Rec   :=
        (case Verb is
         when Reduce    =>
           (Reduce, +Production_1, LHS_ID_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1, Name_Index_1),
         when Accept_It =>
           (Accept_It, +Production_1, LHS_ID_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1, Name_Index_1),
         when others => raise WisiToken.Programmer_Error);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, +Production_2, LHS_ID_2, Semantic_Action_2, Semantic_Check_2, RHS_Token_Count_2, Name_Index_2);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Error (State  : in out LR.Parse_State)
   is
      Action : constant Parse_Action_Rec := (Verb => Error, Productions => Production_ID_Arrays.Empty_Vector);
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
      Production : in     Positive;
      Symbol     : in     Token_ID;
      To_State   : in     State_Index)
   is
      use all type Token_ID;
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
     (Prod : in out Production;
      LHS  : in     Token_ID;
      RHS  : in     Token_ID_Array)
   is begin
      Prod.LHS := LHS;
      Prod.RHS.Set_Length (RHS'Length);
      for I in RHS'Range loop
         Prod.RHS (I) := RHS (I);
      end loop;
   end Set_Production;

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr
   is
      use type Token_ID;
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
      use type Token_ID;
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
      use type Token_ID;
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
      --  In the absence of conflicts, there is only one reduce action in each state.

      --  FIXME: if this is useful, cache shift, reduce counts in table.
      --  FIXME: also compress reduce?
   begin
      Shift_Count := 0;
      loop
         exit when Is_Done (Iter);

         if Iter.Item.Item.Verb = Reduce then
            if Action.LHS = Invalid_Token_ID then
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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec)
   is
      use Ada.Containers;
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));
         Put (" " & Image (Item.Productions, Strict => False));
      when Reduce =>
         Put
           ("reduce" & Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Item.LHS, Descriptor));
         Put (" " & Image (Item.Productions, Strict => False));
      when Accept_It =>
         Put ("accept it");
         Put (" " & Image (Item.Productions, Strict => False));
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
           ("  " & Image (Goto_Ptr.Production, Width => 4) & ": " & Image (Goto_Ptr.Symbol, Descriptor) &
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

   function Next_Grammar_Token
     (Terminals        : in out          Base_Token_Arrays.Vector;
      Errors           : in out          WisiToken.Lexer.Error_Lists.List;
      Line_Begin_Token : in out          Line_Begin_Token_Vectors.Vector;
      Descriptor       : in              WisiToken.Descriptor'Class;
      Lexer            : not null access WisiToken.Lexer.Instance'Class;
      User_Data        : in              WisiToken.Syntax_Trees.User_Data_Access)
     return Token_Index
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.User_Data_Access;

      Token : Base_Token;
      Error : Boolean;
   begin
      loop
         Error := Lexer.Find_Next (Token, Errors);

         if User_Data /= null then
            User_Data.Lexer_To_Augmented (Token, Lexer);
         end if;

         exit when Token.ID >= Descriptor.First_Terminal;
      end loop;
      Terminals.Append (Token);

      if Lexer.First then
         Line_Begin_Token.Set_Length (Ada.Containers.Count_Type (Token.Line));
         Line_Begin_Token (Token.Line) := Terminals.Last_Index;
      end if;

      if Error then
         declare
            Error : WisiToken.Lexer.Error renames Errors.Reference (Errors.Last);
         begin
            if Error.Recover_Char (1) /= ASCII.NUL then
               Error.Recover_Token := Terminals.Last_Index;
            end if;
         end;
      end if;

      return Terminals.Last_Index;
   end Next_Grammar_Token;

end WisiToken.LR;
