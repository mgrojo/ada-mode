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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
package body WisiToken.LR is

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

   function Image
     (Stack      : in Parser_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor'Class;
      Tree       : in Syntax_Trees.Tree;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
   is
      use all type Syntax_Trees.Node_Index;
      use all type SAL.Base_Peek_Type;
      use Ada.Strings.Unbounded;

      Last : constant SAL.Base_Peek_Type :=
        (if Depth = 0
         then Stack.Depth
         else SAL.Base_Peek_Type'Min (Depth, Stack.Depth));

      Result : Unbounded_String := +"(";
   begin
      for I in 1 .. Last loop
         Result := Result &
           (Image (Stack.Peek (I).State) & " : " &
              (if I = Stack.Depth
               then ""
               else
                 (if Stack.Peek (I).Token = Syntax_Trees.Invalid_Node_Index -- From recover fast-forward
                  then ""
                  else Tree.Image (Stack.Peek (I).Token, Descriptor) & ", ")));
      end loop;
      return To_String (Result & ")");
   end Image;

   function Symbol (List : in Goto_Node_Ptr) return Token_ID
   is begin
      return List.Symbol;
   end Symbol;

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
         return "(Shift," & State_Index'Image (Item.State) & ")";

      when Reduce =>
         return "(Reduce," & Count_Type'Image (Item.Token_Count) & ", " &
           Image (Item.LHS, Descriptor) & ")";
      when Accept_It =>
         return "(Accept It)";
      when Error =>
         return "(Error)";
      end case;
   end Image;

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
      Symbol      : in     Token_ID;
      State_Index : in     WisiToken.State_Index)
   is
      Action   : constant Parse_Action_Rec := (Shift, State_Index);
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
      Action   : Parse_Action_Rec;
      New_Node : Action_Node_Ptr;
      Node     : Action_Node_Ptr;
   begin
      case Verb is
      when Reduce =>
         Action := (Reduce, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Production, Name_Index);
      when Accept_It =>
         Action := (Accept_It, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Production, Name_Index);
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
     (State           : in out LR.Parse_State;
      Symbol          : in     Token_ID;
      State_Index     : in     WisiToken.State_Index;
      Production      : in     Positive;
      LHS_ID          : in     Token_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Name_Index      : in     Natural;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     Semantic_Checks.Semantic_Check)
   is
      Action_1 : constant Parse_Action_Rec := (Shift, State_Index);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, LHS_ID, Semantic_Action, Semantic_Check, RHS_Token_Count, Production, Name_Index);
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
      Action_1 : constant Parse_Action_Rec   :=
        (case Verb is
         when Reduce    =>
           (Reduce, LHS_ID_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1, Production_1, Name_Index_1),
         when Accept_It =>
           (Accept_It, LHS_ID_1, Semantic_Action_1, Semantic_Check_1, RHS_Token_Count_1, Production_1, Name_Index_1),
         when others => raise WisiToken.Programmer_Error);
      Action_2 : constant Parse_Action_Rec :=
        (Reduce, LHS_ID_2, Semantic_Action_2, Semantic_Check_2, RHS_Token_Count_2, Production_2, Name_Index_2);
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
     (State    : in out LR.Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     State_Index)
   is
      use all type Token_ID;
      List     : Goto_Node_Ptr renames State.Goto_List;
      New_Item : constant Goto_Node_Ptr := new Goto_Node'(Symbol, To_State, null);
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
      Goto_Node : Goto_Node_Ptr := Table.States (State).Goto_List;
   begin
      while Goto_Node /= null and then Goto_Node.Symbol /= ID loop
         Goto_Node := Goto_Node.Next;
      end loop;

      if Goto_Node = null then
         --  We can only get here during error recovery.
         return Unknown_State;
      else
         return Goto_Node.State;
      end if;
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

   function Minimal_Terminal_Sequence
     (Table   : in Parse_Table;
      Nonterm : in Token_ID)
     return Token_ID_Arrays.Vector
   is begin
      return Table.Terminal_Sequences (Nonterm);
   end Minimal_Terminal_Sequence;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec)
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
              Image (Item.LHS, Descriptor));
      when Accept_It =>
         Put ("accept it");
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
           ("   " & Image (Goto_Ptr.Symbol, Descriptor) &
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
           ", found '" & Tree.Image (Item.Error_Token, Descriptor);

      when Check =>
         return "Check: " & Semantic_Checks.Image (Item.Check_Status, Descriptor);
      end case;
   end Image;

   procedure Put
     (Source_File_Name : in String;
      Errors           : in Parse_Error_Lists.List;
      Tree             : in Syntax_Trees.Tree;
      Descriptor       : in WisiToken.Descriptor)
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
   begin
      if Errors.Length = 0 then
         return;
      end if;

      Put_Line ("parser errors:");
      for Item of Errors loop
         case Item.Label is
         when Action =>
            Put_Line
              (Source_File_Name & ": syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                 ", found '" & Tree.Image (Item.Error_Token, Descriptor) & "'");

         when Check =>
            Put_Line
              (Source_File_Name & ": semantic check error: " & Semantic_Checks.Image (Item.Check_Status, Descriptor));
         end case;

         if Item.Recover.Stack.Depth /= 0 then
            Put_Line ("   recovered: " & Image (Item.Recover.Ops, Descriptor));
         end if;
      end loop;
      New_Line;
   end Put;

   function Next_Grammar_Token
     (Terminals      : in out          Base_Token_Arrays.Vector;
      Lexer          : not null access WisiToken.Lexer.Instance'Class;
      Semantic_State : in out          WisiToken.Semantic_State.Semantic_State;
      Descriptor     : in              WisiToken.Descriptor'Class)
     return Token_Index
   is
      Token : Base_Token;
   begin
      loop
         Token.ID := Lexer.Find_Next;
         Token.Byte_Region := Lexer.Byte_Region;

         Semantic_State.Lexer_To_Augmented (Descriptor, Token, Lexer);

         exit when Token.ID >= Descriptor.First_Terminal;
      end loop;
      Terminals.Append (Token);
      return Terminals.Last_Index;
   end Next_Grammar_Token;

   function Reduce_Stack
     (Stack           : in out Parser_Stacks.Stack;
      Tree            : in out Syntax_Trees.Tree;
      Action          : in     Reduce_Action_Rec;
      Nonterm         :    out Syntax_Trees.Valid_Node_Index;
      Lexer           : in     WisiToken.Lexer.Handle;
      Trace           : in out WisiToken.Trace'Class;
      Trace_Level     : in     Integer;
      Trace_Prefix    : in     String := "";
      Default_Virtual : in     Boolean)
     return Semantic_Checks.Check_Status
   is
      use all type Semantic_Checks.Semantic_Check;
      use all type Semantic_Checks.Check_Status_Label;

      Children_Tree : Syntax_Trees.Valid_Node_Index_Array (1 .. SAL.Base_Peek_Type (Action.Token_Count));
      --  for Set_Children.

      --  FIXME: move back to -lr-parser.adb
   begin
      for I in reverse Children_Tree'Range loop
         Children_Tree (I) := Stack.Pop.Token;
      end loop;

      Nonterm := Tree.Add_Nonterm
        (Action.LHS, Action.Action, Action.Production, Action.Name_Index, Children_Tree, Default_Virtual);
      --  Computes Nonterm.Byte_Region, Virtual

      if Trace_Level > Detail then
         declare
            Action_Name : constant String := Ada.Characters.Handling.To_Lower
              (Image (Action.LHS, Trace.Descriptor.all)) & "_" & Int_Image (Action.Name_Index);
         begin
            Trace.Put_Line
              (Trace_Prefix & Action_Name & ": " &
                 Tree.Image (Nonterm, Trace.Descriptor.all) & " <= " &
                 Tree.Image (Children_Tree, Trace.Descriptor.all));
         end;
      end if;

      if Action.Check = null then
         return (Label => Ok);

      else
         declare
            Nonterm_Token  : Recover_Token := Tree.Recover_Token (Nonterm);
            Children_Token : constant Recover_Token_Array := Tree.Recover_Token_Array (Children_Tree);
         begin
            return Status : constant Semantic_Checks.Check_Status := Action.Check
              (Lexer, Nonterm_Token, Children_Token)
            do
               Tree.Set_Name_Region (Nonterm, Nonterm_Token.Name);

               if Trace_Level > Detail then
                  Trace.Put_Line
                    (Trace_Prefix & "semantic check " & Semantic_Checks.Image (Status, Trace.Descriptor.all));
               end if;
            end return;
         end;
      end if;
   end Reduce_Stack;

end WisiToken.LR;
