--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017 Stephe Leake
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
package body WisiToken.Parser.LR is

   overriding
   function Image (Item : in Recover_Pattern_1) return String
   is begin
      return
        "WisiToken.Parser.LR.Recover_Pattern_1'(" &
        Int_Image (Integer (Item.Stack)) & "," &
        Token_ID'Image (Item.Error) & "," &
        Token_ID'Image (Item.Expecting) &
        ")";
   end Image;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in McKenzie_Param_Type)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      Put_Line ("(Insert =>");
      for I in Item.Insert'Range loop
         Put (" " & Image (Descriptor, I, Pad => True) & " =>" & Natural'Image (Item.Insert (I)));
         if I = Item.Insert'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("(Delete =>");
      for I in Item.Delete'Range loop
         Put (" " & Image (Descriptor, I, Pad => True) & " =>" & Natural'Image (Item.Delete (I)));
         if I = Item.Delete'Last then
            Put_Line (")");
         else
            Put_Line (",");
         end if;
      end loop;
      Put_Line ("Cost_Limit =>" & Integer'Image (Item.Cost_Limit));
      New_Line;
   end Put;

   function Extract_IDs (Stack : in Parser_Stacks.Stack_Type) return Token_Array
   is begin
      return Result : Token_Array do
         Result.Reserve_Capacity (Ada.Containers.Count_Type (Stack.Depth));
         for I in reverse 1 .. Stack.Depth loop
            Result.Append (Stack.Peek (I).ID);
         end loop;
      end return;
   end Extract_IDs;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Stack : in Parser_Stacks.Stack_Type)
   is
      use all type SAL.Base_Peek_Type;
      Last : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type'Min (10, Stack.Depth);
   begin
      for I in 1 .. Last loop
         Trace.Put
           (State_Index'Image (Stack.Peek (I).State) & " : " &
              (if I = Last
               then ""
               else Image (Trace.Descriptor.all, Stack.Peek (I).ID) & ", "));
      end loop;
      Trace.New_Line;
   end Put_Top_10;

   function State_Image (Item : in Unknown_State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Item = Unknown_State then
         return " ";
      else
         return Trim (State_Index'Image (Item), Both);
      end if;
   end State_Image;

   function Image
     (Descriptor : in WisiToken.Descriptor'Class;
      Stack      : in Parser_Stacks.Stack_Type;
      Depth      : in SAL.Base_Peek_Type := 0;
      Top_First  : in Boolean            := True)
     return String
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Strings.Unbounded;
      use Parser_Stack_Interfaces;

      Last : constant SAL.Base_Peek_Type :=
        (if Depth = 0
         then Stack.Depth
         else SAL.Base_Peek_Type'Min (Depth, Stack.Depth));

      Result : Unbounded_String;
   begin
      if Top_First then
         for I in 1 .. Last loop
            Result := Result &
              (State_Image (Stack.Peek (I).State) & " : " &
                 (if I = Stack.Depth
                  then ""
                  else Image (Descriptor, Stack.Peek (I).ID) & ", "));
         end loop;
      else
         for I in reverse 1 .. Last loop
            Result := Result &
              (State_Image (Stack.Peek (I).State) & " : " &
                 (if I = Stack.Depth
                  then ""
                  else Image (Descriptor, Stack.Peek (I).ID) & ", "));
         end loop;
      end if;
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

   function Image (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec) return String
   is
   begin
      case Item.Verb is
      when Shift =>
         return "(Shift," & State_Index'Image (Item.State) & ")";

      when Reduce =>
         return "(Reduce," & Ada.Containers.Count_Type'Image (Item.Token_Count) & ", " &
           Image (Descriptor, Item.LHS) & ")";
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
              Image (Trace.Descriptor.all, Item.LHS));
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
      State_Index : in     LR.State_Index)
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
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_Action)
   is
      Action   : Parse_Action_Rec;
      New_Node : Action_Node_Ptr;
      Node     : Action_Node_Ptr;
   begin
      case Verb is
      when Reduce =>
         Action := (Reduce, LHS_ID, Semantic_Action, Index, RHS_Token_Count);
      when Accept_It =>
         Action := (Accept_It, LHS_ID, Semantic_Action, Index, RHS_Token_Count);
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
      State_Index     : in     LR.State_Index;
      LHS_ID          : in     Token_ID;
      Index           : in     Integer;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Semantic_Action)
   is
      Action_1 : constant Parse_Action_Rec   := (Shift, State_Index);
      Action_2 : constant Parse_Action_Rec   := (Reduce, LHS_ID, Semantic_Action, Index, RHS_Token_Count);
   begin
      State.Action_List := new Action_Node'
        (Symbol, new Parse_Action_Node'(Action_1, new Parse_Action_Node'(Action_2, null)), State.Action_List);
   end Add_Action;

   procedure Add_Action
     (State             : in out LR.Parse_State;
      Symbol            : in     Token_ID;
      Verb              : in     LR.Parse_Action_Verbs;
      LHS_ID_1          : in     Token_ID;
      Index_1           : in     Integer;
      RHS_Token_Count_1 : in     Ada.Containers.Count_Type;
      Semantic_Action_1 : in     Semantic_Action;
      LHS_ID_2          : in     Token_ID;
      Index_2           : in     Integer;
      RHS_Token_Count_2 : in     Ada.Containers.Count_Type;
      Semantic_Action_2 : in     Semantic_Action)
   is
      Action_1 : constant Parse_Action_Rec   :=
        (case Verb is
         when Reduce    => (Reduce, LHS_ID_1, Semantic_Action_1, Index_1, RHS_Token_Count_1),
         when Accept_It => (Accept_It, LHS_ID_1, Semantic_Action_1, Index_1, RHS_Token_Count_1),
         when others => raise WisiToken.Programmer_Error);
      Action_2 : constant Parse_Action_Rec   := (Reduce, LHS_ID_2, Semantic_Action_2, Index_2, RHS_Token_Count_2);
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
      To_State : in     LR.State_Index)
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

   function Expecting
     (Descriptor : in WisiToken.Descriptor'Class;
      Table      : in Parse_Table;
      State      : in State_Index)
     return Token_ID_Set
   is
      Result : Token_ID_Set    := (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False);
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

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Item : in Parse_Action_Rec)
   is
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));
      when Reduce =>
         Put
           ("reduce" & Ada.Containers.Count_Type'Image (Item.Token_Count) & " tokens to " &
              Image (Descriptor, Item.LHS));
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
            Put (Image (Descriptor, Action_Ptr.Symbol) &
                   (Descriptor.Image_Width - Image (Descriptor, Action_Ptr.Symbol)'Length) * ' '
                   & " => ");
         end if;
         Put (Descriptor, Action_Ptr.Action);
         New_Line;
         Action_Ptr := Action_Ptr.Next;
      end loop;

      New_Line;

      while Goto_Ptr /= null loop
         Put_Line
           ("   " & Image (Descriptor, Goto_Ptr.Symbol) &
              (Descriptor.Image_Width - Image (Descriptor, Goto_Ptr.Symbol)'Length) * ' ' &
              " goto state" & State_Index'Image (Goto_Ptr.State));
         Goto_Ptr := Goto_Ptr.Next;
      end loop;
   end Put;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Table : in Parse_Table)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Follow:");
      Put (Descriptor, Table.Follow);

      if Table.McKenzie.Cost_Limit /= Default_McKenzie_Param.Cost_Limit then
         Put_Line ("McKenzie:");
         Put (Descriptor, Table.McKenzie);
         New_Line;
      end if;

      for State in Table.States'Range loop
         Put_Line ("State" & State_Index'Image (State) & ":");
         Put (Descriptor, Table.States (State));
         New_Line;
      end loop;
   end Put;

end WisiToken.Parser.LR;
