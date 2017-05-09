--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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
package body FastToken.Parser.LR is

   function State_Image (Item : in State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (State_Index'Image (Item), Both);
   end State_Image;

   function Symbol (List : in Goto_Node_Ptr) return Token.Token_ID
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

   procedure Add
     (List   : in out Action_Node_Ptr;
      Symbol : in     Token.Token_ID;
      Action : in     Parse_Action_Rec)
   is
      use all type Token.Token_ID;
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
      Symbol      : in     Token_Pkg.Token_ID;
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
      Symbol          : in     Token_Pkg.Token_ID;
      Verb            : in     LR.Parse_Action_Verbs;
      LHS_ID          : in     Token_Pkg.Token_ID;
      RHS_Token_Count : in     Natural;
      Synthesize      : in     Nonterminal.Synthesize)
   is
      Action   : Parse_Action_Rec;
      LHS      : constant Nonterminal.Handle := new Nonterminal.Class'(Nonterminal.Get (LHS_ID));
      New_Node : Action_Node_Ptr;
      Node     : Action_Node_Ptr;
   begin
      case Verb is
      when Reduce =>
         Action := (Reduce, LHS, Synthesize, 0, RHS_Token_Count);
      when Accept_It =>
         Action := (Accept_It, LHS, Synthesize, 0, RHS_Token_Count);
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
      Node.Next := new Action_Node'(Token.Terminal_ID'Last, new Parse_Action_Node'(Action, null), null);
   end Add_Error;

   procedure Add_Goto
     (State    : in out LR.Parse_State;
      Symbol   : in     Token_Pkg.Token_ID;
      To_State : in     LR.State_Index)
   is
      use all type Token.Token_ID;
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

   procedure Put (Item : in Parse_Action_Rec)
   is
      use Ada.Text_IO;
   begin
      case Item.Verb is
      when Shift =>
         Put ("shift and goto state" & State_Index'Image (Item.State));

      when Reduce =>
         Put
           ("reduce" & Integer'Image (Item.Token_Count) & " tokens to " & Token.Token_Image (Token.ID (Item.LHS.all)));
      when Accept_It =>
         Put ("accept it");
      when Error =>
         Put ("ERROR");
      end case;
   end Put;

   procedure Put (Action : in Parse_Action_Node_Ptr)
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
   end Put;

   procedure Put (State : in Parse_State)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      Action_Ptr : Action_Node_Ptr := State.Action_List;
      Goto_Ptr   : Goto_Node_Ptr   := State.Goto_List;
   begin
      while Action_Ptr /= null loop
         if Action_Ptr.Next = null then
            Put ("   default" & (Token_Image_Width - 7) * ' ' & " => ");
            Put (Action_Ptr.Action);
            New_Line;
         else
            Put ("   " & Token.Token_Image (Action_Ptr.Symbol) &
                   (Token_Image_Width - Token.Token_Image (Action_Ptr.Symbol)'Length) * ' '
                   & " => ");
            Put (Action_Ptr.Action);
            New_Line;
         end if;
         Action_Ptr := Action_Ptr.Next;
      end loop;

      New_Line;

      while Goto_Ptr /= null loop
         Put_Line
           ("   " & Token.Token_Image (Goto_Ptr.Symbol) &
              (Token_Image_Width - Token.Token_Image (Goto_Ptr.Symbol)'Length) * ' ' &
              " goto state" & State_Index'Image (Goto_Ptr.State));
         Goto_Ptr := Goto_Ptr.Next;
      end loop;
   end Put;

   procedure Put (Table : in Parse_Table)
   is
      use Ada.Text_IO;
   begin
      for State in Table'Range loop
         Put_Line ("State" & State_Index'Image (State) & ":");
         Put (Table (State));
         New_Line;
      end loop;
   end Put;

end FastToken.Parser.LR;
