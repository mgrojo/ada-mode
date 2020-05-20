--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019, 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);
package body WisiToken.Syntax_Trees.LR_Utils is

   procedure Raise_Programmer_Error
     (Label      : in String;
      Descriptor : in WisiToken.Descriptor;
      Lexer      : in WisiToken.Lexer.Handle;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Terminals  : in WisiToken.Base_Token_Arrays.Vector;
      Node       : in Node_Index)
   is
      Terminal_Index : constant Base_Token_Index := Tree.First_Shared_Terminal (Node);
   begin
      raise SAL.Programmer_Error with Error_Message
        (Lexer.File_Name,
         --  Not clear why we need Line + 1 here, to match Emacs.
         (if Terminal_Index = Invalid_Token_Index then 1 else Terminals (Terminal_Index).Line + 1), 0,
         Label & ":" &
           Tree.Image (Node, Descriptor, Include_Children => True, Include_RHS_Index => True, Node_Numbers => True));
   end Raise_Programmer_Error;

   function Has_Element (Cursor : in LR_Utils.Cursor) return Boolean is (Cursor.Node /= Invalid_Node_Index);

   function Node (Cursor : in LR_Utils.Cursor) return Node_Index is (Cursor.Node);

   overriding function First (Iter : Iterator) return Cursor
   is begin
      if Iter.Root = Invalid_Node_Index then
         return (Node => Invalid_Node_Index);
      else
         return Result : Cursor do
            Result.Node := Iter.Root;
            loop
               declare
                  Children : constant Valid_Node_Index_Array := Iter.Tree.Children (Result.Node);
               begin
                  --  An empty list looks like:
                  --
                  --  parent_2:
                  --  | list
                  --  | | list
                  --  | | | element: First
                  --  | | element: 2
                  --  | element: 3
                  --  element: Last
                  if Iter.Tree.ID (Children (1)) = Iter.List_ID then
                     Result.Node := Children (1);
                  elsif Iter.Tree.ID (Children (1)) = Iter.Element_ID then
                     Result.Node := Children (1);
                     exit;
                  else
                     Raise_Programmer_Error
                       ("lr_utils.first", Iter.Descriptor.all, Iter.Lexer, Iter.Tree.all,
                        Iter.Terminals.all, Result.Node);
                  end if;
               end;
            end loop;
         end return;
      end if;
   end First;

   overriding function Last  (Iter : Iterator) return Cursor
   is begin
      if Iter.Root = Invalid_Node_Index then
         return (Node => Invalid_Node_Index);
      else
         declare
            --  Tree is one of:
            --
            --  case a: single element list
            --  element_list : root
            --  | element: Last
            --
            --  case c: no next
            --  element_list: root
            --  | element_list
            --  | | element:
            --  | element: Last
            Children : constant Valid_Node_Index_Array := Iter.Tree.Children (Iter.Root);
         begin
            return (Node => Children (Children'Last));
         end;
      end if;
   end Last;

   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor
   is begin
      if Position.Node = Invalid_Node_Index then
         return Position;
      else
         return Result : Cursor do
            declare
               use all type SAL.Base_Peek_Type;
               --  Tree is one of:
               --
               --  case a: first element, no next
               --  rhs
               --  | rhs_item_list
               --  | | rhs_item: Element
               --  | action
               --
               --  case b: first element, next
               --  rhs_item_list
               --  | rhs_item_list
               --  | | rhs_item: Element
               --  | rhs_item: next element
               --
               --  case c: non-first element, no next
               --  rhs
               --  | rhs_item_list : Grand_Parent
               --  | | rhs_item_list
               --  | | | rhs_item:
               --  | | rhs_item: Element
               --  | action : Aunt
               --
               --  case d: non-first element, next
               --  rhs_item_list
               --  | rhs_item_list : Grand_Parent
               --  | | rhs_item_list
               --  | | | rhs_item:
               --  | | rhs_item: Element
               --  | rhs_item: next element : Aunt

               Grand_Parent    : constant Valid_Node_Index       := Iter.Tree.Parent (Position.Node, 2);
               Aunts           : constant Valid_Node_Index_Array := Iter.Tree.Children (Grand_Parent);
               Last_List_Child : SAL.Base_Peek_Type              := Aunts'First - 1;
            begin
               if Iter.Tree.ID (Grand_Parent) /= Iter.List_ID then
                  --  No next
                  Result.Node := Invalid_Node_Index;
               else
                  for I in Aunts'Range loop
                     if Iter.Tree.ID (Aunts (I)) in Iter.List_ID | Iter.Element_ID then
                        Last_List_Child := I;
                     end if;
                  end loop;

                  if Last_List_Child = 1 then
                     --  No next
                     Result.Node := Invalid_Node_Index;
                  else
                     Result.Node := Aunts (Last_List_Child);
                  end if;
               end if;
            end;
         end return;
      end if;
   end Next;

   overriding function Previous (Iter   : Iterator; Position : Cursor) return Cursor
   is begin
      if Position.Node = Invalid_Node_Index then
         return Position;
      else
         return Result : Cursor do
            --  Tree is one of:
            --
            --  case a: first element, no prev
            --  ?
            --  | rhs_item_list
            --  | | rhs_item: Element
            --
            --  case b: second element
            --  ?
            --  | rhs_item_list
            --  | | rhs_item: prev item
            --  | rhs_item: Element
            --
            --  case c: nth element
            --  ?
            --  | rhs_item_list
            --  | | rhs_item_list
            --  | | | rhs_item:
            --  | | rhs_item: prev element
            --  | rhs_item: Element
            declare
               Parent : constant Valid_Node_Index := Iter.Tree.Parent (Position.Node);
            begin
               if Position.Node = Iter.Tree.Child (Parent, 1) then
                  --  No prev
                  Result.Node := Invalid_Node_Index;

               else
                  declare
                     Prev_Children : constant Valid_Node_Index_Array := Iter.Tree.Children
                       (Iter.Tree.Child (Parent, 1));
                  begin
                     Result.Node := Prev_Children (Prev_Children'Last);
                  end;
               end if;
            end;
         end return;
      end if;
   end Previous;

   function Iterate
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Terminals    :         in     WisiToken.Base_Token_Array_Access;
      Lexer        :         in     WisiToken.Lexer.Handle;
      Descriptor   :         in     WisiToken.Descriptor_Access_Constant;
      Root         :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID := WisiToken.Invalid_Token_ID)
     return Iterator
   is
      pragma Unreferenced (List_ID); --  checked in precondition.
   begin
      return
        (Iterator_Interfaces.Reversible_Iterator with
         Tree'Access, Terminals, Lexer, Descriptor, Root,
         List_ID      => Tree.ID (Root),
         Element_ID   => Element_ID,
         Separator_ID => Separator_ID);
   end Iterate;

   function Invalid_Iterator (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return Iterator
   is begin
      return
        (Iterator_Interfaces.Reversible_Iterator with
         Tree         => Tree'Access,
         Terminals    => null,
         Lexer        => null,
         Descriptor   => null,
         Root         => Invalid_Node_Index,
         List_ID      => Invalid_Token_ID,
         Element_ID   => Invalid_Token_ID,
         Separator_ID => Invalid_Token_ID);
   end Invalid_Iterator;

   function Is_Invalid (Iter : in Iterator) return Boolean
   is begin
      return Iter.Root = Invalid_Node_Index;
   end Is_Invalid;

   function To_Cursor (Iter : in Iterator; Node : in Valid_Node_Index) return Cursor
   is begin
      if Iter.Root = Invalid_Node_Index then
         return (Node => Invalid_Node_Index);
      else
         pragma Assert (Iter.Tree.Is_Descendant_Of (Iter.Root, Node));
         pragma Assert (Iter.Tree.ID (Node) = Iter.Element_ID);

         return (Node => Node);
      end if;
   end To_Cursor;

   function Root (Iter : in Iterator) return Valid_Node_Index
   is begin
      return Iter.Root;
   end Root;

   function Count (Iter : Iterator) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Result : Count_Type := 0;
   begin
      for Item in Iter loop
         Result := Result + 1;
      end loop;
      return Result;
   end Count;

   function Copy_List
     (List  : in out Iterator;
      First : in     Cursor := No_Element;
      Last  : in     Cursor := No_Element)
     return Valid_Node_Index
   is begin
      return List.Tree.Copy_Subtree
        (Root => (if First = No_Element then List.Root else List.Tree.Parent (First.Node)),
         Last => (if Last = No_Element then List.First.Node else Last.Node));
   end Copy_List;

end WisiToken.Syntax_Trees.LR_Utils;
