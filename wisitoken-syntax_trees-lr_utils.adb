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
         Label & ": " &
           Tree.Image (Node, Descriptor, Include_Children => True, Include_RHS_Index => True, Node_Numbers => True));
   end Raise_Programmer_Error;

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
                  if Iter.Tree.ID (Children (1)) = Iter.List_ID then
                     Result.Node := Children (1);
                  elsif Iter.Tree.ID (Children (1)) = Iter.Element_ID then
                     Result.Node := Children (1);
                     exit;
                  else
                     raise SAL.Programmer_Error;
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
         return (Node => Iter.Tree.Child (Iter.Root, SAL.Base_Peek_Type (Iter.Tree.Child_Count (Iter.Root))));
      end if;
   end Last;

   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor
   is begin
      if Position.Node = Invalid_Node_Index then
         return Position;
      else
         return Result : Cursor do
            declare
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

               Grand_Parent : constant Node_Index := Iter.Tree.Parent (Position.Node, 2);

               Aunts           : constant Valid_Node_Index_Array :=
                 (if Grand_Parent = Invalid_Node_Index
                  then (1 .. 0 => Invalid_Node_Index)
                  else Iter.Tree.Children (Grand_Parent));

               Last_List_Child : SAL.Base_Peek_Type := Aunts'First - 1;
            begin
               if Grand_Parent = Invalid_Node_Index or else Iter.Tree.ID (Grand_Parent) /= Iter.List_ID then
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
      Root         :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return Iterator
   is
      pragma Unreferenced (List_ID); --  checked in precondition.

      Multi_Element_RHS : constant Natural :=
        (if Tree.Child_Count (Root) = 1
         then (if Tree.RHS_Index (Root) = 0 then 1 else 0)
         elsif Tree.Child_Count (Root) in 2 .. 3 --  3 if there is a separator
         then Tree.RHS_Index (Root)
         else raise SAL.Programmer_Error);
   begin
      return
        (Iterator_Interfaces.Reversible_Iterator with
         Tree'Access, Root,
         List_ID           => Tree.ID (Root),
         One_Element_RHS   => (if Multi_Element_RHS = 0 then 1 else 0),
         Multi_Element_RHS => Multi_Element_RHS,
         Element_ID        => Element_ID,
         Separator_ID      => Separator_ID);
   end Iterate;

   function Iterate_Constant
     (Tree       : aliased in out WisiToken.Syntax_Trees.Tree;
      Root       :         in     Valid_Node_Index;
      List_ID    :         in     WisiToken.Token_ID;
      Element_ID :         in     WisiToken.Token_ID)
     return Iterator
   is begin
      return Iterate (Tree, Root, List_ID, Element_ID, Invalid_Token_ID);
   end Iterate_Constant;

   function Iterate (Iter : in Iterator; Root : in Valid_Node_Index) return Iterator
   is begin
      return Iterate (Iter.Tree.all, Root, Iter.List_ID, Iter.Element_ID, Iter.Separator_ID);
   end Iterate;

   function Iterate_From_Element
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      Element      :         in     Valid_Node_Index;
      List_ID      :         in     WisiToken.Token_ID;
      Element_ID   :         in     WisiToken.Token_ID;
      Separator_ID :         in     WisiToken.Token_ID)
     return Iterator
   is
      Root : Valid_Node_Index := Tree.Parent (Element);
   begin
      loop
         exit when Tree.Parent (Root) = Invalid_Node_Index or else Tree.ID (Tree.Parent (Root)) /= List_ID;
         Root := Tree.Parent (Root);
      end loop;
      return Iterate (Tree, Root, List_ID, Element_ID, Separator_ID);
   end Iterate_From_Element;

   function Iterate_From_Element (Iter : in Iterator; Element : in Valid_Node_Index) return Iterator
   is begin
      return Iterate_From_Element (Iter.Tree.all, Element, Iter.List_ID, Iter.Element_ID, Iter.Separator_ID);
   end Iterate_From_Element;

   function Invalid_Iterator (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return Iterator
   is begin
      return
        (Iterator_Interfaces.Reversible_Iterator with
         Tree              => Tree'Access,
         Root              => Invalid_Node_Index,
         List_ID           => Invalid_Token_ID,
         One_Element_RHS   => 0,
         Multi_Element_RHS => 0,
         Element_ID        => Invalid_Token_ID,
         Separator_ID      => Invalid_Token_ID);
   end Invalid_Iterator;

   function Empty_Iterator
     (Tree              : aliased in out WisiToken.Syntax_Trees.Tree;
      List_ID           :         in     WisiToken.Token_ID;
      Multi_Element_RHS :         in     Natural;
      Element_ID        :         in     WisiToken.Token_ID;
      Separator_ID      :         in     WisiToken.Token_ID)
     return Iterator
   is begin
      return
        (Iterator_Interfaces.Reversible_Iterator with
         Tree'Access,
         Root              => Invalid_Node_Index,
         List_ID           => List_ID,
         One_Element_RHS   => (if Multi_Element_RHS = 0 then 1 else 0),
         Multi_Element_RHS => Multi_Element_RHS,
         Element_ID        => Element_ID,
         Separator_ID      => Separator_ID);
   end Empty_Iterator;

   function Empty_Iterator (Iter : in Iterator) return Iterator
   is begin
      return Empty_Iterator (Iter.Tree.all, Iter.List_ID, Iter.Multi_Element_RHS, Iter.Element_ID, Iter.Separator_ID);
   end Empty_Iterator;

   function To_Cursor (Iter : in Iterator; Node : in Valid_Node_Index) return Cursor
   is begin
      return (Node => Node);
   end To_Cursor;

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

   procedure Append
     (Iter        : in out Iterator;
      New_Element : in     Valid_Node_Index)
   is
      Tree : Syntax_Trees.Tree renames Iter.Tree.all;
   begin
      if Iter.Root = Invalid_Node_Index then
         --  First item in Dest
         Iter :=
           (Iter.Tree,
            List_ID           => Iter.List_ID,
            One_Element_RHS   => Iter.One_Element_RHS,
            Multi_Element_RHS => Iter.Multi_Element_RHS,
            Element_ID        => Iter.Element_ID,
            Separator_ID      => Iter.Separator_ID,
            Root              => Tree.Add_Nonterm
              (Production     => (Iter.List_ID, Iter.One_Element_RHS),
               Children       => (1 => New_Element)));

      else
         --  Adding element Last in spec example
         declare
            List_Parent : constant Node_Index       := Tree.Parent (Iter.Root);
            Old_Root    : constant Valid_Node_Index := Iter.Root;
         begin
            Iter.Root :=
              Tree.Add_Nonterm
                (Production     => (Iter.List_ID, Iter.Multi_Element_RHS),
                 Children       =>
                   (if Iter.Separator_ID = Invalid_Token_ID
                    then (Iter.Root, New_Element)
                    else (Iter.Root, Tree.Add_Terminal (Iter.Separator_ID), New_Element)));

            if List_Parent /= Invalid_Node_Index and then
              (Tree.ID (List_Parent) /= Invalid_Token_ID and
                 Tree.Label (List_Parent) = Nonterm)
            then
               Tree.Replace_Child
                 (List_Parent,
                  Old_Child            => Old_Root,
                  New_Child            => Iter.Root,
                  Old_Child_New_Parent => Iter.Root);
            end if;
         end;
      end if;
   end Append;

   procedure Insert
     (Iter        : in out Iterator;
      New_Element : in     Valid_Node_Index;
      After       : in     Cursor)
   is
      --  Current Tree (see wisitoken_syntax_trees-test.adb Test_Insert_1):
      --
      --  list: Tree.Root
      --  | list = Parent
      --  | | list
      --  | | | list
      --  | | | | element: 1 = First
      --  | | | separator
      --  | | | element: 2 = After
      --  | | separator
      --  | | element: 3 = Before
      --  | separator
      --  | element: 4 = Last

      --  Insert New_Element after 2:
      --
      --  list: Tree.Root
      --  | list
      --  | | list = Parent
      --  | | | list: new_list_nonterm
      --  | | | | list
      --  | | | | | element: First
      --  | | | | separator
      --  | | | | element: After
      --  | | | separator
      --  | | | element: new
      --  | | separator
      --  | | element: Before
      --  | separator
      --  | element: Last

      Before : constant Node_Index := Iter.Next (After).Node;
   begin
      if Before = Invalid_Node_Index then
         Append (Iter, New_Element);
      else
         declare
            Old_Child : constant Valid_Node_Index := Iter.Tree.Parent (After.Node);

            New_List_Nonterm : constant Valid_Node_Index := Iter.Tree.Add_Nonterm
              (Production => (Iter.List_ID, Iter.Multi_Element_RHS),
               Children   =>
                 (if Iter.Separator_ID = Invalid_Token_ID
                  then (Old_Child, New_Element)
                  else (Old_Child, Iter.Tree.Add_Terminal (Iter.Separator_ID), New_Element)));

            Parent : constant Valid_Node_Index := Iter.Tree.Parent (Before);
         begin
            --  After cannot be Iter.First, because then Before would be
            --  Invalid_Node_Index. So we don't need to change After.RHS_Index.
            Iter.Tree.Replace_Child
              (Parent               => Parent,
               Old_Child            => Old_Child,
               New_Child            => New_List_Nonterm,
               Old_Child_New_Parent => New_List_Nonterm);
         end;
      end if;
   end Insert;

   procedure Copy
     (Source_Iter  : in     Iterator;
      Source_First : in     Cursor := No_Element;
      Source_Last  : in     Cursor := No_Element;
      Dest_Iter    : in out Iterator)
   is
      Item : Cursor          := (if Source_First = No_Element then Source_Iter.First else Source_First);
      Last : constant Cursor := (if Source_Last = No_Element then Source_Iter.Last else Source_Last);
   begin
      loop
         exit when not Has_Element (Item);

         Dest_Iter.Append (Dest_Iter.Tree.Copy_Subtree (Item.Node));

         exit when Item = Last;

         Item := Source_Iter.Next (Item);
      end loop;
   end Copy;

   function Valid_Skip_List (Tree : aliased in out Syntax_Trees.Tree; Skip_List : in Skip_Array) return Boolean
   is begin
      if Skip_List'Length = 0 then return False; end if;

      if Skip_List (Skip_List'Last).Label /= Skip then return False; end if;

      if (for some I in Skip_List'First .. Skip_List'Last - 1 => Skip_List (I).Label /= Nested) then
         return False;
      end if;

      for I in Skip_List'First + 1 .. Skip_List'Last loop
         if Tree.ID (Skip_List (I).Element) /= Skip_List (I - 1).Element_ID then
            return False;
         end if;
         if Iterate_From_Element
           (Tree, Skip_List (I - 1).Element, Skip_List (I).List_ID, Skip_List (I).Element_ID,
            Skip_List (I).Separator_ID).Count = 1
         then
            return False;
         end if;
      end loop;

      return True;
   end Valid_Skip_List;

   function Copy_Skip_Nested
     (Source_Iter : in     Iterator;
      Skip_List   : in     Skip_Array;
      Skip_Found  : in out Boolean)
      return Valid_Node_Index
   is
      Tree : Syntax_Trees.Tree renames Source_Iter.Tree.all;

      Dest_Iter : Iterator := Empty_Iterator (Source_Iter);

      function Get_Dest_Child
        (Node      : in Valid_Node_Index;
         Skip_List : in Skip_Array)
        return Valid_Node_Index
      with Pre => Tree.Is_Nonterm (Node) and
                  (Skip_List'Length > 1 and then
                   (Skip_List (Skip_List'First).Label = Nested and Skip_List (Skip_List'Last).Label = Skip))
      is
         Skip_This : Nested_Skip_Item renames Skip_List (Skip_List'First);
      begin
         if Node = Skip_This.List_Root then
            return Copy_Skip_Nested
              (Source_Iter     => Iterate
                 (Tree,
                  Root         => Skip_This.List_Root,
                  List_ID      => Skip_This.List_ID,
                  Element_ID   => Skip_This.Element_ID,
                  Separator_ID => Skip_This.Separator_ID),
               Skip_List       => Skip_List (Skip_List'First + 1 .. Skip_List'Last),
               Skip_Found      => Skip_Found);
         else
            declare
               Source_Children : constant Valid_Node_Index_Array := Tree.Children (Node);
               Dest_Children   : Valid_Node_Index_Array (Source_Children'Range);
            begin
               for I in Source_Children'Range loop
                  if Source_Children (I) = Skip_This.List_Root then
                     Dest_Children (I) := Copy_Skip_Nested
                       (Source_Iter     => Iterate
                          (Tree,
                           Root         => Skip_This.List_Root,
                           List_ID      => Skip_This.List_ID,
                           Element_ID   => Skip_This.Element_ID,
                           Separator_ID => Skip_This.Separator_ID),
                        Skip_List       => Skip_List (Skip_List'First + 1 .. Skip_List'Last),
                        Skip_Found      => Skip_Found);
                  else
                     if Tree.Label (Source_Children (I)) = Nonterm then
                        Dest_Children (I) := Get_Dest_Child (Source_Children (I), Skip_List);
                     else
                        Dest_Children (I) := Tree.Copy_Subtree (Source_Children (I));
                     end if;
                  end if;
               end loop;

               return Tree.Add_Nonterm (Tree.Production_ID (Node), Dest_Children, Tree.Action (Node));
            end;
         end if;
      end Get_Dest_Child;

      Skip_This : Nested_Skip_Item renames Skip_List (Skip_List'First);
   begin
      --  See test_lr_utils.adb Test_Copy_Skip for an example.
      for Item in Source_Iter loop
         if Skip_This.Element = Item.Node then
            case Skip_This.Label is
            when Skip =>
               --  Done nesting; skip this one
               Skip_Found := True;

            when Nested =>
               Dest_Iter.Append (Get_Dest_Child (Item.Node, Skip_List));
            end case;
         else
            Dest_Iter.Append (Tree.Copy_Subtree (Item.Node));
         end if;
      end loop;
      return Dest_Iter.Root;
   end Copy_Skip_Nested;

   procedure Splice
     (Left_Iter   : in out Iterator;
      Left_Last   : in     Cursor;
      Right_Iter  : in     Iterator;
      Right_First : in     Cursor)
   is begin
      Left_Iter.Tree.Set_Children
        (Left_Iter.Tree.Parent (Right_First.Node),
         (Left_Iter.List_ID, Left_Iter.Multi_Element_RHS),
         (Left_Iter.Tree.Parent (Left_Last.Node), Right_First.Node));

      --  Preserve Iter.Last = iter.tree.child (iter.root).
      Left_Iter.Root := Right_Iter.Root;
   end Splice;

   function List_Root
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Index;
      List_ID : in Token_ID)
     return Valid_Node_Index
   is
      Root : Node_Index := Node;
   begin
      loop
         exit when Tree.Parent (Root) = Invalid_Node_Index or else Tree.ID (Tree.Parent (Root)) /= List_ID;
         Root := Tree.Parent (Root);
      end loop;
      return Root;
   end List_Root;

end WisiToken.Syntax_Trees.LR_Utils;
