--  Abstract :
--
--  see spec
--
--  Copyright (C) 2013, 2014, 2015, 2017, 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

pragma License (Modified_GPL);

package body WisiToken.Production is

   function Only (Item : in Token_ID) return WisiToken.Token_ID_Lists.List
   is begin
      return List : WisiToken.Token_ID_Lists.List do
         List.Append (Item);
      end return;
   end Only;

   function "&" (Left : in Token_ID; Right : in Token_ID) return WisiToken.Token_ID_Lists.List
   is begin
      return Result : WisiToken.Token_ID_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function "+" (Tokens : in Token_ID_Lists.List; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Tokens, Action, 0);
   end "+";

   function "+" (Tokens : in Token_ID; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Only (Tokens), Action, 0);
   end "+";

   function "+" (Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Token_ID_Lists.Empty_List, Action, 0);
   end "+";

   function "+" (Tokens : in Token_ID_Lists.List; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Tokens, null, Index);
   end "+";

   function "+" (Tokens : in Token_ID; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Only (Tokens), null, Index);
   end "+";

   function "+" (Index  : in Integer) return Right_Hand_Side
   is begin
      return (Token_ID_Lists.Empty_List, null, Index);
   end "+";

   function "<=" (LHS : in Token_ID; RHS : in Right_Hand_Side) return Instance
   is begin
      return (Index => 1, LHS => LHS, RHS => RHS);
   end "<=";

   function First_Token (Item : in Instance) return Token_ID_Lists.Cursor
   is begin
      return Item.RHS.Tokens.First;
   end First_Token;

   package body List is

      function Constant_Reference
        (Container : aliased in Instance'Class;
         Position  :         in List_Node_Ptr)
        return Constant_Reference_Type
      is
         pragma Unreferenced (Container);
      begin
         --  WORKAROUND: gcc 6 reports an error for Position.Item'Access here; this passes all tests
         return (Element => Position.all.Production'Access);
      end Constant_Reference;

      function Has_Element (Cursor : in List_Node_Ptr) return Boolean
      is begin
         return Cursor /= null;
      end Has_Element;

      type List_Access_Constant is access constant Instance;
      type Iterator is new Iterator_Interfaces.Forward_Iterator with record
         Container : List_Access_Constant;
      end record;

      overriding function First (Object : Iterator) return List_Node_Ptr;
      overriding function Next
        (Object   : Iterator;
         Position : List_Node_Ptr)
        return List_Node_Ptr;

      overriding function First (Object : Iterator) return List_Node_Ptr
      is begin
         return Object.Container.Head;
      end First;

      overriding function Next
        (Object   : Iterator;
         Position : List_Node_Ptr)
        return List_Node_Ptr
      is
         pragma Unreferenced (Object);
      begin
         if Position = null then
            return null;
         else
            return Position.Next;
         end if;
      end Next;

      function Iterate (Container : aliased Instance) return Iterator_Interfaces.Forward_Iterator'Class
      is begin
         return Iterator'(Container => Container'Access);
      end Iterate;

      function Length (Container : in Instance) return Ada.Containers.Count_Type
      is begin
         return Container.Count;
      end Length;

      function Only (Subject : in Production.Instance) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Subject, null);
      begin
         return (Count => 1, Head => New_Node, Tail => New_Node);
      end Only;

      function "and" (Left : in Production.Instance; Right : in Production.Instance) return Instance
      is
         Right_Node : constant List_Node_Ptr := new List_Node'(Right, null);
         Left_Node  : constant List_Node_Ptr := new List_Node'(Left, Right_Node);
      begin
         Left_Node.Production.Index  := 1;
         Right_Node.Production.Index := 2;

         return
           (Count => 2,
            Head  => Left_Node,
            Tail  => Right_Node);
      end "and";

      function "and" (Left  : in Instance; Right : in Production.Instance) return Instance
      is
         use all type Ada.Containers.Count_Type;

         New_Node : constant List_Node_Ptr := new List_Node'(Right, null);
      begin
         New_Node.Production.Index := Integer (Left.Count) + 1;

         Left.Tail.Next := New_Node;

         return
           (Count => Left.Count + 1,
            Head  => Left.Head,
            Tail  => New_Node);
      end "and";

      function "and" (Left : in Instance; Right : in Instance) return Instance
      is
         use all type Ada.Containers.Count_Type;
      begin
         Left.Tail.Next := Right.Head;

         return
           (Count => Left.Count + Right.Count,
            Head  => Left.Head,
            Tail  => Right.Tail);
      end "and";

      procedure Clean (List : in out Instance)
      is
         Node : List_Node_Ptr := List.Head;
         Next : List_Node_Ptr;
      begin
         while Node /= null loop
            Next := Node.Next;
            Free (Node);
            Node := Next;
         end loop;

         List :=
           (Count => 0,
            Head  => null,
            Tail  => null);
      end Clean;

      function First (List : in Instance) return List_Iterator
      is begin
         return List_Iterator (List.Head);
      end First;

      procedure Next (Iterator : in out List_Iterator)
      is begin
         if Iterator /= null then
            Iterator := List_Iterator (Iterator.Next);
         end if;
      end Next;

      function Current (Iterator : in List_Iterator) return Production.Instance
      is begin
         return Iterator.Production;
      end Current;

      function LHS (Iterator : in List_Iterator) return Token_ID
      is begin
         return Iterator.Production.LHS;
      end LHS;

      function RHS (Iterator : in List_Iterator) return Right_Hand_Side
      is begin
         return Iterator.Production.RHS;
      end RHS;

      function Last_Production (Iterator : in List_Iterator) return Boolean
      is begin
         return Iterator = null or else Iterator.Next = null;
      end Last_Production;

      function Is_Done (Iterator : in List_Iterator) return Boolean
      is begin
         return Iterator = null;
      end Is_Done;

   end List;

end WisiToken.Production;
