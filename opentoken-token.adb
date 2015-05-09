-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014, 2015 Stephe Leake
--
--  This file is part of the OpenToken package.
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
--
-------------------------------------------------------------------------------

package body OpenToken.Token is

   procedure Free (Item : in out Handle)
   is begin
      Dispose (Item);
   end Free;

   function Image (Token : in Instance) return String
   is begin
      return Token_Image (Token.ID) & (if Token.Name /= null then "." & Token.Name.all else "");
   end Image;

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "")
     return Instance'Class
   is begin
      if Name = "" then
         return Instance'Class (Instance'(Name => null, ID => ID));
      else
         return Instance'Class (Instance'(new String'(Name), ID));
      end if;
   end Get;

   function "+" (Item : in Token_ID) return Instance'Class
   is begin
      return Get (Item);
   end "+";

   procedure Set_Name (Token : in out Class; Name : in String)
   is begin
      if Name /= "" then
         Token.Name := new String'(Name);
      end if;
   end Set_Name;

   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID)
   is begin
      Token.ID := ID;
   end Set_ID;

   function Copy (Token : in Handle) return Handle
   is begin
      if Token = null then
         return null;
      else
         return new Class'(Token.all);
      end if;
   end Copy;

   function ID (Token : in Instance'Class) return Token_ID is
   begin
      return Token.ID;
   end ID;

   function Name (Token : in Instance) return String is
   begin
      if Token.Name = null then
         return Token_Image (Token.ID);
      else
         return Token.Name.all;
      end if;
   end Name;

   function Has_Name (Token : in Instance) return Boolean
   is begin
      return Token.Name /= null;
   end Has_Name;

   function Name_Dispatch (Token : in Class) return String
   is begin
      return Name (Token);
   end Name_Dispatch;

   function Name_Dispatch (Token : access constant Instance'Class) return String
   is begin
      return Name (Token.all);
   end Name_Dispatch;

   package body List is

      procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

      function Length (Item : in Instance) return Natural
      is
         Node   : List_Node_Ptr := Item.Head;
         Result : Natural       := 0;
      begin
         loop
            exit when Node = null;
            Result := Result + 1;
            Node   := Node.Next;
         end loop;

         return Result;
      end Length;

      function Only (Subject : in Class) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'
           (Token => new Class'(Subject),
            Next  => null);
      begin
         return
           (Head => New_Node,
            Tail => New_Node);
      end Only;

      function Only (Subject : in Handle) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'
           (Token => Subject,
            Next  => null);
      begin
         return
           (Head => New_Node,
            Tail => New_Node);
      end Only;

      function "&" (Left  : in Class; Right : in Class) return Instance
      is
         Right_Node : constant List_Node_Ptr := new List_Node'
           (Token => new Class'(Right),
            Next  => null);
      begin
         return
           (Head     => new List_Node'
              (Token => new Class'(Left),
               Next  => Right_Node),
            Tail     => Right_Node);
      end "&";

      function "&" (Left  : in Class; Right : in Instance) return Instance
      is
         Left_Node : constant List_Node_Ptr := new List_Node'
           (Token => new Class'(Left),
            Next  => Right.Head);

         Last_Node : List_Node_Ptr := Right.Tail;
      begin
         if Last_Node = null then
            Last_Node := Left_Node;
         end if;
         return (Head => Left_Node,
                 Tail => Last_Node);
      end "&";

      function "&" (Left  : in Instance; Right : in Class) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'
           (Token => new Class'(Right),
            Next  => null);

         First_Node : List_Node_Ptr;
      begin
         if Left.Tail = null then
            First_Node := New_Node;
         else
            First_Node     := Left.Head;
            Left.Tail.Next := New_Node;
         end if;
         return (Head => First_Node,
                 Tail => New_Node);
      end "&";

      function "&" (Left  : in Instance; Right : in Instance) return Instance
      is begin
         Left.Tail.Next := Right.Head;
         return
           (Head => Left.Head,
            Tail => Right.Tail);
      end "&";

      function "&" (Left  : in Handle; Right : in Handle) return Instance
      is
         Tail : constant List_Node_Ptr := new List_Node'(Right, null);
         Head : constant List_Node_Ptr := new List_Node'(Left, Tail);
      begin
         return (Head, Tail);
      end "&";

      function "&" (Left  : in Instance; Right : in Handle) return Instance
      is begin
         Left.Tail.Next := new List_Node'(Right, null);
         return (Left.Head, Left.Tail.Next);
      end "&";

      function "&" (Left  : in Handle; Right : in Instance) return Instance
      is begin
         Right.Tail.Next := new List_Node'(Left, null);
         return (Right.Head, Right.Tail.Next);
      end "&";

      function Names (List : in List_Iterator) return String
      is
         I : constant List_Iterator := Next (List);

         This_Name : constant String := Name_Dispatch (Token_Handle (List));
      begin
         if I = Null_Iterator then
            return This_Name;
         else
            return This_Name & ", " & Names (I);
         end if;
      end Names;

      function Names (List : in Instance) return String
      is begin
         return Names (Initial_Iterator (List));
      end Names;

      procedure Enqueue (List  : in out Instance; Token : in     Handle)
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Token => Token, Next  => List.Head);
      begin
         if List.Tail = null then
            List.Tail := New_Node;
         end if;
         List.Head := New_Node;
      end Enqueue;

      procedure Append (List  : in out Instance; Token : in     Handle)
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Token, null);
      begin
         if List.Tail = null then
            List.Head := New_Node;
         else
            List.Tail.Next := New_Node;
         end if;

         List.Tail := New_Node;
      end Append;

      procedure Clean (List : in out Instance)
      is
         Node : List_Node_Ptr := List.Head;
         Next : List_Node_Ptr;
      begin
         --  Deallocate all the nodes in the list, along with all their tokens
         while Node /= null loop
            Next := Node.Next;
            Free (Node.Token);
            Free (Node);
            Node := Next;
         end loop;

         List.Head := null;
         List.Tail := null;
      end Clean;

      function Initial_Iterator (List : in Instance) return List_Iterator is
      begin
         return List_Iterator (List.Head);
      end Initial_Iterator;

      procedure Next_Token (Iterator : in out List_Iterator) is
      begin
         if Iterator /= null then
            Iterator := List_Iterator (Iterator.Next);
         end if;
      end Next_Token;

      function Next_Token (Iterator : in List_Iterator) return List_Iterator
      is begin
         return List_Iterator (Iterator.Next);
      end Next_Token;

      function Token_Handle (Iterator : in List_Iterator) return Handle is
      begin
         return Iterator.Token;
      end Token_Handle;

      function ID (Iterator : in List_Iterator) return Token_ID
      is begin
         return ID (Token_Handle (Iterator).all);
      end ID;

      procedure Put_Trace (Item : in Instance)
      is
         I : List_Iterator := Initial_Iterator (Item);
      begin
         loop
            exit when I = Null_Iterator;
            Put_Trace (Token_Handle (I).Image);
            Next_Token (I);
            if I /= Null_Iterator then
               Put_Trace (", ");
            end if;
         end loop;
      end Put_Trace;

   end List;

end OpenToken.Token;
