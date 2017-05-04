--  Abstract :
--
--  see spec
--
--  Copyright (C) 2013, 2014, 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

pragma License (Modified_GPL);

package body FastToken.Production is

   function "+" (Tokens : in Token.List.Instance; Action : in Nonterminal.Synthesize) return Right_Hand_Side
   is begin
      return (Tokens, Action, 0);
   end "+";

   function "+" (Tokens : in Token.Class; Action : in Nonterminal.Synthesize) return Right_Hand_Side
   is begin
      return (Token.List.Only (Tokens), Action, 0);
   end "+";

   function "+" (Tokens : in Token.Token_ID; Action : in Nonterminal.Synthesize) return Right_Hand_Side
   is begin
      return (Token.List.Only (Token.Get (Tokens)), Action, 0);
   end "+";

   function "+" (Action : in Nonterminal.Synthesize) return Right_Hand_Side
   is begin
      return (Token.List.Null_List, Action, 0);
   end "+";

   function "+" (Tokens : in Token.List.Instance; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Tokens, null, Index);
   end "+";

   function "+" (Tokens : in Token.Class; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Token.List.Only (Tokens), null, Index);
   end "+";

   function "+" (Index  : in Integer) return Right_Hand_Side
   is begin
      return (Token.List.Null_List, null, Index);
   end "+";

   function "<=" (LHS : in Nonterminal.Handle; RHS : in Right_Hand_Side) return Instance
   is begin
      return (LHS, RHS);
   end "<=";

   function "<=" (LHS : in Token.Nonterminal_ID; RHS : in Right_Hand_Side) return Instance
   is begin
      return (new Nonterminal.Class'(Nonterminal.Get (LHS)), RHS);
   end "<=";

   function "<=" (LHS : in Nonterminal.Class; RHS : in Right_Hand_Side) return Instance
   is begin
      return (new Nonterminal.Class'(LHS), RHS);
   end "<=";

   function First_Token (Item : in Instance) return Token.List.List_Iterator
   is begin
      return Item.RHS.Tokens.First;
   end First_Token;

   package body List is

      function Only (Subject : in Production.Instance) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Subject, null);
      begin
         return (Head => New_Node, Tail => New_Node);
      end Only;

      function "and" (Left  : in Production.Instance; Right : in Production.Instance) return Instance
      is
         Right_Node : constant List_Node_Ptr := new List_Node'(Right, null);
      begin
         return
           (Head => new List_Node'(Left, Right_Node),
            Tail => Right_Node);
      end "and";

      function "and" (Left  : in Production.Instance; Right : in Instance) return Instance
      is begin
         return
           (Head => new List_Node'(Left, Right.Head),
            Tail => Right.Tail);
      end "and";

      function "and" (Left  : in Instance; Right : in Production.Instance) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Right, null);
      begin
         Left.Tail.Next := New_Node;

         return
           (Head => Left.Head,
            Tail => New_Node);
      end "and";

      function "and" (Left  : in Instance; Right : in Instance) return Instance
      is begin
         Left.Tail.Next := Right.Head;

         return
           (Head => Left.Head,
            Tail => Right.Tail);
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

         List.Head := null;
         List.Tail := null;
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

      function Last_Production (Iterator : in List_Iterator) return Boolean
      is begin
         return Iterator = null or else Iterator.Next = null;
      end Last_Production;

      function Is_Done (Iterator : in List_Iterator) return Boolean
      is begin
         return Iterator = null;
      end Is_Done;

   end List;

end FastToken.Production;
