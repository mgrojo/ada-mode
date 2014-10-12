--  Abstract :
--
--  See spec
--
--  Copyright (C) 2014  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with AUnit.Assertions;
with Ada.Text_IO;
package body OpenToken.Production.Parser.LALR.Parser_Lists.Test is

   function Debug_Image (Item : in Token.Handle) return String
   is begin
      return Item.Image & "." & Item.Name;
   end Debug_Image;

   function Is_In (Item : in Nonterminal.Handle; Stack : in Stack_Node_Access) return Boolean
   is
      use type Token.Handle;
      Stack_Node : Stack_Node_Access := Stack;
   begin
      loop
         exit when Stack_Node = null;
         if Stack_Node.Item.Token = Token.Handle (Item) then
            return True;
         end if;
         Stack_Node := Stack_Node.Next;
      end loop;
      return False;
   end Is_In;

   function Is_In (Item : in Nonterminal.Handle; Tokens : Token_List.Instance) return Boolean
   is
      use type Token.Handle;
      use Token_List;
      Iter : List_Iterator := Initial_Iterator (Tokens);
   begin
      loop
         exit when Iter = Null_Iterator;

         if Token.Handle (Item) = Token_Handle (Iter) then
            return True;
         end if;
         Next (Iter);
      end loop;
      return False;
   end Is_In;

   function Is_In_Prev_Tokens
     (Token        : in Nonterminal.Handle;
      Action_Token : in Action_Token_Node_Access)
     return Boolean
   is
      Iter : Action_Token_Node_Access := Action_Token;
   begin
      loop
         exit when Iter = null;
         if Is_In (Token, Iter.Item.Tokens) then
            return True;
         end if;
         Iter := Iter.Prev;
      end loop;
      return False;
   end Is_In_Prev_Tokens;

   function Is_In_Prev_New_Token
     (Item         : in Token.Handle;
      Action_Token : in Action_Token_Node_Access)
     return Boolean
   is
      use type Token.Handle;
      Iter : Action_Token_Node_Access := Action_Token;
   begin
      loop
         exit when Iter = null;
         if Item = Token.Handle (Iter.Item.New_Token) then
            return True;
         end if;
         Iter := Iter.Prev;
      end loop;
      return False;
   end Is_In_Prev_New_Token;

   procedure Check_Are_In_Prev_New_Token
     (Label        : in String;
      Tokens       : in Token_List.Instance;
      Action_Token : in Action_Token_Node_Access)
   is
      use AUnit.Assertions;
      use Token_List;
      Iter : List_Iterator := Initial_Iterator (Tokens);
   begin
      loop
         exit when Iter = Null_Iterator;
         if Token_Handle (Iter).all in Nonterminal.Instance then
            Assert
              (Is_In_Prev_New_Token (Token_Handle (Iter), Action_Token.Prev),
               Label & " - nonterminal Tokens " & Debug_Image (Token_Handle (Iter)) & " not in prev actions tokens");
         end if;
         Next (Iter);
      end loop;
   end Check_Are_In_Prev_New_Token;

   procedure Check_Action_Stack
     (Label  : in String;
      Cursor : in Parser_Lists.Cursor)
   is
      use AUnit.Assertions;
      use type Token.Handle;
      Stack        : Stack_Node_Access        := Cursor.Ptr.Item.Stack;
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Action_Token.Head;
   begin
      loop
         exit when Action_Token = null;
         Assert
           (Is_In_Prev_Tokens (Action_Token.Item.New_Token, Action_Token.Prev) or
              Is_In (Action_Token.Item.New_Token, Stack),
            Label & " - action.new_token " & Debug_Image (Token.Handle (Action_Token.Item.New_Token)) &
              " not in action_next.tokens or stack");
         Action_Token := Action_Token.Next;
      end loop;

      Action_Token := Cursor.Ptr.Item.Action_Token.Tail;
      loop
         exit when Stack = null;
         --  last item on stack has no token
         if Stack.Item.Token /= null and then Stack.Item.Token.all in Nonterminal.Instance then
            Assert
              (Is_In_Prev_New_Token (Stack.Item.Token, Action_Token),
               Label & " - stack " & Debug_Image (Stack.Item.Token) & " not in action.new_token");
         end if;
         Stack := Stack.Next;
      end loop;

      loop
         exit when Action_Token = null;
         Check_Are_In_Prev_New_Token (Label, Action_Token.Item.Tokens, Action_Token.Prev);
         Action_Token := Action_Token.Next;
      end loop;

   end Check_Action_Stack;

   procedure Put (Item : in Action_Token)
   is begin
      --  We don't put item.Action here; not useful in tests
      Ada.Text_IO.Put (Debug_Image (Token.Handle (Item.New_Token)) & ": ");
      Token_List_Print.Print (Item.Tokens);
   end Put;

   procedure Put_Action_Tokens (Cursor : in Parser_Lists.Cursor)
   is
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Action_Token.Head;
   begin
      loop
         exit when Action_Token = null;
         Put (Action_Token.Item);
         Ada.Text_IO.New_Line;
         Action_Token := Action_Token.Next;
      end loop;
   end Put_Action_Tokens;


end OpenToken.Production.Parser.LALR.Parser_Lists.Test;
