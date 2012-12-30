--  Abstract :
--
--  see spec
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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

package body Wisi.Declarations_Token is

   procedure Noop
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs)
   is
      pragma Unreferenced (To_ID);
      use Token_List;
      Token : constant List_Iterator := Token_List.Initial_Iterator (Source); --  Declarations
   begin
      New_Token := Nonterminal.Class (Token_Handle (Token).all);
   end Noop;

   procedure Add
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs)
   is
      use Token_List;
      Token : constant List_Iterator := Token_List.Initial_Iterator (Source); --  Declaration
      I     : List_Iterator          := Token;
   begin
      Next_Token (I); --  semicolon or null

      if I = Null_Iterator then
         --  Just a single declaration

         New_Token := Nonterminal.Class
           (Instance'
              (Tokens.Instance
                 (Tokens.Get (To_ID)) with
                  List => Only (Token_Handle (Token).all)));

      else
         Next_Token (I); --  Declarations

         declare
            Declarations : Instance renames Instance (Token_Handle (I).all);
         begin
            New_Token := Nonterminal.Class
              (Instance'
                 (Tokens.Instance (Tokens.Get (To_ID)) with
                  List =>
                    Token_Handle (Token).all &
                    Declarations.List));
         end;

      end if;
   end Add;

   procedure Create
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token_IDs)
   is
      use Token_List;
      Token : constant List_Iterator := Token_List.Initial_Iterator (Source); --  Declaration
      I     : List_Iterator          := Token;

      Result : Instance := (Nonterminal.Instance (Nonterminal.Get (To_ID)) with Only (Token_Handle (Token).all));
   begin
      loop
         exit when I = Null_Iterator;
         Result.List := Token_Handle (I).all & Result.List;

         Next_Token (I);
      end loop;

      New_Token := Nonterminal.Class (Result);
   end Create;

   procedure Free (Token : in out Instance)
   is begin
      Token_List.Clean (Token.List);
   end Free;

end Wisi.Declarations_Token;
