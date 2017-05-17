--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014, 2015, 2017  All Rights Reserved.
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

with Ada.Strings.Bounded;
with Ada.Tags;
package body FastToken.Wisi_Tokens is

   function Get (ID : in Token_ID) return Token.Instance'Class
   is begin
      return Instance'(Token.Instance (Token.Get (ID)) with Token.Null_Buffer_Range);
   end Get;

   function Get (ID : in Token_ID) return Token.Handle
   is begin
      return new Instance'(Token.Instance (Token.Get (ID)) with Token.Null_Buffer_Range);
   end Get;

   overriding
   procedure Create
     (Lexeme     : in     String;
      Bounds     : in     Token.Buffer_Range;
      New_Token  : in out Instance)
   is
      pragma Unreferenced (Lexeme);
   begin
      New_Token.Buffer_Range := Bounds;
   end Create;

   function Get (ID : in Token_ID; Buffer_Range : in Token.Buffer_Range) return Token.Instance'Class
   is begin
      return Instance'(Token.Instance (Token.Get (ID)) with Buffer_Range);
   end Get;

   function Total_Buffer_Range
     (Tokens : in Wisi_Tokens.Token.List.Instance'Class)
     return Wisi_Tokens.Token.Buffer_Range
   is
      use Wisi_Tokens.Token.List;
      use Wisi_Tokens.Token;
      I      : List_Iterator := Tokens.First;
      Result : Buffer_Range  := Null_Buffer_Range;
   begin
      if I = Null_Iterator then
         return Null_Buffer_Range;
      end if;

      loop
         exit when I = Null_Iterator;
         if Token_Handle (I).all not in Wisi_Tokens.Instance then
            raise Programmer_Error with Ada.Tags.Expanded_Name (Token_Handle (I).all'Tag) &
              " " & Image (Token_Handle (I).all) & " passed to Wisi_Tokens.Total_Buffer_Range";
         end if;
         declare
            Buffer_Region : Buffer_Range renames Wisi_Tokens.Instance (Token_Handle (I).all).Buffer_Range;
         begin
            if Result.Begin_Pos > Buffer_Region.Begin_Pos then
               Result.Begin_Pos := Buffer_Region.Begin_Pos;
            end if;

            if Result.End_Pos < Buffer_Region.End_Pos then
               Result.End_Pos := Buffer_Region.End_Pos;
            end if;
         end;

         Next_Token (I);
      end loop;
      return Result;
   end Total_Buffer_Range;

   procedure Self
     (Nonterm : in Token.Nonterminal_ID;
      Source  : in Token.List.Instance)
   is
      pragma Unreferenced (Nonterm, Source);
   begin
      null;
      --  FIXME: move to Token
      --  New_Token := Get (To_ID, Total_Buffer_Range (Source));
   end Self;

   overriding
   function Image (Item : in Instance) return String
   is
      use Token;
      Name : constant String := Token_Image (Item.ID);
   begin
      if Item.Buffer_Range = Token.Null_Buffer_Range then
         return "(" & Name & ")";
      else
         return "(" & Name & Integer'Image (Item.Buffer_Range.Begin_Pos) & " ." &
           --  Elisp region end is one past the last character
           Integer'Image (Item.Buffer_Range.End_Pos + 1) & ")";
      end if;
   end Image;

   function To_Codes (Tokens : in Wisi_Tokens.Token.List.Instance'Class) return String
   is
      use Wisi_Tokens.Token.List;
      Chars_Per_Token : constant Integer := 4 + 2 * Integer'Width;
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 18 + Tokens.Length * Chars_Per_Token);
      use Bounded;

      I  : List_Iterator := Tokens.First;

      --  In the elisp parser, 'wisi-tokens' is bound to the tokens in the
      --  RHS of the production.
      --
      --  We return a list of tokens as integer codes '[(code range)
      --  (code range) ...]'; the elisp code will bind that to
      --  wisi-tokens.

      Token_Line : Bounded_String := To_Bounded_String ("[");
   begin
      loop
         exit when I = Null_Iterator;
         declare
            use type Wisi_Tokens.Token.Buffer_Range;
            Token : Instance renames Instance (Token_Handle (I).all);
         begin
            Token_Line := Token_Line & '(' & Int_Image (Token_ID'Pos (ID (I)));

            if Token.Buffer_Range /= Wisi_Tokens.Token.Null_Buffer_Range then
               Token_Line := Token_Line & Integer'Image (Token.Buffer_Range.Begin_Pos) & " ." &
                 --  Elisp region end is one past the last character
                 Integer'Image (Token.Buffer_Range.End_Pos + 1);
            end if;
            Token_Line := Token_Line & ")";
         end;

         Next_Token (I);
         if I = Null_Iterator then
            Token_Line := Token_Line & "]";
         end if;
      end loop;
      return To_String (Token_Line);
   end To_Codes;

end FastToken.Wisi_Tokens;
