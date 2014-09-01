--  Abstract :
--
--  see spec
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
with Ada.Strings.Unbounded;
package body OpenToken.Wisi_Tokens is

   function Get (ID : in Token_IDs) return Instance'Class
   is begin
      return Instance'(Nonterminals.Instance (Nonterminals.Get (ID)) with (0, 0));
   end Get;

   procedure Decorate (Token : in out Tokens.Class; Analyzer : in Analyzers.Instance)
   is
      pragma Unreferenced (Analyzer);
   begin
      Instance (Token).Buffer_Range := (0, 0); --  FIXME: add access functions to analyzer
   end Decorate;

   function Get (ID : in Token_IDs; Buffer_Range : in Wisi_Tokens.Buffer_Range) return Instance'Class
   is begin
      return Instance'(Nonterminals.Instance (Nonterminals.Get (ID)) with Buffer_Range);
   end Get;

   function Total_Buffer_Range (Tokens : in Token_Lists.Instance'Class) return Buffer_Range
   is
      use Token_Lists;
      I  : List_Iterator := Tokens.Initial_Iterator;
      Result : Buffer_Range  := (Integer'First, Integer'Last);
   begin
      loop
         exit when I = Null_Iterator;
         declare
            Buffer_Range : Wisi_Tokens.Buffer_Range renames Wisi_Tokens.Instance (Token_Handle (I).all).Buffer_Range;
         begin
            if Result.Begin_Pos > Buffer_Range.Begin_Pos then
               Result.Begin_Pos := Buffer_Range.Begin_Pos;
            end if;

            if Result.End_Pos < Buffer_Range.End_Pos then
               Result.End_Pos := Buffer_Range.End_Pos;
            end if;
         end;

         Next_Token (I);
      end loop;
      return Result;
   end Total_Buffer_Range;

   procedure Self
     (New_Token : out Nonterminals.Class;
      Source    : in  Token_Lists.Instance'Class;
      To_ID     : in  Token_IDs)
   is begin
      New_Token := Get (To_ID, Total_Buffer_Range (Source));
   end Self;

   function Image (Token : in Instance) return String
   is begin
      return "(" & Int_Image (Token.Buffer_Range.Begin_Pos) & ", " & Int_Image (Token.Buffer_Range.End_Pos) & ")";
   end Image;

   function Image (Tokens : in Token_Lists.Instance'Class) return String
   is
      use Ada.Strings.Unbounded;
      use Token_Lists;
      I  : List_Iterator := Initial_Iterator (Tokens);

      Token_Line : Unbounded_String := To_Unbounded_String ("(let ((tokens '(");
   begin
      loop
         exit when I = Null_Iterator;
         Token_Line := Token_Line & Image (Instance (Token_Handle (I).all));

         Next_Token (I);
      end loop;
      Token_Line := Token_Line & ")))";
      return To_String (Token_Line);
   end Image;

end OpenToken.Wisi_Tokens;
