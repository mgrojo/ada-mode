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
package body FastToken.Token.Wisi_Process_Runtime is

   function To_Code (Nonterm : in Nonterminal_ID) return String
   is begin
      return Int_Image (Token_ID'Pos (Nonterm));
   end To_Code;

   function To_Codes (Tokens : in Token.List.Instance) return String
   is
      use Token.List;
      Chars_Per_Token : constant Integer := 4 + 2 * Integer'Width;
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 18 + Tokens.Length * Chars_Per_Token);
      use Bounded;

      I  : List_Iterator := Tokens.First;

      Token_Line : Bounded_String := To_Bounded_String ("[");
   begin
      loop
         exit when I = Null_Iterator;
         declare
            use type Buffer_Region;
            Token : Instance renames Current (I);
         begin
            Token_Line := Token_Line & '(' & Int_Image (Token_ID'Pos (ID (I)));

            if Token.Region /= Null_Buffer_Region then
               Token_Line := Token_Line & " " & Int_Image (Token.Region.Begin_Pos) & " . " &
                 --  Elisp region end is one past the last character
                 Int_Image (Token.Region.End_Pos + 1);
            end if;
            Token_Line := Token_Line & ")";
         end;

         Next (I);
         if I = Null_Iterator then
            Token_Line := Token_Line & "]";
         end if;
      end loop;
      return To_String (Token_Line);
   end To_Codes;

end FastToken.Token.Wisi_Process_Runtime;
