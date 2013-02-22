--  Abstract :
--
--  Parse the declarations from Input_File, add to List.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Declarations
  (Input_File  : in     Standard.Ada.Text_IO.File_Type;
   Keywords    : in out String_Pair_Lists.List;
   Tokens      : in out Token_Lists.List;
   Start_Token :    out Standard.Ada.Strings.Unbounded.Unbounded_String)
is
   use Standard.Ada.Strings.Fixed;

   Keyword_Str : constant String := "%keyword";
   Token_Str   : constant String := "%token";
   Start_Str   : constant String := "%start";

begin
   loop
      declare
         Line     : constant String := Skip_Comments (Input_File);
         Key_Last : Integer         := Line'First;

         function Match (ID : in String) return Boolean
         is begin
            Key_Last := ID'Length;
            return ID = Line (1 .. ID'Length);
         end Match;

      begin
         exit when Line = "%%";

         if Match (Keyword_Str) then
            declare
               Name_First : constant Integer := Index_Non_Blank (Source => Line, From => Key_Last + 1);

               Name_Last : constant Integer := -1 + Index (Pattern => " ", Source => Line, From => Name_First);

               Value_First : constant Integer := Index_Non_Blank (Source => Line, From => Name_Last + 1);
            begin
               Keywords.Append ((+Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (Token_Str) then
            declare
               use Standard.Ada.Strings.Unbounded;

               --  kind has syntax <name>; strip < >.

               Kind_First : constant Integer := 1 + Index_Non_Blank (Source => Line, From => Key_Last + 1);

               Kind_Last : constant Integer := -2 +
                 Index (Pattern => " ", Source => Line, From => Kind_First);

               Name_First : constant Integer := Index_Non_Blank (Source => Line, From => Kind_Last + 2);

               Name_Last : constant Integer := -1 + Index (Pattern => " ", Source => Line, From => Name_First);

               Value_First : constant Integer :=
                 (if Name_Last = -1 then 0
                  else Index_Non_Blank (Source => Line, From => Name_Last + 1));
            begin
               if Value_First = 0 then
                  Add_Token
                    (Tokens,
                     Kind  => """" & Line (Kind_First .. Kind_Last) & """",
                     Name  => Line (Name_First .. Line'Last),
                     Value => "");
               else
                  Add_Token
                    (Tokens,
                     """" & Line (Kind_First .. Kind_Last) & """",
                     Line (Name_First .. Name_Last),
                     Line (Value_First .. Line'Last));
               end if;
            end;

         elsif Match (Start_Str) then
            declare
               Value_First : constant Integer := Index_Non_Blank (Source => Line, From => Key_Last + 1);
            begin
               Start_Token := +Line (Value_First .. Line'Last);
            end;

         else
            Put_Error (Input_File, "unexpected");
            raise Syntax_Error;
         end if;
      end;
   end loop;
end Wisi.Declarations;
