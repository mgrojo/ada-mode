--  Abstract :
--
--  Parse the declarations from Input_File, add to List.
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Declarations
  (Input_File : in     Ada.Text_IO.File_Type;
   List       : in out Declaration_Lists.List)
is
   use Ada.Strings.Fixed;

   Package_Str : constant String := "%package";
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

         if Match (Package_Str) then
            List.Append ((Package_ID, +Line (Key_Last + 1 .. Line'Last)));

         elsif Match (Keyword_Str) then
            declare
               Name_First  : constant Integer := Index_Non_Blank (Source => Line, From => Key_Last + 1);

               Name_Last : constant Integer := -1 +
                 Index (Pattern => " ", Source => Line, From => Name_First);

               Value_First : constant Integer := Index_Non_Blank (Source => Line, From => Name_Last + 1);
            begin
               List.Append ((Keyword_ID, +Line (Name_First .. Name_Last), +Line (Value_First .. Line'Last)));
            end;

         elsif Match (Token_Str) then
            declare
               Kind_First : constant Integer := Index_Non_Blank (Source => Line, From => Key_Last + 1);

               Kind_Last : constant Integer := -1 +
                 Index (Pattern => " ", Source => Line, From => Kind_First);

               Name_First  : constant Integer := Index_Non_Blank (Source => Line, From => Kind_Last + 1);
            begin
               List.Append ((Token_ID, +Line (Name_First .. Line'Last), +Line (Kind_First .. Kind_Last)));
            end;

         elsif Match (Start_Str) then
            List.Append ((Start_ID, +Line (Key_Last + 1 .. Line'Last)));

         else
            raise Syntax_Error;
         end if;
      end;
   end loop;
end Wisi.Declarations;
