--  Abstract :
--
--  Parse the prologue from Input_File, generate Ada for it.
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

with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Prologue
  (Input_File : in     Ada.Text_IO.File_Type;
   Text       :    out String_Lists.List)
is
   use Ada.Strings.Unbounded;
begin
   if Skip_Comments (Input_File) /= "%{" then
      Put_Error (Input_File, "expected %{");
      raise Syntax_Error;
   end if;

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         exit when Line = "%}";
         --  We'll handle a missing terminator when someone actually forgets one :)

         Text.Append (Line);
      end;
   end loop;
end Wisi.Prologue;
