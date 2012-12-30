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
  (Input_File          : in Ada.Text_IO.File_Type;
   Output_Package_Root : in String)
is
   Output_File : File_Type;
begin
   Create (Output_File, Out_File, Output_Package_Root & "-prologue.adb");
   Put_Line (Output_File, "function " & Output_Package_Root & ".Prologue return String");
   Put_Line (Output_File, "is begin");
   Put_Line (Output_File, "   return");

   if Skip_Comments (Input_File) /= "%{" then
      Put_Error (Input_File, "expected %{");
      raise Syntax_Error;
   end if;

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         exit when Line = "%}";
         Put_Line (Output_File, "   """ & Line & """;");
         --  We'll handle a missing terminator when someone actually forgets one :)
      end;
   end loop;

   Put_Line (Output_File, "end " & Output_Package_Root & ".Prologue;");
   Close (Output_File);
end Wisi.Prologue;
