--  Abstract :
--
--  Parse the prologue from Input_File
--
--  Copyright (C) 2012 - 2014 Stephen Leake.  All Rights Reserved.
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
  (Prologue_Filename : in     Standard.Ada.Strings.Unbounded.Unbounded_String;
   Input_File        : in     Standard.Ada.Text_IO.File_Type;
   Text              :    out String_Lists.List)
is
   use Standard.Ada.Strings.Unbounded;
   Prologue_File : File_Type;
begin
   --  The syntax requires a prologue in Input_File, and we have to
   --  read it even if we are going to override it.
   if Skip_Comments (Input_File) /= "%{" then
      Put_Error (Input_File, "expected %{");
      raise Syntax_Error;
   end if;

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         exit when Line = "%}";
         Text.Append (Line);
      end;
   end loop;

   if Length (Prologue_Filename) > 0 then
      Text := String_Lists.Empty_List;

      Open (Prologue_File, In_File, -Prologue_Filename);
      loop
         declare
            Line : constant String := Get_Line (Prologue_File);
         begin
            exit when End_Of_File (Prologue_File);
            Text.Append (Line);
         end;
      end loop;

      Close (Prologue_File);
   end if;

   Set_Input (Standard_Input);
end Wisi.Prologue;
