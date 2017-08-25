--  Abstract :
--
--  Parse the prologue from Input_File
--
--  Copyright (C) 2012 - 2014, 2017 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Prologue
  (Input_File     : in     Standard.Ada.Text_IO.File_Type;
   Context_Clause :    out String_Lists.List;
   Declarations   :    out String_Lists.List)
is
   Done : Boolean := False;
begin

   if Skip_Comments (Input_File) /= "%{" then
      Put_Error (Input_File, "expected %{");
      raise Syntax_Error;
   end if;

   loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         if Line = "%}" then
            Done := True;
            exit;
         end if;
         exit when Line = "%%";
         Context_Clause.Append (Line);
      end;
   end loop;

   if not Done then
      loop
         declare
            Line : constant String := Get_Line (Input_File);
         begin
            exit when Line = "%}";
            Declarations.Append (Line);
         end;
      end loop;
   end if;

end Wisi.Prologue;
