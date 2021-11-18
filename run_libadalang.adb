--  Abstract :
--
--  Parse a file with the libadalang parser, output the corrected token stream.
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Langkit_Support.Diagnostics;
with Libadalang.Analysis; use Libadalang.Analysis;
procedure Run_Libadalang
is
   procedure Put_Usage
   is begin
      Put_Line ("run_libadalang <file> [count]");
      Put_Line ("run the libadalang parser on file count times, report time.");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   Count      : Integer := 1;
   Start_Time : Ada.Calendar.Time;

   Ctx : constant Analysis_Context := Create_Context (Charset => "UTF-8", With_Trivia => False);
   Unit : aliased Analysis_Unit;
begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      File_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (1));

      if Argument_Count > 1 then
         Count := Integer'Value (Argument (2));
      end if;
   end;

   Start_Time := Ada.Calendar.Clock;
   for I in 1 .. Count loop
      Unit := Get_From_File (Ctx, Ada.Strings.Unbounded.To_String (File_Name));
   end loop;

   declare
      use Ada.Calendar;
      End_Time : constant Time := Clock;
   begin
      Put_Line ("time per parse:" & Duration'Image ((End_Time - Start_Time) / Count));
   end;

   for D of Diagnostics (Unit) loop
      Put_Line (Langkit_Support.Diagnostics.To_Pretty_String (D));
   end loop;

   if Root (Unit) = No_Ada_Node then
      Put_Line (Current_Error, "parse failed; no libadalang tree");
   end if;

exception
when E : others =>
   Put_Line ("exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Run_Libadalang;
