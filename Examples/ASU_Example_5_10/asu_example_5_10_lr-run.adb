-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2013 Stephe Leake
-- Copyright (C) 1999,2000 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This example is an implementation of Example 5.10 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). It demonstrates handling of synthesized attributes
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure ASU_Example_5_10_LR.Run is

   procedure Put_Usage
   is begin
      Put_Line ("asu_example_5_10_lr-run [-t] [filename]");
      Put_Line ("  -t : output trace of parser generation, execution");
      Put_Line ("  if no filename given, parse standard input");
   end Put_Usage;

   --  Create a user-settable text feeder, and a string buffer to fill it with
   Line        : String (1 .. 1024);
   Line_Length : Natural;

   Test_Parser : LALR_Parser.Instance;

   Input_File : File_Type;

   procedure Use_File (File_Name : in String)
   is begin
      Open (Input_File, In_File, File_Name);
      Set_Input (Input_File);
   end Use_File;

begin

   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when 1 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := True;

         else
            Use_File (Argument (1));
         end if;

      when 2 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := True;

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Use_File (Argument (2));

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   end;

   Test_Parser := LALR_Parser.Generate
     (Grammar, Analyzer,
      Trace       => OpenToken.Trace_Parse,
      Put_Grammar => OpenToken.Trace_Parse);

   if not Is_Open (Input_File) then
      Put_Line ("A simple calculator, as specified in example 5.10 in Aho, Sethi, and Ullman's");
      Put_Line ("""Compilers Principles, Techniques and Tools""");
      New_Line;
      Put_Line ("""+"", ""*"", and ""( num )"" are understood.");
      Put_Line ("(Enter a blank line to quit)");
   end if;

   --  Read and parse lines from the console until an empty line or end of file is read.
   loop
      Get_Line (Line, Line_Length);

      exit when Line_Length = 0;

      OpenToken.Text_Feeder.String.Set
        (Feeder => Feeder,
         Value  => Line (1 .. Line_Length));

      Put_Line ("Input_String => " & Line (1 .. Line_Length));

      LALR_Parser.Parse (Test_Parser);
   end loop;

exception
when End_Error =>
   null;
end ASU_Example_5_10_LR.Run;
