-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2014, 2015 Stephen Leake.
--  Copyright (C) 1999,2000 Ted Dennison
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
procedure ASU_Example_5_10_RD_List.Run is

   procedure Put_Usage
   is begin
      Put_Line ("asu_example_5_10_rd_list-run [-t] [filename]");
      Put_Line ("  -t : output trace of parser generation, execution");
      Put_Line ("  if no filename given, parse standard input");
   end Put_Usage;

   Input_File : File_Type;

   procedure Use_File (File_Name : in String)
   is begin
      Open (Input_File, In_File, File_Name);
      Set_Input (Input_File);
   end Use_File;

   --  Create a user-settable text feeder, and a string buffer to fill it with
   Line        : String (1 .. 1024);
   Line_Length : Natural;

begin

   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when 1 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := 1;

         else
            Use_File (Argument (1));
         end if;

      when 2 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := 1;

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

   --  Finish the non-terminals
   E.all := Operation_List.Get
     (Element     => T,
      Separator   => Plus,
      Initialize  => Init_Plus'Access,
      Add_Element => Plus_Element'Access,
      Name        => "E");

   if not Is_Open (Input_File) then
      Put_Line ("A simple calculator, as specified in example 5.10 in Aho, Sethi, and Ullman's");
      Put_Line ("""Compilers Principles, Techniques and Tools""");
      New_Line;
      Put_Line ("""+"", ""*"", and ""( num )"" are understood.");
      Put_Line ("(Enter a blank line to quit)");
   end if;

   --  Set debugging info
   Master_Token.Set_Name (L.all, "L");
   Master_Token.Set_Name (F1.all, "F1");
   Master_Token.Set_Name (F.all, "F");
   Master_Token.Set_Name (T.all, "T");

   --  Read and parse lines from Current_Input until an empty line is read.
   loop
      Get_Line (Line, Line_Length);

      exit when Line_Length = 0;

      if OpenToken.Trace_Parse > 0 then
         Put_Line (Line (1 .. Line_Length));

         --  reset initial state of tokens, to help interpret the trace
         L.Value  := -1;
         F1.Value := -1;
         F.Value  := -1;
         T.Value  := -1;
         E.Value  := -1;
      end if;

      OpenToken.Text_Feeder.String.Set
        (Feeder => Feeder,
         Value  => Line (1 .. Line_Length));

      Put_Line ("Input_String => " & Line (1 .. Line_Length));

      --  Load up the first token
      Analyzer.Find_Next;

      Integer_Sequence.Parse (L, Analyzer);

   end loop;

exception
when End_Error =>
   null;
end ASU_Example_5_10_RD_List.Run;
