-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephen Leake.
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
with Ada.Text_IO;

-------------------------------------------------------------------------------
--  This example is an implementation of Example 5.10 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). It demonstrates handling of synthesized attributes
-------------------------------------------------------------------------------
procedure ASU_Example_5_10_RD.Run is

   --  Create a user-settable text feeder, and a string buffer to fill it with
   Line        : String (1 .. 1024);
   Line_Length : Natural;

begin

   --  Finish the non-terminals
   E.all := Operation_List.Get
     (Element     => T,
      Separator   => Plus,
      Initialize  => Init_Plus'Access,
      Add_Element => Plus_Element'Access,
      Name        => "T {+ T}");

   Ada.Text_IO.Put_Line ("A simple calculator, as specified in example 5.10 in Aho, Sethi, and Ullman's");
   Ada.Text_IO.Put_Line ("""Compilers Principles, Techniques and Tools""");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("""+"", ""*"", and ""( num )"" are understood.");
   Ada.Text_IO.Put_Line ("(Enter a blank line to quit)");

   --  Set debugging info
   Integer_Sequence.Set_Name (L.all, "L");
   Integer_Sequence.Set_Name (F1.all, "( E )");
   Integer_Selection.Set_Name (F.all, "F");
   Operation_List.Set_Name (T.all, "F {* F}");
   OpenToken.Token.Trace_Parse := True;

   --  Read and parse lines from Current_Input until an empty line is read.
   loop
      Ada.Text_IO.Get_Line (Line, Line_Length);

      exit when Line_Length = 0;

      OpenToken.Text_Feeder.String.Set
        (Feeder => Feeder,
         Value  => Line (1 .. Line_Length));

      --  Load up the first token
      Tokenizer.Find_Next (Analyzer);

      Integer_Sequence.Parse (L, Analyzer);

   end loop;

exception
when Ada.Text_IO.End_Error =>
   null;
end ASU_Example_5_10_RD.Run;
