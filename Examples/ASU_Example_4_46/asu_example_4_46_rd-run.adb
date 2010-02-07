-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 2000 Ted Dennison
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
--  This example is an implementation of Example 4.46 from "Compilers
--  Principles, Techniques, and Tools" by Aho, Sethi, and Ullman (aka: "The
--  Dragon Book"). The example was meant to demonstrate basic LALR(1) parsing.
--  Here we show to to perform LL(n) parsing with it.
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
procedure ASU_Example_4_46_RD.Run is

   Test_File_Name : constant String := "Example.txt";

begin

   R.all := L.all;

   Put ("Parsing file " & Test_File_Name & "...");
   Flush;

   Open
     (File => Input_File,
      Name => Test_File_Name,
      Mode => In_File);

   --  Load up the first token
   Tokenizer.Find_Next (Analyzer);

   OpenToken.Token.Parse
     (Match    => S_Prime,
      Analyzer => Analyzer);

   Put_Line ("passed");
exception
when Error : OpenToken.Parse_Error =>
   Put_Line ("failed at line" & Integer'Image (Tokenizer.Line (Analyzer)) &
               ", column" & Integer'Image (Tokenizer.Column (Analyzer)) &
               " due to parse exception:");
   Put_Line (Ada.Exceptions.Exception_Information (Error));
end ASU_Example_4_46_RD.Run;
