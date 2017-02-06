--  Abstract :
--
--  see spec
--
--  References:
--
--  gnu plot manual:    /usr/share/doc/gnuplot/manual/gnuplot.pdf
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with Ada.Directories;
with Ada.Text_IO;
package body SAL.Gen_Histogram.Gen_Gnuplot is

   procedure Create_File (File : in out Ada.Text_IO.File_Type; Name : in String)
   is
      use Ada.Directories;
      use Ada.Text_IO;
   begin
      if Exists (Name) then
         Delete_File (Name);
      end if;

      Create (File, Out_File, Name);
   end Create_File;

   procedure Put_Plot
     (Histogram      : in Object;
      Data_File_Name : in String;
      Plot_File_Name : in String;
      X_Label        : in String)
   is
      use Ada.Text_IO;

      Data_File : File_Type;
      --  Histogram data:
      --
      --  1 bin 0 .. 12
      --  2 bin count 0 .. Max_Bin_Count

      Plot_File : File_Type;
   begin
      Create_File (Data_File, Data_File_Name);
      Create_File (Plot_File, Plot_File_Name);

      for I in Histogram.Bins'Range loop
         Put_Line (Data_File, Integer'Image (I) & Integer'Image (Histogram.Bins (I)));
      end loop;

      Put_Line (Plot_File, "set term png size 950,480");
      Put_Line (Plot_File, "set style data histograms");
      Put_Line (Plot_File, "set style histogram cluster");
      Put_Line (Plot_File, "plot '" & Data_File_Name & "' using 2 title '" & X_Label & "'");

      Close (Data_File);
      Close (Plot_File);
   end Put_Plot;

end SAL.Gen_Histogram.Gen_Gnuplot;
