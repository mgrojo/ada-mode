--  Abstract :
--
--  Stand-alone parser for gpr source files
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

with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder;
with WisiToken.Token_Region;
generic
   Name : in String; -- For Usage, error messages. "run_" will be prepended, "_parser" appended.

   with function Create_Parser
     (Algorithm   : in WisiToken.Parser_Algorithm_Type;
      Text_Feeder : in WisiToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size : in Integer                               := 1024)
      return WisiToken.Parser.LR.Parser.Instance;

   aflex_debug : in out Boolean; -- for verbosity
   Aflex_Feeder : in out WisiToken.Text_Feeder.Text_Feeder_Ptr;

   Errors     : in WisiToken.Token_Region.Error_List_Arrays.Vector;
   Descriptor : in WisiToken.Descriptor'Class;

procedure Gen_Run_Wisi_Parser;
