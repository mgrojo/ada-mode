-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
procedure ASU_Example_5_10_RD_No_Mixin.Run is
   Input_String : constant String := "10 + 5 * 6";

   use Integer_Sequence;
   use Integer_Selection;
begin

   OpenToken.Token.Default_Lookahead := 2;

   L.all := Copy (E & EOF + Build_Print'Access).all;

   E.all := Copy ((T & Plus & E + Build_Plus'Access and "T + T") / T + Build_Selection'Access and "E").all;

   T.all := Copy ((F & Times & T + Build_Multiply'Access and "F * F") / F + Build_Selection'Access and "T").all;

   F.all :=
     Copy ((Left_Paren & E & Right_Paren + Build_Parens'Access and "( E )") / Int + Build_Selection'Access and "F").all;

   Put_Line ("Input_String => " & Input_String);

   OpenToken.Text_Feeder.String.Set (Feeder, "10 + 5 * 6");

   Tokenizer.Reset (Analyzer);

   Tokenizer.Find_Next (Analyzer);

   OpenToken.Token.Trace_Parse := True;
   Integer_Sequence.Parse (L, Analyzer);

   if L.Value /= 40 then
      Ada.Text_IO.Put_Line ("ERROR: expecting 40");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

exception
when Ada.Text_IO.End_Error =>
   null;
end ASU_Example_5_10_RD_No_Mixin.Run;
