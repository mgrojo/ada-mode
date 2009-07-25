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

begin
   --  If we implement the grammar literally, the parser enters an
   --  infinite loop; see asu_example_5_10_rd_no_mixin-run_bad.adb.
   --
   --  The problem is that the first element of a production is the
   --  same as the result of the production.
   --
   --  So we rearrange the grammar to avoid this. Note that this
   --  rearrangement is not the same given in [1] section 2.5; that
   --  would require the action Build to be called in the middle of
   --  parsing a sequence, rather than at the end of the sequence.
   --
   --  Instead, we take advantage of the commutivity of + and *.
   --
   --  L -> E         print (L.val)
   --  E -> T + E     E.val := E1.val + T.val
   --  E -> T
   --  T -> F * T     T.val := T1.val * F.val
   --  T -> F
   --  F -> ( E )     F.val := E.val
   --  F -> digit
   --
   --  However, this grammar requires 2 lookaheads; it needs to see
   --  the + or * of the sequences for E and T. If we had used the
   --  rearrangement from [1] 2.5, it would only need 1 lookahead.

   Sequence.Default_Lookahead := 2;

   L.all := E & EOF;

   E.all := New_Expression_Instance ("T + E", T & Plus & E) or T;
   E.Name := new String'("E");

   T.all := New_Expression_Instance ("F * T", F & Times & T) or F;
   T.Name := new String'("T");

   F.all := New_Expression_Instance ("( E )", Left_Paren & E & Right_Paren) or Int_Literal;
   F.Name := new String'("F");

   Put_Line ("Input_String => " & Input_String);

   OpenToken.Text_Feeder.String.Set (Feeder, "10 + 5 * 6");

   Tokenizer.Reset (Analyzer);

   Tokenizer.Find_Next (Analyzer);

   OpenToken.Token.Trace_Parse := True;
   Parse (L, Analyzer);

exception
when Ada.Text_IO.End_Error =>
   null;
end ASU_Example_5_10_RD_No_Mixin.Run;
