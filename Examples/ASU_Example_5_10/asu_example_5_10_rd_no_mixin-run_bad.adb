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
begin
   --  Define the grammar. This first naive implementation exhibits
   --  infinite recursion, as expected (see the dragon book, section
   --  2.4, page 47).
   --
   --  The problem is that the first element of the production E is
   --  the same as the result of the production. Parse gets stuck in
   --  loop where Selection.Parse (E) calls Sequence.Parse (E) which
   --  calls Selection.Parse (E) ... without consuming any input.
   begin
      L.all := E & EOF;

      E.all := New_Expression_Instance ("E + T", E & Plus & T) or T;

      T.all := New_Expression_Instance ("T * F", T & Times & F) or F;

      F.all := New_Expression_Instance ("( E )", Left_Paren & E & Right_Paren) or Int_Literal;

      OpenToken.Text_Feeder.String.Set (Feeder, "10 + 5 * 6");

      Tokenizer.Find_Next (Analyzer);

      Parse (L, Analyzer);

      Put_Line ("Hmm, didn't get Storage_Error!");
   exception
   when Storage_Error =>
      Put_Line ("Got Storage_Error, as expected");
   end;

end ASU_Example_5_10_RD_No_Mixin.Run;
