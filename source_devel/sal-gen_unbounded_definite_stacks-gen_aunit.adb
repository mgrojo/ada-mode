--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with SAL.AUnit;
procedure SAL.Gen_Unbounded_Definite_Stacks.Gen_AUnit
  (Label    : in String;
   Computed : in Stack_Type;
   Expected : in Stack_Type)
is begin
   SAL.AUnit.Check (Label & ".Depth", Computed.Depth, Expected.Depth);
   for I in 1 .. Computed.Depth loop
      Check_Element (Label & "." & SAL.Base_Peek_Type'Image (I), Computed.Peek (I), Expected.Peek (I));
   end loop;
end SAL.Gen_Unbounded_Definite_Stacks.Gen_AUnit;
