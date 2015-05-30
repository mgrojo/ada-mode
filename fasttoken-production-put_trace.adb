--  Abstract :
--
--  See spec
--
--  Copyright (C) 2002, 2014, 2015 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

package body FastToken.Production.Put_Trace is

   procedure Put_Trace (Item : in Instance)
   is begin
      Put_Trace ("(" & Token.Token_Image (Token.ID (Item.LHS.all)) & " <= ");
      Put_Trace (Item.RHS);
      Put_Trace (")");
   end Put_Trace;

   procedure Put_Trace (Item : in Right_Hand_Side)
   is
      use type Nonterminal.Synthesize;
   begin
      Token.List.Put_Trace (Item.Tokens);
      Put_Trace (", Action => ");
      Put_Trace_Action (Item.Action);
   end Put_Trace;

   procedure Put_Trace (Item : in List.Instance)
   is
      use List;
      I : List_Iterator := First (Item);
   begin
      Put_Trace ("(");
      loop
         exit when Is_Done (I);
         Put_Trace (Current (I));
         Put_Trace_Line (",");
         Next (I);
      end loop;
      Put_Trace (")");
   end Put_Trace;

end FastToken.Production.Put_Trace;
