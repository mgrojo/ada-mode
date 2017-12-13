--  Abstract :
--
--  see spec
--
--
--  Copyright (C) 2009, 2014, 2015, 2017 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body WisiToken.Semantic_State is

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token'Class)
   is begin
      Put (Trace, Item.Image (Trace.Descriptor.all, ID_Only => False));
   end Put;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token_Queues.Queue_Type)
   is
      use all type SAL.Base_Peek_Type;
   begin
      Trace.Put ("(");
      for I in 1 .. Item.Count loop
         Put (Trace, Item.Peek (I));
         if I < Item.Count then
            Put (Trace, ", ");
         end if;
      end loop;
      Trace.Put (")");
   end Put;

end WisiToken.Semantic_State;
