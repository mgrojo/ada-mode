--  Abstract :
--
--  See spec.
--
--  Copyright (C) 1999, 2014, 2015 Ted Dennison
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

package body WisiToken.Text_Feeder.String is

   overriding procedure Get
     (Feeder   : in out Instance;
      New_Text :    out Standard.String;
      Text_End :    out Integer)
   is
      use Ada.Strings.Unbounded;
      Data_Length : constant Natural := Length (Feeder.Next_Value);
   begin
      if New_Text'Length < Data_Length then
         New_Text := To_String (Head (Feeder.Next_Value, New_Text'Length));

         Tail (Feeder.Next_Value, Data_Length - New_Text'Length);
         Text_End := New_Text'Last;
      else
         Text_End := New_Text'First + Data_Length - 1;
         New_Text (New_Text'First .. Text_End) := Ada.Strings.Unbounded.To_String (Feeder.Next_Value);
         Feeder.Next_Value := Ada.Strings.Unbounded.To_Unbounded_String ((1 => WisiToken.EOF_Character));
      end if;
   end Get;

   procedure Set
     (Feeder :    out Instance;
      Value  : in     Standard.String)
   is begin
      Feeder.Next_Value := Ada.Strings.Unbounded.To_Unbounded_String
        (Value & WisiToken.EOF_Character);
   end Set;

   overriding function End_Of_Text (Feeder : Instance) return Boolean
   is begin
      return Ada.Strings.Unbounded.Length (Feeder.Next_Value) <= 1;
   end End_Of_Text;

end WisiToken.Text_Feeder.String;
