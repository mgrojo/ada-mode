--  Abstract :
--
--  An abstract text feeder interface.
--
--  Copyright (C) 2012, 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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

package FastToken.Text_Feeder is

   type Instance is abstract tagged limited null record;
   type Text_Feeder_Ptr is access all Instance'Class;

   procedure Get
     (Feeder   : in out Instance;
      New_Text :    out String;
      Text_End :    out Integer)
     is abstract;
   --  Returns a block of text from the input.
   --
   --  Text_End is the index of the last character set
   --  in New_Text.
   --
   --  New_Text'length must be > 0, but may be as small as 1.
   --
   --  If the end of a line is reached, a FastToken.EOL_Character
   --  must be retured in New_Text.
   --
   --  If the end of the input is reached, a FastToken.EOF_Character
   --  must be retured in New_Text.

   function End_Of_Text (Feeder : Instance) return Boolean is abstract;

end FastToken.Text_Feeder;
