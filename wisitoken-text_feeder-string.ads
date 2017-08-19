--  Abstract :
--
--  A text feeder that returns user-defined strings.
--
--  Mostly useful for unit tests of lexers and parsers.
--
--  Copyright (C) 1999, 2015, 2017 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it and/or
--  modify it under the terms of the  GNU General Public License as published
--  by the Free Software Foundation; either version 3, or (at your option)
--  any later version. The WisiToken package is distributed in the hope that
--  it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for  more details.  You should have received
--  a copy of the GNU General Public License  distributed with the WisiToken
--  package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
--  59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
package WisiToken.Text_Feeder.String is

   type Instance is new WisiToken.Text_Feeder.Instance with private;

   procedure Set (Feeder : out Instance; Value  : in  Standard.String);
   --  Sets the string to be returned the next time Get is called

   overriding procedure Get
     (Feeder   : in out Instance;
      New_Text :    out Standard.String;
      Text_End :    out Integer);

   overriding function End_Of_Text (Feeder : in Instance) return Boolean;

   overriding function Line (Feeder : in Instance) return Ada.Text_IO.Count is (0);
   --  We don't define line terminators within the string.

   overriding function Col (Feeder : in Instance) return Ada.Text_IO.Count is (1);
   --  We always trim the internal buffer, so the next Get will start at 1.

private
   type Instance is new WisiToken.Text_Feeder.Instance with record
      Next_Value : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;

end WisiToken.Text_Feeder.String;
