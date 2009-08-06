--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
package body AUnit.Check is

   procedure Gen_Check_Discrete
       (Label    : in String;
        Computed : in Item_Type;
        Expected : in Item_Type)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Item_Type'Image (Computed) & " expecting " & Item_Type'Image (Expected));
   end Gen_Check_Discrete;

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in String)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & ASCII.LF &
           "got       '" & Computed & "'" & ASCII.LF &
           "expecting '" & Expected & "'");
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Integer;
      Expected : in Integer)
   is begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & Integer'Image (Computed) & " expecting " & Integer'Image (Expected));
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Ada.Tags.Tag;
      Expected : in Ada.Tags.Tag)
   is
      use Ada.Tags;
   begin
      Standard.AUnit.Assertions.Assert
        (Computed = Expected,
         Label & " got " & External_Tag (Computed) & " expecting " & External_Tag (Expected));
   end Check;

end AUnit.Check;
