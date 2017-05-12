--  Abstract :
--
--  Text feed that reads from Ada.Text_IO.
--
--  Copyright (C) 2017  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

private with Ada.Text_IO;
package FastToken.Text_Feeder.Text_IO is

   type Instance is limited new FastToken.Text_Feeder.Instance with private;
   type Handle is access all Instance'Class;

   function Create (File_Name : in String) return Text_Feeder_Ptr;
   --  Open internal file object for read.
   --
   --  Raises Text_IO exceptions.

   procedure Close (Feeder : in out Instance);
   --  Close the file.

   overriding procedure Get
     (Feeder   : in out Instance;
      Text     :    out String;
      Text_End :    out Integer);

   overriding function End_Of_Text (Feeder : in Instance) return Boolean;
   --  Returns True after Max_Bytes has been read.

private

   type Instance is limited new FastToken.Text_Feeder.Instance with record
      File : Ada.Text_IO.File_Type;
   end record;

end FastToken.Text_Feeder.Text_IO;
