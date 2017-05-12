--  Abstract :
--
--  see spec
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

package body FastToken.Text_Feeder.Text_IO is

   function Create (File_Name : in String) return Text_Feeder_Ptr
   is
      Result : constant Handle := new Instance;
   begin
      Ada.Text_IO.Open (Result.File, Ada.Text_IO.In_File, File_Name);
      return Text_Feeder_Ptr (Result);
   end Create;

   procedure Close (Feeder : in out Instance)
   is begin
      Ada.Text_IO.Close (Feeder.File);
   end Close;

   overriding procedure Get
     (Feeder   : in out Instance;
      Text     :    out String;
      Text_End :    out Integer)
   is
      use Ada.Text_IO;
   begin
      if End_Of_File (Feeder.File) then
         Text_End := Text'First + 1;
         Text (Text_End) := EOF_Character;
      else
         Get_Line (Feeder.File, Text, Text_End);
         if End_Of_File (Feeder.File) then
            if Text_End < Text'Last then
               Text_End := Text_End + 1;
               Text (Text_End) := EOF_Character;
            end if;
         elsif Text_End < Text'Last then
            Text_End := Text_End + 1;
            Text (Text_End) := EOL_Character;

         elsif Text_End < Text'First then
            --  Blank line
            Text_End := Text'First + 1;
            Text (Text_End) := EOL_Character;
         end if;
      end if;
   end Get;

   overriding function End_Of_Text (Feeder : in Instance) return Boolean
   is begin
      return Ada.Text_IO.End_Of_File (Feeder.File);
   end End_Of_Text;
end FastToken.Text_Feeder.Text_IO;
