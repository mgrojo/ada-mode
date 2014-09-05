--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014  All Rights Reserved.
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

package body OpenToken.Text_Feeder.Counted_GNAT_OS_Lib is

   function Create (File : in GNAT.OS_Lib.File_Descriptor) return Text_Feeder_Ptr
   is begin
      return new Instance'(File, 0, 0);
   end Create;

   procedure Reset (Feeder : in out Instance; Max_Bytes : in Integer) is
   begin
      Feeder.Max_Bytes  := Max_Bytes;
      Feeder.Read_Bytes := 0;
   end Reset;

   overriding procedure Get
     (Feeder   : in out Instance;
      Text     :    out String;
      Text_End :    out Integer)
   is
      use GNAT.OS_Lib;
      Bytes_To_Read : constant Integer := Integer'Min (Text'Length, Feeder.Max_Bytes - Feeder.Read_Bytes);
      Read_Bytes    : Integer;
   begin
      if Feeder.Read_Bytes >= Feeder.Max_Bytes then

         Text_End := Text'First;
         Text (Text_End) := EOF_Character;
      else
         Read_Bytes := Read (Feeder.File, Text'Address, Bytes_To_Read);

         Text_End := Text'First + Read_Bytes - 1;

         --  Translate end of line to EOL_Character.
         for I in Text'First .. Text_End loop
            if I < Text_End and then (Text (I) = ASCII.LF and Text (I + 1) = ASCII.CR) then
               --  DOS line end

               --  We should delete the second character entirely, but
               --  this is simpler. Unless the user code is trying to
               --  reproduce the source exactly, it should be
               --  harmless.
               Text (I)     := EOL_Character;
               Text (I + 1) := ' ';

            elsif I < Text_End and then (Text (I) = ASCII.CR and Text (I + 1) = ASCII.LF) then
               --  (Old) Mac line end
               Text (I) := EOL_Character;
               Text (I + 1) := ' ';

            elsif Text (I) = ASCII.LF then
               --  Unix line end
               Text (I) := EOL_Character;
            end if;
         end loop;

         if Read_Bytes < Bytes_To_Read then
            --  premature end of file
            Feeder.Read_Bytes := Feeder.Max_Bytes;

            Text_End := Text_End + 1;
            Text (Text_End) := EOF_Character;
         end if;

      end if;
   end Get;

   overriding function End_Of_Text (Feeder : in Instance) return Boolean is
   begin
      return Feeder.Read_Bytes < Feeder.Max_Bytes;
   end End_Of_Text;

end OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
