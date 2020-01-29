--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body SAL.Web_Utils is

   function Normalize (Path : in String) return String
   is begin
      return Result : String := Path do
         for I in Result'Range loop
            if Result (I) = '\' then
               Result (I) := '/';
            end if;
         end loop;
      end return;
   end Normalize;


   function Relative_Name (Root : in String; Full_Name : in String) return String
   is
      Dir_Root : constant String := As_Directory (Root);
   begin
      if Full_Name'Length >= Dir_Root'Length and then
        Dir_Root = Full_Name (Full_Name'First .. Full_Name'First + Dir_Root'Length - 1)
      then
         return Full_Name (Full_Name'First + Dir_Root'Length .. Full_Name'Last);
      else
         --  Assume it's already relative.
         return Full_Name;
      end if;
   end Relative_Name;

   function As_Directory (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Temp (Temp'Last) = '/' then
         return Temp;
      else
         return Temp & '/';
      end if;
   end As_Directory;

   function As_File (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Path'Length = 0 then
         return Path;
      end if;

      if Temp (Temp'Last) = '/' then
         return Temp (Temp'First .. Temp'Last - 1);
      else
         return Temp;
      end if;
   end As_File;

end SAL.Web_Utils;
