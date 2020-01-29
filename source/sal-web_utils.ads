--  Abstract :
--
--  Utilities for AdaCore AWS web servers
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

package SAL.Web_Utils is

   function Plus_To_Star (C : in Character) return Character
     is (if C = '+' then '*' else C);
   --  For translating a search parameter to a pattern for
   --  Ada.Directories.Search.

   function Local_Href (Relative_Resource, Label : in String) return String
     is ("<a href=""/" & Relative_Resource & """>" & Label & "</a>");

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;
   --  If Full_Name starts with Root, return relative part. Otherwise return Full_Name.

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   function As_File (Path : in String) return String;
   --  delete trailing '/' if needed.

end SAL.Web_Utils;
