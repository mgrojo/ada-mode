--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019, 2021 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Simple_Test_Cases;
with Ada.Tags;
with Ada.Text_IO;
package body AUnit.Test_Filters.Verbose is

   overriding function Is_Active (Filter : Verbose.Filter; T : AUnit.Tests.Test'Class) return Boolean
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      if not (T in AUnit.Simple_Test_Cases.Test_Case'Class) then
         return False;
      end if;

      declare
         Simple_T : AUnit.Simple_Test_Cases.Test_Case'Class renames AUnit.Simple_Test_Cases.Test_Case'Class (T);
      begin
         if Length (Filter.Test_Name) = 0 then
            null; -- matches
         elsif Simple_T.Name = null then
            return False;
         elsif Simple_T.Name.all = To_String (Filter.Test_Name) then
            null; -- matches
         else
            return False;
         end if;

         if Length (Filter.Routine_Name) = 0 then
            null; -- matches
         elsif Simple_T.Routine_Name.all = To_String (Filter.Routine_Name) then
            null; -- matches
         else
            return False;
         end if;

         if Filter.Verbose then
            if Simple_T.Name = null then
               Put_Line (Standard_Error, "unnamed test, type: " & Ada.Tags.Expanded_Name (T'Tag));
            else
               Put_Line
                 (Standard_Error, Simple_T.Name.all &
                    (if Simple_T.Routine_Name = null then "" else " " & Simple_T.Routine_Name.all));
            end if;
         end if;
      end;
      return True;
   end Is_Active;

end AUnit.Test_Filters.Verbose;
