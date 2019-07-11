--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
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
      use Ada.Text_IO;

      Result : constant Boolean := Name_Filter (Filter).Is_Active (T);
   begin
      if Filter.Verbose and Result then
         if T in AUnit.Simple_Test_Cases.Test_Case'Class then
            declare
               Name         : constant Message_String := AUnit.Simple_Test_Cases.Test_Case'Class (T).Name;
               Routine_Name : constant Message_String := AUnit.Simple_Test_Cases.Test_Case'Class (T).Routine_Name;
            begin
               if Name = null then
                  Put_Line (Standard_Error, "unnamed test, type: " & Ada.Tags.Expanded_Name (T'Tag));
               else
                  Put_Line (Standard_Error, Name.all & (if Routine_Name = null then "" else " " & Routine_Name.all));
               end if;
            end;
         else
            --  We don't know how to get a name.
            Put_Line (Standard_Error, "unnamed test, type: " & Ada.Tags.Expanded_Name (T'Tag));
         end if;
      end if;
      return Result;
   end Is_Active;

end AUnit.Test_Filters.Verbose;
