--  Abstract :
--
--  Test Emacs_Wisi_Common_Parse.Edit_Source
--
--  Copyright (C) 2020, 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with AUnit.Test_Cases;
package Test_Edit_Source is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   --  Caller sets Trace_Parse, Trace_Incremental.

   type Test_Case_Access is access all Test_Case;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String;

end Test_Edit_Source;
