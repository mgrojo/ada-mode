--  Abstract :
--
--  Add to Name_Filter a verbose option; output to Standard_Error each
--  test name before it is run, to help find which is hanging, or
--  otherwise crashing without helpful output.
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

with AUnit.Tests;
package AUnit.Test_Filters.Verbose is

   type Filter is new Name_Filter with record
      Verbose : Boolean := False;
   end record;
   --  Same filter as parent, and if Verbose, outputs to
   --  Text_IO.Standard_Error the name of each test before it is run.

   overriding function Is_Active (Filter : Verbose.Filter; T : AUnit.Tests.Test'Class) return Boolean;

end AUnit.Test_Filters.Verbose;
