--  Abstract :
--
--  AUnit utils for parent
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit;
package WisiToken.Parse.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in KMN;
      Expected : in KMN);

   package KMN_Lists_AUnit is new KMN_Lists.Gen_AUnit (Check);

end WisiToken.Parse.AUnit;
