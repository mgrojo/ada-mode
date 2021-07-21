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

with SAL.AUnit;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
package WisiToken.Lexer.AUnit is

   procedure Check (Label : in String; Computed, Expected : in Token);

   function To_Token_Array (Item : in Token_ID_Array) return Token_Arrays.Vector;

   package Token_Arrays_AUnit is new Token_Arrays.Gen_AUnit
     (Check_Index   => SAL.AUnit.Check,
      Check_Element => Check);

end WisiToken.Lexer.AUnit;
