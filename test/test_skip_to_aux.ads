--  Abstract :
--
--  Stuff needed by Skip_To_Grammar and Test_Skip_To.
--
--  Copyright (C) 2017, 2018, 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with WisiToken.Lexer;
package Test_Skip_To_Aux is

   Enable : Boolean;
   --  If True, the following procedures execute checks. If False, they do nothing.

   Lexer : access WisiToken.Lexer.Instance'Class;

   procedure Test_Declaration_0 (Nonterm : in WisiToken.Base_Token);

end Test_Skip_To_Aux;
