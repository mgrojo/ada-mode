--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

with Libadalang.Analysis.Implementation.More;
package body Libadalang.Analysis.More is

   function TDH (Unit : aliased in Analysis_Unit) return access constant
     Libadalang.Lexer.Token_Data_Handlers.Token_Data_Handler
   is begin
      return Libadalang.Analysis.Implementation.More.TDH (Unit.all);
   end TDH;

end Libadalang.Analysis.More;
