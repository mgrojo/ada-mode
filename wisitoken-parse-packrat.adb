--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
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

package body WisiToken.Parse.Packrat is

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Syntax_Trees.Stream_ID;
      Element : in Syntax_Trees.Stream_Index)
     return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Tree.Get_Node_Index (Stream, Element)'Image;
      end if;
   end Image_Pos;

end WisiToken.Parse.Packrat;
