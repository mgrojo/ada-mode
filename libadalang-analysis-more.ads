--  Abstract :
--
--  Functions we need that are not provided by
--  Libadalang.Analysis.
--
--  Copyright (C) 2018, 2020 Free Software Foundation, Inc.
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

package Libadalang.Analysis.More is

   function Is_Node (Node : in Ada_Node'class) return Boolean;
   --  Return True if Node.Internal is not No_Entity.
   --
   --  Comparison with Ada_No_Node fails if internal is no_entity but
   --  safety_net is not no_node_safety_net, which is common in error
   --  recovery.
end Libadalang.Analysis.More;
