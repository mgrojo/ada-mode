--  Abstract :
--
--  A generalized LR parser.
--
--  In a child package of Parser.LR partly for historical reasons,
--  partly to allow McKenzie_Recover to be in a sibling package.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Semantic_State;
package WisiToken.LR.Parser is

   procedure New_Parser
     (Parser               :    out Instance;
      Lexer                : in     WisiToken.Lexer.Handle;
      Table                : in     Parse_Table_Ptr;
      Semantic_State       : in     WisiToken.Semantic_State.Semantic_State_Access;
      Max_Parallel         : in     Ada.Containers.Count_Type := 15;
      First_Parser_Label   : in     Integer                   := 1;
      Terminate_Same_State : in     Boolean                   := True);

   procedure Parse (Shared_Parser : in out Instance);

end WisiToken.LR.Parser;
