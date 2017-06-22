--  Abstract :
--
--  A generalized LR parser.
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

with WisiToken.Token;
package WisiToken.Parser.LR.Parser is

   type Instance is new WisiToken.Parser.LR.Instance with record
      Max_Parallel         : Integer;
      First_Parser_Label   : Integer;
      Terminate_Same_State : Boolean;
   end record;

   function New_Parser
     (Lexer                :         in     Lexer_Pkg.Handle;
      Table                :         in     Parse_Table_Ptr;
      Semantic_State       : aliased in out WisiToken.Token.Semantic_State'Class;
      Max_Parallel         :         in     Integer := 15;
      First_Parser_Label   :         in     Integer := 1;
      Terminate_Same_State :         in     Boolean := False)
     return Instance;

   overriding procedure Parse (Parser : in out Instance);
   --  Trace_Parse setttings:
   --  0 - no info
   --  1 - parallel parser create, delete
   --  2 - input tokens, reduce actions
   --  3 - parse stack

end WisiToken.Parser.LR.Parser;
