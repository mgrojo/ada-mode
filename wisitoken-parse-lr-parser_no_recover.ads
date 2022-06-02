--  Abstract :
--
--  A generalized LR parser, with no error recovery, no semantic
--  checks, no incremental parse.
--
--  This allows wisi-generate (which uses the generated wisi_grammar)
--  to not depend on wisitoken-lr-mckenzie_recover, so editing that
--  does not cause everything to be regenerated/compiled.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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

with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR.Parser_No_Recover is

   procedure New_Parser
     (Parser      : in out WisiToken.Parse.Parser.Parser'Class;
      Lexer       : in     WisiToken.Lexer.Handle;
      Table       : in     Parse_Table_Ptr;
      Productions : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data   : in     Syntax_Trees.User_Data_Access);

   function New_Parser
     (Lexer       : in     WisiToken.Lexer.Handle;
      Table       : in     Parse_Table_Ptr;
      Productions : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data   : in     Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.Parser.Parser'Class;

   procedure LR_Parse_No_Recover (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class);

end WisiToken.Parse.LR.Parser_No_Recover;
