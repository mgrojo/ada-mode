--  Abstract :
--
--  A generalized LR parser.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
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

with Ada.Text_IO;
with FastToken.Parser.LR.Parser_Lists;
generic
   First_Parser_Label : in Integer;

   with procedure Put_Trace (Item : in String) is Ada.Text_IO.Put;
   --  Accumulate Item in the trace buffer.

   with procedure Put_Trace_Line (Item : in String) is Ada.Text_IO.Put_Line;
   --  Accumulate Item in the trace buffer, output the trace buffer to
   --  the display.

   with package Parser_Lists is new FastToken.Parser.LR.Parser_Lists
     (First_Parser_Label, Put_Trace, Put_Trace_Line);
package FastToken.Parser.LR.Parser is

   type Instance is new FastToken.Parser.Instance with record
      Table                : Parse_Table_Ptr;
      Max_Parallel         : Integer;
      Terminate_Same_State : Boolean;
   end record;

   function Initialize
     (Lexer                : in Lexer_Pkg.Handle;
      Table                : in Parse_Table_Ptr;
      Max_Parallel         : in Integer := 15;
      Terminate_Same_State : in Boolean := False)
     return Instance;

   overriding procedure Parse (Parser : in out Instance);

end FastToken.Parser.LR.Parser;
