--  Abstract:
--
--  Root of FastToken lexer/parser generator and exector.
--
--  Copyright (C) 2009, 2010, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 FlightSafety International and Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.
--
--  This software was originally developed by the following company,
--  and was released as open-source software as a service to the
--  community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000

pragma License (Modified_GPL);

with Ada.Characters.Latin_1;
package FastToken is

   Syntax_Error : exception; -- no token matching current input could be found.

   Parse_Error : exception; -- Input does not conform to the grammar

   Grammar_Error : exception; -- Grammar is not consistent (ie unused tokens, missing productions)

   User_Error : exception; -- other user error (ie command line parameter)

   Programmer_Error : exception; -- a programming convention has been violated

   --  We use this regardless of OS, since we need a standard way of
   --  representing an end of line in a string buffer. We use
   --  LF to match FastToken.Token.Aflex; Aflex hard-codes LF.
   EOL_Character : constant Character := Ada.Characters.Latin_1.LF;

   --  Similarly, this is independent of OS
   EOF_Character : constant Character := Ada.Characters.Latin_1.EOT;

   Trace_Parse : Integer := 0;
   --  If Trace_Parse > 0, Parse prints helpful messages; higher value
   --  prints more.

   function Int_Image (Item : in Integer) return String;
   --  No leading space

   type Parser_Algorithm_Type is (LALR, LR1);

end FastToken;
