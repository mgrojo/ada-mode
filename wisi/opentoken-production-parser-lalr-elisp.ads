--  Abstract :
--
--  Elisp output for Wisi
--
--  Copyright (C) 2012 - 2014 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Wisi;
generic
   with function Token_Image (ID : in Token.Token_ID) return String;
package OpenToken.Production.Parser.LALR.Elisp is

   procedure Output
     (Elisp_Package : in String;
      Tokens        : in Wisi.Token_Lists.List;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Parse_Table_Ptr);

end OpenToken.Production.Parser.LALR.Elisp;
