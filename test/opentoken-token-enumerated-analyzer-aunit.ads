--  Abstract :
--
--  AUnit stuff for analyzer unit tests
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

generic
   with procedure Check_Token_ID
     (Label    : in String;
      Computed : in Token_ID;
      Expected : in Token_ID);

package OpenToken.Token.Enumerated.Analyzer.AUnit is

   type Token_Array is array (Integer range <>) of Token_ID;

   Null_Tokens : constant Token_Array := (1 .. 0 => Token_ID'First);

   procedure Check
     (Label       : in String;
      Analyzer    : in Instance;
      Last_Token  : in Token_ID;
      Tail_Null   : in Boolean     := False;
      Tail_Tokens : in Token_Array := Null_Tokens;
      Queue_Null  : in Boolean     := False;
      Queue_Token : in Token_ID    := Token_ID'First;
      Head_Null   : in Boolean     := False;
      Head_Token  : in Token_ID    := Token_ID'First);
   --  Raise AUnit.Assert_Error if Analyzer lookahead queue does not
   --  match the given state.

end OpenToken.Token.Enumerated.Analyzer.AUnit;
