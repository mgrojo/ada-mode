-------------------------------------------------------------------------------
--
-- Copyright (C) 2003, 2009 Stephen Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This package declares a type for designating an integer literal.
-------------------------------------------------------------------------------
generic
package OpenToken.Token.Enumerated.Integer_Literal is

   type Instance is new OpenToken.Token.Enumerated.Instance with private;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   ----------------------------------------------------------------------------
   --  Get an integer literal token with the given ID and value.
   ----------------------------------------------------------------------------
   function Get
     (ID     : in Token_ID;
      Value  : in Integer := 0)
     return Instance'Class;

   overriding procedure Create
     (Lexeme     : in     String;
      Recognizer : in     Recognizer_Handle;
      New_Token  : in out Instance);

   overriding procedure Copy
     (To   : in out Instance;
      From : in     Token.Class);

   --------------------------------------------------------------------
   --  If Trace_Parse, include the current value in the name, to help
   --  decipher parser trace output. We don't include it otherwise
   --  since it is confusing as part of an "expected ..." error
   --  message.
   --------------------------------------------------------------------
   overriding function Name (Token : in Instance) return String;

   ----------------------------------------------------------------------------
   --  Return the value of the given integer token.
   ----------------------------------------------------------------------------
   function Value (Subject : in Instance) return Integer;

private
   type Instance is new OpenToken.Token.Enumerated.Instance with record
      Value : Integer;
   end record;

end OpenToken.Token.Enumerated.Integer_Literal;
