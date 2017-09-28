--  Abstract :
--
--  An abstract interface for a parser for grammars.
--
--  Copyright (C) 2002, 2003, 2010, 2012 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Finalization;
with WisiToken.Lexer;
package WisiToken.Parser is

   type Instance is abstract new Ada.Finalization.Limited_Controlled with record
      Lexer : WisiToken.Lexer.Handle;
   end record;

   procedure Parse (Parser : in out Instance) is abstract;
   --  Attempt a parse. Does _not_ reset Parser.Lexer on each call, to
   --  allow continuing in the same input stream.
   --
   --  Raises Syntax_Error for lexer errors, Parse_Error for
   --  parser errors.
   --
   --  If an error is encountered but a recover strategy succeeds, no
   --  exception is raised. Parser.Invalid_Regions contains the
   --  regions ignored.

end WisiToken.Parser;
