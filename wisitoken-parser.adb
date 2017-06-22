--  Abstract :
--
--
--  Copyright (C) 2002, 2003, 2010, 2012, 2014 - 2015, 2017 Stephe Leake
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

package body WisiToken.Parser is

   procedure Reset (Parser : in out Instance; Buffer_Size : in Integer := 0)
   is begin
      Parser.Lexer.Reset (Buffer_Size);
   end Reset;

   function Line (Parser : in Instance) return Natural is
   begin
      return Parser.Lexer.Line;
   end Line;

   function Column (Parser : in Instance) return Natural is
   begin
      return Parser.Lexer.Column;
   end Column;

   function Last_Char_Pos (Parser : in Instance) return Integer
   is begin
      return Parser.Lexer.Bounds.End_Pos;
   end Last_Char_Pos;

end WisiToken.Parser;
