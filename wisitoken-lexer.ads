--  Abstract :
--
--  An abstract lexer interface.
--
--  Copyright (C) 2014 - 2015, 2017 Stephe Leake
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

with Ada.Text_IO;
package WisiToken.Lexer is

   type Instance (Trace : not null access WisiToken.Trace'Class) is abstract tagged limited
   record
      Enable_Line_Numbers : Boolean;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   procedure Reset (Lexer : in out Instance; Input : in String) is abstract;
   --  Reset Lexer to start a new parse, reading Input.
   --
   --  Input is converted to 32 bit Unicode before
   --  passing to Quex.

   function Lexeme (Lexer : in Instance) return String is abstract;
   --  Return the actual text of the last token that was matched.

   function Bounds (Lexer : in Instance) return Buffer_Region is abstract;
   --  Returns the position of the start and end of the last token
   --  that was matched, in the internal buffer.
   --
   --  Most useful when the internal buffer holds the entire input text
   --  (as it will for editor parsers), and when there is a simple
   --  position mapping between the character encoding used in the editor
   --  and lexer.

   function Line (Lexer : in Instance) return Ada.Text_IO.Count is abstract;
   --  Returns the current text line number in which the most recent
   --  token was found.
   --
   --  If the underlying text feeder does not support the notion of
   --  'line', or if Lexer.Enable_Line_Numbers is False, returns 0.

   function Column (Lexer : in Instance) return Ada.Text_IO.Count is abstract;
   --  Return the current text column number of the start of the most
   --  recent token..
   --
   --  If the underlying text feeder does not support the notion of
   --  'line', returns buffer offset in underlying buffer.

   function Find_Next (Lexer : in out Instance) return Token_ID is abstract;
   --  Return the next token, getting more text from Feeder as needed.
   --
   --  Raises Syntax_Error with an appropriate message if no token
   --  is found.

end WisiToken.Lexer;
