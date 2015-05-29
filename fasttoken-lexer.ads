--  Abstract :
--
--  An abstract lexer interface.
--
--  Copyright (C) 2014 - 2015 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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

with FastToken.Text_Feeder;
with FastToken.Token;
generic
   with package Token is new FastToken.Token (<>);
package FastToken.Lexer is

   type Instance is abstract tagged record
      Feeder : FastToken.Text_Feeder.Text_Feeder_Ptr;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer := 1024) is abstract;
   --  Reset Lexer, to start finding tokens. Reallocate input
   --  buffer to Buffer_Size.
   --
   --  This is appropriate when the Feeder text has been
   --  changed.

   procedure Set_Text_Feeder
     (Lexer : in out Instance;
      Feeder   : in     FastToken.Text_Feeder.Text_Feeder_Ptr)
     is abstract;

   function End_Of_Text (Lexer : in Instance) return Boolean is abstract;
   --  True if Lexer's internal buffer is empty, and
   --  Lexer.Text_Feeder reports End_Of_Text.

   function End_Of_Buffered_Text (Lexer : in Instance) return Boolean is abstract;
   --  True if Lexer's internal buffer is empty.
   --  FIXME: delete?

   procedure Discard_Buffered_Text (Lexer : in out Instance) is abstract;
   --  Discard text in Lexer's internal buffer. Do this when a
   --  parse error is encountered, and you want to start over.

   function Lexeme (Lexer : in Instance) return String is abstract;
   --  Return the actual text of the last token that was matched.

   function Bounds (Lexer : in Instance) return Token.Buffer_Range is abstract;
   --  Returns the position of the start and end of the last token
   --  that was matched, in the internal buffer.
   --
   --  Most useful when the internal buffer holds the entire input
   --  text, as it will for editor parsers.

   function Line (Lexer : in Instance) return Natural is abstract;
   --  Returns the current text line number at which processing will resume.
   --  This is particularly useful for printing error messages when
   --  syntax errors are detected.

   function Column (Lexer : in Instance) return Natural is abstract;
   --  Return the current text column number at which processing will
   --  resume. This is particularly useful for printing error messages
   --  when syntax errors are detected. First column number is given
   --  in Initialize.

   procedure Find_Next (Lexer : in out Instance) is abstract;
   --  Locate the next token.
   --
   --  Raises Syntax_Error with an appropriate message if no token
   --  is found and there is no default token.

   function Get (Lexer : in Instance) return Token.Class is abstract;
   --  Return a token object for the last token that was matched.

end FastToken.Lexer;
