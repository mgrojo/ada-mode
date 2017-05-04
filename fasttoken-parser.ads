--  Abstract :
--
--  An abstract interface for a parser for grammars.
--
--  Copyright (C) 2002, 2003, 2010, 2012 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

with FastToken.Lexer;
with FastToken.Token;
generic
   with package Token is new FastToken.Token (<>);
   EOF_Token    : in Token.Token_ID;
   Accept_Token : in Token.Token_ID;
   --  Accept_Token is the grammar start symbol; the LHS of the
   --  production whose action is accept - we assume there is only
   --  one.
   pragma Unreferenced (EOF_Token, Accept_Token); -- used in children
   with package Lexer is new FastToken.Lexer (Token);
package FastToken.Parser is

   --  Make generic params visible in child packages (ie
   --  fasttoken-parser-lalr-parser.ads); FastToken.Parser.Lexer does
   --  not work. Private does not work either.
   package Token_Pkg renames Token;
   package Lexer_Pkg renames Lexer;

   type Instance is abstract tagged record
      Lexer : Lexer_Pkg.Handle;
   end record;

   procedure Parse (Parser : in out Instance) is abstract;
   --  Attempt a parse.
   --
   --  Raises Syntax_Error for lexer errors, Parse_Error for
   --  parser errors.

   procedure Reset (Parser : in out Instance; Buffer_Size : in Integer := 0);
   --  Reset the internal lexer, reallocating the input buffer to
   --  Buffer_Size (if there is a buffer).
   --
   --  Appropriate if the Text_Feeder's input has changed.

   function End_Of_Text (Parser : in Instance) return Boolean;
   --  True if the lexer input buffer is empty, and text feeder
   --  reports end of text.

   function Line (Parser : in Instance) return Natural;
   --  Returns the current text line in the text feeder input file at
   --  which parsing will resume (if there is an input file).
   --
   --  Useful for error messages when errors are detected.

   function Column (Parser : in Instance) return Natural;
   --  Returns the current text column in the text feeder input file
   --  at which processing will resume (if there is an input file).
   --
   --  Useful for printing error messages when errors are detected.

   function Last_Char_Pos (Parser : in Instance) return Integer;
   --  Returns the position in the internal lexer input buffer of
   --  the end of the last token parsed.
   --
   --  Useful when the input buffer contains the entire input text.

end FastToken.Parser;
