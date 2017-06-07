--  Abstract:
--
--  FastToken lexer using compiled regular expressions interpreted at runtime.
--
--  This is slower, but easier to use, than the Aflex lexer; it is
--  used in most of the FastToken unit tests. Since it uses regexp, it
--  is easy to convert to an Aflex lexer.
--
--  Copyright (C) 2015, 2017 Stephe Leake
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

with Ada.Unchecked_Deallocation;
with FastToken.Regexp;
with FastToken.Text_Feeder;
package FastToken.Lexer.Regexp is

   type Syntax_Item is record
      Regexp : FastToken.Regexp.Regexp;
      Report : Boolean;
   end record;

   function Get
     (Regexp         : in String;
      Case_Sensitive : in Boolean := True;
      Report         : in Boolean := True)
     return Syntax_Item;
   --  Compiles Regexp with Case_Sensitive.

   type Syntax is array (Token_ID range <>) of Syntax_Item;

   type Instance (Last_Terminal : Token_ID) is new FastToken.Lexer.Instance with private;

   function New_Lexer
     (Syntax       : in FastToken.Lexer.Regexp.Syntax;
      Feeder       : in FastToken.Text_Feeder.Text_Feeder_Ptr;
      Buffer_Size  : in Integer                               := 1024)
     return FastToken.Lexer.Handle;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer);

   overriding function Find_Next (Lexer : in out Instance) return Token_ID;

   overriding
   function Line (Lexer : in Instance) return Natural;

   overriding
   function Column (Lexer : in Instance) return Natural;

   overriding function Lexeme (Lexer : in Instance) return String;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region;

private

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type Instance (Last_Terminal : Token_ID) is new FastToken.Lexer.Instance with
   record
      ID          : Token_ID; --  last token read by find_next
      Syntax      : FastToken.Lexer.Regexp.Syntax (Token_ID'First .. Last_Terminal);
      Buffer      : String_Access;
      Buffer_Head : Integer;
      Buffer_Tail : Integer;
      Lexeme_Head : Integer;
      Lexeme_Tail : Integer;
   end record;

end FastToken.Lexer.Regexp;
