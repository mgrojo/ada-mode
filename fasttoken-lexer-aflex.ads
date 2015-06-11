--  Abstract:
--
--  FastToken wrapper around the aflex lexer
--
--  aflex must be run with the following options:
--
--    -E -Owisi/fasttoken_aflex_io_template.adb
--
--  Copyright (C) 2015 Stephe Leake
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
generic
   Feeder : in out FastToken.Text_Feeder.Text_Feeder_Ptr;
   --  Must represent line end as ASCII.LF (that's what aflex uses).

   with function YYLex return FastToken.Lexer.Token.Token_ID;
   --  Read tokens from Feeder

   with function YY_Text return String;
   --  Lexeme for last token

   YY_Text_Ptr : in out Integer;
   --  Index of start of YY_Text in internal buffer.

   with function YY_Length return Integer;
   --  Length of YY_Text

   with procedure Set_Buffer_Size (Size : in Integer);
   --  Set lexer internal buffer size.

   YY_Begin_Line : in out Integer;
   YY_Begin_Column : in out Integer;
   --  Line, column of last token in input stream.

   YY_Init              : in out Boolean;
   YY_EOF_Has_Been_Seen : in out Boolean;

   with function Get_Token (ID : in FastToken.Lexer.Token.Token_ID) return Token.Handle;
   --  All token objects have the same type

package FastToken.Lexer.Aflex is

   type Instance is new FastToken.Lexer.Instance with private;

   function Initialize
     (Feeder       : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size  : in Integer                               := 1024;
      First_Column : in Integer                               := 1)
     return FastToken.Lexer.Handle;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer);

   overriding
   function End_Of_Text (Lexer : in Instance) return Boolean;

   overriding procedure Find_Next (Lexer : in out Instance);

   overriding
   function Line (Lexer : in Instance) return Natural;

   overriding
   function Column (Lexer : in Instance) return Natural;

   overriding function Get (Lexer : in Instance) return Token.Class;

   overriding function Lexeme (Lexer : in Instance) return String;

   overriding function Bounds (Lexer : in Instance) return Token.Buffer_Range;

private

   type ID_Array_Tokens is array (Token.Token_ID) of Token.Handle;

   type Instance is new FastToken.Lexer.Instance with
   record
      Token      : FastToken.Lexer.Token.Token_ID; --  last token read by find_next
      Token_List : ID_Array_Tokens;
   end record;

end FastToken.Lexer.Aflex;
