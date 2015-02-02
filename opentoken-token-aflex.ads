--  Abstract:
--
--  OpenToken wrapper around the aflex lexer
--
--  Copyright (C) 2015 Stephe Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
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

with OpenToken.Token.Nonterminal;
generic
   with function YYLex return Token_ID;
   --  Read tokens from <grammar>_IO.User_Input_File or Ada.Text_IO.Current_Input
   --  FIXME: change to use Text_Feeder

   with function YY_Text return String;
   --  Lexeme for last token

   YY_Init              : in out Boolean;
   YY_CP                : in out Integer;
   YY_EOF_Has_Been_Seen : in out Boolean;

   with package Nonterminals is new OpenToken.Token.Nonterminal;
   with function Get_Token (ID : in Token_ID) return Nonterminals.Instance'Class;

package OpenToken.Token.Aflex is

   type Instance (Max_Buffer_Size : Integer) is new OpenToken.Token.Source with private;
   type Handle is access all Instance;

   function Initialize
     (Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                               := 1024;
      First_Column    : in Integer                               := 1)
     return Handle;

   overriding function Name (Lexer : in Instance; ID : in Token_ID) return String;

   overriding procedure Reset (Lexer : in out Instance);

   overriding
   procedure Set_Text_Feeder (Lexer : in out Instance; Feeder : in OpenToken.Text_Feeder.Text_Feeder_Ptr);

   overriding
   function End_Of_Text (Lexer : in Instance) return Boolean;

   overriding
   function End_Of_Buffered_Text (Lexer : in Instance) return Boolean;
   --  FIXME: why is this visible?

   overriding
   procedure Discard_Buffered_Text (Lexer : in out Instance);
   --  FIXME: is this ever called without Reset?

   overriding procedure Find_Next
     (Lexer   : in out Instance;
      Look_Ahead : in     Boolean := False);

   overriding function Mark_Push_Back (Lexer : in Instance) return Token.Queue_Mark'Class;
   overriding procedure Push_Back (Lexer : in out Instance; Mark : in Token.Queue_Mark'Class);

   overriding
   function Line (Lexer : in Instance) return Natural;

   overriding
   function Column (Lexer : in Instance) return Natural;

   overriding function Get (Lexer : in Instance) return OpenToken.Token.Class;

   overriding function Lexeme (Lexer : in Instance) return String;

private

   type ID_Array_Tokens is array (Token_ID) of Token.Handle;

   type Instance (Max_Buffer_Size : Integer) is new OpenToken.Token.Source with
   record
      Token      : Token_ID; --  last token read by find_next
      Token_List : ID_Array_Tokens;
      Text_Pos : Integer; -- character position o
   end record;

end OpenToken.Token.Aflex;
