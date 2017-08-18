--  Abstract:
--
--  WisiToken wrapper around the Elisp lexer, with a process interface
--
--  Copyright (C) 2017 Stephe Leake
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

with WisiToken.Token;
package WisiToken.Lexer.Elisp_Process is

   type Instance is new WisiToken.Lexer.Instance with private;

   procedure Initialize (Lexer : in out Instance; EOF_ID : in Token_ID);

   function New_Lexer
     (EOF_ID : in              Token_ID;
      Trace  : not null access WisiToken.Trace'Class)
     return WisiToken.Lexer.Handle;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer);

   overriding function Find_Next (Lexer : in out Instance) return Token_ID;

   overriding function Line (Lexer : in Instance) return Ada.Text_IO.Count;

   overriding function Column (Lexer : in Instance) return Ada.Text_IO.Count;

   overriding function Lexeme (Lexer : in Instance) return String;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region;

   procedure Discard_Rest_Of_Input (Lexer : in out Instance);
   --  Read and discard input until an EOF token is found.

private

   type Instance is new WisiToken.Lexer.Instance with
   record
      Buffer      : String (1 .. 4096);
      Buffer_Last : Integer;
      Tokens      : Token.List.Instance; --  Filled at tail, emptied at head by Find_Next
      Last_Token  : Token_ID;
      EOF_ID      : Token_ID;
   end record;

end WisiToken.Lexer.Elisp_Process;
