--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2015, 2017 Stephe Leake
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

with Ada.Exceptions;
package body WisiToken.Lexer.Aflex is

   function New_Lexer
     (Trace        : not null access WisiToken.Trace'Class;
      Feeder       : in              Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size  : in              Integer                     := 1024;
      First_Column : in              Integer                     := 1)
     return Handle
   is
      pragma Unreferenced (First_Column);
      New_Lexer : constant access Instance := new Instance (Trace);
   begin
      Set_Buffer_Size (Buffer_Size);

      New_Lexer.Feeder := Feeder;
      WisiToken.Lexer.Aflex.Feeder := Feeder;

      return Handle (New_Lexer);
   end New_Lexer;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer)
   is begin
      --  yyrestart is not visible in yylex.adb; it does this
      YY_Init := True;
      Set_Buffer_Size (Buffer_Size + 2); -- +2 for EOL EOF

      --  Feeder is not reset here; user resets it. But we do copy
      --  Lexer.Feeder to Aflex.Feeder, since the user only knows
      --  about the former.
      WisiToken.Lexer.Aflex.Feeder := Lexer.Feeder;
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is begin
      Lexer.Token := YYLex;
      return Lexer.Token;
   exception
   when E : others =>
      raise Syntax_Error with
         Error_Message
           ("", Lexer.Line, Lexer.Column,
            "lexer find_next error: " & Ada.Exceptions.Exception_Name (E) &
              " : " & Ada.Exceptions.Exception_Message (E));
   end Find_Next;

   overriding function Line (Lexer : in Instance) return Ada.Text_IO.Count
   is
      pragma Unreferenced (Lexer);
   begin
      return Ada.Text_IO.Count (YY_Begin_Line);
   end Line;

   overriding function Column (Lexer : in Instance) return Ada.Text_IO.Count
   is
      pragma Unreferenced (Lexer);
   begin
      return Ada.Text_IO.Count (YY_Begin_Column);
   end Column;

   overriding function Lexeme (Lexer : in Instance) return String
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_Text;
   end Lexeme;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region
   is
      pragma Unreferenced (Lexer);
   begin
      --  aflex buffer has an extra char at the start, so it is 2
      --  indexed; we want 1 indexed (mostly for backward
      --  compatibility).
      return (YY_Text_Ptr - 1, YY_Text_Ptr + YY_Length - 2);
   end Bounds;

end WisiToken.Lexer.Aflex;
