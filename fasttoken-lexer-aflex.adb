--  Abstract:
--
--  see spec.
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

with Ada.Exceptions;
package body FastToken.Lexer.Aflex is

   function Initialize
     (Feeder          : in Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                     := 1024;
      First_Column    : in Integer                     := 1)
     return Handle
   is
      pragma Unreferenced (First_Column);
      New_Lexer : constant Handle := new Instance;
   begin
      --  We don't set New_Lexer.Feeder, because we don't actually use it

      Set_Buffer_Size (Buffer_Size);

      FastToken.Lexer.Aflex.Feeder := Feeder;

      for ID in Token.Token_ID loop
         New_Lexer.Token_List (ID) := Get_Token (ID);
      end loop;
      return New_Lexer;
   end Initialize;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer)
   is
      pragma Unreferenced (Lexer);
   begin
      --  yyrestart is not visible in yylex.adb; it does this
      YY_Init := True;
      Set_Buffer_Size (Buffer_Size);

      --  Feeder is not reset here; user resets it.
   end Reset;

   overriding function End_Of_Text (Lexer : in Instance) return Boolean
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_EOF_Has_Been_Seen;
   end End_Of_Text;

   overriding function End_Of_Buffered_Text (Lexer : in Instance) return Boolean
   is begin
      raise Programmer_Error;
      return False;
   end End_Of_Buffered_Text;

   overriding procedure Discard_Buffered_Text (Lexer : in out Instance)
   is begin
      raise Programmer_Error;
   end Discard_Buffered_Text;

   overriding procedure Find_Next (Lexer : in out Instance)
   is begin
      Lexer.Token := YYLex;
   exception
   when E : others =>
      raise Syntax_Error with
        Int_Image (Lexer.Line) &
        ":" &
        Int_Image (Lexer.Column) &
        ": " & Ada.Exceptions.Exception_Name (E) &
        " : " & Ada.Exceptions.Exception_Message (E);
   end Find_Next;

   overriding function Line (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_Begin_Line;
   end Line;

   overriding function Column (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_Begin_Column;
   end Column;

   overriding function Get (Lexer : in Instance) return Token.Class
   is
      Temp : Token.Class := Lexer.Token_List (Lexer.Token).all;
   begin
      Token.Create
        (Lexeme     => Lexer.Lexeme,
         Bounds     => Lexer.Bounds,
         New_Token  => Temp);

      return Temp;
   end Get;

   overriding function Lexeme (Lexer : in Instance) return String
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_Text;
   end Lexeme;

   overriding function Bounds (Lexer : in Instance) return Token.Buffer_Range
   is
      pragma Unreferenced (Lexer);
   begin
      return (YY_Text_Ptr, YY_Text_Ptr + YY_Length - 1);
   end Bounds;

end FastToken.Lexer.Aflex;
