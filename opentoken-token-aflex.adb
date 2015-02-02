--  Abstract:
--
--  see spec.
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

with Ada.Exceptions;
with Ada.Text_IO;
package body OpenToken.Token.Aflex is

   function Initialize
     (Feeder          : in Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size     : in Integer                     := 1024;
      First_Column    : in Integer                     := 1)
     return Handle
   is
      pragma Unreferenced (First_Column);
      New_Lexer : constant Handle := new Instance (Buffer_Size);
   begin
      New_Lexer.Feeder := Feeder;
      for ID in Token_ID loop
         New_Lexer.Token_List (ID) := new Nonterminals.Class'(Get_Token (ID));
      end loop;
      return New_Lexer;
   end Initialize;

   overriding function Name (Lexer : in Instance; ID : in Token_ID) return String
   is
      pragma Unreferenced (Lexer);
   begin
      return Token_Image (ID);
   end Name;

   overriding procedure Reset (Lexer : in out Instance)
   is
      pragma Unreferenced (Lexer);
   begin
      --  yyrestart is in yylex.adb; it does this
      YY_Init := True;
   end Reset;

   overriding procedure Set_Text_Feeder
     (Lexer : in out Instance;
      Feeder : in Text_Feeder.Text_Feeder_Ptr)
   is begin
      Lexer.Feeder := Feeder;
   end Set_Text_Feeder;

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

   overriding procedure Find_Next
     (Lexer   : in out Instance;
      Look_Ahead : in     Boolean := False)
   is begin
      if Look_Ahead then
         raise Programmer_Error;
      end if;

      Lexer.Token := YYLex;
   exception
   when E : others =>
      --  FIXME: need lexer.line, column
      raise Syntax_Error with "0:" & Int_Image (YY_CP) & ": " & Ada.Exceptions.Exception_Message (E);
   end Find_Next;

   type Dummy_Queue_Mark is new Queue_Mark with record Foo : Integer; end record;

   overriding function Mark_Push_Back (Lexer : in Instance) return Token.Queue_Mark'Class
   is begin
      raise Programmer_Error;
      return Dummy_Queue_Mark'(Foo => 1);
   end Mark_Push_Back;

   overriding procedure Push_Back (Lexer : in out Instance; Mark : in Token.Queue_Mark'Class)
   is begin
      raise Programmer_Error;
   end Push_Back;

   overriding function Line (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      --  FIXME: Currently this is the only input we are using
      return Natural (Ada.Text_IO.Line (Ada.Text_IO.Current_Input));
   end Line;

   overriding function Column (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      --  FIXME: Currently this is the only input we are using
      return Natural (Ada.Text_IO.Col (Ada.Text_IO.Current_Input));
   end Column;

   overriding function Get (Lexer : in Instance) return OpenToken.Token.Class
   is begin
      --  FIXME: bounds?
      return Lexer.Token_List (Lexer.Token).all;
   end Get;

   overriding function Lexeme (Lexer : in Instance) return String
   is
      pragma Unreferenced (Lexer);
   begin
      return YY_Text;
   end Lexeme;

end OpenToken.Token.Aflex;
