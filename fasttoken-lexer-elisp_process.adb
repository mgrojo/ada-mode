--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2017 Stephe Leake
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

with Ada.Strings.Fixed;
with GNAT.OS_Lib;
package body FastToken.Lexer.Elisp_Process is

   function Initialize return Handle
   is
      New_Lexer : constant access Instance := new Instance;
   begin
      --  We don't set New_Lexer.Feeder, because we don't actually use it
      New_Lexer.Buffer_Last := New_Lexer.Buffer'First - 1;
      New_Lexer.Last_Token  := Token_ID'First;
      return Handle (New_Lexer);
   end Initialize;

   overriding procedure Reset (Lexer : in out Instance; Buffer_Size : in Integer)
   is
      pragma Unreferenced (Buffer_Size);
   begin
      Lexer.Tokens.Clean;
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is begin
      if Lexer.Last_Token = EOF_Token then
         return EOF_Token;
      end if;

      if Lexer.Tokens.Is_Empty then
         declare
            --  FIXME: wrong for module; use text_feeder?
            use Ada.Strings.Fixed;
            use GNAT.OS_Lib;
            Bytes_To_Read : constant Integer := Lexer.Buffer'Last - Lexer.Buffer_Last;
            Read_Bytes    : constant Integer := Read (Standin, Lexer.Buffer'Address, Bytes_To_Read);
            First         : Integer          := Lexer.Buffer'First;
            Space_Index   : Integer;
         begin
            Lexer.Buffer_Last := Lexer.Buffer_Last + Read_Bytes;

            --  Input is a sequence of integers separated by spaces;
            --  Token_ID'Pos. Check for the trailing space to be sure
            --  an integer did not get truncated by buffering.
            loop
               Space_Index := Index (Pattern => " ", Source => Lexer.Buffer (First .. Lexer.Buffer_Last));
               exit when Space_Index < First;
               Lexer.Tokens.Append (Token_ID'Val (Integer'Value (Lexer.Buffer (First .. Space_Index))));
            end loop;
            if Space_Index = Lexer.Buffer_Last then
               Lexer.Buffer_Last := Lexer.Buffer'First - 1;
            else
               First := Lexer.Buffer_Last - Space_Index + 1; -- actually "last"
               Lexer.Buffer (1 .. First) := Lexer.Buffer (Space_Index .. Lexer.Buffer_Last);
               Lexer.Buffer_Last := First;
            end if;
         end;
      end if;

      Lexer.Last_Token := Lexer.Tokens.Pop;

      return Lexer.Last_Token;
   end Find_Next;

   overriding function Line (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      return 0;
   end Line;

   overriding function Column (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      return 0;
   end Column;

   overriding function Lexeme (Lexer : in Instance) return String
   is
      pragma Unreferenced (Lexer);
   begin
      return "";
   end Lexeme;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region
   is
      pragma Unreferenced (Lexer);
   begin
      return Null_Buffer_Region;
   end Bounds;

   procedure Discard_Rest_Of_Input (Lexer : in out Instance)
   is
      Token : Token_ID;
   begin
      loop
         Token := Lexer.Find_Next;
         exit when Token = EOF_Token;
      end loop;
   end Discard_Rest_Of_Input;

end FastToken.Lexer.Elisp_Process;
