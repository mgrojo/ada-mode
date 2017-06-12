--  Abstract:
--
--  See spec
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

package body FastToken.Lexer.Regexp is

   procedure Get_More_Text (Lexer : in out Instance; Current_Char : in out Integer)
   is
      New_Tail : constant Integer := Lexer.Buffer'First + Lexer.Buffer_Tail - Lexer.Buffer_Head;
   begin
      --  Slide current text to start of buffer, get more; adjust
      --  Current_Char to match.

      Current_Char := Lexer.Buffer'First + Current_Char - Lexer.Buffer_Head;

      Lexer.Buffer (Lexer.Buffer'First .. New_Tail) := Lexer.Buffer (Lexer.Buffer_Head .. Lexer.Buffer_Tail);

      Lexer.Feeder.Get
        (New_Text => Lexer.Buffer (Lexer.Buffer_Tail + 1 .. Lexer.Buffer'Last),
         Text_End => Lexer.Buffer_Tail);
   end Get_More_Text;

   function Find_Best_Match (Lexer : in out Instance) return Boolean
   is
      --  Find the longest matching character sequence in the buffer
      --  that matches a token. If the buffer tail is reached, more
      --  will be fetched from the text feeder.
      --
      --  Return True if a token is matched, False if not.

      use FastToken.Regexp;
      use type Token_ID;

      Current_Char         : Integer := Lexer.Buffer_Head;
      Current_State        : Match_State;
      Current_Match_Length : Integer := 0;
      Best_Match_ID        : Token_ID;
      Best_Match_Length    : Natural := 0;
      Still_Matching       : Boolean := False;
   begin
      if Current_Char > Lexer.Buffer_Tail and Lexer.Feeder.End_Of_Text then
         return False;
      end if;

      for I in Lexer.Syntax'Range loop
         Clear (Lexer.Syntax (I).Regexp);
      end loop;

      loop
         Still_Matching := False;

         if Current_Char > Lexer.Buffer_Tail then
            if not Lexer.Feeder.End_Of_Text then
               Get_More_Text (Lexer, Current_Char);
            end if;
         end if;

         for I in Lexer.Syntax'Range loop
            if State (Lexer.Syntax (I).Regexp) /= Error then
               Current_State := Match
                 (Lexer.Syntax (I).Regexp,
                  Lexer.Buffer (Lexer.Buffer_Head .. Lexer.Buffer_Tail),
                  Current_Char);

               case Current_State is
               when Matching =>
                  Still_Matching := True;

               when Final =>
                  Still_Matching := True;

                  Current_Match_Length := Current_Char - Lexer.Buffer_Head + 1;

                  if Best_Match_Length < Current_Match_Length then
                     Best_Match_ID  := I;
                     Best_Match_Length := Current_Match_Length;
                  end if;

               when Error =>
                  null;
               end case;
            end if;
         end loop;

         exit when (not Still_Matching) or else
           (Current_Char = Lexer.Buffer_Tail and then Lexer.Feeder.End_Of_Text);

         if Best_Match_Length = Lexer.Buffer'Length then
            raise Programmer_Error with
              "token larger than buffer size of" & Integer'Image (Lexer.Buffer'Length);
         end if;

         Current_Char := Current_Char + 1;
      end loop;

      if Best_Match_Length > 0 then
         Lexer.Lexeme_Head := Lexer.Buffer_Head;
         Lexer.Lexeme_Tail := Lexer.Buffer_Head + Best_Match_Length - 1;
         Lexer.ID          := Best_Match_ID;

         if Lexer.Lexeme_Head = Lexer.Buffer_Tail and then Lexer.Buffer (Lexer.Lexeme_Head) = EOF_Character then
            --  matched EOF; repeat that next time
            null;
         else
            Lexer.Buffer_Head := Lexer.Lexeme_Tail + 1;
         end if;
         return True;
      else
         return False;
      end if;

   end Find_Best_Match;

   ----------
   --  Public subprograms

   function Get
     (Regexp         : in String;
      Case_Sensitive : in Boolean := True;
      Report         : in Boolean := True)
      return Syntax_Item
   is begin
      return
        (FastToken.Regexp.Compile (Regexp, Case_Sensitive),
         Report);
   end Get;

   function New_Lexer
     (Trace       : not null access FastToken.Trace'Class;
      Syntax      : in              FastToken.Lexer.Regexp.Syntax;
      Feeder      : in              FastToken.Text_Feeder.Text_Feeder_Ptr;
      Buffer_Size : in              Integer := 1024)
     return FastToken.Lexer.Handle
   is
      use type Token_ID;
      New_Lexer : constant access Instance := new Instance (Trace, Syntax'Last);
   begin
      New_Lexer.Syntax := Syntax;
      New_Lexer.Feeder := Feeder;

      Reset (New_Lexer.all, Buffer_Size);

      return Handle (New_Lexer);
   end New_Lexer;

   overriding procedure Reset
     (Lexer       : in out Instance;
      Buffer_Size : in     Integer)
   is begin
      if Lexer.Buffer = null then
         Lexer.Buffer := new String (1 .. Buffer_Size);
      elsif Lexer.Buffer'Size /= Buffer_Size then
         Free (Lexer.Buffer);
         Lexer.Buffer := new String (1 .. Buffer_Size);
      end if;

      Lexer.Lexeme_Head := Lexer.Buffer'First;
      Lexer.Lexeme_Tail := Lexer.Buffer'First - 1;
      Lexer.ID          := Token_ID'First;
      Lexer.Buffer_Head := Lexer.Buffer'First;
      Lexer.Buffer_Tail := Lexer.Buffer'First - 1;
   end Reset;

   overriding function Find_Next (Lexer : in out Instance) return Token_ID
   is
      use type Token_ID;
   begin
      loop
         if not Find_Best_Match (Lexer) then
            if Lexer.Buffer_Head > Lexer.Buffer'Last then
               raise Syntax_Error with "Unrecognized EOF";
            else
               raise Syntax_Error with "Unrecognized character '" & Lexer.Buffer (Lexer.Buffer_Head) & "'";
            end if;
         end if;

         exit when Lexer.Syntax (Lexer.ID).Report;

      end loop;

      return Lexer.ID;
   end Find_Next;

   overriding function Line (Lexer : in Instance) return Natural
   is
      pragma Unreferenced (Lexer);
   begin
      --  Match current unit tests
      return 1;
   end Line;

   overriding function Column (Lexer : in Instance) return Natural
   is begin
      --  Useful for unit tests
      return Lexer.Lexeme_Head;
   end Column;

   overriding function Lexeme (Lexer : in Instance) return String
   is begin
      return Lexer.Buffer (Lexer.Lexeme_Head .. Lexer.Lexeme_Tail);
   end Lexeme;

   overriding function Bounds (Lexer : in Instance) return Buffer_Region
   is begin
      return (Lexer.Lexeme_Head, Lexer.Lexeme_Tail);
   end Bounds;

end FastToken.Lexer.Regexp;
