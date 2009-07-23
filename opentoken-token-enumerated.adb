-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Text_IO;
package body OpenToken.Token.Enumerated is

   function Get (ID : in Token_ID := Token_ID'First) return Instance'Class
   is begin
      return Instance'Class (Instance'(ID => ID));
   end Get;

   procedure Create
     (Lexeme     : in     String;
      ID         : in     Token_ID;
      Recognizer : in     Recognizer_Handle;
      New_Token  :    out Instance)
   is
      pragma Unreferenced (Recognizer);
      pragma Unreferenced (Lexeme);
   begin
      New_Token.ID := ID;
   end Create;

   function ID (Token : in Instance'Class) return Token_ID is
   begin
      return Token.ID;
   end ID;

   procedure Set_ID
     (Token : in out Instance'Class;
      ID    : in     Token_ID)
   is begin
      Token.ID := ID;
   end Set_ID;

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : access Source_Class;
      Actively : in     Boolean      := True)
   is
      Next_Token : constant OpenToken.Token.Class := Get (Analyzer.all);
   begin
      if Trace_Parse then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line (" enumerated " & Name_Dispatch (Match));
      end if;

      if Instance (Next_Token).ID = Match.ID then
         begin
            Match.all := Instance (Next_Token);
         exception
         when Constraint_Error =>
            raise Parse_Error with
              "Expected a token of type" & Ada.Tags.Expanded_Name (Instance'Tag) & " but found a " &
              Ada.Tags.Expanded_Name (Next_Token'Tag);
         end;

         if Actively then
            Create
              (Lexeme     => Lexeme (Source'Class (Analyzer.all)),
               ID         => Match.ID,
               Recognizer => Last_Recognizer (Source'Class (Analyzer.all)),
               New_Token  => Class (Match.all));
         end if;
      else
         raise Parse_Error with "Expected " & Name_Dispatch (Match) & " but found " & Name_Dispatch (Next_Token);
      end if;

      Find_Next (Analyzer.all, Look_Ahead => not Actively);

      if Trace_Parse then
         Trace_Put ("...succeeded"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
   exception
   when others =>
      if Trace_Parse then
         Trace_Put ("...failed"); Ada.Text_IO.New_Line;
         Trace_Indent := Trace_Indent - 1;
      end if;
      raise;
   end Parse;

   overriding function Could_Parse_To
     (Match    : access Instance;
      Analyzer : access Source_Class)
     return Boolean
   is begin
      return Instance (Get (Analyzer.all)).ID = Match.ID;
   end Could_Parse_To;

   overriding function Name (Token : in Instance) return String
   is begin
      return Token_ID'Image (Token.ID);
   end Name;

end OpenToken.Token.Enumerated;
