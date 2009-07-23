-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 2000 Ted Dennison
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

with Ada.Text_IO;
package body OpenToken.Token.Selection is

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean      := True)
   is
      use Linked_List;
      I : List_Iterator := First (Match.Members);
   begin

      if Trace_Parse then
         if Actively then
            Ada.Text_IO.Put ("parsing");
         else
            Ada.Text_IO.Put ("trying");
         end if;
         Ada.Text_IO.Put_Line
           (" selection " & Name_Dispatch (Match) &
              "'(" & Names (Match.Members) & ") match " & Name_Dispatch (Get (Analyzer)));
      end if;

      while
        not Token.Could_Parse_To (Token_Handle (I).all, Analyzer)
      loop
         Next_Token (I);
         if I = Null_Iterator then
            if Actively then
               declare
                  Expected : Linked_List.Instance;
               begin
                  Expecting (Match, Expected);
                  raise Parse_Error with "Found " & Name_Dispatch (Get (Analyzer)) & "; expected one of " &
                    Token.Linked_List.Names (Expected) & ".";
               end;
            else
               --  Don't waste time since this is probably *not* an error
               --  condition in this mode, and it will probably be handled
               --  so no one will ever see the message anyway.
               raise Parse_Error;
            end if;
         end if;
      end loop;

      if Trace_Parse then
         Ada.Text_IO.Put_Line ("matched " & Name_Dispatch (Token_Handle (I)));
      end if;

      Parse (Token_Handle (I), Analyzer, Actively);

      if Actively then
         Build (Match.all, Token_Handle (I).all);
      end if;

   end Parse;

   function "or"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return (Members => OpenToken.Token.Handle (Left) & OpenToken.Token.Handle (Right));
   end "or";

   function "or"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return (Members => OpenToken.Token.Handle (Left) & Right.Members);
   end "or";

   function "or"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return (Members => Left.Members & OpenToken.Token.Handle (Right));
   end "or";

   function "or"
     (Left  : in Instance;
      Right : in Instance)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return (Members => Left.Members & Right.Members);
   end "or";

   function New_Instance (Old_Instance : in Instance) return Handle
   is begin
      return new Class'(Class (Old_Instance));
   end New_Instance;

   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean
   is
      use Linked_List;
      I : List_Iterator := First (Match.Members);
   begin
      while I /= Null_Iterator loop
         if Could_Parse_To (Token_Handle (I).all, Analyzer) then
            return True;
         end if;
         Next_Token (I);
      end loop;
      return False;
   end Could_Parse_To;

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is
      use Linked_List;
      I : List_Iterator := First (Token.Members);
   begin
      loop
         exit when I = Null_Iterator;
         Expecting (Token_Handle (I), List);
         Next_Token (I);
      end loop;
   end Expecting;

end OpenToken.Token.Selection;
