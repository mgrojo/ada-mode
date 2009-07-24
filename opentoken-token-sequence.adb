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
package body OpenToken.Token.Sequence is

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True)
   is
      use Token.Linked_List;

      I : List_Iterator := Initial_Iterator (Match.Members);
   begin
      if Trace_Parse then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line (" sequence " & Name_Dispatch (Match) &
              "'(" & Names (Match.Members) & ") match " & Name_Dispatch (Get (Analyzer)));
      end if;

      if Actively then
         while I /= Null_Iterator loop
            Parse (Token_Handle (I), Analyzer, Actively);
            Next_Token (I);
         end loop;

         Build (Match.all, Match.Members);
      else
         if Match.First_Only then
            Parse (Token_Handle (First (Match.Members)), Analyzer, Actively => False);
         else
            declare
               Count : Integer := 0;
            begin
               while I /= Null_Iterator loop
                  Parse (Token_Handle (I), Analyzer, Actively => False);
                  Count := Count + 1;
                  Next_Token (I);
               end loop;
            exception
            when Parse_Error =>
               Push_Back (Analyzer, Count);
               raise;
            end;
         end if;
      end if;

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

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use type Linked_List.Instance;
   begin
      return
        (Members    => OpenToken.Token.Handle (Left) & OpenToken.Token.Handle (Right),
         First_Only => False);
   end "&";

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Members    => OpenToken.Token.Handle (Left) & Right.Members,
         First_Only => False);
   end "&";

   function "&"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Members    => Left.Members & OpenToken.Token.Handle (Right),
         First_Only => False);
   end "&";

   function "&"
     (Left  : in Instance;
      Right : in Instance)
     return Instance
   is
      use Linked_List;
   begin
      return
        (Members    => Left.Members & Right.Members,
         First_Only => False);
   end "&";

   function New_Instance (Old_Instance : in Instance) return Handle
   is begin
      return new Class'(Class (Old_Instance));
   end New_Instance;

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is
      use Linked_List;
   begin
      Add (List, Token_Handle (First (Token.Members)));
   end Expecting;

   procedure Set_First_Only (Token : in out Instance; First_Only : in Boolean)
   is begin
      Token.First_Only := First_Only;
   end Set_First_Only;

end OpenToken.Token.Sequence;
