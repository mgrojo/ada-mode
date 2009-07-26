-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
--  Copyright (C) 2000 Ted Dennison
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
-------------------------------------------------------------------------------

with Ada.Text_IO;
with OpenToken.Token.Linked_List;
package body OpenToken.Token.List_Mixin is

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean      := True)
   is
      --  Since this routine can be called recursively, we have to
      --  keep the working copy on the stack.
      Local_Match : Instance'Class := Match.all;
   begin
      if Trace_Parse then
         Trace_Indent := Trace_Indent + 1;
         if Actively then
            Trace_Put ("parsing");
         else
            Trace_Put ("trying");
         end if;
         Ada.Text_IO.Put_Line
           (" list " & Name_Dispatch (Match) &
              "'(" & Name_Dispatch (Match.Element) & ", " &
              Name_Dispatch (Match.Separator) & ") match " &
              Name_Dispatch (Get (Analyzer)));
      end if;

      Initialize (Local_Match);

      --  Read element, separator until we don't find another
      --  separator. We don't store the parsed tokens; that's up to
      --  the user version of Add_List_Element. Match.Element,
      --  Match.Separator are just patterns, not storage.

      if Actively then
         loop
            OpenToken.Token.Parse (OpenToken.Token.Handle (Local_Match.Element), Analyzer, Actively => True);

            Add_List_Element
              (Match   => Local_Match,
               Element => Local_Match.Element.all);

            begin
               OpenToken.Token.Parse (Local_Match.Separator, Analyzer, Actively => True);
            exception
            when Parse_Error =>
               exit;
            end;
         end loop;

         Build (Local_Match);
         Instance'Class (Match.all) := Local_Match;
      else
         for J in 1 .. Match.Lookahead loop
            OpenToken.Token.Parse (OpenToken.Token.Handle (Local_Match.Element), Analyzer, Actively => False);

            Add_List_Element
              (Match   => Local_Match,
               Element => Local_Match.Element.all);

            begin
               OpenToken.Token.Parse (Local_Match.Separator, Analyzer, Actively => False);
            exception
            when Parse_Error =>
               exit;
            end;
         end loop;
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

   function Get
     (Element   : access Component_Token'Class;
      Separator : access OpenToken.Token.Class;
      Name      : in     String                := "";
      Lookahead : in     Integer               := Default_Lookahead)
     return Instance
   is begin
      return
        (Parent_Token with
         Element   => Component_Handle (Element),
         Separator => OpenToken.Token.Handle (Separator),
         Name      => new String'(Name),
         Lookahead => Lookahead);
   end Get;

   overriding function Name (Token : in Instance) return String
   is begin
      if Token.Name = null then
         return OpenToken.Token.Name (OpenToken.Token.Instance (Token));
      else
         return Token.Name.all;
      end if;
   end Name;

   overriding procedure Expecting (Token : access Instance; List : in out Linked_List.Instance)
   is begin
      Linked_List.Add (List, OpenToken.Token.Handle (Token.Element));
   end Expecting;

end OpenToken.Token.List_Mixin;
