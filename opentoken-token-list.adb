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
--
-------------------------------------------------------------------------------

package body OpenToken.Token.List is

   overriding procedure Parse
     (Match    : access Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean      := True)
   is
      --  Since this routine can be called recursively, we have to
      --  keep the working copy on the stack.
      Local_Match : Instance'Class := Match.all;
   begin
      --  Read element, separator until we don't find another
      --  separator. We don't store the parsed tokens; that's up to
      --  the user version of Add_List_Element. Match.Element,
      --  Match.Separator are just patterns, not storage.
      loop
         OpenToken.Token.Parse (Local_Match.Element, Analyzer, Actively);

         if Actively then
            Add_List_Element
              (Match   => Local_Match,
               Element => Local_Match.Element.all);
         end if;

         begin
            OpenToken.Token.Parse (Local_Match.Separator, Analyzer, Actively);
         exception
         when Parse_Error =>
            exit;
         end;
      end loop;

      if Actively then
         Build (Local_Match);
         Instance'Class (Match.all) := Local_Match;
      end if;

   end Parse;

   function Get
     (Element   : access OpenToken.Token.Class;
      Separator : access OpenToken.Token.Class)
     return Class
   is begin
      return Instance'
        (Element   => OpenToken.Token.Handle (Element),
         Separator => OpenToken.Token.Handle (Separator));
   end Get;

end OpenToken.Token.List;
