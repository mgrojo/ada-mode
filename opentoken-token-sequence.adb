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

package body OpenToken.Token.Sequence is

   overriding procedure Parse
     (Match    : in out Instance;
      Analyzer : in out Source_Class;
      Actively : in     Boolean := True)
   is
      use Token.Linked_List;

      I : List_Iterator := Initial_Iterator (Match.Members);
   begin

      while I /= Null_Iterator loop
         Parse
           (Match    => Token_Handle (I).all,
            Analyzer => Analyzer,
            Actively => Actively);

         Next_Token (I);
      end loop;

      if Actively then
         Build (Match, Match.Members);
      end if;

   end Parse;

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use Linked_List;
   begin
      return (Members => OpenToken.Token.Handle (Left) & OpenToken.Token.Handle (Right));
   end "&";

   function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Instance)
     return Instance
   is
      use Linked_List;
   begin
      return (Members => OpenToken.Token.Handle (Left) & Right.Members);
   end "&";

   function "&"
     (Left  : in     Instance;
      Right : access OpenToken.Token.Class)
     return Instance
   is
      use Linked_List;
   begin
      return (Members => Left.Members & OpenToken.Token.Handle (Right));
   end "&";

   function "&"
     (Left  : in Instance;
      Right : in Instance)
     return Instance
   is
      use Linked_List;
   begin
      return (Members => Left.Members & Right.Members);
   end "&";

   function New_Instance (Old_Instance : in Instance) return Handle
   is begin
      return new Class'(Class (Old_Instance));
   end New_Instance;

   overriding function Could_Parse_To
     (Match    : in Instance;
      Analyzer : in Source_Class)
     return Boolean
   is begin
      return Could_Parse_To
        (Match    => Token.Linked_List.Token_Handle
         (Token.Linked_List.Initial_Iterator (Match.Members)).all,
         Analyzer => Analyzer
         );
   end Could_Parse_To;

end OpenToken.Token.Sequence;
