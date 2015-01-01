-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2014, 2015 Stephe Leake
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

package body OpenToken.Token.Nonterminal is

   function Get
     (ID    : in Token_ID := Token_ID'First;
      Name  : in String   := "";
      Build : in Action   := null)
     return Instance'Class
   is begin
      if Name = "" then
         return Instance'Class (Instance'(null, ID, Build));
      else
         return Instance'Class (Instance'(new String'(Name), ID, Build));
      end if;
   end Get;

   function Copy (Token : in Handle) return Handle
   is begin
      return Nonterminal.Handle (OpenToken.Token.Copy (OpenToken.Token.Handle (Token)));
   end Copy;

   procedure Synthesize_By_Copying
     (New_Token : out Instance;
      Source    : in  OpenToken.Token.Instance'Class;
      To_ID     : in  OpenToken.Token.Token_ID)
   is begin
      New_Token := Instance (Source);
      New_Token.ID := To_ID;

   exception
   when Constraint_Error =>
      raise Invalid_Synth_Argument with
        "Token " & OpenToken.Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
        "solely from a " &
        OpenToken.Token.Token_ID'Image (OpenToken.Token.ID (Source)) &
        "; need Synthesize_Self or other action?";

   end Synthesize_By_Copying;

   procedure Default_Synthesize
     (New_Token : out Instance;
      Source    : in  Token.List.Instance'Class;
      To_ID     : in  OpenToken.Token.Token_ID)
   is begin
      Synthesize_By_Copying
        (New_Token => Class (New_Token),
         Source    => Token.List.Token_Handle (Token.List.Initial_Iterator (Source)).all,
         To_ID     => To_ID);
   end Default_Synthesize;

   procedure Self_Synthesize
     (New_Token : out Class;
      Source    : in  Token.List.Instance'Class;
      To_ID     : in  OpenToken.Token.Token_ID)
   is
      pragma Unreferenced (Source);
   begin
      New_Token.ID := To_ID;
   end Self_Synthesize;

   procedure Synthesize_From_First
     (New_Token : out Class;
      Source    : in  Token.List.Instance'Class;
      To_ID     : in  OpenToken.Token.Token_ID)
   is
      Checked_Source : Handle;
   begin
      begin
         Checked_Source := Handle (Token.List.Token_Handle (Token.List.Initial_Iterator (Source)));
      exception
      when Constraint_Error =>
         raise Invalid_Synth_Argument with
           "Token " & OpenToken.Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
           "solely from a " &
           OpenToken.Token.Token_ID'Image
           (OpenToken.Token.ID
              (Token.List.Token_Handle (Token.List.Initial_Iterator (Source)).all)) & ".";
      end;

      Synthesize_By_Copying
        (New_Token => New_Token,
         Source    => Checked_Source.all,
         To_ID     => To_ID);
   end Synthesize_From_First;

   procedure Default_Synthesize_Class
     (New_Token : out Class;
      Source    : in  Token.List.Instance'Class;
      To_ID     : in  OpenToken.Token.Token_ID)
   is begin
      Default_Synthesize (New_Token, Source, To_ID);
   end Default_Synthesize_Class;

end OpenToken.Token.Nonterminal;
