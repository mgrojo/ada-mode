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
-------------------------------------------------------------------------------
with Ada.Text_IO;

package body Simple_Integer_Token is

   function Get (ID     : in Token.Token_ID;
                 Value  : in Integer := 0) return Instance'Class is
   begin
      return Instance'Class (Instance'(Nonterminal.Instance (Nonterminal.Get (ID)) with Value => Value));
   end Get;

   function Value (Subject : in Instance) return Integer is
   begin
      return Subject.Value;
   end Value;

   overriding procedure Synthesize_By_Copying
     (New_Token :    out Instance;
      Source    : in     Token.Instance'Class;
      To_ID     : in     Token.Token_ID)
   is begin
      if Source in Integer_Literal.Class then
         New_Token := (Nonterminal.Instance (Nonterminal.Get (To_ID)) with Integer_Literal.Class (Source).Value);

      elsif Source in Class then
         New_Token := (Nonterminal.Instance (Nonterminal.Get (To_ID)) with Instance (Source).Value);

      else
         raise Nonterminal.Invalid_Synth_Argument with
            "Token " & Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
              "solely from a " & Token.Token_ID'Image (Token.ID (Source)) & ".";
      end if;
   end Synthesize_By_Copying;

   procedure Synthesize_Add
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token.Token_ID)
   is
      use Token_List;
      Left  : constant List_Iterator := Initial_Iterator (Source);
      Right : List_Iterator := Initial_Iterator (Source);

   begin
      --  Move "Right" over to the third item;
      Next_Token (Right);
      Next_Token (Right);

      New_Token := Class
        (Instance'
           (Token.Instance (Token.Get (To_ID)) with
              (Value (Class (Token_Handle (Left).all)) +
                 Value (Class (Token_Handle (Right).all)))));
   exception
   when Constraint_Error =>
      raise Nonterminal.Invalid_Synth_Argument with
         "Token " & Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
           "from a " &
           Token.Token_ID'Image (Token.ID (Token_Handle (Left).all)) &
           " and a " &
           Token.Token_ID'Image (Token.ID (Token_Handle (Right).all)) &
           ".";
   end Synthesize_Add;

   procedure Synthesize_Multiply
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token.Token_ID)
   is
      use Token_List;
      Left  : constant List_Iterator := Initial_Iterator (Source);
      Right : List_Iterator := Initial_Iterator (Source);

   begin
      --  Move "Right" over to the third item;
      Next_Token (Right);
      Next_Token (Right);

      New_Token := Class
        (Instance'
           (Token.Instance (Token.Get (To_ID)) with
              (Value (Class (Token_Handle (Left).all)) *
                 Value (Class (Token_Handle (Right).all)))));
   exception
   when Constraint_Error =>
      raise Nonterminal.Invalid_Synth_Argument with
         "Token " & Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
           "from a " &
           Token.Token_ID'Image (Token.ID (Token_Handle (Left).all)) &
           " and a " &
           Token.Token_ID'Image (Token.ID (Token_Handle (Right).all)) &
           ".";
   end Synthesize_Multiply;

   procedure Synthesize_From_Second_Argument
     (New_Token : out Nonterminal.Class;
      Source    : in  Token_List.Instance'Class;
      To_ID     : in  Token.Token_ID)
   is
      use Token_List;
      Second : List_Iterator := Initial_Iterator (Source);
   begin
      --  Move "Second" over to the second item;
      Next_Token (Second);

      New_Token := Class
        (Instance'(Nonterminal.Instance (Nonterminal.Get (To_ID)) with
                     Class (Token_Handle (Second).all).Value));
   exception
   when Constraint_Error =>
      raise Nonterminal.Invalid_Synth_Argument with
         "Token " & Token.Token_ID'Image (To_ID) & " cannot be synthesized " &
           "solely from a " &
           Token.Token_ID'Image
           (Token.ID (Token_Handle (Second).all)) & ".";
   end Synthesize_From_Second_Argument;

   procedure Print_Value
     (New_Token :    out Nonterminal.Class;
      Source    : in     Token_List.Instance'Class;
      To_ID     : in     Token.Token_ID)
   is begin
      Ada.Text_IO.Put_Line
        (Integer'Image (Value (Class (Token_List.Token_Handle (Token_List.Initial_Iterator (Source)).all))));

      Nonterminal.Synthesize_Self (New_Token, Source, To_ID);
   end Print_Value;

end Simple_Integer_Token;
