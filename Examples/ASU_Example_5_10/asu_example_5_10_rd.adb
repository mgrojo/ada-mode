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
-------------------------------------------------------------------------------

with Ada.Text_IO;
package body ASU_Example_5_10_RD is

   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class)
   is begin
      Match.Value := From.Value;
   end Build_Selection;

   procedure Build_Parens
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;
      Iterator : List_Iterator := First (Using); -- (
   begin
      Next_Token (Iterator); -- E
      Match.Value := Integer_Token.Handle (Token_Handle (Iterator)).Value;
   end Build_Parens;

   procedure Build_Print
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;
      Iterator : constant List_Iterator := First (Using); -- E
   begin
      Match.Value := Integer_Token.Handle (OpenToken.Token.Linked_List.Token_Handle (Iterator)).Value;
      Ada.Text_IO.Put_Line (Integer'Image (Match.Value));
   end Build_Print;

   procedure Init_Plus (Match : in out Operation_List.Instance)
   is begin
      Match.Value := 0;
   end Init_Plus;

   procedure Plus_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      Match.Value := Match.Value + Element.Value;
   end Plus_Element;

   procedure Init_Multiply (Match : in out Operation_List.Instance)
   is begin
      Match.Value := 1;
   end Init_Multiply;

   procedure Multiply_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      Match.Value := Match.Value * Element.Value;
   end Multiply_Element;

end ASU_Example_5_10_RD;
