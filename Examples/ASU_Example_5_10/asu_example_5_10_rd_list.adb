-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2014 Stephe Leake
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
package body ASU_Example_5_10_RD_List is

   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class)
   is begin
      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put
           ("Build_Selection:" & Integer'Image (Match.Value) & " =>" & Integer'Image (From.Value));
         Ada.Text_IO.New_Line;
      end if;

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

      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put ("Build_Parens:" & Integer'Image (Match.Value));
         Ada.Text_IO.New_Line;
      end if;
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
      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put ("Init_Plus:" & Integer'Image (Match.Value) & " => 0");
         Ada.Text_IO.New_Line;
      end if;
      Match.Value := 0;
   end Init_Plus;

   procedure Plus_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put
           ("Plus_Element:" & Integer'Image (Match.Value) & " +" &
              Integer'Image (Element.Value) & " =>");
      end if;

      Match.Value := Match.Value + Element.Value;

      if OpenToken.Trace_Parse > 0 then
         Ada.Text_IO.Put_Line (Integer'Image (Match.Value));
      end if;
   end Plus_Element;

   procedure Init_Times (Match : in out Operation_List.Instance)
   is begin
      Match.Value := 1;
      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put ("Init_Times:" & Integer'Image (Match.Value) & " => 1");
         Ada.Text_IO.New_Line;
      end if;
   end Init_Times;

   procedure Times_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      if OpenToken.Trace_Parse > 0 then
         OpenToken.Trace_Put
           ("Times_Element:" & Integer'Image (Match.Value) & " *" &
              Integer'Image (Element.Value) & " =>");
      end if;

      Match.Value := Match.Value * Element.Value;

      if OpenToken.Trace_Parse > 0 then
         Ada.Text_IO.Put_Line (Integer'Image (Match.Value));
      end if;
   end Times_Element;

end ASU_Example_5_10_RD_List;
