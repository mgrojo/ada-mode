-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
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
package body ASU_Example_5_10_RD_No_Mixin is

   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class)
   is begin
      Match.Value := From.Value;

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put ("Build_Selection:" & Integer'Image (Match.Value));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Selection;

   procedure Build_Print
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Match);
      use OpenToken.Token.Linked_List;
      I          : constant List_Iterator := First (Using); -- E
      Left_Token : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (I).all);
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Left_Token.Value));
   end Build_Print;

   procedure Build_Plus
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;

      I    : List_Iterator          := First (Using);
      Left : constant List_Iterator := I;
   begin
      Next_Token (I); -- +
      Next_Token (I); -- T

      declare
         Left_Token  : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (Left).all);
         Right_Token : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (I).all);
      begin
         Match.Value := Left_Token.Value + Right_Token.Value;

         if OpenToken.Token.Trace_Parse then
            OpenToken.Token.Trace_Put
              ("Build_Plus:" & Integer'Image (Left_Token.Value) & " +" &
                 Integer'Image (Right_Token.Value) & " =>" & Integer'Image (Match.Value));
         end if;
      end;

   end Build_Plus;

   procedure Build_Multiply
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;

      I      : List_Iterator          := Initial_Iterator (Using); -- T
      Left   : constant List_Iterator := I;
   begin
      Next_Token (I); -- *

      if Master_Token.ID (Master_Token.Class (Token_Handle (I).all)) /= Multiply_ID then
         raise OpenToken.Programmer_Error;
      end if;

      Next_Token (I); -- F

      declare
         Left_Token  : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (Left).all);
         Right_Token : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (I).all);
      begin
         Match.Value := Left_Token.Value * Right_Token.Value;

         if OpenToken.Token.Trace_Parse then
            OpenToken.Token.Trace_Put
              ("Build_Multiply:" & Integer'Image (Left_Token.Value) & " *" &
                 Integer'Image (Right_Token.Value) & " =>" & Integer'Image (Match.Value));
            Ada.Text_IO.New_Line;
         end if;
      end;

   end Build_Multiply;

   procedure Build_Parens
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;

      I : List_Iterator := Initial_Iterator (Using); -- (
   begin
      Next_Token (I); -- E

      declare
         Middle_Token : Integer_Selection.Instance renames Integer_Selection.Instance (Token_Handle (I).all);
      begin
         Match.Value := Middle_Token.Value;
      end;

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put ("Build_Parens:" & Integer'Image (Match.Value));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Parens;

end ASU_Example_5_10_RD_No_Mixin;
