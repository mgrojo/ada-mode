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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Gen_Stacks_Bounded;
package body ASU_Example_5_10_RD_No_Mixin is

   package Integer_Stacks is new Gen_Stacks_Bounded
     (Item_Type => Integer,
      Null_Item => -1);

   Stack : Integer_Stacks.Stack_Type (100);

   function Image (Stack : in Integer_Stacks.Stack_Type) return String
   is
      use Ada.Strings.Unbounded;
      use Integer_Stacks;
      Result : Unbounded_String;
   begin
      for I in 1 .. Depth (Stack) loop
         Result := Result & Integer'Image (Peek (Stack, I));
         if I /= Depth (Stack) then
            Result := Result & ", ";
         end if;
      end loop;
      return To_String (Result);
   end Image;

   procedure Clear_Stack
   is begin
      Integer_Stacks.Clear (Stack);
   end Clear_Stack;

   procedure Build_Selection
     (Match : in out OpenToken.Token.Selection.Instance;
      From  : in     OpenToken.Token.Class)
   is
      pragma Unreferenced (From);
      pragma Unreferenced (Match);
      use Integer_Stacks;
   begin
      --  No stack operations; just trace

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put
           ("Build_Selection:" & Integer'Image (Top (Stack)));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Selection;

   procedure Build_Print
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Using);
      pragma Unreferenced (Match);
      use Integer_Stacks;
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Top (Stack)));
      Pop (Stack);

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put ("Stack: " & Image (Stack));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Print;

   procedure Build_Plus
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Using);
      pragma Unreferenced (Match);
      use Integer_Stacks;
      Left   : constant Integer := Top (Stack);
      Right  : Integer;
      Result : Integer;
   begin
      Pop (Stack);
      Right := Top (Stack);
      Pop (Stack);

      Result := Left + Right;

      Push (Stack, Result);

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put
           ("Build_Plus:" & Integer'Image (Left) & " +" & Integer'Image (Right) & " =>" & Integer'Image (Result));
         Ada.Text_IO.New_Line;
         OpenToken.Token.Trace_Put ("Stack: " & Image (Stack));
         Ada.Text_IO.New_Line;
      end if;

   end Build_Plus;

   procedure Build_Multiply
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Using);
      pragma Unreferenced (Match);
      use Integer_Stacks;
      Left   : constant Integer := Top (Stack);
      Right  : Integer;
      Result : Integer;
   begin
      Pop (Stack);
      Right := Top (Stack);
      Pop (Stack);

      Result := Left * Right;

      Push (Stack, Result);

      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put
           ("Build_Multiply:" & Integer'Image (Left) & " *" & Integer'Image (Right) & " =>" & Integer'Image (Result));
         Ada.Text_IO.New_Line;
         OpenToken.Token.Trace_Put ("Stack: " & Image (Stack));
         Ada.Text_IO.New_Line;
      end if;

   end Build_Multiply;

   procedure Build_Parens
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Using);
      pragma Unreferenced (Match);
      use Integer_Stacks;
   begin
      --  No stack operations; just trace
      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put ("Build_Parens:" & Integer'Image (Top (Stack)));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Parens;

   procedure Build_Integer (Token : in out Master_Token.Instance'Class)
   is
      Int : Integer_Token.Instance renames Integer_Token.Instance (Token);
   begin
      Integer_Stacks.Push (Stack, Int.Value);
      if OpenToken.Token.Trace_Parse then
         OpenToken.Token.Trace_Put ("Stack: " & Image (Stack));
         Ada.Text_IO.New_Line;
      end if;
   end Build_Integer;

end ASU_Example_5_10_RD_No_Mixin;
