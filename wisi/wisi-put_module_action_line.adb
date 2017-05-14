--  Abstract :
--
--  Translate Line into an Ada fragment for an Ada Emacs Module call
--  to a wisi action, output to Standard_Output.
--
--  Separate subprogram to allow unit test.
--
--  Line must contain a complete elisp form that is a single call to a
--  wisi action, with two exceptions:
--
--  - the first line in a multi-line action is '(progn'; it is
--    ignored.
--
--  - the last line in a multi-line action has an extra closing paren;
--    it is ignored.
--
--  - There must be no extra spaces in the line.
--
--  Copyright (C) 2015, 2017  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils; use Wisi.Utils;
with Wisi.Output_Elisp_Common;
procedure Wisi.Put_Module_Action_Line (Line : in String)
is
   --  See test/wisi_module_action_test for typical lines.

   function Item_Image (Item : in String) return String
   is
      use Standard.Ada.Characters.Handling;
   begin
      if Is_Digit (Item (Item'First)) then
         return "Elisp_Numbers (" & Item & ")";
      else
         return "Elisp_Symbols (" & Wisi.Output_Elisp_Common.Elisp_Name_To_Ada (Item) & ")";
      end if;
   end Item_Image;

   function Scan_Brackets (From : in Integer) return Integer
   is
      Bracket_Count : Integer := 1;
   begin
      for I in From .. Line'Last loop
         if Line (I) = ']' then
            Bracket_Count := Bracket_Count - 1;
            if Bracket_Count = 0 then
               return I;
            end if;
         elsif Line (I) = '[' then
            Bracket_Count := Bracket_Count + 1;
         end if;
      end loop;
      raise Programmer_Error with "mismatched brackets: '" & Line & "'";
   end Scan_Brackets;

   type State_Type is (Start, First_Item, Items, Closed_Bracket);
   State : State_Type := Start;

   Post_Closed_Bracket_State : State_Type;

   First : Integer := Line'First;
   Last  : Integer := Line'First - 1;
begin
   if Line = "(progn" then
      return;
   elsif Line (First) /= '(' or Line (Line'Last) /= ')' then
      raise Programmer_Error with "To_Module_Action_Line: unsupported line '" & Line & "'";
   end if;

   for I in Line'Range loop

      case State is
      when Start =>
         case Line (I) is
         when '(' =>
            Indent_Line ("Funcall");
            Indent_Line ("  (Env,");
            Indent := Indent + 3;
            First  := I + 1;
            Last   := I;

         when ' ' =>
            Indent_Line (Item_Image (Line (First .. Last)) & ',');
            State := First_Item;
            First := I + 1;
            Last  := I;

         when others =>
            Last := Last + 1;
         end case;

      when First_Item =>
         case Line (I) is
         when '[' =>
            if Line (Scan_Brackets (I + 1) + 1) = ')' then
               Indent_Line ("(1 => Vector");
            else
               Indent_Line ("(Vector");
            end if;
            Indent_Line ("   (Env,");
            Indent := Indent + 4;
            First  := I + 1;
            Last   := I;

         when ' ' =>
            Indent_Line ('(' & Item_Image (Line (First .. Last)) & ',');
            Indent := Indent + 1;
            State  := Items;
            First  := I + 1;
            Last   := I;

         when ']' =>
            Set_Col (Indent);
            Put ("(1 => " & Item_Image (Line (First .. Last)) & "))");
            Indent := Indent - 4;
            First  := I + 1;
            Last   := I;
            State  := Closed_Bracket;

            Post_Closed_Bracket_State := First_Item;

         when ')' =>
            Indent_Line ("(1 => " & Item_Image (Line (First .. Last)) & "));");
            Indent := Indent - 3;
            return;

         when others =>
            Last := Last + 1;
         end case;

      when Items =>
         case Line (I) is
         when '[' =>
            Indent_Line ("Vector");
            Indent_Line ("  (Env,");
            Indent := Indent + 3;
            State  := First_Item;
            First  := I + 1;
            Last   := I;

         when ' ' =>
            Indent_Line (Item_Image (Line (First .. Last)) & ',');
            First := I + 1;
            Last  := I;

         when ']' =>
            Set_Col (Indent);
            Put (Item_Image (Line (First .. Last)) & "))");
            Indent := Indent - 4;
            First  := I + 1;
            Last   := I;
            State  := Closed_Bracket;

            Post_Closed_Bracket_State := Items;

         when ')' =>
            Indent_Line (Item_Image (Line (First .. Last)) & "));");
            Indent := Indent - 4;
            return;

         when others =>
            Last := Last + 1;
         end case;

      when Closed_Bracket =>
         case Line (I) is
         when ' ' =>
            Put_Line (",");
            State := Post_Closed_Bracket_State;
            First := I + 1;
            Last  := I;

         when ']' =>
            Put ("))");
            State  := Closed_Bracket;
            Indent := Indent - 4;
            First  := I + 1;
            Last   := I;

            Post_Closed_Bracket_State := Closed_Bracket;

         when ')' =>
            Put_Line ("));");
            Indent := Indent - 4;
            return;

         when others =>
            Last := Last + 1;
         end case;
      end case;
   end loop;

exception
when Programmer_Error =>
   raise;
when E : others =>
   declare
      use Standard.Ada.Exceptions;
   begin
      raise Programmer_Error with
        "Put_Module_Action_Line: " & Line & "': " & Exception_Name (E) & " " & Exception_Message (E);
   end;
end Wisi.Put_Module_Action_Line;
