--  Abstract :
--
--  Parse the production rules from Input_File, add to List.
--
--  Copyright (C) 2012 - 2014 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Rules
  (Input_File : in     Standard.Ada.Text_IO.File_Type;
   Rule_List  : in out Rule_Lists.List)
is
   use Standard.Ada.Strings;
   use Standard.Ada.Strings.Fixed;
   use Standard.Ada.Strings.Unbounded;

   type State_Type is (Left_Hand_Side, Production, Action);
   State : State_Type := Left_Hand_Side;

   Paren_Count   : Integer; --  For reporting unbalanced parens
   Bracket_Count : Integer; -- ""
   Token_Count   : Integer; -- for reporting out of range tokens

   Rule : Rule_Type;
   RHS  : RHS_Type;

   Error : Boolean := False;

   procedure Check_Numbers (Line : in String)
   is
      Number    : Integer   := 0;
      First     : Integer   := 0;
      Last_Char : Character := Line (Line'First);
   begin
      for I in Line'Range loop
         case Line (I) is
         when '0' .. '9' =>
            if Last_Char = ' ' then
               First := I;
            end if;

         when ' ' =>
            if First > 0 then
               Number := Integer'Value (Line (First .. I));
               if Number > Token_Count then
                  raise Syntax_Error with "token number " & Line (First .. I) &
                    " out of range 1 .." & Integer'Image (Token_Count);
               end if;
               First := 0;
            end if;

         when others =>
            null;
         end case;
         Last_Char := Line (I);
      end loop;
   end Check_Numbers;

   procedure Update_Paren_Count (Line : in String)
   is begin
      for I in Line'Range loop
         case Line (I) is
         when '(' => Paren_Count := Paren_Count + 1;
         when ')' => Paren_Count := Paren_Count - 1;
         when '[' => Bracket_Count := Bracket_Count + 1;
         when ']' => Bracket_Count := Bracket_Count - 1;
         when others => null;
         end case;
      end loop;
   end Update_Paren_Count;

begin
   --  We assume actions start on a new line starting with either ` or
   --  (, and are terminated by ; on a new line.

   loop
      declare
         Line          : constant String := Skip_Comments (Input_File);
         Cursor        : Integer         := Line'First;
         Need_New_Line : Boolean         := False;

         procedure Parse_Production
         is
            use Standard.Ada.Strings.Maps;
            Last : Integer;
         begin
            Last := -1 + Index (Set => To_Set (" |;"), Source => Line, From => Cursor);

            if Last = -1 then Last := Line'Last; end if;

            RHS.Production.Append (Line (Cursor .. Last));

            if Last = Line'Last then
               Need_New_Line := True;
            else
               Cursor := Index_Non_Blank (Source => Line, From => Last + 1);
            end if;
         end Parse_Production;

         procedure Parse_State
         is begin
            case State is
            when Left_Hand_Side =>
               Cursor := -1 + Index (Set => Standard.Ada.Strings.Maps.To_Set (" :"), Source => Line);
               if Cursor = -1 then Cursor := Line'Last; end if;

               Rule.Left_Hand_Side := +Line (Line'First .. Cursor);
               Rule.Source_Line    := Standard.Ada.Text_IO.Line (Input_File) - 1;

               Token_Count := 0;

               State := Production;

               Cursor := Index (Pattern => ":", Source => Line);
               Need_New_Line := Cursor = 0;

            when Production =>

               case Line (Cursor) is
               when '`' | '(' =>
                  State         := Action;
                  RHS.Action    := RHS.Action + Line;
                  Need_New_Line := True;
                  Paren_Count   := 0;
                  Bracket_Count := 0;

                  Check_Numbers (Line);
                  Update_Paren_Count (Line);

               when ';' =>
                  Rule.Right_Hand_Sides.Append (RHS);
                  Rule_List.Append (Rule);
                  State         := Left_Hand_Side;
                  Need_New_Line := True;

               when '|' =>
                  Rule.Right_Hand_Sides.Append (RHS);
                  RHS.Production.Clear;
                  RHS.Action.Clear;

                  Token_Count   := 0;

                  Cursor := Index_Non_Blank (Line, From => Cursor + 1);
                  Need_New_Line := Cursor = 0;

               when ':' =>
                  Rule.Right_Hand_Sides.Clear;
                  RHS.Production.Clear;
                  RHS.Action.Clear;

                  Cursor := Index_Non_Blank (Line, From => Cursor + 1);
                  Need_New_Line := Cursor = 0;

               when others =>
                  Parse_Production;
                  Token_Count := Token_Count + 1;

               end case;

            when Action =>
               case Line (Cursor) is
               when ';' =>
                  Rule.Right_Hand_Sides.Append (RHS);
                  Rule_List.Append (Rule);
                  State         := Left_Hand_Side;
                  Need_New_Line := True;

                  if Paren_Count /= 0 then
                     raise Syntax_Error with "unbalanced parens in action";
                  end if;

                  if Bracket_Count /= 0 then
                     raise Syntax_Error with "unbalanced brackets in action";
                  end if;

               when '|' =>
                  Rule.Right_Hand_Sides.Append (RHS);
                  State := Production;
                  RHS.Production.Clear;
                  RHS.Action.Clear;

                  Cursor := Index_Non_Blank (Line, From => Cursor + 1);
                  Need_New_Line := Cursor = 0;

                  if Paren_Count /= 0 then
                     raise Syntax_Error with "unbalanced parens in action";
                  end if;

                  if Bracket_Count /= 0 then
                     raise Syntax_Error with "unbalanced brackets in action";
                  end if;

               when others =>
                  RHS.Action    := RHS.Action + Line;
                  Need_New_Line := True;

                  Check_Numbers (Line);
                  Update_Paren_Count (Line);
               end case;
            end case;

         end Parse_State;

      begin
         exit when Line = "%%";

         loop
            Parse_State;
            exit when Need_New_Line;
         end loop;
      exception
      when E : Syntax_Error =>
         Error := True;
         declare
            use Standard.Ada.Exceptions;
         begin
            Standard.Ada.Text_IO.Put_Line
              (Name (Input_File) & ":" &
                 Trim (Standard.Ada.Text_IO.Count'Image (Standard.Ada.Text_IO.Line (Input_File)), Left) & ":0: " &
                 Exception_Message (E));
         end;
      when E : others =>
         declare
            use Standard.Ada.Exceptions;
         begin
            Standard.Ada.Text_IO.Put_Line
              (Name (Input_File) & ":" &
                 Trim (Standard.Ada.Text_IO.Count'Image (Standard.Ada.Text_IO.Line (Input_File)), Left) &
                 ":0: unhandled exception " & Exception_Name (E));
         end;
         raise Syntax_Error;
      end;
   end loop;

   if Error then
      raise Syntax_Error;
   end if;
end Wisi.Rules;
