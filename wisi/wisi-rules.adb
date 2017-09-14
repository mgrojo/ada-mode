--  Abstract :
--
--  Parse the production rules from Input_File, add to List.
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Rules
  (Input_File      : in     Standard.Ada.Text_IO.File_Type;
   Output_Language : in     Valid_Output_Language;
   Lexer           : in     Lexer_Type;
   Rule_List       : in out Rule_Lists.List;
   Rule_Count      :    out Integer;
   Action_Count    :    out Integer)
is
   use Standard.Ada.Strings;
   use Standard.Ada.Strings.Fixed;
   use Standard.Ada.Strings.Unbounded;

   If_Str     : constant String := "%if lexer =";
   End_If_Str : constant String := "%end if";
   If_Active  : Boolean         := False;

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
      if Output_Language not in Elisp | Ada_Emacs then
         return;
      end if;

      --  Verify that integers in Line are valid token numbers
      for I in Line'Range loop
         case Line (I) is
         when '0' .. '9' =>
            if Last_Char = ' ' then
               First := I;
            end if;

         when ' ' | ']' | ')' | ';' =>
            --  We include ';' here because we have one unit test that has Ada
            --  syntax actions but tests output_language Emacs.

            if First > 0 then
               Number := Integer'Value (Line (First .. I - 1));
               if Number > Token_Count then
                  raise Syntax_Error with "token number " & Line (First .. I - 1) &
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
   --  Actions start on a new line starting with (, and are terminated
   --  by ). Those delimiters are stripped here.

   Rule_Count   := 0;
   Action_Count := 0;

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
               Rule_Count := Rule_Count + 1;

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
               when '%' =>
                  Need_New_Line := True;

                  if Line'Length > If_Str'Length and then Line (Cursor .. Cursor + If_Str'Length - 1) = If_Str then
                     declare
                        Value_First : constant Integer := Index_Non_Blank (Line, If_Str'Length + 1);
                     begin
                        If_Active := Lexer /= To_Lexer (Line (Value_First .. Line'Last));
                     end;
                  elsif Line'Length = End_If_Str'Length and then
                    Line (Cursor .. Cursor + End_If_Str'Length - 1) = End_If_Str
                  then
                     If_Active := False;
                  else
                     raise Syntax_Error with "invalid directive";
                  end if;

               when '(' =>
                  State         := Action;
                  Need_New_Line := True;
                  Paren_Count   := 0;
                  Bracket_Count := 0;

                  Check_Numbers (Line);
                  Update_Paren_Count (Line);

                  if Output_Language in Elisp | Ada_Emacs then
                     --  keep parens
                     RHS.Action := RHS.Action + Line;

                  elsif Paren_Count = 0 then
                     --  Current line also has the terminating ')'; assume no trailing whitespace
                     RHS.Action := RHS.Action + Line (Line'First + 1 .. Line'Last - 1);
                  else
                     RHS.Action := RHS.Action + Line (Line'First + 1 .. Line'Last);
                  end if;

                  Action_Count := Action_Count + 1;

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
               if Paren_Count = 0 then
                  --  Current line has the terminating ')'
                  case Line (Cursor) is
                  when ';' =>
                     Rule.Right_Hand_Sides.Append (RHS);
                     Rule_List.Append (Rule);
                     State         := Left_Hand_Side;
                     Need_New_Line := True;

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

                  when others =>
                     raise Syntax_Error with "expecting ';' or '|'";
                  end case;

               else
                  Update_Paren_Count (Line);

                  if Output_Language in Elisp | Ada_Emacs then
                     --  keep parens
                     RHS.Action := RHS.Action + Line;

                  elsif Paren_Count = 0 then
                     --  Current line has the terminating ')'; assume no trailing whitespace
                     RHS.Action := RHS.Action + Line (Line'First .. Line'Last - 1);
                  else
                     RHS.Action := RHS.Action + Line;
                  end if;

                  Need_New_Line := True;

                  Check_Numbers (Line);
               end if;
            end case;

         end Parse_State;

      begin
         exit when Line = "%%";
         if If_Active then
            If_Active := Line /= End_If_Str;
         else
            loop
               Parse_State;
               exit when Need_New_Line;
            end loop;
         end if;
      exception
      when E : Syntax_Error =>
         Error := True;
         declare
            use Standard.Ada.Exceptions;
         begin
            Put_Error (Input_File, Exception_Message (E));
         end;
      when E : others =>
         declare
            use Standard.Ada.Exceptions;
         begin
            Put_Error (Input_File, "unhandled exception " & Exception_Name (E));
         end;
         raise Syntax_Error;
      end;
   end loop;

   if Error then
      raise Syntax_Error;
   end if;
end Wisi.Rules;
