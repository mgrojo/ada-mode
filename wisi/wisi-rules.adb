--  Abstract :
--
--  Parse the production rules from Input_File, add to List.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Rules
  (Input_File : in     Ada.Text_IO.File_Type;
   Rule_List  : in out Rule_Lists.List)
is
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   type State_Type is (Left_Hand_Side, Production, Action);
   State : State_Type := Left_Hand_Side;

   Rule : Rule_Type;
   RHS  : RHS_Type;
begin
   --  We assume:
   --
   --  1) production lines start with : or |, or are
   --  continuations of previous lines.
   --
   --  2) actions start on a new line
   --
   --  3) terminating semicolon is alone on a line

   loop
      declare
         Line  : constant String  := Skip_Comments (Input_File);
         Colon : constant Integer := Index (Pattern => ":", Source => Line);
         Bar   : constant Integer := Index (Pattern => "|", Source => Line);
         Paren : constant Integer := Index (Pattern => "(", Source => Line);
         Semi  :  Integer         := Index (Pattern => ";", Source => Line);

         Non_Blank : Integer := Index_Non_Blank
           (Line, From =>
              (if Colon > 0 then Colon + 1
               elsif Bar > 0 then Bar + 1
               else Line'First));

         procedure Parse_Production
         is
            Last : Integer := Non_Blank;
         begin
            RHS.Production.Clear;
            RHS.Action.Clear;
            loop
               Last := -1 + Index (Pattern => " ", Source => Line, From => Non_Blank);
               if Last = -1 then Last := Line'Last; end if;

               RHS.Production.Append (Line (Non_Blank .. Last));

               exit when Last = Line'Last;
               Non_Blank := Index_Non_Blank (Line, Last + 2);
            end loop;
         end Parse_Production;

      begin
         exit when Line = "%%";

         if Semi > 0 and Non_Blank /= Semi then
            --  ignore trailing elisp comments in rules
            Semi := 0;
         end if;

         case State is
         when Left_Hand_Side =>
            Rule.Left_Hand_Side := +Trim
              (Line (Non_Blank .. (if Colon > 0 then Colon - 1 else Line'Last)), Ada.Strings.Right);

            Rule.Right_Hand_Sides.Clear;

            State := Production;

         when Production =>

            if Bar > 0 then
               Rule.Right_Hand_Sides.Append (RHS);
               Parse_Production;

            elsif Paren > 0 then
               State       := Action;
               RHS.Action := RHS.Action + Line;

            elsif Semi > 0 then
               Rule.Right_Hand_Sides.Append (RHS);
               Rule_List.Append (Rule);
               State := Left_Hand_Side;

            else
               --  first production
               Parse_Production;
            end if;

         when Action =>
            if Semi > 0 then
               Rule.Right_Hand_Sides.Append (RHS);
               Rule_List.Append (Rule);
               State := Left_Hand_Side;

            elsif Bar > 0 then
               Rule.Right_Hand_Sides.Append (RHS);
               State := Production;
               RHS.Production.Clear;
               RHS.Action.Clear;
               Parse_Production;

            else
               RHS.Action := RHS.Action + Line;
            end if;
         end case;
      end;
   end loop;
end Wisi.Rules;
