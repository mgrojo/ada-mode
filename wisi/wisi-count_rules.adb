--  Abstract :
--
--  Output the number of grammar rules in the input file
--
--  Copyright (C) 2016 Stephen Leake All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Declarations;
with Wisi.Prologue;
with Wisi.Rules;
procedure Wisi.Count_Rules
is
   procedure Put_Usage
   is begin
      Put_Line ("wisi-count_rules {wisi grammar file}");
   end Put_Usage;

   Input_File   : Standard.Ada.Text_IO.File_Type;
   Prologue     : String_Lists.List;
   pragma Unreferenced (Prologue);
   Keywords     : String_Pair_Lists.List;
   Tokens       : Token_Lists.List;
   Start_Token  : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts    : Conflict_Lists.List;
   Rules        : Rule_Lists.List;
   Rule_Count   : Integer;
   Action_Count : Integer;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         Open (Input_File, In_File, Argument (1));
      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
      end case;
   end;

   --  We have to skip the prologue and the declarations; easiest to just call these
   Wisi.Prologue (Input_File, Prologue);
   Wisi.Declarations (Input_File, Keywords, Tokens, Start_Token, Conflicts);

   Wisi.Rules (Input_File, Rules, Rule_Count, Action_Count);
   Put_Line ("Rule count:  " & Integer'Image (Rule_Count));
   Put_Line ("Rules'length:" & Ada.Containers.Count_Type'Image (Rule_Lists.Length (Rules)));
end Wisi.Count_Rules;
