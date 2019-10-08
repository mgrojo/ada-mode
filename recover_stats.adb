--  Abstract :
--
--  Summarize ada-mode recover log.
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.Parse.LR;
procedure Recover_Stats
is
   subtype Strategies is WisiToken.Parse.LR.Strategies;

   File : File_Type;

   Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (",)");

   type Strategy_Counts is array (Strategies) of Natural;

   Strat_Counts_Total    : Strategy_Counts := (others => 0);
   Strat_Counts_Present  : Strategy_Counts := (others => 0); -- 1 per recover event if used
   Ignore_Error          : Integer         := 0;             -- ie, error is name mismatch.
   Recover_Count_Total   : Integer         := 0;             -- sum of all strategy counts
   Recover_Count_Present : Integer         := 0;             -- 1 per recover event
   Fail_Count            : Integer         := 0;             -- for all reasons
   Fail_Enqueue_Limit    : Integer         := 0;
   Fail_No_Configs_Left  : Integer         := 0;
   Fail_Programmer_Error : Integer         := 0;
   Fail_Other            : Integer         := 0;

begin
   Open (File, In_File, "c:/Projects/org.emacs.ada-mode/recover.log");

   Skip_Line (File); -- header

   loop
      exit when End_Of_File (File);
      Recover_Count_Present := Recover_Count_Present + 1;
      declare
         Line  : constant String := Get_Line (File);
         First : Integer         := 2 + Index (Line, ")(");
         Last  : Integer         := Index (Line, Delimiters, First);

         Strategy_Found : Boolean := False;

         function Line_Eq (Item : in String) return Boolean
         is begin
            return Line (26 .. 26 + Item'Length - 1) = Item;
         end Line_Eq;
      begin
         if Line (21 .. 24) = "FAIL" then
            Fail_Count := Fail_Count + 1;

            if Line_Eq ("NO_CONFIGS_LEFT") then
               Fail_No_Configs_Left := Fail_No_Configs_Left + 1;
            elsif Line_Eq ("ENQUEUE_LIMIT") then
               Fail_Enqueue_Limit := Fail_Enqueue_Limit + 1;
            elsif Line_Eq ("PROGRAMMER_ERROR") then
               Fail_Programmer_Error := Fail_Programmer_Error + 1;
            else
               Fail_Other := Fail_Other + 1;
            end if;

         else
            for I in Strategies loop
               declare
                  Count : constant Integer := Integer'Value (Line (First .. Last - 1));
               begin
                  if Count > 0 then
                     Strategy_Found := True;
                  end if;
                  Recover_Count_Total    := Recover_Count_Total + Count;
                  Strat_Counts_Total (I) := Strat_Counts_Total (I) + Count;
                  if Count > 0 then
                     Strat_Counts_Present (I) := Strat_Counts_Present (I) + 1;
                  end if;
               end;
               First := Last + 2;
               Last := Index (Line, Delimiters, First);
            end loop;
            if not Strategy_Found then
               Ignore_Error := Ignore_Error + 1;
            end if;
         end if;
      end;
   end loop;

   Put_Line ("recover_count present/total:" & Recover_Count_Present'Image & " /" & Recover_Count_Total'Image);
   declare
      use Ada.Strings;
      Label_Field     : String (1 .. 23);
      Count_Field     : String (1 .. 8);
      Percent_Field   : String (1 .. 4);
      Percent_Present : Integer;
      Percent_Total   : Integer;
      procedure Put_Fail (Label : in String; Count : in Integer; Always : in Boolean := False)
      is begin
         if Count > 0 or Always then
            Move (Label, Label_Field); Put (Label_Field & " => ");
            Move (Count'Image, Count_Field, Justify => Right); Put (Count_Field);
            Percent_Present := Integer (100.0 * Float (Count) / Float (Recover_Count_Present));
            Move (Percent_Present'Image & "%", Percent_Field, Justify => Right); Put_Line (Percent_Field);
         end if;
      end Put_Fail;

   begin
      Put_Fail ("FAIL", Fail_Count, Always => True);
      Put_Fail ("FAIL_ENQUEUE_LIMIT", Fail_Enqueue_Limit);
      Put_Fail ("FAIL_NO_CONFIGS_LEFT", Fail_No_Configs_Left);
      Put_Fail ("FAIL_PROGRAMMER_ERROR", Fail_Programmer_Error);
      Put_Fail ("FAIL_OTHER", Fail_Other);
      Put_Fail ("Ignore_Error", Ignore_Error);
      for I in Strategies loop
         Percent_Present := Integer (100.0 * Float (Strat_Counts_Present (I)) / Float (Recover_Count_Present));
         Percent_Total   := Integer (100.0 * Float (Strat_Counts_Total (I)) / Float (Recover_Count_Total));
         Move (I'Image, Label_Field); Put (Label_Field & " => ");
         Move (Strat_Counts_Present (I)'Image, Count_Field, Justify => Right); Put (Count_Field);
         Move (Percent_Present'Image & "%", Percent_Field, Justify => Right); Put (Percent_Field & " /");
         Move (Strat_Counts_Total (I)'Image, Count_Field, Justify => Right); Put (Count_Field);
         Move (Percent_Total'Image & "%", Percent_Field, Justify => Right); Put_Line (Percent_Field);
      end loop;
   end;
end Recover_Stats;
