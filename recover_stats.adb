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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Long_Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Stats.Gen_Image;
with SAL.Long_Float_Stats;
with WisiToken.Parse.LR;
procedure Recover_Stats
is
   subtype Strategies is WisiToken.Parse.LR.Strategies;

   File : File_Type;

   Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (",) ");
   Number     : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0123456789");

   type Strategy_Counts is array (Strategies) of Natural;

   --  FIXME: post-parse action counts
   --  FIXME: increase parser number count

   Partial_Count : Integer := 0;
   --  Count of recover events with partial parse active

   Enqueue_Stats : SAL.Long_Float_Stats.Stats_Type;
   Check_Stats   : SAL.Long_Float_Stats.Stats_Type;

   Strat_Counts_Total   : Strategy_Counts := (others => 0);
   Strat_Counts_Present : Strategy_Counts := (others => 0);
   --  1 per recover event if used

   Ignore_Error : Integer := 0;
   --  ie, error is name mismatch.

   Recover_Count_Total : Integer := 0;
   --  Sum of all strategy counts

   Recover_Count_Present : Integer := 0;
   --  1 per recover event

   Fail_Count            : Integer := 0; -- for all reasons
   Fail_Enqueue_Limit    : Integer := 0;
   Fail_No_Configs_Left  : Integer := 0;
   Fail_Programmer_Error : Integer := 0;
   Fail_Other            : Integer := 0;

begin
   Open (File, In_File, Ada.Command_Line.Argument (1));

   loop
      exit when End_Of_File (File);
      Recover_Count_Present := Recover_Count_Present + 1;
      declare
         Line  : constant String := Get_Line (File);
         First : Integer         := Index (Line, " "); -- after date
         Last  : Integer;

         Strategy_Found : Boolean := False;

         function Line_Eq (Item : in String) return Boolean
         is begin
            return Line (First .. First + Item'Length - 1) = Item;
         end Line_Eq;

         function Next_Integer return Integer
         is begin
            Find_Token
              (Line, Number,
               From  => Last + 1,
               Test  => Ada.Strings.Inside,
               First => First,
               Last  => Last);
            return Integer'Value (Line (First .. Last));
         exception
         when Constraint_Error =>
            raise Constraint_Error with "bad integer '" & Line (First .. Last - 1) & "' " &
              Ada.Text_IO.Count'Image (Ada.Text_IO.Line (File) - 1) & First'Image & Last'Image;
         end Next_Integer;

         function Next_Boolean return  Boolean
         is begin
            First := Last + 2;
            Last  := Index (Line, Delimiters, First);
            return Boolean'Value (Line (First .. Last - 1));
         end Next_Boolean;

      begin
         First := Index (Line, " ", First + 1); -- after time
         Last  := Index (Line, " ", First + 1); -- after Partial_Parse_Active
         if Boolean'Value (Line (First + 1 .. Last - 1)) then
            Partial_Count := Partial_Count + 1;
         end if;

         First := Last + 1;
         if Line (First .. First + 3) = "FAIL" then
            Fail_Count := Fail_Count + 1;
            First := First + 4;

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
            --  Handle parser enqueue, check, success
            Last := 1 + Index (Line, "(", First);
            loop
               declare
                  Enqueue_Count : constant Integer := Next_Integer;
                  Check_Count   : constant Integer := Next_Integer;
                  Success       : constant Boolean := Next_Boolean;
               begin
                  if Success then
                     Enqueue_Stats.Accumulate (Long_Float (Enqueue_Count));
                     Check_Stats.Accumulate (Long_Float (Check_Count));
                  end if;
               end;
               exit when Line (Last) = ')';
            end loop;

            Last := Last + 1; -- First strategy
            for I in Strategies loop
               declare
                  Count : constant Integer := Next_Integer;
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
            end loop;
            if not Strategy_Found then
               Ignore_Error := Ignore_Error + 1;
            end if;
         end if;
      end;
   end loop;

   declare
      use Ada.Strings;
      Label_Field     : String (1 .. 23);
      Count_Field     : String (1 .. 8);
      Percent_Field   : String (1 .. 4);
      Percent_Present : Integer;
      Percent_Total   : Integer;

      procedure Put_If (Label : in String; Count : in Integer; Always : in Boolean := False)
      is begin
         if Count > 0 or Always then
            Move (Label, Label_Field); Put (Label_Field & " => ");
            Move (Count'Image, Count_Field, Justify => Right); Put (Count_Field);
            Percent_Present := Integer (100.0 * Float (Count) / Float (Recover_Count_Present));
            Move (Percent_Present'Image & "%", Percent_Field, Justify => Right); Put_Line (Percent_Field);
         end if;
      end Put_If;

      package Stats_Image is new SAL.Long_Float_Stats.Gen_Image
        (Real_IO           => Ada.Long_Float_Text_IO,
         Default_Mean_Fore => 7,
         Default_Mean_Aft  => 0,
         Default_Mean_Exp  => 0,
         Default_Sd_Fore   => 7,
         Default_Sd_Aft    => 1,
         Default_Sd_Exp    => 0);
   begin
      Put_Line ("           mean        std. dev.    min     max");
      Put_Line ("Enqueue: " & Stats_Image.Image (Enqueue_Stats.Display));
      Put_Line ("Check:   " & Stats_Image.Image (Check_Stats.Display));
      Put_If ("Partial parse", Partial_Count, Always => True);
      Put_If ("FAIL", Fail_Count, Always => True);
      Put_If ("FAIL_ENQUEUE_LIMIT", Fail_Enqueue_Limit);
      Put_If ("FAIL_NO_CONFIGS_LEFT", Fail_No_Configs_Left);
      Put_If ("FAIL_PROGRAMMER_ERROR", Fail_Programmer_Error);
      Put_If ("FAIL_OTHER", Fail_Other);
      Put_If ("Ignore_Error", Ignore_Error);
      Put_Line ("recover_count present/total:" & Recover_Count_Present'Image & " /" & Recover_Count_Total'Image);
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
exception
when E : others =>
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Recover_Stats;
