--EMACSCMD:(setq skip-recase-test t)
------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.12 + 3.11b patches
--                                                                          --
--              Copyright (C) 1998 Ada Core Technologies, Inc.              --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;

package body GNAT.Command_Line is

   package CL renames Ada.Command_Line;

   Colon : constant Character := ':';
   --  Character in the string parameter to Getopt that indicates if the
   --  previous switches expects a parameter

   type Parameter_Type is
      record
         Arg_Num : Positive;
         First   : Positive;
         Last    : Positive;
      end record;
   The_Parameter : Parameter_Type;
   The_Switch    : Parameter_Type;
   --  This type and this variable are provided to store the current switch
   --  and parameter

   Is_Switch : array (1 .. CL.Argument_Count) of Boolean := (others => False);
   --  Indicates wich arguments on the command line are considered not be
   --  switches or parameters to switches (this leaves e.g. the filenames...)

   Current_Argument : Positive := 1;
   --  Number of the current argument parsed on the command line

   Current_Index : Positive := 1;
   --  Index in the current argument of the character to be processed

   Expansion_It : aliased Expansion_Iterator;
   --  When Get_Argument is expanding a file name, this is the iterator used

   In_Expansion : Boolean := False;
   --  True if we are expanding a file

   Switch_Character : Character := '-';
   --  The character at the beginning of the command line arguments,
   --  indicating the beginning of a switch

   Stop_At_First : Boolean := False;
   --  If it is True then Getopt stops at the first non-switch argument

   procedure Set_Parameter
     (Variable : out Parameter_Type;
      Arg_Num  : Positive;
      First    : Positive;
      Last     : Positive);
   pragma Inline (Set_Parameter);
   --  Set the parameter that will be returned by Parameter below

   ---------------
   -- Expansion --
   ---------------

   function Expansion (Iterator : Expansion_Iterator) return String is
      use GNAT.Directory_Operations;
      type Pointer is access all Expansion_Iterator;

      S    : String (1 .. 1024);
      Last : Natural;
      It   : Pointer := Iterator'Unrestricted_Access;

   begin
      loop
         Read (It.Dir, S, Last);

         if Last = 0 then
            Close (It.Dir);
            return String'(1 .. 0 => ' ');
         end if;

         if GNAT.Regexp.Match (S (1 .. Last), Iterator.Regexp) then
            return S (1 .. Last);
         end if;

      end loop;

      return String'(1 .. 0 => ' ');
   end Expansion;

   -----------------
   -- Full_Switch --
   -----------------

   function Full_Switch return String is
   begin
      return CL.Argument (The_Switch.Arg_Num)
        (The_Switch.First .. The_Switch.Last);
   end Full_Switch;

   ------------
   -- Getopt --
   ------------

   function Getopt (Switches : String) return Character is
   begin
      --  If we have already read the last item on the command line

      if Current_Argument > CL.Argument_Count then
         Current_Index := 1;
         return Ascii.NUL;
      end if;

      if Current_Index > CL.Argument (Current_Argument)'Last then
         Current_Argument := Current_Argument + 1;

         Current_Index := 1;

         if Current_Argument > CL.Argument_Count then
            return Ascii.NUL;
         end if;
      end if;

      if Current_Index = 1 then
         if CL.Argument (Current_Argument)(1) /= Switch_Character then

            if Stop_At_First then
               Current_Argument := Positive'Last;
               return Ascii.NUL;

            else
               Current_Argument := Current_Argument + 1;
               return Getopt (Switches);
            end if;
         end if;

         Current_Index := 2;
         Is_Switch (Current_Argument) := True;
      end if;

      declare
         Arg            : String renames CL.Argument (Current_Argument);
         Index_Switches : Natural := 0;
         Max_Length     : Natural := 0;
         Index          : Natural := Switches'First;
         Length         : Natural := 1;
         End_Index      : Natural;

      begin
         while Index <= Switches'Last loop

            --  Search the length of the parameter at this position in Switches

            Length := Index;
            while Length <= Switches'Last
              and then Switches (Length) /= ' '
            loop
               Length := Length + 1;
            end loop;

            if (Switches (Length - 1) = ':'
                  or else Switches (Length - 1) = '?')
              and then Length > Index + 1
            then
               Length := Length - 1;
            end if;

            --  If it is the one we searched, it may be a candidate

            if Current_Index + Length - 1 - Index <= Arg'Last
              and then
              Switches (Index .. Length - 1) =
              Arg (Current_Index .. Current_Index + Length - 1 - Index)
              and then Length - Index > Max_Length
            then
               Index_Switches := Index;
               Max_Length     := Length - Index;
            end if;

            --  Look for the next switch in Switches
            while Index <= Switches'Last
              and then Switches (Index) /= ' ' loop
               Index := Index + 1;
            end loop;
            Index := Index + 1;

         end loop;

         End_Index := Current_Index + Max_Length - 1;

         --  If the switch is not accepted, skip it

         if Index_Switches = 0 then
            Set_Parameter (The_Switch,
                           Arg_Num => Current_Argument,
                           First   => Current_Index,
                           Last    => Current_Index);
            Current_Index := Current_Index + 1;
            raise Invalid_Switch;
         end if;

         Set_Parameter (The_Switch,
                        Arg_Num => Current_Argument,
                        First   => Current_Index,
                        Last    => End_Index);

         --  If switch needs an argument

         if Index_Switches + Max_Length <= Switches'Last
           and then Switches (Index_Switches + Max_Length) = ':'
         then
            if End_Index < Arg'Last then
               Set_Parameter (The_Parameter,
                              Arg_Num => Current_Argument,
                              First   => End_Index + 1,
                              Last    => Arg'Last);
               Current_Index := 1;
               Current_Argument := Current_Argument + 1;

            elsif Current_Argument < CL.Argument_Count then
               Set_Parameter
                 (The_Parameter,
                  Arg_Num => Current_Argument + 1,
                  First   => 1,
                  Last    => CL.Argument (Current_Argument + 1)'Last);
               Current_Index := 1;
               Current_Argument := Current_Argument + 2;
               Is_Switch (Current_Argument - 1) := True;

            else
               Current_Index := End_Index + 1;
               raise Invalid_Parameter;
            end if;

            --  If switch can have an optionnal argument
         elsif Index_Switches + Max_Length <= Switches'Last
           and then Switches (Index_Switches + Max_Length) = '?'
         then
            if End_Index < Arg'Last then
               Set_Parameter (The_Parameter,
                              Arg_Num => Current_Argument,
                              First   => End_Index + 1,
                              Last    => Arg'Last);

            else
               Set_Parameter (The_Parameter,
                              Arg_Num => Current_Argument,
                              First   => 2,
                              Last    => 1);
            end if;

            Current_Index := 1;
            Current_Argument := Current_Argument + 1;

         else
            Current_Index := End_Index + 1;
         end if;

         return Switches (Index_Switches);
      end;
   end Getopt;

   ------------------
   -- Get_Argument --
   ------------------

   function Get_Argument (Do_Expansion : Boolean := False) return String is
      Total : Natural := CL.Argument_Count;

   begin
      if In_Expansion then
         declare
            S : String := Expansion (Expansion_It);
         begin
            if S'Length /= 0 then
               return S;
            else
               In_Expansion := False;
            end if;

         end;
      end if;

      if Current_Argument > Total then

         --  If this is the first time this function is called

         if Current_Index = 1 then
            Current_Argument := 1;

         else
            return String'(1 .. 0 => ' ');
         end if;
      end if;

      Current_Index := 2;

      while Current_Argument <= Total
        and then Is_Switch (Current_Argument)
      loop
         Current_Argument := Current_Argument + 1;
      end loop;

      if Current_Argument > Total then
         return String'(1 .. 0 => ' ');
      end if;

      Current_Argument := Current_Argument + 1;

      --  Could it be a file name with wild cards to expand ?

      if Do_Expansion then
         declare
            Arg       : String renames CL.Argument (Current_Argument - 1);
            Index     : Positive := Arg'First;

         begin
            while Index <= Arg'Last loop

               if Arg (Index) = '*'
                 or else Arg (Index) = '?'
                 or else Arg (Index) = '['
               then
                  In_Expansion := True;
                  Start_Expansion (Expansion_It, Arg);
                  return Get_Argument (Do_Expansion);
               end if;

               Index := Index + 1;
            end loop;
         end;
      end if;

      return CL.Argument (Current_Argument - 1);
   end Get_Argument;

   ----------------------------
   -- Initialize_Option_Scan --
   ----------------------------

   procedure Initialize_Option_Scan
     (Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean := False)
   is
   begin
      Current_Argument := 1;
      Current_Index := 1;
      In_Expansion := False;
      Switch_Character := Switch_Char;
      Stop_At_First := Stop_At_First_Non_Switch;
   end Initialize_Option_Scan;

   ---------------
   -- Parameter --
   ---------------

   function Parameter return String is
   begin
      if The_Parameter.First > The_Parameter.Last then
         return String'(1 .. 0 => ' ');
      else
         return CL.Argument (The_Parameter.Arg_Num)
           (The_Parameter.First .. The_Parameter.Last);
      end if;
   end Parameter;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
     (Variable : out Parameter_Type;
      Arg_Num  : Positive;
      First    : Positive;
      Last     : Positive) is
   begin
      Variable.Arg_Num := Arg_Num;
      Variable.First   := First;
      Variable.Last    := Last;
   end Set_Parameter;

   ---------------------
   -- Start_Expansion --
   ---------------------

   procedure Start_Expansion
     (Iterator     : out Expansion_Iterator;
      Pattern      : String;
      Directory    : String := "";
      Basic_Regexp : Boolean := True)
   is
      function Directory_Separator return Character;
      pragma Import (C, Directory_Separator, "Get_Dirsep_Char");

   begin
      if Directory = "" then
         GNAT.Directory_Operations.Open (Iterator.Dir,
                                         "." & Directory_Separator);
      else
         GNAT.Directory_Operations.Open (Iterator.Dir, Directory);
      end if;

      Iterator.Regexp := GNAT.Regexp.Compile (Pattern, Basic_Regexp, True);
   end Start_Expansion;
end GNAT.Command_Line;
