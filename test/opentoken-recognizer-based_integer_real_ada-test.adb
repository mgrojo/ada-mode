--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2014 Stephe Leake
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

pragma License (GPL);

with Ada.Exceptions;
with AUnit.Assertions;
with OpenToken.Recognizer.AUnit;
package body OpenToken.Recognizer.Based_Integer_Real_Ada.Test is

   Recognizer : Instance := Get;

   procedure Test
     (Label         : in String;
      Item          : in String;
      Expected      : in String  := "";
      Expected_Fail : in Integer := 0)
   is
      --  If Expected = "", Item contains one more character after valid number.
      --
      --  Otherwise, Item contains other stuff; compare result to expected.

      use OpenToken.Recognizer.AUnit;

      Verdict : Analysis_Verdict;
      Last    : Integer := Item'First;
   begin
      Clear (Recognizer);

      loop
         begin
            Analyze (Recognizer, Item (Last), Verdict);
         exception
         when E : others =>
            Standard.AUnit.Assertions.Assert (False, Label & " " & Ada.Exceptions.Exception_Message (E));
         end;

         Standard.AUnit.Assertions.Assert
           ((if Last < (if Expected'Length = 0 then Item'Last else Expected_Fail)
             then Verdict in Matches .. So_Far_So_Good
             else Verdict = Failed),
            Label & "." & Integer'Image (Last) & " expecting " &
              (if Last < (if Expected'Length = 0 then Item'Last else Expected_Fail)
               then "Matches .. So_Far_So_Good"
               else "Failed") &
              " got " & Analysis_Verdict'Image (Verdict));

         exit when Last = Item'Last or Verdict = Failed;
         Last := Last + 1;
      end loop;
   end Test;

   ----------
   --  Test procedures

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  The only way to fail is to start with a non-decimal integer
      declare
         use OpenToken.Recognizer.AUnit;
         Verdict : Analysis_Verdict;
      begin
         Analyze (Recognizer, 'A', Verdict);
         Check ("fail", Verdict, Failed);
      end;

      Test ("1", "1234;");
      Test ("2", "1234.0;");
      Test ("3", "1234.5678E+90;");
      Test ("4", "1234.5678e-90;");
      Test ("5", "1234.5678E90;");

      Test ("6", "123_456;");
      Test ("7", "123_456.901_234;");
      Test ("8", "1_234.567_8E+90;");

      Test ("9", "16#ABCD#;");
      Test ("10", "16#EF34.0#;");
      Test ("11", "16#1234.ABCD#E+90;");
      Test ("12", "16#1234.5678#e-90;");
      Test ("13", "16#1234.5678#E90;");

      Test ("14", "16#ABCD_EF01#;");
      Test ("15", "16#EF_34.0_1#;");
      Test ("16", "16#12_34.AB_CD#E+90;");

      --  problematic cases
      Test ("17", "1234.;", "1234", 6); -- trailing .
      Test ("18", "1234E+90;", "1234", 5); -- no .

      Test ("19", "123_;", "123", 5); -- trailing _

      Test ("20", "16#ABCD;", "16", 8); -- missing trailing #
      Test ("21", "16_#ABCD;", "16", 4); -- trailing _ in base
      Test ("22", "16#1234.ABCD#E;", "16#1234.ABCD#", 15); -- no exponent
   end Nominal;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../Test/opentoken-recognizer-based_integer_real_ada-test.adb");
   end Name;

end OpenToken.Recognizer.Based_Integer_Real_Ada.Test;
