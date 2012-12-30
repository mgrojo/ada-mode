--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2009, 2012 Stephe Leake
--  Copyright (C) 1999 Christoph Karl Walter Grein
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

with AUnit.Assertions;
package body OpenToken.Recognizer.Bracketed_Comment.Test is

   Recognizer : Instance;

   procedure Test
     (Label            : in String;
      Input            : in String;
      Expected_Verdict : in Analysis_Verdict)
   is
      use AUnit.Assertions;

      Verdict : Analysis_Verdict;
   begin
      Clear (Recognizer);

      for I in Input'Range loop
         Analyze (Recognizer, Input (I), Verdict);
      end loop;

      Assert (Verdict = Expected_Verdict, Label & " got " & Analysis_Verdict'Image (Verdict));
   end Test;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use OpenToken.Recognizer;

   begin
      --  Verify that a valid bracketed comment is recognized correctly.

      Recognizer := Get ("/*", "*.*..", Reportable => False);

      --  Single line
      Case_1 :
      declare
         Text_1 : constant String := "/* A comment that ends here *.*..";
         Text_2 : constant String := "/* Another comment that ends a bit later *.*.*.*..";
         Text_3 : constant String := "/* It ends here *.*..";
      begin
         Test ("1-1", Text_1, Matches);
         Test ("1-2", Text_2, Matches);
         Test ("1-3", Text_3, Matches);
      end Case_1;

      --  Multi-line
      Case_2 :
      declare
         Text_1 : constant String := "/* A comment that starts here";
         Text_2 : constant String := "   and keeps going";
         Text_3 : constant String := "   and finally ends here *.*..";
      begin
         Test
           ("2-1",
            Text_1 & OpenToken.EOL_Character &
              Text_2 & OpenToken.EOL_Character &
              Text_3,
            Matches);
      end Case_2;
   end Nominal;

   procedure Wisi (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use OpenToken.Recognizer;

   begin
      --  Verify that a Wisi prologue (implimented as a bracketed comment) is recognized correctly.

      Recognizer := Get ("%{", "%}", Reportable => True);

      declare
         Text_1 : constant String := "%{";
         Text_2 : constant String := "(require 'wisi)";
         Text_3 : constant String := "%}";
      begin
         Test
           ("1",
            Text_1 & OpenToken.EOL_Character &
              Text_2 & OpenToken.EOL_Character &
              Text_3,
            Matches);

         --  FIXME: test Value
      end;
   end Wisi;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Wisi'Access, "Wisi");

      --  FIXME: test for bracketed comment terminated by end of file (user forgot closing bracket)
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../Test/opentoken-recognizer-bracketed_comment-test.adb");
   end Name;

end OpenToken.Recognizer.Bracketed_Comment.Test;
