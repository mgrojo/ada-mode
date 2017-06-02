--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2004, 2005, 2008, 2009, 2011, 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with AUnit.Assertions;
with AUnit.Checks;
with SAL.Gen_Definite_Queues.Gen_Test;
package body Test_Definite_Queues
is
   package Integer_Queues is new SAL.Gen_Definite_Queues (Integer);
   package Integer_Queues_Test is new Integer_Queues.Gen_Test;
   use Integer_Queues;

   Queue : Queue_Type (5);
   Item  : Integer;

   procedure Check_Head_Tail
     (Label    : in String;
      Exp_Head : in Integer;
      Exp_Tail : in Integer)
   is
      use AUnit.Checks;
      use Integer_Queues_Test;
   begin
      Check (Label & ".Head", Get_Head (Queue), Exp_Head);
      Check (Label & ".Tail", Get_Tail (Queue), Exp_Tail);
   end Check_Head_Tail;

   procedure Check
     (Label    : in String;
      Exp_Head : in Integer;
      Exp_Tail : in Integer;
      Exp_Item : in Integer)
   is
      use AUnit.Checks;
      use Integer_Queues_Test;
   begin
      Check (Label & ".Item", Item, Exp_Item);
      Check_Head_Tail (Label, Exp_Head, Exp_Tail);
   end Check;

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Check ("Is_Empty 0", Is_Empty (Queue), True);
      Add (Queue, 1);
      Check ("Is_Empty 1", Is_Empty (Queue), False);
      Check_Head_Tail ("1", Exp_Head => 1, Exp_Tail => 1);
      Check ("1 count", Count (Queue), 1);
      Check ("1 peek", Peek (Queue), 1);
      Item := Remove (Queue);
      Check ("Is_Empty 2", Is_Empty (Queue), True);
      Check ("2", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 1);
      Check ("2 count", Count (Queue), 0);

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Check_Head_Tail ("1234", Exp_Head => 1, Exp_Tail => 4);
      Check ("1234 count", Count (Queue), 4);
      Check ("Is_Empty 1234", Is_Empty (Queue), False);

      Item := Remove (Queue);
      Check ("234", Exp_Head => 2, Exp_Tail => 4, Exp_Item => 1);
      Check ("234 count", Count (Queue), 3);
      Check ("234 peek", Peek (Queue), 2);

      Add (Queue, 5);
      Check_Head_Tail ("2345", Exp_Head => 2, Exp_Tail => 5);

      Add (Queue, 6);
      Check ("Is_Full 23456", Is_Full (Queue), True);
      Check_Head_Tail ("23456", Exp_Head => 2, Exp_Tail => 1);
      Item := Remove (Queue);
      Check ("3456", Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
      Check ("Is_Full 3456", Is_Full (Queue), False);

      Add_To_Head (Queue, 2);
      Check_Head_Tail ("23456", Exp_Head => 2, Exp_Tail => 1);
      Item := Remove (Queue);
      Check ("3456", Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
   end Nominal;

   procedure Errors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;
   begin
      Clear (Queue);
      begin
         Item := Remove (Queue);
         Assert (False, "didn't get exception for Remove on empty");
      exception
      when SAL.Container_Empty =>
         Assert (True, "");
      end;

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Add (Queue, 5);
      begin
         Add (Queue, 6);
         Assert (False, "didn't get exception for add on full");
      exception
      when SAL.Container_Full =>
         Assert (True, "");
      end;
   end Errors;

   procedure Overwrite (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Set_Overflow_Handling (Queue, SAL.Overwrite);
      Clear (Queue);

      Add (Queue, 1);
      Add (Queue, 2);
      Add (Queue, 3);
      Add (Queue, 4);
      Add (Queue, 5);
      begin
         Add (Queue, 6);
         Item := Remove (Queue);
         Check ("2", Exp_Head => 3, Exp_Tail => 1, Exp_Item => 2);
         Item := Remove (Queue);
         Check ("3", Exp_Head => 4, Exp_Tail => 1, Exp_Item => 3);
         Item := Remove (Queue);
         Check ("4", Exp_Head => 5, Exp_Tail => 1, Exp_Item => 4);
         Item := Remove (Queue);
         Check ("5", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 5);
         Item := Remove (Queue);
         Check ("6", Exp_Head => 1, Exp_Tail => 1, Exp_Item => 6);
      exception
      when SAL.Container_Full =>
         AUnit.Assertions.Assert (False, "got exception for add on full");
      end;
   end Overwrite;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_definite_queues.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Errors'Access, "Errors");
      Register_Routine (T, Overwrite'Access, "Overwrite");
   end Register_Tests;

end Test_Definite_Queues;
