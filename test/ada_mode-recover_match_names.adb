--  Abstract :
--
-- One each of all grammar statements that can use the Match_Names
-- check in error recovery. In each case, the name of the statement in
-- this file matches the nonterminal being tested; the code in the
-- contruct causes an error for which recovery calls Match_Names on
-- the statement; it should fail.
--
-- The error recovery behavior for all of this is similar to
-- subprogram_body_0: without the Match_Name check, error recovery
-- treats 'end subprogram_body' as matching 'loop'; the errors then
-- cascade until a lot of new 'end's are needed at EOF.
--
-- With the Match_Name check, error recovery inserts the required 'end
-- loop' in the right place, so the errors do not cascade, and 'end
-- subprogram_body' is indented properly.
--
--  Does not compile.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

--EMACS_SKIP_UNLESS:(eq 'process ada-parser)

pragma License (Modified_Gpl);

package body Ada_Mode.Recover_Match_Names is

   task body Accept_Statement_0
   is
      A : Integer;
   begin
      accept Start do
         loop
         loop;
      end Start;
   end Accept_Statement_0;

   procedure Block_Statement_0
   is
      A : Integer;
   begin
   Block_1:
      declare
      begin
         loop
         loop;
      end Block_1;
   end Block_Statement_0;

   procedure Block_Statement_1
   is
      A : Integer;
   begin
   Block_1:
      begin
         loop
         loop;
      end Block_1;
   end Block_Statement_1;

   protected body Entry_Body_0
   is
      entry E1 when A is
      begin
         loop
         loop;
      end E1;
   end Entry_Body_0;

   procedure Loop_Statement_0
   is
      A : Integer;
   begin
   Loop_1 :
      for I in B'Range loop
         if then
         if; -- meant 'end if;'
      end loop Loop_1;
   end Loop_Statement_0;

   procedure Loop_Statement_1
   is
      A : Integer;
   begin
   Loop_1 :
      loop
         exit when C;
         if then
         if; -- meant 'end if;'
      end loop Loop_1;
   end Loop_Statement_1;

   package body Package_Body_0 is
      A : Integer;
   begin -- for package_body_1
      loop
      loop; -- meant 'end loop;'
   end Package_Body_0;

   package body Package_Body_1 is
      procedure A
      is
         B : Integer;
      begin
         C;
         -- missing 'end A;'
   end Package_Body_1;

   package Package_Specification_0 is
      A : Integer;
   private
      type A is record
        record; -- meant 'end record'
   end Package_Specification_0;

   package Package_Specification_1 is
      A : Integer;
      type A is record
        record; -- meant 'end record'
   end Package_Specification_1;

   protected body Protected_Body_0 is
      entry E2 when True is begin end E2;
      entry E1 when True is begin
      -- missing 'end E1'
   end Protected_Body_0;

   --  protected protected_definition
   --
   --  There are no legal protected_definition declarations that can be
   --  missing an 'end'

   -- single_task_declaration_0
   --
   --  There are no legal task_definition declarations that can be
   --  missing an 'end'

   procedure Subprogram_Body_0
   is
      A : Integer;
   begin
      loop
      loop; --  meant 'end loop;'
   end Subprogram_Body_0;

   task body Task_Body_0
   is
      A : Integer;
   begin
      loop
      loop; --  meant 'end loop;'
   end Task_Body_0;

   --  task_type_declaration
   --
   --  There are no legal task_definition declarations that can be
   --  missing an 'end'

end Ada_Mode.Recover_Match_Names;
