--  Abstract :
--
--  Base utilities for McKenzie_Recover
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

pragma License (Modified_GPL);

with Ada.Exceptions;
with WisiToken.LR.Parser_Lists;
private package WisiToken.LR.McKenzie_Recover.Base is

   ----------
   --  Protected object specs.
   --
   --  Tasking design requirements:
   --
   --  1) For each parse_state, find all solutions of the same lowest
   --  cost.
   --
   --  2) use as many CPUs as available as fully as possible.
   --
   --  3) avoid
   --     a) busy waits
   --     b) race conditions
   --     c) deadlocks.
   --
   --  For 2), we use worker_tasks to perform the check computations on
   --  each configuration. We allocate N - 1 worker_tasks, where N is the
   --  number of available CPUs, saving one CPU for Supervisor and the
   --  foreground IDE.
   --
   --  For 1), worker_tasks always get the lowest cost configuration
   --  available. However, some active worker_task may have a lower cost
   --  configuration that it has not yet delivered to Supervisor.
   --  Therefore we always wait until all current active worker_tasks
   --  deliver their results before deciding we are done.
   --
   --  For 3a) we have one Supervisor protected object that controls
   --  access to all Parse_States and configurations, and a
   --  Shared_Lookahead protected object that controls access to the
   --  Shared_Parser lookahead and lexer.
   --
   --  It is tempting to try to reduce contention for Supervisor by
   --  having one protected object per parser, but that requires the
   --  worker tasks to busy loop checking all the parsers.
   --
   --  There is still a race condition on Success; the solutions can be
   --  delivered in different orders on different runs. This matters
   --  because each solution results in a successful parse, possibly with
   --  different actions (different indentation computed, for example).
   --  Which solution finally succeeds depends on which are terminated
   --  due to identical parser stacks, which in turn depends on the order
   --  they were delivered. See ada-mode/tests/ada_mode-interactive_2.adb
   --  for an example.

   type Config_Status is (Valid, All_Done);
   type Parser_Status is (Active, Ready, Success, Fail);
   type Parser_Status_Array is array (SAL.Peek_Type range <>) of Parser_Status;
   type Parser_Natural_Array is array (SAL.Peek_Type range <>) of Natural;

   type Parser_State_Array is array (SAL.Peek_Type range <>) of Parser_Lists.State_Access;
   --  Index is same as Parser_Status.

   protected type Supervisor
     (Trace             : not null access WisiToken.Trace'Class;
      Parsers           : not null access Parser_Lists.List;
      Terminals         : not null access Base_Token_Arrays.Vector;
      Cost_Limit        : Natural;
      Check_Delta_Limit : Natural;
      Parser_Count      : SAL.Peek_Type)
   is
      --  There is only one object of this type, declared in Recover.

      procedure Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status);
      --  Get a new configuration to check. Available when there is a
      --  configuration to get, or when all configs have been checked.
      --
      --  Increments active worker count.
      --
      --  Status values mean:
      --
      --  Valid - Parser_Index, Config are valid, should be checked.
      --
      --  All_Done - Parser_Index, Config are not valid.

      procedure Success
        (Parser_Index : in     SAL.Peek_Type;
         Config       : in     Configuration;
         Configs      : in out Config_Heaps.Heap_Type);
      --  Report that Configuration succeeds for Parser_Label, and enqueue
      --  Configs.
      --
      --  Decrements active worker count.

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type);
      --  Add Configs to the McKenzie_Data Config_Heap for Parser_Label
      --
      --  Decrements active worker count.

      function Recover_Result return Recover_Status;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence);
      --  Report a fatal error; abort all processing, make Done
      --  available.

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String);
      --  Available when all parsers have failed or succeeded, or an error
      --  occured.
      --
      --  If Error_ID is not Null_Id, an error occured.

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type;
      function Label (Parser_Index : in SAL.Peek_Type) return Natural;

   private
      Active_Workers : Parser_Natural_Array (1 .. Parser_Count);
      --  Worker_Tasks for each Parser that have done Get but not Put or
      --  Success.

      All_Parsers_Done        : Boolean;
      Success_Counter         : Natural;
      Min_Success_Check_Count : Natural;
      Fatal_Called            : Boolean;
      Result                  : Recover_Status;
      Error_ID                : Ada.Exceptions.Exception_Id;
      Error_Message           : Ada.Strings.Unbounded.Unbounded_String;
      Parser_Status           : Parser_Status_Array (1 .. Parser_Count);
      Parser_States           : Parser_State_Array (1 .. Parser_Count);
      Parser_Labels           : Parser_Natural_Array (1 .. Parser_Count); -- For Trace
   end Supervisor;

   protected type Shared_Lookahead (Shared_Parser : not null access LR.Parser.Parser)
   is
      --  There is only one object of this type, declared in Recover. It
      --  controls access to Get_Next_Token and direct access to shared
      --  Terminals.

      function Get_Token (Index : in Token_Index) return Token_Index;
      --  Return Index, after assuring there is a token in shared Terminals
      --  there, reading from the lexer if necessary.

      function Token (Index : in Token_Index) return Base_Token;
      --  Return Shared_Parser.Terminals (Index).

   end Shared_Lookahead;

   procedure Put
     (Message      : in              String;
      Super        : not null access Base.Supervisor;
      Shared       : not null access Base.Shared_Lookahead;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration;
      Task_ID      : in              Boolean := True);

end WisiToken.LR.McKenzie_Recover.Base;
