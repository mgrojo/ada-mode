--  Abstract :
--
--  Base utilities for McKenzie_Recover
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
package WisiToken.Parse.LR.McKenzie_Recover.Base is

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
   --  access to all Parse_States and configurations, and a Shared object
   --  that provides appropriate access to the Shared_Parser components.
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
   --
   --  There is also a race condition on how many failed or higher cost
   --  configurations are checked, before the final solutions are found.

   type Config_Status is (Valid, All_Done);
   type Recover_State is (Active, Ready, Success, Fail);

   type Parser_Status is record
      Recover_State : Base.Recover_State;
      Parser_State  : Parser_Lists.State_Access;
      Fail_Mode     : Recover_Status;

      Active_Workers : Natural;
      --  Count of Worker_Tasks that have done Get but not Put or Success.
   end record;

   type Parser_Status_Array is array (SAL.Peek_Type range <>) of Parser_Status;

   protected type Supervisor
     (Trace             : not null access WisiToken.Trace'Class;
      Tree              : not null access constant Syntax_Trees.Tree;
      Check_Delta_Limit : Natural;
      Enqueue_Limit     : Natural;
      Parser_Count      : SAL.Peek_Type)
   is
      --  There is only one object of this type, declared in Recover.

      procedure Initialize (Parsers : not null access Parser_Lists.List);

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

      procedure Config_Full (Prefix : in String; Parser_Index : in SAL.Peek_Type);
      --  Report that a config.ops was full when trying to add another op.
      --  This is counted towards the enqueue limit.

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
      function Stream (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_ID;

      function Min_Sequential_Index return Syntax_Trees.Sequential_Index;
      function Min_Sequential_Index_All_SOI return Boolean;
      function Max_Sequential_Index return Syntax_Trees.Sequential_Index;
      function Max_Sequential_Index_All_EOI return Boolean;
      function Min_Sequential_Index (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_Node_Parents;
      function Max_Sequential_Index (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_Node_Parents;

      procedure Extend_Min_Sequential_Index (Target : in Syntax_Trees.Sequential_Index);
      procedure Extend_Max_Sequential_Index (Target : in Syntax_Trees.Sequential_Index);
      --  In all parse streams. Clients should use
      --  Base.Extend_Sequential_Index, below.
   private
      Parsers : access Parser_Lists.List; --  FIXME: why do we have both Parsers and Parser_Status.Parser_State?

      All_Parsers_Done        : Boolean;
      Success_Counter         : Natural;
      Min_Success_Check_Count : Natural;
      Total_Enqueue_Count     : Natural;
      Fatal_Called            : Boolean;
      Result                  : Recover_Status;
      Error_ID                : Ada.Exceptions.Exception_Id;
      Error_Message           : Ada.Strings.Unbounded.Unbounded_String;
      Parser_Status           : Parser_Status_Array (1 .. Parser_Count);

      Min_Sequential_Indices : Syntax_Trees.Stream_Node_Parents_Array (1 .. Parser_Count);
      Max_Sequential_Indices : Syntax_Trees.Stream_Node_Parents_Array (1 .. Parser_Count);
   end Supervisor;

   procedure Extend_Sequential_Index
     (Super    : not null access Base.Supervisor;
      Thru     : in              Syntax_Trees.Valid_Node_Access;
      Positive : in              Boolean);
   --  If Thru.Node has valid Sequential_Index, return.
   --
   --  Else extend Sequential_Index range thru node Thru, in Positive
   --  direction.

   procedure Extend_Sequential_Index
     (Super : not null access Base.Supervisor;
      Thru  : in              Syntax_Trees.Sequential_Index);
   --  Ensure Sequential_Index range includes Thru, or SOI/EOI.

   type Shared
     --  Don't duplicate values that are in Supervisor discriminants.
     (Table                          : not null access constant Parse_Table;
      Language_Fixes                 : WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access)
     is null record;
   --  There is only one object of this type, declared in Recover. Along
   --  with Supervisor, it provides appropriate access to Shared_Parser
   --  components.
   --
   --  Since all the accessible objects are read-only, there are no
   --  protected operations, and this is not a protected type.

   procedure Put
     (Message      : in              String;
      Super        : not null access Base.Supervisor;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration;
      Task_ID      : in              Boolean := True);

end WisiToken.Parse.LR.McKenzie_Recover.Base;
