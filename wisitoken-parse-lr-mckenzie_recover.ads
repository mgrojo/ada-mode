--  Abstract :
--
--  Implement [McKenzie] error recovery, extended to parallel parsers.
--
--  References:
--
--  [McKenzie] McKenzie, Bruce J., Yeatman, Corey, and De Vere,
--  Lorraine. Error repair in shift reduce parsers. ACM Trans. Prog.
--  Lang. Syst., 17(4):672-689, July 1995.  Described in [Grune 2008] ref 321.
--
--  [Grune 2008] Parsing Techniques, A Practical Guide, Second
--  Edition. Dick Grune, Ceriel J.H. Jacobs.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Task_Attributes;
with WisiToken.Parse.LR.Parser;
with WisiToken.Lexer;
package WisiToken.Parse.LR.McKenzie_Recover is
   use all type WisiToken.Syntax_Trees.Node_Access;
   use all type WisiToken.Syntax_Trees.Stream_Index;
   use all type Ada.Containers.Count_Type;

   Bad_Config : exception;
   --  Raised when a config is determined to violate some programming
   --  convention; abandon it.

   type Recover_Status is (Fail_Check_Delta, Fail_Enqueue_Limit, Fail_No_Configs_Left, Fail_Programmer_Error, Success);

   function Recover (Shared_Parser : in out WisiToken.Parse.LR.Parser.Parser) return Recover_Status;
   --  Attempt to modify Parser.Parsers state and Parser.Lookahead to
   --  allow recovering from an error state.

   Force_Full_Explore        : Boolean := False;
   --  Sometimes recover throws an exception in a race condition case
   --  that is hard to reproduce. Setting this True ignores all Success,
   --  so all configs are checked.

   Force_High_Cost_Solutions : Boolean := False;
   --  Similarly, setting this true keeps all solutions that are found,
   --  and forces at least three.

private

   ----------
   --  Visible for child packages. Alphabetical.
   --
   --  The various Check subprograms raise Bad_Config for check fail, and
   --  there are no preconditions, so the checks are always performed.

   type Config_Stream_Parents (Stream : access constant Bounded_Streams.List) is
   record
      --  Like Syntax_Trees.Stream_Node_Parents, but using a Configuration
      --  input stream.
      Element : Bounded_Streams.Cursor;
      Node    : Syntax_Trees.Node_Access;
      Parents : Syntax_Trees.Node_Stacks.Stack;
   end record;

   type Peek_Shared_State (Stream : access constant Bounded_Streams.List) is record
      Input_Terminal  : Config_Stream_Parents (Stream);
      Shared_Terminal : Syntax_Trees.Stream_Node_Parents;
   end record;

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID)
   with Inline => True;
   --  Check that ID = Expected_ID; raise Bad_Config if not.

   procedure Delete_Check
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      ID     : in     Token_ID);
   --  Check that the next input token in Config has ID. Append a Delete op
   --  to Config.Ops, and append it to Config.Insert_Delete.
   --
   --  This or the next routine must be used instead of Config.Ops.Append
   --  (Delete...) unless the code also takes care of changing
   --  Config.Current_Shared_Token or Config.Input_Stream. Note that this
   --  routine does _not_ increment Config.Current_Shared_Token or
   --  Config.Input_Stream, so it can only be used to delete one token.

   procedure Delete_Check
     (Tree   :         in     Syntax_Trees.Tree;
      Config : aliased in out Configuration;
      IDs    :         in     Token_ID_Array);
   --  Call Delete_Check for each ID in IDs, incrementing to the next
   --  token for each.

   procedure Delete_Check
     (Tree       : in     Syntax_Trees.Tree;
      Config     : in out Configuration;
      Peek_State : in out Peek_Shared_State;
      ID         : in     Token_ID);
   --  If ID is not Invalid_Token_ID, check that
   --  Parse.Peek_Shared_Terminal (Peek_State) has ID. Append a Delete op
   --  to Config.Ops, and append it to Config.Insert_Delete. Then
   --  increment Peek_State to the next shared terminal.
   --
   --  Peek_State is initialized by Parse.Peek_Shared_Start

   procedure Do_Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   with Pre => not Config_Op_Arrays.Is_Full (Config.Ops);
   --  Push back Config.Stack top to Config.Input_Stream. Appends to
   --  Config.Ops. Nonterms are not broken down. We assume caller has
   --  checked Push_Back_Valid.

   procedure Ensure_Sequential_Node_Index
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration);
   --  Ensure that Node_Index in Tree.Shared_Stream terminals is
   --  sequential for Config.Current_Shared_Token ..
   --  Config.Resume_Token_Goal.

   function Find_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration;
      ID     : in Token_ID)
     return Boolean;
   --  Search Config.Stack for a token with ID, starting at
   --  stack top. Return True if found, False if not.

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with ID, starting at
   --  Matching_Index. If found, Matching_Index points to it.
   --  If not found, Matching_Index = Config.Stack.Depth.

   procedure Find_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with ID in IDs, starting at
   --  Matching_Index. If found, Matching_Index points to it.
   --  If not found, Matching_Index = Config.Stack.Depth.

   procedure Find_Descendant_ID
     (Tree           : in     Syntax_Trees.Tree;
      Config         : in     Configuration;
      ID             : in     Token_ID;
      ID_Set         : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with id in ID_Set, with a
   --  descendant with id = ID, starting at Matching_Index. If found,
   --  Matching_Index points to it. If not found, Matching_Index =
   --  Config.Stack.Depth.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean);
   --  Search Config.Stack for a token matching Name, starting at
   --  Matching_Name_Index. If found, Matching_Name_Index points to it.
   --  If not found, Matching_Name_Index = Config.Stack.Depth.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean);
   --  Search Config.Stack for a token matching Name, starting at
   --  Matching_Name_Index. If found, Matching_Name_Index points to it.
   --  If not found, Matching_Name_Index = Config.Stack.Depth.
   --
   --  Also count tokens with ID = Other_ID.

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      ID     : in     Token_ID);
   --  Append an Insert before Config.Current_Shared_Token or
   --  Config.Input_Stream.First op to Config.Ops, and append it to
   --  Config.Insert_Deleted.

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      IDs    : in     Token_ID_Array);
   --  Call Insert for each item in IDs.

   procedure Insert
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Before : in     Syntax_Trees.Valid_Node_Access;
      ID     : in     Token_ID);
   --  Same as Insert, but before Before.

   function Peek_Shared_Start
     (Tree   :         in Syntax_Trees.Tree;
      Config : aliased in Configuration)
     return Peek_Shared_State;

   function Peek_Shared_Terminal (State : in Peek_Shared_State) return Syntax_Trees.Node_Access;

   procedure Peek_Next_Shared_Terminal
     (Tree  : in     Syntax_Trees.Tree;
      State : in out Peek_Shared_State);

   function Push_Back_Valid
     (Tree                  : in Syntax_Trees.Tree;
      Config                : in Configuration;
      Prev_Recover_End      : in Syntax_Trees.Node_Index := 0;
      Push_Back_Undo_Reduce : in Boolean                 := False)
     return Boolean;
   --  True if Push_Back is a valid op for Config.
   --
   --  Language_Fixes may use the default Prev_Recover_End = 0, however
   --  other checks may prevent modifying a previous fix.

   procedure Push_Back
     (Tree                  : in     Syntax_Trees.Tree;
      Config                : in out Configuration;
      Push_Back_Undo_Reduce : in     Boolean := False);
   --  If not Push_Back_Valid, raise Bad_Config. Otherwise do Push_Back.
   --
   --  Normally Push_Back_Valid forbids push_back of an entire
   --  Undo_Reduce; Language_Fixes may override that by setting
   --  Push_Back_Undo_Reduce True.

   procedure Push_Back_Check
     (Tree              : in     Syntax_Trees.Tree;
      Config            : in out Configuration;
      Expected_ID       : in     Token_ID);
   --  Check that Config.Stack top has Expected_ID; raise Bad_Config if
   --  not. Then call Push_Back.

   procedure Push_Back_Check
     (Tree     : in     Syntax_Trees.Tree;
      Config   : in out Configuration;
      Expected : in     Token_ID_Array);
   --  Call Push_Back_Check for each item in Expected.
   --
   --  Raises Bad_Config if any of the push_backs is invalid.

   procedure Put
     (Message      : in     String;
      Trace        : in out WisiToken.Trace'Class;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Config       : in     Configuration;
      Task_ID      : in     Boolean := True;
      Strategy     : in     Boolean := False);
   --  Put Message and an image of Config to Trace.

   procedure Put_Line
     (Trace        : in out WisiToken.Trace'Class;
      Tree         : in     Syntax_Trees.Tree;
      Parser_Label : in     Syntax_Trees.Stream_ID;
      Message      : in     String;
      Task_ID      : in     Boolean := True);
   --  Put message to Trace, with parser and task info.

   function Undo_Reduce_Valid
     (Tree     : in     Syntax_Trees.Tree;
      Config   : in out Configuration)
     return Boolean;
   --  True if Undo_Reduce is valid for Config.

   procedure Unchecked_Undo_Reduce
     (Config : in out Configuration;
      Tree  : in     Syntax_Trees.Tree;
      Table : in     Parse_Table);
   --  Undo the reduction that produced the top stack item, append op.

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Table    : in     Parse_Table;
      Expected : in     Token_ID)
   with Inline => True;
   --  If not Undo_Reduce_Valid, raise Bad_Config. Else call Check,
   --  Unchecked_Undo_Reduce. Caller should check for space in
   --  Config.Ops.

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Table    : in     Parse_Table;
      Expected : in     Token_ID_Array);
   --  Call Undo_Reduce_Check for each item in Expected.

   package Task_Attributes is new Ada.Task_Attributes (Integer, 0);

end WisiToken.Parse.LR.McKenzie_Recover;
