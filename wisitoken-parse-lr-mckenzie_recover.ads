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
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

   procedure Check (ID : Token_ID; Expected_ID : in Token_ID)
   with Inline => True;
   --  Check that ID = Expected_ID; raise Bad_Config if not.

   procedure Delete_Check
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      ID     : in     Token_ID);
   --  Check that Config.Current_Shared_Token has ID. Append a Delete op
   --  to Config.Ops, and append it to Config.Insert_Delete.
   --
   --  This or the next routine must be used instead of Config.Ops.Append
   --  (Delete...) unless the code also takes care of changing
   --  Config.Current_Shared_Token. Note that this routine does _not_
   --  increment Config.Current_Shared_Token, so it can only be used to
   --  delete one token.

   procedure Delete_Check
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Ref    : in out Syntax_Trees.Terminal_Ref;
      ID     : in     Token_ID);
   --  Check that Ref has ID. Append a Delete op to Config.Ops, and
   --  append it to Config.Insert_Delete. Increments Ref, for convenience
   --  when deleting several tokens.

   procedure Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration;
      Ref    : in out Syntax_Trees.Terminal_Ref);
   --  Same as Delete_Check, without the check.

   procedure Do_Push_Back
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   with Pre => not Config_Op_Arrays.Is_Full (Config.Ops);
   --  Push back Config.Stack top to Config.Input_Stream. Appends to
   --  Config.Ops. Nonterms are not broken down. We assume caller has
   --  checked Push_Back_Valid.

   function Find_ID
     (Config         : in     Configuration;
      ID             : in     Token_ID)
     return Boolean;
   --  Search Config.Stack for a token with ID, starting at
   --  stack top. Return True if found, False if not.

   procedure Find_ID
     (Config         : in     Configuration;
      ID             : in     Token_ID;
      Matching_Index : in out SAL.Peek_Type);
   --  Search Config.Stack for a token with ID, starting at
   --  Matching_Index. If found, Matching_Index points to it.
   --  If not found, Matching_Index = Config.Stack.Depth.

   procedure Find_ID
     (Config         : in     Configuration;
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
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean);
   --  Search Config.Stack for a token matching Name, starting at
   --  Matching_Name_Index. If found, Matching_Name_Index points to it.
   --  If not found, Matching_Name_Index = Config.Stack.Depth.

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : access constant WisiToken.Lexer.Instance'Class;
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
     (Config : in out Configuration;
      Before : in     Syntax_Trees.Valid_Node_Access;
      ID     : in     Token_ID);
   --  Same as Insert, but before Before.

   function Push_Back_Valid
     (Tree             : in Syntax_Trees.Tree;
      Config           : in Configuration;
      Prev_Recover_End : in Syntax_Trees.Node_Index)
     return Boolean;
   --  True if Push_Back is a valid op for Config.

   procedure Push_Back
     (Tree             : in     Syntax_Trees.Tree;
      Config           : in out Configuration;
      Prev_Recover_End : in     Syntax_Trees.Node_Index);
   --  If not Push_Back_Valid, raise Bad_Config. Otherwise pop the top
   --  Config.Stack item, set Config.Current_Shared_Token to the first
   --  terminal in that item. If the item is empty,
   --  Config.Current_Shared_Token is unchanged.

   procedure Push_Back_Check
     (Tree             : in     Syntax_Trees.Tree;
      Config           : in out Configuration;
      Prev_Recover_End : in     Syntax_Trees.Node_Index;
      Expected_ID      : in     Token_ID);
   --  In effect, call Check and Push_Back.

   procedure Push_Back_Check
     (Tree             : in     Syntax_Trees.Tree;
      Config           : in out Configuration;
      Prev_Recover_End : in     Syntax_Trees.Node_Index;
      Expected         : in     Token_ID_Array);
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

   function Unchecked_Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
     return Ada.Containers.Count_Type;
   --  Undo the reduction that produced the top stack item, return the
   --  token count for that reduction.

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID)
   with Inline => True;
   --  If not Undo_Reduce_Valid, raise Bad_Config. Else call Check, Undo_Reduce.

   procedure Undo_Reduce_Check
     (Config   : in out Configuration;
      Tree     : in     Syntax_Trees.Tree;
      Expected : in     Token_ID_Array);
   --  Call Undo_Reduce_Check for each item in Expected.

   package Task_Attributes is new Ada.Task_Attributes (Integer, 0);

end WisiToken.Parse.LR.McKenzie_Recover;
