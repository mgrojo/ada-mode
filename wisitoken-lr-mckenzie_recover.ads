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
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.LR.Parser;
with WisiToken.Lexer;
package WisiToken.LR.McKenzie_Recover is

   Bad_Config : exception;
   --  Raised when a config is determined to violate some programming
   --  convention; abandon it.

   type Recover_Status is
     (Fail_Check_Delta, Fail_Enqueue_Limit, Fail_Cost, Fail_No_Configs_Left,
      Success);
   subtype Fail_Recover_Status is Recover_Status range Fail_Check_Delta .. Fail_No_Configs_Left;

   function Recover (Shared_Parser : in out WisiToken.LR.Parser.Parser) return Recover_Status;
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
   --  Visible for language-specific child packages. Alphabetical.

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

   procedure Put
     (Message      : in     String;
      Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Terminals    : in     Base_Token_Arrays.Vector;
      Config       : in     Configuration;
      Task_ID      : in     Boolean := True);
   --  Put Message and an image of Config to Trace.

   procedure Put_Line
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Message      : in     String;
      Task_ID      : in     Boolean := True);
   --  Put message to Trace, with parser and task info.

   function Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
     return Ada.Containers.Count_Type
   with Pre => Tree.Is_Nonterm (Stack (1).Tree_Index);
   --  Undo the reduction that produced the top stack item, return the
   --  token count for that reduction.

end WisiToken.LR.McKenzie_Recover;
