--  Abstract :
--
--  Config parsing subprograms.
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

with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
private package WisiToken.Parse.LR.McKenzie_Recover.Parse is
   use all type WisiToken.Syntax_Trees.Node_Label;

   function Reduce_Stack
     (Super                    : not null access Base.Supervisor;
      Stack                    : in out          Recover_Stacks.Stack;
      Action                   : in              Reduce_Action_Rec;
      Nonterm                  :    out          Syntax_Trees.Recover_Token;
      Default_Contains_Virtual : in              Boolean)
     return In_Parse_Actions.Status;
   --  Reduce Stack according to Action, setting Nonterm.

   function Delete_Current_Applies
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Boolean;
   --  True if Config has a Delete op that applies to the current token.

   function Peek_Current_Token_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Token_ID
   with Pre => not Delete_Current_Applies (Tree, Config);
   --  Return ID of Config current token. In incremental parse, this may
   --  be a nonterminal.
   --
   --  In Parse because it has similar code to Current_Token.

   procedure Current_Token_ID_Peek_3
     (Tree   : in     Syntax_Trees.Tree;
      Config : in     Configuration;
      Tokens :    out Token_ID_Array_1_3)
   with Post => (for all Tok of Tokens => Tok = Invalid_Token_ID or else Is_Terminal (Tok, Tree.Lexer.Descriptor.all));
   --  Return the current terminal token from Config in Tokens (1).
   --  Return the two following terminal tokens in Tokens (2 .. 3). In
   --  incremental parse, they may be virtual.
   --
   --  In Parse because it has similar code to Current_Token, Next_Token.

   function Peek_Current_Element_Node
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  Stream element from Config.Shared_Token or Config.Input_Stream.

   function Peek_Current_First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  First_Terminal from Config.Shared_Token or Config.Input_Stream.

   function Peek_Current_First_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access;
   --  First_Shared_Terminal from Config.Shared_Token or Config.Input_Stream.

   function First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Bounded_Streams.List)
     return Syntax_Trees.Node_Access;

   function First_Shared_Terminal
     (Tree           : in     Syntax_Trees.Tree;
      Stream         : in     Bounded_Streams.List;
      Stream_Parents : in out Syntax_Trees.Node_Stacks.Stack)
     return Syntax_Trees.Node_Access;

   procedure Next_Shared_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Stream       : in     Bounded_Streams.List;
      Element_Node : in out Bounded_Streams.Cursor;
      Node         : in out Syntax_Trees.Node_Access;
      Parents      : in out Syntax_Trees.Node_Stacks.Stack);

   procedure Prev_Shared_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Stream       : in     Bounded_Streams.List;
      Element_Node : in out Bounded_Streams.Cursor;
      Node         : in out Syntax_Trees.Node_Access;
      Parents      : in out Syntax_Trees.Node_Stacks.Stack);

   procedure Breakdown
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in out Bounded_Streams.List)
   with Pre => Stream.Length > 0 and then
               (declare Node : constant Syntax_Trees.Node_Access := Stream (Stream.First);
                begin Node /= Syntax_Trees.Invalid_Node_Access and then
                   (Tree.Label (Node) = Syntax_Trees.Nonterm and
                      Tree.First_Terminal (Node) /= Syntax_Trees.Invalid_Node_Access)),
     Post =>
       (declare Node : constant Syntax_Trees.Node_Access := Stream (Stream.First);
        begin Node /= Syntax_Trees.Invalid_Node_Access and then
           (Tree.Label (Node) in Syntax_Trees.Terminal_Label));
   --  Bring the first terminal in Stream (which cannot be empty) to
   --  Stream.

   procedure Do_Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration);
   --  Delete Config Current_Token. Does not append to Config.Ops.

   type Parse_Item is record
      Config      : Configuration;
      Action      : Parse_Action_Node_Ptr;
      Parsed      : Boolean := False;
      Shift_Count : Natural := 0;

      --  On return from Parse, if Parsed = False, this item was queued by a
      --  conflict, but not parsed; it should be ignored.
      --
      --  Otherwise, if Config.Error_Token.ID = Invalid_Token_ID and
      --  Config.User_Parse_Action_Status.Label = Ok, Config was parsed
      --  successfully to the goal.
      --
      --  Otherwise, the parser failed a semantic check, or encountered an
      --  Error action. Action gives the last action processed. Shift_Count
      --  gives the number of shifts performed. If
      --  User_Parse_Action_Status.Label is Error, Action.Item.Verb must be
      --  Reduce, and Config is in the pre-reduce state.
   end record;

   package Parse_Item_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive, Parse_Item, Default_Element => (others => <>), Capacity => 10);
   --  Parse_Item_Arrays.Capacity sets maximum conflicts in one call to Parse

   package Parse_Item_Array_Refs is new Parse_Item_Arrays.Gen_Refs;

   function Parse
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased    out          Parse_Item_Arrays.Vector;
      Config            :         in              Configuration;
      Shared_Token_Goal :         in              Syntax_Trees.Node_Index;
      All_Conflicts     :         in              Boolean;
      Trace_Prefix      :         in              String)
     return Boolean;
   --  Attempt to parse Config and any conflict configs. A config is
   --  parsed when Config.Insert_Delete is all processed, and either
   --  Shared_Token_Goal = Invalid_Token_Index, or Shared_Token_Goal is
   --  shifted or an error is encountered. If All_Conflicts, return when
   --  all conflict configs have been parsed; if not All_Conflicts,
   --  return when one config is parsed without error.
   --
   --  Parsed configs are in Parse_Items; there is more than one if a
   --  conflict is encountered. Parse returns True if at least one
   --  Parse_Item parsed successfully to the goal. In that case, the
   --  other items are either not parsed or failed. See comment in
   --  Parse_Item for more detail.
   --
   --  Raises Bad_Config if parse encounters Unknown_State.

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
