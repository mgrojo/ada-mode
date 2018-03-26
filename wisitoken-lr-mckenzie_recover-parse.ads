--  Abstract :
--
--  Config parsing subprograms.
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

with WisiToken.LR.McKenzie_Recover.Base;
private package WisiToken.LR.McKenzie_Recover.Parse is

   function Compute_Nonterm
     (Stack  : in Recover_Stacks.Stack;
      Action : in Reduce_Action_Rec)
     return Recover_Token;

   function Reduce_Stack
     (Shared  : not null access Base.Shared_Lookahead;
      Stack   : in out          Recover_Stacks.Stack;
      Action  : in              Reduce_Action_Rec;
      Nonterm :    out          Recover_Token)
     return Semantic_Checks.Check_Status;

   type Parse_Item is record
      Config : Configuration;
      Action : Parse_Action_Node_Ptr;
      Parsed : Boolean;

      --  On return from Parse, if Parsed is False, this item was queued by
      --  a conflict, but not parsed; it should be ignored.
      --
      --  Otherwise, If Action.Item.Verb is Error, the parse failed.
      --
      --  Otherwise, if Config.Check_Status.Label is Ok, Config was parsed
      --  successfully to the goal.
      --
      --  Otherwie, Check_Status.Label must be Error, Action.Item.Verb must
      --  be Reduce, and Semantic_Check_Fixes can be called; Config is in
      --  the pre-reduce state.
   end record;

   package Parse_Item_Arrays is new SAL.Gen_Bounded_Definite_Vectors (Positive, Parse_Item, Capacity => 10);
   --  Parse_Item_Arrays.Capacity sets maximum conflicts in one call to Parse

   function Parse
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Items       :    out          Parse_Item_Arrays.Vector;
      Config            : in              Configuration;
      Shared_Token_Goal : in              Base_Token_Index;
      Trace_Prefix      : in              String)
     return Boolean;
   --  Attempt to parse Config until Config.Inserted is all shifted, and
   --  either Shared_Token_Goal = Invalid_Token_Index or
   --  Shared_Token_Goal is shifted.
   --
   --  Parsed configs are in Parse_Items; there is more than one if a
   --  conflict is encountered. Parse returns True if at least one
   --  Parse_Item parsed successfully to the goal. In that case, the
   --  other items are either not parsed or failed. See comment in
   --  Parse_Item for more detail.

end WisiToken.LR.McKenzie_Recover.Parse;
