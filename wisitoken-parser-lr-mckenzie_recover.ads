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
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with WisiToken.Parser.LR.Parser_Lists;
with WisiToken.Token;
package WisiToken.Parser.LR.McKenzie_Recover is

   function Recover
     (Parser  : in out LR.Instance'Class;
      Parsers : in out Parser_Lists.List)
     return Boolean;
   --  Attempt to modify Parsers state and Parser.Lookahead to allow
   --  recovering from an error state. Return True if successful.

   --  Visible for unit test, sending to Emacs process.
   type Configuration is new WisiToken.Token.Recover_Data with record
      Stack                  : Parser_Stacks.Stack_Type;
      Verb                   : All_Parse_Action_Verbs;
      Shared_Lookahead_Index : SAL.Base_Peek_Type; -- index into shared parser.lookahead for next input token

      Local_Lookahead        : Token_Arrays.Vector;
      Local_Lookahead_Index  : Ada.Containers.Count_Type;
      --  Local_Lookahead contains tokens inserted by special rules.
      --  It is not a queue type, because we always access it via
      --  Local_Lookahead_Index

      Popped   : Token_Arrays.Vector;
      Pushed   : Parser_Stacks.Stack_Type;
      Inserted : Token_Arrays.Vector;
      Deleted  : Token_Arrays.Vector;
      Cost     : Float := 0.0;
   end record;

   procedure Put (Descriptor : in WisiToken.Descriptor'Class; Config : in Configuration);
   --  Put Config to Ada.Text_IO.Current_Output

   Default_Configuration : constant Configuration :=
     (Stack                  => Parser_Stacks.Empty_Stack,
      Verb                   => Shift_Local_Lookahead,
      Shared_Lookahead_Index => 0,
      Local_Lookahead        => Token_Arrays.Empty_Vector,
      Local_Lookahead_Index  => Natural_Index_Type'First,
      Popped                 => Token_Arrays.Empty_Vector,
      Pushed                 => Parser_Stacks.Empty_Stack,
      Inserted               => Token_Arrays.Empty_Vector,
      Deleted                => Token_Arrays.Empty_Vector,
      Cost                   => 0.0);

end WisiToken.Parser.LR.McKenzie_Recover;
