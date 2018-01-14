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

with WisiToken.LR.Parser_Lists;
package WisiToken.LR.McKenzie_Recover is

   type Recover_Status is
     (Fail,    --  Error is not fixed; fail parse with Syntax_Error
      Success, --  Error is fixed; parser continue with Resume_Active = True
      Ignore); --  Error is a semantic check, and is ignored; parser continue with Resume_Active = False

   function Recover
     (Shared_Parser : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List)
     return Recover_Status;
   --  Attempt to modify Parsers state and Parser.Lookahead to allow
   --  recovering from an error state.

end WisiToken.LR.McKenzie_Recover;
