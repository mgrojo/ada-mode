--  Abstract :
--
--  Implement [dragon] panic mode error recovery, extended to parallel parsers.
--
--  Algorithm:
--
--  loop
--      for all active parsers:
--          pop stack until top state s has a reduce action on a "good" nonterminal
--      end loop;
--
--      if all parsers at stack bottom, give up
--
--      loop
--          for all active parsers:
--              if next_input in Follow (good nonterm);
--                  push goto_for (s, nonterm)
--                      _don't_ call reduce action, since don't have all tokens
--          end loop
--
--          if any parsers active, exit all loops
--
--          discard next_input
--              save for next try
--
--          if reach disard limit, exit loop
--      end loop
--  end loop
--
--  continue with shift input; parsers that did not get adjusted above
--  will error and terminate.
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

with FastToken.Parser.LR.Parser_Lists;
package FastToken.Parser.LR.Panic_Mode is

   function Panic_Mode
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean;
   --  Attempt to modify Parsers stacks, and Parser.Lexer current
   --  input, to allow recovering from an error state.
   --  Return True if successful.

end FastToken.Parser.LR.Panic_Mode;
