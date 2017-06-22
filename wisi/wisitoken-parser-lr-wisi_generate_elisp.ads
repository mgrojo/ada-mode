--  Abstract :
--
--  Elisp output for Wisi
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Wisi;
package WisiToken.Parser.LR.Wisi_Generate_Elisp is

   procedure Output
     (Elisp_Package : in String;
      Tokens        : in Wisi.Token_Lists.List;
      Keywords      : in Wisi.String_Pair_Lists.List;
      Rules         : in Wisi.Rule_Lists.List;
      Parser        : in Parse_Table_Ptr;
      Descriptor    : in WisiToken.Descriptor'Class);

end WisiToken.Parser.LR.Wisi_Generate_Elisp;
