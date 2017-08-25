--  Abstract :
--
--  Generalized LALR parse table generator.
--
--  Copyright (C) 2002 - 2003, 2009 - 2010, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
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

with WisiToken.Parser.LR.Generator_Utils; use WisiToken.Parser.LR.Generator_Utils;
with WisiToken.Parser.LR.LR1_Items;
with WisiToken.Production;
package WisiToken.Parser.LR.LALR_Generator is

   function Generate
     (Grammar                  : in Production.List.Instance;
      Descriptor               : in LALR_Descriptor;
      First_State_Index        : in State_Index;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param           : in McKenzie_Param_Type := Default_McKenzie_Param;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr;
   --  Generate a generalized LALR parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  If Trace, output debug info to Standard_Error about generation
   --  process. We don't use WisiToken.Trace here; we often want to
   --  see a trace of the parser execution without the parser
   --  generation.
   --
   --  If Put_Parse_Table, output the parse table to Standard_Output
   --
   --  Unless Ignore_Unused_Tokens is True, raise Grammar_Error if
   --  there are unused tokens.
   --
   --  Unless Ignore_Unknown_Conflicts is True, raise Grammar_Error if there
   --  are unknown conflicts.

   ----------
   --  Visible for unit tests

   function LALR_Goto_Transitions
     (Kernel     : in LR1_Items.Item_Set;
      Symbol     : in Token_ID;
      First      : in Token_Array_Token_Set;
      Grammar    : in Production.List.Instance;
      Descriptor : in LALR_Descriptor;
      Trace      : in Boolean)
     return LR1_Items.Item_Set;

   function LALR_Kernels
     (Grammar           : in Production.List.Instance;
      First             : in Token_Array_Token_Set;
      First_State_Index : in State_Index;
      Descriptor        : in LALR_Descriptor;
      Trace             : in Boolean)
     return LR1_Items.Item_Set_List;

end WisiToken.Parser.LR.LALR_Generator;
