--  Abstract :
--
--  LR1 (Left-to-right scanning 1 look-ahead) parser table generator.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  Copyright (C) 2017, 2018 Stephe Leake
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

with WisiToken.LR.Generator_Utils;
with WisiToken.LR.LR1_Items;
with WisiToken.Productions;
package WisiToken.LR.LR1_Generator is

   use Generator_Utils;

   function Generate
     (Grammar                  : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor               : in WisiToken.Descriptor;
      First_State_Index        : in State_Index;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param           : in McKenzie_Param_Type := Default_McKenzie_Param;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr;
   --  Generate a generalized LR1 parse table for Grammar. The
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
   --  visible for unit test

   function LR1_Goto_Transitions
     (Set                  : in LR1_Items.Item_Set;
      Symbol               : in Token_ID;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set;
   --  'goto' from [dragon] algorithm 4.9

   function LR1_Item_Sets
     (Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      First_State_Index    : in State_Index;
      Descriptor           : in WisiToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set_List;
   --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

end WisiToken.LR.LR1_Generator;
