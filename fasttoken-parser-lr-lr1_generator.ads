--  Abstract :
--
--  LR1 (Left-to-right scanning 1 look-ahead) parser table generator.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book").
--
--  Copyright (C) 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
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

pragma License (GPL);

with FastToken.Production;
with FastToken.Parser.LR1_Items;
generic
   EOF_Token : in Token_Pkg.Token_ID;
   pragma Unreferenced (EOF_Token);
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   with package LR1_Items is new FastToken.Parser.LR1_Items
     (Unknown_State_Index, Unknown_State, Nonterminal, Production);
   --  FIXME: either move this LR1_Items into package, or move LALR_Generators.LR1_Items out
package FastToken.Parser.LR.LR1_Generator is

   function Generate
     (Grammar                  : in Production.List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr;
   --  Generate a generalized LR1 parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  If Trace, output debug info to Standard_Error about generation
   --  process. We don't use FastToken.Trace here; we often want to
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
   --  Visible for unit test

   function LR1_Item_Sets
     (Has_Empty_Production : in LR1_Items.Nonterminal_ID_Set;
      First                : in LR1_Items.Derivation_Matrix;
      Grammar              : in Production.List.Instance;
      EOF_Token            : in Token.Token_ID;
      First_State_Index    : in Unknown_State_Index;
      Trace                : in Boolean)
     return LR1_Items.Item_Set_List;

end FastToken.Parser.LR.LR1_Generator;
