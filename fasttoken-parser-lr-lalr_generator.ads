--  Abstract :
--
--  Generalized LALR parse table generator.
--
--  Copyright (C) 2002 - 2003, 2009 - 2010, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

pragma License (Modified_GPL);

with FastToken.Parser.LR1_Items;
with FastToken.Production;
generic
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
package FastToken.Parser.LR.LALR_Generator is

   package LR1_Items is new Parser.LR1_Items
     (Unknown_State_Index, Unknown_State, Nonterminal, Production);

   function Generate
     (Grammar                  : in Production.List.Instance;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
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
   --  Visible for unit tests

   procedure Fill_In_Lookaheads
     (Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      First                : in     LR1_Items.Derivation_Matrix;
      Kernels              : in out LR1_Items.Item_Set_List;
      Accept_State         : in     State_Index;
      Used_Tokens          : in out Token.Token_Array_Boolean;
      Trace                : in     Boolean);

   procedure Add_Actions
     (Kernel               : in     LR1_Items.Item_Set_Ptr;
      Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      First                : in     LR1_Items.Derivation_Matrix;
      Conflicts            : in out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean);

end FastToken.Parser.LR.LALR_Generator;
