--  Abstract :
--
--  Common utilities for LR parser table generators. These are not in
--  LR, because they are only needed at generator time, not parse
--  time.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with FastToken.Parser.LR1_Items;
with FastToken.Production;
generic
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   with package LR1_Items is new Parser.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, Nonterminal, Production);
package FastToken.Parser.LR.Generator_Utils is

   --  Alphabetical order

   procedure Add_Action
     (Symbol               : in     Token.Terminal_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LR1_Items.Item_Set;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean);
   --  Add (Symbol, Action) to Action_List; check for conflicts
   --
   --  Closure .. Conflicts are for conflict reporting

   procedure Add_Actions
     (Closure              : in     LR1_Items.Item_Set;
      Table                : in out Parse_Table;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Trace                : in     Boolean);
   --  Add actions for Closure to Table. Has_Empty_Production,
   --  Conflicts used for conflict reporting.

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item_Ptr;
      Action_List          : in out Action_Node_Ptr;
      Has_Empty_Production : in     LR1_Items.Nonterminal_ID_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Trace                : in     Boolean);
   --  Add actions for Item.Lookaheads to Action_List
   --  Closure must be from the item set containing Item.
   --  Has_Empty_Production .. Closure used for conflict reporting.

   function Find
     (Symbol      : in Token.Terminal_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr;

   function Find
     (Closure              : in LR1_Items.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token.Token_ID;
      Has_Empty_Production : in LR1_Items.Nonterminal_ID_Set)
     return Token.Token_ID;

   function Is_Present (Item : in Conflict; Conflicts : in Conflict_Lists.List) return Boolean;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean;

end FastToken.Parser.LR.Generator_Utils;
