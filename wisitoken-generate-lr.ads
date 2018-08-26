--  Abstract :
--
--  Common utilities for LR parser table generators. These are not in
--  LR, because they are only needed at generator time, not parse
--  time.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with Ada.Containers.Doubly_Linked_Lists;
with WisiToken.LR.LR1_Items;
with WisiToken.Productions;
package WisiToken.Generate.LR is
   use WisiToken.LR;

   subtype Conflict_Parse_Actions is Parse_Action_Verbs range Shift .. Accept_It;
   type Conflict is record
      --  A typical conflict is:
      --
      --  SHIFT/REDUCE in state: 11 on token IS
      --
      --  State numbers change with minor changes in the grammar, so
      --  we identify the state by the LHS of the two productions
      --  involved. We also store the state number for generated
      --  conflicts (not for known conflicts from the grammar
      --  definition file), for Text_IO output.
      Action_A    : Conflict_Parse_Actions;
      LHS_A       : Token_ID;
      Action_B    : Conflict_Parse_Actions;
      LHS_B       : Token_ID;
      State_Index : Unknown_State_Index;
      On          : Token_ID;
   end record;

   package Conflict_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict);

   procedure Add_Action
     (Symbol               : in     Token_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Node_Ptr;
      Closure              : in     LR1_Items.Item_Set;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add (Symbol, Action) to Action_List; check for conflicts
   --
   --  Closure .. Conflicts are for conflict reporting

   procedure Add_Actions
     (Closure              : in     LR1_Items.Item_Set;
      Table                : in out Parse_Table;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add actions for Closure to Table. Has_Empty_Production, First,
   --  Conflicts used for conflict reporting.

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item;
      Action_List          : in out Action_Node_Ptr;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add actions for Item.Lookaheads to Action_List
   --  Closure must be from the item set containing Item.
   --  Has_Empty_Production .. Closure used for conflict reporting.

   procedure Delete_Known
     (Conflicts       : in out Conflict_Lists.List;
      Known_Conflicts : in out Conflict_Lists.List);
   --  Delete Known_Conflicts from Conflicts.

   function Find
     (Symbol      : in Token_ID;
      Action_List : in Action_Node_Ptr)
     return Action_Node_Ptr;

   function Find
     (Closure              : in LR1_Items.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token_ID;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Descriptor           : in WisiToken.Descriptor)
     return Token_ID;
   --  Return the LHS of a production in kernel of Closure, for an Action
   --  conflict on Lookahead; for naming a Conflict object.

   function Image (Descriptor : in WisiToken.Descriptor; Item : in Conflict) return String;

   function Is_Present (Item : in Conflict; Conflicts : in Conflict_Lists.List) return Boolean;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean;

   procedure Put
     (Item       : in Conflict_Lists.List;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor);

   procedure Put_Parse_Table
     (Table      : in Parse_Table_Ptr;
      Title      : in String;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Kernels    : in LR1_Items.Item_Set_List;
      Ancestors  : in Token_Array_Token_Set;
      Conflicts  : in Conflict_Lists.List;
      Descriptor : in WisiToken.Descriptor);

   procedure Compute_Minimal_Terminal_Sequences
     (Grammar    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor;
      Result     : in out Token_Sequence_Arrays.Vector);
   --  For each production in Grammar, compute the minimal sequence of
   --  terminals that will complete it. Result is an empty sequence if
   --  the production may be empty.

   function Minimal_Terminal_First
     (Grammar    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
      return Token_Array_Token_ID;
   --  For each nonterminal in Grammar, return the first of the minimal
   --  sequence of terminals that will complete it; Invalid_Token_ID if
   --  the minimal sequence is empty.

   procedure Set_Minimal_Complete_Actions
     (State                  : in out Parse_State;
      Kernel                 : in     LR1_Items.Item_Set;
      Minimal_Terminal_First : in     Token_Array_Token_ID;
      Ancestors              : in     Token_Array_Token_Set;
      Descriptor             : in     WisiToken.Descriptor;
      Grammar                : in     WisiToken.Productions.Prod_Arrays.Vector);
   --  Set State.Minimal_Terminal_First to the set of terminals that will
   --  most quickly complete the productions in Kernel (which must be for
   --  State). Useful in error correction when we know the next actual
   --  terminal is a block ending or statement start.

end WisiToken.Generate.LR;
