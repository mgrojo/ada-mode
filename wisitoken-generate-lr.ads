--  Abstract :
--
--  Common utilities for LR parser table generators.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with SAL.Gen_Unbounded_Definite_Vectors_Sorted;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with WisiToken.Generate.LR1_Items;
with WisiToken.Parse.LR;
with WisiToken.Productions;
package WisiToken.Generate.LR is
   use WisiToken.Parse.LR;

   subtype Conflict_Parse_Actions is Parse_Action_Verbs range Shift .. Accept_It;
   type Conflict_Item is record
      Action : Conflict_Parse_Actions;
      LHS    : Token_ID;
   end record;

   function Conflict_Item_Compare (Left, Right : in Conflict_Item) return SAL.Compare_Result
   is (if Left.Action > Right.Action
       then SAL.Greater
       elsif Left.Action < Right.Action
       then SAL.Less
       else
         (if Left.LHS > Right.LHS
          then SAL.Greater
          elsif Left.LHS < Right.LHS
          then SAL.Less
          else SAL.Equal));

   function To_Key (Item : in Conflict_Item) return Conflict_Item
   is (Item);

   package Conflict_Item_Lists is new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Element_type => Conflict_Item,
      Key_Type     => Conflict_Item,
      To_Key       => To_Key,
      Key_Compare  => Conflict_Item_Compare);

   type Conflict is record
      --  In the parse table, a "conflict" occurs when there are two or more
      --  actions for one token in a state:
      --
      --  RIGHT_PAREN => shift and goto state 833 222.1,
      --                 reduce 1 tokens to explicit_actual_parameter 256.1,
      --                 reduce 1 tokens to subtype_indication 118.3,
      --                 reduce 1 tokens to primary 209.3
      --
      --  The same conflict can occur in multiple states.
      --
      --  The user must declare all known conflicts in the grammar file;
      --  this helps them eliminate unnecessary conflicts, which can
      --  significantly slow both normal parsing and error recovery.
      --
      --  We identify the conflict by the token, and the action and LHS of
      --  all the productions involved. We also store all the states it
      --  occurs in, for debugging.
      On     : Token_ID;
      Items  : Conflict_Item_Lists.Vector;
      States : State_Index_Arrays.Vector;
   end record;

   function Image (Conflict : in LR.Conflict; Descriptor : in WisiToken.Descriptor) return String;

   function Conflict_Compare (Left, Right : in Conflict) return SAL.Compare_Result;
   --  Sort on On, Items.

   function To_Key (Item : in Conflict) return Conflict
   is (Item);

   package Conflict_Lists is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Conflict,
      Key_Type     => Conflict,
      Key          => To_Key,
      Key_Compare  => Conflict_Compare);

   procedure Put
     (Item       : in Conflict_Lists.Tree;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor);

   procedure Check_Conflicts
     (Label            : in     String;
      Found_Conflicts  : in out Conflict_Lists.Tree;
      Known_Conflicts  : in out Conflict_Lists.Tree;
      File_Name        : in     String;
      Descriptor       : in     WisiToken.Descriptor;
      Ignore_Conflicts : in     Boolean);
   --  Compare Found and Known Conflicts. If they differ, and
   --  Ignore_Conflicts is false, output appropriate error messages.

   type Conflict_Count is record
      Accept_Reduce : Integer := 0;
      Shift_Reduce  : Integer := 0;
      Reduce_Reduce : Integer := 0;
   end record;

   package Conflict_Count_Lists is new SAL.Gen_Unbounded_Definite_Vectors
     (State_Index, Conflict_Count, Default_Element => (others => <>));

   procedure Collect_Conflicts
     (Table           : in Parse_Table;
      Conflicts       : in out Conflict_Lists.Tree;
      Conflict_Counts : in out Conflict_Count_Lists.Vector);

   ----------
   --  Build parse table

   procedure Add_Action
     (Symbol      : in     Token_ID;
      Action      : in     Parse_Action_Rec;
      Action_List : in out Action_Arrays.Vector;
      Descriptor  : in     WisiToken.Descriptor);
   --  Add (Symbol, Action) to Action_List

   procedure Add_Actions
     (Closure    : in     LR1_Items.Item_Set;
      Table      : in out Parse_Table;
      Grammar    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor);
   --  Add actions for Closure to Table.

   procedure Add_Lookahead_Actions
     (Item        : in     LR1_Items.Item;
      Action_List : in out Action_Arrays.Vector;
      Grammar     : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor  : in     WisiToken.Descriptor);
   --  Add actions for Item.Lookaheads to Action_List
   --  Closure must be from the item set containing Item.

   ----------
   --  Minimal terminal sequences.

   package RHS_Sequence_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Token_ID_Arrays.Vector, Default_Element => Token_ID_Arrays.Empty_Vector);

   function Image is new RHS_Sequence_Arrays.Gen_Image_Aux (Descriptor, Trimmed_Image, Image_No_Assoc);

   function Min_Length (Item : in RHS_Sequence_Arrays.Vector) return Ada.Containers.Count_Type;
   --  Return minimum length of elements of Item.

   function Min (Item : in RHS_Sequence_Arrays.Vector) return Token_ID_Arrays.Vector;
   --  Return element of Item with minimum length;

   type Minimal_Sequence_Item is record
      Min_RHS  : Natural := Natural'Last;
      Sequence : RHS_Sequence_Arrays.Vector;
   end record;

   type Minimal_Sequence_Array is array (Token_ID range <>) of Minimal_Sequence_Item;

   function Compute_Minimal_Terminal_Sequences
     (Descriptor        : in WisiToken.Descriptor;
      Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      Grammar_File_Name : in String)
     return Minimal_Sequence_Array;
   --  For each production in Grammar, compute the minimal sequence of
   --  terminals that will complete it. Result is an empty sequence if
   --  the production may be empty.
   --
   --  If some minimal sequences cannot be computed due to bad grammar
   --  structure, an error message using Grammar_File_Name is put to
   --  Current_Error, and Parse_Error is raised.

   function Compute_Minimal_Terminal_First
     (Descriptor                 : in WisiToken.Descriptor;
      Minimal_Terminal_Sequences : in Minimal_Sequence_Array)
      return Token_Array_Token_ID;
   --  For each nonterminal in Grammar, return the first of the minimal
   --  sequence of terminals that will complete it; Invalid_Token_ID if
   --  the minimal sequence is empty.

   procedure Set_Minimal_Complete_Actions
     (State                      : in out Parse_State;
      Kernel                     : in     LR1_Items.Item_Set;
      Descriptor                 : in     WisiToken.Descriptor;
      Grammar                    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Nullable                   : in     Token_Array_Production_ID;
      Minimal_Terminal_Sequences : in     Minimal_Sequence_Array;
      Minimal_Terminal_First     : in     Token_Array_Token_ID);
   --  Set State.Minimal_Complete_Actions to the set of actions that will
   --  most quickly complete the productions in Kernel (which must be for
   --  State). Useful in error correction.
   --
   --  The Minimal_Complete_Actions will be empty in a state where there
   --  is nothing useful to do; the accept state, or one where all
   --  productions are recursive.
   --
   --  Also set State.Kernels; used to resolve multiple reduce actions at
   --  runtime.

   ----------
   --  Parse table output

   procedure Put_Text_Rep
     (Table        : in Parse_Table;
      File_Name    : in String;
      Action_Names : in Names_Array_Array;
      Check_Names  : in Names_Array_Array);
   --  Write machine-readable text format of Table.States to a file
   --  File_Name, to be read by the parser executable at startup, using
   --  WisiToken.Parse.LR.Get_Text_Rep.

   procedure Put (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor);
   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor);
   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor; State : in Parse_State);
   --  Put Item to Ada.Text_IO.Current_Output in parse table format.

   procedure Put_Parse_Table
     (Table                 : in Parse_Table_Ptr;
      Parse_Table_File_Name : in String;
      Title                 : in String;
      Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Recursions            : in Generate.Recursions;
      Kernels               : in LR1_Items.Item_Set_List;
      Conflicts             : in Conflict_Count_Lists.Vector;
      Descriptor            : in WisiToken.Descriptor;
      Include_Extra         : in Boolean := False);
   --  "Extra" is recursions.

end WisiToken.Generate.LR;
