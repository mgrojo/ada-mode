--  Abstract :
--
--  AUnit routines useful in FastToken tests
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks;
with FastToken.Lexer;
with FastToken.Parser.LR;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Token.Nonterminal;
generic
   type Token_ID is (<>);
   First_Terminal    : in Token_ID;
   Last_Terminal     : in Token_ID;
   with package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, Last_Terminal, Token_ID'Image);
   with package Nonterminal is new Token_Pkg.Nonterminal;
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   with package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   with package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   First_State_Index : in Integer;
   with package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal => Nonterminal);
   with package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, Nonterminal, Production);
   Grammar           : in Production.List.Instance;
package Gen_FastToken_AUnit is

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Token_ID);

   procedure Check
     (Label    : in String;
      Computed : in Token_Pkg.List.List_Iterator;
      Expected : in Token_Pkg.List.List_Iterator);

   procedure Check
     (Label    : in String;
      Computed : in Production.Instance;
      Expected : in Production.Instance);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Token_ID,
      Array_Type  => LR1_Items.Token_ID_Set,
      Check_Index => Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Parser_Root.Nonterminal_ID,
      Array_Type  => LR1_Items.Nonterminal_ID_Set,
      Check_Index => Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => LR1_Items.Token_ID_Set,
      Index_Type  => Parser_Root.Nonterminal_ID,
      Array_Type  => LR1_Items.Derivation_Matrix,
      Check_Index => Check,
      Check_Item  => Check);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Lookahead_Ptr;
      Expected : in LR1_Items.Lookahead_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Ptr;
      Expected : in LR1_Items.Item_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Set;
      Expected : in LR1_Items.Item_Set);

   function "&" (Left, Right : in LR1_Items.Goto_Item) return LR1_Items.Goto_Item_Ptr;
   function "&"
     (Left  : in LR1_Items.Goto_Item_Ptr;
      Right : in LR1_Items.Goto_Item)
     return LR1_Items.Goto_Item_Ptr;

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Goto_Item_Ptr;
      Expected : in LR1_Items.Goto_Item_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Set_Ptr;
      Expected : in LR1_Items.Item_Set_Ptr);

   function Get_Production (Prod : in Integer) return Production.Instance;
   --  Return Prod production in Grammar.

   function Get_Item_Node
     (Prod       : in Integer;
      Dot        : in Integer;
      Lookaheads : in LR1_Items.Lookahead_Ptr;
      Next       : in LR1_Items.Item_Ptr := null;
      State      : in LR.Unknown_State_Index  := LR.Unknown_State)
     return LR1_Items.Item_Ptr;
   --  Construct an LR1_Items item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function "+" (Item : in LR1_Items.Item_Ptr) return LR1_Items.Item_Set_Ptr;

   function Get_Item_Set
     (Prod      : in Integer;
      Dot       : in Integer;
      Lookahead : in LR1_Items.Lookahead_Ptr)
     return LR1_Items.Item_Set;

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LR1_Items.Item_Set_Ptr)
     return LR1_Items.Item_Set;
   --  Construct an LR1_Items item_set with Prod from Grammar, Dot before
   --  token Dot (1 indexed; use last + 1 for after last), null lookaheads
   --  and goto_list.

   type Token_Array is array (Positive range <>) of Token_ID;

   function "+" (Item : in Token_ID) return LR1_Items.Lookahead_Ptr;
   function "+" (Item : in Token_Array) return LR1_Items.Lookahead_Ptr;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (LR.Parse_Action_Verbs);
   procedure Check is new AUnit.Checks.Gen_Check_Discrete (LR.Unknown_State_Index);

   procedure Check (Label : in String; Computed : in LR.Parse_Action_Rec; Expected : in LR.Parse_Action_Rec);

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Action_Node_Ptr;
      Expected : in LR.Parse_Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LR.Action_Node_Ptr; Expected : in LR.Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LR.Goto_Node_Ptr; Expected : in LR.Goto_Node_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_State;
      Expected : in LR.Parse_State);

   procedure Check
     (Label    : in String;
      Computed : in LR.Parse_Table;
      Expected : in LR.Parse_Table);

end Gen_FastToken_AUnit;
