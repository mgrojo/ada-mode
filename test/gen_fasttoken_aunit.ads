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

with Ada.Text_IO;
with AUnit.Checks;
with FastToken.Lexer;
with FastToken.Parser.LR;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Token;
generic
   type Token_ID is (<>);
   First_Terminal    : in Token_ID;
   Last_Terminal     : in Token_ID;
   --  We assume: Last_Terminal = EOF_ID, Token_ID'Succ (Last_Terminal) = Accept_ID
   with package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, Last_Terminal, Token_ID'Image);
   with package Production is new FastToken.Production (Token_Pkg);
   with package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   with package Parser_Root is new FastToken.Parser
     (Token_ID, First_Terminal, Last_Terminal, Last_Terminal, Token_ID'Succ (Last_Terminal), Token_ID'Image,
      Ada.Text_IO.Put, Token_Pkg, Lexer_Root);
   First_State_Index : in Integer;
   with package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width);
   with package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, Production);
   Grammar           : in Production.List.Instance;
package Gen_FastToken_AUnit is

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Token_ID);

   procedure Check
     (Label    : in String;
      Computed : in Token_Pkg.Buffer_Region;
      Expected : in Token_Pkg.Buffer_Region);

   procedure Check
     (Label    : in String;
      Computed : in Token_Pkg.Instance;
      Expected : in Token_Pkg.Instance);

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
      Index_Type  => Token_Pkg.Reporting_ID,
      Array_Type  => Token_Pkg.Token_ID_Set,
      Check_Index => Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Token_Pkg.Nonterminal_ID,
      Array_Type  => Token_Pkg.Nonterminal_ID_Set,
      Check_Index => Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Token_Pkg.Token_ID_Set,
      Index_Type  => Token_Pkg.Nonterminal_ID,
      Array_Type  => Token_Pkg.Nonterminal_Array_Token_Set,
      Check_Index => Check,
      Check_Item  => Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Boolean,
      Index_Type  => Token_Pkg.Terminal_ID,
      Array_Type  => Token_Pkg.Terminal_ID_Set,
      Check_Index => Check,
      Check_Item  => AUnit.Checks.Check);

   procedure Check
     is new AUnit.Checks.Gen_Check_Array
     (Item_Type   => Token_Pkg.Terminal_ID_Set,
      Index_Type  => Token_Pkg.Nonterminal_ID,
      Array_Type  => Token_Pkg.Nonterminal_Array_Terminal_Set,
      Check_Index => Check,
      Check_Item  => Check);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Lookahead;
      Expected : in LR1_Items.Lookahead);

   procedure Check
     (Label            : in String;
      Computed         : in LR1_Items.Item_Ptr;
      Expected         : in LR1_Items.Item_Ptr;
      Match_Lookaheads : in Boolean);

   procedure Check
     (Label            : in String;
      Computed         : in LR1_Items.Item_Set;
      Expected         : in LR1_Items.Item_Set;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Goto_Item_Ptr;
      Expected : in LR1_Items.Goto_Item_Ptr);

   procedure Check
     (Label            : in String;
      Computed         : in LR1_Items.Item_Set_Ptr;
      Expected         : in LR1_Items.Item_Set_Ptr;
      Match_Lookaheads : in Boolean := True);

   procedure Check
     (Label    : in String;
      Computed : in LR1_Items.Item_Set_List;
      Expected : in LR1_Items.Item_Set_List);

   function Get_Production (Prod : in Positive) return Production.List.List_Iterator;
   function Get_Production (Prod : in Positive) return Production.Instance;
   --  Return Prod production in Grammar; 1 indexed.

   function Get_Item_Node
     (Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in LR1_Items.Lookahead;
      State      : in LR.Unknown_State_Index := LR.Unknown_State)
     return LR1_Items.Item_Ptr;
   --  Construct an LR1_Items item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function Get_Item
     (Prod       : in Positive;
      Dot        : in Positive;
      Lookaheads : in LR1_Items.Lookahead;
      State      : in LR.Unknown_State_Index := LR.Unknown_State)
     return LR1_Items.Item_Ptr
     renames Get_Item_Node;

   function "+" (Item : in LR1_Items.Item_Ptr) return LR1_Items.Item_Set;
   function "+" (Item : in LR1_Items.Item_Ptr) return LR1_Items.Item_Set_Ptr;

   function "+" (State : in LR.Unknown_State_Index; Item : in LR1_Items.Item_Ptr) return LR1_Items.Item_Set_List;
   function "&"
     (Left  : in LR1_Items.Item_Set_List;
      Right : in LR1_Items.Item_Set_List)
     return LR1_Items.Item_Set_List;

   function Get_Set
     (To_State : in LR.State_Index;
      Set_List : in LR1_Items.Item_Set_List)
     return LR1_Items.Item_Set_Ptr;

   type AUnit_Goto_Item is record
      Symbol : Token_ID;
      Set    : LR1_Items.Item_Set_Ptr;
   end record;

   function "+" (Right : in AUnit_Goto_Item) return LR1_Items.Goto_Item_Ptr;
   function "&" (Left : in LR1_Items.Goto_Item_Ptr; Right : in AUnit_Goto_Item) return LR1_Items.Goto_Item_Ptr;

   procedure Add_Gotos
     (List  : in LR1_Items.Item_Set_List;
      State : in LR.State_Index;
      Gotos : in LR1_Items.Goto_Item_Ptr);

   function Get_Item_Set
     (Prod      : in Positive;
      Dot       : in Positive;
      Lookahead : in LR1_Items.Lookahead)
     return LR1_Items.Item_Set;

   function Get_Item_Set
     (Prod : in Positive;
      Dot  : in Positive;
      Next : in LR1_Items.Item_Set_Ptr)
     return LR1_Items.Item_Set;
   --  Construct an LR1_Items item_set with Prod from Grammar, Dot before
   --  token Dot (1 indexed; use last + 1 for after last), null lookaheads
   --  and goto_list.

   type Token_Array is array (Positive range <>) of Token_ID;

   function "+" (Item : in Token_Array) return LR1_Items.Lookahead;

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
