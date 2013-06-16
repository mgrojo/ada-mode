--  Abstract :
--
--  AUnit routines useful in OpenToken tests
--
--  Copyright (C) 2013 Stephen Leake.  All Rights Reserved.
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

with AUnit.Check;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
generic
   type Token_ID is (<>);
   with package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_ID, Token_ID'Image, Token_ID'Width);
   with package Token_Lists is new Tokens_Pkg.List;
   with package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   with package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   with package Production_Lists is new Productions.List;
   First_Terminal : in Token_ID;
   Last_Terminal : in Token_ID;
   with package Analyzers is new Tokens_Pkg.Analyzer (First_Terminal, Last_Terminal);
   with package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   First_State_Index : in Integer;
   with package LALR is new Parsers.LALR (First_State_Index);
   Grammar : in Production_Lists.Instance;
package Gen_OpenToken_AUnit is

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_ID);

   procedure Check
     (Label    : in String;
      Computed : in Token_Lists.List_Iterator;
      Expected : in Token_Lists.List_Iterator);

   procedure Check
     (Label    : in String;
      Computed : in Productions.Instance;
      Expected : in Productions.Instance);

   procedure Check
     (Label    : in String;
      Computed : in LALR.LRk.Item_Lookahead_Ptr;
      Expected : in LALR.LRk.Item_Lookahead_Ptr);

   procedure Check (Label : in String; Computed : in LALR.LRk.Item_Ptr; Expected : in LALR.LRk.Item_Ptr);

   procedure Check (Label : in String; Computed : in LALR.LRk.Item_Set; Expected : in LALR.LRk.Item_Set);

   procedure Check
     (Label    : in String;
      Computed : in LALR.LRk.Set_Reference_Ptr;
      Expected : in LALR.LRk.Set_Reference_Ptr);

   function Get_Production (Prod : in Integer) return Productions.Instance;
   --  Return Prod production in Grammar.

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LALR.LRk.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LALR.LRk.Item_Ptr)
     return LALR.LRk.Item_Ptr;
   --  Construct an LR1 item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LALR.LRk.Item_Set_Ptr)
     return LALR.LRk.Item_Set;
   --  Construct an LR1 item_set with Prod from Grammar, Dot before
   --  token Dot (1 indexed; use last + 1 for after last), null lookaheads
   --  and goto_list.

   type Token_Array is array (Positive range <>) of Token_ID;

   function "+" (Item : in Token_Array) return LALR.LRk.Item_Lookahead_Ptr;

   procedure Check is new AUnit.Check.Gen_Check_Discrete (LALR.Parse_Action_Verbs);
   procedure Check is new AUnit.Check.Gen_Check_Discrete (LALR.State_Index);

   procedure Check (Label : in String; Computed : in LALR.Parse_Action_Rec; Expected : in LALR.Parse_Action_Rec);

   procedure Check
     (Label    : in String;
      Computed : in LALR.Parse_Action_Node_Ptr;
      Expected : in LALR.Parse_Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LALR.Action_Node_Ptr; Expected : in LALR.Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LALR.Reduction_Node_Ptr; Expected : in LALR.Reduction_Node_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LALR.Parse_State;
      Expected : in LALR.Parse_State);

end Gen_OpenToken_AUnit;
