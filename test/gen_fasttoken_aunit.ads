--  Abstract :
--
--  AUnit routines useful in FastToken tests
--
--  Copyright (C) 2013-2015 Stephen Leake.  All Rights Reserved.
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
with FastToken.Parser.LALR.Generator;
with FastToken.Production;
with FastToken.Token.Nonterminal;
generic
   type Token_ID is (<>);
   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;
   with package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, Last_Terminal, Token_ID'Image);
   with package Nonterminal is new Token_Pkg.Nonterminal;
   with package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   with package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   with package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   First_State_Index : in Integer;
   with package LALR is new Parser_Root.LALR (First_State_Index, Nonterminal => Nonterminal);
   with package LALR_Generator is new LALR.Generator (Token_ID'Width, Production);
   Grammar : in Production.List.Instance;
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
     (Label    : in String;
      Computed : in LALR_Generator.LR1.Item_Lookahead_Ptr;
      Expected : in LALR_Generator.LR1.Item_Lookahead_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generator.LR1.Item_Ptr;
      Expected : in LALR_Generator.LR1.Item_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generator.LR1.Item_Set;
      Expected : in LALR_Generator.LR1.Item_Set);

   procedure Check
     (Label    : in String;
      Computed : in LALR_Generator.LR1.Set_Reference_Ptr;
      Expected : in LALR_Generator.LR1.Set_Reference_Ptr);

   function Get_Production (Prod : in Integer) return Production.Instance;
   --  Return Prod production in Grammar.

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LALR_Generator.LR1.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LALR_Generator.LR1.Item_Ptr := null;
      State      : in LALR.Unknown_State_Index    := LALR.Unknown_State)
     return LALR_Generator.LR1.Item_Ptr;
   --  Construct an LR1 item with Prod from Grammar, Dot before token
   --  Dot (1 indexed; use last + 1 for after last).

   function Get_Item_Set
     (Prod : in Integer;
      Dot  : in Integer;
      Next : in LALR_Generator.LR1.Item_Set_Ptr)
     return LALR_Generator.LR1.Item_Set;
   --  Construct an LR1 item_set with Prod from Grammar, Dot before
   --  token Dot (1 indexed; use last + 1 for after last), null lookaheads
   --  and goto_list.

   type Token_Array is array (Positive range <>) of Token_ID;

   function "+" (Item : in Token_Array) return LALR_Generator.LR1.Item_Lookahead_Ptr;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (LALR.Parse_Action_Verbs);
   procedure Check is new AUnit.Checks.Gen_Check_Discrete (LALR.Unknown_State_Index);

   procedure Check (Label : in String; Computed : in LALR.Parse_Action_Rec; Expected : in LALR.Parse_Action_Rec);

   procedure Check
     (Label    : in String;
      Computed : in LALR.Parse_Action_Node_Ptr;
      Expected : in LALR.Parse_Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LALR.Action_Node_Ptr; Expected : in LALR.Action_Node_Ptr);

   procedure Check (Label : in String; Computed : in LALR.Goto_Node_Ptr; Expected : in LALR.Goto_Node_Ptr);

   procedure Check
     (Label    : in String;
      Computed : in LALR.Parse_State;
      Expected : in LALR.Parse_State);

end Gen_FastToken_AUnit;
