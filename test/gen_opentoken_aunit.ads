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
with OpenToken.Production.Parser.LRk_Item;
with OpenToken.Production.Parser;
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
   with package LR1 is new Parsers.LRk_Item (1);
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

   procedure Check (Label : in String; Computed : in LR1.Item_Lookahead_Ptr; Expected : in LR1.Item_Lookahead_Ptr);

   procedure Check (Label : in String; Computed : in LR1.Item_Ptr; Expected : in LR1.Item_Ptr);

   procedure Check (Label : in String; Computed : in LR1.Item_Set; Expected : in LR1.Item_Set);

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LR1.Item_Lookahead_Ptr;
      Dot        : in Integer;
      Next       : in LR1.Item_Ptr)
     return LR1.Item_Ptr;
   --  Construct an LR1 item with Prod from Grammar, Dot before token Dot.

   function Get_Item_Set
     (Prod       : in Integer;
      Dot        : in Integer;
      Next       : in LR1.Item_Set_Ptr)
     return LR1.Item_Set;
   --  Construct an LR1 item_set with Prod from Grammar, Dot before token Dot, null lookaheads and goto_list.

   type Token_Array is array (Positive range <>) of Token_ID;

   function "+" (Item : in Token_Array) return LR1.Item_Lookahead_Ptr;

end Gen_OpenToken_AUnit;
