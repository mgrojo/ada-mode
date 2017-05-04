--  Abstract :
--
--  Type and operations for building grammar
--  productions.
--
--  Copyright (C) 2003, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
with FastToken.Token.Nonterminal;
generic
   with package Token is new FastToken.Token (<>);
   with package Nonterminal is new Token.Nonterminal;
package FastToken.Production is

   type Right_Hand_Side is record
      Tokens : Token.List.Instance;
      Action : Nonterminal.Synthesize;
      Index  : Integer; -- In grammar rule
   end record;
   --  The Right Hand Side of a production is a token list "+"ed with a
   --  synthesization routine. For example:
   --
   --     Number & Minus_Sign & Number + Nonterminal.Synthesize_First
   --
   --  The synthesization routine is called whenever the production is
   --  reduced by the parser.

   function "+" (Tokens : in Token.List.Instance; Action : in Nonterminal.Synthesize) return Right_Hand_Side;
   function "+" (Tokens : in Token.Class; Action : in Nonterminal.Synthesize) return Right_Hand_Side;
   function "+" (Tokens : in Token.Token_ID; Action : in Nonterminal.Synthesize) return Right_Hand_Side;
   function "+" (Action : in Nonterminal.Synthesize) return Right_Hand_Side;

   function "+" (Tokens : in Token.List.Instance; Index  : in Integer) return Right_Hand_Side;
   function "+" (Tokens : in Token.Class; Index  : in Integer) return Right_Hand_Side;
   function "+" (Index  : in Integer) return Right_Hand_Side;
   --  Create the right hand side of a production.
   --
   --  The index is used to identify the production in error messages
   --  and generated grammar table source code.

   type Instance is record
      LHS : Nonterminal.Handle;
      RHS : Right_Hand_Side;
   end record;
   type Handle is access all Instance;
   --  A production consists of a nonterminal token instance "<=" ed to
   --  a Right Hand Side . For example:
   --
   --    Subtraction <= Number & Minus_Sign & Number + Nonterminal.Synthesize_Self

   function "<=" (LHS : in Token.Nonterminal_ID; RHS : in Right_Hand_Side) return Instance;
   function "<=" (LHS : in Nonterminal.Handle; RHS : in Right_Hand_Side) return Instance;
   function "<=" (LHS : in Nonterminal.Class; RHS : in Right_Hand_Side) return Instance;

   function First_Token (Item : in Instance) return Token.List.List_Iterator;

   package List is

      type Instance is tagged private;

      function Only (Subject : in Production.Instance) return Instance;
      function "+" (Subject : in Production.Instance) return Instance
        renames Only;

      function "and" (Left : in Production.Instance; Right : in Production.Instance) return Instance;
      function "and" (Left : in Production.Instance; Right : in Instance) return Instance;
      function "and" (Left : in Instance; Right : in Production.Instance) return Instance;
      function "and" (Left : in Instance; Right : in Instance) return Instance;

      procedure Clean (List : in out Instance);
      --  Delete all elements from list, freeing them.

      type List_Iterator is private;

      function First (List : in Instance) return List_Iterator;

      procedure Next (Iterator : in out List_Iterator);

      function Current (Iterator : in List_Iterator) return Production.Instance;

      function Last_Production (Iterator : in List_Iterator) return Boolean;
      --  Return true if the iterator is at the last production.

      function Is_Done (Iterator : in List_Iterator) return Boolean;
      --  Return true if the iterator is past the last production.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         Production : FastToken.Production.Instance;
         Next       : List_Node_Ptr;
      end record;

      type Instance is tagged record
         Head : List_Node_Ptr;
         Tail : List_Node_Ptr;
      end record;

      type List_Iterator is new List_Node_Ptr;

      procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

   end List;
end FastToken.Production;
