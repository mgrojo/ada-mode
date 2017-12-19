--  Abstract :
--
--  Type and operations for building grammar
--  productions.
--
--  Copyright (C) 2003, 2013 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
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

with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
with WisiToken.Semantic_State;
with WisiToken.Token_ID_Lists;
package WisiToken.Production is

   type Right_Hand_Side is record
      Tokens : WisiToken.Token_ID_Lists.List;
      Action : WisiToken.Semantic_State.Semantic_Action;
      --  No semantic_check here; only supported in Wisi source files.
      Index  : Integer;
      --  Index of production among productions for a single nonterminal (the LHS)
   end record;

   function Only (Item : in Token_ID) return WisiToken.Token_ID_Lists.List;
   function "&" (Left : in Token_ID; Right : in Token_ID) return WisiToken.Token_ID_Lists.List;

   function "+"
     (Tokens : in Token_ID_Lists.List;
      Action : in WisiToken.Semantic_State.Semantic_Action)
     return Right_Hand_Side;
   function "+"
     (Tokens : in Token_ID;
      Action : in WisiToken.Semantic_State.Semantic_Action)
     return Right_Hand_Side;
   function "+" (Action : in WisiToken.Semantic_State.Semantic_Action) return Right_Hand_Side;

   function "+" (Tokens : in Token_ID_Lists.List; Index  : in Integer) return Right_Hand_Side;
   function "+" (Tokens : in Token_ID; Index  : in Integer) return Right_Hand_Side;
   function "+" (Index  : in Integer) return Right_Hand_Side;
   --  Create the right hand side of a production.
   --
   --  The index is used to identify the production in error messages
   --  and generated grammar table source code.

   type Instance is record
      LHS : Token_ID;
      RHS : aliased Right_Hand_Side;
   end record;
   type Handle is access all Instance;

   function "<=" (LHS : in Token_ID; RHS : in Right_Hand_Side) return Instance;

   function First_Token (Item : in Instance) return Token_ID_Lists.Cursor;

   package List is

      type Instance is tagged private
      with
        Constant_Indexing => Constant_Reference,
        Default_Iterator  => Iterate,
        Iterator_Element  => Production.Instance;
      --  Instance is _not_ Controlled; assignment does a shallow copy of
      --  the root list pointers. WisiToken.LR.LR1_Items takes advantage of
      --  this.

      type Constant_Reference_Type (Element : not null access constant Production.Instance) is null record
      with Implicit_Dereference => Element;

      type List_Node_Ptr is private;

      function Constant_Reference
        (Container : aliased in Instance'Class;
         Position  :         in List_Node_Ptr)
        return Constant_Reference_Type;

      function Has_Element (Cursor : in List_Node_Ptr) return Boolean;
      package Iterator_Interfaces is new Ada.Iterator_Interfaces (List_Node_Ptr, Has_Element);
      function Iterate (Container : aliased Instance) return Iterator_Interfaces.Forward_Iterator'Class;

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
      function LHS (Iterator : in List_Iterator) return Token_ID;
      function RHS (Iterator : in List_Iterator) return Right_Hand_Side;

      function Last_Production (Iterator : in List_Iterator) return Boolean;
      --  Return true if the iterator is at the last production.

      function Is_Done (Iterator : in List_Iterator) return Boolean;
      function Is_Null (Iterator : in List_Iterator) return Boolean renames Is_Done;
      --  Return true if the iterator is past the last production.

   private
      type List_Node;
      type List_Node_Ptr is access List_Node;
      type List_Node is record
         Production : aliased WisiToken.Production.Instance;
         Next       : List_Node_Ptr;
      end record;

      type Instance is tagged record
         Head : List_Node_Ptr;
         Tail : List_Node_Ptr;
      end record;

      type List_Iterator is new List_Node_Ptr;

      procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

   end List;
end WisiToken.Production;
