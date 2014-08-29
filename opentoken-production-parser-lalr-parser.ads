-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2009, 2010, 2013, 2014 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package provides the LALR parser.

with Ada.Iterator_Interfaces;
generic
package OpenToken.Production.Parser.LALR.Parser is

   type Instance is new OpenToken.Production.Parser.Instance with record
      Table        : Parse_Table_Ptr;
      Max_Parallel : Integer := 15;
   end record;

   overriding procedure Parse (Parser : in out Instance);

   ----------
   --  Visible for unit tests

   package Parser_Lists is

      type Parser_State is private;

      type List is tagged private
      with
        Constant_Indexing => Constant_Reference,
        Default_Iterator  => Iterate,
        Iterator_Element  => Parser_State;

      function Initialize return List;

      function Count (List : in Parser_Lists.List) return Integer;

      type Cursor is tagged private;

      function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
      procedure Next (Cursor : in out Parser_Lists.Cursor);
      function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;

      function Label (Cursor : in Parser_Lists.Cursor) return Integer;

      procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs);
      function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;

      --  Parser stack
      type Stack_Item is record
         State : Unknown_State_Index;
         Token : OpenToken.Production.Token.Handle;
      end record;

      function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
      function Peek (Cursor : in Parser_Lists.Cursor) return Stack_Item;
      function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item;
      procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item);

      procedure Put_Top_10 (Cursor : in Parser_Lists.Cursor);
      --  Put image of top 10 stack items to Current_Output.

      --  pending user actions
      type Action_Token is record
         Action      : Nonterminal.Synthesize;
         Token_Count : Integer;
         Tokens      : Token_List.Instance;
      end record;

      function Action_Tokens_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
      procedure Append (Cursor : in Parser_Lists.Cursor; Item : in Action_Token);
      function Pop (Cursor : in Parser_Lists.Cursor) return Action_Token;

      procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
      --  Copy parser at Cursor, add to current list. New copy will not
      --  appear in Cursor.Next ...; it is accessible as First (List).

      procedure Free (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
      --  Move Cursor to the internal free list, free its stack and
      --  pending actions; it will not appear in future iterations.

      ----------
      --  Stuff for iterators, to allow
      --  'for Parser of Parsers loop'
      --  'for Cursor in Parsers.First loop'
      --
      --  requires Parser_State to be not an incomplete type.

      --  We'd like to use Cursor here, but we want that to be tagged,
      --  to allow 'Cursor.Next' syntax, and the requirements of
      --  iterators prevent a tagged cursor type (two tagged types on
      --  First in body). So we define a separate Iterator_Cursor type just
      --  for Iterators.

      type Iterator_Cursor is private;

      type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
      with Implicit_Dereference => Element;

      function Constant_Reference
        (Container : aliased in List'Class;
         Position  : in Iterator_Cursor)
        return Constant_Reference_Type;

      function Has_Element (Cursor : in Iterator_Cursor) return Boolean;
      function Verb (Cursor : in Iterator_Cursor) return Parse_Action_Verbs;

      package Iterator_Interfaces is new Ada.Iterator_Interfaces (Iterator_Cursor, Has_Element);

      function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class;

      ----------
      --  For unit tests

      function Stack_Free_Count (List : in Parser_Lists.List) return Integer;

   private

      type Stack_Node;
      type Stack_Node_Access is access Stack_Node;
      type Stack_Node is record
         Item : Stack_Item;
         Next : Stack_Node_Access;
      end record;

      type Stack is record
         Head : Stack_Node_Access;
         --  Popped nodes go on the parsers free_stack list, pushed
         --  nodes come from the free list.
      end record;

      type Parser_State_Access is access Parser_State;

      type Parser_State is record
         Label : Integer;            -- for debugging
         Verb  : Parse_Action_Verbs; -- last action performed
         Stack : Parser_Lists.Stack;
         --  Pending_Actions : Action_Token_List; -- FIXME: accumulated actions while parallel parsing
         Next  : Parser_State_Access;
      end record;

      type Cursor is tagged record
         List : access Parser_Lists.List;
         Ptr  : Parser_State_Access;
      end record;

      type Iterator_Cursor is access all Parser_State;

      type List is tagged record
         Head        : Parser_State_Access;
         Parser_Free : Parser_State_Access;
         Stack_Free  : Stack_Node_Access;
         Count       : Integer;
      end record;

   end Parser_Lists;

end OpenToken.Production.Parser.LALR.Parser;
