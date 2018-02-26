--  Abstract :
--
--  Utilities used by a generalized LR parser.
--
--  Copyright (C) 2014-2015, 2017, 2018 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Iterator_Interfaces;
with SAL.Gen_Indefinite_Doubly_Linked_Lists;
with WisiToken.Syntax_Trees;
package WisiToken.LR.Parser_Lists is

   type Base_Parser_State (Terminals : not null access Base_Token_Arrays.Vector) is tagged
   record
      --  Visible components for direct access

      Shared_Token : Base_Token_Index := Base_Token_Arrays.No_Index;
      --  Last token read from Shared_Parser.Terminals.

      Local_Lookahead : Syntax_Trees.Valid_Node_Index_Queues.Queue_Type;
      --  Tokens in Tree that were inserted during error recovery.

      Current_Token : Syntax_Trees.Node_Index := Syntax_Trees.No_Node_Index;
      --  Current terminal, in Tree

      Current_Token_Is_Virtual : Boolean := False; -- FIXME: Tree.Virtual (Current_Token)?
      Last_Shift_Was_Virtual   : Boolean := False;
      Stack                    : Parser_Stacks.Stack;
      Tree                     : aliased Syntax_Trees.Tree (Terminals);
      Recover                  : aliased LR.McKenzie_Data := (others => <>);
      Zombie_Token_Count       : Integer := 0;
      --  If Zombie_Token_Count > 0, this parser has errored, but is waiting
      --  to see if other parsers do also. When it reaches 0, the parser is
      --  terminated.

      Errors : Parse_Error_Lists.List;
   end record;

   type Parser_State is new Base_Parser_State with private;

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Parser_State;

   function New_List
     (First_Parser_Label : in Natural;
      Terminals          : access Base_Token_Arrays.Vector)
     return List;

   function Count (List : in Parser_Lists.List) return SAL.Base_Peek_Type;

   type Cursor is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Has_Element (Cursor : in Parser_Lists.Cursor) return Boolean is (not Is_Done (Cursor));

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return SAL.Base_Peek_Type;

   function Label (Cursor : in Parser_Lists.Cursor) return Natural;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs;

   procedure Save_Verb (Cursor : in Parser_Lists.Cursor);
   --  Saves current verb to Prev_Verb
   function Prev_Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;

   type State_Reference (Element : not null access Parser_State) is null record
   with Implicit_Dereference => Element;

   function State_Ref (Position : in Cursor) return State_Reference;
   --  Direct access to visible components of Parser_State

   function State_Ref_2
     (Container : not null access List'Class;
      Label     : in              Natural)
     return State_Reference;
   --  WORKAROUND: GNAT GPL 2017 does not like overloading this as "State_Ref".

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);
   --  Put image of top 10 stack items to Trace.

   procedure Pre_Reduce_Stack_Save (Cursor : in Parser_Lists.Cursor);
   --  Save a copy of top Cursor.Stack item.
   --
   --  McKenzie error recover algorithm needs the parse state before
   --  last reduce action.

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
   --  Copy parser at Cursor, prepend to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).
   --
   --  Copy.Recover is set to Default_McKenzie.

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class);
   --  Delete the Cursor parser. It will not appear in future
   --  iterations. On return, Cursor points to next parser (or none).

   ----------
   --  Stuff for iterators, to allow
   --  'for Parser of Parsers loop'
   --  'for I in Parsers.Iterate loop'
   --
   --  requires Parser_State to be not an incomplete type.

   --  We'd like to use Cursor here, but we want that to be tagged, to
   --  allow 'Cursor.operation' syntax, and the requirements of
   --  iterators prevent a tagged iterator type (two tagged types on
   --  First in this package body). So we use Parser_Node_Access as
   --  the iterator type for Iterators, and typical usage is:
   --
   --  for I in Parsers.Iterate loop
   --     declare
   --        Cursor : Parser_Lists.Cursor renames To_Cursor (Parsers, I);
   --     begin
   --        Cursor.<cursor operation>
   --
   --        ... Parsers (I).<visible parser_state component> ...
   --     end;
   --  end loop;
   --
   --  or:
   --  for Current_Parser of Parsers loop
   --     ... Current_Parser.<visible parser_state component> ...
   --  end loop;

   type Parser_Node_Access is private;

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference;

   function McKenzie_Ref (Position : in Parser_Node_Access) return McKenzie_Access;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class;

   --  Access to some private Parser_State components

   function Label (Iterator : in Parser_State) return Natural;
   procedure Set_Verb (Iterator : in out Parser_State; Verb : in All_Parse_Action_Verbs);
   function Verb (Iterator : in Parser_State) return All_Parse_Action_Verbs;
   function Pre_Reduce_Stack_Item (Iterator : in Parser_State) return Parser_Stack_Item;

private

   type Parser_State is new Base_Parser_State with record
      Label : Natural; -- for debugging/verbosity

      Verb : All_Parse_Action_Verbs := Shift; -- current action to perform

      Prev_Verb       : All_Parse_Action_Verbs := Parse_Action_Verbs'First; -- previous action performed
      Pre_Reduce_Item : Parser_Stack_Item      := (others => <>);
   end record;

   package Parser_State_Lists is new SAL.Gen_Indefinite_Doubly_Linked_Lists (Parser_State);

   type List is tagged record
      Elements     : aliased Parser_State_Lists.List;
      Parser_Label : Natural; -- label of last added parser.
   end record;

   type Cursor is tagged record
      Elements : access Parser_State_Lists.List;
      Ptr      : Parser_State_Lists.Cursor;
   end record;

   type Parser_Node_Access is record
      Elements : access Parser_State_Lists.List;
      Ptr      : Parser_State_Lists.Cursor;
   end record;

end WisiToken.LR.Parser_Lists;
