--  Abstract :
--
--  Utilities used by a generalized LR parser.
--
--  Copyright (C) 2014-2015, 2017 Stephe Leake
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with SAL.Gen_Queue_Interfaces;
with SAL.Gen_Unbounded_Definite_Queues;
package WisiToken.Parser.LR.Parser_Lists is

   --  pending semantic actions
   type Action_Token is record
      Action : Parse_Action_Rec;
      Tokens : Token.List.Instance;
   end record;

   Null_Action_Token : constant Action_Token := (Null_Reduce_Action_Rec, Token.List.Null_List);

   package Pend_Items_Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Action_Token);
   package Pend_Items_Queues is new SAL.Gen_Unbounded_Definite_Queues (Action_Token, Pend_Items_Queue_Interfaces);

   type Base_Parser_State is tagged record
      --  Visible components for direct access
      Stack           : Parser_Stacks.Stack_Type;
      Pending_Actions : Pend_Items_Queues.Queue_Type;  --  FIXME: include panic/recovery
      Recover         : Recover_Data_Access;
   end record;

   type Parser_State is new Base_Parser_State with private;

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Parser_State;

   function New_List
     (First_State_Index  : in State_Index;
      First_Parser_Label : in Integer)
     return List;

   function Count (List : in Parser_Lists.List) return Ada.Containers.Count_Type;

   type Cursor is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Ada.Containers.Count_Type;

   function Label (Cursor : in Parser_Lists.Cursor) return Integer;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;
   function Prev_Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;

   type State_Reference (Element : not null access Parser_State) is null record
   with Implicit_Dereference => Element;

   function State_Ref (Position : in Cursor) return State_Reference;
   --  Direct access to visible components of Parser_State

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

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;

   type Reference_Type (Element : not null access Parser_State) is null record
   with Implicit_Dereference => Element;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return Reference_Type;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class;

   --  Read only access to some private Parser_State components

   function Label (Iterator : in Parser_State) return Integer;
   function Verb (Iterator : in Parser_State) return Parse_Action_Verbs;
   function Prev_Verb (Iterator : in Parser_State) return Parse_Action_Verbs;
   function Pre_Reduce_Stack_Item (Iterator : in Parser_State) return Parser_Stack_Item;
   procedure Put_Top_10 (Iterator : in Parser_State; Trace : in out WisiToken.Trace'Class);

   ----------
   --  For unit tests, debug assertions

   procedure Put (Trace : in out WisiToken.Trace'Class; Action_Token : in Parser_Lists.Action_Token);
   procedure Put_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);

private

   type Parser_State is new Base_Parser_State with record
      Label           : Integer;            -- for debugging/verbosity
      Verb            : Parse_Action_Verbs; -- last action performed
      Prev_Verb       : Parse_Action_Verbs; -- previous action performed
      Pre_Reduce_Item : Parser_Stack_Item := Default_Parser_Stack_Item;
   end record;

   package Parser_State_Lists is new Ada.Containers.Doubly_Linked_Lists (Parser_State);

   type List is tagged record
      Elements : aliased Parser_State_Lists.List;

      New_Elements : aliased Parser_State_Lists.List;
      --  filled by Prepend_Copy, emptied by First.

      Parser_Label : Integer; -- label of last added parser.
   end record;

   type Cursor is tagged record
      Elements : access Parser_State_Lists.List;
      Ptr      : Parser_State_Lists.Cursor;
   end record;

   type Parser_Node_Access is record
      Elements : access Parser_State_Lists.List;
      Ptr      : Parser_State_Lists.Cursor;
   end record;

end WisiToken.Parser.LR.Parser_Lists;
