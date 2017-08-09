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

   type Parser_State is private;

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
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

   procedure Set_Recover (Cursor : in Parser_Lists.Cursor; Data : in Recover_Data_Access);

   type Recover_Reference (Element : not null access Recover_Data'Class) is null record
   with Implicit_Dereference => Element;

   function Recover_Ref (Position : in Cursor) return Recover_Reference;

   --  Parser stack
   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Peek (Cursor : in Parser_Lists.Cursor; Depth : in SAL.Base_Peek_Type := 1) return Parse_Stack_Item;
   function Copy_Stack (Cursor : in Parser_Lists.Cursor) return Parse_Stacks.Stack_Type;
   function Pop (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item;
   procedure Pop (Cursor : in Parser_Lists.Cursor);
   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Parse_Stack_Item);

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);
   --  Put image of top 10 stack items to Trace.

   procedure Pre_Reduce_Stack_Save (Cursor : in Parser_Lists.Cursor);
   --  Save a copy of top Cursor.Stack item.
   --
   --  McKenzie error recover algorithm needs the parse state before
   --  last reduce action.

   function Pre_Reduce_Stack_Item (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item;

   --  pending user actions
   type Action_Token is record
      Action : Parse_Action_Rec;
      Tokens : Token.List.Instance;
   end record;

   Null_Action_Token : constant Action_Token := (Null_Reduce_Action_Rec, Token.List.Null_List);

   function Pending_Actions_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Pending_Actions_Count (Cursor : in Parser_Lists.Cursor) return SAL.Base_Peek_Type;
   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token);
   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
   --  Copy parser at Cursor, prepend to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class);
   --  Delete the Cursor parser. It will not appear in future
   --  iterations. On return, Cursor points to next parser (or none).

   procedure Free (List : in out Parser_Lists.List);
   --  Free all parsers in List.

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
   --
   --  We only provide access to variable iterators; not worth
   --  duplicating the code for constant.

   type Parser_Node_Access is private;

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class;

   function Verb (Iterator : in Parser_Node_Access) return Parse_Action_Verbs;

   ----------
   --  For unit tests, debug assertions

   procedure Put (Trace : in out WisiToken.Trace'Class; Action_Token : in Parser_Lists.Action_Token);
   procedure Put_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);

private

   package Pend_Items_Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Action_Token);
   package Pend_Items_Queues is new SAL.Gen_Unbounded_Definite_Queues (Action_Token, Pend_Items_Queue_Interfaces);

   type Parser_State is record
      Label           : Integer;            -- for debugging/verbosity
      Verb            : Parse_Action_Verbs; -- last action performed
      Prev_Verb       : Parse_Action_Verbs; -- previous action performed
      Stack           : Parse_Stacks.Stack_Type;
      Pre_Reduce_Item : Parse_Stack_Item := Default_Parse_Stack_Item;
      Pending_Actions : Pend_Items_Queues.Queue_Type;  --  FIXME: include panic/recovery
      Recover         : Recover_Data_Access;
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
