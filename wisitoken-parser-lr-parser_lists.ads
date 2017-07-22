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

with Ada.Iterator_Interfaces;
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

   function Count (List : in Parser_Lists.List) return Integer;

   type Cursor is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Integer;

   function Label (Cursor : in Parser_Lists.Cursor) return Integer;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs;

   procedure Set_Recover (Cursor : in Parser_Lists.Cursor; Data : in Recover_Data_Access);

   type Recover_Reference (Element : not null access Recover_Data'Class) is null record
   with Implicit_Dereference => Element;

   function Recover_Ref (Position : in Cursor) return Recover_Reference;

   --  Parser stack
   type Stack_Item is record
      State : Unknown_State_Index;
      Token : Token_ID;
   end record;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Peek (Cursor : in Parser_Lists.Cursor; Depth : in Integer := 1) return Stack_Item;
   function Copy_Stack (Cursor : in Parser_Lists.Cursor) return State_Stacks.Stack_Type;
   function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item;
   procedure Pop (Cursor : in Parser_Lists.Cursor);
   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item);

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean;

   procedure Put_Trace_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);
   --  Put image of top 10 stack items to Put_Trace.

   --  pending user actions
   type Action_Token is record
      Action : Parse_Action_Rec;
      Tokens : Token.List.Instance;
   end record;

   Null_Action_Token : constant Action_Token := (Null_Reduce_Action_Rec, Token.List.Null_List);

   function Pending_Actions_Empty (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Pending_Actions_Count (Cursor : in Parser_Lists.Cursor) return Integer;
   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token);
   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class);
   --  Copy parser at Cursor, add to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class);
   --  Move Cursor to the internal free list, free its stack and
   --  pending actions; it will not appear in future iterations. On
   --  return, Cursor points to next parser (or none).

   ----------
   --  Stuff for iterators, to allow
   --  'for Parser of Parsers loop'
   --  'for Cursor in Parsers.Iterate loop'
   --
   --  requires Parser_State to be not an incomplete type.

   --  We'd like to use Cursor here, but we want that to be tagged,
   --  to allow 'Cursor.Next' syntax, and the requirements of
   --  iterators prevent a tagged cursor type (two tagged types on
   --  First in body). So we use Parser_Node_Access as the iterator
   --  type for Iterators.

   type Parser_Node_Access is private;

   function To_Cursor
     (List : aliased in out Parser_Lists.List'Class;
      Ptr  :         in     Parser_Node_Access)
     return Cursor;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;

   function Has_Element (Cursor : in Parser_Node_Access) return Boolean;
   function Verb (Cursor : in Parser_Node_Access) return Parse_Action_Verbs;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class;

   ----------
   --  For unit tests, debug assertions

   function Parser_Free_Count (List : in Parser_Lists.List) return Integer;
   function Stack_Free_Count (List : in Parser_Lists.List) return Integer;
   function Action_Token_Free_Count (List : in Parser_Lists.List) return Integer;

   procedure Put_Trace (Trace : in out WisiToken.Trace'Class; Action_Token : in Parser_Lists.Action_Token);
   procedure Put_Trace_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor);

private

   --  FIXME: change stack to bounded vector
   type Stack_Node;
   type Stack_Node_Access is access Stack_Node;
   type Stack_Node is record
      Item : Stack_Item;
      Next : Stack_Node_Access;
   end record;

   type Action_Token_Node;
   type Action_Token_Node_Access is access Action_Token_Node;
   type Action_Token_Node is record
      Item : Action_Token;
      Next : Action_Token_Node_Access;
      Prev : Action_Token_Node_Access;
   end record;

   type Action_Token_List is record
      Head : Action_Token_Node_Access;
      Tail : Action_Token_Node_Access;
      --  Enqueue to tail, dequeue from head, so 'prev', 'next' make sense
   end record;

   function Count (Action_Token : in Action_Token_List) return Integer;

   type Parser_State is record
      Label           : Integer;            -- for debugging
      Verb            : Parse_Action_Verbs; -- last action performed
      Stack           : Stack_Node_Access;
      Pending_Actions : Action_Token_List;  --  FIXME: include panic/recovery ;
      Recover         : Recover_Data_Access;
   end record;

   type Parser_Node;
   type Parser_Node_Access is access Parser_Node;

   type Parser_Node is record
      Item : aliased Parser_State;
      Next : Parser_Node_Access;
      Prev : Parser_Node_Access;
   end record;

   type List is tagged record
      Parser_Label      : Integer;
      Head              : Parser_Node_Access;
      Parser_Free       : Parser_Node_Access;
      Stack_Free        : Stack_Node_Access;
      Action_Token_Free : Action_Token_Node_Access;
      Count             : Integer;
   end record;

   type Cursor is tagged record
      List : access Parser_Lists.List;
      Ptr  : Parser_Node_Access;
   end record;

end WisiToken.Parser.LR.Parser_Lists;
