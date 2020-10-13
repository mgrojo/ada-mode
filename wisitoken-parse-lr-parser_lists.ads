--  Abstract :
--
--  Generalized LR parser state.
--
--  Copyright (C) 2014-2015, 2017 - 2020 Free Software Foundation, Inc.
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
package WisiToken.Parse.LR.Parser_Lists is

   function Parser_Stack_Image
     (Stack : in Syntax_Trees.Stream_ID;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
     return String;
   --  If Depth = 0, put all of Stack. Otherwise put Min (Depth,
   --  Stack.Depth) items.
   --
   --  Unique name for calling from debugger

   function Image
     (Stack : in Syntax_Trees.Stream_ID;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
     return String renames Parser_Stack_Image;

   type Base_Parser_State is tagged
   record
      --  Visible components for direct access

      Shared_Token : Syntax_Trees.Stream_Index := Syntax_Trees.Invalid_Stream_Index;
      --  Last token read from input text, in Terminal_Stream.

      Recover_Insert_Delete : aliased Recover_Op_Arrays.Vector;
      --  Tokens that were inserted or deleted during error recovery.
      --  Contains only Insert and Delete ops. Filled by error recover, used
      --  by main parse and Execute_Actions.
      --
      --  Not emptied between error recovery sessions, so Execute_Actions
      --  knows about all insert/delete.

      Recover_Insert_Delete_Current : Recover_Op_Arrays.Extended_Index := Recover_Op_Arrays.No_Index;
      --  Next item in Recover_Insert_Delete to be processed by main parse;
      --  No_Index if all done.

      Current_Token : Syntax_Trees.Stream_Index := Syntax_Trees.Invalid_Stream_Index;
      --  Shared_Token in Tree.Terminal_Stream when there are no error
      --  recover tokens to insert; inserted token in parse stream when
      --  there are.

      Inc_Shared_Token : Boolean := True;

      --  The parse stack is Shared_Parser.Tree (Stream).

      Recover : aliased LR.McKenzie_Data := (others => <>);

      Zombie_Token_Count : Syntax_Trees.Element_Index := 0;
      --  If Zombie_Token_Count > 0, this parser has errored, but is waiting
      --  to see if other parsers do also.

      Resume_Active          : Boolean                    := False;
      Resume_Token_Goal      : Syntax_Trees.Element_Index := Syntax_Trees.Invalid_Element_Index;
      Conflict_During_Resume : Boolean                    := False;

      Errors : Parse_Error_Lists.List;
   end record;

   type Parser_State is new Base_Parser_State with private;
   type State_Access is access all Parser_State;

   type List is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Parser_State;

   function New_List (Tree : in out Syntax_Trees.Tree) return List;
   --  If Tree.Fully_Parsed, reuse parse stream. Otherwise, create a new one.

   function Count (List : in Parser_Lists.List) return SAL.Base_Peek_Type;

   type Cursor (<>) is tagged private;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor;
   procedure Next (Cursor : in out Parser_Lists.Cursor);
   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean;
   function Has_Element (Cursor : in Parser_Lists.Cursor) return Boolean is (not Is_Done (Cursor));
   function Stream (Cursor : in Parser_Lists.Cursor) return Syntax_Trees.Stream_ID;
   function Total_Recover_Cost (Cursor : in Parser_Lists.Cursor) return Integer;
   function Max_Recover_Ops_Length (Cursor : in Parser_Lists.Cursor) return Ada.Containers.Count_Type;
   function Min_Recover_Cost (Cursor : in Parser_Lists.Cursor) return Integer;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs);
   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs;

   procedure Terminate_Parser
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in out Syntax_Trees.Tree;
      Message : in     String;
      Trace   : in out WisiToken.Trace'Class);
   --  Terminate Current. Current is set to next element.
   --
   --  Tree is used to report the current token in the message.

   procedure Duplicate_State
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in out Syntax_Trees.Tree;
      Trace   : in out WisiToken.Trace'Class);
   --  If any other parser in Parsers has a stack equivalent to Current,
   --  Terminate one of them. Current is either unchanged, or advanced to
   --  the next parser.
   --
   --  Terminals is used to report the current token in the message.

   type State_Reference (Element : not null access Parser_State) is null record
   with Implicit_Dereference => Element;

   function State_Ref (Position : in Cursor) return State_Reference
   with Pre => Has_Element (Position);
   --  Direct access to visible components of Parser_State

   function First_State_Ref (List : in Parser_Lists.List'Class) return State_Reference
   with Pre => List.Count > 0;
   --  Direct access to visible components of first parser's Parser_State

   type Constant_State_Reference (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function First_Constant_State_Ref (List : in Parser_Lists.List'Class) return Constant_State_Reference
   with Pre => List.Count > 0;
   --  Direct access to visible components of first parser's Parser_State

   procedure Prepend_Copy
     (List      : in out Parser_Lists.List;
      Cursor    : in     Parser_Lists.Cursor'Class;
      Tree      : in out Syntax_Trees.Tree;
      User_Data : in     Syntax_Trees.User_Data_Access;
      Trace     : in out WisiToken.Trace'Class);
   --  Copy parser at Cursor, prepend to current list. New copy will not
   --  appear in Cursor.Next ...; it is accessible as First (List).
   --
   --  Copy.Recover is set to default.

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

   type Parser_Node_Access (<>) is private;

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor;

   type Constant_Reference_Type (Element : not null access constant Parser_State) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference;
   pragma Inline (Reference);

   function Persistent_State_Ref (Position : in Parser_Node_Access) return State_Access;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Parser_Node_Access, Has_Element);

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class;

   --  Access to private Parser_State components

   function Stream (State : in Parser_State) return Syntax_Trees.Stream_ID;
   procedure Set_Verb (State : in out Parser_State; Verb : in All_Parse_Action_Verbs);
   function Verb (State : in Parser_State) return All_Parse_Action_Verbs;

private

   type Parser_State is new Base_Parser_State with record
      Stream : Syntax_Trees.Stream_ID;

      Verb : All_Parse_Action_Verbs := Shift; -- current action to perform
   end record;

   package Parser_State_Lists is new SAL.Gen_Indefinite_Doubly_Linked_Lists (Parser_State);

   type List is tagged record
      Elements : aliased Parser_State_Lists.List;
   end record;

   type Cursor (Elements : access Parser_State_Lists.List) is tagged
   record
      Ptr : Parser_State_Lists.Cursor;
   end record;

   type Parser_Node_Access (Elements : access Parser_State_Lists.List) is
   record
      Ptr : Parser_State_Lists.Cursor;
   end record;

end WisiToken.Parse.LR.Parser_Lists;
