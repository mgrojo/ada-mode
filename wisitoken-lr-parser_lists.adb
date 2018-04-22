--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014-2018  All Rights Reserved.
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

package body WisiToken.LR.Parser_Lists is

   function Parser_Stack_Image
     (Stack      : in Parser_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor'Class;
      Tree       : in Syntax_Trees.Tree;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
   is
      use all type Syntax_Trees.Node_Index;
      use all type SAL.Base_Peek_Type;
      use Ada.Strings.Unbounded;

      Last : constant SAL.Base_Peek_Type :=
        (if Depth = 0
         then Stack.Depth
         else SAL.Base_Peek_Type'Min (Depth, Stack.Depth));

      Result : Unbounded_String := +"(";
   begin
      for I in 1 .. Last loop
         Result := Result &
           (Image (Stack.Peek (I).State) & " :" &
              (if I = Stack.Depth
               then ""
               else
                 (if Stack.Peek (I).Token = Syntax_Trees.Invalid_Node_Index -- From recover fast-forward
                  then ""
                  else Tree.Image (Stack.Peek (I).Token, Descriptor) & ", ")));
      end loop;
      return To_String (Result & ")");
   end Parser_Stack_Image;

   function New_List
     (First_Parser_Label : in Natural;
      Shared_Tree        : in Syntax_Trees.Base_Tree_Access)
     return List
   is
      Parser : Parser_State := (Label => First_Parser_Label, others => <>);
   begin
      Parser.Tree.Initialize (Shared_Tree, Flush => True);

      return Result : List
      do
         Result.Parser_Label := First_Parser_Label;

         Result.Elements.Append (Parser);
      end return;
   end New_List;

   function Last_Label (List : in Parser_Lists.List) return Natural
   is begin
      return List.Parser_Label;
   end Last_Label;

   function Count (List : in Parser_Lists.List) return SAL.Base_Peek_Type
   is begin
      return List.Elements.Length;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      return (Elements => List.Elements'Access, Ptr => List.Elements.First);
   end First;

   procedure Next (Cursor : in out Parser_Lists.Cursor)
   is begin
      Parser_State_Lists.Next (Cursor.Ptr);
   end Next;

   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean
   is
      use Parser_State_Lists;
   begin
      return Cursor.Ptr = No_Element;
   end Is_Done;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return SAL.Base_Peek_Type
   is begin
      return Cursor.Elements.Length;
   end Active_Parser_Count;

   function Label (Cursor : in Parser_Lists.Cursor) return Natural
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Ptr).Label;
   end Label;

   function Max_Recover_Ops_Length (Cursor : in Parser_Lists.Cursor) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Result : Count_Type := 0;
      Errors : Parse_Error_Lists.List renames Parser_State_Lists.Constant_Reference (Cursor.Ptr).Errors;
   begin
      for Error of Errors loop
         if Error.Recover.Ops.Length > Result then
            Result := Error.Recover.Ops.Length;
         end if;
      end loop;
      return Result;
   end Max_Recover_Ops_Length;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs)
   is begin
      Parser_State_Lists.Reference (Cursor.Ptr).Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Ptr).Verb;
   end Verb;

   function State_Ref (Position : in Cursor) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Reference (Position.Ptr).Element);
   end State_Ref;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      use all type SAL.Base_Peek_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser_State_Lists.Constant_Reference (Cursor.Ptr);
   begin
      Trace.Put (Natural'Image (Parser_State.Label) & " stack: ");
      Trace.Put_Line (Image (Parser_State.Stack, Trace.Descriptor.all, Parser_State.Tree, Depth => 10));
   end Put_Top_10;

   procedure Prepend_Copy
     (List   : in out Parser_Lists.List;
      Cursor : in     Parser_Lists.Cursor'Class)
   is
      New_Item : Parser_State;
   begin
      List.Parser_Label := List.Parser_Label + 1;
      declare
         Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Ptr).Element.all;
         --  We can't do 'Prepend' in the scope of this 'renames';
         --  that would be tampering with cursors.
      begin
         Item.Tree.Set_Flush_False;

         --  We specify all items individually, rather copy Item and then
         --  override a few, to avoid copying large items like Recover.
         --  We copy Recover.Enqueue_Count, .Check_Count for unit tests.
         New_Item :=
           (Shared_Token          => Item.Shared_Token,
            Recover_Insert_Delete => Item.Recover_Insert_Delete,
            Current_Token         => Item.Current_Token,
            Inc_Shared_Token      => Item.Inc_Shared_Token,
            Stack                 => Item.Stack,
            Tree                  => Item.Tree,
            Recover               =>
              (Enqueue_Count      => Item.Recover.Enqueue_Count,
               Check_Count        => Item.Recover.Check_Count,
               others             => <>),
            Conflict_During_Resume => Item.Conflict_During_Resume,
            Zombie_Token_Count    => 0,
            Errors                => Item.Errors,
            Label                 => List.Parser_Label,
            Verb                  => Item.Verb);
      end;
      List.Elements.Prepend (New_Item);
   end Prepend_Copy;

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
   is
      Temp : Parser_State_Lists.Cursor := Cursor.Ptr;
   begin
      Parser_State_Lists.Next (Cursor.Ptr);
      Parser_State_Lists.Delete (Cursor.Elements.all, Temp);
   end Free;

   ----------
   --  stuff for iterators

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor
   is begin
      return (Ptr.Elements, Ptr.Ptr);
   end To_Cursor;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Constant_Reference (Position.Ptr).Element);
   end Constant_Reference;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Reference (Position.Ptr).Element);
   end Reference;

   function Persistent_State_Ref (Position : in Parser_Node_Access) return State_Access
   is begin
      return State_Access (Parser_State_Lists.Persistent_Ref (Position.Ptr));
   end Persistent_State_Ref;

   type List_Access is access all List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access;
   end record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return (Elements => Object.Container.Elements'Access, Ptr => Object.Container.Elements.First);
   end First;

   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access
   is
      pragma Unreferenced (Object);
   begin
      return (Position.Elements, Parser_State_Lists.Next (Position.Ptr));
   end Next;

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean
   is begin
      return Parser_State_Lists.Has_Element (Iterator.Ptr);
   end Has_Element;

   function Label (Iterator : in Parser_State) return Natural
   is begin
      return Iterator.Label;
   end Label;

   function Verb (Iterator : in Parser_State) return All_Parse_Action_Verbs
   is begin
      return Iterator.Verb;
   end Verb;

   procedure Set_Verb (Iterator : in out Parser_State; Verb : in All_Parse_Action_Verbs)
   is begin
      Iterator.Verb := Verb;
   end Set_Verb;

end WisiToken.LR.Parser_Lists;
