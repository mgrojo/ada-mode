--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014 - 2020  All Rights Reserved.
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

package body WisiToken.Parse.LR.Parser_Lists is

   function Parser_Stack_Image
     (Stack      : in Parser_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor;
      Tree       : in Syntax_Trees.Tree;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
   is
      use Ada.Strings.Unbounded;

      Last : constant SAL.Base_Peek_Type :=
        (if Depth = 0
         then Stack.Depth
         else SAL.Base_Peek_Type'Min (Depth, Stack.Depth));

      Result : Unbounded_String := +"(";
   begin
      for I in 1 .. Last loop
         declare
            use all type WisiToken.Syntax_Trees.Stream_Index;
            Item : Parser_Stack_Item renames Stack.Peek (I);
         begin
            Result := Result &
              ((if Item.State = Unknown_State then " " else Trimmed_Image (Item.State)) & " :" &
                 (if I = Stack.Depth
                  then ""
                  else
                    (if Item.Token = Syntax_Trees.Invalid_Stream_Index -- From recover fast-forward
                     then ""
                     else Tree.Image (Item.Token, Descriptor) & ", ")));
         end;
      end loop;
      return To_String (Result & ")");
   end Parser_Stack_Image;

   function New_List (Tree : in out Syntax_Trees.Tree) return List
   is begin
      return Result : List
      do
         Result.Elements.Append ((Stream_ID => Tree.New_Stream, others => <>));
      end return;
   end New_List;

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

   function Stream_ID (Cursor : in Parser_Lists.Cursor) return Syntax_Trees.Stream_ID
   is begin
      return Parser_State_Lists.Constant_Ref (Cursor.Ptr).Stream_ID;
   end Stream_ID;

   function Total_Recover_Cost (Cursor : in Parser_Lists.Cursor) return Integer
   is
      Result : Integer := 0;
   begin
      for Error of Parser_State_Lists.Constant_Ref (Cursor.Ptr).Errors loop
         Result := Error.Recover.Cost;
      end loop;
      return Result;
   end Total_Recover_Cost;

   function Max_Recover_Ops_Length (Cursor : in Parser_Lists.Cursor) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      use Config_Op_Arrays;
      Result : Count_Type := 0;
      Errors : Parse_Error_Lists.List renames Parser_State_Lists.Constant_Ref (Cursor.Ptr).Errors;
   begin
      for Error of Errors loop
         if Length (Error.Recover.Ops) > Result then
            Result := Length (Error.Recover.Ops);
         end if;
      end loop;
      return Result;
   end Max_Recover_Ops_Length;

   function Min_Recover_Cost (Cursor : in Parser_Lists.Cursor) return Integer
   is
      Result : Integer := Integer'Last;
      Errors : Parse_Error_Lists.List renames Parser_State_Lists.Constant_Ref (Cursor.Ptr).Errors;
   begin
      for Error of Errors loop
         if Error.Recover.Cost < Result then
            Result := Error.Recover.Cost;
         end if;
      end loop;
      return Result;
   end Min_Recover_Cost;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs)
   is begin
      Parser_State_Lists.Variable_Ref (Cursor.Ptr).Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Ref (Cursor.Ptr).Verb;
   end Verb;

   procedure Terminate_Parser
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in     Syntax_Trees.Tree;
      Message : in     String;
      Trace   : in out WisiToken.Trace'Class)
   is
      State : Parser_State renames Parser_State_Lists.Constant_Ref (Current.Ptr);

      procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
      is
         Temp : Parser_State_Lists.Cursor := Cursor.Ptr;
      begin
         Parser_State_Lists.Next (Cursor.Ptr);
         Parser_State_Lists.Delete (Cursor.Elements.all, Temp);
      end Free;
   begin
      if Trace_Parse > Outline then
         Trace.Put_Line
           (Syntax_Trees.Trimmed_Image (Current.Stream_ID) & ": terminate (" &
              Trimmed_Image (Integer (Parsers.Count) - 1) & " active)" &
              ": " & Message & Tree.Image
                (Tree.First_Shared_Terminal (Tree.Get_Node (State.Current_Token)),
                 Trace.Descriptor.all));
      end if;

      --  We don't delete the Tree nodes created by this parser, because it
      --  is too hard to keep track of the nodes that are also used by other
      --  parsers. After parsing is finished, we can prune the tree by
      --  copying the surviving tree from it's root, and finalizing this
      --  tree.
      Free (Current);
   end Terminate_Parser;

   procedure Duplicate_State
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in     Syntax_Trees.Tree;
      Trace   : in out WisiToken.Trace'Class)
   is
      use all type Ada.Containers.Count_Type;

      function Compare
        (Stack_1 : in Parser_Stacks.Stack;
         Stack_2 : in Parser_Stacks.Stack)
        return Boolean
      is
      begin
         if Stack_1.Depth /= Stack_2.Depth then
            return False;
         else
            for I in reverse 1 .. Stack_1.Depth - 1 loop
               --  Assume they differ near the top; no point in comparing bottom
               --  item. The syntax trees will differ even if the tokens on the stack
               --  are the same, so compare the tokens.
               declare
                  use all type WisiToken.Syntax_Trees.Node_Label;
                  Item_1 : Parser_Stack_Item renames Stack_1 (I);
                  Item_2 : Parser_Stack_Item renames Stack_2 (I);

                  function Same_Token
                    (Index_1 : in Syntax_Trees.Stream_Index;
                     Index_2 : in Syntax_Trees.Stream_Index)
                    return Boolean
                  is
                     use Syntax_Trees;
                     Node_1 : constant Valid_Node_Access := Tree.Get_Node (Index_1);
                     Node_2 : constant Valid_Node_Access := Tree.Get_Node (Index_2);
                  begin
                     return (Tree.Label (Node_1) = Tree.Label (Node_2) and then
                               Tree.ID (Node_1) = Tree.ID (Node_2) and then
                               Tree.Byte_Region (Node_1) = Tree.Byte_Region (Node_2));
                  end Same_Token;
               begin
                  if Item_1.State /= Item_2.State then
                     return False;
                  else
                     if not Same_Token (Item_1.Token, Item_2.Token) then
                        return False;
                     end if;
                  end if;
               end;
            end loop;
            return True;
         end if;
      end Compare;

      Other : Cursor := Parsers.First;
   begin
      loop
         exit when Other.Is_Done;
         declare
            use all type WisiToken.Syntax_Trees.Stream_ID;
            Other_Parser : Parser_State renames Other.State_Ref;
         begin
            if Other.Stream_ID /= Current.Stream_ID and then
              Other.Verb /= Error and then
              Compare (Other_Parser.Stack, Current.State_Ref.Stack)
            then
               exit;
            end if;
         end;
         Other.Next;
      end loop;

      if not Other.Is_Done then
         --  Both have the same number of errors, otherwise one would have been
         --  terminated earlier.
         if Other.Total_Recover_Cost = Current.Total_Recover_Cost then
            if Other.Max_Recover_Ops_Length = Current.Max_Recover_Ops_Length then
               Parsers.Terminate_Parser (Other, Tree, "duplicate state: random", Trace);
            else
               --  Keep the minimum ops length
               if Other.Max_Recover_Ops_Length > Current.Max_Recover_Ops_Length then
                  null;
               else
                  Other := Cursor (Current);
                  Current.Next;
               end if;
               Parsers.Terminate_Parser (Other, Tree, "duplicate state: ops length", Trace);
            end if;
         else
            if Other.Total_Recover_Cost > Current.Total_Recover_Cost then
               null;
            else
               Other := Cursor (Current);
               Current.Next;
            end if;
            Parsers.Terminate_Parser (Other, Tree, "duplicate state: cost", Trace);
         end if;
      end if;
   end Duplicate_State;

   function State_Ref (Position : in Cursor) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Ref (Position.Ptr).Element);
   end State_Ref;

   function First_State_Ref (List : in Parser_Lists.List'Class) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Ref (List.Elements.First).Element);
   end First_State_Ref;

   function First_Constant_State_Ref (List : in Parser_Lists.List'Class) return Constant_State_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Ref (List.Elements.First).Element);
   end First_Constant_State_Ref;

   procedure Prepend_Copy
     (List   : in out Parser_Lists.List;
      Cursor : in     Parser_Lists.Cursor'Class;
      Tree   : in out Syntax_Trees.Tree)
   is
      New_Item : Parser_State;
   begin
      declare
         Item : Parser_State renames Parser_State_Lists.Variable_Ref (Cursor.Ptr);
         --  We can't do 'Prepend' in the scope of this 'renames';
         --  that would be tampering with cursors.
      begin
         --  We specify all items individually, rather copy Item and then
         --  override a few, to avoid copying large items like Recover.
         --  We copy Recover.Enqueue_Count .. Check_Count for unit tests.
         New_Item :=
           (Shared_Token                  => Item.Shared_Token,
            Recover_Insert_Delete         => Item.Recover_Insert_Delete,
            Recover_Insert_Delete_Current => Item.Recover_Insert_Delete_Current,
            Current_Token                 => Item.Current_Token,
            Inc_Shared_Token              => Item.Inc_Shared_Token,
            Stack                         => Item.Stack,
            Recover                       =>
              (Enqueue_Count              => Item.Recover.Enqueue_Count,
               Config_Full_Count          => Item.Recover.Config_Full_Count,
               Check_Count                => Item.Recover.Check_Count,
               others                     => <>),
            Resume_Active                 => Item.Resume_Active,
            Resume_Token_Goal             => Item.Resume_Token_Goal,
            Conflict_During_Resume        => Item.Conflict_During_Resume,
            Zombie_Token_Count            => 0,
            Errors                        => Item.Errors,
            Stream_ID                     => Tree.New_Stream,
            Verb                          => Item.Verb);
      end;
      List.Elements.Prepend (New_Item);
   end Prepend_Copy;

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
      return (Element => Parser_State_Lists.Constant_Ref (Position.Ptr).Element);
   end Constant_Reference;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Variable_Ref (Position.Ptr).Element);
   end Reference;

   function Persistent_State_Ref (Position : in Parser_Node_Access) return State_Access
   is begin
      return State_Access (Parser_State_Lists.Persistent_Ref (Position.Ptr));
   end Persistent_State_Ref;

   type Iterator (Elements : access Parser_State_Lists.List) is new Iterator_Interfaces.Forward_Iterator
     with null record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return (Elements => Object.Elements, Ptr => Object.Elements.First);
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
      return Iterator'(Elements => Container.Elements'Access);
   end Iterate;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean
   is begin
      return Parser_State_Lists.Has_Element (Iterator.Ptr);
   end Has_Element;

   function Stream_ID (State : in Parser_State) return Syntax_Trees.Stream_ID
   is begin
      return State.Stream_ID;
   end Stream_ID;

   function Verb (State : in Parser_State) return All_Parse_Action_Verbs
   is begin
      return State.Verb;
   end Verb;

   procedure Set_Verb (State : in out Parser_State; Verb : in All_Parse_Action_Verbs)
   is begin
      State.Verb := Verb;
   end Set_Verb;

end WisiToken.Parse.LR.Parser_Lists;
