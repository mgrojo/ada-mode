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
     (Stack      : in Syntax_Trees.Stream_ID;
      Descriptor : in WisiToken.Descriptor;
      Tree       : in Syntax_Trees.Tree;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
   is
      use Ada.Strings.Unbounded;

      Stack_Depth : constant SAL.Base_Peek_Type := Tree.Stack_Depth (Stack);

      Last : constant SAL.Base_Peek_Type :=
        (if Depth = 0
         then Stack_Depth
         else SAL.Base_Peek_Type'Min (Depth, Stack_Depth));

      Result : Unbounded_String := +"(";
   begin
      for I in 1 .. Last loop
         declare
            Item : constant Syntax_Trees.Stream_Index := Tree.Peek (Stack, I);
            State : constant Unknown_State_Index := Tree.State (Stack, Item);
         begin
            Result := Result &
              (if State = Unknown_State then " - : " else Trimmed_Image (State) & " : ") &
              (if I = Stack_Depth
               then ""
               else Tree.Image (Tree.Get_Node (Stack, Item), Descriptor) & ", ");
         end;
      end loop;
      return To_String (Result & ")");
   end Parser_Stack_Image;

   function New_List (Tree : in out Syntax_Trees.Tree) return List
   is begin
      return Result : List
      do
         Result.Elements.Append ((Stream => Tree.New_Stream (Syntax_Trees.Invalid_Stream_ID, null), others => <>));
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

   function Stream (Cursor : in Parser_Lists.Cursor) return Syntax_Trees.Stream_ID
   is begin
      return Parser_State_Lists.Constant_Ref (Cursor.Ptr).Stream;
   end Stream;

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
      Tree    : in out Syntax_Trees.Tree;
      Message : in     String;
      Trace   : in out WisiToken.Trace'Class)
   is
      procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
      is
         Temp : Parser_State_Lists.Cursor := Cursor.Ptr;
      begin
         Parser_State_Lists.Next (Cursor.Ptr);
         Parser_State_Lists.Delete (Cursor.Elements.all, Temp);
      end Free;
   begin
      declare
         State : Parser_State renames Parser_State_Lists.Variable_Ref (Current.Ptr);
      begin
         if Trace_Parse > Outline then
            Trace.Put_Line
              (" " & Tree.Trimmed_Image (Current.Stream) & ": terminate (" &
                 Trimmed_Image (Integer (Parsers.Count) - 1) & " active)" &
                 ": " & Message & " " & Tree.Image (State.Current_Token, Trace.Descriptor.all));
         end if;

         Tree.Delete_Stream (State.Stream);
      end;
      Free (Current);
   end Terminate_Parser;

   procedure Duplicate_State
     (Parsers : in out List;
      Current : in out Cursor'Class;
      Tree    : in out Syntax_Trees.Tree;
      Trace   : in out WisiToken.Trace'Class)
   is
      use all type Ada.Containers.Count_Type;

      function Compare
        (Stack_1 : in Syntax_Trees.Stream_ID;
         Stack_2 : in Syntax_Trees.Stream_ID)
        return Boolean
      --  True if equal
      is
      begin
         if Tree.Stream_Length (Stack_1) /= Tree.Stream_Length (Stack_2) then
            return False;
         else
            for I in reverse 1 .. Tree.Stream_Length (Stack_1) - 1 loop
               --  Assume they differ near the top; no point in comparing bottom
               --  item. The syntax tree nodes will differ even if the tokens are the
               --  same, so only compare the tokens.
               if Tree.State (Stack_1) /= Tree.State (Stack_2) then
                  return False;
               else
                  declare
                     use Syntax_Trees;
                     Node_1 : constant Valid_Node_Access := Tree.Get_Node (Stack_1, Tree.Peek (Stack_1, I));
                     Node_2 : constant Valid_Node_Access := Tree.Get_Node (Stack_2, Tree.Peek (Stack_2, I));
                  begin
                     if not (Tree.Label (Node_1) = Tree.Label (Node_2) and then
                               Tree.ID (Node_1) = Tree.ID (Node_2) and then
                               Tree.Byte_Region (Node_1) = Tree.Byte_Region (Node_2))
                     then
                        return False;
                     end if;
                  end;
               end if;
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
            if Other.Stream /= Current.Stream and then
              Other.Verb /= Error and then
              Compare (Other_Parser.Stream, Current.Stream)
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
     (List      : in out Parser_Lists.List;
      Cursor    : in     Parser_Lists.Cursor'Class;
      Tree      : in out Syntax_Trees.Tree;
      User_Data : in     Syntax_Trees.User_Data_Access;
      Trace     : in out WisiToken.Trace'Class)
   is
      New_Item : Parser_State;
   begin
      declare
         use all type WisiToken.Syntax_Trees.Stream_Index;

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
            Current_Token                 =>
              (if Item.Shared_Token = Item.Current_Token
               then Item.Current_Token
               else Syntax_Trees.Invalid_Stream_Index), --  corrected below.
            Inc_Shared_Token              => Item.Inc_Shared_Token,
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
            Stream                        => Tree.New_Stream (Item.Stream, User_Data),
            Verb                          => Item.Verb);

         if Item.Shared_Token /= Item.Current_Token then
            New_Item.Current_Token := Tree.Stream_Next (New_Item.Stream, Tree.Peek (New_Item.Stream));

            declare
               use all type WisiToken.Syntax_Trees.Node_Access;
               use Recover_Op_Array_Refs;
               Item_Op : Recover_Op_Array_Refs.Constant_Reference_Type renames Recover_Op_Array_Refs.Constant_Ref
                 (Item.Recover_Insert_Delete,
                  (if Item.Recover_Insert_Delete_Current = Recover_Op_Arrays.No_Index
                   then Recover_Op_Arrays.Last_Index (Item.Recover_Insert_Delete)
                   else Item.Recover_Insert_Delete_Current - 1));
            begin
               pragma Assert (Item_Op.Op = Insert);

               if Tree.Get_Node (Item.Stream, Item.Current_Token) = Item_Op.Ins_Node then
                  declare
                     New_Item_Op : Variable_Reference_Type renames Variable_Ref
                       (New_Item.Recover_Insert_Delete,
                        (if New_Item.Recover_Insert_Delete_Current = Recover_Op_Arrays.No_Index
                         then Recover_Op_Arrays.Last_Index (New_Item.Recover_Insert_Delete)
                         else New_Item.Recover_Insert_Delete_Current - 1));
                  begin
                     New_Item_Op.Ins_Node := Tree.Get_Node (New_Item.Stream, New_Item.Current_Token);
                  end;
               end if;
            end;
         end if;
      end;

      if Trace_Parse > Extra then
         Trace.Put (Tree.Trimmed_Image (New_Item.Stream) & ": stack: ");
         Trace.Put_Line
           (Parser_Lists.Image (New_Item.Stream, Trace.Descriptor.all, Tree));
      end if;

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

   function Stream (State : in Parser_State) return Syntax_Trees.Stream_ID
   is begin
      return State.Stream;
   end Stream;

   function Verb (State : in Parser_State) return All_Parse_Action_Verbs
   is begin
      return State.Verb;
   end Verb;

   procedure Set_Verb (State : in out Parser_State; Verb : in All_Parse_Action_Verbs)
   is begin
      State.Verb := Verb;
   end Set_Verb;

end WisiToken.Parse.LR.Parser_Lists;
