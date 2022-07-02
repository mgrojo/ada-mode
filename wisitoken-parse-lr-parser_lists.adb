--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014 - 2022  All Rights Reserved.
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

with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Image_Aux;
package body WisiToken.Parse.LR.Parser_Lists is

   --  Body subprograms, alphabetical

   function Get_List_Cursor
     (List  : in Syntax_Trees.Error_Data_Lists.List;
      Index : in SAL.Base_Peek_Type)
     return Syntax_Trees.Error_Data_Lists.Cursor
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return Result : Syntax_Trees.Error_Data_Lists.Cursor := List.First do
         for I in 1 .. Index loop
            Next (Result);
         end loop;
      end return;
   end Get_List_Cursor;

   ----------
   --  Spec public subprogams, declaration order.

   function Parser_Stack_Image
     (Stack : in Syntax_Trees.Stream_ID;
      Tree  : in Syntax_Trees.Tree;
      Depth : in SAL.Base_Peek_Type := 0)
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
              (if State = Unknown_State or Trace_Parse_No_State_Numbers
               then " - : " else Trimmed_Image (State) & " : ") &
              (if I = Stack_Depth
               then ""
               else Tree.Image (Tree.Get_Node (Stack, Item), Terminal_Node_Numbers => True) & ", ");
         end;
      end loop;
      return To_String (Result & ")");
   end Parser_Stack_Image;

   function Constant_Ref (Ref : in Recover_Op_Ref) return Recover_Op_Nodes_Arrays.Constant_Reference_Type
   is
      Ops : Recover_Op_Nodes_Arrays.Vector renames Recover_Op_Array_Const_Ref_From_Cursor (Ref.Err_Cur);
   begin
      return Ops.Constant_Ref (Ref.Op_Index);
   end Constant_Ref;

   procedure Next_Op (Ref : in out Recover_Op_Ref)
   is begin
      if Ref.Op_Index < Recover_Op_Array_Const_Ref_From_Cursor (Item => Ref.Err_Cur).Last_Index then
         Ref.Op_Index := @ + 1;
      else
         Ref := No_Element;
      end if;
   end Next_Op;

   function Recover_Image (Item : in Syntax_Trees.Stream_Node_Ref; Tree : in Syntax_Trees.Tree) return String
   is begin
      return Recover_Image (Tree.Error_List (Item.Node), Tree);
   end Recover_Image;

   function Recover_Image
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree;
      First        : in     Recover_Op_Ref := No_Element)
     return String
   is
      use Ada.Strings.Unbounded;
      Ref : constant Recover_Op_Ref := First;
   begin
      if First = No_Element then
         declare
            function Recover_Image is new Stream_Node_Ref_Lists.Gen_Image_Aux (Syntax_Trees.Tree, Recover_Image);
         begin
            return Recover_Image (Parser_State.Recover_Insert_Delete, Tree);
         end;
      end if;

      --  First is always in the last error, which is the active error.
      declare
         Result : Unbounded_String := +"(";
         Ops : Recover_Op_Nodes_Arrays.Vector renames Recover_Op_Array_Const_Ref_From_Cursor (Ref.Err_Cur);
      begin
         for I in Ref.Op_Index .. Ops.Last_Index loop
            Append (Result, Image (Ops (I), Tree));
         end loop;
         return -Result & ")";
      end;
   end Recover_Image;

   function Last_Recover_Insert_Delete
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree)
     return Recover_Op_Ref
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Parser_State.Recover_Insert_Delete.Length = 0 then
         return No_Element;
      end if;

      return Result      : Recover_Op_Ref do
         Result.Ref_Cur  := Parser_State.Recover_Insert_Delete.Last;
         Result.Err_Cur  := Tree.Error_List (Parser_State.Recover_Insert_Delete (Result.Ref_Cur).Node).Last;
         Result.Op_Index := 1;
      end return;
   end Last_Recover_Insert_Delete;

   procedure Next_Recover_Op_Ref
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree)
   is begin
      Next_Recover_Op_Ref (Parser_State, Tree, Parser_State.Recover_Insert_Delete_Current);
   end Next_Recover_Op_Ref;

   procedure Next_Recover_Op_Ref
     (Parser_State : in out Parser_Lists.Parser_State;
      Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Recover_Op_Ref)
   is
      use Syntax_Trees;
      use all type Stream_Node_Ref_Lists.Cursor;
      use all type Error_Data_Lists.Cursor;

      procedure First_Error
      is
         Error_List : Error_Data_Lists.List renames Tree.Error_List
           (Parser_State.Recover_Insert_Delete (Ref.Ref_Cur).Node);
      begin
         Ref.Err_Cur := Error_List.First;

         if Error_Data_Lists.Constant_Ref (Ref.Err_Cur) in Lexer_Error then
            Error_Data_Lists.Next (Ref.Err_Cur);
            --  There can be only one lexer error per node.
         end if;

         if Ref.Err_Cur /= Error_List.Last then
            raise SAL.Programmer_Error with "parser_lists: more than one error on a node";
         end if;
         Ref.Op_Index := 1;
      end First_Error;

   begin
      if Ref.Ref_Cur = Stream_Node_Ref_Lists.No_Element then
         Ref.Ref_Cur := Parser_State.Recover_Insert_Delete.First;
         First_Error;

      else
         if Ref.Op_Index < Recover_Op_Array_Const_Ref_From_Cursor (Ref.Err_Cur).Last_Index then
            Ref.Op_Index := @ + 1;

         else
            Error_Data_Lists.Next (Ref.Err_Cur);
            if Error_Data_Lists.Has_Element (Ref.Err_Cur) then
               pragma Assert (not (Error_Data_Lists.Constant_Ref (Ref.Err_Cur) in Lexer_Error));
               Ref.Op_Index := 1;

            else
               Stream_Node_Ref_Lists.Next (Ref.Ref_Cur);
               if Stream_Node_Ref_Lists.Has_Element (Ref.Ref_Cur) then
                  First_Error;
               else
                  Ref.Op_Index := 0;
               end if;
            end if;
         end if;
      end if;
   end Next_Recover_Op_Ref;

   procedure Recover_Op_Ref_Update
     (Parser_State    : in out Parser_Lists.Parser_State;
      Tree            : in out Syntax_Trees.Tree;
      Error           : in     Syntax_Trees.Error_Data'Class;
      User_Data       : in     Syntax_Trees.User_Data_Access_Constant)
   is
      use all type Stream_Node_Ref_Lists.Cursor;

      Update_Current : constant Boolean :=
        Parser_State.Recover_Insert_Delete_Current.Ref_Cur = Parser_State.Recover_Insert_Delete.Last;

      Ref_Parents : Syntax_Trees.Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (Parser_State.Recover_Insert_Delete (Parser_State.Recover_Insert_Delete.Last));

      --  FIXME: this assumes there is only one parse or in_parse_action
      --  error on the error node, after any lexer error.
      Err_Cur_Index : constant SAL.Base_Peek_Type := Tree.Error_List (Ref_Parents.Ref.Node).Length;
   begin
      Tree.Update_Error (Parser_State.Stream, Ref_Parents, Error, User_Data);
      Parser_State.Recover_Insert_Delete (Parser_State.Recover_Insert_Delete.Last) := Ref_Parents.Ref;

      if Update_Current then
         Parser_State.Recover_Insert_Delete_Current.Err_Cur := Get_List_Cursor
           (Tree.Error_List (Ref_Parents.Ref.Node), Err_Cur_Index);
      end if;
   end Recover_Op_Ref_Update;

   function Current_Insert_Delete_Error
     (Parser_State : in Parser_Lists.Parser_State)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Syntax_Trees.Error_Data_Lists.Element (Parser_State.Recover_Insert_Delete_Current.Err_Cur);
   end Current_Insert_Delete_Error;

   function Current_Insert_Delete_Op_Index
     (Parser_State : in Parser_Lists.Parser_State)
     return SAL.Base_Peek_Type
   is begin
      return Parser_State.Recover_Insert_Delete_Current.Op_Index;
   end Current_Insert_Delete_Op_Index;

   function Current_Error_Node
     (Parser_State : in Parser_Lists.Parser_State;
      Tree         : in Syntax_Trees.Tree)
     return Syntax_Trees.Stream_Node_Parents
   is begin
      return Ref : Syntax_Trees.Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (Tree.Current_Token (Parser_State.Stream))
      do
         if Tree.Has_Error (Ref.Ref.Node) then
            null;
         else
            --  Error on shifting a nonterm. test_incremental.adb Lexer_Errors_1.
            Tree.First_Terminal (Ref, Following => False);
            if Ref.Ref.Node /= Syntax_Trees.Invalid_Node_Access and then Tree.Has_Error (Ref.Ref.Node) then
               null;
            else
               Ref := Tree.To_Stream_Node_Parents
                 (Tree.To_Rooted_Ref (Parser_State.Stream, Tree.Peek (Parser_State.Stream)));
               pragma Assert (Tree.Has_Error (Ref.Ref.Node));
            end if;
         end if;
      end return;
   end Current_Error_Node;

   function Peek_Current_Sequential_Terminal
     (Parser_State : in Parser_Lists.Parser_State;
      Tree         : in Syntax_Trees.Tree)
     return Syntax_Trees.Terminal_Ref
   is
      use Syntax_Trees;
      Result : Stream_Node_Parents := Tree.To_Stream_Node_Parents
        (Tree.Current_Token (Parser_State.Stream));
   begin
      Tree.First_Sequential_Terminal (Result);
      return Result.Ref;
   end Peek_Current_Sequential_Terminal;

   function New_List (Tree : in out Syntax_Trees.Tree) return List
   is begin
      return Result : List
      do
         Result.Elements.Append
           ((Stream => Tree.New_Stream (Syntax_Trees.Invalid_Stream_ID),
             others => <>));
      end return;
   end New_List;

   procedure Clear (List : in out Parser_Lists.List)
   is begin
      List.Elements.Finalize;
   end Clear;

   function Count (List : in Parser_Lists.List) return SAL.Base_Peek_Type
   is begin
      return List.Elements.Length;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      return (Ptr => List.Elements.First);
   end First;

   procedure Next (Cursor : in out Parser_Lists.Cursor)
   is begin
      Parser_State_Lists.Next (Cursor.Ptr);
   end Next;

   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean
   is
      use all type Parser_State_Lists.Cursor;
   begin
      return Cursor.Ptr = Parser_State_Lists.No_Element;
   end Is_Done;

   function Stream (Cursor : in Parser_Lists.Cursor) return Syntax_Trees.Stream_ID
   is begin
      return Parser_State_Lists.Constant_Ref (Cursor.Ptr).Stream;
   end Stream;

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
         Parsers.Elements.Delete (Temp);
      end Free;
   begin
      declare
         State : Parser_State renames Parser_State_Lists.Variable_Ref (Current.Ptr);
      begin
         if Trace_Parse > Outline then
            Trace.Put_Line
              (" " & Tree.Trimmed_Image (Current.Stream) & ": terminate (" &
                 Trimmed_Image (Integer (Parsers.Count) - 1) & " active)" &
                 ": " & Message & " " & Tree.Image (Tree.Current_Token (State.Stream), Terminal_Node_Numbers => True));
         end if;

         Tree.Delete_Stream (State.Stream);
         State.Clear_Stream;
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
         function Same_Last_Terminal return Boolean
         is
            use Syntax_Trees;
            Ref_1 : Stream_Node_Parents := Tree.To_Stream_Node_Parents
              (Tree.To_Rooted_Ref (Stack_1, Tree.Peek (Stack_1)));
            Ref_2 : Stream_Node_Parents := Tree.To_Stream_Node_Parents
              (Tree.To_Rooted_Ref (Stack_2, Tree.Peek (Stack_2)));
         begin
            Tree.Last_Terminal (Ref_1, Stack_1);
            loop
               exit when Tree.Label (Ref_1.Ref.Node) = Source_Terminal;
               Tree.Prev_Terminal (Ref_1, Stack_1);
            end loop;

            Tree.Last_Terminal (Ref_2, Stack_2);
            loop
               exit when Tree.Label (Ref_1.Ref.Node) = Source_Terminal;
               Tree.Prev_Terminal (Ref_2, Stack_2);
            end loop;
            return Tree.Byte_Region (Ref_1.Ref.Node, Trailing_Non_Grammar => False) =
              Tree.Byte_Region (Ref_2.Ref.Node, Trailing_Non_Grammar => False);
         end Same_Last_Terminal;

      begin
         if Tree.Stack_Depth (Stack_1) /= Tree.Stack_Depth (Stack_2) then
            return False;

         elsif not Same_Last_Terminal then
            --  ada_mode-bad_duplicate_state.adb requires this check; otherwise it
            --  reports a syntax_error on 'renames'.
            return False;

         else
            for I in reverse 1 .. Tree.Stack_Depth (Stack_1) - 1 loop
               --  Assume they differ near the top; no point in comparing bottom
               --  item.
               if Tree.State (Stack_1) /= Tree.State (Stack_2) then
                  return False;
               else
                  declare
                     use Syntax_Trees;
                     Node_1 : constant Valid_Node_Access := Tree.Get_Node (Stack_1, Tree.Peek (Stack_1, I));
                     Node_2 : constant Valid_Node_Access := Tree.Get_Node (Stack_2, Tree.Peek (Stack_2, I));
                  begin
                     --  We can't use Node_1 = Node_2, because the nodes were created
                     --  independently by separate parsers. For LR parsing, the only node
                     --  attribute that matters is ID.
                     if Tree.ID (Node_1) /= Tree.ID (Node_2) then
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
         declare
            use Ada.Strings.Unbounded;
            One_Stream : constant Syntax_Trees.Stream_ID := Current.Stream;
            Another_Stream : constant Syntax_Trees.Stream_ID := Other.Stream;
            Msg : Unbounded_String;
         begin
            if Other.State_Ref.Total_Recover_Cost = Current.State_Ref.Total_Recover_Cost then
               if Other.State_Ref.Max_Recover_Ops_Length = Current.State_Ref.Max_Recover_Ops_Length then
                  Append (Msg, ": random");
               else
                  Append (Msg, ": min ops length");
                  --  Keep the minimum ops length
                  if Other.State_Ref.Max_Recover_Ops_Length > Current.State_Ref.Max_Recover_Ops_Length then
                     null;
                  else
                     Other := Cursor (Current);
                     Current.Next;
                  end if;
               end if;
            else
               Append (Msg, ": cost");
               if Other.State_Ref.Total_Recover_Cost > Current.State_Ref.Total_Recover_Cost then
                  null;
               else
                  Other := Cursor (Current);
                  Current.Next;
               end if;
            end if;
            Parsers.Terminate_Parser
              (Other, Tree, "duplicate state with " & Tree.Trimmed_Image
                 (if Another_Stream = Other.Stream
                  then One_Stream
                  else Another_Stream) & (-Msg),
               Trace);
         end;
      end if;
   end Duplicate_State;

   function State_Ref (Position : in Cursor) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Unchecked_Ref (Position.Ptr));
   end State_Ref;

   function First_State_Ref (List : in Parser_Lists.List'Class) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Unchecked_Ref (List.Elements.First));
   end First_State_Ref;

   function First_Constant_State_Ref (List : in Parser_Lists.List'Class) return Constant_State_Reference
   is begin
      return (Element => Parser_State_Lists.Unchecked_Ref (List.Elements.First));
   end First_Constant_State_Ref;

   procedure Prepend_Copy
     (List      : in out Parser_Lists.List;
      Cursor    : in     Parser_Lists.Cursor'Class;
      Tree      : in out Syntax_Trees.Tree;
      User_Data : in     Syntax_Trees.User_Data_Access_Constant;
      Trace     : in out WisiToken.Trace'Class)
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
         --  We copy Recover.Enqueue_Count, Check_Count for test_mckenzie_recover.adb.
         New_Item :=
           (Recover_Insert_Delete         => Item.Recover_Insert_Delete,
            Recover_Insert_Delete_Current => Item.Recover_Insert_Delete_Current,
            Recover                 =>
              (Enqueue_Count        => Item.Recover.Enqueue_Count,
               Check_Count          => Item.Recover.Check_Count,
               others               => <>),
            Total_Recover_Cost      => Item.Total_Recover_Cost,
            Max_Recover_Ops_Length  => Item.Max_Recover_Ops_Length,
            Error_Count             => Item.Error_Count,
            Resume_Active           => Item.Resume_Active,
            Resume_Token_Goal       => Item.Resume_Token_Goal,
            Conflict_During_Resume  => Item.Conflict_During_Resume,
            Zombie_Token_Count      => 0,
            Last_Action             => Item.Last_Action,
            Stream                  => Tree.New_Stream (Item.Stream),
            Verb                    => Item.Verb);
      end;

      List.Elements.Prepend (New_Item);
   end Prepend_Copy;

   ----------
   --  stuff for iterators

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor
   is begin
      return (Ptr => Ptr.Ptr);
   end To_Cursor;

   function To_Parser_Node_Access (Cur : in Cursor) return Parser_Node_Access
   is begin
      return (Ptr => Cur.Ptr);
   end To_Parser_Node_Access;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Unchecked_Ref (Position.Ptr));
   end Constant_Reference;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Unchecked_Ref (Position.Ptr));
   end Reference;

   function Unchecked_State_Ref (Position : in Parser_Node_Access) return State_Access
   is begin
      return State_Access (Parser_State_Lists.Unchecked_Ref (Position.Ptr));
   end Unchecked_State_Ref;

   type Iterator (Elements : access Parser_State_Lists.List) is new Iterator_Interfaces.Forward_Iterator
     with null record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return (Ptr => Object.Elements.First);
   end First;

   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access
   is
      pragma Unreferenced (Object);
   begin
      return (Ptr => Parser_State_Lists.Next (Position.Ptr));
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

   procedure Clear_Stream (State : in out Parser_State)
   is begin
      State.Stream := WisiToken.Syntax_Trees.Invalid_Stream_ID;
      State.Recover_Insert_Delete.Clear;
      State.Recover_Insert_Delete_Current := No_Element;
   end Clear_Stream;

   function Verb (State : in Parser_State) return All_Parse_Action_Verbs
   is begin
      return State.Verb;
   end Verb;

   procedure Set_Verb (State : in out Parser_State; Verb : in All_Parse_Action_Verbs)
   is begin
      State.Verb := Verb;
   end Set_Verb;

end WisiToken.Parse.LR.Parser_Lists;
