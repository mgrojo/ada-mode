--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
with Ada.Text_IO;
package body WisiToken.Syntax_Trees is

   --  Body specs, alphabetical, as needed

   type Visit_Parent_Mode is (Before, After);

   function Append_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     Unknown_State_Index)
     return Terminal_Ref
   with Post => Tree.Valid_Terminal (Append_Stream_Element'Result);
   --  Add Node at Stream.Last; if not Shared_Stream, set Stack_Top to
   --  element containing Node. If Node is from Shared_Stream, it has
   --  been copied

   function Child_Index (N : in Node; Child : in Valid_Node_Access) return SAL.Peek_Type;

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Rooted_Ref;
   --  If Before is No_Element, add Node after Stream.Stack_Top (at
   --  beginning of input). Otherwise add Node before Before.
   --
   --  Caller must change Stream.Stack_Top if necessary.

   function Last_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Ref);

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;

   procedure Next_Source_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents);

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID;

   function Pop (Parse_Stream : in out Syntax_Trees.Parse_Stream) return Valid_Node_Access;

   procedure Prev_Non_Grammar
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref);

   procedure Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access);

   procedure Prev_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack);

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Access;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Access)
        return Boolean)
     return Boolean;
   --  Call Process_Node on nodes in tree rooted at Node. Return when
   --  Process_Node returns False (Process_Tree returns False), or when
   --  all nodes have been processed (Process_Tree returns True).

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in out Valid_Node_Access;
      Children : in     Node_Access_Array);

   function Subtree_Image
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Node_Access;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Level        : in Integer                   := 0;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String;

   ----------
   --  Public and body operations, alphabetical

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Post_Parse_Action
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Action;
   end Action;

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Identifier,
         Child_Count => 0,
         ID          => ID,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         Identifier  => Identifier,
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Identifier;

   function Add_Nonterm_1
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Action          : in     Post_Parse_Action;
      Default_Virtual : in     Boolean;
      Clear_Parents   : in     Boolean)
     return Valid_Node_Access
   is
      Nonterm_Node : constant Valid_Node_Access := new Node'
        (Label       => Syntax_Trees.Nonterm,
         Child_Count => Children'Last,
         ID          => Production.LHS,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         Children    => To_Node_Access (Children),
         Action      => Action,
         RHS_Index   => Production.RHS,
         Virtual     => (if Children'Length = 0 then Default_Virtual else False),
         others      => <>);
   begin
      Tree.Nodes.Append (Nonterm_Node);

      for Child of Children loop
         case Child.Label is
         when Source_Terminal | Virtual_Identifier =>
            null;

         when Virtual_Terminal =>
            Nonterm_Node.Virtual := True;

         when Nonterm =>
            if Child.Virtual then
               Nonterm_Node.Virtual := True;
            end if;
         end case;

         if Child.Parent /= Invalid_Node_Access then
            if Clear_Parents then
               declare
                  Other_Parent : constant Node_Access := Child.Parent;
                  Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                    (Other_Parent.all, Child);
               begin
                  Other_Parent.Children (Child_Index) := Invalid_Node_Access;
               end;
            else
               pragma Assert (False, "attempt to use children with existing parents");
            end if;
         end if;

         if Tree.Parents_Set then
            Child.Parent := Nonterm_Node;
         end if;
      end loop;

      return Nonterm_Node;
   end Add_Nonterm_1;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Clear_Parents   : in     Boolean;
      Action          : in     Post_Parse_Action := null;
      Default_Virtual : in     Boolean           := False)
     return Valid_Node_Access
   is begin
      return Add_Nonterm_1 (Tree, Production, Children, Action, Default_Virtual, Clear_Parents);
   end Add_Nonterm;

   function Add_Source_Terminal_1
     (Tree             : in out Syntax_Trees.Tree;
      Terminal         : in     WisiToken.Lexer.Token;
      In_Shared_Stream : in     Boolean)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Source_Terminal,
         Child_Count => 0,
         ID          => Terminal.ID,

         Node_Index  =>
           (if In_Shared_Stream
            then Tree.Next_Terminal_Node_Index
            else -(Tree.Nodes.Last_Index + 1)),

         Byte_Region => Terminal.Byte_Region,
         Char_Region => Terminal.Char_Region,
         others      => <>)
      do
         if Terminal.ID = Tree.Lexer.Descriptor.EOI_ID then
            Tree.EOI := Result;
            Result.Non_Grammar.Append (Terminal);
         end if;
         if In_Shared_Stream then
            Tree.Next_Terminal_Node_Index := @ + 1;
         end if;
         Tree.Nodes.Append (Result);
      end return;
   end Add_Source_Terminal_1;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Lexer.Token)
     return Single_Terminal_Ref
   is begin
      return Append_Stream_Element
        (Tree, Stream, Add_Source_Terminal_1 (Tree, Terminal, Stream = Tree.Shared_Stream), State => Unknown_State);
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Lexer.Token)
     return Valid_Node_Access
   is (Add_Source_Terminal_1 (Tree, Terminal, In_Shared_Stream => False));

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Terminal;

   function Append_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     Unknown_State_Index)
     return Terminal_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Stream_Element_Lists.Cursor := Parse_Stream.Elements.Append
        ((Node  => Node,
          State => State,
          Label => Parse_Stream.Label));
   begin
      if Stream = Tree.Shared_Stream then
         --  Stack_Top is always Invalid_Stream_Element.
         null;
      else
         Parse_Stream.Stack_Top := New_Element;
      end if;

      return (Stream, (Cur => New_Element), Node);
   end Append_Stream_Element;

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access
   is begin
      return Node.Augmented;
   end Augmented;

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Augmented_Class_Access_Constant
   is begin
      return Augmented_Class_Access_Constant (Node.Augmented);
   end Augmented_Const;

   procedure Breakdown
     (Tree : in out Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   is
      use Stream_Element_Lists;
      Target    : constant Valid_Node_Access := Ref.Node;
      Stream    : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Stream.Cur);
      Cur       : Cursor                    renames Ref.Element.Cur;
      To_Delete : Cursor                     := Cur;
      Parents   : Node_Stacks.Stack;
   begin
      declare
         Node      : Valid_Node_Access          := Ref.Node;
         Root_Node : constant Valid_Node_Access := Stream.Elements (Ref.Element.Cur).Node;
      begin
         loop
            exit when Node = Root_Node;
            Parents.Push (Node);
            Node := Node.Parent;
         end loop;
      end;

      Undo_Reduce_Loop :
      loop
         declare
            Node : constant Valid_Node_Access := Stream.Elements (Cur).Node;
         begin
            pragma Assert (Node.Label = Nonterm);
            exit Undo_Reduce_Loop when First_Terminal (Tree, Node) = Target;

            pragma Assert (Node.Child_Count > 0); -- otherwise we would not get here.

            for Child of reverse Node.Children loop
               Cur := Stream.Elements.Insert
                 (Element  =>
                    (Node  => Child,
                     State => Unknown_State,
                     Label => Stream.Label),
                  Before   => Cur);

               Child.Parent := Invalid_Node_Access;
            end loop;
         end;

         Stream.Elements.Delete (To_Delete);

         --  Find stream element containing Target
         declare
            Node : constant Valid_Node_Access := Parents.Peek;
         begin
            Find_Element :
            loop
               if Stream.Elements (Cur).Node = Node then
                  Parents.Pop;
                  exit Undo_Reduce_Loop when Node.Label in Terminal_Label;
                  exit Find_Element;
               end if;
               Next (Cur);
            end loop Find_Element;
         end;
         To_Delete := Cur;
      end loop Undo_Reduce_Loop;
   end Breakdown;

   function Byte_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := False)
     return WisiToken.Buffer_Region
   is begin
      case Node.Label is
      when Source_Terminal =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            if Node.Byte_Region = Null_Buffer_Region then
               return
                 (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);
            else
               return
                 (First => Node.Byte_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);
            end if;
         else
            return Node.Byte_Region;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            return
              (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First,
               Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last);
         else
            return Null_Buffer_Region;
         end if;

      when Nonterm =>
         declare
            First : constant Node_Access := First_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
            Last  : constant Node_Access := Last_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
         begin
            if First = Invalid_Node_Access then
               return Null_Buffer_Region;
            else
               return
                 (First => Byte_Region (Tree, First, Trailing_Non_Grammar).First,
                  Last  => Byte_Region (Tree, Last, Trailing_Non_Grammar).Last);
            end if;
         end;
      end case;
   end Byte_Region;

   function Byte_Region (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region
   is begin
      if Item.Virtual then
         return Item.Byte_Region;
      elsif Item.Element_Node = Invalid_Node_Access then
         return Item.Node.Byte_Region;
      else
         return Tree.Byte_Region (Item.Element_Node);
      end if;
   end Byte_Region;

   function Char_Region
     (Tree                : in Syntax_Trees.Tree;
      Node                : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := False)
     return Buffer_Region
   is begin
      case Node.Label is
      when Source_Terminal =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            if Node.Char_Region = Null_Buffer_Region then
               return
                 (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Char_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last);
            else
               return
                 (First => Node.Char_Region.First,
                  Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last);
            end if;
         else
            return Node.Char_Region;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if Trailing_Non_Grammar and Node.Non_Grammar.Length > 0 then
            return
              (First => Node.Non_Grammar (Node.Non_Grammar.First_Index).Char_Region.First,
               Last  => Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last);
         else
            return Null_Buffer_Region;
         end if;

      when Nonterm =>
         declare
            First : constant Node_Access := First_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
            Last  : constant Node_Access := Last_Source_Terminal (Tree, Node, Trailing_Non_Grammar);
         begin
            if First = Invalid_Node_Access then
               return Null_Buffer_Region;
            else
               return
                 (First => Char_Region (Tree, First, Trailing_Non_Grammar).First,
                  Last  => Char_Region (Tree, Last, Trailing_Non_Grammar).Last);
            end if;
         end;
      end case;
   end Char_Region;

   function Check_Non_Grammar
     (Tree     : in     Syntax_Trees.Tree;
      Node     : in     Valid_Node_Access;
      Line     : in     Line_Number_Type;
      Char_Pos : in out Buffer_Pos)
     return Boolean
   with Pre => Node.Label in Terminal_Label
   --  Return True if Node contains non_grammar that ends Line - 1; set
   --  Char_Pos to the character position following the New_Line.
   is begin
      for Tok of Node.Non_Grammar loop
         if Tok.ID in Tree.Lexer.Descriptor.New_Line_ID | Tree.Lexer.Descriptor.EOI_ID and
           Tok.Line_Region.First = Line - 1
         then
            Char_Pos := Tok.Char_Region.Last + 1;
            return True;
         end if;
      end loop;
      return False;
   end Check_Non_Grammar;

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Child_Index : in Positive_Index_Type)
     return Node_Access
   is
   begin
      if Child_Index in Node.Children'Range then
         return Node.Children (Child_Index);
      else
         return Invalid_Node_Access;
      end if;
   end Child;

   function Child_Index (N : in Node; Child : in Valid_Node_Access) return SAL.Peek_Type
   is begin
      for I in N.Children'Range loop
         if N.Children (I) = Child then
            return I;
         end if;
      end loop;
      raise SAL.Programmer_Error; -- Should be prevented by precondition
   end Child_Index;

   function Child_Index
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Access;
      Child  : in     Valid_Node_Access)
     return SAL.Peek_Type
   is
      pragma Unreferenced (Tree);
   begin
      return Child_Index (Parent.all, Child);
   end Child_Index;

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access_Array
   is begin
      return Node.Children;
   end Children;

   function Children_Recover_Tokens
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Recover_Token_Array
   is
      Node : constant Node_Access := Stream_Element_Lists.Constant_Ref (Element.Cur).Node;
   begin
      --  WORKAROUND: GNAT Community 2020 doesn't support 'of' here, and it
      --  hangs if there are any errors in the statement with 'in'.
      --  return (for I in Node.Children'Range => Tree.Get_Recover_Token (Node.Children (I)));
      return Result : Recover_Token_Array (1 .. Node.Child_Count) do
         for I in Node.Children'Range loop
            Result (I) := Tree.Get_Recover_Token (Node.Children (I));
         end loop;
      end return;
   end Children_Recover_Tokens;

   procedure Clear
     (Tree        : in out Syntax_Trees.Tree;
      Free_Memory : in     Boolean := False)
   is begin
      for N of Tree.Nodes loop
         Free (N);
      end loop;
      Tree.Nodes.Clear (Free_Memory);

      Tree.Streams.Clear;

      Tree.Root                     := Invalid_Node_Access;
      Tree.SOI                      := Invalid_Node_Access;
      Tree.EOI                      := Invalid_Node_Access;
      Tree.Next_Stream_Label        := Shared_Stream_Label + 1;
      Tree.Next_Terminal_Node_Index := 1;
      Tree.Traversing               := False;
      Tree.Parents_Set              := False;
   end Clear;

   procedure Clear_Parse_Streams
     (Tree       : in out Syntax_Trees.Tree;
      Keep_Nodes : in     Valid_Node_Access_Lists.List := Valid_Node_Access_Lists.Empty_List)
   is begin
      if Tree.Root = Invalid_Node_Access then
         Tree.Root := Syntax_Trees.Root (Tree);
      end if;

      --  Add SOI, EOI to Root children, so Prev/Next_Non_Grammar can find them.
      declare
         New_Children : Node_Access_Array (1 .. Tree.Root.Child_Count + 2);
      begin
         New_Children (1) := Tree.SOI;
         New_Children (2 .. New_Children'Last - 1) := Tree.Root.Children;
         New_Children (New_Children'Last) := Tree.EOI;

         Tree.Root := new Node'
           (Label       => Nonterm,
            Child_Count => Tree.Root.Child_Count + 2,
            ID          => Tree.Root.ID,
            Node_Index  => Tree.Root.Node_Index,
            Parent      => null,
            Augmented   => Tree.Root.Augmented,
            Virtual     => Tree.Root.Virtual,
            RHS_Index   => Tree.Root.RHS_Index,
            Action      => Tree.Root.Action,
            Name_Offset => Tree.Root.Name_Offset,
            Name_Length => Tree.Root.Name_Length,
            Children    => New_Children);

         for Child of New_Children loop
            Child.Parent := Tree.Root;
         end loop;

         Tree.Nodes.Append (Tree.Root);
      end;

      Tree.Streams.Clear;
      Tree.Next_Stream_Label := Shared_Stream_Label + 1;

      Tree.Shared_Stream.Cur := Parse_Stream_Lists.No_Element;

      if not Tree.Parents_Set then
         Set_Parents (Tree);
      end if;

      for Node of Tree.Nodes loop
         if Node.Parent = null and then
           Node /= Tree.Root and then
           Node /= Tree.SOI and then
           Node /= Tree.EOI and then
           not (for some N of Keep_Nodes => N = (Node))
         then
            Free (Node);
         end if;
      end loop;

      --  Compact Tree.Nodes
      declare
         Free : Node_Index := Tree.Nodes.First_Index - 1;
      begin
         for I in Tree.Nodes.First_Index .. Tree.Nodes.Last_Index loop
            if Free < Tree.Nodes.First_Index then
               if Tree.Nodes (I) = Invalid_Node_Access then
                  Free := I;
               end if;
            else
               if Tree.Nodes (I) /= Invalid_Node_Access then
                  Tree.Nodes (Free) := Tree.Nodes (I);
                  Free := @ + 1;
               end if;
            end if;
         end loop;

         if Free > Tree.Nodes.First_Index then
            Tree.Nodes.Set_First_Last (First => Tree.Nodes.First_Index, Last => Free - 1);
         end if;
      end;
   end Clear_Parse_Streams;

   function Column (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Ada.Text_IO.Count
   is
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node);
   begin
      if Char_Region = Null_Buffer_Region then
         return 0;
      else
         declare
            Begin_Char_Pos : constant Buffer_Pos := Line_Begin_Char_Pos (Tree, Tree.Line_Region (Node).First);
         begin
            return
              (if Begin_Char_Pos = Invalid_Buffer_Pos
               then 0
               else Ada.Text_IO.Count (Char_Region.First - Begin_Char_Pos));
         end;
      end if;
   end Column;

   function Column
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Stream : in Stream_ID)
     return Ada.Text_IO.Count
   is
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node);
   begin
      if Char_Region.First = Invalid_Buffer_Pos then
         return 0;
      else
         declare
            Begin_Char_Pos : constant Buffer_Pos := Line_Begin_Char_Pos (Tree, Tree.Line_Region (Node).First, Stream);
         begin
            return
              (if Begin_Char_Pos = Invalid_Buffer_Pos
               then 0
               else Ada.Text_IO.Count (Char_Region.First - Begin_Char_Pos));
         end;
      end if;
   end Column;

   function Contains_Virtual_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Item.Virtual
         then Item.Contains_Virtual_Terminal
         else
           (case Item.Element_Node.Label is
            when Source_Terminal => False,
            when Virtual_Terminal | Virtual_Identifier => True,
            when Nonterm => Item.Element_Node.Virtual));
   end Contains_Virtual_Terminal;

   function Copy_Augmented
     (User_Data : in User_Data_Type;
      Augmented : in Augmented_Class_Access)
     return Augmented_Class_Access
   is begin
      raise SAL.Programmer_Error;
      return null;
   end Copy_Augmented;

   function Copy_Subtree
     (Tree      : in out Syntax_Trees.Tree;
      Root      : in     Node_Access;
      User_Data : in     User_Data_Access)
     return Node_Access
   is
      function Copy_Node
        (Tree   : in out Syntax_Trees.Tree;
         Node   : in     Valid_Node_Access;
         Parent : in     Node_Access)
        return Valid_Node_Access
      is
         New_Node : Node_Access;
      begin
         case Node.Label is
         when Source_Terminal =>
            New_Node := new Syntax_Trees.Node'
              (Label       => Source_Terminal,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Tree.Next_Terminal_Node_Index + 1,
               Byte_Region => Node.Byte_Region,
               Char_Region => Node.Char_Region,
               Parent      => (if Tree.Parents_Set then Parent else Invalid_Node_Access),
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               Non_Grammar      => Node.Non_Grammar,
               Sequential_Index => Node.Sequential_Index);

            Tree.Nodes.Append (New_Node);
            Tree.Next_Terminal_Node_Index := @ + 1;

         when Virtual_Terminal =>
            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Tree.Next_Terminal_Node_Index + 1,
               Parent      => Parent,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               Non_Grammar      => Node.Non_Grammar,
               Sequential_Index => Node.Sequential_Index);

            Tree.Nodes.Append (New_Node);
            Tree.Next_Terminal_Node_Index := @ + 1;

         when Virtual_Identifier =>

            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Tree.Next_Terminal_Node_Index + 1,
               Parent      => Parent,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               Non_Grammar      => Node.Non_Grammar,
               Sequential_Index => Node.Sequential_Index,
               Identifier       => Node.Identifier);

            Tree.Nodes.Append (New_Node);
            Tree.Next_Terminal_Node_Index := @ + 1;

         when Nonterm =>
            declare
               New_Children : Node_Access_Array (Node.Children'Range);
            begin
               for I in New_Children'Range loop
                  New_Children (I) := Copy_Node (Tree, Node.Children (I), Invalid_Node_Access);
               end loop;

               New_Node := new Syntax_Trees.Node'
                 (Label       => Nonterm,
                  Child_Count => New_Children'Last,
                  ID          => Node.ID,
                  Node_Index  => -(Tree.Nodes.Last_Index + 1),
                  Parent      => Parent,
                  Augmented   =>
                    (if Node.Augmented = null or User_Data = null
                     then null
                     else Copy_Augmented (User_Data.all, Node.Augmented)),
                  Virtual     => Node.Virtual,
                  RHS_Index   => Node.RHS_Index,
                  Action      => Node.Action,
                  Name_Offset => Node.Name_Offset,
                  Name_Length => Node.Name_Length,
                  Children    => New_Children);

               for Child of New_Node.Children loop
                  Child.Parent := New_Node;
               end loop;
               Tree.Nodes.Append (New_Node);
            end;
         end case;
         return New_Node;
      end Copy_Node;

   begin
      if Root = Invalid_Node_Access then
         return Invalid_Node_Access;
      else
         return Copy_Node (Tree, Root, Invalid_Node_Access);
      end if;
   end Copy_Subtree;

   procedure Copy_Tree
     (Source      : in     Tree;
      Destination :    out Tree;
      User_Data   : in     User_Data_Access)
   is
      function Copy_Node
        (Source_Node : in Valid_Node_Access;
         Dest_Parent : in Node_Access)
        return Valid_Node_Access
      is
         New_Dest_Node : Node_Access;
      begin
         case Source_Node.Label is
         when Source_Terminal =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Source_Terminal,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => Source_Node.Node_Index,
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Byte_Region      => Source_Node.Byte_Region,
               Char_Region      => Source_Node.Char_Region,
               Non_Grammar      => Source_Node.Non_Grammar,
               Sequential_Index => Source_Node.Sequential_Index);

            Destination.Nodes.Append (New_Dest_Node);

         when Virtual_Terminal =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  =>
                 (if Source_Node.Node_Index > 0
                  then Source_Node.Node_Index -- from Shared_Stream
                  else -(Destination.Nodes.Last_Index + 1)),
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Non_Grammar      => Source_Node.Non_Grammar,
               Sequential_Index => Source_Node.Sequential_Index);

            Destination.Nodes.Append (New_Dest_Node);

         when Virtual_Identifier =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  =>
                 (if Source_Node.Node_Index > 0
                  then Source_Node.Node_Index -- from Shared_Stream
                  else -(Destination.Nodes.Last_Index + 1)),
               Parent      => Dest_Parent,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Non_Grammar      => Source_Node.Non_Grammar,
               Sequential_Index => Source_Node.Sequential_Index,
               Identifier       => Source_Node.Identifier);

            Destination.Nodes.Append (New_Dest_Node);

         when Nonterm =>
            declare
               New_Children : Node_Access_Array (Source_Node.Children'Range);
            begin
               for I in New_Children'Range loop
                  New_Children (I) := Copy_Node (Source_Node.Children (I), Dummy_Node);
               end loop;

               New_Dest_Node := new Syntax_Trees.Node'
                 (Label       => Nonterm,
                  Child_Count => New_Children'Last,
                  ID          => Source_Node.ID,
                  Node_Index  => -(Destination.Nodes.Last_Index + 1),
                  Parent      => Dest_Parent,
                  Augmented   =>
                    (if Source_Node.Augmented = null or User_Data = null
                     then null
                     else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
                  Virtual     => Source_Node.Virtual,
                  RHS_Index   => Source_Node.RHS_Index,
                  Action      => Source_Node.Action,
                  Name_Offset => Source_Node.Name_Offset,
                  Name_Length => Source_Node.Name_Length,
                  Children    => New_Children);

               for Child of New_Dest_Node.Children loop
                  Child.Parent := New_Dest_Node;
               end loop;

               Destination.Nodes.Append (New_Dest_Node);
            end;
         end case;
         return New_Dest_Node;
      end Copy_Node;
   begin
      Destination.Clear (Free_Memory => False);
      Destination.Lexer                    := Source.Lexer;
      Destination.Next_Terminal_Node_Index := Source.Next_Terminal_Node_Index;
      Destination.Traversing               := False;
      Destination.Parents_Set              := True;

      Destination.Root := Copy_Node (Source.Root, Invalid_Node_Access);
      Destination.SOI  := Copy_Node (Source.SOI, Invalid_Node_Access);
      Destination.EOI  := Copy_Node (Source.EOI, Invalid_Node_Access);
   end Copy_Tree;

   function Count_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return SAL.Base_Peek_Type
   is begin
      return Result : SAL.Base_Peek_Type := 0 do
         if Node.ID = ID then
            Result := 1;
         end if;
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            null;
         when Nonterm =>
            for I of Node.Children loop
               --  We don't check for deleted child here; encountering one indicates
               --  an error in the user algorithm.
               Result := @ + Count_IDs (Tree, I, ID);
            end loop;
         end case;
      end return;
   end Count_IDs;

   function Count_Terminals
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Integer
   --  Count_Terminals must return Integer for Get_Terminals,
   --  Positive_Index_Type for Get_Terminal_IDs.
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return 1;

      when Nonterm =>
         return Result : Integer := 0 do
            for C of Node.Children loop
               --  This can be called to build a debugging image while editing the tree
               if C /= null then
                  Result := Result + Count_Terminals (Tree, C);
               end if;
            end loop;
         end return;
      end case;
   end Count_Terminals;

   procedure Clear_Parent
     (Tree           : in out Syntax_Trees.Tree;
      Node           : in     Valid_Node_Access;
      Clear_Children : in     Boolean)
   is begin
      if Node.Parent /= Invalid_Node_Access and Clear_Children then
         Node.Parent.Children (Child_Index (Node.Parent.all, Node)) := null;

         if Node.Parent = Tree.Root then
            Tree.Root := Node;
         end if;
      end if;
      Node.Parent := null;
   end Clear_Parent;

   procedure Delete_Stream (Tree : in out Syntax_Trees.Tree; Stream : in out Stream_ID)
   is
      use Parse_Stream_Lists;
   begin
      Tree.Streams.Delete (Stream.Cur);
   end Delete_Stream;

   procedure Delete_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in out Node_Access)
   is
      procedure Delete_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in out Node_Access)
      is begin
         Free (Node.Augmented);
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            Free (Node);

         when Nonterm =>
            for I in Node.Children'Range loop
               Delete_Node (Tree, Node.Children (I));
            end loop;

            Free (Node);
         end case;
      end Delete_Node;
   begin
      if Root = Invalid_Node_Access then
         null;
      else
         Delete_Node (Tree, Root);
      end if;
   end Delete_Subtree;

   procedure Delete_Token
     (Data          : in out User_Data_Type;
      Tree          : in     Syntax_Trees.Tree'Class;
      Deleted_Token : in     Valid_Node_Access;
      Prev_Token    : in     Node_Access)
   is begin
      Prev_Token.Non_Grammar.Append (Deleted_Token.Non_Grammar);
   end Delete_Token;

   function EOI (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      return Tree.EOI;
   end EOI;

   function Empty_Line
     (Tree        : in Syntax_Trees.Tree;
      Non_Grammar : in Lexer.Token_Arrays.Vector;
      Line        : in Line_Number_Type)
     return Boolean
   with Pre => (for some Token of Non_Grammar => Token.Line_Region.First = Line - 1)
   --  Return True if Line is empty
   is
      EOI_ID      : Token_ID renames Tree.Lexer.Descriptor.EOI_ID;
      New_Line_ID : Token_ID renames Tree.Lexer.Descriptor.New_Line_ID;
   begin
      for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
         if Non_Grammar (I).ID = New_Line_ID then
            if Non_Grammar (I).Line_Region.Last = Line then
               if I < Non_Grammar.Last_Index then
                  return Non_Grammar (I + 1).ID = New_Line_ID;
               else
                  return False;
               end if;
            end if;

         elsif Non_Grammar (I).ID = EOI_ID then
            return True;
         end if;
      end loop;
      --  Getting here violates the precondition
      raise SAL.Programmer_Error;
   end Empty_Line;

   function Error_Message_1
     (Tree             : in Syntax_Trees.Tree;
      Prev_Non_Grammar : in WisiToken.Lexer.Token_Arrays.Vector;
      First_Terminal   : in Node_Access;
      Message          : in String)
     return String
   is
      Line   : Line_Number_Type  := Line_Number_Type'First;
      Column : Ada.Text_IO.Count := Ada.Text_IO.Count'First;
   begin
      if Prev_Non_Grammar.Length > 0 then
         Line := Prev_Non_Grammar (Prev_Non_Grammar.Last_Index).Line_Region.Last;

         if First_Terminal /= Invalid_Node_Access then
            if First_Terminal.Char_Region.First /= Invalid_Buffer_Pos then
               declare
                  Begin_Char_Pos : constant Buffer_Pos :=
                    (if Tree.Editable
                     then Tree.Line_Begin_Char_Pos (Line)
                     else Tree.Line_Begin_Char_Pos (Line, Tree.Shared_Stream));
               begin
                  Column :=
                    (if Begin_Char_Pos = Invalid_Buffer_Pos or
                       First_Terminal.Char_Region.First < Begin_Char_Pos
                     then 0
                     else Ada.Text_IO.Count (First_Terminal.Char_Region.First - Begin_Char_Pos));
               end;
            end if;
         else
            --  No char_pos, so no column
            null;
         end if;

      else
         --  No line information, so also no column.
         null;
      end if;
      return WisiToken.Error_Message (Tree.Lexer.File_Name, Line, Column, Message);
   end Error_Message_1;

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Access;
      Message : in String)
     return String
   is
      Non_Grammar : constant Node_Access := Tree.Prev_Non_Grammar (Node);
   begin
      return Error_Message_1 (Tree, Non_Grammar.Non_Grammar, Tree.First_Terminal (Node), Message);
   end Error_Message;

   function Error_Message
     (Tree    : in Syntax_Trees.Tree;
      Ref     : in Stream_Node_Ref;
      Message : in String)
     return String
   is begin
      if Tree.Parents_Set then
         declare
            Non_Grammar    : Stream_Node_Ref := Ref;
            First_Terminal : Stream_Node_Ref := Ref;
         begin
            Tree.Prev_Non_Grammar (Non_Grammar);
            Tree.First_Terminal (First_Terminal);
            return Error_Message_1 (Tree, Non_Grammar.Node.Non_Grammar, First_Terminal.Node, Message);
         end;
      elsif Rooted (Ref) then
         declare
            Non_Grammar    : Stream_Node_Parents := (Ref, Parents => <>);
            First_Terminal : Stream_Node_Parents := (Ref, Parents => <>);
         begin
            Tree.Prev_Non_Grammar (Non_Grammar);
            Tree.First_Terminal (First_Terminal);
            return Error_Message_1 (Tree, Non_Grammar.Ref.Node.Non_Grammar, First_Terminal.Ref.Node, Message);
         end;

      else
         declare
            Non_Grammar    : constant Node_Access := Tree.First_Non_Grammar  (Ref.Node);
            First_Terminal : constant Node_Access := Tree.First_Terminal (Ref.Node);
         begin
            return Error_Message_1
              (Tree,
               (if Non_Grammar = Invalid_Node_Access
                then Lexer.Token_Arrays.Empty_Vector
                else Non_Grammar.Non_Grammar),
               First_Terminal,
               Message);
         end;
      end if;
   end Error_Message;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree)
   is begin
      Clear (Tree, Free_Memory => True);
      --  Tree.* array memory is freed by SAL Vectors Finalize.
   end Finalize;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      ID         : in Token_ID;
      Max_Parent : in Boolean := False)
     return Node_Access
   is
      N           : Node_Access := Node;
      Last_Parent : Node_Access := Invalid_Node_Access;
   begin
      loop
         N := N.Parent;

         exit when N = Invalid_Node_Access;
         Last_Parent := N;

         exit when ID = N.ID;
      end loop;

      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Access
   is
      N           : Node_Access := Node;
      Last_Parent : Node_Access := Invalid_Node_Access;
   begin
      loop
         N := N.Parent;

         exit when N = Invalid_Node_Access;
         Last_Parent := N;

         exit when (for some ID of IDs => ID = N.ID);
      end loop;

      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Node     : in Node_Access;
      Byte_Pos : in Buffer_Pos)
     return Node_Access
   --  Return terminal node in subtree under Node that contains (including
   --  non_grammar) or is after Byte_Pos. Invalid_Node_Access if byte_Pos
   --  is after all of Node.
   is begin
      case Node.Label is
      when Source_Terminal =>
         if Byte_Pos <= Node.Byte_Region.Last then
            return Node;
         elsif Node.Non_Grammar.Length > 0 and then
           Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if Node.Non_Grammar.Length > 0 and then
           Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         for Child of Node.Children loop
            declare
               Region : constant Buffer_Region := Tree.Byte_Region (Child, Trailing_Non_Grammar => True);
            begin
               if Region = Null_Buffer_Region then
                  --  Child is empty or virtual; try next
                  null;
               elsif Byte_Pos <= Region.First then
                  return Tree.First_Terminal (Child);
               elsif Byte_Pos <= Region.Last then
                  return Find_Byte_Pos (Tree, Child, Byte_Pos);
               else
                  null; -- try next child
               end if;
            end;
         end loop;
         --  Byte_Pos is after last child
         return Invalid_Node_Access;
      end case;
   end Find_Byte_Pos;

   function Find_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Byte_Pos : in Buffer_Pos)
     return Node_Access
   is
      Node : constant Node_Access := Root (Tree);
      Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node, Trailing_Non_Grammar => True);
   begin
      if Byte_Pos < Byte_Region.First then
         return Tree.First_Terminal (Node);
      elsif Byte_Pos > Byte_Region.Last then
         return Invalid_Node_Access;
      else
         return Find_Byte_Pos (Tree, Node, Byte_Pos);
      end if;
   end Find_Byte_Pos;

   function Find_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Byte_Pos : in Buffer_Pos)
     return Stream_Node_Ref
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Result       : Stream_Node_Ref :=
        (Stream, (Cur => Parse_Stream.Elements.First), null);
   begin
      loop
         Result.Node := Find_Byte_Pos (Tree, Parse_Stream.Elements (Result.Element.Cur).Node, Byte_Pos);
         if Result.Node = Invalid_Node_Access then
            --  Try next stream element
            Result.Element.Cur := Next (Result.Element.Cur);
            exit when Result.Element.Cur = No_Element;
         else
            return Result;
         end if;
      end loop;
      --  end of stream reached; Byte_Pos is after all of Stream
      return Result;
   end Find_Byte_Pos;

   function Find_Char_Pos
     (Tree                : in Syntax_Trees.Tree;
      Node                : in Node_Access;
      Char_Pos            : in Buffer_Pos;
      After               : in Boolean;
      Include_Non_Grammar : in Boolean)
     return Node_Access
   --  Return terminal in subtree under Node that contains (possibly including
   --  non_grammar) or is after Char_Pos. Invalid_Node_Access if char_Pos
   --  is after all of Node.
   is begin
      case Node.Label is
      when Source_Terminal =>
         if After and Char_Pos <= Node.Char_Region.Last then
            return Node;
         elsif (Include_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Char_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Virtual_Terminal | Virtual_Identifier =>
         if (Include_Non_Grammar and Node.Non_Grammar.Length > 0) and then
           Char_Pos <= Node.Non_Grammar (Node.Non_Grammar.Last_Index).Char_Region.Last
         then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         for Child of Node.Children loop
            declare
               Region : constant Buffer_Region := Tree.Char_Region (Child, Include_Non_Grammar);
            begin
               if Region = Null_Buffer_Region then
                  --  Child is empty or virtual; try next
                  null;
               elsif Char_Pos <= Region.Last then
                  return Find_Char_Pos (Tree, Child, Char_Pos, After, Include_Non_Grammar);
               else
                  null; -- try next child
               end if;
            end;
         end loop;
         --  Char_Pos is after last child
         return Invalid_Node_Access;
      end case;
   end Find_Char_Pos;

   function Find_Char_Pos
     (Tree                 : in Syntax_Trees.Tree;
      Char_Pos             : in Buffer_Pos;
      After                : in Boolean;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      Node : constant Node_Access := Root (Tree);
      Char_Region : constant Buffer_Region := Tree.Char_Region (Node, Trailing_Non_Grammar);
   begin
      if After and Char_Pos < Char_Region.First then
         return Tree.First_Terminal (Node);
      elsif Char_Pos > Char_Region.Last then
         return Invalid_Node_Access;
      else
         return Find_Char_Pos (Tree, Node, Char_Pos, After, Trailing_Non_Grammar);
      end if;
   end Find_Char_Pos;

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Invalid_Node_Access;
      when Nonterm =>
         for C of Node.Children loop
            if C /= null then
               if ID = C.ID then
                  return C;
               end if;
            end if;
         end loop;
         return Invalid_Node_Access;
      end case;
   end Find_Child;

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is
      Found : Node_Access := Invalid_Node_Access;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
      is
         pragma Unreferenced (Tree);
      begin
         if Node.ID = ID then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_Descendant
     (Tree      : in     Syntax_Trees.Tree;
      Node      : in     Valid_Node_Access;
      Predicate : access function (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean)
     return Node_Access
   is
      Found : Node_Access := Invalid_Node_Access;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
      is begin
         if Predicate (Tree, Node) then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_New_Line
     (Tree     : in     Syntax_Trees.Tree;
      Line     : in     Line_Number_Type;
      Node     : in     Node_Access;
      Char_Pos :    out Buffer_Pos)
     return Node_Access
   with Pre => Line > Line_Number_Type'First and Tree.Parents_Set,
     Post => Find_New_Line'Result = Invalid_Node_Access or else
             (Find_New_Line'Result.Label in Terminal_Label and then
                (for some Token of Find_New_Line'Result.Non_Grammar =>
                   Token.ID in Tree.Lexer.Descriptor.New_Line_ID | Tree.Lexer.Descriptor.EOI_ID and
                     Token.Line_Region.First = Line - 1))
   --  Return node under Node that contains the non-grammar New_Line or
   --  EOI that ends Line - 1. Update Char_Pos to the position of the
   --  first character on Line (or EOI). If not found, result is
   --  Invalid_Node_Access, Char_Pos is Invalid_Buffer_Pos.
   is begin
      Char_Pos := Invalid_Buffer_Pos;

      if Node = Invalid_Node_Access then
         return Invalid_Node_Access;
      end if;

      case Node.Label is
      when Terminal_Label =>
         if Node.ID = Tree.Lexer.Descriptor.EOI_ID and then
           Node.Non_Grammar (Node.Non_Grammar.First_Index).Line_Region.First = Line - 1
         then
            Char_Pos := Node.Char_Region.First;
            return Node;
         elsif Check_Non_Grammar (Tree, Node, Line, Char_Pos) then
            return Node;
         else
            return Invalid_Node_Access;
         end if;

      when Nonterm =>
         declare
            Node_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
              (Node, Trailing_Non_Grammar => True);
         begin
            if Node.Child_Count = 0 then
               --  This must be an empty stream element.
               return Invalid_Node_Access;

            elsif Line - 1 in Node_Line_Region.First .. Node_Line_Region.Last then
               if Node_Line_Region.First = Node_Line_Region.Last or
                 Line - 1 = Node_Line_Region.Last
               then
                  --  Faster to check last child first.
                  for I in reverse Node.Children'Range loop
                     declare
                        Temp : constant Node_Access := Find_New_Line
                          (Tree, Line, Node.Children (I), Char_Pos);
                     begin
                        if Temp = Invalid_Node_Access then
                           if I = Node.Children'First then
                              return Invalid_Node_Access;
                           else
                              --  Check next child
                              null;
                           end if;
                        else
                           return Temp;
                        end if;
                     end;
                  end loop;
                  return Invalid_Node_Access;
               else
                  for I in Node.Children'Range loop
                     declare
                        Temp : constant Node_Access := Find_New_Line (Tree, Line, Node.Children (I), Char_Pos);
                     begin
                        if Temp = Invalid_Node_Access then
                           if I = Node.Children'Last then
                              return Invalid_Node_Access;
                           else
                              --  Check next child
                              null;
                           end if;
                        else
                           return Temp;
                        end if;
                     end;
                  end loop;
                  return Invalid_Node_Access;
               end if;
            else
               return Invalid_Node_Access;
            end if;
         end;
      end case;
   end Find_New_Line;

   function Find_New_Line
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   is
      Char_Pos : Buffer_Pos;
   begin
      return Find_New_Line (Tree, Line, Tree.Root, Char_Pos);
   end Find_New_Line;

   function Find_New_Line
     (Tree                : in     Syntax_Trees.Tree;
      Line                : in     Line_Number_Type;
      Line_Begin_Char_Pos :    out Buffer_Pos)
     return Node_Access
   is begin
      return Find_New_Line (Tree, Line, Tree.Root, Line_Begin_Char_Pos);
   end Find_New_Line;

   procedure Find_New_Line_1
     (Tree     : in     Syntax_Trees.Tree;
      Ref      : in out Stream_Node_Parents;
      Line     : in     Line_Number_Type;
      Char_Pos :    out Buffer_Pos)
   with Pre => Line > Line_Number_Type'First and Ref.Ref.Element /= Invalid_Stream_Index,
     Post => Ref.Ref.Element = Ref.Ref.Element'Old and
             (Ref.Ref.Node = Invalid_Node_Access or else
                (Ref.Ref.Node.Label in Terminal_Label and then
                   (for some Token of Ref.Ref.Node.Non_Grammar =>
                      Token.ID in Tree.Lexer.Descriptor.New_Line_ID | Tree.Lexer.Descriptor.EOI_ID and
                        Token.Line_Region.First = Line - 1)))
   --  Update Ref to node under Ref.Node in Ref.Stream that ends Line -
   --  1. Set Char_Pos to the position of the first character on Line. If
   --  not found, Ref.Ref.Node is Invalid_Node_Access, Char_Pos is
   --  Invalid_Buffer_Pos.
   is
   begin
      Char_Pos := Invalid_Buffer_Pos;

      if Ref.Ref.Node = Invalid_Node_Access then
         return;
      end if;

      case Ref.Ref.Node.Label is
      when Terminal_Label =>
         if Ref.Ref.Node.ID = Tree.Lexer.Descriptor.EOI_ID and then
           Ref.Ref.Node.Non_Grammar (Ref.Ref.Node.Non_Grammar.First_Index).Line_Region.First = Line - 1
         then
            Char_Pos := Ref.Ref.Node.Char_Region.First;
            return;
         elsif Check_Non_Grammar (Tree, Ref.Ref.Node, Line, Char_Pos) then
            return;
         else
            Ref.Ref.Node := Invalid_Node_Access;
            return;
         end if;

      when Nonterm =>
         if Ref.Ref.Node.Child_Count = 0 then
            --  This must be an empty stream element.
            Ref.Ref.Node := Invalid_Node_Access;
            return;

         else
            declare
               Node_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region
                 (Ref, Trailing_Non_Grammar => True);

               function Check_Child (I : in SAL.Peek_Type) return Boolean
               --  True => return from Find_New_Line; False => check next child.
               is
                  Temp : Stream_Node_Parents :=
                    ((Ref.Ref.Stream, Ref.Ref.Element, Ref.Ref.Node.Children (I)),
                     Ref.Parents);
               begin
                  Temp.Parents.Push (Ref.Ref.Node);
                  Find_New_Line_1 (Tree, Temp, Line, Char_Pos);

                  if Temp.Ref.Node = Invalid_Node_Access then
                     if I = Ref.Ref.Node.Children'First then
                        Ref.Ref.Node := Invalid_Node_Access;
                        return True;
                     else
                        --  Check next child
                        return False;
                     end if;
                  else
                     Ref := Temp;
                     return True;
                  end if;
               end Check_Child;

            begin
               if Contains (Node_Line_Region, Line - 1) then
                  if Node_Line_Region.First = Node_Line_Region.Last or
                    Line - 1 = Node_Line_Region.Last
                  then
                     --  Faster to check last child first.
                     for I in reverse Ref.Ref.Node.Children'Range loop
                        if Check_Child (I) then
                           return;
                        end if;
                     end loop;

                  else
                     for I in Ref.Ref.Node.Children'Range loop
                        if Check_Child (I) then
                           return;
                        end if;
                     end loop;
                  end if;
               end if;
               Ref.Ref.Node := Invalid_Node_Access;
               return;
            end;
         end if;
      end case;
   end Find_New_Line_1;

   procedure Find_New_Line
     (Tree     : in     Syntax_Trees.Tree;
      Ref      : in out Stream_Node_Parents;
      Line     : in     Line_Number_Type;
      Char_Pos :    out Buffer_Pos)
   with Pre => Line > Line_Number_Type'First and Ref.Parents.Is_Empty and
               Ref.Ref.Node = Stream_Element_Lists.Constant_Ref (Ref.Ref.Element.Cur).Node,
     Post => Ref.Ref = Invalid_Stream_Node_Ref or else
             (Ref.Ref.Node.Label in Terminal_Label and then
                (for some Token of Ref.Ref.Node.Non_Grammar => Token.Line_Region.First = Line - 1))

     --  On entry, Ref.Ref should be Stream_First (Ref.Stream). Update Ref
     --  to node in Ref.Stream or Tree.Shared_Stream that ends Line - 1 (or
     --  EOI). Set Char_Pos to the position of the first character on Line
     --  (or EOI). If not found (ie Line < SOI.Line or Line > EOI.Line),
     --  Ref.Ref is Invalid_Stream_Node_Ref, Char_Pos is
     --  Invalid_Buffer_Pos.
   is begin
      loop
         Find_New_Line_1 (Tree, Ref, Line, Char_Pos);
         if Ref.Ref = Invalid_Stream_Node_Ref then
            return;

         elsif Ref.Ref.Node = Invalid_Node_Access then
            Stream_Next (Tree, Ref, Rooted => True);

            if Ref.Ref = Invalid_Stream_Node_Ref then
               if Ref.Ref.Stream /= Tree.Shared_Stream then
                  declare
                     Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Ref.Stream.Cur);
                  begin
                     Ref :=
                       (Ref => (Tree.Shared_Stream, (Cur => Parse_Stream.Shared_Link), Invalid_Node_Access),
                        Parents => <>);
                     First_Terminal (Tree, Ref);
                  end;
               else
                  return;
               end if;
            end if;
         else
            return;
         end if;
      end loop;
   end Find_New_Line;

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is
   begin
      if Node.Parent = Invalid_Node_Access then
         return Invalid_Node_Access;

      else
         case Node.Parent.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Invalid_Node_Access;

         when Nonterm =>
            for C of Node.Parent.Children loop
               if C /= null then
                  if ID = C.ID then
                     return C;
                  end if;
               end if;
            end loop;
            return Invalid_Node_Access;
         end case;
      end if;
   end Find_Sibling;

   function First_Input
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Rooted_Ref
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Cur : constant Cursor := Next (Parse_Stream.Stack_Top);
   begin
      return (Stream, (Cur => Cur), Constant_Ref (Cur).Node);
   end First_Input;

   function First_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := First_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Non_Grammar.Length > 0;
         Next_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end First_Non_Grammar;

   function First_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      Parents : Node_Stacks.Stack;
   begin
      return First_Non_Grammar (Tree, Node, Parents);
   end First_Non_Grammar;

   function First_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean;
      Following            : in Boolean := False)
     return Node_Access
   is
      --  If not Following, we use a Parents stack to limit Next_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access :=
        (if Following
         then First_Terminal (Tree, Node)
         else First_Terminal (Tree, Node, Parents));
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         if Following then
            Next_Terminal (Tree, Result);
         else
            Next_Terminal (Tree, Result, Parents);
         end if;
      end loop;
      return Result;
   end First_Source_Terminal;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Node_Access
   is begin
      return
        (if Item.Virtual
         then Item.First_Terminal
         else First_Terminal (Tree, Item.Element_Node));
   end First_Terminal;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node;
      when Nonterm =>
         for C of Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            declare
               Term : constant Node_Access := First_Terminal (Tree, C);
            begin
               if Term /= Invalid_Node_Access then
                  return Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Access;
      end case;
   end First_Terminal;

   function First_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
   begin
      case Node.Label is
      when Terminal_Label =>
         return Node;

      when Nonterm =>
         for C of Node.Children loop
            --  We tolerate null C here because this function is called while
            --  printing a tree for debug.
            if C /= Invalid_Node_Access then
               Parents.Push (Node);
               declare
                  First_Term : constant Node_Access := First_Terminal (Tree, C, Parents);
               begin
                  if First_Term /= Invalid_Node_Access then
                     return First_Term;
                  else
                     Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to First_Terminal.
                  end if;
               end;
            end if;
         end loop;

         --  All children are empty
         return Invalid_Node_Access;
      end case;
   end First_Terminal;

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is
      use Stream_Element_Lists;
   begin
      Ref.Node := First_Terminal (Tree, Constant_Ref (Ref.Element.Cur).Node);
      loop
         exit when Ref.Node /= Invalid_Node_Access;
         Stream_Next (Tree, Ref, Rooted => False);
         exit when not Has_Element (Ref.Element.Cur);
      end loop;
   end First_Terminal;

   function First_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   is
      Result : Terminal_Ref := Ref;
   begin
      First_Terminal (Tree, Result);
      return Result;
   end First_Terminal;

   function First_Terminal_In_Node
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Terminal_Ref
   is begin
      return
        (Ref.Stream,
         Ref.Element,
         First_Terminal (Tree, Ref.Node));
   end First_Terminal_In_Node;

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   is
      use Stream_Element_Lists;
   begin
      Ref.Ref.Node := First_Terminal (Tree, Constant_Ref (Ref.Ref.Element.Cur).Node, Ref.Parents);
      loop
         exit when Ref.Ref.Node /= Invalid_Node_Access;
         Next_Terminal (Tree, Ref);
         exit when not Has_Element (Ref.Ref.Element.Cur);
      end loop;
   end First_Terminal;

   procedure First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      if Node = Invalid_Node_Access then
         return;
      end if;

      Node := First_Terminal (Tree, Node, Parents);
      loop
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;

         Tree.Next_Terminal (Node, Parents);
      end loop;
   end First_Sequential_Terminal;

   function First_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := Node;
   begin
      First_Sequential_Terminal (Tree, Result, Parents);
      return Result;
   end First_Sequential_Terminal;

   function First_Sequential_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Node_Access)
     return Node_Access
   is
      Parents : Node_Stacks.Stack;
   begin
      return Tree.First_Sequential_Terminal (Node, Parents);
   end First_Sequential_Terminal;

   procedure First_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Parents)
   is begin
      Tree.First_Terminal (Ref);
      loop
         exit when Ref.Ref = Invalid_Stream_Node_Ref;

         exit when Ref.Ref.Node /= Invalid_Node_Access and then
           Tree.Get_Sequential_Index (Ref.Ref.Node) /= Invalid_Sequential_Index;

         Tree.Next_Terminal (Ref);
      end loop;
   end First_Sequential_Terminal;

   procedure Free_Augmented (Tree : in Syntax_Trees.Tree)
   is begin
      for Node of Tree.Nodes loop
         Free (Node.Augmented);
      end loop;
   end Free_Augmented;

   procedure Get_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      ID     : in     Token_ID;
      Result : in out Valid_Node_Access_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      if Node.ID = ID then
         Last := Last + 1;
         Result (Last) := Node;
      end if;
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         null;
      when Nonterm =>
         for I of Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            Get_IDs (Tree, I, ID, Result, Last);
         end loop;
      end case;
   end Get_IDs;

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Valid_Node_Access_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Valid_Node_Access_Array (1 .. Count_IDs (Tree, Node, ID)) := (others => Dummy_Node) do
         Get_IDs (Tree, Node, ID, Result, Last);
      end return;
   end Get_IDs;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Recover_Token
   is begin
      return
        (Virtual      => False,
         Element_Node =>
           (if Ref.Element = Invalid_Stream_Index
            then Invalid_Node_Access
            else Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node),
         Node         => Ref.Node);
   end Get_Recover_Token;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token
   is begin
      --  Used in McKenzie_Recover.Undo_Reduce, so same value as in Tree.Push (Node)
      return
        (Virtual      => False,
         Element_Node => Node,
         Node         => Node);
   end Get_Recover_Token;

   function Get_Sequential_Index (Tree : in Syntax_Trees.Tree; Node : in Node_Access) return Base_Sequential_Index
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Node = Invalid_Node_Access
         then Invalid_Sequential_Index
         else Node.Sequential_Index);
   end Get_Sequential_Index;

   procedure Get_Terminals
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Result : in out Valid_Node_Access_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         Last := Last + 1;
         Result (Last) := Node;

      when Nonterm =>
         for C of Node.Children loop
            --  This is called to build an edited source image while editing the tree
            if C /= null then
               Get_Terminals (Tree, C, Result, Last);
            end if;
         end loop;
      end case;
   end Get_Terminals;

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Valid_Node_Access_Array (1 .. SAL.Base_Peek_Type (Count_Terminals (Tree, Node))) :=
        (others => Dummy_Node)
      do
         Get_Terminals (Tree, Node, Result, Last);
      end return;
   end Get_Terminals;

   procedure Get_Terminal_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Result : in out Token_ID_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      case Node.Label is
      when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
         Last := Last + 1;
         Result (Integer (Last)) := Node.ID;

      when Nonterm =>
         for I of Node.Children loop
            --  Encountering Deleted_Child here is an error in the user algorithm.
            Get_Terminal_IDs (Tree, I, Result, Last);
         end loop;
      end case;
   end Get_Terminal_IDs;

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Token_ID_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminal_IDs (Tree, Node, Result, Last);
      end return;
   end Get_Terminal_IDs;

   function Get_Virtuals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access_Array
   is
      N     : Node_Access        := Tree.First_Terminal (Node);
      Count : SAL.Base_Peek_Type := 0;
      I     : SAL.Base_Peek_Type := 0;
   begin
      loop
         exit when N = Invalid_Node_Access;
         if N.Label in Virtual_Terminal_Label then
            Count := @ + 1;
         end if;
         N := Tree.Next_Terminal (N);
      end loop;

      N := Tree.First_Terminal (Node);
      return Result : Valid_Node_Access_Array (1 .. Count) := (others => Dummy_Node) do
         loop
            exit when N = Invalid_Node_Access;
            if N.Label in Virtual_Terminal_Label then
               I := @ + 1;
               Result (I) := N;
            end if;
            N := Tree.Next_Terminal (N);
         end loop;
      end return;
   end Get_Virtuals;

   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Child : in Valid_Node_Access)
     return Boolean
   is begin
      for C of Node.Children loop
         if C = Child then
            return True;
         end if;
      end loop;
      return False;
   end Has_Child;

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Children'Length > 0;
   end Has_Children;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Access) return Boolean
   is begin
      return Child.Parent /= Invalid_Node_Access;
   end Has_Parent;

   function ID
     (Tree   : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      return
        (if Item.Virtual
         then Item.ID
         elsif Item.Element_Node /= Invalid_Node_Access
         then Item.Element_Node.ID
         else Item.Node.ID);
   end ID;

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Token_ID
   is begin
      return Node.ID;
   end ID;

   function ID
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return WisiToken.Token_ID
   is begin
      if Ref.Node /= Invalid_Node_Access then
         return Ref.Node.ID;
      elsif Ref.Element /= Invalid_Stream_Index then
         return Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node.ID;
      else
         return Invalid_Token_ID;
      end if;
   end ID;

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Base_Identifier_Index
   is begin
      return Node.Identifier;
   end Identifier;

   function Image
     (Tree : in Syntax_Trees.Tree;
      Item : in Recover_Token)
     return String
   is begin
      if Item.Virtual then
         return "(" & Image (Item.ID, Tree.Lexer.Descriptor.all) &
           (if Item.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Item.Byte_Region)) & ")";
      else
         return "(" & Image (Tree, Item.Element_Node, Node_Numbers => True) &
           (if Item.Element_Node.Node_Index = Item.Node.Node_Index
            then ""
            else ", " & Image (Tree, Item.Node, Terminal_Node_Numbers => True)) & ")";
      end if;
   end Image;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Stream       : in Parse_Stream;
      Stack        : in Boolean                   := True;
      Input        : in Boolean                   := True;
      Shared       : in Boolean                   := False;
      Children     : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String
   is
      use Ada.Strings.Unbounded;
      use Stream_Element_Lists;
      Result     : Unbounded_String := +"(" & Trimmed_Image (Stream.Label) & ", ";
      Element    : Cursor           :=
        (if Stack or Stream.Stack_Top = No_Element
         then Stream.Elements.First
         else Next (Stream.Stack_Top));
      Need_Comma : Boolean          := False;
      Current_Stream_Label : Stream_Label := Stream.Label;
      In_Stack : Boolean := Stream.Stack_Top /= No_Element;
   begin
      loop
         if not Has_Element (Element) then
            if Shared and Current_Stream_Label /= Shared_Stream_Label then
               if not Has_Element (Stream.Shared_Link) then
                  exit;
               else
                  Current_Stream_Label := Shared_Stream_Label;
                  Element              := Stream.Shared_Link;

                  In_Stack := False;
                  Result   := @ & "/";
               end if;
            else
               exit;
            end if;
         end if;

         if Need_Comma then
            Result := @ & (if Children then "," & ASCII.LF else ", ");
         else
            Need_Comma := True;
         end if;

         Result := @ &
           (if Stream.Stack_Top = Element then "^(" else "(") &
           Trimmed_Image (Constant_Ref (Element).State) & ", " &
           (if not In_Stack and Children and Constant_Ref (Element).Node.Label = Nonterm
            then Tree.Subtree_Image
              (Constant_Ref (Element).Node,
               Non_Grammar  => Non_Grammar,
               Augmented    => Augmented,
               Line_Numbers => Line_Numbers,
               Image_Action => Image_Action)
            else Tree.Image
              (Constant_Ref (Element).Node,
               Children              => False,
               RHS_Index             => False,
               Node_Numbers          => True,
               Terminal_Node_Numbers => False,
               Non_Grammar           => Non_Grammar,
               Augmented             => Augmented,
               Line_Numbers          => Line_Numbers,
               Image_Action          => Image_Action))
           & ")";

         if not Input then
            exit when Element = Stream.Stack_Top;
         elsif Element = Stream.Stack_Top then
            In_Stack := False;
            Result := @ & "/";
         end if;

         Element := Next (Element);
      end loop;
      Result := @ & ")";
      return -Result;
   end Image;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Children     : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Root         : in Node_Access               := Invalid_Node_Access;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String
   is begin
      if Root /= Invalid_Node_Access then
         --  Assuming children = true in this case.
         return Subtree_Image (Tree, Root, Non_Grammar, Augmented, Line_Numbers);

      elsif Tree.Streams.Length = 0 then
         if Tree.Root = Invalid_Node_Access then
            return "invalid_tree: no streams, Tree.Root not set";
         else
            --  Assuming children = true in this case.
            return Subtree_Image
              (Tree, Tree.Root,
               Non_Grammar  => Non_Grammar,
               Augmented    => Augmented,
               Line_Numbers => Line_Numbers,
               Image_Action => Image_Action);
         end if;
      else
         declare
            use Ada.Strings.Unbounded;
            Result : Unbounded_String;
            Need_New_Line : Boolean := False;
         begin
            for Stream of Tree.Streams loop
               if Need_New_Line then
                  Result := @ & ASCII.LF;
               else
                  Need_New_Line := True;
               end if;
               Result := @ & Image
                 (Tree, Stream, Children,
                  Non_Grammar  => Non_Grammar,
                  Augmented    => Augmented,
                  Line_Numbers => Line_Numbers,
                  Image_Action => Image_Action);
            end loop;
            return -Result;
         end;
      end if;
   end Image;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Stream       : in Stream_ID;
      Stack        : in Boolean                   := True;
      Input        : in Boolean                   := True;
      Shared       : in Boolean                   := False;
      Children     : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String
   is begin
      return Image
        (Tree, Tree.Streams (Stream.Cur), Stack, Input, Shared,
         Children, Non_Grammar, Augmented, Line_Numbers, Image_Action);
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      Children              : in Boolean                   := False;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String
   is begin
      if Element.Cur = Stream_Element_Lists.No_Element then
         return "<deleted>";
      else
         return Image
           (Tree, Stream_Element_Lists.Constant_Ref (Element.Cur).Node, Children,
            RHS_Index, Node_Numbers, Terminal_Node_Numbers,
            Line_Numbers => Line_Numbers,
            Non_Grammar  => Non_Grammar,
            Augmented    => Augmented,
            Image_Action => Image_Action);
      end if;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Node                  : in Node_Access;
      Children              : in Boolean                   := False;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String
   is
      use Ada.Strings.Unbounded;
   begin
      if Node = null then
         return "<deleted>";
      else
         declare
            Result : Unbounded_String :=
              +(if Terminal_Node_Numbers
                then
                  (case Node.Label is
                   when Source_Terminal    => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Terminal   => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Identifier => Trimmed_Image (Node.Identifier) & ":",
                   when Nonterm            => "")
                elsif Node_Numbers
                then
                  (if Node.Label in Terminal_Label and then Node.Sequential_Index /= Invalid_Sequential_Index
                   then Trimmed_Image (Node.Sequential_Index) & ";"
                   else Trimmed_Image (Node.Node_Index) & ":")
                else "");

            Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node);
         begin
            Result := @ & "(" & Image (Node.ID, Tree.Lexer.Descriptor.all);
            Result := @ & (if RHS_Index and Node.Label = Nonterm then "_" & Trimmed_Image (Node.RHS_Index) else "");

            if Node_Byte_Region /= Null_Buffer_Region then
               Result := @ & ", " & Image (Node_Byte_Region);
            end if;

            Result := @ &
              (if (Line_Numbers and Tree.Editable) and then Tree.Line_Region (Node) /= Null_Line_Region
               then ", " & Image (Tree.Line_Region (Node))
               else "") & ")";

            if Children and Node.Label = Nonterm then
               Result := @ & " <= " & Image
                 (Tree, Node.Children, RHS_Index, Node_Numbers, Terminal_Node_Numbers, Non_Grammar, Augmented);
            end if;

            if (Non_Grammar and Node.Label in Terminal_Label) and then Node.Non_Grammar.Length > 0 then
               Result := @ & "(";
               for Token of Node.Non_Grammar loop
                  Result := @ & "(";
                  Result := @ & Image (Token.ID, Tree.Lexer.Descriptor.all) &
                    (if Token.ID = Tree.Lexer.Descriptor.New_Line_ID or
                       Token.ID = Tree.Lexer.Descriptor.SOI_ID or
                       Token.ID = Tree.Lexer.Descriptor.EOI_ID
                     then ", " & Image (Token.Line_Region)
                     elsif Token.Char_Region = Null_Buffer_Region
                     then ""
                     else ", " & Image (Token.Char_Region));
                  Result := @ & ")";
               end loop;
               Result := @ & ")";
            end if;

            if Node.Augmented /= null and Augmented then
               Result := @ & Image_Augmented (Node.Augmented.all);
            end if;

            if Image_Action /= null and then Node.Label = Nonterm and then Node.Action /= null then
               Result := @ & Image_Action (Node.Action);
            end if;

            return -Result;
         end;
      end if;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Nodes                 : in Node_Access_Array;
      RHS_Index             : in Boolean                   := False;
      Node_Numbers          : in Boolean                   := False;
      Terminal_Node_Numbers : in Boolean                   := False;
      Line_Numbers          : in Boolean                   := False;
      Non_Grammar           : in Boolean                   := False;
      Augmented             : in Boolean                   := False;
      Image_Action          : in Syntax_Trees.Image_Action := null)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           (if Nodes (I) = null then " - "
            else Tree.Image
              (Nodes (I),
               Node_Numbers          => Node_Numbers,
               Terminal_Node_Numbers => Terminal_Node_Numbers,
               Line_Numbers          => Line_Numbers,
               Non_Grammar           => Non_Grammar,
               Image_Action          => Image_Action));
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Tree           : in Syntax_Trees.Tree;
      Ref            : in Stream_Node_Ref;
      First_Terminal : in Boolean                   := False;
      Line_Numbers   : in Boolean                   := False;
      Non_Grammar    : in Boolean                   := False;
      Augmented      : in Boolean                   := False;
      Image_Action   : in Syntax_Trees.Image_Action := null)
     return String
   is
      use Stream_Element_Lists;
   begin
      if Ref.Element.Cur /= No_Element then
         declare
            Element_Node : constant Valid_Node_Access := Constant_Ref (Ref.Element.Cur).Node;
         begin
            return "(" & Trimmed_Image (Tree.Streams (Ref.Stream.Cur).Label) & ", " &
              Image
                (Tree, Ref.Element,
                 Node_Numbers => True,
                 Line_Numbers => Line_Numbers,
                 Non_Grammar  => Non_Grammar,
                 Augmented    => Augmented,
                 Image_Action => Image_Action) &
              (if Ref.Node = Invalid_Node_Access or Element_Node.Label in Terminal_Label
               then ""
               elsif Element_Node.Label = Nonterm and Element_Node = Ref.Node and First_Terminal
               then ", " & Image
                 (Tree,
                  Tree.First_Terminal (Ref.Node),
                  Terminal_Node_Numbers => True,
                  Line_Numbers          => Line_Numbers,
                  Non_Grammar           => Non_Grammar,
                  Augmented             => Augmented,
                  Image_Action          => Image_Action)

               else ", " & Image
                 (Tree,
                  Ref.Node,
                  Terminal_Node_Numbers => True,
                  Line_Numbers          => Line_Numbers,
                  Non_Grammar           => Non_Grammar,
                  Augmented             => Augmented,
                  Image_Action          => Image_Action)) & ")";
         end;
      elsif Ref.Node /= Invalid_Node_Access then
         return "(" & Image
           (Tree, Ref.Node,
            Terminal_Node_Numbers => True,
            Line_Numbers          => Line_Numbers,
            Non_Grammar           => Non_Grammar) & ")";
      else
         return "()";
      end if;
   end Image;

   function Insert_After
     (User_Data           : in out User_Data_Type;
      Tree                : in     Syntax_Trees.Tree'Class;
      Insert_Token        : in     Valid_Node_Access;
      Insert_Before_Token : in     Valid_Node_Access;
      Comment_Present     : in     Boolean;
      Blank_Line_Present  : in     Boolean)
     return Insert_Location
   is
      pragma Unreferenced (User_Data, Tree, Insert_Token, Insert_Before_Token, Comment_Present, Blank_Line_Present);
   begin
      return Before_Next;
   end Insert_After;

   function Insert_Source_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Lexer.Token;
      Before   : in     Stream_Index)
     return Single_Terminal_Ref
   is
      New_Node : constant Valid_Node_Access := Add_Source_Terminal_1
        (Tree, Terminal,
         In_Shared_Stream => Stream = Tree.Shared_Stream);
   begin
      return Insert_Stream_Element (Tree, Stream, New_Node, Before => Before.Cur);
   end Insert_Source_Terminal;

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Rooted_Ref
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Cursor := Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Node,
            State => Unknown_State,
            Label => Parse_Stream.Label),
         Before   =>
           (if Before /= No_Element
            then Before
            else
              (if Parse_Stream.Stack_Top = No_Element
               then No_Element
               else Next (Parse_Stream.Stack_Top))));
   begin
      return (Stream, (Cur => New_Element), Node);
   end Insert_Stream_Element;

   procedure Insert_Token
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Rooted_Ref)
   is begin
      Ref := Insert_Stream_Element (Tree, Stream, Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node);
   end Insert_Token;

   function Insert_Virtual_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID)
     return Single_Terminal_Ref
   is
      New_Node : constant Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => -(Tree.Nodes.Last_Index + 1),
         others      => <>);
   begin
      Tree.Nodes.Append (New_Node);
      return Insert_Stream_Element (Tree, Stream, New_Node);
   end Insert_Virtual_Terminal;

   function Is_Descendant_Of
     (Tree       : in Syntax_Trees.Tree;
      Root       : in Valid_Node_Access;
      Descendant : in Valid_Node_Access)
     return Boolean
   is
      Node : Node_Access := Descendant;
   begin
      loop
         exit when Node = Invalid_Node_Access;
         if Node = Root then
            return True;
         end if;

         Node := Tree.Parent (Node);
      end loop;
      return False;
   end Is_Descendant_Of;

   function Is_Empty_Nonterm
     (Tree       : in Syntax_Trees.Tree;
      Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor)
     return Boolean
   is begin
      return
        (case Item.Virtual is
         when True => Is_Nonterminal (Item.ID, Descriptor) and Item.First_Terminal = Invalid_Node_Access,
         when False => Item.Node /= Invalid_Node_Access and then Tree.Is_Empty_Nonterm (Item.Node));
   end Is_Empty_Nonterm;

   function Is_Empty_Nonterm
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Boolean
   is begin
      return Node.Label = Nonterm and then Tree.First_Terminal (Node) = Invalid_Node_Access;
   end Is_Empty_Nonterm;

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Nonterm;
   end Is_Nonterm;

   function Is_Source_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Source_Terminal;
   end Is_Source_Terminal;

   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Virtual_Terminal;
   end Is_Virtual_Terminal;

   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label in Virtual_Terminal | Virtual_Identifier or (Node.Label = Nonterm and then Node.Virtual);
   end Is_Virtual;

   function Is_Virtual
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Boolean
   is begin
      return Is_Virtual (Tree, Tree.Streams (Stream.Cur).Elements (Element.Cur).Node);
   end Is_Virtual;

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Virtual_Identifier;
   end Is_Virtual_Identifier;

   function Last_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      --  We always use a Parents stack, to limit Prev_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access := Last_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Non_Grammar.Length > 0;
         Prev_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end Last_Non_Grammar;

   function Last_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Result : Node_Access := Tree.Last_Terminal (Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when Result.Sequential_Index /= Invalid_Sequential_Index;
         Tree.Prev_Terminal (Result, Parents);
      end loop;
      return Result;
   end Last_Sequential_Terminal;

   function Last_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      --  We always use a Parents stack, to limit Prev_Terminal to
      --  descendants of Node.
      Parents : Node_Stacks.Stack;
      Result  : Node_Access := Last_Terminal (Tree, Node, Parents);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal => Result.Non_Grammar.Length > 0,
                  when Virtual_Identifier => Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         Prev_Terminal (Tree, Result, Parents);
      end loop;
      return Result;
   end Last_Source_Terminal;

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is begin
      case Node.Label is
      when Terminal_Label =>
         return Node;
      when Nonterm =>
         for C of reverse Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            declare
               Term : constant Node_Access := Last_Terminal (Tree, C);
            begin
               if Term /= Invalid_Node_Access then
                  return Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Access;
      end case;
   end Last_Terminal;

   function Last_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in     Valid_Node_Access;
      Parents : in out Node_Stacks.Stack)
     return Node_Access
   is
      Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
   begin
      case Node.Label is
      when Terminal_Label =>
         return Node;

      when Nonterm =>
         for C of reverse Node.Children loop
            --  We tolerate null C here because this function is called while
            --  printing a tree for debug.
            if C /= Invalid_Node_Access then
               Parents.Push (Node);
               declare
                  Last_Term : constant Node_Access := Last_Terminal (Tree, C, Parents);
               begin
                  if Last_Term /= Invalid_Node_Access then
                     return Last_Term;
                  else
                     Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to Last_Terminal.
                  end if;
               end;
            end if;
         end loop;
         --  All children are empty
         return Invalid_Node_Access;
      end case;
   end Last_Terminal;

   procedure Last_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   is
      use Stream_Element_Lists;
   begin
      Ref.Ref.Node := Last_Terminal (Tree, Constant_Ref (Ref.Ref.Element.Cur).Node, Ref.Parents);
      loop
         exit when Ref.Ref.Node /= Invalid_Node_Access;
         Prev_Terminal (Tree, Ref);
         exit when not Has_Element (Ref.Ref.Element.Cur);
      end loop;
   end Last_Terminal;

   procedure Left_Breakdown
     (Tree : in out Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is
      --  [Wagner Graham 1998] doesn't modify the tree structure for
      --  Left_Breakdown; it just moves the Current_Token pointer around.
      --  That means the rest of the parser must understand that.
      --
      --  Here we actually decompose the tree, as in [Lahav 2008]. Empty
      --  nonterms are handled by caller.
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Stream.Cur);

      Cur       : Cursor            := Ref.Element.Cur;
      To_Delete : Cursor            := Cur;
      Node      : Valid_Node_Access := Parse_Stream.Elements (Ref.Element.Cur).Node;
      Next_I    : Positive_Index_Type;
   begin
      loop
         Next_I := Positive_Index_Type'Last;

         for I in reverse 2 .. Node.Child_Count loop
            if Node.Children (I).Child_Count > 0 or Node.Children (I).Label in Terminal_Label then
               Next_I := I;
            end if;

            Cur := Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => Node.Children (I),
                  State => Unknown_State,
                  Label => Parse_Stream.Label),
               Before   => Cur);

            Node.Children (I).Parent := Invalid_Node_Access;
         end loop;

         Node.Children (1).Parent := Invalid_Node_Access;

         if Node.Children (1).Child_Count > 0 or Node.Children (1).Label in Terminal_Label then
            Next_I := 1;
         else
            --  Node is an empty nonterm. First non_empty is in Node.Children (Next_I);
            if Next_I > 2 then
               --  Delete other empty nonterms that were added to the stream.
               for I in 2 .. Next_I - 1 loop
                  declare
                     To_Delete : Cursor := Cur;
                  begin
                     Next (Cur);
                     Parse_Stream.Elements.Delete (To_Delete);
                  end;
               end loop;
            end if;
            pragma Assert (Element (Cur).Node = Node.Children (Next_I));
         end if;

         Node := Node.Children (Next_I);

         if Node.Label in Terminal_Label then
            if Next_I = 1 then
               Ref.Element.Cur := Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Node,
                     State => Unknown_State,
                     Label => Parse_Stream.Label),
                  Before   => Cur);
            else
               Ref.Element.Cur := Cur;
            end if;

            Ref.Node := Node;

            Parse_Stream.Elements.Delete (To_Delete);
            exit;
         end if;
      end loop;
   end Left_Breakdown;

   function Line_At_Byte_Pos
     (Tree     : in Syntax_Trees.Tree;
      Byte_Pos : in Buffer_Pos)
     return Line_Number_Type
   is
      function Line_At_Byte_Pos (Node : in Valid_Node_Access) return Line_Number_Type
      is begin
         case Node.Label is
         when Terminal_Label =>
            --  Byte_Pos can be in whitespace or non_grammar, so we construct the
            --  region to check from two successive terminals.
            declare
               Next_Source_Terminal : constant Node_Access :=
                 (if Node = Tree.EOI
                  then Node
                  else Tree.Next_Source_Terminal (Node, Trailing_Non_Grammar => True));

               Prev_Source_Terminal : constant Node_Access :=
                 (if Node = Tree.SOI
                  then Node
                  else Tree.Prev_Source_Terminal (Node, Trailing_Non_Grammar => True));

               Check_Region : constant Buffer_Region :=
                 (First =>
                    (case Terminal_Label'(Node.Label) is
                     when Source_Terminal => Node.Byte_Region.First,
                     when Virtual_Terminal | Virtual_Identifier =>
                       (if Node.Non_Grammar.Length > 0
                        then Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First
                        elsif Prev_Source_Terminal.Label = Source_Terminal
                        then Prev_Source_Terminal.Byte_Region.Last
                        else Prev_Source_Terminal.Non_Grammar (Prev_Source_Terminal.Non_Grammar.Last_Index)
                          .Byte_Region.Last)),
                  Last  =>
                    (if Node = Tree.EOI
                     then Node.Byte_Region.First
                     elsif Next_Source_Terminal.Label = Source_Terminal
                     then Next_Source_Terminal.Byte_Region.First - 1
                     else Next_Source_Terminal.Non_Grammar (Next_Source_Terminal.Non_Grammar.First_Index)
                       .Byte_Region.First));
            begin
               if Node.Label = Source_Terminal and then Contains (Node.Byte_Region, Byte_Pos) then
                  if Node.Non_Grammar.Length > 0 then
                     return Node.Non_Grammar (Node.Non_Grammar.First_Index).Line_Region.First;
                  else
                     declare
                        Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Next_Non_Grammar (Node).Non_Grammar;
                     begin
                        return Non_Grammar (Non_Grammar.First_Index).Line_Region.First;
                     end;
                  end if;

               elsif Contains (Check_Region, Byte_Pos) then
                  if Node.Non_Grammar.Length > 0 then
                     if Byte_Pos <= Node.Non_Grammar (Node.Non_Grammar.First_Index).Byte_Region.First then
                        return Node.Non_Grammar (Node.Non_Grammar.First_Index).Line_Region.First;

                     elsif Byte_Pos > Node.Non_Grammar (Node.Non_Grammar.Last_Index).Byte_Region.Last then
                        declare
                           Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Next_Non_Grammar (Node).Non_Grammar;
                        begin
                           return Non_Grammar (Non_Grammar.First_Index).Line_Region.First;
                        end;
                     end if;

                     for I in Node.Non_Grammar.First_Index .. Node.Non_Grammar.Last_Index - 1 loop
                        if Node.Non_Grammar (I).ID = Tree.Lexer.Descriptor.New_Line_ID then
                           if Byte_Pos = Node.Non_Grammar (I).Byte_Region.First then
                              return Node.Non_Grammar (I).Line_Region.First;
                           elsif Byte_Pos < Node.Non_Grammar (I + 1).Byte_Region.First then
                              return Node.Non_Grammar (I).Line_Region.Last;
                           end if;

                        else
                           if Byte_Pos >= Node.Non_Grammar (I).Byte_Region.First and
                             Byte_Pos < Node.Non_Grammar (I + 1).Byte_Region.First
                           then
                              return Node.Non_Grammar (I).Line_Region.First;
                           end if;
                        end if;
                     end loop;

                     declare
                        Token : Lexer.Token renames Node.Non_Grammar (Node.Non_Grammar.Last_Index);
                     begin
                        pragma Assert (Byte_Pos = Token.Byte_Region.Last);
                        if Token.ID = Tree.Lexer.Descriptor.New_Line_ID then
                           return Token.Line_Region.First;
                        else
                           return Token.Line_Region.Last;
                        end if;
                     end;

                  else
                     declare
                        Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Next_Non_Grammar (Node).Non_Grammar;
                     begin
                        return Non_Grammar (Non_Grammar.First_Index).Line_Region.First;
                     end;
                  end if;
               else
                  return Invalid_Line_Number;
               end if;
            end;

         when Nonterm =>
            --  FIXME: check if start with last child is better? do binary search?
            for Child of Node.Children loop
               declare
                  First : constant Node_Access := First_Source_Terminal (Tree, Child, Trailing_Non_Grammar => True);
                  Next  : constant Valid_Node_Access :=
                    (if Child = Tree.EOI
                     then Tree.EOI
                     else Next_Source_Terminal (Tree, Child, Trailing_Non_Grammar  => True));
               begin
                  if First = Invalid_Node_Access then
                     --  Empty nonterm; check next child
                     null;
                  else
                     declare
                        Check_Region : constant Buffer_Region :=
                          (First => Byte_Region (Tree, First, Trailing_Non_Grammar => True).First,
                           Last  => Byte_Region (Tree, Next, Trailing_Non_Grammar => True).First);
                     begin
                        if Contains (Check_Region, Byte_Pos) then
                           declare
                              Temp : constant Line_Number_Type := Line_At_Byte_Pos (Child);
                           begin
                              if Temp = Invalid_Line_Number then
                                 --  Check next child
                                 null;
                              else
                                 return Temp;
                              end if;
                           end;
                        else
                           --  Check next child
                           null;
                        end if;
                     end;
                  end if;
               end;
            end loop;
            return Invalid_Line_Number;
         end case;
      end Line_At_Byte_Pos;
   begin
      return Line_At_Byte_Pos (Tree.Root);
   end Line_At_Byte_Pos;

   function Line_Begin_Char_Pos
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Buffer_Pos
   is
      Node           : Node_Access := Tree.Root;
      Begin_Char_Pos : Buffer_Pos  := Invalid_Buffer_Pos;
   begin
      if Line = Line_Number_Type'First then
         return Buffer_Pos'First;
      end if;

      Node := Find_New_Line (Tree, Line, Node, Begin_Char_Pos);
      return Begin_Char_Pos;
   end Line_Begin_Char_Pos;

   function Line_Begin_Char_Pos
     (Tree   : in Syntax_Trees.Tree;
      Line   : in Line_Number_Type;
      Stream : in Stream_ID)
     return Buffer_Pos
   is
      Begin_Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
   begin
      if Line = Line_Number_Type'First then
         return Buffer_Pos'First;
      end if;

      declare
         Ref : Stream_Node_Parents;
      begin
         Ref.Ref := Tree.Stream_First (Stream);
         Find_New_Line (Tree, Ref, Line, Begin_Char_Pos);
         if Ref.Ref.Node = Invalid_Node_Access then
            if Stream /= Tree.Shared_Stream then
               return Line_Begin_Char_Pos (Tree, Line, Tree.Shared_Stream);
            else
               return Invalid_Buffer_Pos;
            end if;
         else
            return Begin_Char_Pos;
         end if;
      end;
   end Line_Begin_Char_Pos;

   function Line_Begin_Token
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type)
     return Node_Access
   is begin
      declare
         Node : constant Node_Access := Tree.First_Non_Grammar (Root (Tree));
      begin
         if Node = Invalid_Node_Access then
            --  Tree has no tokens with a Line_Region. Note that for LR parse, EOI
            --  is not in the tree, only in the parse stream.
            return Invalid_Node_Access;
         end if;

         if Line = Tree.Line_Region (Node).First then
            return Node;
         elsif Line < Tree.Line_Region (Node).First then
            return Invalid_Node_Access;
         end if;
      end;

      declare
         Begin_Char_Pos : Buffer_Pos;
         Node           : Node_Access := Find_New_Line (Tree, Line, Root (Tree), Begin_Char_Pos);
      begin
         if Node = Invalid_Node_Access then
            return Invalid_Node_Access;

         else
            --  Node now contains the non-grammar that ends Line - 1
            if Empty_Line (Tree, Node.Non_Grammar, Line) then
               return Invalid_Node_Access;
            else
               Next_Terminal (Tree, Node);
               return Node;
            end if;
         end if;
      end;
   end Line_Begin_Token;

   function Line_Begin_Token
     (Tree                      : in Syntax_Trees.Tree;
      Line                      : in Line_Number_Type;
      Stream                    : in Stream_ID;
      Following_Source_Terminal : in Boolean)
     return Node_Access
   is
      Ref             : Stream_Node_Parents;
      Begin_Char_Pos  : Buffer_Pos;

      EOI_Line : constant Line_Number_Type := Tree.EOI.Non_Grammar (Tree.EOI.Non_Grammar.First).Line_Region.First;
   begin
      Ref.Ref := Stream_First (Tree, Stream);

      if Line = Line_Number_Type'First then
         if Line = Tree.Line_Region (Ref).First then
            return Tree.First_Terminal (Ref.Ref.Node);
         else
            if Following_Source_Terminal then
               Next_Source_Terminal (Tree, Ref);
               return Ref.Ref.Node;
            else
               return Invalid_Node_Access;
            end if;
         end if;

      elsif Line = EOI_Line + 1 then
         return Tree.EOI;

      elsif Line > EOI_Line + 1 then
         return Invalid_Node_Access;
      end if;

      Find_New_Line (Tree, Ref, Line, Begin_Char_Pos);

      if Ref.Ref = Invalid_Stream_Node_Ref then
         return Invalid_Node_Access;
      else
         --  Ref now contains the non-grammar that ends Line - 1

         if Empty_Line (Tree, Ref.Ref.Node.Non_Grammar, Line) then
            if Following_Source_Terminal then
               Next_Source_Terminal (Tree, Ref);
               return Ref.Ref.Node;
            else
               return Invalid_Node_Access;
            end if;
         else
            Next_Terminal (Tree, Ref);
            return Ref.Ref.Node;
         end if;
      end if;
   end Line_Begin_Token;

   function Line_Region_Internal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Node_Access;
      Prev_Non_Grammar     : in Valid_Node_Access;
      Next_Non_Grammar     : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return WisiToken.Line_Region
   is
      --  Since all non_grammar have line_region, we don't have to look for
      --  a new_line, just any non_grammar.
      --
      --  We always have to find a previous and next non_grammar, to allow
      --  for multi-line tokens.
      --
      --  The last few tokens in a nonterm may have no non_grammar; then we
      --  have to find the following Non_Grammar.

      Last_Non_Grammar : constant Syntax_Trees.Node_Access :=
        (if Node = Invalid_Node_Access
         then Invalid_Node_Access
         else Tree.Last_Non_Grammar (Node));

      Last_Terminal : constant Syntax_Trees.Node_Access :=
        (if Node = Invalid_Node_Access
         then Invalid_Node_Access
         else Tree.Last_Terminal (Node));

      Actual_Last_Non_Grammar : constant Syntax_Trees.Valid_Node_Access :=
        (if Last_Non_Grammar = Invalid_Node_Access
         then Next_Non_Grammar
         elsif Last_Non_Grammar = Last_Terminal
         then Last_Non_Grammar
         else Next_Non_Grammar);
   begin
      return Result : constant WisiToken.Line_Region :=
        (First =>
           (if (Node = Tree.Root and Prev_Non_Grammar = Tree.SOI)
               --  We are finding the line_region of wisi_accept in an Editable
               --  tree; we want to include the leading non_grammar in SOI.
              or Node = Tree.SOI
              --  We are finding the line_region of SOI.
            then
               Prev_Non_Grammar.Non_Grammar (Prev_Non_Grammar.Non_Grammar.First_Index).Line_Region.First
            else
               --  We are finding the line_region of a leading non_terminal; we don't
               --  want to include the leading non_grammar in SOI.
               Prev_Non_Grammar.Non_Grammar (Prev_Non_Grammar.Non_Grammar.Last_Index).Line_Region.Last),

         Last => Actual_Last_Non_Grammar.Non_Grammar
           (if Trailing_Non_Grammar
            then Actual_Last_Non_Grammar.Non_Grammar.Last_Index
            else Actual_Last_Non_Grammar.Non_Grammar.First_Index).Line_Region.First);
   end Line_Region_Internal;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   is begin
      return Line_Region_Internal
        (Tree, Node,
         Prev_Non_Grammar     => Tree.Prev_Non_Grammar (Node),
         Next_Non_Grammar     => Tree.Next_Non_Grammar (Node),
         Trailing_Non_Grammar => Trailing_Non_Grammar);
   end Line_Region;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Ref;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   is begin
      if Tree.Parents_Set then
         declare
            Prev_Non_Grammar : Stream_Node_Ref := Ref;
            Next_Non_Grammar : Stream_Node_Ref := Ref;
         begin
            Tree.Prev_Non_Grammar (Prev_Non_Grammar);
            Tree.Next_Non_Grammar (Next_Non_Grammar);
            return Line_Region_Internal
              (Tree, Ref.Node, Prev_Non_Grammar.Node, Next_Non_Grammar.Node, Trailing_Non_Grammar);
         end;

      else
         return Line_Region (Tree, To_Stream_Node_Parents (Tree, Ref), Trailing_Non_Grammar);
      end if;
   end Line_Region;

   function Line_Region
     (Tree                 : in Syntax_Trees.Tree;
      Ref                  : in Stream_Node_Parents;
      Trailing_Non_Grammar : in Boolean := True)
     return WisiToken.Line_Region
   is
      Prev_Non_Grammar : Stream_Node_Parents := Ref;
      Next_Non_Grammar : Stream_Node_Parents := Ref;
   begin
      Tree.Prev_Non_Grammar (Prev_Non_Grammar);
      Tree.Next_Non_Grammar (Next_Non_Grammar);
      return Line_Region_Internal
        (Tree, Ref.Ref.Node, Prev_Non_Grammar.Ref.Node, Next_Non_Grammar.Ref.Node, Trailing_Non_Grammar);
   end Line_Region;

   procedure Move_Shared_To_Input
     (Tree   : in out Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Stream : in     Stream_ID)
   is
      Junk : constant Stream_Node_Ref := Insert_Stream_Element
        (Tree, Stream, Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node);
      pragma Unreferenced (Junk);
   begin
      Tree.Stream_Next (Ref, Rooted => True);
      Tree.Set_Shared_Link (Stream, Ref);
   end Move_Shared_To_Input;

   function Name (Tree : in Syntax_Trees.Tree; Item : in Recover_Token) return Buffer_Region
   is begin
      if Item.Virtual then
         if Item.Name = Null_Buffer_Region then
            return Item.Byte_Region;
         else
            return Item.Name;
         end if;
      else
         return Tree.Name (Item.Element_Node);
      end if;
   end Name;

   function Name (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Buffer_Region
   is begin
      case Node.Label is
      when Nonterm =>
         if Node.Name_Length = 0 then
            return Tree.Byte_Region (Node);
         else
            declare
               First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
               Byte_First     : constant Buffer_Pos  := Tree.Byte_Region (First_Terminal).First;
            begin
               return
                 (Byte_First + Node.Name_Offset,
                  Byte_First + Node.Name_Offset + Node.Name_Length - 1);
            end;
         end if;

      when Terminal_Label =>
         return Tree.Byte_Region (Node);
      end case;
   end Name;

   function Name (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Buffer_Region
   is begin
      --  We use the Element node because the nonterminal has the most valid Name.
      return Tree.Name (Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node);
   end Name;

   function Next_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Valid_Node_Access
   is
      Result : Node_Access := Node;
   begin
      if Node = Tree.EOI or Node = Tree.Root then
         return Tree.EOI;
      end if;

      loop
         Result := Next_Terminal (Tree, Result);
         exit when Result.Non_Grammar.Length > 0;
      end loop;
      return Result;
   end Next_Non_Grammar;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Ref)
   is begin
      if Ref.Node = Tree.EOI then
         return;
      end if;
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Node = Invalid_Node_Access;
         exit when Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Next_Non_Grammar;

   procedure Next_Non_Grammar
     (Tree    : in     Syntax_Trees.Tree;
      Ref     : in out Stream_Node_Parents)
   is begin
      if Ref.Ref.Node = Tree.EOI then
         return;
      end if;
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Next_Non_Grammar;

   procedure Next_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      loop
         Next_Terminal (Tree, Node, Parents);
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Next_Sequential_Terminal;

   procedure Next_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Parents)
   is begin
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Next_Sequential_Terminal;

   function Next_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      Result : Node_Access := Next_Terminal (Tree, Node);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         Next_Terminal (Tree, Result);
      end loop;
      return Result;
   end Next_Source_Terminal;

   procedure Next_Source_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   is begin
      loop
         Next_Terminal (Tree, Ref);
         exit when Ref.Ref = Invalid_Stream_Node_Ref;
         exit when Ref.Ref.Node.Label = Source_Terminal;
      end loop;
   end Next_Source_Terminal;

   procedure Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access)
   is begin
      Node := Next_Terminal (Tree, Node);
   end Next_Terminal;

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is
      pragma Unreferenced (Tree);

      function First_Child (Node : in Valid_Node_Access) return Node_Access
      is
      begin
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
            for J of Node.Children loop
               --  We tolerate deleted children here for edited trees.
               if J /= Invalid_Node_Access then
                  declare
                     Result : constant Node_Access := First_Child (J);
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     end if;
                  end;
               end if;
            end loop;
            --  All Children are empty
            return Invalid_Node_Access;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Access; Parent : in Node_Access) return Node_Access
      is begin
         --  Parent is parent of Child; return node immediately after Child.
         if Parent = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            pragma Assert (Parent.Label = Nonterm);
            for I in Parent.Children'Range loop
               if Parent.Children (I) = Child then
                  --  Use first non-empty next from I + 1.
                  for J in I + 1 .. Parent.Children'Last loop
                     --  We tolerate deleted children here for edited trees.
                     if Parent.Children (J) /= Invalid_Node_Access then
                        declare
                           Result : constant Node_Access := First_Child (Parent.Children (J));
                        begin
                           if Result /= Invalid_Node_Access then
                              return Result;
                           end if;
                        end;
                     end if;
                  end loop;
                  --  All next Children are empty
                  return Next_Child (Parent, Parent.Parent);
               end if;
            end loop;
            raise SAL.Programmer_Error; -- Child not found in Node.Children
         end if;
      end Next_Child;
   begin
      return Next_Child (Node, Node.Parent);
   end Next_Terminal;

   procedure Next_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is
      pragma Unreferenced (Tree);

      function First_Child (Node : in Valid_Node_Access) return Node_Access
      is
      begin
         case Node.Label is
         when Terminal_Label =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
            Parents.Push (Node);
            for J of Node.Children loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               declare
                  Result : constant Node_Access := First_Child (J);
               begin
                  if Result /= Invalid_Node_Access then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            Parents.Pop;
            return Invalid_Node_Access;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Access; Parent : in Valid_Node_Access) return Node_Access
      is
         Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
      begin
         --  Parent is Parent of Child; return node immediately after Child.
         pragma Assert (Parent.Label = Nonterm);
         for I in Parent.Children'Range loop
            --  Encountering a deleted child here is an error in the user
            --  algorithm.
            if Parent.Children (I) = Child then
               --  Use first non-empty from I + 1.
               for J in I + 1 .. Parent.Children'Last loop
                  Parents.Push (Parent);
                  declare
                     Result : constant Node_Access := First_Child (Parent.Children (J));
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     else
                        Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to First_Child.
                     end if;
                  end;
               end loop;
               --  All next Children are empty (or there are none); move to
               --  next cousin.
               if Parents.Is_Empty then
                  return Invalid_Node_Access;
               else
                  return Next_Child (Parent, Parents.Pop);
               end if;
            end if;
         end loop;
         raise SAL.Programmer_Error; -- Child not found in Node.Children
      end Next_Child;
   begin
      if Parents.Is_Empty then
         Node := Invalid_Node_Access;
      else
         Node := Next_Child (Node, Parents.Pop);
      end if;
   end Next_Terminal;

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Constant_Ref (Ref.Element.Cur).Node.Label in Terminal_Label or else
           Ref.Node = Invalid_Node_Access
           --  A previous Stream_Next arrived at an empty nonterm, or
           --  Next_Terminal reached the end of an element node.
         then
            Stream_Next (Tree, Ref, Rooted => False);
            exit when Ref.Element = Invalid_Stream_Index;

         else
            Ref.Node := Next_Terminal (Tree, Ref.Node);
         end if;

         exit when Ref.Node /= Invalid_Node_Access;
      end loop;
   end Next_Terminal;

   function Next_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   is begin
      return Result : Terminal_Ref := Ref do
         Next_Terminal (Tree, Result);
      end return;
   end Next_Terminal;

   procedure Next_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Constant_Ref (Ref.Ref.Element.Cur).Node.Label in Terminal_Label or else
           --  Can only be true on the first loop

           Ref.Ref.Node = Invalid_Node_Access
           --  The previous loop reached the end of an element node.

         then
            Stream_Next (Tree, Ref, Rooted => False);

            exit when Ref.Ref.Element = Invalid_Stream_Index; -- end of stream

            exit when Ref.Ref.Node /= Invalid_Node_Access and then Ref.Ref.Node.Label in Terminal_Label;

         else
            Next_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
            exit when Ref.Ref.Node /= Invalid_Node_Access;
         end if;

      end loop;
   end Next_Terminal;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID
   is begin
      return Result : constant Stream_ID :=
        (Cur             => Tree.Streams.Append
           ((Label       => Tree.Next_Stream_Label,
             Stack_Top   => Invalid_Stream_Index.Cur,
             Shared_Link => Tree.Stream_First (Tree.Shared_Stream).Element.Cur,
             Elements    => <>)))
      do
         Tree.Next_Stream_Label := @ + 1;
      end return;
   end New_Stream;

   function New_Stream
     (Tree       : in out Syntax_Trees.Tree;
      Old_Stream : in     Stream_ID;
      User_Data  : in     User_Data_Access)
     return Stream_ID
   is begin
      if Old_Stream = Invalid_Stream_ID then
         return New_Stream (Tree);
      else
         declare
            Old_Parse_Stream : Parse_Stream renames Tree.Streams (Old_Stream.Cur);
            Old_Stack_Top    : constant Stream_Element_Lists.Cursor := Old_Parse_Stream.Stack_Top;

            Result_Cur : constant Parse_Stream_Lists.Cursor := Tree.Streams.Append
              ((Label       => Tree.Next_Stream_Label,
                Stack_Top   => Invalid_Stream_Index.Cur,
                Shared_Link => Old_Parse_Stream.Shared_Link,
                Elements    => <>));

            New_Stream : Parse_Stream renames Tree.Streams (Result_Cur);

            New_Cur : Stream_Element_Lists.Cursor;
         begin
            for Old_Cur in Old_Parse_Stream.Elements.Iterate loop
               declare
                  Old_Element : Stream_Element renames Stream_Element_Lists.Constant_Ref (Old_Cur);
                  New_Node    : constant Node_Access := Old_Element.Node;
                  --  We do not deep copy any nodes for the new stream; they are all
                  --  shared with other streams.
               begin
                  New_Cur := New_Stream.Elements.Append
                    ((Node  => New_Node,
                      State => Old_Element.State,
                      Label => Tree.Next_Stream_Label));
               end;

               if Old_Cur = Old_Stack_Top then
                  New_Stream.Stack_Top := New_Cur;
               end if;
            end loop;

            Tree.Next_Stream_Label := @ + 1;

            return (Cur => Result_Cur);
         end;
      end if;
   end New_Stream;

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result
   is
     --  Within one batch parsed subtree, positive and negative
     --  Node_Indices are separately unique. Positive Node_Index first, abs
     --  value for wisitoken_grammar_editing.translate_EBNF_to_BNF.
     (if Left.Node_Index > 0 and Right.Node_Index <= 0 then SAL.Less
      elsif Left.Node_Index <= 0 and Right.Node_Index > 0 then SAL.Greater
      elsif abs Left.Node_Index > abs Right.Node_Index then SAL.Greater
      elsif abs Left.Node_Index < abs Right.Node_Index then SAL.Less
      else SAL.Equal);

   function Non_Grammar_Var
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in     Valid_Node_Access)
     return Token_Array_Var_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return
        (Element =>
           (case Terminal.Label is
            when Terminal_Label => Terminal.Non_Grammar'Access,
            when others         => raise SAL.Programmer_Error),
         Dummy => 0);
   end Non_Grammar_Var;

   function Non_Grammar_Const (Terminal : in Valid_Node_Access) return Token_Array_Const_Ref
   is begin
      return
        (Element =>
           (case Terminal.Label is
            when Terminal_Label => Terminal.Non_Grammar'Access,
            when others         => raise SAL.Programmer_Error),
         Dummy => 0);
   end Non_Grammar_Const;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Token_Array_Const_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return Non_Grammar_Const (Terminal);
   end Non_Grammar_Const;

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Count : in Positive := 1)
     return Node_Access
   is
      Result : Node_Access := Node;
      N      : Natural    := 0;
   begin
      loop
         Result := Result.Parent;
         N := N + 1;
         exit when N = Count or Result = Invalid_Node_Access;
      end loop;
      return Result;
   end Parent;

   function Peek
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Count  : in SAL.Peek_Type := 1)
     return Stream_Index
   is
      use Stream_Element_Lists;

      Result : Cursor := Tree.Streams (Stream.Cur).Stack_Top;
   begin
      for I in 1 .. Count - 1 loop
         Result := Previous (@);
      end loop;
      return (Cur => Result);
   end Peek;

   function Pop (Parse_Stream : in out Syntax_Trees.Parse_Stream) return Valid_Node_Access
   is
      use Stream_Element_Lists;
      Temp : Cursor := Parse_Stream.Stack_Top;
   begin
      return Result : constant Valid_Node_Access := Constant_Ref (Parse_Stream.Stack_Top).Node do
         Parse_Stream.Stack_Top := Previous (@);
         Parse_Stream.Elements.Delete (Temp);
         --  This does not change Parse_Stream.Shared_Link
      end return;
   end Pop;

   function Pop
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID)
     return Valid_Node_Access
   is begin
      return Pop (Tree.Streams (Stream.Cur));
   end Pop;

   function Prev_Non_Grammar
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Valid_Node_Access
   is
      Result : Node_Access := Node;
   begin
      if Node = Tree.SOI or Node = Tree.Root then
         return Tree.SOI;
      end if;

      loop
         Result := Prev_Terminal (Tree, Result);
         exit when Result.Non_Grammar.Length > 0;
      end loop;
      return Result;
   end Prev_Non_Grammar;

   procedure Prev_Non_Grammar
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is begin
      if Ref.Node = Tree.SOI then
         return;
      end if;

      loop
         Prev_Terminal (Tree, Ref);
         exit when Ref.Node = Invalid_Node_Access;
         exit when Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Prev_Non_Grammar;

   procedure Prev_Non_Grammar
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Parents)
   is begin
      if Ref.Ref.Node = Tree.SOI then
         return;
      end if;
      loop
         Prev_Terminal (Tree, Ref);
         exit when Ref.Ref.Node = Invalid_Node_Access;
         exit when Ref.Ref.Node.Non_Grammar.Length > 0;
      end loop;
   end Prev_Non_Grammar;

   procedure Prev_Sequential_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is begin
      loop
         Prev_Terminal (Tree, Node, Parents);
         exit when Node = Invalid_Node_Access;
         exit when Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Prev_Sequential_Terminal;

   procedure Prev_Sequential_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Syntax_Trees.Stream_Node_Parents)
   is begin
      loop
         Prev_Terminal (Tree, Ref);
         exit when Ref.Ref = Invalid_Stream_Node_Ref;
         exit when Ref.Ref.Node.Sequential_Index /= Invalid_Sequential_Index;
      end loop;
   end Prev_Sequential_Terminal;

   function Prev_Source_Terminal
     (Tree                 : in Syntax_Trees.Tree;
      Node                 : in Valid_Node_Access;
      Trailing_Non_Grammar : in Boolean)
     return Node_Access
   is
      Result : Node_Access := Prev_Terminal (Tree, Node);
   begin
      loop
         exit when Result = Invalid_Node_Access;
         exit when
           (if Trailing_Non_Grammar
            then (case Terminal_Label'(Result.Label) is
                  when Source_Terminal => True,
                  when Virtual_Terminal | Virtual_Identifier =>
                     Result.Non_Grammar.Length > 0)
            else Result.Label = Source_Terminal);

         Prev_Terminal (Tree, Result);
      end loop;
      return Result;
   end Prev_Source_Terminal;

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      function Last_Child (Node : in Valid_Node_Access) return Node_Access
      is begin
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
            for J of reverse Node.Children loop
               --  We tolerate deleted children here for edited trees.
               if J /= Invalid_Node_Access then
                  declare
                     Result : constant Node_Access := Last_Child (J);
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     end if;
                  end;
               end if;
            end loop;
            --  All Children are empty
            return Invalid_Node_Access;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Access; Parent : in Node_Access) return Node_Access
      is begin
         --  Parent is Parent of Child; return terminal node immediately previous to Child.
         if Parent = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            pragma Assert (Parent.Label = Nonterm);
            for I in reverse Parent.Children'Range loop
               if Parent.Children (I) = Child then
                  --  Use first non-empty from I - 1.
                  for J in reverse Parent.Children'First .. I - 1 loop
                     --  We tolerate deleted children here for edited trees.
                     if Parent.Children (J) /= Invalid_Node_Access then
                        declare
                           Result : constant Node_Access := Last_Child (Parent.Children (J));
                        begin
                           if Result /= Invalid_Node_Access then
                              return Result;
                           end if;
                        end;
                     end if;
                  end loop;
                  --  All previous Children are empty
                  return Prev_Child (Parent, Parent.Parent);
               end if;
            end loop;
            raise SAL.Programmer_Error; -- Child not found in Parent.Children
         end if;
      end Prev_Child;
   begin
      return Prev_Child (Node, Node.Parent);
   end Prev_Terminal;

   procedure Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in out Node_Access)
   is begin
      Node := Prev_Terminal (Tree, Node);
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree    : in     Syntax_Trees.Tree;
      Node    : in out Node_Access;
      Parents : in out Node_Stacks.Stack)
   is
      pragma Unreferenced (Tree);

      function Last_Child (Node : in Valid_Node_Access) return Node_Access
      is begin
         case Node.Label is
         when Terminal_Label =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
            Parents.Push (Node);
            for J of reverse Node.Children loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               declare
                  Result : constant Node_Access := Last_Child (J);
               begin
                  if Result /= Invalid_Node_Access then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            Parents.Pop;
            return Invalid_Node_Access;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Access; Parent : in Valid_Node_Access) return Node_Access
      is
         Parent_Depth : constant SAL.Base_Peek_Type := Parents.Depth;
      begin
         --  Parent is parent of Child; return node immediately previous to Child.
         pragma Assert (Parent.Label = Nonterm);
         for I in reverse Parent.Children'Range loop
            --  Encountering a deleted child here is an error in the user
            --  algorithm.
            if Parent.Children (I) = Child then
               --  Use first non-empty from I - 1.
               for J in reverse Parent.Children'First .. I - 1 loop
                  Parents.Push (Parent);
                  declare
                     Result : constant Node_Access := Last_Child (Parent.Children (J));
                  begin
                     if Result /= Invalid_Node_Access then
                        return Result;
                     else
                        Parents.Pop (Parents.Depth - Parent_Depth); -- discard parents from call to Last_Child.
                     end if;
                  end;
               end loop;

               --  All previous Parent.Children are empty (or there are none); move to
               --  prev cousin.
               if Parents.Is_Empty then
                  return Invalid_Node_Access;
               else
                  return Prev_Child (Parent, Parents.Pop);
               end if;
            end if;
         end loop;
         raise SAL.Programmer_Error; -- Child not found in Parent.Children
      end Prev_Child;
   begin
      if Parents.Is_Empty then
         Node := Invalid_Node_Access;

      else
         Node := Prev_Child (Node, Parents.Pop);
      end if;
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Terminal_Ref)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements

         if Constant_Ref (Ref.Element.Cur).Node.Label in Terminal_Label or else
              Ref.Node = Invalid_Node_Access
           --  A previous Prev_Element arrived at an empty nonterm, or
           --  Prev_Terminal reached the beginning of an element node.
         then
            Stream_Prev (Tree, Ref, Rooted => False);
            exit when Ref.Element = Invalid_Stream_Index;

         else
            Ref.Node := Prev_Terminal (Tree, Ref.Node);
         end if;
         exit when Ref.Node /= Invalid_Node_Access;
      end loop;
   end Prev_Terminal;

   function Prev_Terminal
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Terminal_Ref)
     return Terminal_Ref
   is begin
      return Result : Terminal_Ref := Ref do
         Prev_Terminal (Tree, Result);
      end return;
   end Prev_Terminal;

   procedure Prev_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Ref          : in out Stream_Node_Parents;
      Parse_Stream : in     Stream_ID := Invalid_Stream_ID)
   is
      use Stream_Element_Lists;
   begin
      loop -- Handle empty Elements
         if Constant_Ref (Ref.Ref.Element.Cur).Node.Label in Terminal_Label or else
           Ref.Ref.Node = Invalid_Node_Access
           --  Ref is at a terminal element or an empty nonterm, or was at a
           --  first terminal; move to next stream element.
         then
            if Parse_Stream /= Invalid_Stream_ID and Ref.Ref.Stream = Tree.Shared_Stream then
               declare
                  P_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Parse_Stream.Cur);
               begin
                  if Ref.Ref.Element.Cur = P_Stream.Shared_Link then
                     Ref := (Ref     => (Parse_Stream, (Cur => P_Stream.Elements.Last), Invalid_Node_Access),
                             Parents => <>);
                     Tree.Last_Terminal (Ref);
                  else
                     Stream_Prev (Tree, Ref, Rooted => False);
                  end if;
               end;
            else
               Stream_Prev (Tree, Ref, Rooted => False);
            end if;

            exit when Ref.Ref.Element = Invalid_Stream_Index;
         else
            Prev_Terminal (Tree, Ref.Ref.Node, Ref.Parents);
         end if;
         exit when Ref.Ref.Node /= Invalid_Node_Access;
      end loop;
   end Prev_Terminal;

   procedure Print_Streams (Tree : in Syntax_Trees.Tree; Non_Grammar : in Boolean := False)
   is begin
      for Stream of Tree.Streams loop
         Ada.Text_IO.Put_Line (Tree.Image (Stream, Non_Grammar => Non_Grammar));
      end loop;
   end Print_Streams;

   procedure Print_Tree
     (Tree         : in Syntax_Trees.Tree;
      Root         : in Node_Access               := Invalid_Node_Access;
      Image_Action : in Syntax_Trees.Image_Action := null;
      Line_Numbers : in Boolean                   := False;
      Non_Grammar  : in Boolean                   := False)
   is
      use Ada.Text_IO;

      procedure Print_Node (Node : in Valid_Node_Access; Level : in Integer)
      is begin
         Put (Decimal_Image (Node.Node_Index, Width => 4) & ": ");
         for I in 1 .. Level loop
            Put ("| ");
         end loop;
         Put (Image (Tree, Node, Children => False, RHS_Index => True, Terminal_Node_Numbers => True,
                     Line_Numbers => Line_Numbers, Non_Grammar => Non_Grammar));

         if Node.Augmented /= null then
            Put (Image_Augmented (Node.Augmented.all));
         end if;
         if Node.Label = Nonterm and then (Image_Action /= null and Node.Action /= null) then
            Put (" - " & Image_Action (Node.Action));
         end if;

         New_Line;
         if Node.Label = Nonterm then
            for Child of Node.Children loop
               if Child = null then
                  Put ("    : ");
                  for I in 1 .. Level + 1 loop
                     Put ("| ");
                  end loop;
                  Put_Line ("<deleted>");
               else
                  Print_Node (Child, Level + 1);
               end if;
            end loop;
         end if;
      end Print_Node;

      Print_Root : constant Node_Access := (if Root = Invalid_Node_Access then Syntax_Trees.Root (Tree) else Root);

      Print_SOI_EOI : constant Boolean := Root = Invalid_Node_Access and Print_Root /= Tree.Root;
   begin
      if Print_Root = Invalid_Node_Access then
         Put_Line ("<empty tree>");
      else
         if Print_SOI_EOI then
            Print_Node (Tree.SOI, 0);
         end if;
         Print_Node (Print_Root, 0);
         if Print_SOI_EOI then
            Print_Node (Tree.EOI, 0);
         end if;
      end if;
   end Print_Tree;

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Access;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Access)
        return Boolean)
     return Boolean
   is
   begin
      if Visit_Parent = Before then
         if not Process_Node (Tree, Node) then
            return False;
         end if;
      end if;

      if Node.Label = Nonterm then
         for Child of Node.Children loop
            if Child /= null then
               if not Process_Tree (Tree, Child, Visit_Parent, Process_Node) then
                  return False;
               end if;
            end if;
         end loop;
      end if;

      if Visit_Parent = After then
         return Process_Node (Tree, Node);
      else
         return True;
      end if;
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Node         : in     Valid_Node_Access;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access))
   is begin
      if Node.Label = Nonterm then
         for Child of Node.Children loop
            if Child /= null then
               Process_Tree (Tree, Child, Process_Node);
            end if;
         end loop;
      end if;

      Process_Node (Tree, Node);
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access);
      Root         : in     Node_Access := Invalid_Node_Access)
   is begin
      Tree.Traversing := True;
      Process_Tree (Tree, (if Root = Invalid_Node_Access then Syntax_Trees.Root (Tree) else Root), Process_Node);
      Tree.Traversing := False;
   exception
   when others =>
      Tree.Traversing := False;
      raise;
   end Process_Tree;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Production_ID
   is begin
      return (Node.ID, Node.RHS_Index);
   end Production_ID;

   function Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Stream_ID    : in     Syntax_Trees.Stream_ID;
      Node         : in     Valid_Node_Access;
      State        : in     State_Index)
     return Rooted_Ref
   is
      use Stream_Element_Lists;
      New_Element : constant Cursor := Parse_Stream.Elements.Insert
        (Element  => (Node, State, Parse_Stream.Label),
         Before   => Next (Parse_Stream.Stack_Top));
   begin
      Parse_Stream.Stack_Top := New_Element;
      --  caller must change Parse_Stream.Shared_Link if needed.
      return (Stream_ID, (Cur => New_Element), Node);
   end Push;

   procedure Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Stream_ID    : in     Syntax_Trees.Stream_ID;
      Node         : in     Valid_Node_Access;
      State        : in     State_Index)
   is
      Junk : Stream_Node_Ref := Push (Parse_Stream, Stream_ID, Node, State);
      pragma Unreferenced (Junk);
   begin
      null;
   end Push;

   procedure Push
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      State  : in     State_Index)
   is begin
      Push (Tree.Streams (Stream.Cur), Stream, Node, State);
   end Push;

   procedure Push_Back
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Parse_Stream.Stack_Top := Stream_Element_Lists.Previous (Parse_Stream.Stack_Top);
      --  This does not change Parse_Stream.Shared_Link
   end Push_Back;

   function Reduce
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Child_Count     : in     Ada.Containers.Count_Type;
      Action          : in     Post_Parse_Action := null;
      State           : in     State_Index;
      Default_Virtual : in     Boolean           := False)
     return Rooted_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      function Pop_Children return Valid_Node_Access_Array
      is begin
         return Result : Valid_Node_Access_Array (1 .. SAL.Base_Peek_Type (Child_Count)) := (others => Dummy_Node) do
            --  FIXME: use iterated_component_association to avoid bogus init. Waiting on compiler support.
            for I in reverse Result'Range loop
               Result (I) := Pop (Parse_Stream);
            end loop;
         end return;
      end Pop_Children;

      New_Node : constant Node_Access := Tree.Add_Nonterm_1
        (Production, Pop_Children, Action, Default_Virtual, Clear_Parents => False);
   begin
      return Push (Parse_Stream, Stream, New_Node, State);
   end Reduce;

   procedure Replace_Child
     (Tree                 : in out Syntax_Trees.Tree;
      Parent               : in     Valid_Node_Access;
      Child_Index          : in     SAL.Peek_Type;
      Old_Child            : in     Node_Access;
      New_Child            : in     Node_Access;
      Old_Child_New_Parent : in     Node_Access := Invalid_Node_Access)
   is begin
      Parent.Children (Child_Index) := New_Child;

      if Old_Child /= null then
         Old_Child.Parent := Old_Child_New_Parent;
      end if;

      New_Child.Parent := Parent;
   end Replace_Child;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   is begin
      return Node.RHS_Index;
   end RHS_Index;

   procedure Right_Breakdown
     (Tree : in out Syntax_Trees.Tree;
      Ref  : in out Stream_Node_Ref)
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Ref.Stream.Cur);

      Insert_Before : constant Cursor   := Next (Ref.Element.Cur);
      To_Delete     : Cursor            := Ref.Element.Cur;
      Node          : Valid_Node_Access := Parse_Stream.Elements (Ref.Element.Cur).Node;
      Next_Node     : Node_Access;
   begin
      loop
         Next_Node := Invalid_Node_Access;

         for I in 1 .. Node.Child_Count - 1 loop
            if Node.Children (I).Child_Count > 0 then
               Next_Node := Node.Children (I);
            end if;

            Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => Node.Children (I),
                  State => Unknown_State,
                  Label => Parse_Stream.Label),
               Before   => Insert_Before);

            Node.Children (I).Parent := Invalid_Node_Access;
            if Tree.Parents_Set then
               Node.Children (I) := Invalid_Node_Access;
            end if;
         end loop;

         if Node.Children (Node.Child_Count).Child_Count > 0 or
           Node.Children (Node.Child_Count).Label in Terminal_Label
         then
            declare
               Temp : constant Node_Access := Node.Children (Node.Child_Count);
            begin
               if Tree.Parents_Set then
                  Node.Children (Node.Child_Count) := Invalid_Node_Access;
               end if;
               Node        := Temp;
               Node.Parent := Invalid_Node_Access;
            end;

            if Node.Label in Terminal_Label then
               Ref.Element.Cur := Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Node,
                     State => Unknown_State,
                     Label => Parse_Stream.Label),
                  Before   => Insert_Before);

               Ref.Node := Node;

               Parse_Stream.Elements.Delete (To_Delete);
               exit;
            end if;
         else
            --  Node is an empty nonterm. Note that Next_Node cannot be null; the
            --  precondition asserts that Ref was not empty.
            Node := Next_Node;
         end if;
      end loop;
   end Right_Breakdown;

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      if Tree.Root = Invalid_Node_Access then
         if Tree.Streams.Length = 0 then
            return Invalid_Node_Access;
         else
            declare
               use Stream_Element_Lists;
               Stream : Parse_Stream renames Tree.Streams (Tree.Streams.Last);
               --  parse stream from Parse or shared_stream from Edit_Tree with no changes

               Cur : Cursor := Stream.Elements.First; -- SOI
            begin
               if Has_Element (Cur) then
                  Cur := Next (Cur); -- wisitoken_accept
                  if Has_Element (Cur) then
                     return Stream.Elements (Cur).Node;
                  end if;
               end if;
               return Invalid_Node_Access;
            end;
         end if;
      else
         return Tree.Root;
      end if;
   end Root;

   procedure Set_Augmented
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Value : in Augmented_Class_Access)
   is begin
      Node.Augmented := Value;
   end Set_Augmented;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in out Valid_Node_Access;
      Children : in     Node_Access_Array)
   is begin
      --  See Design note in spec about Parents, Parent_Set.

      --  Clear current Children.Parent first, in case some are also in new
      --  children.
      for C of Parent.Children loop
         if C /= null then
            C.Parent := Invalid_Node_Access;
         end if;
      end loop;

      if Parent.Children'Length = Children'Length then
         --  reuse current node
         Parent.Virtual     := False;
         Parent.Children    := Children;

      else
         --  reallocate node with new child_count
         declare
            Realloc_Parent : constant Node_Access := new Node'
              (Label       => Nonterm,
               Child_Count => Children'Last,
               ID          => Parent.ID,
               Node_Index  => -(Tree.Nodes.Last_Index + 1),
               Parent      => Parent.Parent,
               Augmented   => Parent.Augmented,
               Virtual     => False,
               RHS_Index   => Parent.RHS_Index,
               Action      => Parent.Action,
               Name_Offset => Parent.Name_Offset,
               Name_Length => Parent.Name_Length,
               Children    => Children);
         begin
            Tree.Nodes.Append (Realloc_Parent);

            if Parent.Parent /= null then
               Parent.Parent.Children (Child_Index (Parent.Parent.all, Parent)) := Realloc_Parent;
            end if;

            Parent := Realloc_Parent;
         end;
      end if;

      for Child of Children loop
         if Child.Parent /= Invalid_Node_Access then
            declare
               Other_Parent : constant Node_Access := Child.Parent;
               Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                 (Other_Parent.all, Child);
            begin
               Other_Parent.Children (Child_Index) := null;
            end;
         end if;

         Child.Parent := Parent;
      end loop;
   end Set_Children;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in out Valid_Node_Access;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Node_Access_Array)
   is
   begin
      if New_ID /= (Node.ID, Node.RHS_Index) then
         Node.Action := null;
      end if;

      Node.ID        := New_ID.LHS;
      Node.RHS_Index := New_ID.RHS;

      Set_Children (Tree, Node, Children);
   end Set_Children;

   procedure Set_Name
     (Tree : in     Syntax_Trees.Tree;
      Item : in out Recover_Token;
      Name : in     Buffer_Region)
   is begin
      if Item.Virtual then
         Item.Name := Name;
      else
         Tree.Set_Name (Item.Element_Node, Name);
      end if;
   end Set_Name;

   procedure Set_Name
     (Tree   : in Syntax_Trees.Tree;
      Node   : in Valid_Node_Access;
      Region : in Buffer_Region)
   is
      First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
      Byte_First     : constant Buffer_Pos  := Tree.Byte_Region (First_Terminal).First;
   begin
      Node.Name_Offset := Region.First - Byte_First;
      Node.Name_Length := Region.Last - Region.First + 1;
   end Set_Name;

   procedure Set_Node_Index
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Access;
      Node_Index : in Syntax_Trees.Node_Index)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Node_Index := Node_Index;
   end Set_Node_Index;

   procedure Set_Parents
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID := Invalid_Stream_ID)
   is
      procedure Set_Parents
        (Tree   : in out Syntax_Trees.Tree;
         Node   : in     Valid_Node_Access;
         Parent : in     Node_Access)
      is
      begin
         Node.Parent := Parent;
         case Node.Label is
         when Source_Terminal | Virtual_Terminal | Virtual_Identifier =>
            null;

         when Nonterm =>
            for C of Node.Children loop
               if C = null then
                  --  This can only happen if someone calls Set_Parents after parents
                  --  are already set and the tree is edited.
                  raise SAL.Programmer_Error with "encountered deleted child";
               end if;
               Set_Parents (Tree, C, Node);
            end loop;
         end case;
      end Set_Parents;
   begin
      --  FIXME: only need to handle fully parsed tree, no streams. Use
      --  incremental algorithm; if find a set parent link, assume subtree
      --  under that node has parent links set? requires all "node.parent =
      --  null" to do all ancestors as well.
      if Stream = Invalid_Stream_ID then
         if Tree.Streams.Length = 0 then
            if Tree.Root = Invalid_Node_Access then
               raise SAL.Parameter_Error with "invalid_tree: no streams, Tree.Root not set";
            else
               Set_Parents (Tree, Tree.Root, Invalid_Node_Access);
            end if;
         else
            for Element of Tree.Streams (Tree.Shared_Stream.Cur).Elements loop
               Set_Parents (Tree, Element.Node, Invalid_Node_Access);
            end loop;
         end if;
      else
         for Element of Tree.Streams (Stream.Cur).Elements loop
            Set_Parents (Tree, Element.Node, Invalid_Node_Access);
         end loop;
      end if;
      Tree.Parents_Set := True;
   end Set_Parents;

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access)
   is begin
      if New_Root.Children (1) = Tree.SOI and
        New_Root.Children (New_Root.Children'Last) = Tree.EOI
      then
         Tree.Root := New_Root;
      else
         declare

            function Create_New_Children return Node_Access_Array
            is
               Last : Positive_Index_Type := New_Root.Children'Last;
               New_Children : Node_Access_Array (1 .. New_Root.Children'Last + 2);
            begin
               if New_Root.Children (1) /= Tree.SOI then
                  New_Children (1) := Tree.SOI;
                  Last := 1 + New_Root.Children'Length;
                  New_Children (2 .. Last) := New_Root.Children;
               end if;

               if New_Root.Children (New_Root.Children'Last) /= Tree.EOI then
                  Last := @ + 1;
                  New_Children (Last) := Tree.EOI;
               end if;
               return New_Children (1 .. Last);
            end Create_New_Children;

            New_Children : constant Node_Access_Array := Create_New_Children;
         begin
            Tree.Root := new Node'
              (Label       => Nonterm,
               Child_Count => New_Children'Last,
               ID          => New_Root.ID,
               Node_Index  => New_Root.Node_Index,
               Parent      => null,
               Augmented   => New_Root.Augmented,
               Virtual     => New_Root.Virtual,
               RHS_Index   => New_Root.RHS_Index,
               Action      => New_Root.Action,
               Name_Offset => New_Root.Name_Offset,
               Name_Length => New_Root.Name_Length,
               Children    => New_Children);

            for Child of New_Children loop
               Child.Parent := Tree.Root;
            end loop;

            Tree.Nodes.Append (Tree.Root);
         end;
      end if;
   end Set_Root;

   procedure Set_Sequential_Index
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Access;
      Index : in Base_Sequential_Index)
   is
      pragma Unreferenced (Tree);
   begin
      Node.Sequential_Index := Index;
   end Set_Sequential_Index;

   procedure Set_Shared_Link
     (Tree        : in out Syntax_Trees.Tree;
      Stream      : in     Stream_ID;
      Shared_Link : in     Stream_Node_Ref)
   is begin
      Tree.Streams (Stream.Cur).Shared_Link := Shared_Link.Element.Cur;
   end Set_Shared_Link;

   procedure Shift
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index;
      Token  : in     Stream_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Element      : Stream_Element renames Stream_Element_Lists.Constant_Ref (Token.Cur);
   begin
      if Element.Label = Shared_Stream_Label then
         Push (Parse_Stream, Stream, Element.Node, State);
         Parse_Stream.Shared_Link := Stream_Element_Lists.Next (Token.Cur);
      else
         Parse_Stream.Stack_Top := Token.Cur;
         Parse_Stream.Elements (Parse_Stream.Stack_Top).State := State;
      end if;
   end Shift;

   procedure Shift
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Shift_Bytes : in Base_Buffer_Pos;
      Shift_Chars : in Base_Buffer_Pos;
      Shift_Line  : in Base_Line_Number_Type)
   is
      pragma Unreferenced (Tree);
   begin
      case Terminal_Label'(Node.Label) is
      when Source_Terminal =>
         if Node.Byte_Region /= Null_Buffer_Region then
            Node.Byte_Region := @ + Shift_Bytes;
         end if;
         if Node.Char_Region /= Null_Buffer_Region then
            Node.Char_Region := @ + Shift_Chars;
         end if;
      when Virtual_Terminal | Virtual_Identifier =>
         null;
      end case;
      for Token of Node.Non_Grammar loop
         Token.Byte_Region := @ + Shift_Bytes;
         Token.Char_Region := @ + Shift_Chars;
         Token.Line_Region := @ + Shift_Line;
      end loop;
      if Node.Augmented /= null then
         Shift (Node.Augmented.all, Shift_Bytes, Shift_Chars, Shift_Line);
      end if;
   end Shift;

   function SOI (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      return Tree.SOI;
   end SOI;

   function Stack_Depth (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Element : Cursor             := Parse_Stream.Stack_Top;
      Result  : SAL.Base_Peek_Type := 0;
   begin
      loop
         exit when not Has_Element (Element);
         Result := @ + 1;
         Element := Previous (Element);
      end loop;
      return Result;
   end Stack_Depth;

   procedure Start_Edit (Tree : in out Syntax_Trees.Tree)
   is
      use Stream_Element_Lists;
   begin
      Tree.Shared_Stream :=
        (Cur             => Tree.Streams.Append
           ((Label       => Shared_Stream_Label,
             Stack_Top   => Invalid_Stream_Index.Cur,
             Shared_Link => Invalid_Stream_Index.Cur,
             Elements    => <>)));

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.SOI,
          State => Unknown_State,
          Label => Shared_Stream_Label));

      Tree.SOI.Parent := Invalid_Node_Access;

      --  Delete SOI, EOI from root children (added in Clear_Parse_Streams)
      declare
         New_Children : Node_Access_Array (1 .. Tree.Root.Child_Count - 2);
      begin
         New_Children := Tree.Root.Children (2 .. Tree.Root.Children'Last - 1);

         Tree.Root := new Node'
           (Label       => Nonterm,
            Child_Count => Tree.Root.Child_Count - 2,
            ID          => Tree.Root.ID,
            Node_Index  => Tree.Root.Node_Index,
            Parent      => null,
            Augmented   => Tree.Root.Augmented,
            Virtual     => Tree.Root.Virtual,
            RHS_Index   => Tree.Root.RHS_Index,
            Action      => Tree.Root.Action,
            Name_Offset => Tree.Root.Name_Offset,
            Name_Length => Tree.Root.Name_Length,
            Children    => New_Children);

         for Child of New_Children loop
            Child.Parent := Tree.Root;
         end loop;

         Tree.Nodes.Append (Tree.Root);
      end;

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.Root,
          State => Unknown_State,
          Label => Shared_Stream_Label));

      Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
        ((Node  => Tree.EOI,
          State => Unknown_State,
          Label => Shared_Stream_Label));

      Tree.EOI.Parent := Invalid_Node_Access;

      Tree.Root := Invalid_Node_Access;
   end Start_Edit;

   procedure Start_Lex
     (Tree           : in out Syntax_Trees.Tree)
   is
      Begin_Byte_Pos : Buffer_Pos;
      Begin_Char_Pos : Buffer_Pos;
      Begin_Line     : Line_Number_Type;
   begin
      Tree.Lexer.Begin_Pos (Begin_Byte_Pos, Begin_Char_Pos, Begin_Line);
      declare
         Token          : constant Lexer.Token :=
           (ID          => Tree.Lexer.Descriptor.SOI_ID,
            Byte_Region => (Begin_Byte_Pos, Begin_Byte_Pos - 1),
            Char_Region => (Begin_Char_Pos, Begin_Char_Pos - 1),
            Line_Region => (Begin_Line, Begin_Line));
         --  Tree.Line_Region uses SOI.Line_Region.First as first line in text,
         --  .Last as first line in next token.

      begin
         if Tree.SOI = null then
            Tree.SOI := new Node'
              (Label       => Source_Terminal,
               Child_Count => 0,
               others => <>);
            Tree.Nodes.Append (Tree.SOI);
         end if;

         Tree.SOI.all :=
           (Label            => Source_Terminal,
            Child_Count      => 0,
            ID               => Tree.Lexer.Descriptor.SOI_ID,
            Node_Index       => 0,
            Parent           => Invalid_Node_Access,
            Augmented        => null,
            Non_Grammar      => Lexer.Token_Arrays.To_Vector (Token),
            Sequential_Index => Invalid_Sequential_Index,
            Byte_Region      => Token.Byte_Region,
            Char_Region      => Token.Char_Region);

         Tree.Shared_Stream :=
           (Cur             => Tree.Streams.Append
              ((Label       => Shared_Stream_Label,
                Stack_Top   => Stream_Element_Lists.No_Element,
                Shared_Link => Stream_Element_Lists.No_Element,
                Elements    => <>)));

         Tree.Streams (Tree.Shared_Stream.Cur).Elements.Append
           ((Node => Tree.SOI,
             State => Unknown_State,
             Label => Shared_Stream_Label));
      end;
   end Start_Lex;

   procedure Start_Parse
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   is
      Junk : Terminal_Ref := Append_Stream_Element (Tree, Stream, Tree.SOI, State);
      pragma Unreferenced (Junk);
   begin
      Tree.Parents_Set := False;
   end Start_Parse;

   procedure Stream_Delete
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in out Stream_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Element.Cur then
         Parse_Stream.Stack_Top := Stream_Element_Lists.No_Element;
      end if;

      Parse_Stream.Elements.Delete (Element.Cur);
   end Stream_Delete;

   function Stream_First
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean := True)
     return Stream_Index
   is begin
      return Result : Stream_Index := (Cur => Tree.Streams (Stream.Cur).Elements.First) do
         if Skip_SOI then
            Result.Cur := Stream_Element_Lists.Next (@);
         end if;
      end return;
   end Stream_First;

   function Stream_First
     (Tree     : in Syntax_Trees.Tree;
      Stream   : in Stream_ID;
      Skip_SOI : in Boolean := True)
     return Rooted_Ref
   is
      Cur : constant Stream_Element_Lists.Cursor := Tree.Streams (Stream.Cur).Elements.First;
   begin
      return Result : Rooted_Ref := (Stream, (Cur => Cur), Stream_Element_Lists.Constant_Ref (Cur).Node) do
         if Skip_SOI then
            Tree.Stream_Next (Result, Rooted => True);
         end if;
      end return;
   end Stream_First;

   function Stream_Input_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is
      use Stream_Element_Lists;
      use SAL;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      Element : Cursor         := Next (Parse_Stream.Stack_Top);
      Result  : Base_Peek_Type := 0;
   begin
      loop
         exit when not Has_Element (Element);
         Result := @ + 1;
         Element := Next (Element);
      end loop;
      return Result;
   end Stream_Input_Length;

   function Stream_Length (Tree : in Syntax_Trees.Tree; Stream : in Stream_ID) return SAL.Base_Peek_Type
   is (SAL.Base_Peek_Type (Tree.Streams (Stream.Cur).Elements.Length));

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is (if Element = Invalid_Stream_Index
       then (Cur => Tree.Streams (Stream.Cur).Elements.First)
       else (Cur => Stream_Element_Lists.Next (Element.Cur)));

   function Stream_Next
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   is
      Result : Stream_Node_Ref := Ref;
   begin
      Stream_Next (Tree, Result, Rooted => True);
      return Result;
   end Stream_Next;

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Element := (Cur => Next (Ref.Element.Cur));

      if not Has_Element (Ref.Element.Cur) then
         if Ref.Stream = Tree.Shared_Stream then
            Ref.Stream := Invalid_Stream_ID;

         elsif not Has_Element (Tree.Streams (Ref.Stream.Cur).Shared_Link) then
            Ref.Stream := Invalid_Stream_ID;

         else
            Ref.Element.Cur := Tree.Streams (Ref.Stream.Cur).Shared_Link;
            Ref.Stream      := Tree.Shared_Stream;
         end if;
      end if;

      Ref.Node :=
        (if Has_Element (Ref.Element.Cur)
         then (if Rooted
               then Constant_Ref (Ref.Element.Cur).Node
               else First_Terminal (Tree, Constant_Ref (Ref.Element.Cur).Node))
         else Invalid_Node_Access);
   end Stream_Next;

   procedure Stream_Next
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Parents.Clear;
      Stream_Next (Tree, Ref.Ref, Rooted => True);

      if Ref.Ref.Element.Cur /= No_Element and not Rooted then
         Ref.Ref.Node := First_Terminal
           (Tree, Stream_Element_Lists.Constant_Ref (Ref.Ref.Element.Cur).Node, Ref.Parents);
      end if;
   end Stream_Next;

   function Stream_Prev
     (Tree : in Syntax_Trees.Tree;
      Ref  : in Stream_Node_Ref)
     return Rooted_Ref
   is
      Result : Stream_Node_Ref := Ref;
   begin
      Stream_Prev (Tree, Result);
      return Result;
   end Stream_Prev;

   procedure Stream_Prev
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Ref;
      Rooted : in     Boolean := True)
   is
      use Stream_Element_Lists;
   begin
      Ref.Element := (Cur => Previous (Ref.Element.Cur));

      if not Has_Element (Ref.Element.Cur) then
         Ref.Stream := Invalid_Stream_ID;
      end if;

      Ref.Node :=
        (if Has_Element (Ref.Element.Cur)
         then (if Rooted
               then Constant_Ref (Ref.Element.Cur).Node
               else Tree.Last_Terminal (Constant_Ref (Ref.Element.Cur).Node))
         else Invalid_Node_Access);
   end Stream_Prev;

   procedure Stream_Prev
     (Tree   : in     Syntax_Trees.Tree;
      Ref    : in out Stream_Node_Parents;
      Rooted : in     Boolean)
   is
      use Stream_Element_Lists;
   begin
      Ref.Parents.Clear;
      Stream_Prev (Tree, Ref.Ref, Rooted => True);

      if Ref.Ref.Element.Cur /= No_Element and not Rooted then
         Ref.Ref.Node := Last_Terminal
           (Tree, Stream_Element_Lists.Constant_Ref (Ref.Ref.Element.Cur).Node, Ref.Parents);
      end if;
   end Stream_Prev;

   function Subtree_Image
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Node_Access;
      Non_Grammar  : in Boolean                   := False;
      Augmented    : in Boolean                   := False;
      Line_Numbers : in Boolean                   := False;
      Level        : in Integer                   := 0;
      Image_Action : in Syntax_Trees.Image_Action := null)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"" & ASCII.LF;
   begin
      Result := @ &
        (if Node = Invalid_Node_Access
         then "    "
         else Decimal_Image (Node.Node_Index, Width => 4)) & ": "; --  Decimal_Image is unsigned
      for I in 1 .. Level loop
         Result := @ & "| ";
      end loop;
      Result := @ &
        (if Node = Invalid_Node_Access
         then "<deleted>"
         else Image
           (Tree, Node,
            Children              => False,
            RHS_Index             => True,
            Terminal_Node_Numbers => True,
            Non_Grammar           => Non_Grammar,
            Line_Numbers          => Line_Numbers,
            Augmented             => Augmented,
            Image_Action          => Image_Action));

      if Node /= Invalid_Node_Access and then Node.Label = Nonterm then
         for Child of Node.Children loop
            Result := @ & Subtree_Image
              (Tree, Child, Non_Grammar, Augmented, Line_Numbers, Level + 1, Image_Action => Image_Action);
         end loop;
      end if;

      return -Result;
   end Subtree_Image;

   function Subtree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access
   is
      N : Valid_Node_Access := Node;
   begin
      loop
         exit when N.Parent = Invalid_Node_Access;
         N := N.Parent;
      end loop;
      return N;
   end Subtree_Root;

   function To_Node_Access (Item : in Valid_Node_Access_Array) return Node_Access_Array
   is (for I in Item'Range => Item (I));

   function To_Stream_Node_Parents (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return Stream_Node_Parents
   is begin
      return Result : Stream_Node_Parents := (Ref, Parents => <>) do
         if Ref = Invalid_Stream_Node_Ref or else Rooted (Ref) then
            null;
         else
            declare
               use Stream_Element_Lists;
               Terminal : constant Node_Access := Tree.First_Terminal
                 (Constant_Ref (Ref.Element.Cur).Node, Result.Parents);
            begin
               if Terminal /= Ref.Node then
                  raise SAL.Programmer_Error with "unsupported To_Stream_Node_Parents case";
               end if;
            end;
         end if;
      end return;
   end To_Stream_Node_Parents;

   function To_Valid_Node_Access (Item : in Node_Access_Array) return Valid_Node_Access_Array
   is (for I in Item'Range => Item (I));

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Traversing;
   end Traversing;

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String
   is begin
      return Node_Index'(Tree.Nodes.Last_Index)'Image;
   end Tree_Size_Image;

   procedure Validate_Tree
     (Tree           : in out Syntax_Trees.Tree;
      User_Data      : in out User_Data_Type'Class;
      Error_Reported : in out Node_Sets.Set;
      Root           : in     Node_Access                := Invalid_Node_Access;
      Validate_Node  : in     Syntax_Trees.Validate_Node := null)
   is

      Real_Root : Node_Access;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Access)
      is
         use Ada.Text_IO;

         Node_Image_Output : Boolean := False;

         procedure Put_Error (Msg : in String)
         is begin
            if Error_Reported.Contains (Node) then
               return;
            end if;

            Error_Reported.Insert (Node);

            if not Node_Image_Output then
               Put_Line
                 (Current_Error,
                  Tree.Error_Message
                    (Node,
                     Image (Tree, Node,
                            Children     => False,
                            Node_Numbers => True)));
               Node_Image_Output := True;
            end if;

            Put_Line (Current_Error, Tree.Error_Message (Node, "... invalid_tree: " & Msg));
         end Put_Error;

      begin
         if Node = Real_Root then
            if Node.Parent /= null then
               Put_Error ("root parent set expecting null");
            end if;
         elsif Node.Parent = null then
            Put_Error ("parent null expecting set");
         end if;

         if Node.Label = Nonterm then
            for I in Node.Children'Range loop
               if Node.Children (I) = null then
                  Put_Error ("child" & I'Image & " deleted");

               else
                  declare
                     Child_Parent : constant Node_Access := Node.Children (I).Parent;
                  begin
                     if Child_Parent /= Node then
                        Put_Error
                          ((if Child_Parent = Invalid_Node_Access
                            then "child.parent invalid"
                            else "child.parent incorrect"));
                     end if;
                  end;
               end if;
            end loop;
         end if;

         if Validate_Node /= null then
            declare
               Node_Error_Reported : Boolean := Error_Reported.Contains (Node);
            begin
               Validate_Node (Tree, Node, User_Data, Node_Image_Output, Node_Error_Reported);
               if Node_Error_Reported and then not Error_Reported.Contains (Node) then
                  Error_Reported.Insert (Node);
               end if;
            end;
         end if;
      end Process_Node;

   begin
      if Root /= Invalid_Node_Access then
         Real_Root := Root;
         Process_Tree (Tree, Root, Process_Node'Access);
      else
         if Tree.Streams.Length <= 1 then
            --  Only Shared_Stream exists; probably packrat
            if Tree.Root = Invalid_Node_Access then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Current_Error, Error_Message
                    (Tree.Lexer.File_Name, 1, 1, "... invalid_tree: Tree.Root not set"));
            else
               Real_Root := Tree.Root;
               Process_Tree (Tree, Tree.Root, Process_Node'Access);
            end if;
         else
            for Stream of Tree.Streams loop
               for Element of Stream.Elements loop
                  Real_Root := Element.Node;
                  Process_Tree (Tree, Element.Node, Process_Node'Access);
               end loop;
            end loop;
         end if;
      end if;
   end Validate_Tree;

end WisiToken.Syntax_Trees;
