--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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
      Node   : in     Valid_Node_Access)
     return Terminal_Ref
   with Post => Tree.Valid_Terminal_Ref (Stream, Append_Stream_Element'Result);
   --  Add Node at Stream.Last; if not Shared_Stream, set Stack_Top to
   --  element containing Node. If Node is from Shared_Stream, it has
   --  been copied and State set

   function Child_Index (N : in Node; Child : in Valid_Node_Access) return SAL.Peek_Type;

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Terminal_Ref;
   --  If Before is No_Element, add Node after Stream.Stack_Top (at
   --  beginning of input). Otherwise add Node before Before.
   --
   --  Caller must change Stream.Stack_Top if necessary.

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID;

   function Pop (Parse_Stream : in out Syntax_Trees.Parse_Stream) return Valid_Node_Access;

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

   function Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Node         : in     Valid_Node_Access)
     return Stream_Node_Ref;
   --  Add Node to Parse_Stream at Stack_Top, return reference to it. If
   --  Node is originally from Shared_Stream, it has been copied and
   --  State set.

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in out Valid_Node_Access;
      Children : in     Node_Access_Array);

   function Subtree_Image
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Node_Access;
      Non_Grammar : in Boolean := False;
      Level       : in Integer := 0)
     return String;

   procedure Update_Cache
     (Node      : in Valid_Node_Access;
      Recursive : in Boolean := False)
   with Pre => Node.Label = Nonterm;

   ----------
   --  Public and body operations, alphabetical

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Semantic_Action
   is
      pragma Unreferenced (Tree);
   begin
      return Node.Action;
   end Action;

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index;
      Byte_Region : in     WisiToken.Buffer_Region)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Identifier,
         Child_Count => 0,
         ID          => ID,
         Node_Index  => Tree.Nodes.Last_Index + 1,
         Byte_Region => Byte_Region,
         Identifier  => Identifier,
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Identifier;

   function Add_Nonterm_1
     (Tree            : in out Syntax_Trees.Tree;
      State           : in     Unknown_State_Index;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Action          : in     Semantic_Action;
      Default_Virtual : in     Boolean;
      Clear_Parents   : in     Boolean)
     return Valid_Node_Access
   is
      Nonterm_Node : constant Valid_Node_Access := new Node'
        (Label       => Syntax_Trees.Nonterm,
         Child_Count => Children'Last,
         ID          => Production.LHS,
         Node_Index  => Tree.Nodes.Last_Index + 1,
         State       => State,
         Children    => To_Node_Access (Children),
         Action      => Action,
         RHS_Index   => Production.RHS,
         Virtual     => (if Children'Length = 0 then Default_Virtual else False),
         others      => <>);
   begin
      Tree.Nodes.Append (Nonterm_Node);

      if Children'Length > 0 then
         for Child of Children loop
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
                  pragma Assert (False);
               end if;
            end if;

            Child.Parent := Nonterm_Node;
         end loop;

         Update_Cache (Nonterm_Node);
      end if;

      return Nonterm_Node;
   end Add_Nonterm_1;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Clear_Parents   : in     Boolean;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   is begin
      return Add_Nonterm_1 (Tree, Unknown_State, Production, Children, Action, Default_Virtual, Clear_Parents);
   end Add_Nonterm;

   function Add_Terminal_1
     (Tree       : in out Syntax_Trees.Tree;
      Terminal   : in     WisiToken.Base_Token;
      Node_Index : in     Syntax_Trees.Node_Index := Invalid_Node_Index)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Shared_Terminal,
         Child_Count => 0,
         ID          => Terminal.ID,

         Node_Index  =>
           (if Node_Index = Invalid_Node_Index
            then Tree.Next_Terminal_Node_Index
            else Node_Index),

         Byte_Region => Terminal.Byte_Region,
         Line        => Terminal.Line,
         Char_Region => Terminal.Char_Region,
         others      => <>)
      do
         if Node_Index = Invalid_Node_Index then
            Tree.Next_Terminal_Node_Index := @ + 1;
         end if;
         Tree.Nodes.Append (Result);
      end return;
   end Add_Terminal_1;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Base_Token)
     return Terminal_Ref
   is begin
      return Append_Stream_Element (Tree, Stream, Tree.Add_Terminal_1 (Terminal));
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Base_Token)
     return Valid_Node_Access
   is (Add_Terminal_1 (Tree, Terminal));

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => Tree.Nodes.Last_Index + 1,
         others      => <>)
      do
         Tree.Nodes.Append (Result);
      end return;
   end Add_Terminal;

   function Append_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access)
     return Terminal_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Stream_Element_Lists.Cursor := Parse_Stream.Elements.Append
        ((Node  => Node,
          Label => Parse_Stream.Label));
   begin
      if Stream = Tree.Shared_Stream then
         --  Stack_Top is always Invalid_Stream_Element.
         null;
      else
         Parse_Stream.Stack_Top := New_Element;
      end if;

      return (Element => (Cur => New_Element), Node => Node);
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

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Byte_Region = Null_Buffer_Region;
   end Buffer_Region_Is_Empty;

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

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return SAL.Base_Peek_Type
   is begin
      return Node.Children'Last;
   end Child_Count;

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
            Result (I) := Tree.Get_Recover_Token ((Element, Node.Children (I)));
         end loop;
      end return;
   end Children_Recover_Tokens;

   procedure Clear
     (Tree             : in out Syntax_Trees.Tree;
      Free_Memory      : in     Boolean := False;
      Initialize_Parse : in     Boolean := True)
   is begin
      for N of Tree.Nodes loop
         Free (N);
      end loop;
      Tree.Nodes.Clear (Free_Memory);

      Tree.Leading_Non_Grammar.Clear (Free_Memory);
      Tree.Streams.Clear;

      Tree.Root                     := Invalid_Node_Access;
      Tree.Next_Stream_Label        := Shared_Stream_Label + 1;
      Tree.Next_Terminal_Node_Index := 1;
      Tree.Traversing               := False;
      Tree.Incremental_Parse        := False;

      if Initialize_Parse then
         --  Set up for new parse:
         Tree.Shared_Stream :=
           (Cur           => Tree.Streams.Append
              ((Label     => Shared_Stream_Label,
                Stack_Top => Stream_Element_Lists.No_Element,
                Elements  => <>)));
      end if;
   end Clear;

   procedure Clear_Parse_Streams (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Root = Invalid_Node_Access then
         Tree.Root := Syntax_Trees.Root (Tree);
      end if;

      Tree.Streams.Clear;

      Tree.Shared_Stream.Cur := Parse_Stream_Lists.No_Element;
   end Clear_Parse_Streams;

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
         when Shared_Terminal =>

            New_Node := new Syntax_Trees.Node'
              (Label       => Shared_Terminal,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Node.Node_Index, --  source text order
               Byte_Region => Node.Byte_Region,
               Parent      => Parent,
               State       => Node.State,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               Non_Grammar => Node.Non_Grammar,
               Line        => Node.Line,
               Char_Region => Node.Char_Region);

            Tree.Nodes.Append (New_Node);

         when Virtual_Terminal     =>

            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Tree.Nodes.Last_Index + 1,
               Byte_Region => Node.Byte_Region,
               Char_Region => Node.Char_Region,
               Line        => Node.Line,
               Parent      => Parent,
               State       => Node.State,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)));

            Tree.Nodes.Append (New_Node);

         when Virtual_Identifier =>

            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Child_Count => 0,
               ID          => Node.ID,
               Node_Index  => Tree.Nodes.Last_Index + 1,
               Byte_Region => Node.Byte_Region,
               Char_Region => Node.Char_Region,
               Line        => Node.Line,
               Parent      => Parent,
               State       => Node.State,
               Augmented   =>
                 (if Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Node.Augmented)),
               VI_Non_Grammar => Node.VI_Non_Grammar,
               Identifier     => Node.Identifier);

            Tree.Nodes.Append (New_Node);

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
                  Node_Index  => Tree.Nodes.Last_Index + 1,
                  Byte_Region => Node.Byte_Region,
                  Char_Region => Node.Char_Region,
                  Line        => Node.Line,
                  Parent      => Parent,
                  State       => Node.State,
                  Augmented   =>
                    (if Node.Augmented = null or User_Data = null
                     then null
                     else Copy_Augmented (User_Data.all, Node.Augmented)),
                  Virtual               => Node.Virtual,
                  RHS_Index             => Node.RHS_Index,
                  Action                => Node.Action,
                  Name                  => Node.Name,
                  Children              => New_Children,
                  First_Shared_Terminal => Node.First_Shared_Terminal);

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
         when Shared_Terminal =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Shared_Terminal,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => Source_Node.Node_Index,
               Byte_Region => Source_Node.Byte_Region,
               Parent      => Dest_Parent,
               State       => Source_Node.State,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               Non_Grammar => Source_Node.Non_Grammar,
               Line        => Source_Node.Line,
               Char_Region => Source_Node.Char_Region);

            Destination.Nodes.Append (New_Dest_Node);

         when Virtual_Terminal =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => Destination.Nodes.Last_Index + 1,
               Byte_Region => Source_Node.Byte_Region,
               Char_Region => Source_Node.Char_Region,
               Line        => Source_Node.Line,
               Parent      => Dest_Parent,
               State       => Source_Node.State,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)));

            Destination.Nodes.Append (New_Dest_Node);

         when Virtual_Identifier =>

            New_Dest_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Child_Count => 0,
               ID          => Source_Node.ID,
               Node_Index  => Destination.Nodes.Last_Index + 1,
               Byte_Region => Source_Node.Byte_Region,
               Char_Region => Source_Node.Char_Region,
               Line        => Source_Node.Line,
               Parent      => Dest_Parent,
               State       => Source_Node.State,
               Augmented   =>
                 (if Source_Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
               VI_Non_Grammar => Source_Node.VI_Non_Grammar,
               Identifier     => Source_Node.Identifier);

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
                  Node_Index  => Destination.Nodes.Last_Index + 1,
                  Byte_Region => Source_Node.Byte_Region,
                  Char_Region => Source_Node.Char_Region,
                  Line        => Source_Node.Line,
                  Parent      => Dest_Parent,
                  State       => Source_Node.State,
                  Augmented   =>
                    (if Source_Node.Augmented = null or User_Data = null
                     then null
                     else Copy_Augmented (User_Data.all, Source_Node.Augmented)),
                  Virtual               => Source_Node.Virtual,
                  RHS_Index             => Source_Node.RHS_Index,
                  Action                => Source_Node.Action,
                  Name                  => Source_Node.Name,
                  Children              => New_Children,
                  First_Shared_Terminal => Source_Node.First_Shared_Terminal);

               for Child of New_Dest_Node.Children loop
                  Child.Parent := New_Dest_Node;
               end loop;

               Destination.Nodes.Append (New_Dest_Node);
            end;
         end case;
         return New_Dest_Node;
      end Copy_Node;
   begin
      Destination.Clear (Free_Memory => False, Initialize_Parse => False);
      Destination.Leading_Non_Grammar := Source.Leading_Non_Grammar;
      Destination.Traversing          := False;

      if Source.Streams.Length = 2 then
         Destination.Shared_Stream :=
           (Cur           => Destination.Streams.Append
              ((Label     => Shared_Stream_Label,
                Stack_Top => Stream_Element_Lists.No_Element,
                Elements  => <>)));

         for Element of Source.Streams (Source.Shared_Stream.Cur).Elements loop
            declare
               Ref : constant Terminal_Ref := Add_Terminal
                 (Destination, Destination.Shared_Stream, Source.Base_Token (Element.Node));
            begin
               Ref.Node.Non_Grammar := Element.Node.Non_Grammar;
            end;
         end loop;

         declare
            Source_Parse_Stream : Parse_Stream renames Source.Streams (Source.Streams.Last);
            Source_Root_Cur     : constant Stream_Element_Lists.Cursor := Stream_Element_Lists.Previous
              (Source_Parse_Stream.Elements.Last);
            Source_Root_Element : Stream_Element renames Source_Parse_Stream.Elements (Source_Root_Cur);
            Dest_Stream_ID      : constant Stream_ID                   := New_Stream (Destination);
            Dest_Shared_Stream  : Parse_Stream renames Destination.Streams (Destination.Shared_Stream.Cur);
            Dest_Parse_Stream   : Parse_Stream renames Destination.Streams (Dest_Stream_ID.Cur);
            New_Element         : Stream_Element_Lists.Cursor;
         begin
            Start_Parse
              (Destination, Dest_Stream_ID, State => Source_Root_Element.Node.State);

            Destination.Root := Copy_Node (Source_Root_Element.Node, Invalid_Node_Access);

            New_Element := Dest_Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => Destination.Root,
                  Label => Dest_Parse_Stream.Label),
               Before   => Stream_Element_Lists.No_Element);

            Dest_Parse_Stream.Stack_Top := New_Element;

            Finish_Parse (Destination, Dest_Stream_ID, (Cur => Dest_Shared_Stream.Elements.Last), User_Data);

            if Source.Root = null then
               Destination.Root := null;
            end if;
         end;
      else
         Destination.Root := Copy_Node (Source.Root, Invalid_Node_Access);
      end if;
   end Copy_Tree;

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
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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

   procedure Delete_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Access)
   is
   begin
      Node.Parent.Children (Child_Index (Node.Parent.all, Node)) := null;

      if Node.Parent = Tree.Root then
         Tree.Root := Node;
      end if;

      Node.Parent := null;
   end Delete_Parent;

   procedure Delete_Stream (Tree : in out Syntax_Trees.Tree; Stream : in out Stream_ID)
   is
      use Parse_Stream_Lists;
   begin
      Tree.Streams.Delete (Stream.Cur);
   end Delete_Stream;

   function Error_Message
     (Tree                : in Syntax_Trees.Tree;
      Node                : in Valid_Node_Access;
      Line_Begin_Char_Pos : in Line_Pos_Vectors.Vector;
      File_Name           : in String;
      Message             : in String)
     return String
   is
      First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
      Line           : Line_Number_Type     := Line_Number_Type'First;
      Column         : Ada.Text_IO.Count    := Ada.Text_IO.Count'First;
   begin
      if First_Terminal = Invalid_Node_Access then
         --  Node is empty
         null;
      else
         case Tree.Label (First_Terminal) is
         when Shared_Terminal =>
            Line := First_Terminal.Line;

            if Line in Line_Begin_Char_Pos.First_Index .. Line_Begin_Char_Pos.Last_Index then
               Column := Ada.Text_IO.Count (First_Terminal.Char_Region.First - Line_Begin_Char_Pos (Line));
            end if;

         when Virtual_Terminal | Virtual_Identifier =>
            null;

         when others =>
            null;
         end case;
      end if;
      return WisiToken.Error_Message (File_Name, Line, Column, Message);
   end Error_Message;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree)
   is begin
      Clear (Tree, Free_Memory => True, Initialize_Parse => False);
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

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access;
      ID   : in Token_ID)
     return Node_Access
   is begin
      case Node.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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

   procedure Finish_Parse
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      Token     : in     Stream_Index;
      User_Data : in     User_Data_Access)
   is
      Terminal_Element : Stream_Element renames Stream_Element_Lists.Constant_Ref (Token.Cur);
   begin
      if Terminal_Element.Label = Shared_Stream_Label then
         declare
            New_Node : constant Node_Access := new Node'
              (Label       => Shared_Terminal,
               Child_Count => 0,
               ID          => Terminal_Element.Node.ID,
               Node_Index  => Terminal_Element.Node.Node_Index, --  Preserve source text order.
               Byte_Region => Terminal_Element.Node.Byte_Region,
               Line        => Terminal_Element.Node.Line,
               Parent      => Invalid_Node_Access,
               State       => Unknown_State,
               Augmented   =>
                 (if Terminal_Element.Node.Augmented = null or User_Data = null
                  then null
                  else Copy_Augmented (User_Data.all, Terminal_Element.Node.Augmented)),
               Char_Region => Terminal_Element.Node.Char_Region,
               Non_Grammar => Terminal_Element.Node.Non_Grammar);

            Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
         begin
            Tree.Nodes.Append (New_Node);

            Parse_Stream.Elements.Insert
              (Element  =>
                 (Node  => New_Node,
                  Label => Parse_Stream.Label),
               Before   => Stream_Element_Lists.No_Element);
         end;
      end if;
   end Finish_Parse;

   function First_Input
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
     return Terminal_Ref
   is
      use Stream_Element_Lists;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      return Result : Terminal_Ref do
         Result.Element.Cur := Next (Parse_Stream.Stack_Top);
         Result.Node := First_Shared_Terminal (Tree, Constant_Ref (Result.Element.Cur).Node);
      end return;
   end First_Input;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
   begin
      case Node.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node;
      when Nonterm =>
         for C of Node.Children loop
            --  This may be called from Error_Message
            if C /= null then
               declare
                  Term : constant Node_Access := First_Terminal (Tree, C);
               begin
                  if Term /= Invalid_Node_Access then
                     return Term;
                  end if;
               end;
            end if;
         end loop;
         return Invalid_Node_Access;
      end case;
   end First_Terminal;

   function First_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is begin
      case Node.Label is
      when Shared_Terminal =>
         return Node;
      when Virtual_Terminal | Virtual_Identifier =>
         return Invalid_Node_Access;

      when Nonterm =>
         for C of Node.Children loop
            declare
               Term : constant Node_Access := First_Shared_Terminal (Tree, C);
            begin
               if Term /= Invalid_Node_Access then
                  return Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Access;
      end case;
   end First_Shared_Terminal;

   function First_Shared_Terminal
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Terminal_Ref
   is
      pragma Unreferenced (Stream);
   begin
      return
        (Element => Element,
         Node    => First_Shared_Terminal (Tree, Stream_Element_Lists.Constant_Ref (Element.Cur).Node));
   end First_Shared_Terminal;

   function First_Terminal_ID (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Token_ID
   is
   begin
      case Node.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node.ID;

      when Nonterm =>
         for C of Node.Children loop
            --  Encountering a deleted child here is an error in the user
            --  algorithm.
            declare
               ID : constant Token_ID := First_Terminal_ID (Tree, C);
            begin
               if ID /= Invalid_Token_ID then
                  return ID;
               end if;
            end;
         end loop;
         return Invalid_Token_ID;
      end case;
   end First_Terminal_ID;

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
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
      case Ref.Node.Label is
      when Shared_Terminal =>
         return
           (ID                    => Ref.Node.ID,
            Byte_Region           => Ref.Node.Byte_Region,
            First_Shared_Terminal => Ref,
            Name                  => Null_Buffer_Region,
            Virtual               => False);

      when Virtual_Terminal | Virtual_Identifier =>
         return
           (ID                    => Ref.Node.ID,
            Byte_Region           => Null_Buffer_Region,
            First_Shared_Terminal => Ref,
            Name                  => Null_Buffer_Region,
            Virtual               => True);

      when Nonterm =>
         return
           (ID                    => Ref.Node.ID,
            Byte_Region           => Ref.Node.Byte_Region,
            First_Shared_Terminal => (Ref.Element, Ref.Node.First_Shared_Terminal),
            Name                  => Ref.Node.Name,
            Virtual               => Ref.Node.Virtual);
      end case;
   end Get_Recover_Token;

   procedure Get_Terminals
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Result : in out Valid_Node_Access_Array;
      Last   : in out SAL.Base_Peek_Type)
   is begin
      case Node.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Token_ID
   is begin
      return Node.ID;
   end ID;

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Base_Identifier_Index
   is begin
      return Node.Identifier;
   end Identifier;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      return
        (if Item.First_Shared_Terminal = Invalid_Terminal_Ref
         then ""
         else Trimmed_Image (Item.First_Shared_Terminal.Node.Node_Index) & ":") &
        "(" & Image (Item.ID, Descriptor) &
        (if Item.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Item.Byte_Region)) & ")";
   end Image;

   function Image
     (Tree        : in Syntax_Trees.Tree;
      Stream      : in Parse_Stream;
      Children    : in Boolean := False;
      Non_Grammar : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      use Stream_Element_Lists;
      Result     : Unbounded_String := +"(";
      Element    : Cursor           := Stream.Elements.First;
      Need_Comma : Boolean          := False;
   begin
      loop
         exit when not Has_Element (Element);
         if Need_Comma then
            Result := @ & (if Children then "," & ASCII.LF else ", ");
         else
            Need_Comma := True;
         end if;
         Result := @ &
           (if Stream.Stack_Top = Element then "^(" else "(") &
           Trimmed_Image (Constant_Ref (Element).Node.State) & ", " &
           (if Children and Constant_Ref (Element).Node.Label = Nonterm
            then Tree.Subtree_Image (Constant_Ref (Element).Node, Non_Grammar => Non_Grammar)
            else Tree.Image (Constant_Ref (Element).Node, Node_Numbers => True, Non_Grammar => Non_Grammar))
           & ")";

         Element := Next (Element);
      end loop;
      Result := @ & ")";
      return -Result;
   end Image;

   function Image
     (Tree        : in Syntax_Trees.Tree;
      Children    : in Boolean := False;
      Non_Grammar : in Boolean := False;
      Root        : in Node_Access := Invalid_Node_Access)
     return String
   is begin
      if Root /= Invalid_Node_Access then
         --  Assuming children = true in this case.
         return Subtree_Image (Tree, Root, Non_Grammar);

      elsif Tree.Streams.Length = 0 then
         if Tree.Root = Invalid_Node_Access then
            return "invalid_tree: no streams, Tree.Root not set";
         else
            --  Assuming children = true in this case.
            return Subtree_Image (Tree, Tree.Root, Non_Grammar);
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
               Result := @ & Image (Tree, Stream, Children, Non_Grammar);
            end loop;
            return -Result;
         end;
      end if;
   end Image;

   function Image
     (Tree        : in Syntax_Trees.Tree;
      Stream      : in Stream_ID;
      Children    : in Boolean := False;
      Non_Grammar : in Boolean := False)
     return String
   is begin
      return Image (Tree, Tree.Streams (Stream.Cur), Children, Non_Grammar);
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Element               : in Stream_Index;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False)
     return String
   is
      use all type Stream_Element_Lists.Cursor;
   begin
      if Element.Cur = Stream_Element_Lists.No_Element then
         return "<deleted>";
      else
         return Image
           (Tree, Stream_Element_Lists.Constant_Ref (Element.Cur).Node, Children,
            RHS_Index, Node_Numbers, Terminal_Node_Numbers);
      end if;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Node                  : in Node_Access;
      Children              : in Boolean := False;
      RHS_Index             : in Boolean := False;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Non_Grammar           : in Boolean := False)
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
                   when Shared_Terminal    => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Terminal   => Trimmed_Image (Node.Node_Index) & ":",
                   when Virtual_Identifier => Trimmed_Image (Node.Identifier) & ":",
                   when Nonterm            => "")
                elsif Node_Numbers
                then Trimmed_Image (Node.Node_Index) & ":"
                else "");
         begin
            Result := Result & "(" & Image (Node.ID, Tree.Descriptor.all) &
              (if RHS_Index and Node.Label = Nonterm then "_" & Trimmed_Image (Node.RHS_Index) else "") &
              (if Node.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Node.Byte_Region)) & ")";

            if Children and Node.Label = Nonterm then
               Result := Result & " <= " & Image (Tree, Node.Children, Node_Numbers, Terminal_Node_Numbers);
            end if;

            if Non_Grammar then
               case Node.Label is
               when Shared_Terminal =>
                  Result := @ & Image (Node.Non_Grammar, Tree.Descriptor.all);
               when Virtual_Identifier =>
                  Result := @ & Image (Node.VI_Non_Grammar, Tree.Descriptor.all);
               when others =>
                  null;
               end case;
            end if;

            return -Result;
         end;
      end if;
   end Image;

   function Image
     (Tree                  : in Syntax_Trees.Tree;
      Nodes                 : in Node_Access_Array;
      Node_Numbers          : in Boolean := False;
      Terminal_Node_Numbers : in Boolean := False;
      Non_Grammar           : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           (if Nodes (I) = null then " - "
            else Tree.Image (Nodes (I), Node_Numbers => Node_Numbers, Terminal_Node_Numbers => Terminal_Node_Numbers,
                             Non_Grammar => Non_Grammar));
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image (Tree : in Syntax_Trees.Tree; Ref : in Stream_Node_Ref) return String
   is begin
      if Tree.Incremental_Parse then
         return "(" & Image (Tree, Ref.Element, Node_Numbers => True) & ", " &
            Image (Tree, Ref.Node, Terminal_Node_Numbers => True) & ")";
      else
         return Image (Tree, Ref.Node, Node_Numbers => True);
      end if;
   end Image;

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Insert_Token         : in     Valid_Node_Access;
      Insert_Before_Token  : in     Valid_Node_Access;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean
   is
      pragma Unreferenced (User_Data, Tree, Insert_Token, Insert_Before_Token, Insert_On_Blank_Line);
   begin
      return False;
   end Insert_After;

   function Insert_Shared_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Base_Token;
      Index    : in     Node_Index;
      Before   : in     Stream_Index)
     return Terminal_Ref
   is
      New_Node : constant Valid_Node_Access := Add_Terminal_1 (Tree, Terminal, Index);
   begin
      return Insert_Stream_Element (Tree, Stream, New_Node, Before => Before.Cur);
   end Insert_Shared_Terminal;

   function Insert_Stream_Element
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Node   : in     Valid_Node_Access;
      Before : in     Stream_Element_Lists.Cursor := Stream_Element_Lists.No_Element)
     return Terminal_Ref
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      New_Element  : constant Cursor := Parse_Stream.Elements.Insert
        (Element =>
           (Node  => Node,
            Label => Parse_Stream.Label),
         Before =>
           (if Before /= No_Element
            then Before
            else
              (if Parse_Stream.Stack_Top = No_Element
               then No_Element
               else Next (Parse_Stream.Stack_Top))));
   begin
      return (Element => (Cur => New_Element), Node => Node);
   end Insert_Stream_Element;

   function Insert_Virtual_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID;
      Before   : in     Stream_Index)
     return Terminal_Ref
   is
      New_Node : constant Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Node_Index  => Tree.Nodes.Last_Index + 1,
         Line        => Stream_Element_Lists.Element (Before.Cur).Node.Line,
         Byte_Region => Null_Buffer_Region, -- for "is empty"
         Char_Region =>
           (First | Last => Stream_Element_Lists.Element (Before.Cur).Node.Char_Region.First), --  for indent
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

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Nonterm;
   end Is_Nonterm;

   function Is_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Shared_Terminal;
   end Is_Shared_Terminal;

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

   function Last_Shared_Terminal
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Node_Access
   is begin
      case Node.Label is
      when Shared_Terminal =>
         return Node;

      when Virtual_Terminal | Virtual_Identifier =>
         return Invalid_Node_Access;

      when Nonterm =>
         for C of reverse Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            declare
               Last_Term : constant Node_Access := Last_Shared_Terminal (Tree, C);
            begin
               if Last_Term /= Invalid_Node_Access then
                  return Last_Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Access;
      end case;
   end Last_Shared_Terminal;

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
   begin
      case Node.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
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

   function Leading_Non_Grammar (Tree : aliased in out Syntax_Trees.Tree) return Base_Token_Array_Var_Ref
   is begin
      return (Element => Tree.Leading_Non_Grammar'Access, Dummy => 0);
   end Leading_Non_Grammar;

   function Leading_Non_Grammar_Const (Tree : aliased in Syntax_Trees.Tree) return Base_Token_Array_Const_Ref
   is begin
      return (Element => Tree.Leading_Non_Grammar'Access, Dummy => 0);
   end Leading_Non_Grammar_Const;

   procedure Left_Breakdown
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   is
      --  [Wagner Graham 1998] doesn't modify the tree structure for
      --  Left_Breakdown; it just moves the Current_Token pointer around.
      --  That means the rest of the parser must understand that.
      --
      --  Here we actually decompose the tree, as in [Lahav 2008]
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Cur          : Stream_Element_Lists.Cursor := Ref.Element.Cur;
      Node         : Valid_Node_Access           := Parse_Stream.Elements (Cur).Node;
   begin
      loop
         if Node.Child_Count > 0 then
            if Node.Label in Terminal_Label then
               Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Node,
                     Label => Parse_Stream.Label),
                  Before   => Cur);

               Ref.Element.Cur := Cur;
               exit;
            end if;

            for I in reverse 2 .. Node.Child_Count loop
               Cur := Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Node.Children (I),
                     Label => Parse_Stream.Label),
                  Before   => Cur);

               Node.Children (I).Parent := Invalid_Node_Access;
            end loop;

            Node := Node.Children (1);

         else
            --  Cur is an empty nonterm
            Cur := Stream_Element_Lists.Next (Cur);
            if Stream_Element_Lists.Has_Element (Cur) then
               Node := Parse_Stream.Elements (Cur).Node;
            else
               Ref.Element.Cur := Stream_Element_Lists.No_Element;
               exit;
            end if;
         end if;
      end loop;

      Ref.Node :=
        (if Ref.Element = Invalid_Stream_Index
         then Invalid_Node_Access
         else Stream_Element_Lists.Constant_Ref (Ref.Element.Cur).Node);
   end Left_Breakdown;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID
   is begin
      return Result : constant Stream_ID :=
        (Cur           => Tree.Streams.Append
           ((Label     => Tree.Next_Stream_Label,
             Stack_Top => Invalid_Stream_Index.Cur,
             Elements  => <>)))
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
            use all type Stream_Element_Lists.Cursor;
            Old_Parse_Stream : Parse_Stream renames Tree.Streams (Old_Stream.Cur);
            Old_Stack_Top    : constant Stream_Element_Lists.Cursor := Old_Parse_Stream.Stack_Top;

            Result_Cur : constant Parse_Stream_Lists.Cursor := Tree.Streams.Append
              ((Label     => Tree.Next_Stream_Label,
                Stack_Top => Invalid_Stream_Index.Cur,
                Elements => <>));

            New_Stream : Parse_Stream renames Tree.Streams (Result_Cur);

            New_Cur : Stream_Element_Lists.Cursor;
         begin
            for Old_Cur in Old_Parse_Stream.Elements.Iterate loop
               declare
                  --  FIXME: this copies the entire tree for each new parser; will be very slow
                  --  Only copy input stream (after stack top); requires not setting parents until done.
                  New_Node : constant Valid_Node_Access :=
                    Copy_Subtree (Tree, Stream_Element_Lists.Constant_Ref (Old_Cur).Node, User_Data);
               begin
                  New_Cur := New_Stream.Elements.Append
                    ((Node  => New_Node,
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

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      function First_Child (Node : in Valid_Node_Access) return Node_Access
      is
      begin
         case Node.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
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
            return Invalid_Node_Access;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Access; Node : in Node_Access) return Node_Access
      is begin
         --  Node is Parent of Child; return node immediately after Child.
         if Node = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            pragma Assert (Node.Label = Nonterm);
            for I in Node.Children'Range loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               if Node.Children (I) = Child then
                  --  Use first non-empty next from I + 1.
                  for J in I + 1 .. Node.Children'Last loop
                     declare
                        Result : constant Node_Access := First_Child (Node.Children (J));
                     begin
                        if Result /= Invalid_Node_Access then
                           return Result;
                        end if;
                     end;
                  end loop;
                  --  All next Children are empty
                  return Next_Child (Node, Node.Parent);
               end if;
            end loop;
            raise SAL.Programmer_Error;
         end if;
      end Next_Child;
   begin
      return Next_Child (Node, Node.Parent);
   end Next_Terminal;

   function Next_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      Result : Node_Access := Node;
   begin
      loop
         Result := Next_Terminal (Tree, Result);
         if Result = Invalid_Node_Access or else Result.Label = Shared_Terminal then
            return Result;
         end if;
      end loop;
   end Next_Shared_Terminal;

   procedure Next_Shared_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Ref.Node.ID = Tree.Descriptor.EOI_ID then
         Ref := Invalid_Terminal_Ref;
         return;
      end if;

      loop
         Ref.Node := Next_Terminal (Tree, Ref.Node);
         if Ref.Node = Invalid_Node_Access then
            loop
               Ref.Element := (Cur => Stream_Element_Lists.Next (Ref.Element.Cur));
               if Ref.Element = Invalid_Stream_Index then
                  return;
               end if;

               Ref.Node := Tree.First_Shared_Terminal (Parse_Stream.Elements (Ref.Element.Cur).Node);
               if Ref.Node /= Invalid_Node_Access then
                  return;
               end if;
            end loop;

         elsif Ref.Node.Label = Shared_Terminal then
            return;
         end if;
      end loop;
   end Next_Shared_Terminal;

   function Next_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Terminal_Ref)
     return Terminal_Ref
   is
      Temp_Ref : Terminal_Ref := Ref;
   begin
      Next_Shared_Terminal (Tree, Stream, Temp_Ref);
      return Temp_Ref;
   end Next_Shared_Terminal;

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result
   is
     --  Within one subtree, Node_Index is unique within Nonterms and
     --  Terminals separately.
     (if Left.Label > Right.Label then SAL.Greater
      elsif Left.Label < Right.Label then SAL.Less
      elsif Left.Node_Index > Right.Node_Index then SAL.Greater
      elsif Left.Node_Index < Right.Node_Index then SAL.Less
      else SAL.Equal);

   function Non_Grammar_Var
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Valid_Node_Access)
     return Base_Token_Array_Var_Ref
   is
      pragma Unreferenced (Tree);
   begin
      return
        (Element =>
           (case Terminal.Label is
            when Shared_Terminal    => Terminal.Non_Grammar'Access,
            when Virtual_Identifier => Terminal.VI_Non_Grammar'Access,
            when others             => raise SAL.Programmer_Error),
         Dummy => 0);
   end Non_Grammar_Var;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Array_Const_Ref
   is begin
      return
        (Element =>
           (case Terminal.Label is
            when Shared_Terminal    => Terminal.Non_Grammar'Access,
            when Virtual_Identifier => Terminal.VI_Non_Grammar'Access,
            when others             => raise SAL.Programmer_Error),
         Dummy => 0);
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
      end return;
   end Pop;

   procedure Pop (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Junk : Valid_Node_Access := Pop (Parse_Stream);
      pragma Unreferenced (Junk);
   begin
      null;
   end Pop;

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      function Last_Child (Node : in Valid_Node_Access) return Node_Access
      is begin
         case Node.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
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
            return Invalid_Node_Access;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Access; Node : in Node_Access) return Node_Access
      is begin
         --  Node is Parent of Child; return node immediately previous to Child.
         if Node = Invalid_Node_Access then
            return Invalid_Node_Access;
         else
            pragma Assert (Node.Label = Nonterm);
            for I in reverse Node.Children'Range loop
               --  Encountering a deleted child here is an error in the user
               --  algorithm.
               if Node.Children (I) = Child then
                  --  Use first non-empty from I - 1.
                  for J in reverse Node.Children'First .. I - 1 loop
                     declare
                        Result : constant Node_Access := Last_Child (Node.Children (J));
                     begin
                        if Result /= Invalid_Node_Access then
                           return Result;
                        end if;
                     end;
                  end loop;
                  --  All previous Children are empty
                  return Prev_Child (Node, Node.Parent);
               end if;
            end loop;
            raise SAL.Programmer_Error;
         end if;
      end Prev_Child;
   begin
      return Prev_Child (Node, Node.Parent);
   end Prev_Terminal;

   procedure Prev_Shared_Terminal
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      Ref    : in out Terminal_Ref)
   is
      use all type Stream_Element_Lists.Cursor;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Ref.Element.Cur = Parse_Stream.Elements.First then
         Ref := Invalid_Terminal_Ref;
         return;
      end if;

      loop
         Ref.Node := Prev_Terminal (Tree, Ref.Node);
         if Ref.Node = Invalid_Node_Access then
            Ref.Element := (Cur => Stream_Element_Lists.Next (Ref.Element.Cur));
            if Ref.Element = Invalid_Stream_Index then
               return;
            end if;

            Ref.Node := Tree.Last_Shared_Terminal (Parse_Stream.Elements (Ref.Element.Cur).Node);
            if Ref.Node /= Invalid_Node_Access then
               return;
            end if;

         elsif Ref.Node.Label = Shared_Terminal then
            return;
         end if;
      end loop;
   end Prev_Shared_Terminal;

   function Prev_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID;
      Ref    : in Terminal_Ref)
     return Terminal_Ref
   is
      Temp_Ref : Terminal_Ref := Ref;
   begin
      Tree.Prev_Shared_Terminal (Stream, Temp_Ref);
      return Temp_Ref;
   end Prev_Shared_Terminal;

   procedure Print_Streams (Tree : in Syntax_Trees.Tree; Non_Grammar : in Boolean := False)
   is begin
      for Stream of Tree.Streams loop
         Ada.Text_IO.Put_Line (Stream.Label'Image & ": " & Tree.Image (Stream, Non_Grammar => Non_Grammar));
      end loop;
   end Print_Streams;

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Root            : in Node_Access                  := Invalid_Node_Access;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null;
      Non_Grammar     : in Boolean                      := False)
   is
      use Ada.Text_IO;

      Node_Printed : Node_Sets.Set;

      procedure Print_Node (Node : in Valid_Node_Access; Level : in Integer)
      is begin
         if Node_Printed.Contains (Node) then
            --  This does not catch all possible tree edit errors, but it does
            --  catch circles.
            raise SAL.Programmer_Error with "Print_Tree: invalid tree; loop:" & Node.Label'Image & " " &
              Node.Node_Index'Image;
         else
            Node_Printed.Insert (Node);
         end if;

         Put (Decimal_Image (Node.Node_Index, Width => 4) & ": ");
         for I in 1 .. Level loop
            Put ("| ");
         end loop;
         Put (Image (Tree, Node, Children => False, RHS_Index => True, Terminal_Node_Numbers => True,
                     Non_Grammar => Non_Grammar));

         if Image_Augmented /= null and Node.Augmented /= null then
            Put (" - " & Image_Augmented (Augmented_Class_Access_Constant (Node.Augmented)));
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
   begin
      if Print_Root = Invalid_Node_Access then
         Put_Line ("<empty tree>");
      else
         Print_Node (Print_Root, 0);
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
      Node         : in     Valid_Node_Access)
     return Stream_Node_Ref
   is
      use Stream_Element_Lists;
      New_Element : constant Cursor := Parse_Stream.Elements.Insert
        (Element  =>
           (Node  => Node,
            Label => Parse_Stream.Label),
         Before   => Next (Parse_Stream.Stack_Top));
   begin
      Parse_Stream.Stack_Top := New_Element;
      return ((Cur => New_Element), Node);
   end Push;

   procedure Push
     (Parse_Stream : in out Syntax_Trees.Parse_Stream;
      Node         : in     Valid_Node_Access)
   is
      Junk : Stream_Node_Ref := Push (Parse_Stream, Node);
      pragma Unreferenced (Junk);
   begin
      null;
   end Push;

   function Reduce
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Child_Count     : in     Ada.Containers.Count_Type;
      Action          : in     Semantic_Action := null;
      State           : in     Unknown_State_Index;
      Default_Virtual : in     Boolean         := False)
     return Stream_Node_Ref
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);

      function Pop_Children return Valid_Node_Access_Array
      is begin
         return Result : Valid_Node_Access_Array (1 .. SAL.Base_Peek_Type (Child_Count)) := (others => Dummy_Node) do
            --  FIXME: use iterated_component_association to avoid bogus init
            for I in reverse Result'Range loop
               Result (I) := Pop (Parse_Stream);
            end loop;
         end return;
      end Pop_Children;

      New_Node : constant Node_Access := Tree.Add_Nonterm_1
        (State, Production, Pop_Children, Action, Default_Virtual, Clear_Parents => False);
   begin
      return Push (Parse_Stream, New_Node);
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

      Update_Cache (Parent);
   end Replace_Child;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Natural
   is begin
      return Node.RHS_Index;
   end RHS_Index;

   procedure Right_Breakdown
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      use Stream_Element_Lists;

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Before       : constant Cursor := Next (Parse_Stream.Stack_Top);
   begin
      loop
         exit when Parse_Stream.Elements (Parse_Stream.Stack_Top).Node.Label in Terminal_Label;
         declare
            Temp : Cursor := Parse_Stream.Stack_Top;
            Node : constant Valid_Node_Access := Parse_Stream.Elements (Parse_Stream.Stack_Top).Node;
         begin
            Parse_Stream.Stack_Top := Previous (@);
            Parse_Stream.Elements.Delete (Temp);

            for Child of Node.Children loop
               Parse_Stream.Stack_Top := Parse_Stream.Elements.Insert
                 (Element  =>
                    (Node  => Child,
                     Label => Parse_Stream.Label),
                  Before   => Before);

               Child.Parent := Invalid_Node_Access;
               Child.State  := Unknown_State;
            end loop;
         end;
      end loop;
   end Right_Breakdown;

   function Root (Tree : in Syntax_Trees.Tree) return Node_Access
   is begin
      if Tree.Root = Invalid_Node_Access then
         declare
            use Stream_Element_Lists;
            Stream : Parse_Stream renames Tree.Streams (Tree.Streams.Last);
         begin
            return Stream.Elements (Stream.Stack_Top).Node;
         end;
      else
         return Tree.Root;
      end if;
   end Root;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Access;
      Value : in     Augmented_Class_Access)
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
         Parent.Byte_Region := Null_Buffer_Region;
         Parent.Line        := Invalid_Line_Number;
         Parent.Virtual     := False;
         Parent.Children    := Children;

      else
         --  reallocate node with new child_count
         declare
            Realloc_Parent : constant Node_Access := new Node'
              (Label                 => Nonterm,
               Child_Count           => Children'Last,
               ID                    => Parent.ID,
               Node_Index            => Tree.Nodes.Last_Index + 1,
               Byte_Region           => Null_Buffer_Region,
               Char_Region           => Null_Buffer_Region,
               Line                  => Invalid_Line_Number,
               Parent                => Parent.Parent,
               State                 => Parent.State,
               Augmented             => Parent.Augmented,
               Virtual               => False,
               RHS_Index             => Parent.RHS_Index,
               Action                => Parent.Action,
               Name                  => Parent.Name,
               Children              => Children,
               First_Shared_Terminal => Parent.First_Shared_Terminal);
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

      Update_Cache (Parent);
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

   procedure Set_Terminal_Index
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Valid_Node_Access;
      Index    : in     Node_Index)
   is begin
      Terminal.Node_Index := Index;
   end Set_Terminal_Index;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Region : in     Buffer_Region)
   is begin
      Node.Name := Region;
   end Set_Name_Region;

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; New_Root : in Valid_Node_Access)
   is begin
      Tree.Root := New_Root;
   end Set_Root;

   procedure Set_Stack_Top
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in     Stream_Index)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      Parse_Stream.Stack_Top := Element.Cur;
   end Set_Stack_Top;

   procedure Shift
     (Tree      : in out Syntax_Trees.Tree;
      Stream    : in     Stream_ID;
      State     : in     Unknown_State_Index;
      Token     : in     Stream_Index;
      User_Data : in     User_Data_Access)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Element      : Stream_Element renames Stream_Element_Lists.Constant_Ref (Token.Cur);
   begin
      if Element.Label = Shared_Stream_Label then
         Push (Parse_Stream, Copy_Subtree (Tree, Element.Node, User_Data));
      else
         Parse_Stream.Stack_Top := Token.Cur;
      end if;

      Parse_Stream.Elements (Parse_Stream.Stack_Top).Node.State := State;
   end Shift;

   procedure Shift
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Shift_Bytes : in Base_Buffer_Pos;
      Shift_Chars : in Base_Buffer_Pos;
      Shift_Line  : in Base_Line_Number_Type)
   is begin
      Node.Byte_Region := @ + Shift_Bytes;
      Node.Char_Region := @ + Shift_Chars;
      Node.Line        := @ + Shift_Line;
      for Token of Node.Non_Grammar loop
         Token.Byte_Region := @ + Shift_Bytes;
         Token.Char_Region := @ + Shift_Chars;
         Token.Line        := @ + Shift_Line;
      end loop;
   end Shift;

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
   is begin
      Tree.Streams.Delete (Tree.Shared_Stream.Cur);
      Tree.Shared_Stream := (Cur => Tree.Streams.First);
      declare
         --  Ensure compiler makes 'Element' writable
         Stream : Parse_Stream renames Tree.Streams.Variable_Reference (Tree.Shared_Stream.Cur);
      begin
         for Element of Stream.Elements loop
            Element.Label := Shared_Stream_Label;
         end loop;
         Stream.Stack_Top := Stream_Element_Lists.No_Element;

         declare
            Temp : Stream_Element_Lists.Cursor := Stream.Elements.First; -- state 0, invalid node
         begin
            Stream.Elements.Delete (Temp);
         end;
      end;
      Tree.Incremental_Parse := True;
   end Start_Edit;

   procedure Start_Parse
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID;
      State  : in     State_Index)
   is
      New_Node : constant Node_Access := new Node'
        (Label       => Virtual_Identifier,
         Node_Index  => Tree.Nodes.Last_Index + 1,
         Child_Count => 0,
         State       => State,
         Identifier  => Identifier_Index'First,
         others      => <>);

      Junk : Terminal_Ref := Append_Stream_Element (Tree, Stream, New_Node);
      pragma Unreferenced (Junk);
   begin
      Tree.Nodes.Append (New_Node);
   end Start_Parse;

   procedure Stream_Delete
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Element : in out Stream_Index)
   is
      use all type Stream_Element_Lists.Cursor;
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
   begin
      if Parse_Stream.Stack_Top = Element.Cur then
         Parse_Stream.Stack_Top := Stream_Element_Lists.No_Element;
      end if;

      Parse_Stream.Elements.Delete (Element.Cur);
   end Stream_Delete;

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Terminal_Ref
   is
      Result : Terminal_Ref;
   begin
      Result.Element := (Cur => Tree.Streams (Stream.Cur).Elements.First);
      Result.Node := Stream_Element_Lists.Constant_Ref (Result.Element.Cur).Node;
      return Result;
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

   function Subtree_Image
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Node_Access;
      Non_Grammar : in Boolean := False;
      Level       : in Integer := 0)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"" & ASCII.LF;
   begin
      Result := @ &
        (if Node = Invalid_Node_Access
         then "    "
         else Decimal_Image (Node.Node_Index, Width => 4)) & ": ";
      for I in 1 .. Level loop
         Result := @ & "| ";
      end loop;
      Result := @ &
        (if Node = Invalid_Node_Access
         then "<deleted>"
         else Image
           (Tree, Node, Children => False, RHS_Index => True, Terminal_Node_Numbers => True,
            Non_Grammar => Non_Grammar));

      if Node /= Invalid_Node_Access and then Node.Label = Nonterm then
         for Child of Node.Children loop
            Result := @ & Subtree_Image (Tree, Child, Non_Grammar, Level + 1);
         end loop;
      end if;

      return -Result;
   end Subtree_Image;

   function Sub_Tree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Valid_Node_Access
   is
      N : Valid_Node_Access := Node;
   begin
      loop
         exit when N.Parent = Invalid_Node_Access;
         N := N.Parent;
      end loop;
      return N;
   end Sub_Tree_Root;

   function To_Node_Access (Item : in Valid_Node_Access_Array) return Node_Access_Array
   is (for I in Item'Range => Item (I));

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

   procedure Undo_Reduce
     (Tree   : in out Syntax_Trees.Tree;
      Stream : in     Stream_ID)
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream.Cur);
      Nonterm : constant Node_Access := Pop (Parse_Stream);
   begin
      for Child of Nonterm.Children loop
         Push (Parse_Stream, Child);
         Child.Parent := Invalid_Node_Access;
      end loop;
   end Undo_Reduce;

   procedure Update_Cache
     (Node      : in Valid_Node_Access;
      Recursive : in Boolean := False)
   is begin
      Node.First_Shared_Terminal := Invalid_Node_Access;
      Node.Byte_Region.First     := Buffer_Pos'Last;
      Node.Byte_Region.Last      := Buffer_Pos'First;
      Node.Line                  := Line_Number_Type'Last;

      for Child of Node.Children loop
         if Recursive and Child.Label = Nonterm then
            Update_Cache (Child, Recursive);
         end if;

         Node.Virtual := Node.Virtual or
           (case Child.Label is
            when Shared_Terminal                       => False,
            when Virtual_Terminal | Virtual_Identifier => True,
            when Nonterm                               => Child.Virtual);

         if Node.Byte_Region.First > Child.Byte_Region.First then
            Node.Byte_Region.First := Child.Byte_Region.First;
            Node.Char_Region.First := Child.Char_Region.First;
         end if;

         if Node.Byte_Region.Last < Child.Byte_Region.Last then
            Node.Byte_Region.Last := Child.Byte_Region.Last;
            Node.Char_Region.Last := Child.Char_Region.Last;
         end if;

         if Node.Line > Child.Line then
            Node.Line := Child.Line;
         end if;

         if Node.First_Shared_Terminal = Invalid_Node_Access then
            case Child.Label is
            when Shared_Terminal =>
               Node.First_Shared_Terminal := Child;

            when Virtual_Terminal | Virtual_Identifier =>
               null;

            when Nonterm =>
               if Child.First_Shared_Terminal /= Invalid_Node_Access then
                  --  not an empty nonterm
                  Node.First_Shared_Terminal := Child.First_Shared_Terminal;
               end if;
            end case;
         end if;
      end loop;
   end Update_Cache;

   procedure Update_Cache (Tree : in out Syntax_Trees.Tree; Stream : in Stream_ID)
   is begin
      for Element of Tree.Streams (Stream.Cur).Elements loop
         if Element.Node.Label = Nonterm then
            Update_Cache (Element.Node, Recursive => True);
         end if;
      end loop;
   end Update_Cache;

   procedure Update
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Access;
      Byte_Region : in Buffer_Region;
      Char_Region : in Buffer_Region;
      Line        : in Line_Number_Type;
      Column      : in Ada.Text_IO.Count)
   is begin
      Node.Byte_Region := Byte_Region;
      Node.Char_Region := Char_Region;
      Node.Line        := Line;

      if Node.Parent /= null then
         Update_Cache (Node.Parent);
      end if;
   end Update;

   procedure Validate_Tree
     (Tree                : in out Syntax_Trees.Tree;
      User_Data           : in out User_Data_Type'Class;
      Line_Begin_Char_Pos : in     WisiToken.Line_Pos_Vectors.Vector;
      File_Name           : in     String;
      Error_Reported      : in out Node_Sets.Set;
      Root                : in     Node_Access                := Invalid_Node_Access;
      Validate_Node       : in     Syntax_Trees.Validate_Node := null)
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
                    (Node, Line_Begin_Char_Pos, File_Name,
                     Image (Tree, Node,
                            Children     => False,
                            Node_Numbers => True)));
               Node_Image_Output := True;
            end if;

            Put_Line
              (Current_Error, Tree.Error_Message (Node, Line_Begin_Char_Pos, File_Name, "... invalid_tree: " & Msg));
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
               Validate_Node
                 (Tree, Node, User_Data, File_Name, Node_Image_Output, Node_Error_Reported);
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
                 (Ada.Text_IO.Current_Error, Error_Message (File_Name, 1, 1, "... invalid_tree: Tree.Root not set"));
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
