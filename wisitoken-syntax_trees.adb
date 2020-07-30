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
     return Stream_Index;

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
      Parent   : in     Valid_Node_Access;
      Children : in     Node_Access_Array);

   procedure Update_Cache (Node : in Valid_Node_Access);

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
         Debug_Index => Tree.Next_Debug_Index,
         Byte_Region => Byte_Region,
         Identifier  => Identifier,
         others      => <>)
      do
         Tree.Next_Debug_Index := @ + 1;
      end return;
   end Add_Identifier;

   function Add_Nonterm_1
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   is
      Nonterm_Node : constant Valid_Node_Access := new Node'
        (Label       => Syntax_Trees.Nonterm,
         Child_Count => Children'Last,
         ID          => Production.LHS,
         Debug_Index => Tree.Next_Debug_Index,
         Children    => To_Node_Access (Children),
         Action      => Action,
         RHS_Index   => Production.RHS,
         Virtual     => (if Children'Length = 0 then Default_Virtual else False),
         others      => <>);
   begin
      Tree.Next_Debug_Index := @ + 1;

      if Children'Length > 0 then
         Update_Cache (Nonterm_Node);
      end if;

      return Nonterm_Node;
   end Add_Nonterm_1;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Stream          : in     Stream_ID;
      Production      : in     WisiToken.Production_ID;
      Current_Token   : in     Stream_Index;
      Children        : in     Stream_Index_Array;
      Action          : in     Semantic_Action := null;
      State           : in     State_Index;
      Default_Virtual : in     Boolean         := False)
     return Stream_Index
   is
      function Node_Children return Valid_Node_Access_Array
      is begin
         return Result : Valid_Node_Access_Array (Children'Range) := (others => Children (Children'First).Node) do
            for I in Result'Range loop
               Result (I) := Children (I).Node;
            end loop;
         end return;
      end Node_Children;

      New_Element : constant Stream_Index := new Stream_Element'
        (Prev => (if Children'Length = 0 then Current_Token.Prev else Children (Children'First).Prev),
         Next => (if Children'Length = 0 then Current_Token.Next else Children (Children'Last).Next),
         Node => Tree.Add_Nonterm_1 (Production, Node_Children, Action, Default_Virtual));

      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream);
   begin
      New_Element.Node.State := State;

      if New_Element.Prev = null then
         Parse_Stream.First := New_Element;
      else
         New_Element.Prev.Next := New_Element;
      end if;

      if New_Element.Next = null then
         Parse_Stream.Last := New_Element;
      else
         New_Element.Next.Prev := New_Element;
      end if;

      return New_Element;
   end Add_Nonterm;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Access_Array;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Access
   is (Add_Nonterm_1 (Tree, Production, Children, Action, Default_Virtual));

   function Add_Terminal_1
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     WisiToken.Base_Token)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Shared_Terminal,
         Child_Count => 0,
         ID          => Terminal.ID,
         Debug_Index => Tree.Next_Debug_Index,
         Byte_Region => Terminal.Byte_Region,
         Line        => Terminal.Line,
         Column      => Terminal.Column,
         Char_Region => Terminal.Char_Region,
         others      => <>)
      do
         Tree.Next_Debug_Index := @ + 1;
      end return;
   end Add_Terminal_1;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Stream   : in     Stream_ID;
      Terminal : in     WisiToken.Base_Token)
     return Stream_Index
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
      Stream   : in     Stream_ID;
      Terminal : in     Token_ID;
      Before   : in     Valid_Node_Access)
     return Stream_Index
   is begin
      return Result : constant Stream_Index := Append_Stream_Element
        (Tree, Stream, new Node'
           (Label       => Virtual_Terminal,
            Child_Count => 0,
            ID          => Terminal,
            Debug_Index => Tree.Next_Debug_Index,
            Before      => Before,
            others      => <>))
      do
         Tree.Next_Debug_Index := @ + 1;
      end return;
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Access
   is begin
      return Result : constant Valid_Node_Access := new Node'
        (Label       => Virtual_Terminal,
         Child_Count => 0,
         ID          => Terminal,
         Debug_Index => Tree.Next_Debug_Index,
         others      => <>)
      do
         Tree.Next_Debug_Index := @ + 1;
      end return;
   end Add_Terminal;

   function Append_Stream_Element
     (Tree    : in out Syntax_Trees.Tree;
      Stream  : in     Stream_ID;
      Node   : in     Valid_Node_Access)
     return Stream_Index
   is
      Parse_Stream : Syntax_Trees.Parse_Stream renames Tree.Streams (Stream);
      New_Element  : constant Stream_Index := new Stream_Element'(null, null, Node);
   begin
      if Parse_Stream.First = null then
         Parse_Stream.First := New_Element;
         Parse_Stream.Last  := New_Element;
      else
         New_Element.Prev       := Parse_Stream.Last;
         Parse_Stream.Last.Next := New_Element;
         Parse_Stream.Last      := New_Element;
      end if;
      return New_Element;
   end Append_Stream_Element;

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Base_Token_Class_Access
   is begin
      return Node.Augmented;
   end Augmented;

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Base_Token_Class_Access_Constant
   is begin
      return Base_Token_Class_Access_Constant (Node.Augmented);
   end Augmented_Const;

   function Base_Token (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return WisiToken.Base_Token
   is begin
      return (Node.ID, Node.Byte_Region, Node.Line, Node.Column, Node.Char_Region);
   end Base_Token;

   function Before
     (Tree             : in Syntax_Trees.Tree;
      Virtual_Terminal : in Valid_Node_Access)
     return Node_Access
   is begin
      return Virtual_Terminal.Before;
   end Before;

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Byte_Region = Null_Buffer_Region;
   end Buffer_Region_Is_Empty;

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return WisiToken.Buffer_Region
   is begin
      return Node.Byte_Region;
   end Byte_Region;

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

   function Child_Index
     (N     : in Node;
      Child : in Valid_Node_Access)
     return SAL.Peek_Type
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

   procedure Clear (Tree : in out Syntax_Trees.Tree)
   is begin
      for Stream of Tree.Streams loop
         declare
            Element : Stream_Index := Stream.First;
            Temp    : Stream_Index;
         begin
            loop
               exit when Element = Invalid_Stream_Index;
               Temp := Element.Next;
               Tree.Delete_Subtree (Element.Node);
               Free (Element);
               Element := Temp;
            end loop;
         end;
      end loop;

      Tree.Leading_Non_Grammar.Clear;
      Tree.Traversing       := False;
      Tree.Parents_Set      := False;
      Tree.Next_Debug_Index := 1;
   end Clear;

   function Copy_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in     Node_Access)
     return Node_Access
   is
      function Copy_Node
        (Tree   : in out Syntax_Trees.Tree;
         Index  : in     Valid_Node_Access;
         Parent : in     Node_Access)
        return Valid_Node_Access
      is
         Node : Syntax_Trees.Node renames Index.all;
         New_Node : Node_Access;
      begin
         case Node.Label is
         when Shared_Terminal =>
            New_Node := new Syntax_Trees.Node'
              (Label          => Shared_Terminal,
               Child_Count    => 0,
               ID             => Node.ID,
               Debug_Index    => Tree.Next_Debug_Index,
               Byte_Region    => Node.Byte_Region,
               Parent         => Parent,
               State          => Unknown_State,
               Augmented      => Node.Augmented,
               Line           => Node.Line,
               Column         => Node.Column,
               Char_Region    => Node.Char_Region,
               Non_Grammar    => Node.Non_Grammar);

         when Virtual_Terminal =>
            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Terminal,
               Child_Count => 0,
               ID          => Node.ID,
               Debug_Index    => Tree.Next_Debug_Index,
               Byte_Region => Node.Byte_Region,
               Parent      => Parent,
               State       => Unknown_State,
               Augmented   => Node.Augmented,
               Before      => Node.Before);

         when Virtual_Identifier =>
            New_Node := new Syntax_Trees.Node'
              (Label       => Virtual_Identifier,
               Child_Count => 0,
               ID          => Node.ID,
               Debug_Index    => Tree.Next_Debug_Index,
               Byte_Region => Node.Byte_Region,
               Parent      => Parent,
               State       => Unknown_State,
               Augmented   => Node.Augmented,
               Identifier  => Node.Identifier);

         when Nonterm =>
            declare
               New_Children : Node_Access_Array (Index.Children'Range);
            begin
               for I in New_Children'Range loop
                  New_Children (I) := Copy_Node (Tree, Index.Children (I), Parent);
               end loop;

               New_Node := new Syntax_Trees.Node'
                 (Label                => Nonterm,
                  Child_Count          => New_Children'Last,
                  ID                   => Node.ID,
                  Debug_Index          => Tree.Next_Debug_Index,
                  Byte_Region          => Node.Byte_Region,
                  Parent               => Parent,
                  State                => Unknown_State,
                  Augmented            => Node.Augmented,
                  Virtual              => Node.Virtual,
                  RHS_Index            => Node.RHS_Index,
                  Action               => Node.Action,
                  Name                 => Node.Name,
                  Children             => New_Children,
                  First_Terminal_Index => Node.First_Terminal_Index);
            end;
         end case;
         Tree.Next_Debug_Index := @ + 1;
         return New_Node;
      end Copy_Node;

   begin
      if Root = Invalid_Node_Access then
         return Invalid_Node_Access;
      else
         return Copy_Node (Tree, Root, Invalid_Node_Access);
      end if;
   end Copy_Subtree;

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
         Tree.Streams (Tree.Streams.First_Index).First.Node := Node;
      end if;

      Node.Parent := null;
   end Delete_Parent;

   function Error_Message
     (Tree      : in Syntax_Trees.Tree;
      Node      : in Valid_Node_Access;
      File_Name : in String;
      Message   : in String)
     return String
   is
      First_Terminal : constant Node_Access := Tree.First_Terminal (Node);
      Line           : Line_Number_Type    := Line_Number_Type'First;
      Column         : Ada.Text_IO.Count   := Ada.Text_IO.Count'First;
   begin
      if First_Terminal = Invalid_Node_Access then
         --  Node is empty
         null;
      else
         case Tree.Label (First_Terminal) is
         when Shared_Terminal =>
            Line   := First_Terminal.Line;
            Column := First_Terminal.Column;

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
      Clear (Tree);
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
      end if;
   end Find_Sibling;

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

   function Get_Node (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Valid_Node_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Element.Node;
   end Get_Node;

   function Get_Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Access)
     return Recover_Token
   is begin
      case Node.Label is
      when Shared_Terminal =>
         return
           (ID                   => Node.ID,
            Byte_Region          => Node.Byte_Region,
            First_Terminal_Index => Node,
            Name                 => Null_Buffer_Region,
            Virtual              => False);

      when Virtual_Terminal | Virtual_Identifier =>
         return
           (ID                   => Node.ID,
            Byte_Region          => Null_Buffer_Region,
            First_Terminal_Index => Invalid_Node_Access,
            Name                 => Null_Buffer_Region,
            Virtual              => True);

      when Nonterm =>
         return
           (ID                   => Node.ID,
            Byte_Region          => Node.Byte_Region,
            First_Terminal_Index => Node.First_Terminal_Index,
            Name                 => Node.Name,
            Virtual              => Node.Virtual);
      end case;
   end Get_Recover_Token;

   function Get_Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Access_Array)
     return Recover_Token_Array
   is begin
      return Result : Recover_Token_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            Result (I) := Tree.Get_Recover_Token (Nodes (I));
         end loop;
      end return;
   end Get_Recover_Token_Array;

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

   function First_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is begin
      return
        (case Node.Label is
         when Shared_Terminal => Node,
         when Virtual_Terminal |
           Virtual_Identifier => Invalid_Node_Access,
         when Nonterm         => Node.First_Terminal_Index);
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

   function ID (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Token_ID
   is
      pragma Unreferenced (Tree);
   begin
      return Element.Node.ID;
   end ID;

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

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Token                : in     Valid_Node_Access;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean
   is
      pragma Unreferenced (User_Data, Tree, Token, Insert_On_Blank_Line);
   begin
      return False;
   end Insert_After;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is begin
      return
        (if Item.First_Terminal_Index = Invalid_Node_Access
         then ""
         else Trimmed_Image (Item.First_Terminal_Index.Debug_Index) & ":") &
        "(" & Image (Item.ID, Descriptor) &
        (if Item.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Item.Byte_Region)) & ")";
   end Image;

   function Image
     (Tree              : in Syntax_Trees.Tree;
      Element           : in Stream_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String
   is begin
      return Image (Tree, Element.Node, Descriptor, Include_Children, Include_RHS_Index, Node_Numbers);
   end Image;

   function Image
     (Tree              : in Syntax_Trees.Tree;
      Node              : in Node_Access;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +(if Node_Numbers then Trimmed_Image (Node.Debug_Index) & ":" else "");
   begin
      if Node = null then
         return "<deleted>";
      else
         case Node.Label is
         when Virtual_Identifier =>
            Result := Result & Trimmed_Image (Node.Identifier) & ";";

         when others =>
            null;
         end case;

         Result := Result & "(" & Image (Node.ID, Descriptor) &
           (if Include_RHS_Index and Node.Label = Nonterm then "_" & Trimmed_Image (Node.RHS_Index) else "") &
           (if Node.Byte_Region = Null_Buffer_Region then "" else ", " & Image (Node.Byte_Region)) & ")";

         if Include_Children and Node.Label = Nonterm then
            Result := Result & " <= " & Image (Tree, Node.Children, Descriptor, Node_Numbers);
         end if;

         return -Result;
      end if;
   end Image;

   function Image
     (Tree       : in Syntax_Trees.Tree;
      Nodes      : in Node_Access_Array;
      Descriptor : in WisiToken.Descriptor;
      Node_Numbers : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           (if Nodes (I) = null then " - "
            else Tree.Image (Nodes (I), Descriptor, Node_Numbers => Node_Numbers));
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

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
   is
   begin
      return Node.Label in Virtual_Terminal | Virtual_Identifier or (Node.Label = Nonterm and then Node.Virtual);
   end Is_Virtual;

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Boolean
   is begin
      return Node.Label = Virtual_Identifier;
   end Is_Virtual_Identifier;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Label
   is begin
      return Node.Label;
   end Label;

   function Last_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access) return Node_Access
   is
      --  Last_Terminal_Index is not cached, because it is not needed in recover.
   begin
      case Node.Label is
      when Shared_Terminal =>
         return Node;

      when Virtual_Terminal | Virtual_Identifier =>
         return Invalid_Node_Access;

      when Nonterm =>
         for C of reverse Node.Children loop
            --  Encountering a deleted child here is an error in the user algorithm.
            declare
               Last_Term : constant Node_Access := Tree.Last_Shared_Terminal (C);
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

   function Leading_Non_Grammar (Tree : aliased in out Syntax_Trees.Tree) return Base_Token_Arrays_Var_Ref
   is begin
      return (Element => Tree.Leading_Non_Grammar'Access, Dummy => 0);
   end Leading_Non_Grammar;

   function Leading_Non_Grammar_Const (Tree : aliased in Syntax_Trees.Tree) return Base_Token_Arrays_Const_Ref
   is begin
      return (Element => Tree.Leading_Non_Grammar'Access, Dummy => 0);
   end Leading_Non_Grammar_Const;

   function New_Stream (Tree : in out Syntax_Trees.Tree) return Stream_ID
   is begin
      Tree.Streams.Append (Parse_Stream'(others => <>));
      return Tree.Streams.Last_Index;
   end New_Stream;

   function Next_Stream_ID (Tree : in Syntax_Trees.Tree) return Stream_ID
   is begin
      return Tree.Streams.Last_Index + 1;
   end Next_Stream_ID;

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

   function Node_Access_Compare (Left, Right : in Node_Access) return SAL.Compare_Result
   is (if Left.Debug_Index > Right.Debug_Index then SAL.Greater
       elsif Left.Debug_Index < Right.Debug_Index then SAL.Less
       else SAL.Equal);

   function Non_Grammar
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Valid_Node_Access)
     return Base_Token_Arrays_Var_Ref
   is begin
      return (Element => Terminal.Non_Grammar'Access, Dummy => 0);
   end Non_Grammar;

   function Non_Grammar_Const
     (Tree     : in Syntax_Trees.Tree;
      Terminal : in Valid_Node_Access)
     return Base_Token_Arrays_Const_Ref
   is begin
      return (Element => Terminal.Non_Grammar'Access, Dummy => 0);
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

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Descriptor      : in WisiToken.Descriptor;
      Root            : in Node_Access                   := Invalid_Node_Access;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null)
   is
      use Ada.Text_IO;

      Node_Printed : Node_Sets.Set;

      procedure Print_Node (Node : in Valid_Node_Access; Level : in Integer)
      is begin
         if Node_Printed.Contains (Node) then
            --  This does not catch all possible tree edit errors, but it does
            --  catch circles.
            raise SAL.Programmer_Error with "Print_Tree: invalid tree; loop:" & Get_Debug_Index (Node)'Image;
         else
            Node_Printed.Insert (Node);
         end if;

         Put (Decimal_Image (Get_Debug_Index (Node), Width => 4) & ": ");
         for I in 1 .. Level loop
            Put ("| ");
         end loop;
         Put (Image (Tree, Node, Descriptor, Include_Children => False, Include_RHS_Index => True));
         if Image_Augmented /= null and Node.Augmented /= null then
            Put (" - " & Image_Augmented (Node.Augmented));
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

      Print_Root : constant Node_Access := (if Root = Invalid_Node_Access then Tree.Root else Root);
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
      Process_Tree (Tree, (if Root = Invalid_Node_Access then Tree.Root else Root), Process_Node);
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

   procedure Set_Parents (Tree : in out Syntax_Trees.Tree)
   is
      procedure Set_Parents
        (Tree   : in out Syntax_Trees.Tree;
         Node   : in     Valid_Node_Access;
         Parent : in     Node_Access)
      is
      begin
         Node.Parent := Parent;
         case Node.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            null;

         when Nonterm =>
            for C of Node.Children loop
               if C = null then
                  --  This can only happen if someone calls Set_Parents after parents
                  --  are already set.
                  raise SAL.Programmer_Error with "encountered deleted child";
               end if;
               Set_Parents (Tree, C, Node);
            end loop;
         end case;
      end Set_Parents;
   begin
      Set_Parents (Tree, Root (Tree), Invalid_Node_Access);
      Tree.Parents_Set := True;
   end Set_Parents;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Access;
      Value : in     Base_Token_Class_Access)
   is begin
      Node.Augmented := Value;
   end Set_Augmented;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Access;
      Children : in     Node_Access_Array)
   is
      Realloc_Parent : Node_Access := Parent;
   begin
      --  See Design note in spec about Parents, Parent_Set.

      if Tree.Parents_Set then
         --  Clear current Children.Parent first, in case some are also in new
         --  children.
         for C of Parent.Children loop
            if C /= null then
               C.Parent := Invalid_Node_Access;
            end if;
         end loop;
      end if;

      if Parent.Children'Length = Children'Length then
         --  reuse current node
         Realloc_Parent.Byte_Region := Null_Buffer_Region;
         Realloc_Parent.Virtual     := False;
         Realloc_Parent.Children    := Children;

      else
         --  reallocate node with new child_count
         Realloc_Parent := new Node'
           (Label                => Nonterm,
            Child_Count          => Children'Last,
            ID                   => Parent.ID,
            Debug_Index          => Parent.Debug_Index, --  exception to the rule of incrementing on every new node
            Byte_Region          => Null_Buffer_Region,
            Parent               => Parent.Parent,
            State                => Parent.State,
            Augmented            => Parent.Augmented,
            Virtual              => False,
            RHS_Index            => Parent.RHS_Index,
            Action               => Parent.Action,
            Name                 => Parent.Name,
            Children             => Children,
            First_Terminal_Index => Parent.First_Terminal_Index);

         if Parent.Parent /= null then
            Parent.Children (Child_Index (Parent.Parent.all, Parent)) := Realloc_Parent;
         end if;
      end if;

      for I in Children'Range loop
         declare
            Child : constant Node_Access := Children (I);
         begin
            if Tree.Parents_Set then
               if Child.Parent /= Invalid_Node_Access then
                  declare
                     Other_Parent : constant Node_Access := Child.Parent;
                     Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                       (Other_Parent.all, Children (I));
                  begin
                     Other_Parent.Children (Child_Index) := null;
                  end;
               end if;

               Child.Parent := Parent;
            end if;
         end;
      end loop;

      Update_Cache (Realloc_Parent);
   end Set_Children;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in     Valid_Node_Access;
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

   procedure Set_State
     (Tree  : in out Syntax_Trees.Tree;
      Element : in     Stream_Index;
      State : in     State_Index)
   is begin
      Element.Node.State := State;
   end Set_State;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Access;
      Region : in     Buffer_Region)
   is begin
      Node.Name := Region;
   end Set_Name_Region;

   function Stream_First
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Stream_ID)
     return Stream_Index
   is begin
      return Tree.Streams (Stream).First;
   end Stream_First;

   function Stream_Next
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Stream_ID;
      Element : in Stream_Index)
     return Stream_Index
   is
      pragma Unreferenced (Tree, Stream);
   begin
      --  IMPROVEME: keep Current in Stream, so can enforce that Element is
      --  in Tree.Streams (Stream_ID)
      return Element.Next;
   end Stream_Next;

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
   is begin
      return Result : Node_Access_Array (Item'Range) do
         for I in Result'Range loop
            Result (I) := Item (I);
         end loop;
      end return;
   end To_Node_Access;

   function To_Valid_Node_Access (Item : in Node_Access_Array) return Valid_Node_Access_Array
   is begin
      return Result : Valid_Node_Access_Array (Item'Range) := (others => Dummy_Node) do
         for I in Result'Range loop
            Result (I) := Item (I);
         end loop;
      end return;
   end To_Valid_Node_Access;

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Traversing;
   end Traversing;

   function Tree_Size_Image (Tree : in Syntax_Trees.Tree) return String
   is begin
      return Tree.Next_Debug_Index'Image;
   end Tree_Size_Image;

   function State (Tree : in Syntax_Trees.Tree; Element : in Stream_Index) return Unknown_State_Index
   is (Element.Node.State);

   procedure Update_Cache (Node : in Valid_Node_Access)
   is
      First_Terminal_Index_Set : Boolean := False;
   begin
      for I in Node.Children'Range loop
         declare
            Child : constant Node_Access := Node.Children (I);
         begin
            Node.Virtual := Node.Virtual or
              (case Child.Label is
               when Shared_Terminal                       => False,
               when Virtual_Terminal | Virtual_Identifier => True,
               when Nonterm                               => Child.Virtual);

            if Node.Byte_Region.First > Child.Byte_Region.First then
               Node.Byte_Region.First := Child.Byte_Region.First;
            end if;

            if Node.Byte_Region.Last < Child.Byte_Region.Last then
               Node.Byte_Region.Last := Child.Byte_Region.Last;
            end if;

            if not First_Terminal_Index_Set then
               case Child.Label is
               when Shared_Terminal =>
                  First_Terminal_Index_Set  := True;
                  Node.First_Terminal_Index := Child;

               when Virtual_Terminal | Virtual_Identifier =>
                  null;

               when Nonterm =>
                  if Child.First_Terminal_Index /= Invalid_Node_Access then
                     --  not an empty nonterm
                     First_Terminal_Index_Set  := True;
                     Node.First_Terminal_Index := Child.First_Terminal_Index;
                  end if;
               end case;
            end if;
         end;
      end loop;
   end Update_Cache;

   procedure Validate_Tree
     (Tree           : in out Syntax_Trees.Tree;
      User_Data      : in out User_Data_Type'Class;
      Descriptor     : in     WisiToken.Descriptor;
      File_Name      : in     String;
      Error_Reported : in out Node_Sets.Set;
      Root           : in     Node_Access                 := Invalid_Node_Access;
      Validate_Node  : in     Syntax_Trees.Validate_Node := null)
   is
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
                    (Node, File_Name,
                     Image (Tree, Node, Descriptor,
                            Include_Children => False,
                            Node_Numbers     => True)));
               Node_Image_Output := True;
            end if;

            Put_Line (Current_Error, Tree.Error_Message (Node, File_Name, "... invalid_tree: " & Msg));
         end Put_Error;

      begin
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
                          (if Child_Parent = Invalid_Node_Access
                           then "child.parent invalid"
                           else "child.parent" & Child_Parent'Image & " incorrect");
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
                 (Tree, Node, User_Data, Descriptor, File_Name, Node_Image_Output, Node_Error_Reported);
               if Node_Error_Reported and then not Error_Reported.Contains (Node) then
                  Error_Reported.Insert (Node);
               end if;
            end;
         end if;
      end Process_Node;

   begin
      Process_Tree (Tree, (if Root = Invalid_Node_Access then Tree.Root else Root), Process_Node'Access);
   end Validate_Tree;

end WisiToken.Syntax_Trees;
