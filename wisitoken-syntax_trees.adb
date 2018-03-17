--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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
package body WisiToken.Syntax_Trees is

   --  Abstract_Tree public operations

   function Image
     (Tree       : in Abstract_Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           Abstract_Tree'Class (Tree).Image (Nodes (I), Descriptor);
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Tree       : in Abstract_Tree;
      Nodes      : in Valid_Node_Index_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I of Nodes loop
         Result := Result & (if Need_Comma then ", " else "") &
           Abstract_Tree'Class (Tree).Image (I, Descriptor);
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Tree       : in Abstract_Tree;
      Item       : in Valid_Node_Index_Queues.Queue_Type;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I in 1 .. Item.Count loop
         Result := Result & (if Need_Comma then ", " else "") &
           Abstract_Tree'Class (Tree).Image (Item.Peek (I), Descriptor);
         Need_Comma := True;
      end loop;
      return -Result;
   end Image;

   ----------
   --  Syntax_Tree.Tree body operations, alphabetical

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Node         : in     Valid_Node_Index;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index))
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      if N.Label = Nonterm then
         for Child of N.Children loop
            Process_Tree (Tree, Child, Process_Node);
         end loop;
      end if;

      Process_Node (Tree, Node);
   end Process_Tree;

   ----------
   --  Syntax_Tree.Tree public operations

   procedure Initialize
     (Tree      : in out Syntax_Trees.Tree;
      Terminals : in     Base_Token_Arrays.Vector_Access_Constant)
   is begin
      Tree.Terminals := Terminals;
   end Initialize;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree)
   is
      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index)
      is begin
         case Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            null;

         when Virtual_Terminal =>
            Semantic_State.Free (Tree.Nodes (Node).Virtual_Augmented);

         when Nonterm =>
            Semantic_State.Free (Tree.Nodes (Node).Nonterm_Augmented);
         end case;
      end Process_Node;

   begin
      Tree.Traversing := False;
      if Tree.Augmented_Present then
         Tree.Process_Tree (Process_Node'Access);
         Tree.Augmented_Present := False;
      end if;
      Tree.Nodes.Finalize;
      Tree.Terminals := null;
   end Finalize;

   overriding procedure Clear (Tree : in out Syntax_Trees.Tree)
   is
      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index)
      is begin
         case Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            null;

         when Virtual_Terminal =>
            Semantic_State.Free (Tree.Nodes (Node).Virtual_Augmented);

         when Nonterm =>
            Semantic_State.Free (Tree.Nodes (Node).Nonterm_Augmented);
         end case;
      end Process_Node;

   begin
      --  don't reset Tree.Terminals.
      Tree.Traversing := False;
      if Tree.Augmented_Present then
         Tree.Process_Tree (Process_Node'Access);
         Tree.Augmented_Present := False;
      end if;
      Tree.Nodes.Finalize;
   end Clear;

   overriding procedure Adjust (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Augmented_Present then
         --  FIXME:
         raise SAL.Not_Implemented;
      end if;
   end Adjust;

   function Add_Nonterm
     (Tree       : in out Syntax_Trees.Tree;
      Nonterm    : in     WisiToken.Token_ID;
      Action     : in     Semantic_Action;
      Production : in     Positive;
      Name_Index : in     Natural)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append
        ((Label      => Syntax_Trees.Nonterm,
          Nonterm_ID => Nonterm,
          Virtual    => False,
          Action     => Action,
          Production => Production,
          Name_Index => Name_Index,
          others     => <>));
      return Tree.Nodes.Last_Index;
   end Add_Nonterm;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append ((Label => Shared_Terminal, Terminal => Terminal, others => <>));
      return Tree.Nodes.Last_Index;
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append ((Label => Virtual_Terminal, Terminal_ID => Terminal, others => <>));
      return Tree.Nodes.Last_Index;
   end Add_Terminal;

   overriding function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label
   is begin
      return Tree.Nodes (Node).Label;
   end Label;

   overriding
   function Children
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Valid_Node_Index_Array
   is begin
      return Children (Tree.Nodes (Node));
   end Children;

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Tree.Nodes (Node).Children.Length > 0;
   end Has_Children;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      return Tree.Nodes (Node).Parent /= No_Node_Index;
   end Has_Parent;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean
   is begin
      return (for some Child of Children => Tree.Nodes (Child).Parent /= No_Node_Index);
   end Has_Parent;

   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      return N.Label = Virtual_Terminal or (N.Label = Nonterm and then N.Virtual);
   end Is_Virtual;

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Traversing;
   end Traversing;

   overriding function Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   is begin
      return Tree.Nodes (Node).Parent;
   end Parent;

   overriding
   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID
   is begin
      return Get_ID (Tree.Nodes (Node), Tree.Terminals.all);
   end ID;

   overriding
   function Augmented_Token_Ref
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref
   is
      use all type Semantic_State.Augmented_Token_Access;
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         return (Element => Augmented_Terminals (N.Terminal).Element);

      when Virtual_Terminal =>
         if N.Virtual_Augmented = null then
            N.Virtual_Augmented := new Semantic_State.Augmented_Token'
              (ID     => N.Terminal_ID,
               others => <>);
         end if;
         return (Element => N.Virtual_Augmented);

      when Nonterm =>
         if N.Nonterm_Augmented = null then
            N.Nonterm_Augmented := new Semantic_State.Augmented_Token'
              (ID          => N.Nonterm_ID,
               Byte_Region => N.Byte_Region,
               others      => <>);
         end if;
         return (Element => N.Nonterm_Augmented);
      end case;
   end Augmented_Token_Ref;

   overriding
   function Constant_Aug_Token_Ref
     (Tree                : in Syntax_Trees.Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
     return Constant_Augmented_Ref
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         return (Element => Augmented_Terminals (N.Terminal).Element);

      when Virtual_Terminal =>
         return (Element => N.Virtual_Augmented);

      when Nonterm =>
         return (Element => N.Nonterm_Augmented);
      end case;
   end Constant_Aug_Token_Ref;

   procedure Set_Augmented
     (Tree      : in out Syntax_Trees.Tree;
      Node      : in     Valid_Node_Index;
      Augmented : in     Semantic_State.Augmented_Token)
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         raise Program_Error;

      when Virtual_Terminal =>
         N.Virtual_Augmented := new Semantic_State.Augmented_Token'(Augmented);

      when Nonterm =>
         N.Nonterm_Augmented := new Semantic_State.Augmented_Token'(Augmented);
      end case;

      Tree.Augmented_Present := True;
   end Set_Augmented;

   overriding
   function Augmented_Token_Array
     (Tree                : in out Syntax_Trees.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array
   is
      use all type Semantic_State.Augmented_Token_Access;
   begin
      return Result : Semantic_State.Augmented_Token_Access_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            declare
               N : Syntax_Trees.Node renames Tree.Nodes (Nodes (I));
            begin
               case N.Label is
               when Shared_Terminal =>
                  Result (I) := Semantic_State.Augmented_Token_Access (Augmented_Terminals.Variable_Ref (N.Terminal));

               when Virtual_Terminal =>
                  if N.Virtual_Augmented = null then
                     N.Virtual_Augmented := new Semantic_State.Augmented_Token'
                       (ID     => N.Terminal_ID,
                        others => <>);
                  end if;
                  Result (I) := N.Virtual_Augmented;

               when Nonterm =>
                  if N.Nonterm_Augmented = null then
                     N.Nonterm_Augmented := new Semantic_State.Augmented_Token'
                       (ID          => N.Nonterm_ID,
                        Byte_Region => N.Byte_Region,
                        others      => <>);
                  end if;
                  Result (I) := N.Nonterm_Augmented;
               end case;
            end;
         end loop;
      end return;
   end Augmented_Token_Array;

   overriding
   function Virtual
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Boolean
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         return False;
      when Virtual_Terminal =>
         return True;
      when Nonterm =>
         return N.Virtual;
      end case;
   end Virtual;

   overriding
   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   is begin
      return Tree.Nodes (Node).Action;
   end Action;

   overriding
   function Name_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   is begin
      return Tree.Nodes (Node).Name_Index;
   end Name_Index;

   overriding
   function Find_Ancestor
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      N : Node_Index := Tree.Nodes (Node).Parent;
   begin
      loop
         exit when N = No_Node_Index;
         exit when Get_ID (Tree.Nodes (N), Tree.Terminals.all) = ID;
         N := Tree.Nodes (N).Parent;
      end loop;
      return N;
   end Find_Ancestor;

   overriding
   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      Parent : Syntax_Trees.Node renames Tree.Nodes (Tree.Nodes (Node).Parent);
   begin
      case Parent.Label is
      when Shared_Terminal | Virtual_Terminal =>
         return No_Node_Index;

      when Nonterm =>
         for N of Parent.Children loop
            if Get_ID (Tree.Nodes (N), Tree.Terminals.all) = ID then
               return N;
            end if;
         end loop;
         return No_Node_Index;
      end case;
   end Find_Sibling;

   overriding
   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      N : constant Syntax_Trees.Node := Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal | Virtual_Terminal =>
         return No_Node_Index;
      when Nonterm =>
         for C of N.Children loop
            if Get_ID (Tree.Nodes (C), Tree.Terminals.all) = ID then
               return C;
            end if;
         end loop;
         return No_Node_Index;
      end case;
   end Find_Child;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index))
   is begin
      Tree.Traversing := True;
      for N in Tree.Nodes.First_Index .. Tree.Nodes.Last_Index loop
         if Tree.Nodes (N).Parent = 0 then
            Process_Tree (Tree, N, Process_Node);
         end if;
      end loop;
      Tree.Traversing := False;
   exception
   when others =>
      Tree.Traversing := False;
      raise;
   end Process_Tree;

   overriding
   function Image
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is begin
      return Image (Tree.Nodes (Node), Tree.Terminals, Descriptor);
   end Image;

   ----------
   --  Spec private operations, alphabetical

   function Children (N : in Syntax_Trees.Node) return Valid_Node_Index_Array
   is
      use all type Ada.Containers.Count_Type;
   begin
      if N.Children.Length = 0 then
         return (1 .. 0 => <>);
      else
         return Result : Valid_Node_Index_Array (N.Children.First_Index .. N.Children.Last_Index) do
            for I in Result'Range loop
               Result (I) := N.Children (I);
            end loop;
         end return;
      end if;
   end Children;

   function Get_ID (N : in Node; Terminals : in Base_Token_Arrays.Vector) return Token_ID
   is begin
      case N.Label is
      when Shared_Terminal =>
         return Terminals.Element (N.Terminal).ID;
      when Virtual_Terminal =>
         return N.Terminal_ID;
      when Nonterm =>
         return N.Nonterm_ID;
      end case;
   end Get_ID;

   function Image
     (N          : in Syntax_Trees.Node;
      Terminals  : in Base_Token_Arrays.Vector_Access_Constant;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      function Compute
        (ID          : in Token_ID;
         Byte_Region : in Buffer_Region) return String
      is begin
         return "(" & Image (ID, Descriptor) &
           (if Byte_Region = Null_Buffer_Region then "" else ", " & Image (Byte_Region)) & ")";
      end Compute;
   begin
      case N.Label is
      when Shared_Terminal =>
         return Compute (Terminals.all (N.Terminal).ID, Terminals.all (N.Terminal).Byte_Region);
      when Virtual_Terminal =>
         return Compute (N.Terminal_ID, Null_Buffer_Region);
      when Nonterm =>
         return Compute (N.Nonterm_ID, N.Byte_Region);
      end case;
   end Image;

   function Min_Descendant (Nodes : in Node_Arrays.Vector; Node : in Valid_Node_Index) return Valid_Node_Index
   is
      N : Syntax_Trees.Node renames Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal | Virtual_Terminal =>
         return Node;

      when Nonterm =>
         declare
            Min : Node_Index := Node;
         begin
            for C of N.Children loop
               Min := Node_Index'Min (Min, Min_Descendant (Nodes, C));
            end loop;
            return Min;
         end;
      end case;
   end Min_Descendant;

   procedure Set_Children
     (Nodes     : in out Node_Arrays.Vector;
      Terminals : in     Base_Token_Arrays.Vector_Access_Constant;
      Parent    : in     Valid_Node_Index;
      Children  : in     Valid_Node_Index_Array)
   is
      use all type SAL.Base_Peek_Type;
      N : Nonterm_Node renames Nodes (Parent);
      J : Positive_Index_Type := Positive_Index_Type'First;

      Min_Terminal_Index_Set : Boolean := False;
   begin
      N.Children.Clear;
      N.Children.Set_Length (Children'Length);
      for I in Children'Range loop
         N.Children (J) := Children (I);
         declare
            K : Syntax_Trees.Node renames Nodes (Children (I));
            Child_Byte_Region : constant Buffer_Region :=
              (case K.Label is
               when Shared_Terminal  => Terminals.Element (K.Terminal).Byte_Region,
               when Virtual_Terminal => Null_Buffer_Region,
               when Nonterm          => K.Byte_Region);
         begin
            K.Parent := Parent;

            N.Virtual := N.Virtual or
              (case K.Label is
               when Shared_Terminal  => False,
               when Virtual_Terminal => True,
               when Nonterm          => K.Virtual);

            if N.Byte_Region.First > Child_Byte_Region.First then
               N.Byte_Region.First := Child_Byte_Region.First;
            end if;

            if N.Byte_Region.Last < Child_Byte_Region.Last then
               N.Byte_Region.Last := Child_Byte_Region.Last;
            end if;

            if not Min_Terminal_Index_Set then
               case K.Label is
               when Shared_Terminal =>
                  Min_Terminal_Index_Set := True;
                  N.Min_Terminal_Index   := K.Terminal;

               when Virtual_Terminal =>
                  null;

               when Nonterm =>
                  Min_Terminal_Index_Set := True;
                  N.Min_Terminal_Index   := K.Min_Terminal_Index;
               end case;
            end if;
         end;

         J := J + 1;
      end loop;
   end Set_Children;

end WisiToken.Syntax_Trees;
