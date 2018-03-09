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

package body WisiToken.Syntax_Trees is

   --  Abstract_Tree public operations

   function Base_Token_Array
     (Tree  : in Abstract_Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Base_Token_Array
   is begin
      return Result : WisiToken.Base_Token_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            Result (I) := Abstract_Tree'Class (Tree).Base_Token (Nodes (I));
         end loop;
      end return;
   end Base_Token_Array;

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
           Image (Abstract_Tree'Class (Tree).Base_Token (Nodes (I)), Descriptor);
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
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I of Nodes loop
         Result := Result & (if Need_Comma then ", " else "") &
           Image (Abstract_Tree'Class (Tree).Base_Token (I), Descriptor);
         Need_Comma := True;
      end loop;
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
           Image (Abstract_Tree'Class (Tree).Base_Token (Item.Peek (I)), Descriptor);
         Need_Comma := True;
      end loop;
      return -Result;
   end Image;

   ----------
   --  Syntax_Tree.Tree body operations, alphabetical

   function Get_ID (N : in Node; Terminals : in Protected_Base_Token_Arrays.Vector) return Token_ID
      with inline
   is begin
      case N.Label is
      when Shared_Terminal =>
         return Terminals (N.Terminal).ID;
      when Virtual_Terminal =>
         return N.Terminal_ID;
      when Nonterm =>
         return N.Nonterm_ID;
      end case;
   end Get_ID;

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
      Terminals : in     Protected_Base_Token_Arrays.Vector_Access_Constant)
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
      if Tree.Augmented_Present then
         Tree.Process_Tree (Process_Node'Access);
         Tree.Augmented_Present := False;
      end if;
   end Finalize;

   overriding procedure Adjust (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Augmented_Present then
         --  FIXME:
         raise SAL.Not_Implemented;
      end if;
   end Adjust;

   function Add_Nonterm
     (Tree         : in out Syntax_Trees.Tree;
      Nonterm      : in     WisiToken.Token_ID;
      Virtual      : in     Boolean         := False;
      Action       : in     Semantic_Action := null;
      Action_Index : in     Natural         := 0)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append
        ((Label        => Syntax_Trees.Nonterm,
          Nonterm_ID   => Nonterm,
          Virtual      => Virtual,
          Action       => Action,
          Action_Index => Action_Index,
          others       => <>));
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

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is
      use all type SAL.Base_Peek_Type;

      N : Nonterm_Node renames Tree.Nodes (Parent);
      J : Positive_Index_Type := Positive_Index_Type'First;

      Child_Byte_Region : Buffer_Region;
   begin
      N.Children.Clear;
      N.Children.Set_Length (Children'Length);
      for I in Children'Range loop
         N.Children (J) := Children (I);
         Tree.Nodes (Children (I)).Parent := Parent;

         case Tree.Nodes (Children (I)).Label is
         when Shared_Terminal =>
            Child_Byte_Region := Tree.Terminals.all (Tree.Nodes (Children (I)).Terminal).Byte_Region;
         when Virtual_Terminal =>
            Child_Byte_Region := Null_Buffer_Region;
         when Nonterm =>
            Child_Byte_Region := Tree.Nodes (Children (I)).Byte_Region;
         end case;

         if N.Byte_Region.First > Child_Byte_Region.First then
            N.Byte_Region.First := Child_Byte_Region.First;
         end if;

         if N.Byte_Region.Last < Child_Byte_Region.Last then
            N.Byte_Region.Last := Child_Byte_Region.Last;
         end if;

         J := J + 1;
      end loop;
   end Set_Children;

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

   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      return Tree.Nodes (Node).Label = Virtual_Terminal;
   end Is_Virtual_Terminal;

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      return Tree.Nodes (Node).Label = Nonterm;
   end Is_Nonterm;

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Traversing;
   end Traversing;

   function Parent
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Node_Index
   is begin
      return Tree.Nodes (Node).Parent;
   end Parent;

   overriding
   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is begin
      case Tree.Nodes (Node).Label is
      when Shared_Terminal =>
         return Tree.Terminals.all (Tree.Nodes (Node).Terminal).Byte_Region;
      when Virtual_Terminal =>
         return Null_Buffer_Region;
      when Nonterm =>
         return Tree.Nodes (Node).Byte_Region;
      end case;
   end Byte_Region;

   overriding
   function Name_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is begin
      case Tree.Nodes (Node).Label is
      when Shared_Terminal =>
         return Tree.Terminals.all (Tree.Nodes (Node).Terminal).Byte_Region;
      when Virtual_Terminal =>
         return Null_Buffer_Region;
      when Nonterm =>
         return Tree.Nodes (Node).Name;
      end case;
   end Name_Region;

   overriding
   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   is begin
      Tree.Nodes (Node).Name := Region;
   end Set_Name_Region;

   overriding
   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID
   is begin
      return Get_ID (Tree.Nodes (Node), Tree.Terminals.all);
   end ID;

   overriding
   function Base_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         return Tree.Terminals.all (N.Terminal);
      when Virtual_Terminal =>
         return (N.Terminal_ID, Null_Buffer_Region);
      when Nonterm =>
         return (N.Nonterm_ID, N.Byte_Region);
      end case;
   end Base_Token;

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
   function Action_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   is begin
      return Tree.Nodes (Node).Action_Index;
   end Action_Index;

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

   ----------
   --  Spec private operations

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


end WisiToken.Syntax_Trees;
