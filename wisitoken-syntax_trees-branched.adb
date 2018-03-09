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

package body WisiToken.Syntax_Trees.Branched is

   --  body subprograms, alphabetical

   function Count_Terminals
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return SAL.Base_Peek_Type
   is
      use all type SAL.Base_Peek_Type;

      function Compute (N : in Syntax_Trees.Node) return SAL.Base_Peek_Type
      is
         Result : SAL.Base_Peek_Type := 0;
      begin
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal | Virtual_Terminal =>
            return 1;
         when Nonterm =>
            for I of N.Children loop
               Result := Result + Count_Terminals (Tree, I);
            end loop;
            return Result;
         end case;
      end Compute;

   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Count_Terminals;

   procedure Delete_Subtree (Tree : in out Node_Arrays.Vector; Node : in Valid_Node_Index)
   is
      --  Can't hold a Tree.Nodes cursor while recursing; that prevents
      --  deleting nodes.

      Children : constant Valid_Node_Index_Arrays.Vector := Tree (Node).Children;
   begin
      --  Delete in reverse order added, so Tree.Last_Index is reduced as well.
      Tree.Delete (Node);
      for I of reverse Children loop
         Delete_Subtree (Tree, I);
      end loop;
   end Delete_Subtree;

   procedure Get_Terminals
     (Tree   : in     Branched.Tree;
      Node   : in     Valid_Node_Index;
      Result : in out WisiToken.Base_Token_Array;
      Last   : in out Positive_Index_Type)
   is
      use all type SAL.Base_Peek_Type;

      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         case N.Label is
         when Shared_Terminal =>
            Last := Last + 1;
            Result (Last) := Tree.Shared_Tree.Terminals.all (N.Terminal);

         when Virtual_Terminal =>
            Last := Last + 1;
            Result (Last) :=
              (ID          => N.Terminal_ID,
               Byte_Region => Null_Buffer_Region);

         when Nonterm =>
            for I of N.Children loop
               Get_Terminals (Tree, I, Result, Last);
            end loop;
         end case;
      end Compute;
   begin
      Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Get_Terminals;

   function Min (Item : in Valid_Node_Index_Array) return Valid_Node_Index
   is
      Result : Node_Index := Item (Item'First);
   begin
      for I in Item'Range loop
         if Item (I) < Result then
            Result := Item (I);
         end if;
      end loop;
      return Result;
   end Min;

   procedure Move_Branch_Point (Tree : in out Branched.Tree; Required_Node : in Valid_Node_Index)
   is
      Lock : Protected_Base_Token_Arrays.Read_Lock_Type (Tree.Shared_Tree.Terminals);
      pragma Unreferenced (Lock);
   begin
      --  Note that this preserves all stored indices in Branched_Nodes.
      Tree.Branched_Nodes.Prepend (Tree.Shared_Tree.Nodes, Required_Node, Tree.Last_Shared_Node);
      Tree.Last_Shared_Node := Required_Node - 1;
   end Move_Branch_Point;

   procedure Process_Tree
     (Tree         : in out Branched.Tree;
      Node         : in     Valid_Node_Index;
      Process_Node : access procedure
        (Tree : in out Branched.Tree;
         Node : in     Valid_Node_Index))
   is
      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         if N.Label = Nonterm then
            for Child of N.Children loop
               Process_Tree (Tree, Child, Process_Node);
            end loop;
         end if;

         Process_Node (Tree, Node);
      end Compute;
   begin
      if Node <= Tree.Last_Shared_Node then
         Compute (Tree.Shared_Tree.Nodes (Node));
      else
         Compute (Tree.Branched_Nodes (Node));
      end if;
   end Process_Tree;

   ----------
   --  Public subprograms

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Syntax_Trees.Tree_Access;
      Flush         : in     Boolean)
   is begin
      Branched_Tree :=
        (Ada.Finalization.Controlled with
         Shared_Tree      => Shared_Tree,
         Last_Shared_Node => Shared_Tree.Nodes.Last_Index,
         Branched_Nodes   => <>,
         Flush            => Flush);

      Branched_Tree.Branched_Nodes.Set_First (Shared_Tree.Nodes.Last_Index + 1);
   end Initialize;

   procedure Flush (Tree : in out Branched.Tree)
   is begin
      --  This is the opposite of Move_Branch_Point
      Tree.Shared_Tree.Nodes.Splice (Tree.Branched_Nodes);
      Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
      Tree.Flush            := True;
   end Flush;

   procedure Set_Flush_False (Tree : in out Branched.Tree)
   is begin
      Tree.Flush := False;
      Tree.Branched_Nodes.Set_First (Tree.Last_Shared_Node + 1);
   end Set_Flush_False;

   function Add_Nonterm
     (Tree         : in out Branched.Tree;
      Nonterm      : in     WisiToken.Token_ID;
      Virtual      : in     Boolean         := False;
      Action       : in     Semantic_Action := null;
      Action_Index : in     Natural         := 0)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label        => Syntax_Trees.Nonterm,
             Parent       => No_Node_Index,
             Nonterm_ID   => Nonterm,
             Virtual      => Virtual,
             Action       => Action,
             Action_Index => Action_Index,
             others       => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label        => Syntax_Trees.Nonterm,
             Parent       => No_Node_Index,
             Nonterm_ID   => Nonterm,
             Virtual      => Virtual,
             Action       => Action,
             Action_Index => Action_Index,
             others       => <>));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Nonterm;

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label    => Shared_Terminal,
             Parent   => No_Node_Index,
             Terminal => Terminal));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label    => Shared_Terminal,
             Parent   => No_Node_Index,
             Terminal => Terminal));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label             => Virtual_Terminal,
             Parent            => No_Node_Index,
             Terminal_ID       => Terminal,
             Virtual_Augmented => null));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label             => Virtual_Terminal,
             Parent            => No_Node_Index,
             Terminal_ID       => Terminal,
             Virtual_Augmented => null));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Terminal;

   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is begin
      if Children'Length = 0 then
         return;
      end if;

      if Tree.Flush then
         Set_Children (Tree.Shared_Tree.all, Parent, Children);

      else
         declare
            Min_Child_Node : constant Valid_Node_Index := Min (Children);
         begin
            if Min_Child_Node <= Tree.Last_Shared_Node then
               Move_Branch_Point (Tree, Min_Child_Node);
            end if;
         end;

         declare
            use all type SAL.Base_Peek_Type;
            N : Nonterm_Node renames Tree.Branched_Nodes (Parent);
            J : Positive_Index_Type := Positive_Index_Type'First;
         begin
            N.Children.Clear;
            N.Children.Set_Length (Children'Length);
            for I in Children'Range loop
               N.Children (J) := Children (I);
               Tree.Branched_Nodes (Children (I)).Parent := Parent;
               --  FIXME: compute parent.byte_region for semantic checks

               J := J + 1;
            end loop;
         end;
      end if;
   end Set_Children;

   overriding
   function Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Children (Tree.Shared_Tree.Nodes (Node));
      else
         return Children (Tree.Branched_Nodes (Node));
      end if;
   end Children;

   function Has_Branched_Nodes (Tree : in Branched.Tree) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Tree.Branched_Nodes.Length > 0;
   end Has_Branched_Nodes;

   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is
      use all type Ada.Containers.Count_Type;
      use all type Positive_Index_Type;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Children.Length > 0;
      else
         return Tree.Branched_Nodes (Node).Children.Length > 0;
      end if;
   end Has_Children;

   function Has_Parent (Tree : in Branched.Tree; Child : in Valid_Node_Index) return Boolean
   is begin
      return
        (if Child <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Child).Parent /= No_Node_Index
         else Tree.Branched_Nodes (Child).Parent /= No_Node_Index);
   end Has_Parent;

   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean
   is begin
      return
        (for some Child of Children =>
           (if Child <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (Child).Parent /= No_Node_Index
            else Tree.Branched_Nodes (Child).Parent /= No_Node_Index));
   end Has_Parent;

   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Nonterm;
      else
         return Tree.Branched_Nodes (Node).Label = Nonterm;
      end if;
   end Is_Nonterm;

   function Traversing (Tree : in Branched.Tree) return Boolean
   is begin
      return Tree.Shared_Tree.Traversing;
   end Traversing;

   overriding
   function Byte_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is
      function Compute (N : in Syntax_Trees.Node) return Buffer_Region
      is begin
         case N.Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (N.Terminal).Byte_Region;
         when Virtual_Terminal =>
            return Null_Buffer_Region;
         when Nonterm =>
            return N.Byte_Region;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Byte_Region;

   overriding
   function Name_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is
      function Compute (N : in Syntax_Trees.Node) return Buffer_Region
      is begin
         case N.Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (N.Terminal).Byte_Region;
         when Virtual_Terminal =>
            return Null_Buffer_Region;
         when Nonterm =>
            return N.Name;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Name_Region;

   overriding
   procedure Set_Name_Region
     (Tree   : in out Branched.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes (Node).Name := Region;

      else
         if Node <= Tree.Last_Shared_Node then
            Move_Branch_Point (Tree, Node);
         end if;

         Tree.Branched_Nodes (Node).Name := Region;
      end if;
   end Set_Name_Region;

   overriding
   function ID
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Token_ID
   is
      function Compute (N : in Syntax_Trees.Node) return Token_ID
      is begin
         case N.Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (N.Terminal).ID;

         when Virtual_Terminal =>
            return N.Terminal_ID;

         when Nonterm =>
            return N.Nonterm_ID;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end ID;

   overriding
   function Base_Token
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token
   is
      function Compute (N : in Syntax_Trees.Node) return WisiToken.Base_Token
      is begin
         case N.Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (N.Terminal);
         when Virtual_Terminal =>
            return (N.Terminal_ID, Null_Buffer_Region);
         when Nonterm =>
            return (N.Nonterm_ID, N.Byte_Region);
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Base_Token;

   Bogus : aliased Semantic_State.Augmented_Token;

   overriding
   function Augmented_Token_Ref
     (Tree                : in out Branched.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in     Valid_Node_Index)
     return Augmented_Ref
   is
      use all type Semantic_State.Augmented_Token_Access;

      function Compute (N : in out Syntax_Trees.Node) return Augmented_Ref
      is begin
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
      end Compute;

   begin
      if Node <= Tree.Last_Shared_Node then
         declare
            N : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
         begin
            case N.Label is
            when Shared_Terminal =>
               return (Element => Augmented_Terminals (Tree.Shared_Tree.Nodes (Node).Terminal).Element);

            when Virtual_Terminal =>
               if Tree.Flush or N.Virtual_Augmented /= null then
                  return Compute (N);
               end if;

            when Nonterm =>
               if Tree.Flush or N.Nonterm_Augmented /= null then
                  return Compute (N);
               end if;
            end case;
         end;

         Move_Branch_Point (Tree, Node);
         --  Can't do this in the case statement above, that would move node
         --  out from under N.

         return Compute (Tree.Branched_Nodes (Node));

      else
         --  Node > Tree.Last_Shared_Node
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Augmented_Token_Ref;

   overriding
   function Constant_Aug_Token_Ref
     (Tree                : in Branched.Tree;
      Augmented_Terminals : in Semantic_State.Augmented_Token_Arrays.Vector;
      Node                : in Valid_Node_Index)
     return Constant_Augmented_Ref
   is
      pragma Unreferenced (Tree, Node, Augmented_Terminals);
   begin
      raise SAL.Not_Implemented;
      return (Element => Bogus'Access);
   end Constant_Aug_Token_Ref;

   overriding
   function Augmented_Token_Array
     (Tree                : in out Branched.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Access_Array
   is
      function Compute (N : in out Syntax_Trees.Node) return Semantic_State.Augmented_Token_Access
      is
         use all type Semantic_State.Augmented_Token_Access;
      begin
         case N.Label is
         when Shared_Terminal =>
            return Semantic_State.Augmented_Token_Access (Augmented_Terminals.Variable_Ref (N.Terminal));

         when Virtual_Terminal =>
            if N.Virtual_Augmented = null then
               N.Virtual_Augmented := new Semantic_State.Augmented_Token'
                 (ID     => N.Terminal_ID,
                  others => <>);
            end if;
            return N.Virtual_Augmented;

         when Nonterm =>
            if N.Nonterm_Augmented = null then
               N.Nonterm_Augmented := new Semantic_State.Augmented_Token'
                 (ID          => N.Nonterm_ID,
                  Byte_Region => N.Byte_Region,
                  others      => <>);
            end if;
            return N.Nonterm_Augmented;
         end case;
      end Compute;
   begin
      return Result : Semantic_State.Augmented_Token_Access_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            if Nodes (I) > Tree.Last_Shared_Node then
               Result (I) := Compute (Tree.Branched_Nodes (Nodes (I)));
            else
               Result (I) := Compute (Tree.Shared_Tree.Nodes (Nodes (I)));
            end if;
         end loop;
      end return;
   end Augmented_Token_Array;

   overriding
   function Virtual
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Boolean
   is
      function Compute (N : in Syntax_Trees.Node) return Boolean
      is begin
         case N.Label is
         when Shared_Terminal =>
            return False;
         when Virtual_Terminal =>
            return True;
         when Nonterm =>
            return N.Virtual;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Virtual;

   overriding
   function Action
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Action
         else Tree.Branched_Nodes (Node).Action);
   end Action;

   overriding
   function Action_Index
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Natural
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Action_Index
         else Tree.Branched_Nodes (Node).Action_Index);
   end Action_Index;

   procedure Process_Tree
     (Tree         : in out Branched.Tree;
      Process_Node : access procedure
        (Tree : in out Branched.Tree;
         Node : in     Valid_Node_Index))
   is
      Lock : Protected_Base_Token_Arrays.Read_Lock_Type (Tree.Shared_Tree.Terminals);
      pragma Unreferenced (Lock);
   begin
      Tree.Shared_Tree.Traversing := True;
      for N in Tree.Shared_Tree.Nodes.First_Index .. Tree.Shared_Tree.Nodes.Last_Index loop
         if Tree.Shared_Tree.Nodes (N).Parent = 0 then
            Process_Tree (Tree, N, Process_Node);
         end if;
      end loop;
      for N in Tree.Branched_Nodes.First_Index .. Tree.Branched_Nodes.Last_Index loop
         if Tree.Branched_Nodes (N).Parent = 0 then
            Process_Tree (Tree, N, Process_Node);
         end if;
      end loop;
      Tree.Shared_Tree.Traversing := False;
   exception
   when others =>
      Tree.Shared_Tree.Traversing := False;
      raise;
   end Process_Tree;

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index)
   is begin
      --  FIXME: Node is always in shared tree; move branch point first.
      Tree.Shared_Tree.Traversing := True;
      Delete_Subtree (Tree.Branched_Nodes, Node);
      raise SAL.Programmer_Error with "syntax_trees.branched.delete: move branch point";
   end Delete;

   function Get_Terminals (Tree : in Branched.Tree; Node : in Valid_Node_Index) return WisiToken.Base_Token_Array
   is
      Last : SAL.Base_Peek_Type := 0;
      Lock : Protected_Base_Token_Arrays.Read_Lock_Type (Tree.Shared_Tree.Terminals);
      pragma Unreferenced (Lock);
      --  Don't allow adding nodes between Count_Terminals and Get_Terminals.
   begin
      Tree.Shared_Tree.Traversing := True;
      return Result : WisiToken.Base_Token_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminals (Tree, Node, Result, Last);
      end return;
   end Get_Terminals;

end WisiToken.Syntax_Trees.Branched;
