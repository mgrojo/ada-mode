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

   ----------
   --  Public subprograms

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Shared_Tree_Access)
   is begin
      Branched_Tree :=
        (Ada.Finalization.Controlled with
         Shared_Tree      => Shared_Tree,
         Last_Shared_Node => Shared_Tree.Nodes.Last_Index,
         Branched_Nodes   => <>);

      Branched_Tree.Branched_Nodes.Set_First (Shared_Tree.Nodes.Last_Index + 1);
   end Initialize;

   overriding
   function Add_Nonterm
     (Tree    : in out Branched.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean         := False;
      Action  : in     Semantic_Action := null)
     return Valid_Node_Index
   is begin
      Tree.Branched_Nodes.Append
        ((Label      => Syntax_Trees.Nonterm,
          Parent     => No_Node_Index,
          Nonterm_ID => Nonterm,
          Virtual    => Virtual,
          Action     => Action,
          others     => <>));
      return Tree.Branched_Nodes.Last_Index;
   end Add_Nonterm;

   overriding
   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   is begin
      Tree.Branched_Nodes.Append
        ((Label    => Shared_Terminal,
          Parent   => No_Node_Index,
          Terminal => Terminal));
      return Tree.Branched_Nodes.Last_Index;
   end Add_Terminal;

   overriding
   function Add_Terminal
     (Tree     : in out Branched.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   is begin
      Tree.Branched_Nodes.Append
        ((Label       => Virtual_Terminal,
          Parent      => No_Node_Index,
          Terminal_ID => Terminal));
      return Tree.Branched_Nodes.Last_Index;
   end Add_Terminal;

   overriding
   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is begin
      if Children'Length = 0 then
         return;
      end if;

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
   end Set_Children;

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
      if Node <= Tree.Last_Shared_Node then
         Move_Branch_Point (Tree, Node);
      end if;

      Tree.Branched_Nodes (Node).Name := Region;
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
     (Tree : in out Branched.Tree;
      Node : in     Valid_Node_Index)
     return Augmented_Ref
   is
      pragma Unreferenced (Tree, Node);
   begin
      raise SAL.Not_Implemented;
      return (Element => Bogus'Access);
   end Augmented_Token_Ref;

   overriding
   function Augmented_Token_Array
     (Tree                : in out Branched.Tree;
      Augmented_Terminals : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Nodes               : in     Valid_Node_Index_Array)
     return Semantic_State.Augmented_Token_Array
   is
      pragma Unreferenced (Tree, Augmented_Terminals, Nodes);
   begin
      raise SAL.Not_Implemented;
      return (1 .. 0 => <>);
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

   ----------
   --  New operations

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index)
   is begin
      --  FIXME: Node is always in shared tree; move branch point first.
      Delete_Subtree (Tree.Branched_Nodes, Node);
      raise SAL.Programmer_Error with "syntax_trees.branched.delete: move branch point";
   end Delete;

   function Get_Terminals (Tree : in Branched.Tree; Node : in Valid_Node_Index) return WisiToken.Base_Token_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : WisiToken.Base_Token_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminals (Tree, Node, Result, Last);
      end return;
   end Get_Terminals;

end WisiToken.Syntax_Trees.Branched;
