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
package body WisiToken.Syntax_Trees.Branched is

   --  body subprograms, alphabetical

   function Count_Terminals
     (Tree   : in     Branched.Tree;
      Node   : in     Valid_Node_Index)
     return Natural
   is
      use all type SAL.Base_Peek_Type;

      function Compute (N : in Syntax_Trees.Node) return Natural
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal =>
            return 1;

         when Nonterm =>
            return Result : Natural := 0 do
               for I of N.Children loop
                  Result := Result + Count_Terminals (Tree, I);
               end loop;
            end return;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Count_Terminals;

   procedure Get_Terminal_IDs
     (Tree   : in     Branched.Tree;
      Node   : in     Valid_Node_Index;
      Result : in out Token_ID_Array;
      Last   : in out Natural)
   is
      use all type SAL.Base_Peek_Type;

      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal =>
            Last := Last + 1;
            Result (Last) := N.ID;

         when Nonterm =>
            for I of N.Children loop
               Get_Terminal_IDs (Tree, I, Result, Last);
            end loop;
         end case;
      end Compute;
   begin
      Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Get_Terminal_IDs;

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
   is begin
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

   overriding procedure Clear (Tree : in out Branched.Tree)
   is begin
      Tree.Shared_Tree.Clear;
      Tree.Last_Shared_Node := No_Node_Index;
      Tree.Branched_Nodes.Clear;
   end Clear;

   procedure Flush (Tree : in out Branched.Tree)
   is begin
      --  This is the opposite of Move_Branch_Point
      Tree.Shared_Tree.Nodes.Merge (Tree.Branched_Nodes);
      Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
      Tree.Flush            := True;
   end Flush;

   procedure Set_Flush_False (Tree : in out Branched.Tree)
   is begin
      Tree.Flush := False;
      Tree.Branched_Nodes.Set_First (Tree.Last_Shared_Node + 1);
   end Set_Flush_False;

   function Add_Nonterm
     (Tree       : in out Branched.Tree;
      Nonterm    : in     WisiToken.Token_ID;
      Action     : in     Semantic_Action;
      Production : in     Positive;
      Name_Index : in     Natural;
      Children   : in     Valid_Node_Index_Array)
     return Valid_Node_Index
   is
      Nonterm_Node : Valid_Node_Index;
   begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label      => Syntax_Trees.Nonterm,
             ID         => Nonterm,
             Action     => Action,
             Production => Production,
             Name_Index => Name_Index,
             others     => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         Nonterm_Node          := Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label      => Syntax_Trees.Nonterm,
             ID         => Nonterm,
             Action     => Action,
             Production => Production,
             Name_Index => Name_Index,
             others     => <>));
         Nonterm_Node := Tree.Branched_Nodes.Last_Index;
      end if;

      if Children'Length = 0 then
         return Nonterm_Node;
      end if;

      if Tree.Flush then
         Set_Children (Tree.Shared_Tree.Nodes, Nonterm_Node, Children);

      else
         declare
            Min_Child_Node : constant Valid_Node_Index := Min (Children);
         begin
            if Min_Child_Node <= Tree.Last_Shared_Node then
               Move_Branch_Point (Tree, Min_Child_Node);
            end if;
         end;

         Set_Children (Tree.Branched_Nodes, Nonterm_Node, Children);
      end if;

      return Nonterm_Node;
   end Add_Nonterm;

   function Add_Terminal
     (Tree      : in out Branched.Tree;
      Terminal  : in     Token_Index;
      Terminals : in     Base_Token_Arrays.Vector)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label       => Shared_Terminal,
             ID          => Terminals (Terminal).ID,
             Byte_Region => Terminals (Terminal).Byte_Region,
             Terminal    => Terminal,
             others      => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label       => Shared_Terminal,
             ID          => Terminals (Terminal).ID,
             Byte_Region => Terminals (Terminal).Byte_Region,
             Terminal    => Terminal,
             others      => <>));
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
           ((Label  => Virtual_Terminal,
             ID     => Terminal,
             others => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label  => Virtual_Terminal,
             ID     => Terminal,
             others => <>));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Terminal;

   procedure Set_State
     (Tree  : in out Branched.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index)
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes (Node).State := State;
      else
         if Node <= Tree.Last_Shared_Node then
            Tree.Shared_Tree.Nodes (Node).State := State;
         else
            Tree.Branched_Nodes (Node).State := State;
         end if;
      end if;
   end Set_State;

   function State (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Unknown_State_Index
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).State;
      else
         return Tree.Branched_Nodes (Node).State;
      end if;
   end State;

   overriding function Label (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Node_Label
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label;
      else
         return Tree.Branched_Nodes (Node).Label;
      end if;
   end Label;

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

   function Is_Empty (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Byte_Region = Null_Buffer_Region;
      else
         return Tree.Branched_Nodes (Node).Byte_Region = Null_Buffer_Region;
      end if;
   end Is_Empty;

   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Nonterm;
      else
         return Tree.Branched_Nodes (Node).Label = Nonterm;
      end if;
   end Is_Nonterm;

   function Is_Virtual (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is
      function Compute (N : in Syntax_Trees.Node) return Boolean
      is begin
         return N.Label = Virtual_Terminal or (N.Label = Nonterm and then N.Virtual);
      end Compute;

   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Is_Virtual;

   function Min_Terminal_Index (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Base_Token_Index
      is begin
         return
           (case N.Label is
            when Shared_Terminal  => N.Terminal,
            when Virtual_Terminal => Invalid_Token_Index,
            when Nonterm          => N.Min_Terminal_Index);
      end Compute;

   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Min_Terminal_Index;

   function Traversing (Tree : in Branched.Tree) return Boolean
   is begin
      return Tree.Shared_Tree.Traversing;
   end Traversing;

   overriding function Parent (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Node_Index
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Parent;
      else
         return Tree.Branched_Nodes (Node).Parent;
      end if;
   end Parent;

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
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).ID
         else Tree.Branched_Nodes (Node).ID);
   end ID;

   function Same_Token
     (Tree_1  : in Branched.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Branched.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean
   is
      function Compute (N_1, N_2 : in Syntax_Trees.Node) return Boolean
      is begin
         return N_1.Label = N_2.Label and
           N_1.ID = N_2.ID and
           N_1.Byte_Region = N_2.Byte_Region;
      end Compute;
   begin
      return Compute
        ((if Index_1 <= Tree_1.Last_Shared_Node
          then Tree_1.Shared_Tree.Nodes (Index_1)
          else Tree_1.Branched_Nodes (Index_1)),
         (if Index_2 <= Tree_2.Last_Shared_Node
          then Tree_2.Shared_Tree.Nodes (Index_2)
          else Tree_2.Branched_Nodes (Index_2)));
   end Same_Token;

   function Recover_Token
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token
   is
      function Compute (N : Syntax_Trees.Node) return WisiToken.Recover_Token
      is begin
         case N.Label is
         when Shared_Terminal =>
            return
              (ID                 => N.ID,
               Byte_Region        => N.Byte_Region,
               Min_Terminal_Index => N.Terminal,
               Name               => Null_Buffer_Region,
               Virtual            => False);

         when Virtual_Terminal =>
            return
              (ID                 => N.ID,
               Byte_Region        => Null_Buffer_Region,
               Min_Terminal_Index => Invalid_Token_Index,
               Name               => Null_Buffer_Region,
               Virtual            => True);

         when Nonterm =>
            return
              (ID                 => N.ID,
               Byte_Region        => N.Byte_Region,
               Min_Terminal_Index => N.Min_Terminal_Index,
               Name               => N.Name,
               Virtual            => False);
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Recover_Token;

   function Recover_Token_Array
     (Tree  : in Branched.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array
   is begin
      return Result : WisiToken.Recover_Token_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            Result (I) := Tree.Recover_Token (Nodes (I));
         end loop;
      end return;
   end Recover_Token_Array;

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
                 (ID     => N.ID,
                  others => <>);
            end if;
            return (Element => N.Virtual_Augmented);

         when Nonterm =>
            if N.Nonterm_Augmented = null then
               N.Nonterm_Augmented := new Semantic_State.Augmented_Token'
                 (ID          => N.ID,
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
      use all type Semantic_State.Augmented_Token_Access;

      function Compute (N : in Syntax_Trees.Node) return Constant_Augmented_Ref
      is begin
         case N.Label is
         when Shared_Terminal =>
            return (Element => Augmented_Terminals (N.Terminal).Element);

         when Virtual_Terminal =>
            return (Element => N.Virtual_Augmented);

         when Nonterm =>
            return (Element => N.Nonterm_Augmented);
         end case;
      end Compute;

   begin
      return Compute
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node)
         else Tree.Branched_Nodes (Node));
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
                 (ID     => N.ID,
                  others => <>);
            end if;
            return N.Virtual_Augmented;

         when Nonterm =>
            if N.Nonterm_Augmented = null then
               N.Nonterm_Augmented := new Semantic_State.Augmented_Token'
                 (ID          => N.ID,
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
   function Name_Index
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Natural
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Name_Index
         else Tree.Branched_Nodes (Node).Name_Index);
   end Name_Index;

   overriding
   function Find_Ancestor
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      N : Node_Index := Node;
   begin
      loop
         N :=
           (if N <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (N).Parent
            else Tree.Branched_Nodes (N).Parent);

         exit when N = No_Node_Index;
         exit when ID =
           (if N <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (N).ID
            else Tree.Branched_Nodes (N).ID);
      end loop;
      return N;
   end Find_Ancestor;

   overriding
   function Find_Sibling
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      function Compute_2 (N : in Syntax_Trees.Node) return Node_Index
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal =>
            return No_Node_Index;

         when Nonterm =>
            for C of N.Children loop
               if ID =
                 (if C <= Tree.Last_Shared_Node
                  then Tree.Shared_Tree.Nodes (C).ID
                  else Tree.Branched_Nodes (C).ID)
               then
                  return C;
               end if;
            end loop;
            return No_Node_Index;
         end case;
      end Compute_2;

      function Compute_1 (Parent : in Node_Index) return Node_Index
      is begin
         if Parent = No_Node_Index then
            return No_Node_Index;

         else
            return Compute_2
              ((if Parent <= Tree.Last_Shared_Node
                then Tree.Shared_Tree.Nodes (Parent)
                else Tree.Branched_Nodes (Parent)));
         end if;
      end Compute_1;
   begin
      return Compute_1
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node).Parent
          else Tree.Branched_Nodes (Node).Parent));
   end Find_Sibling;

   overriding
   function Find_Child
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Node_Index
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal =>
            return No_Node_Index;
         when Nonterm =>
            for C of N.Children loop
               if ID =
                 (if C <= Tree.Last_Shared_Node
                  then Tree.Shared_Tree.Nodes (C).ID
                  else Tree.Branched_Nodes (C).ID)
               then
                  return C;
               end if;
            end loop;
            return No_Node_Index;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Find_Child;

   procedure Process_Tree
     (Tree         : in out Branched.Tree;
      Process_Node : access procedure
        (Tree : in out Branched.Tree;
         Node : in     Valid_Node_Index))
   is begin
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

   function Min_Shared_Terminal_Index (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Base_Token_Index
      is begin
         case N.Label is
         when Shared_Terminal =>
            return N.Terminal;
         when Virtual_Terminal =>
            return Invalid_Token_Index;
         when Nonterm =>
            for I of N.Children loop
               declare
                  Temp : constant Base_Token_Index := Min_Shared_Terminal_Index (Tree, I);
               begin
                  if Temp /= Invalid_Token_Index then
                     return Temp;
                  end if;
               end;
            end loop;
            return Invalid_Token_Index;
         end case;
      end Compute;

   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Min_Shared_Terminal_Index;

   function Get_Terminal_IDs (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Token_ID_Array
   is
      Last : Natural := 0;
   begin
      Tree.Shared_Tree.Traversing := True;
      return Result : Token_ID_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminal_IDs (Tree, Node, Result, Last);
      end return;
   end Get_Terminal_IDs;

   overriding
   function Image
     (Tree       : in Branched.Tree;
      Node       : in Valid_Node_Index;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is begin
      return Image
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)),
         Descriptor);
   end Image;

end WisiToken.Syntax_Trees.Branched;
