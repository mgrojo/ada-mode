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
     return Integer
   is
      use all type SAL.Base_Peek_Type;
      Result : Integer := 0;
   begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal | Virtual_Terminal =>
            return 1;
         when Nonterm =>
            declare
               N : Syntax_Trees.Nonterm_Node renames Tree.Shared_Tree.Nodes (Node);
            begin
               for I of N.Children loop
                  Result := Result + Count_Terminals (Tree, I);
               end loop;
               return Result;
            end;
         end case;
      else
         case Tree.Branched_Nodes (Node).Label is
         when Shared_Terminal | Virtual_Terminal =>
            return 1;

         when Nonterm =>
            declare
               N : Nonterm_Node renames Tree.Branched_Nodes (Node);
            begin
               for I of N.Children loop
                  Result := Result + Count_Terminals (Tree, I);
               end loop;
               return Result;
            end;
         end case;
      end if;
   end Count_Terminals;

   procedure Delete_Subtree (Tree : in out Node_Arrays.Vector; Node : in Valid_Node_Index)
   is
      --  Can't hold a Tree.Nodes cursor while recursing; that prevents
      --  deleting nodes.

      Children : constant Valid_Node_Index_Arrays.Vector := Tree (Node).Children;
   begin
      for I of Children loop
         Delete_Subtree (Tree, I);
      end loop;
      Tree.Delete (Node);
   end Delete_Subtree;

   procedure Get_Terminals
     (Tree   : in     Branched.Tree;
      Node   : in     Valid_Node_Index;
      Result : in out Base_Token_Array;
      Last   : in out Integer)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            Last := Last + 1;
            Result (Last) := Tree.Shared_Tree.Terminals.all (Tree.Shared_Tree.Nodes (Node).Terminal);
         when Virtual_Terminal =>
            Last := Last + 1;
            Result (Last) :=
              (ID          => Tree.Shared_Tree.Nodes (Node).Terminal_ID,
               Byte_Region => Null_Buffer_Region);
         when Nonterm =>
            declare
               N : Syntax_Trees.Nonterm_Node renames Tree.Shared_Tree.Nodes (Node);
            begin
               for I of N.Children loop
                  Get_Terminals (Tree, I, Result, Last);
               end loop;
            end;
         end case;
      else
         case Tree.Branched_Nodes (Node).Label is
         when Shared_Terminal =>
            Last := Last + 1;
            Result (Last) := Tree.Shared_Tree.Terminals.all (Tree.Shared_Tree.Nodes (Node).Terminal);

         when Virtual_Terminal =>
            Last := Last + 1;
            Result (Last) :=
              (ID          => Tree.Branched_Nodes (Node).Terminal_ID,
               Byte_Region => Null_Buffer_Region);

         when Nonterm =>
            declare
               N : Nonterm_Node renames Tree.Branched_Nodes (Node);
            begin
               for I of N.Children loop
                  Get_Terminals (Tree, I, Result, Last);
               end loop;
            end;
         end case;
      end if;
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
   is begin
      --  Note that this preserves all stored indices in Branched_Nodes.
      --  FIXME: faster to use Slice?
      for I in reverse Required_Node .. Tree.Last_Shared_Node loop
         Tree.Branched_Nodes.Prepend (Tree.Shared_Tree.Nodes (I));
      end loop;
      Tree.Last_Shared_Node := Required_Node - 1;
   end Move_Branch_Point;

   ----------
   --  Public subprograms

   procedure Initialize
     (Branched_Tree : in out Branched.Tree;
      Shared_Tree   : in     Shared_Tree_Access)
   is begin
      Branched_Tree :=
        (Shared_Tree      => Shared_Tree,
         Last_Shared_Node => Shared_Tree.Nodes.Last_Index,
         Branched_Nodes   => <>);
   end Initialize;

   overriding
   function Add_Nonterm
     (Tree    : in out Branched.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean                                  := False;
      Action  : in     WisiToken.Semantic_State.Semantic_Action := null)
     return Valid_Node_Index
   is begin
      Tree.Branched_Nodes.Append
        ((Label      => Syntax_Trees.Nonterm,
          Parent     => No_Node_Index,
          Nonterm_ID => Nonterm,
          Virtual    => Virtual,
          Action     => Action,
          others     => <>));
      return Tree.Last_Shared_Node + Tree.Branched_Nodes.Last_Index;
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
      return Tree.Last_Shared_Node + Tree.Branched_Nodes.Last_Index;
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
      return Tree.Last_Shared_Node + Tree.Branched_Nodes.Last_Index;
   end Add_Terminal;

   overriding
   procedure Set_Children
     (Tree     : in out Branched.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is
      use Ada.Containers;
   begin
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
         N : Nonterm_Node renames Tree.Branched_Nodes (Parent - Tree.Last_Shared_Node);
         J : Count_Type := Count_Type'First;
      begin
         N.Children.Clear;
         N.Children.Set_Length (Children'Length);
         for I in Children'Range loop
            N.Children (J) := Children (I);
            Tree.Branched_Nodes (Children (I) - Tree.Last_Shared_Node).Parent := Parent;
            --  FIXME: compute parent.byte_region?

            J := J + 1;
         end loop;
      end;
   end Set_Children;

   function Has_Children (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Children.Length > 0;
      else
         return Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Children.Length > 0;
      end if;
   end Has_Children;

   function Has_Parent (Tree : in Branched.Tree; Child : in Valid_Node_Index) return Boolean
   is begin
      return
        (if Child <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Child).Parent /= No_Node_Index
         else Tree.Branched_Nodes (Child - Tree.Last_Shared_Node).Parent /= No_Node_Index);
   end Has_Parent;

   function Has_Parent (Tree : in Branched.Tree; Children : in Valid_Node_Index_Array) return Boolean
   is begin
      return
        (for some Child of Children =>
           (if Child <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (Child).Parent /= No_Node_Index
            else Tree.Branched_Nodes (Child - Tree.Last_Shared_Node).Parent /= No_Node_Index));
   end Has_Parent;

   function Is_Nonterm (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Nonterm;
      else
         return Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Label = Nonterm;
      end if;
   end Is_Nonterm;

   overriding
   function Byte_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (Tree.Shared_Tree.Nodes (Node).Terminal).Byte_Region;
         when Virtual_Terminal =>
            return Null_Buffer_Region;
         when Nonterm =>
            return Tree.Shared_Tree.Nodes (Node).Byte_Region;
         end case;
      else
         case Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all
              (Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Terminal).Byte_Region;

         when Virtual_Terminal | Nonterm =>
            return Null_Buffer_Region;
         end case;
      end if;
   end Byte_Region;

   overriding
   function Name_Region
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Buffer_Region
   is begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (Tree.Shared_Tree.Nodes (Node).Terminal).Byte_Region;
         when Virtual_Terminal =>
            return Null_Buffer_Region;
         when Nonterm =>
            return Tree.Shared_Tree.Nodes (Node).Name;
         end case;
      else
         case Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all
              (Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Terminal).Byte_Region;

         when Virtual_Terminal | Nonterm =>
            return Null_Buffer_Region;
         end case;
      end if;
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

      Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Name := Region;
   end Set_Name_Region;

   overriding
   function ID
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID
   is begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            return Tree.Shared_Tree.Terminals.all (Tree.Shared_Tree.Nodes (Node).Terminal).ID;
         when Virtual_Terminal =>
            return Tree.Shared_Tree.Nodes (Node).Terminal_ID;
         when Nonterm =>
            return Tree.Shared_Tree.Nodes (Node).Nonterm_ID;
         end case;
      else
         declare
            N : Syntax_Trees.Node renames Tree.Branched_Nodes (Node - Tree.Last_Shared_Node);
         begin
            case N.Label is
            when Shared_Terminal =>
               return Tree.Shared_Tree.Terminals.all (N.Terminal).ID;

            when Virtual_Terminal =>
               return N.Terminal_ID;

            when Nonterm =>
               return N.Nonterm_ID;
            end case;
         end;
      end if;
   end ID;

   overriding
   function Base_Token
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Base_Token
   is begin
      if Node <= Tree.Last_Shared_Node then
         declare
            N : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
         begin
            case N.Label is
            when Shared_Terminal =>
               return Tree.Shared_Tree.Terminals.all (N.Terminal);
            when Virtual_Terminal =>
               return (N.Terminal_ID, Null_Buffer_Region);
            when Nonterm =>
               return (N.Nonterm_ID, N.Byte_Region);
            end case;
         end;
      else
         declare
            N : Syntax_Trees.Node renames Tree.Branched_Nodes (Node - Tree.Last_Shared_Node);
         begin
            case N.Label is
            when Shared_Terminal =>
               return Tree.Shared_Tree.Terminals.all (N.Terminal);
            when Virtual_Terminal =>
               return (N.Terminal_ID, Null_Buffer_Region);
            when Nonterm =>
               return (N.Nonterm_ID, Null_Buffer_Region); -- FIXME: there could be some real terminals in this
            end case;
         end;
      end if;
   end Base_Token;

   overriding
   function Virtual
     (Tree : in Branched.Tree;
      Node : in Valid_Node_Index)
     return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         case Tree.Shared_Tree.Nodes (Node).Label is
         when Shared_Terminal =>
            return False;
         when Virtual_Terminal =>
            return True;
         when Nonterm =>
            return Tree.Shared_Tree.Nodes (Node).Virtual;
         end case;
      else
         case Tree.Branched_Nodes (Node - Tree.Last_Shared_Node).Label is
         when Shared_Terminal =>
            return False;

         when Virtual_Terminal | Nonterm =>
            return True;
         end case;
      end if;
   end Virtual;

   ----------
   --  New operations

   procedure Delete (Tree : in out Branched.Tree; Node : in Valid_Node_Index)
   is begin
      --  FIXME: Node is always in shared tree; move branch point first.
      Delete_Subtree (Tree.Branched_Nodes, Node - Tree.Last_Shared_Node);
      raise SAL.Programmer_Error with "syntax_trees.branched.delete: move branch point";
   end Delete;

   function Get_Terminals (Tree : in Branched.Tree; Node : in Valid_Node_Index) return Base_Token_Array
   is
      Last : Integer := 0;
   begin
      return Result : Base_Token_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminals (Tree, Node, Result, Last);
      end return;
   end Get_Terminals;


end WisiToken.Syntax_Trees.Branched;
