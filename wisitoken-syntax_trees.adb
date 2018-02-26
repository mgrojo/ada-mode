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

   --  Abstract_Tree operations

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
   --  Syntax_Tree.Tree operations

   overriding
   function Add_Nonterm
     (Tree    : in out Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Token_ID;
      Virtual : in     Boolean                                  := False;
      Action  : in     WisiToken.Semantic_State.Semantic_Action := null)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append
        ((Label      => Syntax_Trees.Nonterm,
          Nonterm_ID => Nonterm,
          Virtual    => Virtual,
          Action     => Action,
          others     => <>));
      return Tree.Nodes.Last_Index;
   end Add_Nonterm;

   overriding
   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_Index)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append ((Label => Shared_Terminal, Terminal => Terminal, others => <>));
      return Tree.Nodes.Last_Index;
   end Add_Terminal;

   overriding
   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   is begin
      Tree.Nodes.Append ((Label => Virtual_Terminal, Terminal_ID => Terminal, others => <>));
      return Tree.Nodes.Last_Index;
   end Add_Terminal;

   procedure Set_Child
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
   is begin
      Tree.Nodes (Parent).Children.Append (Child);
      Tree.Nodes (Child).Parent := Parent;
   end Set_Child;

   overriding
   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is
      use Ada.Containers;
      N : Nonterm_Node renames Tree.Nodes (Parent);
      J : Count_Type := Count_Type'First;

      Child_Byte_Region : Buffer_Region;
   begin
      N.Children.Clear;
      N.Children.Set_Length (Children'Length);
      for I in Children'Range loop
         N.Children (J) := Children (I);
         Tree.Nodes (Children (I)).Parent := Parent;

         --  FIXME: time with and without this. very useful for debugging, not
         --  needed until action execute, some actions don't need it.
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

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      return Tree.Nodes (Node).Label = Nonterm;
   end Is_Nonterm;

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
   is
      N : Syntax_Trees.Node renames Tree.Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal =>
         return Tree.Terminals.all (N.Terminal).ID;
      when Virtual_Terminal =>
         return N.Terminal_ID;
      when Nonterm =>
         return N.Nonterm_ID;
      end case;
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

end WisiToken.Syntax_Trees;
