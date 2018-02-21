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

   ----------
   --  Public subprograms

   function Add_Nonterm
     (Tree    : aliased in out Syntax_Trees.Tree;
      Nonterm :         in     Token_ID;
      Action  :         in     WisiToken.Semantic_State.Semantic_Action := null;
      Check   :         in     WisiToken.Semantic_Checks.Semantic_Check := null)
     return Cursor
   is begin
      Tree.Nodes.Append ((Nonterm => Nonterm, Action => Action, Check => Check, others => <>));
      return (Tree => Tree'Access, Index => Tree.Nodes.Last_Index);
   end Add_Nonterm;

   function Add_Terminal
     (Tree     : aliased in out Syntax_Trees.Tree;
      Terminal :         in     Positive_Index_Type)
     return Cursor
   is begin
      Tree.Nodes.Append ((Terminal => Terminal, others => <>));
      return (Tree => Tree'Access, Index => Tree.Nodes.Last_Index);
   end Add_Terminal;

   procedure Set
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Cursor;
      Child  : in     Cursor)
   is begin
      Tree.Nodes (Parent.Index).Children.Append (Child.Index);
      Tree.Nodes (Child.Index).Parent := Parent.Index;
   end Set;

   function Has_Parent (Node : in Cursor) return Boolean
   is begin
      return Node.Tree.Nodes (Node.Index).Parent /= No_Node_Index;
   end Has_Parent;

end WisiToken.Syntax_Trees;
