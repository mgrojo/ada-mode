--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2021 Free Software Foundation, Inc.
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

package body WisiToken.Parse.Packrat is

   overriding
   procedure Execute_Actions (Parser : in out Packrat.Parser)
   is
      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Post_Parse_Action;
            Tree_Children : constant Syntax_Trees.Node_Access_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node, Syntax_Trees.To_Valid_Node_Access (Tree_Children));
            end if;
         end;
      end Process_Node;

   begin
      if Trace_Action > Outline then
         if Trace_Action > Extra then
            Parser.Tree.Print_Tree (Parser.Tree.Root);
            Parser.Trace.New_Line;
         end if;
         Parser.Trace.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root));
      end if;

      --  We don't do 'Parser.Tree.Clear_Parse_Streams; here; that deletes
      --  Shared_Stream, but those nodes are in the final tree.
      Parser.User_Data.Initialize_Actions (Parser.Tree);
      Parser.Tree.Process_Tree (Process_Node'Access);
   end Execute_Actions;

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Syntax_Trees.Stream_ID;
      Element : in Syntax_Trees.Stream_Index)
     return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Tree.Get_Node_Index (Stream, Element)'Image;
      end if;
   end Image_Pos;

end WisiToken.Parse.Packrat;
