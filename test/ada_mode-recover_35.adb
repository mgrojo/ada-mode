--  Recover used to get "invalid Undo_Reduce in apply config"
procedure Push
  (Tree         : in out Syntax_Trees.Tree;
   Parse_Stream : in out Syntax_Trees.Parse_Stream;
   Node         : in     Valid_Node_Access)
with Pre => Node.State /= Unknown_State
is
   --  Error on next line; missing ': Valid_node_Access' after 'Junk'.
   Junk := Push (Tree, Tree.Streams (Stream.Cur), New_Node);
   pragma Unreferenced (Junk);
   begin
      null;
   end Push;
