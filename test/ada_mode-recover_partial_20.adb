--  'access procedure' confused indent with partial parse

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

--EMACSCMD:(progn (end-of-line 9)(delete-char 1)(wisi-indent-newline-indent)(current-column))
--EMACSRESULT: 3
procedure Process_Tree
  (Tree         : in out Syntax_Trees.Tree;
   Process_Node : access procedure
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Index);
   Root         : in     Node_Index := Invalid_Node_Index)
is

begin
   if Root = Invalid_Node_Index and Tree.Root = Invalid_Node_Index then
      raise Sal.Programmer_Error with "Tree.Root not set";
   end if;
   Tree.Shared_Tree.Traversing := True;
   Process_Tree (Tree, Tree.Root, Process_Node);
   Tree.Shared_Tree.Traversing := False;
exception
   when others =>
      Tree.Shared_Tree.Traversing := False;
      raise;
end Process_Tree;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- wisi-disable-face: t
-- End:
