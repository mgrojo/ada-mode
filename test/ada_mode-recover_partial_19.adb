--  Encountered "error during resume". FIXME: still does

--EMACS_SKIP_UNLESS:(and nil (eq ada-parser 'process))
--EMACSCMD:(switch-to-lr1)

begin
--  We can't use Process_Tree because we are editing the tree.
      for I of Data.EBNF_Nodes.Iterate loop
      Process_Node (I);
end If;

      Data.Meta_Syntax := BNF_Syntax;
