--  Encountered "error during resume".

--EMACSCMD:(switch-to-lr1)

begin
   --  We can't use Process_Tree because we are editing the tree.
   for I of Data.Ebnf_Nodes.Iterate loop
      Process_Node (I);
   end if;

   Data.Meta_Syntax := Bnf_Syntax;
