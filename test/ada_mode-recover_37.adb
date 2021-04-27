-- LALR and LR1 parsers failed error recovery with default enqueue_limit. With
-- higher limit, raised Constraint_Error.
--
-- Constraint_Error now fixed, recover improved.

--EMACSCMD:(setq skip-recase-test t)

procedure Update_Containing_Nonterms (Modified_Token_Index : in WisiToken.Token_Index)
is
     --  recover inserts 'procedure <identifier> (' here.
     Node : in     Valid_Node_Index)
   is begin
      if Tree.Max_Terminal_Index (Node) = Modified_Token_Index then
      --  recover inserts 'end if;' after 'then'
   end Process_Node;

begin

end Update_Containing_Nonterms;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-mckenzie-enqueue-limit: 280000
-- End:
