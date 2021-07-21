-- LALR and LR1 parsers failed error recovery with default enqueue_limit. With
-- higher limit, raised Constraint_Error.
--
-- Constraint_Error now fixed, recover improved.

--EMACSCMD:(setq skip-recase-test t)

-- We get different indent results from partial and incremental parse;
--EMACSCMD:(setq skip-reindent-test (not wisi-incremental-parse-enable))
procedure Update_Containing_Nonterms (Modified_Token_Index : in WisiToken.Token_Index)
is
   -- Missing 'procedure Process_Node ('.
   Node : in     Valid_Node_Index)
   is begin
            if Tree.Max_Terminal_Index (Node) = Modified_Token_Index then
      -- Missing 'end if;'
   end Process_Node;

   begin

end Update_Containing_Nonterms;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-mckenzie-enqueue-limit: 280000
-- End:
