-- From a real editing session, with partial parse. Encountered "error during resume"
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(switch-to-lr1)
end Process_Node;
begin
   if Cur.Node /= null then
      Process_Node (Cur);
   end if;
