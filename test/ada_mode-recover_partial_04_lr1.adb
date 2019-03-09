-- From a real editing session, with partial parse. Encountered "error during resume"
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq debug-on-error t) ;; emacs error on parser error

-- Change parser to LR1 (normally run tests only with LALR)

--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_lr1_parse.exe"))
--EMACSCMD:(setq wisi-process--alist nil)
--EMACSCMD:(ada-mode)
end Process_Node;
begin
   if Cur.Node /= null then
      Process_Node (Cur);
   end if;
