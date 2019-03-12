-- LR1 parser encountered "error during resume"; LALR parser did not
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq debug-on-error t) ;; emacs error on parser error

-- Change parser to LR1 (normally run tests only with LALR)

--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_lr1_parse.exe"))
--EMACSCMD:(setq wisi-process--alist nil)
--EMACSCMD:(ada-mode)

procedure Update_First
is
begin
   end loop;
end loop;
end Update_First;
-- Error recovery has a race condition; force it to return repeatable results
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
