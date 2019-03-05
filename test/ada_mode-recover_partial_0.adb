--  LR1 Recover used to hit enqueue_limit, now works nicely.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

-- Change parser to LR1 (normally run tests only with LALR)

--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_lr1_parse.exe"))
--EMACSCMD:(setq wisi-process--alist nil)
--EMACSCMD:(ada-mode)

-- Before, when hitting the enqueue limit, there were two errors, one
-- from the sytax error, one from recover fail. In addition, all lines
-- are indented to zero. Now there is one error and the indentation is
-- better.

--EMACSCMD:(progn (wisi-parse-buffer 'indent)(length (wisi-parser-parse-errors wisi--parser)))
--EMACSRESULT:1
if Foo then
   Bar;
end if;

exception
   when others =>
      null;
end Parse_File;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-mckenzie-enqueue-limit: 5000
-- End:
