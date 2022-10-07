--  LR1 Recover used to hit enqueue_limit, now works nicely.

--EMACSCMD:(switch-to-lr1)

-- Before, when hitting the enqueue limit, there were two errors, one
-- from the sytax error, one from recover fail. In addition, all lines
-- are indented to zero. Now there is one error and the indentation is
-- better.

--EMACSCMD:(progn (wisi-parse-buffer 'indent)(length (wisi-parser-local-parse-errors wisi-parser-local)))
--EMACSRESULT:2
if Foo then
   Bar;
end if;

exception
   when others =>
      null;
end Parse_File;
-- Local Variables:
-- wisi-mckenzie-enqueue-limit: 5000
-- End:
