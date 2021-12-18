-- Really bad syntax encountered while debugging another problem;
-- found error recovery that deleted same token several times, now fixed.
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
for ID loop
   Recursive");
--  Recover inserts 'end loop;' here.
-- Local Variables:
-- compare-tree-text: nil
-- End:
