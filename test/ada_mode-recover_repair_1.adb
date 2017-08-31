--  Abstract :
--
--  demonstrate reasonable error repair
--
--
--  Does not compile.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

procedure Apply_Pattern
is
begin
   -- We have "a_bogus_identifier" because that's what the error
   -- repair inserts. (forward-sexp) triggers a parse.

   --EMACSCMD:(progn(forward-line 3)(forward-word 1)(kill-word 3))
   --EMACSCMD:(progn(forward-line 2)(forward-word 1)(forward-sexp)(looking-at "then"))
   --EMACSRESULT:t
   if a_bogus_identifier and
     Expect
   then
      Expecting;
   end if;

   --EMACSCMD:(progn(wisi-repair-errors)(and (looking-at "and") (looking-back "a_bogus_identifier ")))
   --EMACSRESULT:t

end;
