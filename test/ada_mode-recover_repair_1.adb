--  Abstract :
--
--  demonstrate reasonable error repair
--
--
--  Does not compile.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

procedure Ada_Mode.Recover_Repair_1
is
   A : Integer;
begin
   -- We insert 'and' to cause a syntax error; 'wisi-repair-errors'
   -- deletes it. (forward-sexp) triggers a navigate parse.

   --EMACSCMD:(progn(forward-line 3)(forward-word 1)(insert " and"))
   --EMACSCMD:(progn(forward-line 2)(forward-word 1)(forward-sexp)(looking-at "then"))
   --EMACSRESULT:t
   if
     Expect
   then
      Expecting;
   end if;

   --EMACSCMD:(wisi-repair-errors)
   -- test is just diff.

end Ada_Mode.Recover_Repair_1;
