--  Abstract :
--
--  demonstrate reasonable error repair
--
--
--  Does not compile.

--EMACSCMD:(setq skip-recase-test t)

procedure Ada_Mode.Recover_Repair_1
is
   A : Integer;
begin
   -- We delete '1234567890' before 'and' to cause a syntax error; 'wisi-repair-errors'
   -- inserts '1234567890' before it. (forward-sexp) triggers a navigate parse.

   --EMACSCMD:(progn(forward-line 3)(forward-word 1)(kill-word 1))
   --EMACSCMD:(progn(forward-line 2)(forward-word 1)(forward-sexp)(looking-at "then"))
   --EMACSRESULT:t
   if 1234567890 and
     Expect
   then
      Expecting;
   end if;

   --EMACSCMD:(wisi-repair-errors)
   -- test is just diff.

end Ada_Mode.Recover_Repair_1;
