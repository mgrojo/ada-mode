--  Test ada-align in a paramlist when there are recoverable syntax
--  errors in the paramlist.
--
--  Does not compile.

--EMACSCMD:(setq skip-recase-test t)

--  ada-align inserts missing tokens in the parameter list, then runs
-- normal align process. To let the diff succeed, we first delete a
-- couple colons.

--EMACSCMD:(progn (forward-line 7)(forward-word 3)(forward-word -1)(delete-char -2))
--EMACSCMD:(progn (forward-line 8)(forward-word 2)(forward-word -1)(delete-char -5)(ada-align))
--EMACSCMD:(progn (forward-line 6)(forward-word 3)(current-column))
--EMACSRESULT: 20
procedure Ada_Mode.Recover_Align_1
  (Pattern      : in     Recover_Pattern_1;
   Parser       : in     LR.Instance'Class;
   Parser_State : in out Parser_Lists.Parser_State;
   Error_ID     : in     Token_ID;
   Config       :    out Configuration)
is begin
   --  missing ';' is not repaired
   --EMACSCMD:(progn (end-of-line 3)(looking-back "if"))
   --EMACSRESULT: t
   if A then B; end if
end Ada_Mode.Recover_Align_1;
