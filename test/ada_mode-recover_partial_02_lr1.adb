-- From a real editing session. LR1 parser encountered enqueue limit.
-- Now it succeeds nicely.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(switch-to-lr1)
begin
   for Error of Errors loop
      for Op of Error.Recover.Ops loop
	 case Op.Op is
	    when Delete =>
	       if Data.Terminals (Op.Token_Index).First then
		  if Op.Token_Index = Data.Terminals.Last_Index then

		     -- Editing condition left next line exposed, with extra right paren.
		     -- Desired solution: insert <if> before '(' to match final 'then',
		     -- delete last ')', after 'then' insert 'end if; ...'.

		       (not Data.Terminals (Op.Token_Index + 1).First))
		     then
