-- -*- wisi-mckenzie-enqueue-limit: 120000 -*-
--  LR1 error recovery fails with default enqueue_limit. IMPROVEME: detect splitting strings across a line
--  FIXME: LALR parser fails with invalid state; bug in generator

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(switch-to-lr1)
--EMACSCMD:(setq skip-recase-test t)

for ID of Prod.RHSs (RHS).Tokens loop
   if ID in Terminals then
      All_Sequences (Nonterm) (RHS).Append (ID);

   else
      if not All_Set (ID) then
	 --  Need to compute ID

	 if ID = Nonterm or Recursing (ID) then
	    --  This nonterm is mutually recursive with itself or some other. We
	    --  mark it as recursive for now, so we can check below if we have
	    --  found a minimum sequence.
	    All_Sequences (Nonterm) (RHS) := To_Vector (Invalid_Token_ID);
	    if Trace_Generate > Extra then
	       Ada.Text_IO.Put_Line
		 (Trimmed_Image (Production_ID'(Nonterm, RHS)) & " => " &
		    recursive");
	       -- trailing string quote due to splitting string across lines.
