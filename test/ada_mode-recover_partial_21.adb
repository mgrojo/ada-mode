-- From a real editing session with partial parse active; computed nil indent for line 13

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
declare
   ID : Token_ID renames Prod.RHSs (RHS).Tokens (I);
begin
   if ID in Terminals then
      All_Sequences (Nonterm) (RHS).Sequence.Append (ID);

   else
      if not All_Set (ID) then
         --  Need to compute some RHSs of ID

         if (I = Positive'First and (ID = Nonterm or Recursing_Index (ID) = Positive'First)) or

           --  Nonterm is mutually recursive with itself or some other.
           All_Sequences (Nonterm)(RHS).Recursive := True;
