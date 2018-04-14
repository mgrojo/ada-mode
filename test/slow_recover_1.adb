-- This is from a real editing session; it found a bug in the parser,
-- but now is much better.
--
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Slow_Recover_1
is
begin
   if Indenting_Token.ID = -Expression_Opt_ID and
     (Prev_1 = -With_ID and
        (Prev_3 = Invalid_Token_ID or
           Prev_3 /= Left_Paren_ID))
        or -- missing paren around this; mixed boolean operators.
       ()
      then
         --  missing 'end if;'

end Slow_Recover_1;
