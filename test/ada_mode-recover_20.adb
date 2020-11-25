-- This is from a real editing session; it found a bug in the parser,
-- but now is much better.
--
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Recover_20
is
begin
   if Indenting_Token.ID = -Expression_Opt_ID and
     (Prev_1 = -With_ID and
        (Prev_3 = Invalid_Token_ID or
           Prev_3 /= Left_Paren_ID))
      -- missing paren around this; mixed boolean operators.
      or
        B
      then
   --  missing 'end if;' - all inserted after 'then'.

end Ada_Mode.Recover_20;
-- Error recovery has a race condition; force it to return repeatable results
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
