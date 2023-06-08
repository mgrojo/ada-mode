-- This is from a real editing session; it found a bug in the parser,
-- but now is much better.
--
-- The 'if' condition is missing parens due to mixed boolean
-- operators. Inserting the missing left paren after 'if' requires
-- three push_backs; recovery inserts 'then' before 'or B' instead,
-- which is much cheaper.
--
--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Recover_20
is
begin
   if Indenting_Token.ID = -Expression_Opt_ID and
     (Prev_1 = -With_ID and
        (Prev_3 = Invalid_Token_ID or
           Prev_3 /= Left_Paren_ID))
      or
     B
   then
      --  missing 'end if;'

end Ada_Mode.Recover_20;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
