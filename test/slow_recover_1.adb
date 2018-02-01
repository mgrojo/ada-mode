-- This is from a real editing session; it found a bug in the parser,
-- but now is much better.
--
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Indent_Hanging_1
is
begin
   if Indenting_Token.ID = -Expression_Opt_ID and
     (Prev_1 = -With_ID and
        (Prev_3 = Invalid_Token_ID or
           Prev_3 /= Left_Paren_ID))
     or
     ()
   then


   end Indent_Hanging_1;
