-- Indent with partial parse after 'end name;'.
--
-- The parse region starts after 'end if;'. With partial parse, 'end
--  Is_Reduce;' and the following blank line are aligned with 'end
--  if;' (which is not what we want). With incremental parse, the
--  indent is correct.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_Partial_10 is
   function Is_Reduce (Item : in LR1_Items.Item) return Boolean
   is begin
      if Has_Element (Item.Dot) then
         return True;
      end if;

      --EMACSCMD:(progn (end-of-line 3)(delete-char 1)(wisi-indent-newline-indent)(current-column))
      --EMACSRESULT:3
   end Is_Reduce;


end Ada_Mode.Recover_Partial_10;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- wisi-disable-face: t
-- End:
