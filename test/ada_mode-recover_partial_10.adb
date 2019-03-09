-- Indent with partial parse after 'end name;'.
--
-- The parse region starts after 'end if;'. Error recovery used to
-- delete 'end' instead of inserting 'begin', giving incorrect indent.
package body Ada_Mode.Recover_Partial_10 is
   function Is_Reduce (Item : in LR1_Items.Item) return Boolean
   is begin
      if Has_Element (Item.Dot) then
         return True;
      end if;

      -- EMACSCMD:(progn (end-of-line 3)(ada-indent-newline-indent)(current-column))
      -- EMACSRESULT:3
   end Is_Reduce;

end Ada_Mode.Recover_Partial_10;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- wisi-disable-face: t
-- End:
