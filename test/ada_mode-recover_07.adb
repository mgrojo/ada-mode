-- From an actual editing session.
--
-- Missing ';' after 'end loop', and 'return' in a procedure.
--
-- The indent parser does not treat 'return' in a procedure as an
-- error; it's just a statement.
--
-- Mckenzie used to encounter Unknown_State in a reduce during check of a
-- config returned by language_fixes. Now finds a good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_7 is -- 6
   procedure Follow
   is
   begin -- 10

      for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop -- 21
      end loop

        Prev_Result := Result; -- 27
      return Result; -- 31
   end Follow;

end Ada_Mode.Recover_7;
