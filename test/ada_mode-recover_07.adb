-- From an actual editing session.
--
-- Missing statement in loop, and missing ';' after 'end loop'.
--
-- Mckenzie used to encounter Unknown_State in a reduce during check of a
-- config returned by language_fixes. Now finds a reasonable solution quickly.
--
-- Ideally it would insert 'null;' before 'end loop', then insert ';'
-- following 'end loop'. However, Prev_Result looks like an end loop
-- name, so the solution (delete 'end') is cheaper.

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
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
