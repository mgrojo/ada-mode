-- Real error of missing string quote, now successfully recovered and
-- repaired.
--
-- We are testing repairing the errors, so we first create them by
-- editing.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Recover_String_Quote_1 is

   procedure Quote_Unquote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Put_Line (Test_File, "8" & Tab & """nine");

      --EMACSCMD:(progn (forward-line 1)(forward-word 1)(delete-forward-char 2)(insert "\" & Tab & \"\"\"")(forward-word 1)(delete-forward-char 2)(insert "\"\"\""))
      ten ; eleven( );
      --  ten" & Tab & """ eleven"""); -- edited line matches this

      --EMACSCMD:(progn (wisi-validate-cache (point-max) t 'navigate)(wisi-repair-errors (point-min)(point-max)))

      Close (Test_File);

   end Quote_Unquote;

end Ada_Mode.Recover_String_Quote_1;
