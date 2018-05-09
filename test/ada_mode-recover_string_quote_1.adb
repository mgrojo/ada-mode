-- Real error of missing string quote, now successfully recovered.
--
-- However, there are two possible results, with equal cost and equal
-- recover op length, so it's a race condition which is used. The
-- indent results are the same but we can't test the results of
-- actually doing the recover.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Recover_String_Quote_1 is

   procedure Quote_Unquote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Put_Line (Test_File, "8" & Tab & """nine");

      ten" & Tab & """ eleven""");
      --  Actual error: missing 'Put_Line ("'

      Close (Test_File);

   end Quote_Unquote;

end Ada_Mode.Recover_String_Quote_1;
