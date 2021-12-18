-- Real error of missing string quote, now successfully recovered.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

-- We get different indent results from partial and incremental parse;
--EMACSCMD:(setq skip-reindent-test (not wisi-incremental-parse-enable))
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
--  Local Variables:
--  wisi-mckenzie-task-count: 1
--  ada-end-name-optional: nil
--  compare-tree-text: nil
--  End:
