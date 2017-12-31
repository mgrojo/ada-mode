-- Real error of missing string quote, now successfully recovered.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Recover_String_Quote is

   procedure Quote_Unquote (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Put_Line (Test_File, "8" & Tab & """nine");

      -- Error here; in the middle of splitting a long Put_Line into two,
      -- missing opening quote and other stuff.
      ten" & Tab & """eleven""");

      Close (Test_File);

   end Quote_Unquote;

end Ada_Mode.Recover_String_Quote;
