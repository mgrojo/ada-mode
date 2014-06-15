separate (Ada_Mode)
function Separate_Function return Integer is
   -- no comment before "separate"
   --EMACSCMD:(setq skip-recase-test t)
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "function Separate_Function return"))
   --EMACSRESULT:t

   -- `save-excursion' in run-indent-test.el run-test-here is defeated
   -- by 'ada-find-other-file', which screws up following tests
   --EMACSCMD:(find-file "ada_mode-separate_function.adb")
begin
   begin  -- should be indented.
      null;
   end;
   return 0;

   -- testing that ada-auto-case is buffer-local
   --EMACSCMD:ada-auto-case
   --EMACSRESULT:nil
   --EMACSCMD:(progn (forward-line 2)(forward-word 3)(execute-kbd-macro " ")(delete-char -1)(let ((case-fold-search nil))(looking-back "separate_function")))
   --EMACSRESULT:t
end separate_function;
--EMACSCMD:(progn (forward-line -1)(forward-word 2)(ada-goto-declaration t)(downcase-word 2)(execute-kbd-macro " ")(delete-char -1)(let ((case-fold-search nil))(looking-back "Separate_Function")))
--EMACSRESULT:t
-- Local Variables:
-- ada-auto-case: nil
-- End:
