package body Ada_Mode is
   --EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "ada_mode.gpr") (gnat "ada_mode-gnatxref.prj")) (ada-prj-default))

   protected body Separate_Protected_Body is separate;

   task Separate_Task_Body is
      entry Please_Abort;
   end Separate_Task_Body;

   task body Separate_Task_Body is separate;

   -- WORKAROUND: prior to GNAT 2016, this went to
   -- ada_mode-separate_procedure.adb. Now it goes to ada_mode.ads.
   --
   --EMACSCMD:(cl-ecase ada-xref-backend ((eglot gnat) nil)(gpr_query (forward-line 2)(forward-word 2)(call-interactively 'wisi-goto-spec/body)(looking-at "Separate_Procedure;")))
   --EMACSRESULT:(eq ada-xref-backend 'gpr_query)
   procedure Separate_Procedure is separate;
   --EMACSCMD:(progn (forward-line -1) (test-all-defs "Separate_Procedure"))
   --EMACSRESULT_START:'("ada_mode.ads" "Separate_Procedure procedure")
   --EMACSRESULT_START:'("ada_mode.ads" "Separate_Procedure body")

   function Separate_Function return Integer is separate;
   --EMACSCMD:ada-auto-case
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(downcase-word 2)(execute-kbd-macro " ")(delete-char -1)(let ((case-fold-search nil))(looking-back "Ada_Mode")))
   --EMACSRESULT:t
end Ada_Mode;
-- Local Variables:
-- ada-auto-case: t
-- ada-xref-full-path: nil
-- ada-eglot-gpr-file: nil
-- End:
