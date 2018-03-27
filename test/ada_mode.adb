package body Ada_Mode is
   --EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

   protected body Separate_Protected_Body is separate;

   task Separate_Task_Body is
      entry Please_Abort;
   end;

   task body Separate_Task_Body is separate;

   -- WORKAROUND: prior to GNAT 2016, this went to
   -- ada_mode-separate_procedure.adb. Now it goes to ada_mode.ads.
   --
   --EMACSCMD:(progn (forward-line 2)(forward-word 2)(ada-goto-declaration)(looking-at "Separate_Procedure;"))
   --EMACSRESULT:t
   procedure Separate_Procedure is separate;

   -- testing that ada-auto-case is buffer-local;  nil in separate_package_1
   function Separate_Function return Integer is separate;
   --EMACSCMD:ada-auto-case
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(downcase-word 2)(execute-kbd-macro " ")(delete-char -1)(let ((case-fold-search nil))(looking-back "Ada_Mode")))
   --EMACSRESULT:t
end Ada_Mode;
-- Local Variables:
-- ada-auto-case: t
-- End:
