-- For testing ada-make-package-body

--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "gnatstub.gpr") (gnat "gnatstub.prj")) (ada-prj-default))

package Ada_Mode.Spec is

   --EMACSCMD:(when (locate-file "gnatstub" exec-path '("" ".exe")) (ada-make-package-body "ada_mode-spec.adb") (ada-find-other-file) (looking-at "package body Ada_Mode.Spec"))
   --EMACSCMD:(condition-case nil (delete-file "ada_mode-spec.adb") (error nil))

   function Function_1 (I : in Integer) return Float;

end Ada_Mode.Spec;
