-- For testing ada-make-package-body

--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "gnatstub.gpr") (gnat "gnatstub.prj")) (ada-prj-default))

package Ada_Mode.Spec is

   --EMACSCMD:(ada-make-package-body "ada_mode-spec.adb")
   --EMACSCMD:(progn (ada-find-other-file)(looking-at "package body Ada_Mode.Spec"))
   --EMACSCMD:(condition-case nil (delete-file "ada_mode-spec.adb") (error nil))

   function Function_1 (I : in Integer) return Float;

end Ada_Mode.Spec;
