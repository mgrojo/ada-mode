function Ada_Mode.Library_Function return Integer is
   -- no comment before "function"
   --EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "Ada_Mode is"))

   --EMACSCMD:(progn (forward-line -6)(forward-word 4)(forward-char 1)(call-interactively 'wisi-goto-spec/body)(looking-at "Library_Function return Integer; -- spec"))
   --EMACSRESULT:t

begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Ada_Mode.Library_Function;
