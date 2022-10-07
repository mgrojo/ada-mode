procedure Ada_Mode.Library_Procedure is
   -- no comment before "procedure"
   --EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "package Ada_Mode is"))
begin
   begin  -- should be indented.
      null;
   end;
end Ada_Mode.Library_Procedure;
