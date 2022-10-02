-- WORKAROUND: ada_language_server 22.0 doesn't support FormatRange
--EMACSCMD:(setq skip-indent-test (eq ada-indent-backend 'eglot))
--EMACSCMD:(wisi-prj-select-cache "subdir/ada_mode.adp" (ada-prj-default))
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "package Ada_Mode"))
procedure Ada_Mode.Child_Procedure
is
begin
   null;
end Ada_Mode.Child_Procedure;
