nprocedure Ada_Mode.Library_Procedure is
   -- no comment before "procedure"
   --EMACSCMD:(wisi-prj-select-file "subdir/ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "package Ada_Mode is"))
begin
   begin  -- should be indented.
      null;
   end;
end Ada_Mode.Library_Procedure;
