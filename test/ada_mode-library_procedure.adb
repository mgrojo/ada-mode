procedure Ada_Mode.Library_Procedure is
   -- no comment before "procedure"
   --EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "package Ada_Mode is"))
begin
   begin  -- should be indented.
      null;
   end;
end Ada_Mode.Library_Procedure;
