procedure Ada_Mode.Library_Procedure is
   -- no comment before "procedure"
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (forward-line -2)(ada-find-other-file t)(looking-at "Ada_Mode is"))
begin
   begin  -- should be indented.
      null;
   end;
end Ada_Mode.Library_Procedure;
