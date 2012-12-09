separate (Ada_Mode)
procedure Separate_Procedure is
   -- no comment before "separate"
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "procedure Separate_Procedure is"))
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Procedure;
