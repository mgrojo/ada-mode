separate (Ada_Mode)
procedure Separate_Procedure is
   -- no comment before "separate"
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")

   --  WORKAROUND: GNAT GPL 2016 doesn't produce a .ali file for this
   --  file, so gpr_query doesn't work. And gnat find doesn't work either
   --
   --  EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "procedure Separate_Procedure is"))
   --  EMACSRESULT:t
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Procedure;
