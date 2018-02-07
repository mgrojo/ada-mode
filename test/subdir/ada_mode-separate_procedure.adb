separate (Ada_Mode)
procedure Separate_Procedure is
   -- no comment before "separate"
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")

   -- WORKAROUND: GNAT GPL 2016/2017 puts a reference to this
   -- declaration in ada_mode.ali, but gpr_query doesn't see it.
   --
   --  EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "procedure Separate_Procedure is"))
   --  EMACSRESULT:t
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Procedure;
